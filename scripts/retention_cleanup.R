#!/usr/bin/env Rscript

##### ---- Scheduled retention cleanup -----------------------------------####
## Standalone worker: no Shiny, no session. Deletes uploaded dataset files and
## their upload logs once they are older than the retention period.
##
## Scope is deliberately narrow: raw uploads only. Models, recipes and outputs
## are left alone, so the notice shown to users must stay just as narrow.
##
## Dry run unless retention_dry_run=false. Runs once and exits: it is expected
## to be started once a day by the deployment, which owns the scheduling.
## Exits non-zero if anything went wrong, so a failed run is visible.
##
## Settings, supplied by DevOps (.env is not in the repository):
##   data_retention_months=3      whole months, at least 1
##                                required explicitly for real deletion
##   retention_grace_months=3     grace for datasets predating the policy
##   retention_policy_start=      YYYY-MM-DD, no default: unset means no deletion
##   retention_dry_run=true       must be set to false to delete anything
##   retention_report=            optional file to append the run report to
##   app_dir=                     the application folder, defaults to this
##                                script's parent folder
##   data_root=                   folder holding the per-user folders
##   users_db=                    path to users.sqlite
##
## An upload interrupted mid-write can leave a ".incoming-" staging file. It
## contains the same uploaded data, so it follows the same configured retention
## and grace periods as completed uploads. Its filesystem modification time is
## the upload time available before metadata exists; it is never deleted early.

##### ---- Where we are -------------------------------------------------####

## Nothing here relies on the working directory: a scheduler may start this
## from anywhere. Paths come from the script's own location unless configured.
this_script = function() {
	args = commandArgs(trailingOnly = FALSE)
	hit = grep("^--file=", args, value = TRUE)
	if (!length(hit)) return(NA_character_)
	normalizePath(sub("^--file=", "", hit[1]), mustWork = FALSE)
}

app_dir = Sys.getenv("app_dir", "")
if (!nzchar(app_dir)) {
	script = this_script()
	app_dir = if (!is.na(script)) dirname(dirname(script)) else getwd()
}

## .env fills in what the environment does not already provide. readRenviron()
## overwrites, so anything the container set explicitly is put back afterwards:
## a stale .env must never quietly win over the deployment's own settings.
settings = c("data_retention_months", "retention_grace_months", "retention_policy_start"
	, "retention_dry_run", "retention_report", "data_root", "users_db")
env_file = file.path(app_dir, ".env")
if (file.exists(env_file)) {
	preset = Sys.getenv(settings, unset = NA)
	readRenviron(env_file)
	preset = preset[!is.na(preset)]
	if (length(preset)) do.call(Sys.setenv, as.list(preset))
}

source(file.path(app_dir, "server", "delete_data_helpers.R"))

data_root = Sys.getenv("data_root", app_dir)
users_db = Sys.getenv("users_db", file.path(app_dir, "users_db", "users.sqlite"))
report_file = Sys.getenv("retention_report", "")
dry_run = !identical(tolower(Sys.getenv("retention_dry_run", "true")), "false")

## Report goes to stdout and, when configured, to a file on a mounted volume so
## a dry run can still be read after the container is gone. A report that stops
## being writable partway through is remembered: the run must not end looking
## clean when part of what it did was never recorded.
report_failed = FALSE
say = function(...) {
	line = paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ...)
	cat(line, "\n")
	if (nzchar(report_file)) {
		ok = tryCatch({
			cat(line, "\n", file = report_file, append = TRUE)
			TRUE
		}, error = function(e) FALSE, warning = function(w) FALSE)
		if (!isTRUE(ok)) report_failed <<- TRUE
	}
}

stop_now = function(...) {
	say("ERROR:", ...)
	say("retention cleanup aborted, nothing deleted")
	quit(status = 1)
}

##### ---- Configuration, all checked before anything is touched --------####

retention_raw = Sys.getenv("data_retention_months", "")
retention_months = if (nzchar(retention_raw)) retention_raw else "3"
grace_months = Sys.getenv("retention_grace_months", retention_months)
## Strictly YYYY-MM-DD: as.Date() would happily read "2026-09-01junk"
policy_raw = Sys.getenv("retention_policy_start", "")
policy_start = if (valid_policy_date(policy_raw)) as.Date(policy_raw, format = "%Y-%m-%d") else as.Date(NA)

if (!valid_months(retention_months)) {
	stop_now(paste0("data_retention_months must be a whole number of months, at least 1; got '"
		, retention_raw, "'"))
}
if (!valid_months(grace_months)) {
	stop_now(paste0("retention_grace_months must be a whole number of months, at least 1; got '"
		, Sys.getenv("retention_grace_months", ""), "'"))
}
retention_months = as.integer(retention_months)
grace_months = as.integer(grace_months)

if (nzchar(policy_raw) && is.na(policy_start)) {
	stop_now(paste0("retention_policy_start must be an exact YYYY-MM-DD date; got '"
		, policy_raw, "'"))
}
## No effective date, no deletion. Falling back to a default here would delete
## every dataset older than the retention period on the very first live run.
if (is.na(policy_start)) {
	say("retention_policy_start is not set: staying in dry run and deleting nothing")
	dry_run = TRUE
}

## Deleting for real is never done on an assumed retention period: the length
## of time people's data is kept has to be stated by whoever turned this on.
if (!dry_run && !nzchar(retention_raw)) {
	stop_now("data_retention_months must be set explicitly when retention_dry_run=false")
}

## A report that cannot be written would hide what was deleted, so it is
## checked now rather than discovered halfway through the run.
if (nzchar(report_file)) {
	writable = tryCatch({
		cat("", file = report_file, append = TRUE)
		TRUE
	}, error = function(e) FALSE, warning = function(w) FALSE)
	if (!isTRUE(writable)) stop_now(paste0("cannot write the report file at '", report_file, "'"))
}

## Without the user database there is no list of users, and a run that quietly
## does nothing must not look like a clean run.
if (!file.exists(users_db)) stop_now(paste0("user database not found at '", users_db, "'"))

## Likewise a data root that is not there: every user would simply be skipped
if (!dir.exists(data_root)) stop_now(paste0("data_root is not a directory: '", data_root, "'"))

##### ---- Users --------------------------------------------------------####

## Registered users only, read from the user database. Folders are never
## scanned: an unexpected directory under the data root must not be treated as
## somebody's data. Names that are not a single path segment, or whose folder
## resolves outside the data root, are skipped.
registered_users = function() {
	con = tryCatch(DBI::dbConnect(RSQLite::SQLite(), users_db), error = function(e) NULL)
	if (is.null(con)) stop_now(paste0("cannot open the user database at '", users_db, "'"))
	on.exit(DBI::dbDisconnect(con), add = TRUE)
	names = tryCatch(DBI::dbGetQuery(con, "SELECT username FROM users")$username
		, error = function(e) NULL)
	if (is.null(names)) stop_now(paste0("cannot read the users table in '", users_db, "'"))
	names = names[!is.na(names)]
	usable = vapply(names, valid_path_segment, logical(1))
	if (any(!usable)) say("skipped unusable user names:", paste(names[!usable], collapse = ", "))
	names = names[usable]
	keep = vapply(names, function(n) {
		user_dir = file.path(data_root, n)
		dir.exists(user_dir) && path_inside(user_dir, data_root) &&
			dir.exists(file.path(user_dir, "datasets")) &&
			dir.exists(file.path(user_dir, ".log_files"))
	}, logical(1))
	names[keep]
}

##### ---- Run ----------------------------------------------------------####

say("retention cleanup starting; months =", retention_months
	, "; grace =", grace_months
	, "; policy start =", if (is.na(policy_start)) "unset" else format(policy_start)
	, "; dry run =", dry_run)

problems = 0

for (user_name in registered_users()) {
	app_username = file.path(data_root, user_name)
	## The lock is taken first: the summary is read, acted on and rewritten
	## inside it, so nothing is based on a listing that has since moved on.
	done = with_upload_lock(app_username, {
		summary = read_upload_summary(app_username)

		## Anything on disk that the summary does not list is kept for ever
		## without ever being considered, whatever state the summary is in.
		## Checked here rather than inside one branch, because a missing or
		## empty summary is exactly when files are most likely to be stranded.
		listed = if (identical(summary$status, "ok")) {
			as.character(summary$logs$file_name[!is.na(summary$logs$file_name)])
		} else {
			character(0)
		}
		on_disk_logs = sub("-upload\\.main\\.log$", ""
			, list.files(file.path(app_username, ".log_files")
				, pattern = "-upload\\.main\\.log$"))
		## Resolve the datasets folder before listing or deleting anything.
		## This refuses a symlinked folder instead of following it elsewhere.
		datasets_dir = safe_dir(app_username, "datasets")
		if (is.null(datasets_dir)) {
			say(user_name, "- datasets folder is outside the user directory, skipped")
			problems = problems + 1
			all_sets = character(0)
		} else {
			## all.files: staging names begin with a dot, so the default listing
			## would not show them at all
			all_sets = list.files(datasets_dir, all.files = TRUE, no.. = TRUE)
		}
		staging = grep("^\\.incoming-", all_sets, value = TRUE)
		if (length(staging)) {
			staging_paths = file.path(datasets_dir, staging)
			staging_logs = data.frame(
				file_name = staging
				, upload_time = format(file.info(staging_paths)$mtime
					, "%d-%m-%Y %H:%M:%S")
				, stringsAsFactors = FALSE
			)
			staging_found = expired_datasets(staging_logs, retention_months
				, policy_start = policy_start, now = Sys.time()
				, grace_months = grace_months)
			if (length(staging_found$unparsed)) {
				say(user_name, "- staging modification time unreadable:"
					, paste(staging_found$unparsed, collapse = ", "))
				problems = problems + 1
			}
			for (staging_name in staging_found$expired) {
				say(user_name, "- expired upload staging file:", staging_name)
				if (!dry_run && !log_deletion(user_name
					, paste0("retention: authorized deletion of expired staging file "
						, staging_name), db = users_db)) {
					say("   ", user_name, "|", staging_name
						, "| audit could not be written, left alone")
					problems = problems + 1
					next
				}
				staging_result = delete_dataset(app_username, staging_name
					, dry_run = dry_run)
				for (i in seq_len(NROW(staging_result))) {
					say("   ", user_name, "|", staging_result$type[i], "|"
						, staging_result$path[i], "->", staging_result$reason[i])
				}
				if (!dry_run && !deletion_complete(staging_result)) {
					problems = problems + 1
				}
			}
		}
		on_disk_sets = setdiff(all_sets, staging)
		unlisted = setdiff(unique(c(on_disk_logs, on_disk_sets)), listed)
		if (length(unlisted)) {
			say(user_name, "- not listed in the summary, so never checked:"
				, paste(sort(unlisted), collapse = ", "))
			problems = problems + 1
		}

		if (summary$status == "empty") {
			## Normal: the last dataset has already gone
			NULL
		} else if (summary$status == "missing") {
			## Normal when nothing was ever uploaded. Anything actually sitting
			## there has already been reported by the check above.
			NULL
		} else if (summary$status != "ok") {
			say(user_name, "- upload summary is", summary$status, ", skipped")
			problems = problems + 1
		} else {
			logs = summary$logs
			found = expired_datasets(logs, retention_months, policy_start = policy_start
				, now = Sys.time(), grace_months = grace_months)
			if (length(found$unparsed)) {
				## Kept on purpose, but it is not a clean state: these can never
				## expire while the timestamp cannot be read
				say(user_name, "- kept, upload_time unreadable:", paste(found$unparsed, collapse = ", "))
				problems = problems + 1
			}
			if (length(found$expired)) {
				say(user_name, "- expired:", paste(found$expired, collapse = ", "))
				remaining = logs
				for (file_name in found$expired) {
					## One authoritative row, or we do not know whether the file
					## belongs to this row or to a newer upload of the same name
					row = find_dataset_row(logs, file_name)
					if (is.na(row)) {
						say("   ", user_name, "|", file_name
							, "| not listed exactly once, left alone")
						problems = problems + 1
						next
					}
					## An intent is only opened for work actually about to be
					## done. Tidying up after a run that already deleted the
					## files opens none: that run wrote its own intent, and a
					## second one would leave the trail with more intents than
					## closing rows.
					## An intent is opened only when there is work to start and
					## no earlier one is still hanging open. A run that died
					## partway through - one file gone, one still there - left
					## its own intent, and a second would never pair up.
					present = dataset_present(app_username, file_name)
					resuming = open_intent_exists(user_name, file_name, db = users_db)
					if (!dry_run && present && !resuming && !log_deletion(user_name
						, paste0("retention: deleting dataset ", file_name), db = users_db)) {
						say("   ", user_name, "|", file_name
							, "| audit could not be written, left alone")
						problems = problems + 1
						next
					}
					result = delete_dataset(app_username, file_name, dry_run = dry_run)
					for (i in seq_len(NROW(result))) {
						say("   ", user_name, "|", result$type[i], "|", result$path[i]
							, "->", result$reason[i])
					}
					if (dry_run) next
					if (!deletion_complete(result)) {
						problems = problems + 1
						next
					}
					## Every deletion is closed off in the audit trail, whether
					## this run removed the files or tidied up after one that
					## already had.
					closing = if (present && !resuming) {
						paste0("retention: deleted dataset ", file_name)
					} else {
						paste0("retention: reconciled dataset ", file_name
							, " (files already absent)")
					}
					if (!log_deletion(user_name, closing, db = users_db)) {
						## The files are gone either way, but the row stays in
						## the summary so the next run tries again. Dropping it
						## here would leave the deletion permanently unrecorded.
						say("   WARNING:", user_name, "|", file_name
							, "| deleted but the closing audit row could not be"
							, "written; the row is kept for another attempt")
						problems = problems + 1
						next
					}
					## Pruned only once the deletion is fully recorded
					remaining = remaining[-find_dataset_row(remaining, file_name), , drop = FALSE]
				}
				if (!dry_run && NROW(remaining) != NROW(logs)) {
					if (!write_upload_summary(app_username, remaining)) {
						say("   ERROR:", user_name, "| files deleted but the upload summary"
							, "could not be rewritten; the next run will repair it")
						problems = problems + 1
					}
				}
			}
		}
		TRUE
	})
	## Work that did not happen is not a clean run: the next one should be
	## looked at, so contention is counted rather than passed over
	if (is.null(done)) {
		say(user_name, "- skipped, another delete is in progress")
		problems = problems + 1
	}
}

if (isTRUE(report_failed)) {
	say("WARNING: part of this run could not be written to the report file")
	problems = problems + 1
}

say("retention cleanup finished; problems =", problems)
## Checked again after the closing line, which can be the write that fails
if (problems == 0 && isTRUE(report_failed)) {
	cat("WARNING: the closing report line could not be written\n")
	problems = 1
}
if (problems > 0) quit(status = 1)
