##### ---- Shared dataset deletion helpers ------------------------------####
## Plain functions: no reactives, no session. Sourced both by the app and by
## scripts/retention_cleanup.R so the deletion rules live in one place only.

##### ---- Names and paths ----------------------------------------------####

## One path segment: a dataset file name, or a user folder name. Anything
## carrying a path separator is rejected outright. basename() is not enough
## here: it turns "../mydata.csv" into "mydata.csv" and would then delete a
## different, perfectly valid file. Note that ".." is only rejected on its own,
## since "report..v2.csv" is a legitimate name.
valid_path_segment = function(name) {
	if (is.null(name) || length(name) != 1 || is.na(name)) return(FALSE)
	name = as.character(name)
	if (!nzchar(trimws(name))) return(FALSE)
	if (grepl("[/\\\\]", name)) return(FALSE)
	if (name %in% c(".", "..")) return(FALSE)
	TRUE
}

## Is child inside parent once both are fully resolved? fs::path_real() follows
## symlinks, so this holds even when something on the way is a link.
## Both paths have to exist for path_real() to resolve them.
path_inside = function(child, parent) {
	if (!fs::file_exists(child) || !fs::file_exists(parent)) return(FALSE)
	fs::path_has_parent(fs::path_real(child), fs::path_real(parent))
}

## Canonical path of a folder that must sit inside the user folder, or NULL.
## The folder is resolved rather than the file: a folder exists, so it resolves
## consistently, while a file that is about to be created does not resolve at
## all. This is what stops a symlinked datasets/ or .log_files/ from
## redirecting deletion somewhere else.
safe_dir = function(app_username, dir_name) {
	dir = fs::path(app_username, dir_name)
	if (!fs::dir_exists(dir)) return(NULL)
	if (!path_inside(dir, app_username)) return(NULL)
	as.character(fs::path_real(dir))
}

## Dataset file and its upload log, both inside the user folder.
## Returns NULL when the id is unusable or a folder resolves outside.
dataset_paths = function(app_username, file_name) {
	if (!valid_path_segment(file_name)) return(NULL)
	datasets = safe_dir(app_username, "datasets")
	log_files = safe_dir(app_username, ".log_files")
	if (is.null(datasets) || is.null(log_files)) return(NULL)
	list(
		dataset = file.path(datasets, file_name)
		, upload_log = file.path(log_files, paste0(file_name, "-upload.main.log"))
	)
}

## The user's .log_files folder, verified to be inside the user folder, or NULL.
## Everything that writes into it goes through here first, so a symlinked
## .log_files cannot redirect a lock file or a summary rewrite somewhere else.
user_log_dir = function(app_username) {
	safe_dir(app_username, ".log_files")
}

## Where the combined upload summary lives for one user, or NULL when the
## folder it should live in does not check out
upload_summary_path = function(app_username) {
	dir = user_log_dir(app_username)
	if (is.null(dir)) return(NULL)
	file.path(dir, ".automl-shiny-upload.main.log")
}

## The one row for this dataset, or NA when it is absent, listed more than
## once, or the column holds missing values.
##
## Comparisons are done on indices rather than sum(x == name): a NA in the
## column makes that comparison NA, and "if (NA != 1)" stops with an error in
## the middle of a destructive path.
find_dataset_row = function(logs, file_name) {
	if (is.null(logs) || !NROW(logs) || !"file_name" %in% names(logs)) return(NA_integer_)
	if (!valid_path_segment(file_name)) return(NA_integer_)
	hits = which(!is.na(logs$file_name) & as.character(logs$file_name) == file_name)
	if (length(hits) != 1) return(NA_integer_)
	as.integer(hits)
}

##### ---- Calendar arithmetic ------------------------------------------####

## Upload times are written by the app as local wall-clock time, so they are
## read back as local time too. Converting them to UTC would move every
## timestamp by the machine's offset and describe a moment that never happened.
## Everything - upload time, policy date, now - is compared on the one clock the
## application already uses.

## Strict "%d-%m-%Y %H:%M:%S", the format the app writes. as.POSIXct() ignores
## anything trailing, so "01-01-2024 09:00:00junk" would otherwise be read as a
## real time and the dataset deleted on the strength of it.
parse_upload_time = function(x) {
	x = as.character(x)
	ok = !is.na(x) & grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$", x)
	out = as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01")
	if (!any(ok)) return(out)
	parsed = suppressWarnings(as.POSIXct(x[ok], format = "%d-%m-%Y %H:%M:%S"))
	## Round trip rejects a well-formed but impossible time such as 31-02-2024
	round_trips = !is.na(parsed) & format(parsed, "%d-%m-%Y %H:%M:%S") == x[ok]
	parsed[!round_trips] = NA
	out[ok] = parsed
	out
}

## Strict YYYY-MM-DD. as.Date() is too forgiving to decide when people's data
## starts being deleted: it reads "2026-09-01junk" as a valid date, and
## "2026-9-1" as well.
valid_policy_date = function(x) {
	if (is.null(x) || length(x) != 1 || is.na(x)) return(FALSE)
	x = as.character(x)
	if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) return(FALSE)
	parsed = tryCatch(suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
		, error = function(e) as.Date(NA))
	if (is.na(parsed)) return(FALSE)
	## Round trip catches a well-formed but impossible date such as 2026-02-31
	identical(format(parsed, "%Y-%m-%d"), x)
}

## Add n months to a date-time, rolling back to the end of the target month.
## base R's seq(x, by = "n months") is wrong at month ends: it turns 31 May
## minus three months into 3 March, which would delete a 1 March upload two days
## early. lubridate rolls back instead, so 31 January plus one month is
## 28 February, or 29 February in a leap year.
add_months = function(x, n) {
	lubridate::add_with_rollback(x, lubridate::period(months = n))
}

## Retention and grace are periods in whole months. Nonsense values must not
## quietly turn into "delete everything", so they are rejected here.
## The ceiling of 1200 months is a hundred years: far past any real retention
## policy, and there to catch a typo such as a date pasted into the field.
valid_months = function(months, max_months = 1200) {
	if (is.null(months) || length(months) != 1) return(FALSE)
	months = suppressWarnings(as.numeric(months))
	if (is.na(months)) return(FALSE)
	if (months != round(months)) return(FALSE)
	months >= 1 && months <= max_months
}

##### ---- What has expired ---------------------------------------------####

## When each dataset is due for deletion.
## A dataset expires retention_months after it was uploaded. Datasets uploaded
## before the policy took effect also get a one-off grace period, so they
## expire at the later of their own due date and the end of that grace window.
## Datasets uploaded after the policy started are not affected by the grace.
expiry_time = function(uploaded, retention_months, policy_start, grace_months) {
	due = add_months(uploaded, retention_months)
	policy_start = as.POSIXct(as.character(policy_start))
	grace_ends = add_months(policy_start, grace_months)
	before_policy = !is.na(uploaded) & uploaded < policy_start
	due[before_policy] = pmax(due[before_policy], grace_ends)
	due
}

## Datasets whose retention has run out. Rows with a date we cannot read are
## reported separately and are never deleted.
##
## policy_start is the day the retention policy takes effect and has no default
## on purpose: without it the first live run would delete everything already
## older than the retention period the moment the policy is switched on.
## Anything unusable - no policy date, a nonsense retention period - returns
## nothing to delete rather than guessing.
expired_datasets = function(logs, retention_months, policy_start, now = Sys.time()
	, grace_months = retention_months) {
	empty = list(expired = character(0), unparsed = character(0))
	if (is.null(logs) || !NROW(logs)) return(empty)
	if (!all(c("file_name", "upload_time") %in% names(logs))) return(empty)
	if (!valid_months(retention_months) || !valid_months(grace_months)) return(empty)
	if (is.null(policy_start) || length(policy_start) != 1 || is.na(policy_start)) return(empty)
	## A row with no name cannot be acted on safely, so it is never selected
	named = !is.na(logs$file_name)
	uploaded = parse_upload_time(logs$upload_time)
	uploaded[!named] = NA
	unparsed = as.character(logs$file_name[named & is.na(uploaded)])
	due = expiry_time(uploaded, retention_months, policy_start, grace_months)
	list(
		expired = as.character(logs$file_name[named & !is.na(uploaded) & now > due])
		, unparsed = unparsed
	)
}

##### ---- Reading and writing the summary ------------------------------####

## The combined upload summary. Always read inside the lock: it is the state
## the rewrite is based on.
##
## Returns a status alongside the rows, because these mean different things:
##   ok       rows to work with
##   empty    a valid summary with no datasets left - normal after the last
##            one is deleted, and not a problem to report
##   missing  no summary file yet - nothing has been uploaded
##   corrupt  there is a file but it cannot be read, or has no file_name column
##   unusable the .log_files folder did not check out
read_upload_summary = function(app_username) {
	f = upload_summary_path(app_username)
	if (is.null(f)) return(list(status = "unusable", logs = NULL))
	if (!file.exists(f)) return(list(status = "missing", logs = NULL))
	logs = try(read.table(f, header = TRUE), silent = TRUE)
	if (any(class(logs) %in% "try-error")) return(list(status = "corrupt", logs = NULL))
	## Both columns are needed: without upload_time nothing can be judged
	## expired, and a summary missing it must be reported, not treated as valid
	if (!all(c("file_name", "upload_time") %in% names(logs))) {
		return(list(status = "corrupt", logs = NULL))
	}
	list(status = if (NROW(logs)) "ok" else "empty", logs = logs)
}

## The one place that rewrites the summary. Writes to a temp file in the same
## verified folder and renames it over the summary, so a crash cannot leave the
## summary half written. Returns TRUE only when the rename actually happened.
##
## The temp name is randomised on purpose. A fixed one such as
## "<summary>.tmp" can be created in advance as a symlink pointing at another
## file, and the write would then follow it and overwrite that file instead.
write_upload_summary = function(app_username, logs) {
	final = upload_summary_path(app_username)
	if (is.null(final)) return(FALSE)
	temp = tempfile(pattern = ".upload-summary-", tmpdir = dirname(final))
	ok = tryCatch({
		suppressWarnings(write.table(logs, file = temp, row.names = FALSE))
		TRUE
	}, error = function(e) FALSE)
	if (!isTRUE(ok)) {
		unlink(temp)
		return(FALSE)
	}
	## rename() replaces the destination itself, symlink included
	renamed = isTRUE(file.rename(temp, final))
	if (!renamed) unlink(temp)
	renamed
}

##### ---- Locking ------------------------------------------------------####

## Serialises the whole read-delete-rewrite cycle for one user across processes,
## so the app and the cleanup worker cannot act on the same folder at once.
## filelock takes an operating system lock: it is released when the process
## exits, crash included, so there is no stale lock to time out or take over.
## Returns NULL without running expr when someone else holds the lock, or when
## the folder the lock belongs in does not check out - the folder is verified
## before the lock file is created, so a symlinked .log_files cannot place it
## outside the user directory.
##
## timeout_ms = 0 means do not wait: a caller that cannot get in right now
## reports that and lets the next run pick the work up, rather than piling up
## behind whoever is deleting.
with_upload_lock = function(app_username, expr, timeout_ms = 3000) {
	dir = user_log_dir(app_username)
	if (is.null(dir)) return(NULL)
	lock_path = file.path(dir, ".delete.lock")
	## A symlink here - dangling or not - would put the lock file wherever it
	## points, outside the folder we just verified
	if (fs::is_link(lock_path)) return(NULL)
	lock = filelock::lock(lock_path, exclusive = TRUE, timeout = timeout_ms)
	if (is.null(lock)) return(NULL)
	on.exit(filelock::unlock(lock), add = TRUE)
	force(expr)
}

##### ---- Deleting -----------------------------------------------------####

## Delete one dataset and its upload log.
## Always returns a data frame so the caller can report exactly what happened:
## path, type, existed, deleted, reason. Never a bare TRUE/FALSE.
delete_dataset = function(app_username, file_name, dry_run = FALSE) {
	paths = dataset_paths(app_username, file_name)
	if (is.null(paths)) {
		return(data.frame(path = NA_character_, type = "dataset_id", existed = FALSE
			, deleted = FALSE, reason = "unusable id, or folder outside the user directory"
			, stringsAsFactors = FALSE))
	}
	out = lapply(names(paths), function(type) {
		p = paths[[type]]
		if (!file.exists(p)) {
			return(data.frame(path = p, type = type, existed = FALSE, deleted = FALSE
				, reason = "not found", stringsAsFactors = FALSE))
		}
		if (isTRUE(dry_run)) {
			return(data.frame(path = p, type = type, existed = TRUE, deleted = FALSE
				, reason = "dry run", stringsAsFactors = FALSE))
		}
		suppressWarnings(file.remove(p))
		## Confirm against the disk rather than trusting the return value
		gone = !file.exists(p)
		data.frame(path = p, type = type, existed = TRUE, deleted = gone
			, reason = if (gone) "deleted" else "delete failed", stringsAsFactors = FALSE)
	})
	do.call(rbind, out)
}

## Is anything still on disk for this dataset? Used to decide whether a run is
## deleting files or tidying up after one that already did, so the audit trail
## records the right thing and does not open a second intent for work that has
## already happened.
dataset_present = function(app_username, file_name) {
	paths = dataset_paths(app_username, file_name)
	if (is.null(paths)) return(FALSE)
	any(vapply(paths, file.exists, logical(1)))
}

## Is there already an intent for this dataset with nothing closing it?
## A run that died partway leaves one behind. Without this check a later run
## opens a second intent for work the first one had already started, and the
## trail no longer pairs up.
open_intent_exists = function(username, file_name, db = "users_db/users.sqlite"
	, prefix = "retention: ") {
	if (!file.exists(db)) return(FALSE)
	con = tryCatch(DBI::dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
	if (is.null(con)) return(FALSE)
	on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
	count_rows = function(action) {
		out = tryCatch(DBI::dbGetQuery(con,
			"SELECT COUNT(*) AS n FROM users_activity WHERE username = ? AND action = ?"
			, params = list(username, action))$n, error = function(e) NA_integer_)
		if (length(out) != 1 || is.na(out)) 0L else as.integer(out)
	}
	## The worker writes "retention: deleting dataset x"; the app writes
	## "deleting dataset: x". Both are checked through the same prefix argument.
	sep = if (nzchar(prefix)) " " else ": "
	opened = count_rows(paste0(prefix, "deleting dataset", sep, file_name))
	closed = count_rows(paste0(prefix, "deleted dataset", sep, file_name)) +
		count_rows(paste0(prefix, "reconciled dataset", sep, file_name, " (files already absent)"))
	opened > closed
}

## TRUE only when this call actually removed something. Used for the audit
## trail: a dry run, or a dataset that was already gone, is not a deletion.
deletion_succeeded = function(result) {
	if (is.null(result) || !NROW(result)) return(FALSE)
	if (any(result$type == "dataset_id")) return(FALSE)
	if (!any(result$existed)) return(FALSE)
	all(!result$existed | result$deleted)
}

## TRUE when nothing is left on disk for this dataset, whether this call
## removed the files or an earlier run already had.
##
## This is what the summary row is pruned on, and it is the difference between
## a half-finished deletion being repairable or not. If the files go but the
## summary rewrite fails, the next run finds them missing: judged on
## deletion_succeeded() alone it would report a failure forever and never tidy
## the row away, so running again has to be able to finish the job.
deletion_complete = function(result) {
	if (is.null(result) || !NROW(result)) return(FALSE)
	if (any(result$type == "dataset_id")) return(FALSE)
	if (any(result$reason == "dry run")) return(FALSE)
	all(result$deleted | !result$existed)
}

##### ---- Audit --------------------------------------------------------####

## A deletion is recorded twice: an intent before the files are touched, and a
## completion once they are gone.
##
## There is no queue for rows that cannot be written. An earlier version kept
## one in the user's folder and retried it later, and it was worse than the
## problem: the queue file could itself be redirected by a symlink, and an
## unreadable queue was treated as an empty one and deleted, losing the very
## records it existed to protect. The rule now is simply that a deletion which
## cannot be recorded does not happen - see require_audit() at the call sites.

## Audit row in users_activity, the table the admin dashboard already reads.
## The database is shared with the running app, so wait and retry when it is
## busy. Returns FALSE when the row could not be written: a deletion that is
## not audited has to be reported, never passed over quietly.
##
## The waits are deliberately short, because this runs while the upload lock is
## held and everything else waiting on that lock is stalled behind it. Two
## attempts with a 2 second SQLite wait covers an app writing at the same
## moment; anything longer would be felt by whoever is using the platform.
log_deletion = function(username, action, db = "users_db/users.sqlite", retries = 2
	, busy_timeout_ms = 2000) {
	if (!file.exists(db)) return(FALSE)
	stamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
	for (i in seq_len(retries)) {
		con = tryCatch(DBI::dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
		if (is.null(con)) {
			Sys.sleep(1)
			next
		}
		ok = tryCatch({
			DBI::dbExecute(con, sprintf("PRAGMA busy_timeout = %d", busy_timeout_ms))
			DBI::dbExecute(con,
				"INSERT INTO users_activity (username, action, timestamp) VALUES (?, ?, ?)"
				, params = list(username, action, stamp))
			TRUE
		}, error = function(e) FALSE)
		try(DBI::dbDisconnect(con), silent = TRUE)
		if (isTRUE(ok)) return(TRUE)
		Sys.sleep(1)
	}
	FALSE
}
