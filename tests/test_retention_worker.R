#!/usr/bin/env Rscript

##### ---- Tests for the retention worker itself -------------------------####
## The helper tests cover the rules; these run the worker as a process and
## check what it actually does to a folder, including the operational failures
## that a unit test cannot see. Run from the app folder with:
##   Rscript tests/test_retention_worker.R

app_dir = normalizePath(".")
worker = file.path(app_dir, "scripts", "retention_cleanup.R")
stopifnot(file.exists(worker))

checks = 0
check = function(label, ...) {
	stopifnot(...)
	checks <<- checks + 1
	cat("ok -", label, "\n")
}

## A throwaway data root holding one user, some datasets and a summary
make_root = function(files, times, summary_rows = NULL) {
	root = file.path(tempdir(), paste0("root-", as.integer(runif(1, 1, 1e9))))
	user = file.path(root, "tester@aphrc.org")
	dir.create(file.path(user, "datasets"), recursive = TRUE)
	dir.create(file.path(user, ".log_files"), recursive = TRUE)
	for (f in files) {
		writeLines("a,b", file.path(user, "datasets", f))
		writeLines("meta", file.path(user, ".log_files", paste0(f, "-upload.main.log")))
	}
	logs = if (is.null(summary_rows)) {
		data.frame(file_name = files, upload_time = times, stringsAsFactors = FALSE)
	} else {
		summary_rows
	}
	write.table(logs, file.path(user, ".log_files", ".automl-shiny-upload.main.log")
		, row.names = FALSE)
	db = file.path(root, "users.sqlite")
	con = DBI::dbConnect(RSQLite::SQLite(), db)
	invisible(DBI::dbExecute(con, "CREATE TABLE users (username TEXT)"))
	invisible(DBI::dbExecute(con, "INSERT INTO users (username) VALUES ('tester@aphrc.org')"))
	invisible(DBI::dbExecute(con,
		"CREATE TABLE users_activity (username TEXT, action TEXT, timestamp TEXT)"))
	DBI::dbDisconnect(con)
	list(root = root, user = user, db = db)
}

## Run the worker as its own process and hand back output and exit status
run_worker = function(fixture, ...) {
	settings = c(list(...)
		, data_root = fixture$root, users_db = fixture$db, app_dir = app_dir)
	env = paste0(names(settings), "=", unlist(settings))
	out = suppressWarnings(system2("Rscript", worker, env = env
		, stdout = TRUE, stderr = TRUE))
	status = attr(out, "status")
	list(output = paste(out, collapse = "\n"), status = if (is.null(status)) 0L else status)
}

datasets_left = function(fixture) sort(list.files(file.path(fixture$user, "datasets")))
audit_rows = function(fixture) {
	con = DBI::dbConnect(RSQLite::SQLite(), fixture$db)
	on.exit(DBI::dbDisconnect(con), add = TRUE)
	DBI::dbGetQuery(con, "SELECT action FROM users_activity")$action
}

old = "01-01-2024 09:00:00"
recent = format(Sys.time(), "%d-%m-%Y %H:%M:%S")

##### ---- Dry run is the default ---------------------------------------####

fx = make_root(c("old.csv", "new.csv"), c(old, recent))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3")
check("a dry run deletes nothing"
	, identical(datasets_left(fx), c("new.csv", "old.csv")))
check("and says what it would have deleted", grepl("old.csv", res$output))
check("and exits cleanly", res$status == 0)
check("and records no audit rows", length(audit_rows(fx)) == 0)

##### ---- Real deletion ------------------------------------------------####

fx = make_root(c("old.csv", "new.csv"), c(old, recent))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("the expired dataset is deleted", identical(datasets_left(fx), "new.csv"))
check("its upload log goes too"
	, !file.exists(file.path(fx$user, ".log_files", "old.csv-upload.main.log")))
check("the summary no longer lists it"
	, !grepl("old.csv", paste(readLines(file.path(fx$user, ".log_files"
		, ".automl-shiny-upload.main.log")), collapse = " ")))
check("both audit rows are written"
	, any(grepl("deleting dataset old.csv", audit_rows(fx)))
	, any(grepl("deleted dataset old.csv", audit_rows(fx))))
check("the run exits cleanly", res$status == 0)

##### ---- Nothing happens without an effective date --------------------####

fx = make_root("old.csv", old)
res = run_worker(fx, retention_dry_run = "false", data_retention_months = "3")
check("no policy date means nothing is deleted", identical(datasets_left(fx), "old.csv"))
check("and it says so", grepl("not set", res$output))

##### ---- A stale summary is reported ----------------------------------####

## The dataset and its upload log are on disk, but the summary never listed it.
## It would otherwise sit there forever without ever being considered.
fx = make_root(c("listed.csv", "unlisted.csv"), c(recent, old)
	, summary_rows = data.frame(file_name = "listed.csv", upload_time = recent
		, stringsAsFactors = FALSE))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("an unlisted upload log is reported", grepl("unlisted.csv", res$output))
check("and the run does not look clean", res$status != 0)
check("but nothing is deleted on the strength of it"
	, identical(datasets_left(fx), c("listed.csv", "unlisted.csv")))

##### ---- Deletion needs a recordable audit trail ----------------------####

fx = make_root("old.csv", old)
invisible(file.remove(fx$db))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("no database means the run aborts", res$status != 0)
check("and nothing is deleted", identical(datasets_left(fx), "old.csv"))

##### ---- Bad configuration fails closed -------------------------------####

fx = make_root("old.csv", old)
res = run_worker(fx, retention_policy_start = "2026-09-01junk")
check("a sloppy date aborts", res$status != 0, grepl("exact YYYY-MM-DD", res$output))
check("and deletes nothing", identical(datasets_left(fx), "old.csv"))

fx = make_root("old.csv", old)
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "0"
	, retention_dry_run = "false")
check("a zero retention period aborts", res$status != 0)
check("and deletes nothing", identical(datasets_left(fx), "old.csv"))

fx = make_root("old.csv", old)
res = run_worker(fx, retention_policy_start = "2020-01-01", retention_dry_run = "false"
	, retention_report = "/proc/nowhere/report.log")
check("an unwritable report aborts before deleting"
	, res$status != 0, identical(datasets_left(fx), "old.csv"))

##### ---- A junk timestamp is never acted on ---------------------------####

fx = make_root("junk.csv", "01-01-2024 09:00:00junk")
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("an unreadable upload time keeps the dataset"
	, identical(datasets_left(fx), "junk.csv"))
check("and is reported rather than ignored", grepl("unreadable", res$output))
## Data that can never expire is not a clean state, so the run has to say so
check("and the run does not exit clean", res$status != 0)

##### ---- Recovering from a crashed run --------------------------------####

## The previous run wrote its intent, deleted the files, then died before it
## could save the summary or close the audit trail. This run has to tidy the
## row away and close that intent - without opening a second one.
fx = make_root(character(0), character(0)
	, summary_rows = data.frame(file_name = "old.csv", upload_time = old
		, stringsAsFactors = FALSE))
con = DBI::dbConnect(RSQLite::SQLite(), fx$db)
invisible(DBI::dbExecute(con,
	"INSERT INTO users_activity (username, action, timestamp) VALUES (?, ?, ?)"
	, params = list("tester@aphrc.org", "retention: deleting dataset old.csv"
		, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
DBI::dbDisconnect(con)

res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
summary_after = paste(readLines(file.path(fx$user, ".log_files"
	, ".automl-shiny-upload.main.log")), collapse = " ")
check("the leftover row is tidied away", !grepl("old.csv", summary_after))
rows = audit_rows(fx)
check("the crashed run's intent is closed"
	, any(grepl("reconciled dataset old.csv", rows)))
check("no second intent is opened"
	, sum(grepl("deleting dataset old.csv", rows)) == 1)
check("intents and closing rows balance"
	, sum(grepl("deleting dataset", rows)) ==
		sum(grepl("deleted dataset|reconciled dataset", rows)))

## And with no prior intent at all, tidying up still opens none
fx = make_root(character(0), character(0)
	, summary_rows = data.frame(file_name = "gone.csv", upload_time = old
		, stringsAsFactors = FALSE))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
rows = audit_rows(fx)
check("tidying up records only a reconciliation"
	, sum(grepl("deleting dataset", rows)) == 0
	, sum(grepl("reconciled dataset gone.csv", rows)) == 1)

##### ---- Dataset files nobody is tracking -----------------------------####

## A dataset file with no summary row would be kept for ever without anything
## ever looking at it
fx = make_root("listed.csv", recent)
writeLines("a,b", file.path(fx$user, "datasets", "stray.csv"))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("a stray dataset file is reported", grepl("stray.csv", res$output))
check("and the run does not look clean", res$status != 0)
check("but it is not deleted on the strength of that"
	, "stray.csv" %in% datasets_left(fx))

##### ---- Stray files are found whatever state the summary is in -------####

## A missing summary is the state in which files are most likely to be
## stranded, so the check cannot be limited to a readable one
fx = make_root("orphan.csv", old)
invisible(file.remove(file.path(fx$user, ".log_files", ".automl-shiny-upload.main.log")))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("a stray dataset is reported when the summary is missing"
	, grepl("orphan.csv", res$output))
check("and that run does not look clean", res$status != 0)
check("and it is not deleted", identical(datasets_left(fx), "orphan.csv"))

## Same with a valid but empty summary
fx = make_root("orphan.csv", old)
write.table(data.frame(file_name = character(0), upload_time = character(0))
	, file.path(fx$user, ".log_files", ".automl-shiny-upload.main.log"), row.names = FALSE)
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("a stray dataset is reported when the summary is empty"
	, grepl("orphan.csv", res$output), res$status != 0)

## A recent upload still in flight has not reached the configured retention
## period and is never touched.
fx = make_root("listed.csv", recent)
recent_staging = file.path(fx$user, "datasets", ".incoming-half.csv")
writeLines("a,b", recent_staging)
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3")
check("an upload in flight is not disturbed"
	, file.exists(recent_staging), !grepl("expired upload staging", res$output))

## Staging contains the same uploaded data, so an old one follows the same
## retention period as a completed upload. A durable audit authorization is
## written before removal.
fx = make_root("listed.csv", recent)
stale = file.path(fx$user, "datasets", ".incoming-crashed.csv")
writeLines("a,b", stale)
Sys.setFileTime(stale, as.POSIXct("2020-01-01 09:00:00"))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("an expired staging file is reported"
	, grepl("expired upload staging file", res$output))
check("and is deleted only after retention", !file.exists(stale))
check("its deletion was authorized in the audit table"
	, any(grepl("authorized deletion of expired staging file", audit_rows(fx))))
check("a successful staging cleanup exits cleanly", res$status == 0)

## Dry run applies the same decision but removes and audits nothing.
fx = make_root("listed.csv", recent)
stale = file.path(fx$user, "datasets", ".incoming-crashed.csv")
writeLines("a,b", stale)
Sys.setFileTime(stale, as.POSIXct("2020-01-01 09:00:00"))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3")
check("dry run reports expired staging without deleting it"
	, grepl("expired upload staging file", res$output), file.exists(stale)
	, length(audit_rows(fx)) == 0)

## The staging path uses the same canonical containment rule as normal dataset
## deletion; a symlinked datasets directory is refused.
fx = make_root("listed.csv", recent)
outside = file.path(tempdir(), paste0("outside-", as.integer(runif(1, 1, 1e9))))
dir.create(outside)
outside_staging = file.path(outside, ".incoming-sensitive.csv")
writeLines("private", outside_staging)
Sys.setFileTime(outside_staging, as.POSIXct("2020-01-01 09:00:00"))
unlink(file.path(fx$user, "datasets"), recursive = TRUE)
stopifnot(file.symlink(outside, file.path(fx$user, "datasets")))
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
check("a symlinked staging directory is refused"
	, file.exists(outside_staging), res$status != 0
	, grepl("outside the user directory", res$output))

##### ---- Resuming a run that died halfway -----------------------------####

## The crash left one file deleted, one still there, and an open intent. This
## run finishes the job without opening a second intent.
fx = make_root("old.csv", old)
invisible(file.remove(file.path(fx$user, "datasets", "old.csv")))
con = DBI::dbConnect(RSQLite::SQLite(), fx$db)
invisible(DBI::dbExecute(con,
	"INSERT INTO users_activity (username, action, timestamp) VALUES (?, ?, ?)"
	, params = list("tester@aphrc.org", "retention: deleting dataset old.csv"
		, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
DBI::dbDisconnect(con)
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
rows = audit_rows(fx)
check("the half-deleted dataset is finished off"
	, !file.exists(file.path(fx$user, ".log_files", "old.csv-upload.main.log")))
check("no second intent is opened for it"
	, sum(grepl("deleting dataset old.csv", rows)) == 1)
check("intents and closings still balance"
	, sum(grepl("deleting dataset", rows)) ==
		sum(grepl("deleted dataset|reconciled dataset", rows)))

##### ---- The row survives a closing audit that cannot be written ------####

## Dropping the whole table would stop the intent too, so the deletion would
## never start and this would prove nothing. A trigger lets the intent through
## and rejects only the closing row, which is the case that matters: the files
## are gone and the summary row must stay so the next run can record it.
fx = make_root("old.csv", old)
con = DBI::dbConnect(RSQLite::SQLite(), fx$db)
invisible(DBI::dbExecute(con, "
	CREATE TRIGGER reject_closing BEFORE INSERT ON users_activity
	WHEN NEW.action LIKE 'retention: deleted dataset%'
		OR NEW.action LIKE 'retention: reconciled dataset%'
	BEGIN SELECT RAISE(ABORT, 'closing row rejected'); END;"))
DBI::dbDisconnect(con)
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
rows = audit_rows(fx)
check("the intent was allowed through"
	, any(grepl("deleting dataset old.csv", rows)))
check("no closing row could be written"
	, !any(grepl("deleted dataset old.csv|reconciled dataset old.csv", rows)))
check("the files are gone, as the intent said they would be"
	, !file.exists(file.path(fx$user, "datasets", "old.csv")))
check("but the summary row is kept for another attempt"
	, grepl("old.csv", paste(readLines(file.path(fx$user
		, ".log_files", ".automl-shiny-upload.main.log")), collapse = " ")))
check("and the run reports it", res$status != 0)

## Running again with the trigger gone finishes the job: it reconciles the
## still-open intent rather than opening a second one
con = DBI::dbConnect(RSQLite::SQLite(), fx$db)
invisible(DBI::dbExecute(con, "DROP TRIGGER reject_closing"))
DBI::dbDisconnect(con)
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
rows = audit_rows(fx)
check("the retry closes the open intent"
	, any(grepl("reconciled dataset old.csv", rows)))
check("without opening a second one"
	, sum(grepl("deleting dataset old.csv", rows)) == 1)
check("and the row is finally pruned"
	, !grepl("old.csv", paste(readLines(file.path(fx$user
		, ".log_files", ".automl-shiny-upload.main.log")), collapse = " ")))

##### ---- A held lock is not a silent skip -----------------------------####

fx = make_root("old.csv", old)
lock_path = file.path(fx$user, ".log_files", ".delete.lock")
holder_pid = file.path(tempdir(), paste0("wpid-", as.integer(runif(1, 1, 1e9))))
holder = tempfile(fileext = ".R")
writeLines(c(
	sprintf('lock <- filelock::lock("%s", exclusive = TRUE)', lock_path)
	, sprintf('writeLines(as.character(Sys.getpid()), "%s")', holder_pid)
	, 'Sys.sleep(30)'
), holder)
system2("Rscript", holder, wait = FALSE, stdout = NULL, stderr = NULL)
for (i in 1:50) {
	if (file.exists(holder_pid)) break
	Sys.sleep(0.2)
}
res = run_worker(fx, retention_policy_start = "2020-01-01", data_retention_months = "3"
	, retention_dry_run = "false")
tools::pskill(as.integer(readLines(holder_pid, warn = FALSE)[1]))
check("a locked folder is skipped, not deleted", identical(datasets_left(fx), "old.csv"))
check("and the skip is reported", grepl("in progress", res$output))

cat("\n", checks, "checks passed\n")
