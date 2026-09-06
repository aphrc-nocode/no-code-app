#!/usr/bin/env Rscript

##### ---- Tests for the shared dataset deletion helpers -----------------####
## Plain stopifnot(), no test package. Run from the app folder with:
##   Rscript tests/test_delete_data_helpers.R

source("server/delete_data_helpers.R")

checks = 0
check = function(label, ...) {
	stopifnot(...)
	checks <<- checks + 1
	cat("ok -", label, "\n")
}

## Builds a throwaway user folder holding one dataset and its upload log
make_user = function(file_name = "mydata.csv") {
	root = file.path(tempdir(), paste0("user-", as.integer(runif(1, 1, 1e9))))
	dir.create(file.path(root, "datasets"), recursive = TRUE)
	dir.create(file.path(root, ".log_files"), recursive = TRUE)
	writeLines("a,b", file.path(root, "datasets", file_name))
	writeLines("meta", file.path(root, ".log_files", paste0(file_name, "-upload.main.log")))
	root
}

##### ---- Path segment validation --------------------------------------####

check("plain file name is accepted", valid_path_segment("mydata.csv"))
check("dots inside a name are fine", valid_path_segment("report..v2.csv"))
check("traversal is rejected", !valid_path_segment("../evil.csv"))
check("forward slash is rejected", !valid_path_segment("a/b.csv"))
check("backslash is rejected", !valid_path_segment("a\\b.csv"))
check("bare dot and double dot are rejected"
	, !valid_path_segment("."), !valid_path_segment(".."))
check("empty and missing are rejected"
	, !valid_path_segment(""), !valid_path_segment(NA), !valid_path_segment(NULL))

##### ---- Paths stay inside the user folder ----------------------------####

root = make_user()
paths = dataset_paths(root, "mydata.csv")
check("paths are resolved for a valid id", !is.null(paths), length(paths) == 2)
check("a traversal id yields no paths", is.null(dataset_paths(root, "../mydata.csv")))

## A symlinked datasets/ pointing out of the user folder must not be followed:
## deletion would otherwise remove somebody else's file
sym_base = file.path(tempdir(), paste0("sym-", as.integer(runif(1, 1, 1e9))))
outside = file.path(sym_base, "OUTSIDE")
sym_user = file.path(sym_base, "user")
dir.create(outside, recursive = TRUE)
dir.create(file.path(sym_user, ".log_files"), recursive = TRUE)
writeLines("precious", file.path(outside, "victim.csv"))
## Symlink support is not universal, so the test asserts it was actually made
stopifnot(file.symlink(outside, file.path(sym_user, "datasets")))
check("a symlinked datasets folder is refused"
	, is.null(dataset_paths(sym_user, "victim.csv")))
result = delete_dataset(sym_user, "victim.csv")
check("nothing outside the user folder is deleted"
	, file.exists(file.path(outside, "victim.csv")), !deletion_succeeded(result))

##### ---- Calendar-safe month arithmetic -------------------------------####

fmt = function(x) format(x, "%Y-%m-%d")
check("month end clamps instead of overflowing"
	, fmt(add_months(as.POSIXct("2026-01-31"), 1)) == "2026-02-28"
	, fmt(add_months(as.POSIXct("2026-05-31"), -3)) == "2026-02-28")
check("leap year is respected"
	, fmt(add_months(as.POSIXct("2028-01-31"), 1)) == "2028-02-29")
check("ordinary months are unchanged"
	, fmt(add_months(as.POSIXct("2026-03-01"), 3)) == "2026-06-01"
	, fmt(add_months(as.POSIXct("2026-12-15"), 1)) == "2027-01-15")
check("december rolls into the next year"
	, fmt(add_months(as.POSIXct("2026-12-31"), 2)) == "2027-02-28")

##### ---- Dry run touches nothing --------------------------------------####

root = make_user()
result = delete_dataset(root, "mydata.csv", dry_run = TRUE)
check("dry run reports both files", NROW(result) == 2, all(result$existed))
check("dry run deletes nothing", !any(result$deleted), all(result$reason == "dry run"))
check("dry run is not a success", !deletion_succeeded(result))
check("files are still on disk", file.exists(file.path(root, "datasets", "mydata.csv")))

##### ---- Real deletion ------------------------------------------------####

root = make_user()
result = delete_dataset(root, "mydata.csv")
check("both files are deleted", all(result$deleted), deletion_succeeded(result))
check("dataset is gone", !file.exists(file.path(root, "datasets", "mydata.csv")))
check("upload log is gone"
	, !file.exists(file.path(root, ".log_files", "mydata.csv-upload.main.log")))

## Deleting something that is not there is reported, not treated as success
root = make_user()
result = delete_dataset(root, "missing.csv")
check("absent files are reported as not found"
	, all(result$reason == "not found"), !deletion_succeeded(result))

## An invalid id never reaches the filesystem
result = delete_dataset(root, "../mydata.csv")
check("invalid id is refused"
	, result$type == "dataset_id", !deletion_succeeded(result))

##### ---- Expiry by upload_time ----------------------------------------####

now = as.POSIXct("2026-09-06 12:00:00")
## Policy switched on long ago, so the grace window is over and normal
## retention applies
settled = as.Date("2026-01-01")
logs = data.frame(
	file_name = c("old.csv", "recent.csv", "broken.csv")
	, upload_time = c("01-01-2026 09:00:00", "01-09-2026 09:00:00", "not a date")
	, stringsAsFactors = FALSE
)
found = expired_datasets(logs, retention_months = 3, policy_start = settled, now = now)
check("only the old dataset expires", identical(found$expired, "old.csv"))
check("an unreadable date is reported, never deleted"
	, identical(found$unparsed, "broken.csv"))

## Exactly on the boundary the dataset is kept: the cutoff is strict
boundary = data.frame(file_name = "edge.csv"
	, upload_time = format(seq(now, by = "-3 months", length.out = 2)[2], "%d-%m-%Y %H:%M:%S")
	, stringsAsFactors = FALSE)
check("a dataset exactly at the cutoff is kept"
	, length(expired_datasets(boundary, 3, settled, now)$expired) == 0)

check("empty and malformed logs are handled"
	, length(expired_datasets(NULL, 3, settled, now)$expired) == 0
	, length(expired_datasets(data.frame(), 3, settled, now)$expired) == 0
	, length(expired_datasets(data.frame(x = 1), 3, settled, now)$expired) == 0)

##### ---- Grace period for data that predates the policy ---------------####

## The case that matters: the policy goes live today, and a dataset uploaded
## two years ago must NOT disappear on launch day
launch = as.Date("2026-09-06")
ancient = data.frame(file_name = "ancient.csv", upload_time = "01-01-2024 09:00:00"
	, stringsAsFactors = FALSE)
check("old data survives the day the policy starts"
	, length(expired_datasets(ancient, 3, policy_start = launch, now = now)$expired) == 0)

## Still protected the day before the grace window closes
almost = as.POSIXct("2026-12-05 12:00:00")
check("old data is still kept just before grace ends"
	, length(expired_datasets(ancient, 3, policy_start = launch, now = almost)$expired) == 0)

## And removed once the grace period has run out
after = as.POSIXct("2026-12-07 12:00:00")
check("old data expires once the grace period ends"
	, identical(expired_datasets(ancient, 3, policy_start = launch, now = after)$expired
		, "ancient.csv"))

## A dataset uploaded after the policy starts still follows normal retention,
## and is not held back by the grace window ending later
fresh = data.frame(file_name = "fresh.csv", upload_time = "07-09-2026 09:00:00"
	, stringsAsFactors = FALSE)
check("a new dataset is not expired early"
	, length(expired_datasets(fresh, 3, policy_start = launch
		, now = as.POSIXct("2026-11-01 12:00:00"))$expired) == 0)
check("a new dataset expires on its own schedule"
	, identical(expired_datasets(fresh, 3, policy_start = launch
		, now = as.POSIXct("2026-12-08 12:00:00"))$expired, "fresh.csv"))

## Without an effective date nothing is ever selected for deletion
check("no policy start means nothing expires"
	, length(expired_datasets(logs, 3, policy_start = NA, now = now)$expired) == 0
	, length(expired_datasets(logs, 3, policy_start = NULL, now = now)$expired) == 0)

## The grace floor is only for data that predates the policy. A dataset
## uploaded afterwards keeps its own schedule even when grace runs longer.
post = data.frame(file_name = "post.csv", upload_time = "02-01-2026 09:00:00"
	, stringsAsFactors = FALSE)
check("a post-policy upload is not held back by a longer grace"
	, identical(expired_datasets(post, retention_months = 3
		, policy_start = as.Date("2026-01-01"), now = as.POSIXct("2026-05-01 12:00:00")
		, grace_months = 6)$expired, "post.csv"))

##### ---- No deletion on a nonsense retention period -------------------####

## The bug that mattered: three months from 1 March ends on 1 June, so nothing
## may be deleted on 31 May
march = data.frame(file_name = "mar1.csv", upload_time = "01-03-2026 09:00:00"
	, stringsAsFactors = FALSE)
check("a 1 March upload is not expired on 31 May"
	, length(expired_datasets(march, 3, settled, as.POSIXct("2026-05-31 12:00:00"))$expired) == 0)
check("a 1 March upload is expired on 2 June"
	, identical(expired_datasets(march, 3, settled
		, as.POSIXct("2026-06-02 12:00:00"))$expired, "mar1.csv"))

brand_new = data.frame(file_name = "new.csv"
	, upload_time = format(Sys.time() - 60, "%d-%m-%Y %H:%M:%S"), stringsAsFactors = FALSE)
check("zero months deletes nothing"
	, length(expired_datasets(brand_new, 0, settled)$expired) == 0)
check("a negative period deletes nothing and does not error"
	, length(expired_datasets(brand_new, -5, settled)$expired) == 0)
check("a fractional or absurd period deletes nothing"
	, length(expired_datasets(brand_new, 1.5, settled)$expired) == 0
	, length(expired_datasets(brand_new, 99999, settled)$expired) == 0)
check("period bounds are rejected up front"
	, !valid_months(0), !valid_months(-1), !valid_months(1.5)
	, !valid_months(NA), !valid_months("abc"), !valid_months(99999)
	, valid_months(3), valid_months("3"))

##### ---- Locking ------------------------------------------------------####

root = make_user()
lock_path = file.path(root, ".log_files", ".delete.lock")

check("the lock runs the body and returns its value"
	, identical(with_upload_lock(root, "ran"), "ran"))
check("the lock is free again afterwards"
	, identical(with_upload_lock(root, "ran again"), "ran again"))

## Held by another process: this is the case that matters, because the app and
## the cleanup worker are separate processes. A background Rscript is used so
## the tests need nothing beyond the packages the app already declares.
pid_file = file.path(tempdir(), paste0("holder-", as.integer(runif(1, 1, 1e9)), ".pid"))
holder_script = tempfile(fileext = ".R")
## The pid file is written only after the lock is held, so waiting for it means
## waiting for the lock rather than merely for the process to start
writeLines(c(
	sprintf('lock <- filelock::lock("%s", exclusive = TRUE)', lock_path)
	, sprintf('writeLines(as.character(Sys.getpid()), "%s")', pid_file)
	, 'Sys.sleep(30)'
), holder_script)
system2("Rscript", holder_script, wait = FALSE, stdout = NULL, stderr = NULL)
## The pid file appears only once the lock is actually held
for (i in 1:50) {
	if (file.exists(pid_file)) break
	Sys.sleep(0.2)
}
check("the holder took the lock", file.exists(pid_file))
check("a lock held by another process blocks this one"
	, is.null(with_upload_lock(root, "should not run")))

## filelock releases on process exit, crash included, so nothing is left behind.
## tools::pskill() rather than the kill command, which is POSIX only.
holder_pid = as.integer(readLines(pid_file, warn = FALSE)[1])
tools::pskill(holder_pid)
for (i in 1:50) {
	if (!is.null(with_upload_lock(root, TRUE))) break
	Sys.sleep(0.2)
}
check("a killed holder's lock is released automatically"
	, identical(with_upload_lock(root, "ran after crash"), "ran after crash"))

##### ---- Summary rewrite ----------------------------------------------####

root = make_user()
logs = data.frame(file_name = "mydata.csv", upload_time = "01-01-2026 09:00:00"
	, stringsAsFactors = FALSE)
check("a successful write reports TRUE", isTRUE(write_upload_summary(root, logs)))
summary_file = file.path(root, ".log_files", ".automl-shiny-upload.main.log")
check("summary is written", file.exists(summary_file))
check("no temp file is left behind"
	, length(list.files(file.path(root, ".log_files"), pattern = "^\\.upload-summary-"
		, all.files = TRUE)) == 0)
check("summary reads back"
	, identical(read_upload_summary(root)$status, "ok")
	, identical(as.character(read_upload_summary(root)$logs$file_name), "mydata.csv"))

## A write that cannot land must say so rather than report success
unwritable = make_user()
unlink(file.path(unwritable, ".log_files"), recursive = TRUE)
check("a failed write reports FALSE", !isTRUE(write_upload_summary(unwritable, logs)))

##### ---- The summary states are told apart ----------------------------####

check("a missing summary is 'missing', not an error"
	, identical(read_upload_summary(make_user())$status, "missing"))

## The last dataset has been deleted: a zero-row summary is valid, not corrupt
emptied = make_user()
invisible(write_upload_summary(emptied, logs[0, ]))
check("an empty summary is 'empty', not corrupt"
	, identical(read_upload_summary(emptied)$status, "empty"))

corrupt = make_user()
writeLines("this is not a table", file.path(corrupt, ".log_files"
	, ".automl-shiny-upload.main.log"))
check("an unreadable summary is 'corrupt'"
	, identical(read_upload_summary(corrupt)$status, "corrupt"))

##### ---- A half-finished deletion can be repaired ---------------------####

## Files deleted but the summary rewrite failed: running again has to be able
## to finish the job, so the row can finally be pruned
half = make_user()
invisible(delete_dataset(half, "mydata.csv"))
retry = delete_dataset(half, "mydata.csv")
check("a retry over already-deleted files counts as complete"
	, deletion_complete(retry))
check("but is not reported as a fresh deletion", !deletion_succeeded(retry))
check("a dry run is never complete"
	, !deletion_complete(delete_dataset(make_user(), "mydata.csv", dry_run = TRUE)))

##### ---- The temp file cannot be hijacked -----------------------------####

## A fixed temp name could be pre-created as a symlink and made to overwrite a
## file elsewhere. The randomised name inside the verified folder prevents it.
hijack = make_user()
target = file.path(tempdir(), paste0("outside-", as.integer(runif(1, 1, 1e9)), ".txt"))
writeLines("original", target)
stopifnot(file.symlink(target, file.path(hijack, ".log_files"
	, ".automl-shiny-upload.main.log.tmp")))
invisible(write_upload_summary(hijack, logs))
check("an external file is not overwritten through a planted temp name"
	, identical(readLines(target, warn = FALSE)[1], "original"))

## A symlinked .log_files must not place the lock or the summary outside either
sym_logs_base = file.path(tempdir(), paste0("lg-", as.integer(runif(1, 1, 1e9))))
sym_logs_out = file.path(sym_logs_base, "OUTSIDE")
sym_logs_user = file.path(sym_logs_base, "user")
dir.create(sym_logs_out, recursive = TRUE)
dir.create(file.path(sym_logs_user, "datasets"), recursive = TRUE)
stopifnot(file.symlink(sym_logs_out, file.path(sym_logs_user, ".log_files")))
check("a symlinked .log_files is refused"
	, is.null(upload_summary_path(sym_logs_user))
	, is.null(with_upload_lock(sym_logs_user, "should not run"))
	, !isTRUE(write_upload_summary(sym_logs_user, logs)))
check("and no lock file is created outside"
	, !file.exists(file.path(sym_logs_out, ".delete.lock")))

##### ---- Missing names never reach a destructive path -----------------####

## A NA in the column used to make "sum(file_name == x) != 1" evaluate to NA,
## which stops with an error partway through a deletion
na_logs = data.frame(file_name = c(NA, "a.csv"), upload_time = "01-01-2024 09:00:00"
	, stringsAsFactors = FALSE)
check("a NA name does not break row matching"
	, identical(find_dataset_row(na_logs, "a.csv"), 2L))
check("a name that is not there gives NA, not an error"
	, is.na(find_dataset_row(na_logs, "nope.csv")))
check("a duplicated name gives NA"
	, is.na(find_dataset_row(data.frame(file_name = c("d.csv", "d.csv")
		, upload_time = "01-01-2024 09:00:00", stringsAsFactors = FALSE), "d.csv")))
check("an unusable id gives NA", is.na(find_dataset_row(na_logs, "../a.csv")))
check("rows with no name are never selected for deletion"
	, !any(is.na(expired_datasets(na_logs, 3, settled, now)$expired)))

##### ---- Policy dates are exact ---------------------------------------####

check("a real date is accepted", valid_policy_date("2026-09-01"))
check("trailing rubbish is rejected", !valid_policy_date("2026-09-01junk"))
check("a loose date is rejected", !valid_policy_date("2026-9-1"))
check("an impossible date is rejected", !valid_policy_date("2026-02-31"))
check("empty and missing are rejected"
	, !valid_policy_date(""), !valid_policy_date(NA), !valid_policy_date(NULL))

##### ---- A summary must carry both columns ----------------------------####

no_time = make_user()
write.table(data.frame(file_name = "a.csv"), file.path(no_time, ".log_files"
	, ".automl-shiny-upload.main.log"), row.names = FALSE)
check("a summary without upload_time is corrupt, not usable"
	, identical(read_upload_summary(no_time)$status, "corrupt"))

##### ---- Audit is required, not queued -------------------------------####

## An earlier version queued audit rows that could not be written. The queue
## could be redirected by a symlink, and an unreadable queue was treated as an
## empty one and deleted. The rule now is simply that an unrecordable deletion
## does not happen, so there is nothing left to protect.
check("a missing database is reported, not ignored"
	, !log_deletion("someone", "test", db = file.path(tempdir(), "no-such.sqlite")))
check("the queue helpers are gone"
	, !exists("queue_audit"), !exists("flush_audit_queue"), !exists("audit_or_queue"))

##### ---- Upload timestamps are exact ----------------------------------####

check("a real timestamp parses"
	, !is.na(parse_upload_time("01-01-2024 09:00:00")))
check("trailing rubbish is rejected"
	, is.na(parse_upload_time("01-01-2024 09:00:00junk")))
check("a loose timestamp is rejected"
	, is.na(parse_upload_time("1-1-2024 9:00:00")))
check("an impossible date is rejected"
	, is.na(parse_upload_time("31-02-2024 09:00:00")))
check("a date with no time is rejected", is.na(parse_upload_time("01-01-2024")))
check("empty and missing are rejected"
	, is.na(parse_upload_time("")), is.na(parse_upload_time(NA)))
check("a junk timestamp is never selected for deletion"
	, length(expired_datasets(data.frame(file_name = "j.csv"
		, upload_time = "01-01-2024 09:00:00junk", stringsAsFactors = FALSE)
		, 3, settled, now)$expired) == 0)

##### ---- The lock file cannot be redirected ---------------------------####

## A symlink at .delete.lock - dangling or not - would put the lock file
## wherever it points
lk_root = make_user()
lk_outside = file.path(tempdir(), paste0("lock-out-", as.integer(runif(1, 1, 1e9))))
stopifnot(file.symlink(lk_outside, file.path(lk_root, ".log_files", ".delete.lock")))
check("a symlinked lock path is refused"
	, is.null(with_upload_lock(lk_root, "should not run")))
check("and no lock file is created where it pointed", !file.exists(lk_outside))

cat("\n", checks, "checks passed\n")
