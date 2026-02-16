jobs_dir <- file.path(getwd(), ".caret_jobs")
if (!dir.exists(jobs_dir)) {
  dir.create(jobs_dir, recursive = TRUE, showWarnings = FALSE)
}

create_job_id <- function() {
  paste0("caret_", digest::digest(Sys.time(), algo = "md5"))
}

get_job_file <- function(job_id, type = "status") {
  filename <- paste0(job_id, "_", type, ".json")
  file.path(jobs_dir, filename)
}

cleanup_old_jobs <- function(max_age_hours = 1) {
  if (!dir.exists(jobs_dir)) return()
  
  files <- list.files(jobs_dir, pattern = "\\.json$", full.names = TRUE)
  current_time <- Sys.time()
  
  for (file in files) {
    file_time <- file.info(file)$mtime
    age_hours <- as.numeric(difftime(current_time, file_time, units = "hours"))
    
    if (age_hours > max_age_hours) {
      unlink(file)
    }
  }
}

store_job_status <- function(job_id, status, progress = 0, message = "", error = NULL) {
  cleanup_old_jobs()
  
  status_data <- list(
    status = status,
    progress = progress,
    message = message,
    timestamp = as.numeric(Sys.time())
  )
  
  if (!is.null(error)) {
    status_data$error <- as.character(error)
  }
  
  file_path <- get_job_file(job_id, "status")
  write(jsonlite::toJSON(status_data, auto_unbox = TRUE), file_path)
}

get_job_status <- function(job_id) {
  file_path <- get_job_file(job_id, "status")
  
  if (!file.exists(file_path)) {
    return(list(status = "not_found", progress = 0, message = "Job not found"))
  }
  
  tryCatch({
    data <- readLines(file_path, warn = FALSE)
    jsonlite::fromJSON(paste(data, collapse = "\n"))
  }, error = function(e) {
    list(status = "error", progress = 0, message = paste("Error reading job status:", e$message))
  })
}

store_job_result <- function(job_id, result) {
  cleanup_old_jobs()
  
  file_path <- get_job_file(job_id, "result")
  write(jsonlite::toJSON(result, auto_unbox = TRUE), file_path)
}

get_job_result <- function(job_id) {
  file_path <- get_job_file(job_id, "result")
  
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  tryCatch({
    data <- readLines(file_path, warn = FALSE)
    jsonlite::fromJSON(paste(data, collapse = "\n"))
  }, error = function(e) {
    return(NULL)
  })
}
