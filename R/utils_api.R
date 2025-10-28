.api_base <- function(rv_ml_ai) { if (!is.null(rv_ml_ai$api_base)) rv_ml_ai$api_base else Sys.getenv("API_BASE", unset = "http://127.0.0.1:8000") }

.api_post_json <- function(url, body, timeout_sec = 600) {
  httr::RETRY("POST", url,
              body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
              httr::content_type_json(), httr::accept_json(),
              times = 3, pause_min = 1, terminate_on = c(400,401,403,404),
              timeout(timeout_sec)
  )
}

.api_post_multipart <- function(url, fields = list(), files = list(), timeout_sec = 1200) {
  form <- list()
  if (length(fields)) for (nm in names(fields)) form[[nm]] <- fields[[nm]]
  if (length(files))  for (nm in names(files))  form[[nm]] <- httr::upload_file(files[[nm]])
  httr::RETRY("POST", url, body = form,
              times = 3, pause_min = 1, terminate_on = c(400,401,403,404),
              timeout(timeout_sec)
  )
}

.safe_content <- function(resp) { tryCatch(httr::content(resp, as="text", encoding="UTF-8"), error = function(e) "") }
.json_or_list <- function(txt) { if (!nzchar(txt)) return(list()); tryCatch(jsonlite::fromJSON(txt, simplifyVector=TRUE), error=function(e) list(raw=txt)) }
.coalesce <- function(...) { x <- list(...); for (v in x) if (!is.null(v)) return(v); NULL }
