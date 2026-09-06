
##### ----- Detete uploaded datasets ------------------------------------#####

get_delete_label = function(key) {
	as.character(get_rv_labels(key)[1])
}

## Keep the filename as a real HTML tag rather than interpolating markup into
## text. That both renders correctly and lets htmltools escape unusual names.
delete_confirm_message_ui = function(file_name) {
	message = get_delete_label("delete_confirm_message")
	token = "{dataset}"
	position = regexpr(token, message, fixed = TRUE)[1]

	if (position < 0) {
		return(tagList(
			div(class = "delete-confirm-file", tags$strong(file_name))
			, p(class = "delete-confirm-message", message)
		))
	}

	before = if (position > 1) substr(message, 1, position - 1) else ""
	after_start = position + nchar(token)
	after = if (after_start <= nchar(message)) {
		substr(message, after_start, nchar(message))
	} else {
		""
	}

	tagList(
		if (nzchar(trimws(before))) p(class = "delete-confirm-message", trimws(before))
		, div(class = "delete-confirm-file", tags$strong(file_name))
		, if (nzchar(trimws(after))) p(class = "delete-confirm-message", trimws(after))
	)
}

## Same shape as the country and data privacy dialogs: a modalDialog with the
## action on the right. Deleting is irreversible, so it is asked for rather
## than done straight off the trash icon.
delete_confirm_modal_ui = function(file_name) {
	modalDialog(
		title = get_delete_label("delete_confirm_title")
		, footer = tagList(
			actionButton("delete_cancel_btn", get_delete_label("delete_confirm_cancel")
				, class = "btn-default")
			, actionButton("delete_confirm_btn", get_delete_label("delete_confirm_apply")
				, class = "btn-danger")
		)
		, size = "m"
		, easyClose = FALSE
		, div(class = "delete-confirm-body", delete_confirm_message_ui(file_name))
	)
}

delete_uploaded_data_server = function() {

	## The trash icon only asks the question. Nothing is removed until the
	## dialog is confirmed, and the name is held here in between.
	pending_delete = reactiveVal(NULL)

	observeEvent(input$current_id, {
	 req(!is.null(input$current_id))
	 ## Anchored: the marker only counts at the start of the id
	 req(grepl("^ytxxdeletezzyt_", input$current_id))
	 file_name = sub("^ytxxdeletezzyt_", "", input$current_id)
	 pending_delete(file_name)
	 showModal(delete_confirm_modal_ui(file_name))
	})

	observeEvent(input$delete_cancel_btn, {
	 pending_delete(NULL)
	 removeModal()
	})

	observeEvent(input$delete_confirm_btn, {
	 file_name = pending_delete()
	 req(!is.null(file_name))
	 pending_delete(NULL)
	 removeModal()
	 rv_current$current_id = file_name
	 ## Everything happens inside the lock: read the summary from disk, check
	 ## the row, delete, rewrite. Reading beforehand would rewrite the summary
	 ## from state that another session or the cleanup worker has moved on from.
	 outcome = with_upload_lock(app_username, {
		summary = read_upload_summary(app_username)
		row = find_dataset_row(summary$logs, file_name)
		if (!identical(summary$status, "ok")) {
			list(ok = FALSE, why = paste("upload summary is", summary$status))
		} else if (is.na(row)) {
			## Exactly one row, or we do not know what we are deleting
			list(ok = FALSE, why = "dataset is not listed exactly once")
		} else if (!open_intent_exists(app_username, file_name, prefix = "") &&
			!log_deletion(app_username, paste0("deleting dataset: ", file_name))) {
			## Recorded before the files are touched. A deletion that cannot be
			## recorded does not happen: the user can try again in a moment.
			## A retry after a completion row failed to save finds its own
			## intent still open and does not add a second one.
			list(ok = FALSE, why = "could not record the deletion")
		} else {
			result = delete_dataset(app_username, file_name)
			remaining = summary$logs[-row, , drop = FALSE]
			if (!deletion_complete(result)) {
				list(ok = FALSE, why = paste(result$reason, collapse = "; "))
			} else if (!log_deletion(app_username, paste0("deleted dataset: ", file_name))) {
				## Files are gone, but the row stays so the next attempt can
				## finish recording it. Dropping it here would leave the
				## deletion permanently unrecorded.
				list(ok = FALSE, why = "could not record the completed deletion")
			} else if (!write_upload_summary(app_username, remaining)) {
				list(ok = FALSE, why = "could not rewrite the upload summary")
			} else {
				list(ok = TRUE, logs = remaining)
			}
		}
	 })
	 if (is.null(outcome)) {
		message(sprintf("Delete skipped for '%s': another delete is in progress", file_name))
		shinyalert::shinyalert("", get_rv_labels("general_error_alert"), type = "error")
		return()
	 }
	 ## The row is dropped only above, on a confirmed deletion. Anything else
	 ## leaves the dataset listed so it can be retried instead of vanishing.
	 if (!isTRUE(outcome$ok)) {
		message(sprintf("Delete failed for '%s': %s", file_name, outcome$why))
		shinyalert::shinyalert("", get_rv_labels("general_error_alert"), type = "error")
		return()
	 }
	 ## Clear the screen first, then hand the table its new rows. Both happen
	 ## here, in the reaction that did the deleting, so nothing depends on the
	 ## order two observers happen to run in: the selector is rebuilt from
	 ## upload_logs after the old selection has already gone.
	 clear_deleted_dataset_view(file_name)
	 rv_metadata$upload_logs = outcome$logs
	 if (isTRUE(!NROW(rv_metadata$upload_logs))) {
		updateCheckboxInput(session, "show_uploaded", value = FALSE)
	 }
	})
}
