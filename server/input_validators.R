#### --- Setup ----------------------------------------------------------####
# Create an InputValidator object
iv = InputValidator$new()
iv_url = InputValidator$new()

##### ---- UI validation -------------------------------------------------####
iv$add_rule("study_name", sv_required())
iv$add_rule("study_country", sv_required())
iv$add_rule("files_with_ext", sv_required())
iv$add_rule("additional_info", sv_required())
iv_url$add_rule("url_upload", sv_url())
iv_url$add_rule("study_name", sv_required())
iv_url$add_rule("study_country", sv_required())
