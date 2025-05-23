library(dplyr)
library(gsheet)

## Labelling files

## local_path = "static_files/labelling_file.xlsx"

## labelling_file = readxl::read_excel(local_path, sheet="ui_labels")
labelling_file = gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1i4QXJ4tC5efgzV7H0pgUL-7XI6ZKjqu8/edit?gid=1571856299#gid=1571856299", sheetid="ui_labels")

## Language choices
language_choices = labelling_file$input_language
language_labels = labelling_file$language_label

## Input choices
## input_choices_file = readxl::read_excel(local_path, sheet="choices")
input_choices_file = gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1i4QXJ4tC5efgzV7H0pgUL-7XI6ZKjqu8/edit?gid=897768892#gid=897768892", sheetid="choices")

## Supported files
## supported_files = readxl::read_excel(local_path, sheet="supported_files") |> pull()

supported_files = gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1i4QXJ4tC5efgzV7H0pgUL-7XI6ZKjqu8/edit?gid=841760633#gid=841760633", sheetid="supported_files") |> pull()

## Recode variable types
## recode_var_types = readxl::read_excel("static_files/labelling_file.xlsx", sheet="recode_var_types") |> pull()
recode_var_types = gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1i4QXJ4tC5efgzV7H0pgUL-7XI6ZKjqu8/edit?gid=1626004338#gid=1626004338", sheetid="recode_var_types") |> pull()

prompts_df___ = gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1i4QXJ4tC5efgzV7H0pgUL-7XI6ZKjqu8/edit?gid=558607190#gid=558607190", sheetid="prompts")

### Get named vector, based on input language
get_named_choices = function(df , lang, var) {
   dd = (df
      |> filter(variable==var)
   )
   base_names = (dd
      |> select(label)
      |> pull()
   )
   lang_names = (dd
      |> select(all_of(lang))
      |> pull()
   )
   labs = setNames(base_names, lang_names)
   return(labs)
}

## Get prompts from the prompts file
get_prompts = function(var) {
	pp  = (prompts_df___
      |> dplyr::filter(variable %in% var)
		|> dplyr::pull(prompt)
   )
	pp = paste0(pp, collapse=". ")
	return(pp)
}


### Get labels from labelling file
get_rv_labels_base = function(df, var) {
	df[[var]]
}


## Supported data types
datatypes = c("factor", "character", "numeric")

modal_confirm <- modalDialog(
  "",
  title = "Data submitted successfully",
  footer = tagList(
    actionButton("upload_ok", "Ok", class = "btn btn-success")
  )
  , size = "l"
)


create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
    paste0(
      '<div class = "btn-group"> <button class="btn btn-default action-button btn-danger action_button" id="ytxxdeletezzyt_',
      .x, '" type="button" onclick=get_id(this.id)><i class="glyphicon glyphicon-trash"></i></button></div>'
    ))
}

## Alter content of renderUI

alter_renderUI = function(ui, session, pattern = "<hr.*", replacement="") {
	ui = ui(shinysession=session)
	ht = ui$html
	ht = gsub(pattern, replacement, ht)
	deps = ui$deps
	ff = function(...) {
		list(html=ht, deps=deps)
	}
	return(ff)
}

## Format print output of renamed and changed type vars
transfun = function(var, old, new) {
	paste0("{", var, ": ", old, " ----> ", new, "}")
}


## Handle missing values UI hack

### Numeric values
transform_data_handle_missing_values_numericUI = function(label, value) {
	transform_data_handle_missing_values_new_numeric = renderUI({
		 numericInput("transform_data_handle_missing_values_new_numeric"
			, label = label # get_rv_labels("transform_data_handle_missing_values_new_numeric")
			, value = value # get_rv_labels("transform_data_handle_missing_values_new_numeric_ph")
			, width = "100%"
		 )
	})
	return(transform_data_handle_missing_values_new_numeric)
}

### Categorical values
transform_data_handle_missing_values_categoricalUI = function(label, value="", placeholder) {
	transform_data_handle_missing_values_new_category = renderUI({
		 textInput("transform_data_handle_missing_values_new_category"
			, label = label # get_rv_labels("transform_data_handle_missing_values_new_category")
			, value=value # ""
			, placeholder = placeholder # get_rv_labels("transform_data_handle_missing_values_new_category_ph")
			, width = "100%"
		 )
	})
	return(transform_data_handle_missing_values_new_category)
}

