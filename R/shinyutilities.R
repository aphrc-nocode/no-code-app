library(dplyr)

## Labelling files
labelling_file = readxl::read_excel("static_files/labelling_file.xlsx", sheet="ui_labels")

## Language choices
language_choices = labelling_file$input_language
language_labels = labelling_file$language_label

## Input choices
input_choices_file = readxl::read_excel("static_files/labelling_file.xlsx", sheet="choices")

## Supported files
supported_files = readxl::read_excel("static_files/labelling_file.xlsx", sheet="supported_files") |> pull()

## Recode variable types
recode_var_types = readxl::read_excel("static_files/labelling_file.xlsx", sheet="recode_var_types") |> pull()


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

