#' Create directory for various artifacts
#'
#' @details Checks if the directory exists otherwise creates it.
#'
#' @param name a directory name or relative path.
#'
#' @return NULL
#'
#' @export

create_dir = function(name="new_folder") {
	dir.create(name, showWarnings = FALSE)
}


#'  Get file extension 
#'  
#' @details Given a file name/path, extract the file extension.
#'
#' @param file file name or file path with a .ext.
#'
#' @return a character specifying file extension.
#'
#' @export
#'

get_file_ext = function(file) {
	(strsplit(basename(file), split="\\.(?=[^\\.]+$)", perl = TRUE)[[1]])[2]
}

#' Get the file name without the extension 
#'
#' @details Given a file name/path, extract the file extension.
#'
#' @param file file name or file path with a .ext.
#'
#' @return a character specifying the file name.
#'
#' @export
#'

get_file_name = function(file) {
	x = (strsplit(basename(file), split="\\.(?=[^\\.]+$)", perl = TRUE)[[1]])[1]
	gsub("\\.+$", "", x)
}

#' Create dataset metadata
#'
#' Create a file with the information about the data.
#'
#' @param data uploaded dataset.
#' @param filename a character string. Can be file name of the data or a file path, with the correct file extension.
#' @param study_name a short description about the study or analysis.
#' @param study_country Country where the study was conducted or where the data was collected from.
#' @param upload_time timestamp indicating when the data was uploaded.
#' @param user user accessing the platform
#' @param group collaborating groups which have access to the data.
#'
#' @details The function collects user provided information, together with the system generate to create a metadata file.
#'
#' @return a dataframe.
#'
#' @export
#'

create_df_metadata = function(data, filename, study_name, study_country, additional_info, upload_time = Sys.time(), last_modified = upload_time, user="Admin", groups="admin") {
	file_metadata = list()
	file_metadata$user = user
	file_metadata$study_name = study_name
	file_metadata$study_country = paste0(study_country, collapse=", ")
	file_metadata$additional_info = additional_info
	file_metadata$file_name = filename
	file_metadata$size = object.size(data)
	file_metadata$observations = NROW(data)
	file_metadata$features = NCOL(data)
	file_metadata$upload_time = upload_time
	file_metadata$last_modified = last_modified
	file_metadata$groups = groups
	file_metadata = cbind.data.frame(file_metadata)
	return(file_metadata)
}

#' Collect log files 
#'
#' Create a data frame of log files based on a particular pattern.
#'
#' @details Collect all log files and create a dataframe.
#'
#' @param path path to the files.
#' @param pattern search pattern.
#'
#' @return dataframe object.
#'
#' @export

collect_logs = function(path, pattern="*.log$") {
	logs = list.files(path = path, pattern=pattern, full.names=TRUE)
	logs = sapply(logs, read.csv, simplify=FALSE)
	logs = do.call("rbind", logs)
	return(logs)
}

#' Get data class
#'
#' Assigns a class to the upload path or url based on the file extension.
#'
#' @param path data path or url.
#'
#' @return A character string specifying the class of the data.
#'
#' @export

get_data_class = function(path) {
	class(path) = get_file_ext(path)
	if (class(path) %in% c("sav", "por")) {
		class(path) = "spss"
	}
	return(path)
}

#' Upload csv data
#'
#' Uploads csv data
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @importFrom readr read_csv
#'
#' @rdname upload_data
#'
#' @export

upload_data.csv = function(path) {
	df = read_csv(path, show_col_types = FALSE)
	return(df)
}

#' Upload Excel data
#'
#' Uploads excel data
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @importFrom openxlsx read.xlsx
#'
#' @rdname upload_data
#'
#' @export

upload_data.xlsx = function(path) {
	df = openxlsx::read.xlsx(path)
	return(df)
}


#' Upload Excel data
#'
#' Uploads excel data
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @importFrom readxl read_xls
#'
#' @rdname upload_data
#'
#' @export

upload_data.xls = function(path) {
	df = readxl::read_xls(path)
	return(df)
}


#' Upload Stata data
#'
#' Uploads Stata data
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @importFrom haven read_dta
#'
#' @rdname upload_data
#'
#' @export

upload_data.dta = function(path) {
	df = read_dta(path)
	return(df)
}

#' Upload rda data
#'
#' Uploads rda data. The rda file should be a dataframe.
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @rdname upload_data
#'
#' @export

upload_data.rda = function(path) {
	df = get(load(path))
	return(df)
}

#' Upload rds data
#'
#' Uploads rds data. The rda file should be a dataframe.
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @rdname upload_data
#'
#' @export

upload_data.rds = function(path) {
	df = readRDS(path)
	return(df)
}


#' Upload Spss data
#'
#' Uploads .sav or .por data.
#'
#' @inheritParams path
#'
#' @return a dataframe.
#'
#' @importFrom haven read_spss
#'
#' @rdname upload_data
#'
#' @export

upload_data.spss = function(path) {
	df = read_spss(path)
	return(df)
}


#' Write csv data
#'
#' Writes csv data
#'
#' @param df data frame
#'
#' @inheritParams path
#'
#' @return NULL.
#'
#' @importFrom readr write_csv
#'
#' @rdname write_data
#'
#' @export

write_data.csv = function(path, df) {
	write_csv(df, file=path)
}


#' Write xlsx data
#'
#' Writes xlsx data
#'
#' @param df data frame
#'
#' @inheritParams path
#'
#' @return NULL.
#'
#' @importFrom openxlsx write.xlsx
#'
#' @rdname write_data
#'
#' @export

write_data.xlsx = function(path, df) {
	write.xlsx(df, file=path)
}


#' Write Stata data
#'
#' Writes Stata data
#'
#' @param df dataframe
#'
#' @inheritParams path
#'
#' @return NULL.
#'
#' @importFrom haven write_dta
#'
#' @rdname write_data
#'
#' @export

write_data.dta = function(path, df) {
	write_dta(df, path=path)
}

#' Write rda data
#'
#' Writes rda data. The rda file should be a dataframe.
#'
#' @param df data frame.
#' @inheritParams path
#'
#' @return NULL.
#'
#' @rdname write_data
#'
#' @export

write_data.rda = function(path, df) {
	save(df, file=path)
}

#' Write rds data
#'
#' Writes rds dataframe.
#'
#' @param df data frame.
#' @inheritParams path
#'
#' @return NULL.
#'
#' @rdname write_data
#'
#' @export

write_data.rds = function(path, df) {
	saveRDS(df, file=path)
}


#' Write Spss data
#'
#' Writes .sav or .por data.
#'
#' @param df data frame.
#' @inheritParams path
#'
#' @return NULL.
#'
#' @importFrom haven write_spss
#'
#' @rdname write_data
#'
#' @export

write_data.spss = function(path, df) {
	write_sav(data=df, path=path)
}

#' Filter data
#'
#' Applies filter patterns to the data.
#'
#' @param df data frame.
#' @param pattern filter pattern.
#'
#' @importFrom dplyr filter
#' @importFrom rlang parse_expr
#'
#' @return data.frame object.
#'
#' @export
#'

filter_data = function(df, pattern) {
	df = (df
		|> dplyr::filter(eval(rlang::parse_expr(pattern)))
	)
	return(df)
}

#' Proportion of missing values
#'
#' @param df input data frame
#'
#' @return data.frame
#'
#' @importFrom dplyr group_by summarise_all arrange rename mutate
#'
#' @export
#'

missing_prop = function(df) {
	df = (df
		|> dplyr::ungroup()
		|> dplyr::summarise_all(~sum(is.na(.)|. %in% c("", " "))/n())
		|> tidyr::pivot_longer(everything())
		|> dplyr::arrange(desc(value))
		|> dplyr::rename("variable"="name", "missing"="value")
		|> dplyr::mutate(missing = scales::percent(missing))
		|> as.data.frame()
	)
	return(df)
}

#' Get variable type
#'
#' @param x vector.
#'
#' @return a character string specifying data type.
#'
#' @export
#'

get_type = function(x) {
	type = class(x)
	if (inherits(x, "factor")|is.factor(x) | type=="factor") {
		type = "factor"
	} else if (type=="character") {
		type = "character"
	} else {
		type = "numeric"
	}
	return(type)
}

#' Convert levels to NA
#'
#' @param x input vector.
#' @param ... levels to be converted.
#'
#' @return a vector.
#'
#' @export

na_codes <- function(x, ...) {
    x[x %in% c(...)] <- NA
    x
}

#' Convert factors to numeric
#'
#' @param x input vector.
#'
#' @return a vector of class numeric.

factor_numeric <- function(x) {
	sjlabelled::as_numeric(x)
}

#' Convert numeric to factors
#'
#' @param x input vector.
#'
#' @return a vector of class factor.

numeric_factor <- function(x) {
	if (isTRUE(!is.null(attr(x, "labels")))) {
		x = sjlabelled::to_label(x)
	} else {
		x = sjlabelled::as_factor(x)
	}
	return(x)
}

#' Convert numeric to character
#'
#' @param x input vector.
#'
#' @return a vector of class character.

numeric_character <- function(x) {
	if (isTRUE(!is.null(attr(x, "labels")))) {
		x = sjlabelled::to_label(x)
	} else {
		x = sjlabelled::as_character(x)
	}
	return(x)
}

#' Generate quick data summary
#'
#' Generate data summary statistics for all the variables.
#'
#' @param df data.frame object.
#'
#' @return summary statistics.
#'
#' @export
#'

generate_data_summary = function(df) {
	out = sapply(df, function(x) {
		if (is.numeric(x) | is.integer(x)) {
		  summary(x)
		} else {
		  out = as.data.frame(table(x, useNA = "always"))
		  row.names(out) = NULL
		  colnames(out) = c("Category", "Frequency")
		  out = dplyr::arrange(out, desc(Frequency))
		  out
		}
	}, simplify = FALSE)
	return(out)
}

#' Format data-time 
#'
#' Customize data-time format for file naming
#'
#' @param x date vector.
#'
#' @param format date-time formate to output.
#'
#' @export
#'

format_date_time = function(x, format="%d-%m-%Y %H:%M:%S") {
	return(format(x, format))
}
