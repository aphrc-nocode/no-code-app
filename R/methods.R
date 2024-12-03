#' Utilities to upload various data formats
#'
#' This function uses the file extension to determine data type and read data appropriately.
#'
#' @param path data path or url.
#'
#' @return a dataframe.
#'
#' @keywords internal

upload_data = function(path)UseMethod("upload_data")

#' Utilities saves data frame into various file formats
#'
#' This function uses the file extension to determine data type and save data appropriately into the local disk.
#'
#' @param df dataframe.
#' @param path data path or url.
#'
#' @return NULL.
#'
#' @keywords internal

write_data = function(path, df)UseMethod("write_data")




