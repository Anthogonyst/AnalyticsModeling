
library(here)

#' Prefer local files, otherwise read from web.
#'
#' @param file The name of the file.
#' @param url Access a website for matching filename as a backup.
#' @param path Defaults to ~/data, otherwise any data folder allowed.
#' @param ReadFUN A closure to read the file into an object, default read.csv.
#' @author Anthogonyst
LoadDataElseWeb <- function(file, url = NULL, path = here::here("./data/"), ReadFUN = read.csv) {
  paste0(path, "/", file) %>%
    ifelse(file.exists(.), ., paste0(url, "/", file)) %>%
      ReadFUN(.)
}

#' Safe version to select columns whereas dplyr::select will crash on missing columns
#' 
#' @param df The dataframe.
#' @param columns The columns to remove if present
#' @param invert A boolean value to invert results (select none except columns)
#' @author Anthogonyst
RemoveCols <- function(df, columns, invert = FALSE) {
  if (invert) {
    df[, which(colnames(df) %in% columns)]
  } else {
    df[, -which(colnames(df) %in% columns)]
  }
}
