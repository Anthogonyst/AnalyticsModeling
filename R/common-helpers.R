

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

#' Produces multiplicative components for easy feature engineering between variables
#' 
#' @param df The dataframe
#' @param dropCols Optional; abstains specific numerical columns when creating derived columns
#' @examples 
#' \dontrun{
#' data(iris)
#' renamedIris = magrittr::set_colnames(iris, c("A", "B", "C", "D", "Words")) %>% 
#' 
#' expandedData = cbind(renamedIris, CrossProdFeatures(renamedIris, dropCols = c("A")))
#' 
#' resultColumns = c("A", "B", "C", "D", "Words", "B\*C", "B\*D", "C\*D")
#' resultColumns == colnames(expandedData)
#' }
#' @author Anthogonyst
CrossProdFeatures <- function(df, dropCols = NULL) {
  numDf = df[, sapply(df, is.numeric) & ! sapply(df, is.factor) & ! colnames(df) %in% dropCols]
  # Suggestion: Redo to not iterate duplicate columns in the first place
  goal = sapply(numDf, function(x) { as.matrix(numDf) * rep(as.matrix(x), ncol(numDf)) }, simplify = FALSE) %>% do.call(cbind, .)
  namae = sapply(colnames(numDf), function(x) { paste0(x, "*", colnames(numDf)) }, simplify = FALSE)
  pruneDupes = reshape2::melt(t(upper.tri(do.call(cbind, namae))), id = NULL, value.name = "vals")[["vals"]]
  
  dimnames(goal)[[2]] = magrittr::set_names(unlist(namae), NULL)
  as.data.frame(goal)[, pruneDupes]
}
