
library(here)

### Prefer local files, otherwise read from web
LoadDataElseWeb <- function(file, url = NULL, path = here::here("./data/"), ReadFUN = read.csv) {
  paste0(path, "/", file) %>%
    ifelse(file.exists(.), ., paste0(url, "/", file)) %>%
      ReadFUN(.)
}

### Safe version to select columns whereas dplyr::select will crash on missing columns
RemoveCols <- function(df, columns, invert = FALSE) {
  if (invert) {
    df[, which(colnames(df) %in% columns)]
  } else {
    df[, -which(colnames(df) %in% columns)]
  }
}
