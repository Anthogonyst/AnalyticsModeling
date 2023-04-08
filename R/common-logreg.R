

library(magrittr)
library(graphics)
library(caret)

#' Uses logistical regression model to make predictions and stores in a dataframe as "class" and "probability"
#' 
#' @param model A trained model object
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @author Anthogonyst
MakePredictions <- function(model, testData, excludeCol = "target", threshold = 0.5) {
  probability = stats::predict(model, testData[, ! colnames(testData) %in% excludeCol])
  class = probability %>%
    { .[. > threshold] = 1 ; . } %>%
      { .[. <= threshold] = 0 ; . } %>%
        as.factor(.)
  
  data.frame(class, probability)
}

#' Uses logistical regression model to create confusion matrix as a test of validity
#' 
#' @param model A trained model object
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @author Anthogonyst
VerifyKfold <- function(model, testData, excludeCol = "target", threshold = 0.5) {
  MakePredictions(model, testData, excludeCol, threshold) %>%
    { caret::confusionMatrix(.$class, testData[, colnames(testData) %in% excludeCol], mode = "everything") }
}

#' Runs all logistical regression tests
#' 
#' @param model A trained model object
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @param ... Arguments passed to graphics::fourfoldplot
#' @author Anthogonyst
ValidationPipeline <- function(model, testData, excludeCol, threshold = 0.5, ...) {
  p = MakePredictions(model, testData, excludeCol, threshold)
  d = VerifyKfold(model, testData, excludeCol, threshold)
  g = graphics::fourfoldplot(d$table, color = c("#B22222", "#2E8B57"), ...)
  
  invisible(list(p, d, g))
}

#' Creates a table to assert differences in model performance
#' 
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param ... Any number of trained model objects
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @author Anthogonyst
CompareModelStats <- function(testData, excludeCol = "target", ..., threshold = 0.5) {
  models = list(...)
  
  lapply(models, VerifyKfold, testData, excludeCol, threshold) %>%
    lapply(., function(x) {
      c(x$byClass["F1"], x$overall["Accuracy"], x$table[1, 1], x$table[1, 2], x$table[2, 1], x$table[2, 2]) %>%
        magrittr::set_names(., c("F1", "Accuracy", "TP", "FP", "FN", "TN"))
    }) %>%
      do.call(rbind, .)
}

