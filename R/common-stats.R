

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(grDevices)


##### Distribution #####

#' Plots the distribution of variables in a dataset
#' The visualization is a violin plot across facets with optional boxplot and groupings
#'
#' @param df The dataframe
#' @param targetCol Optional; grouping column
#' @param showBoxPlot Default true; adds a small boxplot for quantiles
#' @param extend Optional; default null; a ggproto object to append to the visualization
#' @author Anthogonyst
PlotVarDistribution <- function(df, targetCol = NULL, showBoxPlot = TRUE, extend = NULL) {
  meltDf = reshape2::melt(df[, sapply(df, is.numeric) | colnames(df) %in% targetCol], 
                          id = targetCol, value.name = "value") 
  
  if (showBoxPlot) {
    return(
      ggplot2::ggplot(meltDf[! is.na(meltDf["value"]), ]) +
        ggplot2::aes(x = variable, y = value) +
        ggplot2::aes_string(group = targetCol) +
        ggplot2::geom_violin(adjust = 1L, scale = "area", fill = "#FF464A") +
        ggplot2::geom_boxplot(width = 0.1) +
        extend +
        ggplot2::theme_minimal() +
        ggplot2::facet_wrap(~variable, scales = "free")
    )
  } else {
    return(
      ggplot2::ggplot(meltDf[! is.na(meltDf["value"]), ]) +
        ggplot2::aes(x = variable, y = value) +
        ggplot2::aes_string(group = targetCol) +
        ggplot2::geom_violin(adjust = 1L, scale = "area", fill = "#FF464A") +
        extend +
        ggplot2::theme_minimal() +
        ggplot2::facet_wrap(~variable, scales = "free")
    )
  }
}


##### Correlation #####

#' Creates a visualization of a correlation matrix
#' @author Anthogonyst
PlotCorrEllipse <- function(corData, pal = RColorBrewer::brewer.pal(5, "Spectral"),
                            highlight = c("both", "positive", "negative")[1], hiMod = 1) {
  colorRange = 100
  coloredVals = corData*50 + 50
  skewRange = max(min(hiMod[[1]], 2), 0)
  
  if (is.numeric(pal)) {
    warning("Passed number as argument for palette. It works but was this intentional?")
  }
  if (skewRange != hiMod) {
    warning("Color range skew only allowed between 0 and 2 where 0.5*n% of value range are colored.  cor(X) C [-1, 1]")
  }
  if (highlight[[1]] == "positive") {
    colorRange = 50 * skewRange
    coloredVals = 1 - corData*50 + 50
  }
  if (highlight[[1]] == "negative") {
    colorRange = 50 * skewRange
    coloredVals = corData*50 + 50
  }
  
  ellipse::plotcorr(corData, mar = c(1,1,1,1),
                    col = grDevices::colorRampPalette(pal)(colorRange)[coloredVals])  
}


##### Classification #####

#' Creates an annotated table similar to caret::confusionMatrix
#' @author Anthogonyst
ClassificationInfo <- function(df, applyTable = FALSE, verbose = FALSE) {
  if (applyTable) {
    df = table(df)
  }
  
  trueNeg = df[1, 1]
  falsePos = .SumOf(df, upper.tri)
  falseNeg = .SumOf(df, lower.tri)
  truePos = .SumOf(df, .diag, TRUE)
  
  ### Calculates statistical relevance from the data frame
  accuracy = .SumOf(df, .diag) / sum(df)
  errors = 1 - accuracy
  precision = .SumOf(df, .diag, TRUE) / (.SumOf(df, .diag, TRUE) + .SumOf(df, upper.tri))
  sensitivity = .SumOf(df, .diag, TRUE) / (.SumOf(df, .diag, TRUE) + .SumOf(df, lower.tri))
  specificity = df[1, 1] / (df[1, 1] + .SumOf(df, upper.tri))
  f1 = 2 * precision * sensitivity / (precision + sensitivity)
  
  attr(df, "accuracy") = accuracy
  attr(df, "error.rate") = errors
  attr(df, "precision") = precision
  attr(df, "sensitivity") = sensitivity
  attr(df, "specificity") = specificity
  attr(df, "f1") = f1
  
  if (verbose) {
    message("   Accuracy: ", round(accuracy, 4))
    message(" Error Rate: ", round(errors, 4))
    message("  Precision: ", round(precision, 4))
    message("Sensitivity: ", round(sensitivity, 4))
    message("Specificity: ", round(specificity, 4))
    message("   F1 Score: ", round(f1, 4))
  }
  
  return(df)
}

