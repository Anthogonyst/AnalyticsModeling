

library(ggplot2)

##### Linear Regression Assumptions #####

#' One of three plots necessary to verify assumptions of linear regression
#' @author Anthogonyst
PlotLinearity <- function(lm_calc, col = "#FF464A", extend = NULL) {
  ggplot2::ggplot(data = lm_calc) +
    ggplot2::aes(x = lm_calc$fitted.values, y = lm_calc$residuals) +
    ggplot2::geom_jitter(width = 0.01, color = col) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::xlab("Fitted values") +
    ggplot2::ylab("Residuals") +
    ggplot2::theme(legend.position = "none") +
    extend
}

#' One of three plots necessary to verify assumptions of linear regression
#' @author Anthogonyst
PlotDistribution <- function(lm_calc, col = "#FF464A", extend = NULL) {
  ggplot2::ggplot(data = lm_calc) +
    ggplot2::aes(x = lm_calc$residuals, y = "") + 
    ggplot2::geom_violin(adjust = 1L, scale = "area", fill = col, show.legend = FALSE) +
    ggplot2::xlab("Residuals") +
    ggplot2::ylab("Distribution") +
    extend
}

#' One of three plots necessary to verify assumptions of linear regression
#' @author Anthogonyst
PlotQQuantile <- function(lm_calc, col = "#FF464A", extend = NULL) {
  ggplot2::ggplot(data = lm_calc) +
    ggplot2::aes(sample = lm_calc$residuals) +
    ggplot2::stat_qq(color = col) +
    ggplot2::theme(legend.position = "none") +
    extend
}

