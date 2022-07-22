#' Visualize a ROC plot
#'
#' This function is designed to display a ROC plot using Takeda's colors.
#'
#' @import ggplot2
#' @import dplyr
#' @import pROC
#' @importFrom plotly ggplotly
#' @param tag the predicted value.
#' @param score the actual value.
#' @param model_name the name of the model to be added in the plot.
#' @param subtitle the subtitle of the model to added in the plot.
#' @param interval interval used in the axis.
#' @param plotly if want to convert plot into plotly.
#' @export

plot_roc <- function(tag, score, model_name = NA,
                     subtitle = NA, interval = 0.2, plotly = FALSE) {

  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }

  roc <- pROC::roc(tag, score, ci=T)
  coords <- data.frame(
    x <- rev(roc$specificities),
    y <- rev(roc$sensitivities))
  ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))

  p <- ggplot(coords, aes(x = x, y = y)) +
    geom_line(colour = "#891515", size = 1) +
    geom_point(colour = "#E1242A", size = 0.9, alpha = 0.4) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") +
    scale_x_reverse(name = "1 - Specificity [False Positive Rate]", limits = c(1,0),
                    breaks = seq(0, 1, interval)
                    # expand = c(0.1,0.1)
    ) +
    scale_y_continuous(name = "Sensitivity [True Positive Rate]", limits = c(0,1),
                       breaks = seq(0, 1, interval)
                       # expand = c(0.1, 0.1)
    ) +
    theme_minimal() +
    theme(axis.ticks = element_line(color = "grey80")) +
    # coord_equal() +
    ggtitle("ROC Curve: AUC") +
    annotate("text", x = 0.25, y = 0.10, vjust = 0, size = 4.2,
             label = paste("AUC =", round(100*ci[c("AUC"),],2))) +
    annotate("text", x = 0.25, y = 0.05, vjust = 0, size = 2.8,
             label = paste0("95% CI: ",
                            round(100*ci[c("min"),],2),"-",
                            round(100*ci[c("max"),],2)))

  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }

  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }

  if (plotly == TRUE) {
    p <- plotly::ggplotly(p)
  }

  return(p)
}
