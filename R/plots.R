#' Plot stochastic replicates
#'
#' Diagnostic plot to check the stochastic replicates by comparing it with the observed time series
#' @param reps Stochastic replicate
#' @param obs Observed time series
#' @param var.name Name of the variable column to be plotted, must be the same in reps and obs
#' @return A ggplot2 plot
#' @export
plot_reps <- function(reps, obs, var.name) {

    ggplot(reps, aes(year + as.numeric(month) / 12, !!ensym(var.name))) +
        geom_line(aes(group = rep), colour = 'gray80') +
        geom_line(data = obs) +
        labs(x = 'Time', y = var.name)
}
