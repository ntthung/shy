#' ONI replicate
#'
#' Create one replicate of ONI time series using equation (4) in Burgers (1999) and parameter values reported in the same paper.
#' @inheritParams ONI_reps
#' @return A vector of the ONI time series.
#' @export
one_ONI_rep <- function(num.years) {

    a  <- 0.94
    b  <- 0.13
    k  <- 0.86
    ve <- 0.24 # variance of epsilon

    num.months <- num.years * 12
    epsilon  <- rnorm(num.months, 0, sqrt(ve)) # white noise
    x    <- rep(0, num.months)
    x[1] <- rnorm(1, 0, sqrt(ve))   # x0 = 0, x1 = epsilon0
    x[2] <- 2*a*x[1] + epsilon[1] - k*x[1]

    for (i in 2:(num.months - 1)){
        x[i+1] <- 2*a*x[i] - (a^2 + b^2)*x[i-1] + epsilon[i] - k*epsilon[i-1]
    }

    x
}

#' ONI replicates
#'
#' Generate multiple stochastic replicates of ONI time series according to Burgers (1999).
#' @param ONI.obs A data frame of observed ONI with three columns (year, month, ONI). By default, the replicates will have the same years as ONI.obs, unless `years` is provided.
#' @param years If provided, the replicates will have these years.
#' @param num.reps Number of replicates to be generated. Default is 100.
#' @param var.cor Whether variance correction is performed. Setting `var.cor = TRUE` will ignore `trim`.
#' @param trim Used to cut off values that are too high or too low. Values that are more than `trim * max(abs(obs))` will be set to `max(abs(obs))`. Default is 2. Supply `trim = NULL` if you don't want trimming.
#' @return A data.table of 4 columns (rep, year, month, ONI).
#' @export
ONI_reps <- function(ONI.obs, years = NULL, num.reps = 100, trim = 2, var.cor = TRUE) {

    if (is.null(years)) years <- unique(ONI.obs$year)
    num.years <- length(years)
    # This returns a matrix, one row for each time step, one column for each replicate
    reps <- replicate(num.reps, one_ONI_rep(num.years))
    # Trim or variance correction if necessary
    if (var.cor) {
        hist.sd <- sd(ONI.obs$ONI)
        rep.sd <- sd(c(reps))
        reps <- reps / rep.sd * hist.sd
    } else if (!is.null(trim)) {
        norm.val <- trim * max(abs(ONI.obs$ONI))
        reps[which(reps >  norm.val)] <-  norm.val
        reps[which(reps < -norm.val)] <- -norm.val
    }

    data.table(rep = rep(1:num.reps, each = 12*num.years),
               year = rep(years, each = 12),
               month = rep(1:12, times = num.reps),
               ONI = c(reps))
}

#' Stochastic streamflow replicates
#'
#' Generate stochastic replicates of monthly streamflow time series.
#' @param Q.obs A data frame of observed streamflow with three columns (year, month, Q). By default, the replicates will have the same years as Q.obs, unless `years` is provided.
#' @param method Either 'arima' or 'arimax'. 'parma' will be included soon.
#' @param order A vector of ARIMA model order. See `forecast::Arima` for details.
#' @param X.obs Observed exogeneous input, a data frame with 3 columns (year, month, X). If there are more than 3 columns, the desirable column name to be used as exogenous input should be given in `X.name`; otherwise the function will take the 3th column by default.
#' @param X.reps Replicates of X. See [ONI_reps].
#' @param X.name String, the name of the column used to represent the exogenous input.
#' @param X.lag Integer, time lag of X for model fitting
#' @param years If provided, the replicates will be indexed by these years.
#' @param num.reps Number of replicates to be generated. Default is 100.
#' @param trim Used to cut off values that are too high or too low. Values that are more than `trim * max(abs(obs))` will be set to `max(abs(obs))`. Default is 2. Supply `trim = NULL` if you don't want trimming.
#' @param Q.trans The pre-processing transformation that will be applied to Q.obs before fitting.
#' \describe{
#'     \item{log}{Log-transformation.}
#'     \item{std}{Standardization: substracting monthly mean and dividing by monthly standard deviation.}
#'     \item{log-std}{First take log-transform, then do standardization.}
#' }
#' @param X.trans Same as `Q.trans` but applied on `X.obs`.
#' @return A data.table with the following collumns: year, month, X (if `method == 'armax'`) and Q.
#' @details First, an ARMA or ARMAX model will be fitted using the overlapping period of `Q.obs` and `X.obs`. Then, an ARMA model will be fitted using `forecast::Arima`. Finally, stochastic time series will be generated using `stats::simulate`.
#' @export
Q_reps <- function(Q.obs, method, order,
                   X.obs = NULL, X.reps = NULL, X.name = NULL, X.lag = 0,
                   years = NULL, num.reps = 100, trim = 2, Q.trans = 'log-std', X.trans = 'std') {

    # Transform flow
    Q.log <- length(grep('log', Q.trans)) > 0
    Q.std <- length(grep('std', Q.trans)) > 0
    Q <- copy(Q.obs)
    Q[, Q2 := {if (Q.log) log(Q) else Q}
      ][, Q3 := {if (Q.std) (Q2 - mean(Q2)) / sd(Q2) else Q2}, by = month]

    if (method == 'arma') {
        if (is.null(years)) years <- unique(Q.obs[, year])
        num.years <- length(years)
        model <- forecast::Arima(Q[, Q3], order = order)
        Q.sim <- data.table(rep = rep(1:num.reps, each = 12*num.years),
                            year = rep(years, each = 12) %>% rep(num.reps),
                            month = rep(1:12, num.reps),
                            Qsim = c(replicate(num.reps, as.numeric(simulate(model))))) %>%
            merge(Q, by = c('year', 'month'))
    } else if (method == 'armax') {
        X <- copy(X.obs)
        X.reps <- copy(X.reps)
        if (is.null(X.name)) X.name <- colnames(X.reps)[4]
        setnames(X, old = X.name, new = 'X')
        setnames(X.reps, old = X.name, new = 'X')
        if (length(grep('std', X.trans)) > 0) {
            X[, X := (X - mean(X)) / sd(X), by = month]
            X.reps[, X := (X - mean(X)) / sd(X), by = month]
        }
        dt <- merge(Q, X[, .(year, month, X = shift(X, X.lag))], by = c('year', 'month'))
        model <- forecast::Arima(dt[, Q3], order = order, xreg = dt[, X])
        X.reps[year %in% dt[, year], Qsim := as.numeric(simulate(model, xreg = X)), by = rep][, X := NULL]
        Q.sim <- merge(X.reps, Q, by = c('year', 'month'), all.X = TRUE)

    } else warning("Only arma and armax methods are supported at the moment.")

    # Transform back
    if (Q.std) Q.sim[, Qsim := Qsim * sd(Q2) + mean(Q2), by = month]
    if (Q.log) Q.sim[, Qsim := exp(Qsim)]
    # Tidy up
    Q.sim[, c('Q', 'Q2', 'Q3') := NULL]
    setnames(Q.sim, 'Qsim', 'Q')
    # Trim
    if (!is.null(trim)) Q.sim[Q > trim*max(Q.obs[, Q]), Q := trim*max(Q.obs[, Q])]

    Q.sim[]

}

## @param Q.other Other flow processes not modelled by `model`, for example, diversion from another catchment. This will be added to the replicates. Must have the same structure as `Q.obs`.


# write.reps <- function(all_reps, path, norm_val) {
#
#     # Write only ENSO reps for classification
#     lapply(unique(all_reps$reps),
#            function(x) {
#                case <- filter(all_reps, reps == x) %>%
#                    select(year, month, anom)
#                write.csv(case, file = paste0(path, 'ENSO_', x, '.csv'), row.names = FALSE)
#            }
#     )
#     # Write the reps in Christoph's template
#     lapply(unique(all_reps$reps),  # For each reps
#            function(x) {
#                case   <- filter(all_reps, reps == x) # Select current rep
#                Q_lag1 <- lag(case$cms_wU)[-(1:12)]    # Shift Q back by 1 month, omit first year (1967)
#                case %>%
#                    filter(year >= 1968) %>%   # Remove 1967
#                    mutate(day = 1,
#                           hour = 0,
#                           min = 0,
#                           Q = cms_wU,
#                           seasonal = seasons,
#                           ENSO_4avg_normalized = (anom_rm + norm_val) / (2*norm_val),
#                           Inflow_norm = Q_lag1 / Q.norm.val) %>%
#                    select(year, month, day, hour, min, Q, seasonal, ENSO_4avg_normalized, Inflow_norm) %>%
#                    round(2) %>%
#                    write.csv(file = paste0(path, x,'.csv'), row.names = FALSE)
#            }
#     )
# }
#
# reps.withVC <- replicator(n.reps, n.years, var.cor = TRUE)
# norm.val.VC <- max(c(max(abs(reps.withVC$anom)), max(abs(nino.C$anom))))
# #write.reps(reps.withVC, path = 'Output/New_reps_VC/', norm_val = norm.val.VC)
#
# ggplot() +
#     geom_line(data = reps.withVC, aes(year + month/12, anom, group = reps), colour = 'grey') +
#     geom_line(data = nino.C, aes(year + month/12, anom), colour = 'black')
#
# ggplot() +
#     geom_line(data = reps.withVC, aes(year + month/12, cms, group = reps), colour = 'grey') +
#     geom_line(data = Q, aes(year + month/12, cms), colour = 'black')
#
