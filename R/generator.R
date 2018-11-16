# source('init.R')
# source('read_Q.R')
#
# horizon <- 1967:2014 # One extra year for lags
# n.reps  <- 100
# n.years <- length(horizon)
#
#
# Q %<>%
#     mutate(cms_log = log(cms)) %>%
#     group_by(month) %>%
#     mutate(mm = mean(cms_log), msd = sd(cms_log)) %>%
#     mutate(cms_std = (cms_log - mm) / msd)
#
# # Umiray monthly mean
# QU.mm <- QU %>%
#     group_by(month) %>%
#     summarise(mm = mean(cms)) %>%
#     .$mm
#
# Q %<>%
#     group_by(year) %>%
#     mutate(cms_wU = cms + QU.mm) %>%
#     ungroup()
#
# Q.mm <- Q %>%
#     group_by(month) %>%
#     summarise(mm = mean(cms_wU))
#
# seasons <- Q.mm %>%
#     mutate(season = (mm - min(mm)) / (max(mm) - min(mm))) %>%
#     .$season %>%
#     rep(n.years-1)
#
# Q.norm.val <- 2*max(Q$cms) + max(QU.mm)
#
# nino.C.full <- read.csv('01_Data/oni_index.csv') %>%
#     gather(key = 'month', value = 'anom', 2:13) %>%
#     group_by(year) %>%
#     mutate(month = 1:12) %>%
#     ungroup() %>%
#     arrange(year)
#
# nino.C  <- filter(nino.C.full, year %in% horizon)
#
# # Calculate average of previous 4 months
# avg4 <- function(x) c(rep(0,4), sapply(5:length(x), function(i) mean(x[(i-4):(i-1)])))
# nino.C %<>% mutate(anom_rm = avg4(anom))
#
# one_enso_rep <- function(n.years) {
#     # Function to create one replicate using equation (4) in Burgers (1999)
#
#     # Parameters reported by Burgers (1999)
#     a  <- 0.94
#     b  <- 0.13
#     k  <- 0.86
#     ve <- 0.24 # variance of epsilon
#
#     n.months <- n.years * 12
#     epsilon  <- rnorm(n.months, 0, sqrt(ve)) # white noise
#     x    <- rep(0, n.months)
#     x[1] <- rnorm(1, 0, sqrt(ve))   # x0 = 0, x1 = epsilon0
#     x[2] <- 2*a*x[1] + epsilon[1] - k*x[1]
#
#     for (i in 2:(n.months - 1)){
#         x[i+1] <- 2*a*x[i] - (a^2 + b^2)*x[i-1] + epsilon[i] - k*epsilon[i-1]
#     }
#
#     return(x)
# }
#
# replicator <- function(n.reps, n.years, var.cor = TRUE) {
#
#     set.seed(100)
#     enso_reps_mat <- replicate(n.reps, one_enso_rep(n.years))
#
#     if (var.cor) {
#         nino.hist.sd <- sd(nino.C$anom)
#         nino.rep.sd <- sd(c(enso_reps_mat))
#         enso_reps_mat <- enso_reps_mat / nino.rep.sd * nino.hist.sd
#     } else {
#         norm_val <- 2*max(abs(nino.C$anom))
#         enso_reps_mat[which(enso_reps_mat >  norm.val)] <-  norm.val
#         enso_reps_mat[which(enso_reps_mat < -norm.val)] <- -norm.val
#     }
#
#     enso_reps <-
#         enso_reps_mat %>%
#         data.frame() %>%
#         mutate(year = nino.C$year, month = nino.C$month) %>%
#         gather(key = 'reps', value = 'anom', 1:n.reps, factor_key = TRUE)
#
#     # Standardize ENSO: substract mean and divide standard deviation
#     nino.C %<>%
#         group_by(month) %>%
#         mutate(anom_std = (anom - mean(anom))/ sd(anom)) %>%
#         ungroup()
#
#     # Standardize the ENSO replicates
#     enso_reps_mat_std <-
#         enso_reps %>%
#         group_by(reps, month) %>%
#         mutate(anom_std = (anom - mean(anom)) / sd(anom)) %>%
#         select(-anom) %>%
#         spread(reps, anom_std) %>%
#         ungroup() %>%
#         select(-year, -month)
#
#     # Fit lag4 ARMAX
#     armax4.fit <- Arima(ts(Q$cms_std, frequency = 12, start = c(1968,1)),
#                         order = c(1,0,1),
#                         xreg = nino.C$anom_std[9:(nrow(nino.C)-4)])  # Lag 4 is here
#
#     set.seed(11)
#     Q.sim <- apply(enso_reps_mat_std, 2,
#                    function(x) simulate(object = armax4.fit, xreg = x))
#
#     Qm.armax4 <- data.frame(Q.sim) %>%
#         mutate(year = nino.C$year, month = nino.C$month) %>%
#         gather(key = reps, value = cms_std, 1:n.reps, factor_key = TRUE) %>%
#         group_by(reps) %>%
#         mutate(mm = c(Q$mm[1:12], Q$mm),
#                msd = c(Q$msd[1:12], Q$msd),
#                cms = exp(cms_std * msd + mm)) %>%
#         ungroup()
#
#     # Trim
#     which(Qm.armax4$cms > 2*max(Q$cms))
#
#     Qm.armax4 %<>%
#         mutate(cms = replace(cms, which(cms > 2*max(Q$cms)), 2*max(Q$cms)))
#
#     all_reps <- inner_join(enso_reps, Qm.armax4, by = c('year', 'month', 'reps'))
#
#     # Add Umiray
#     all_reps %<>%
#         group_by(reps, year) %>%
#         mutate(cms_wU = cms + QU.mm) %>%
#         ungroup() %>%
#         group_by(reps) %>%
#         mutate(anom_rm = avg4(anom)) %>%
#         ungroup()
#
#     return(all_reps)
# }
#
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
