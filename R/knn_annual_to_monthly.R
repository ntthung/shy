#' Annual to daily disaggregation
#'
#' Disaggregate Q1 by sampling from Q2 using (modified) k-nearest-neighbour resampling (Nowalk et al, 2011). Leap years are handled.
#' @inheritParams knn_annual_to_daily
#' @inherit knn_annual_to_daily return
#' @inherit knn_annual_to_daily references
#' @export
knn_annual_to_monthly <- function(Q1, Q2, K) {
    # Disaggregate Q1 by sampling from Q2 using (modified) knn resampling (Nowalk et al)
    # Arguments
    #   Q1: an annual dataframe (year, Qa)
    #   Q2: a monthly dataframe (year, month, Qm)
    # Returns a data frame (year, nn, month, Qm) (nn for the sampled one of the k nearest neighbours)
    # Packages required: dplyr

    Q2a <- Q2 %>% group_by(year) %>% summarise(Qa = sum(Qm))
    Q2  <- Q2 %>%  group_by(year) %>% mutate(index = Qm / sum(Qm))
    if (missing(K)) K <- ceiling(sqrt(nrow(Q2a))) # K = sqrt(number of years in observed record)

    knn1 <- function(q) {
        # Return one of the K nearest neighbours randomly
        # Argument
        #   q      : an annual flow value
        #   output : the year corresponding to the nearest neighbour

        distance <- abs(Q2a$Qa - q) # Eucledian distance

        #  sample() needs a probability vector of length > 1, so if K = 1 we don't sample
        if (K > 1) {
            knn      <- order(distance)[1:K]             # Take the K nearest neighbours
            w        <- (1 / 1:K) / sum(1 / 1:K)         # Calculate weights
            row      <- sample(knn, size = 1, prob = w)  # Sample according to weights
        } else row <- order(distance)[1]

        return(Q2a$year[row])
    }

    map_flow <- function(v) {
        # Map an annual flow to a vector of monthly flow
        # Arguments
        #   v: a vector with 2 elements (year, Qa)
        # Returns a data frame (yaer, nn, month, Qm)

        y     <- knn1(v['Qa'])       # nearest neighbour selected
        index <- Q2$index[Q2$year==y]  # copy index
        q1    <- v['Qa'] * index       # calculate monthly flow

        return(data.frame(year = rep(v['year'], 12),
                          nn = rep(y, 12),
                          month = 1:12,
                          Qm = q1))
    }

    # Apply map_flow to each row of Q1 (i.e. each year in Q1)
    bind_rows(apply(Q1, 1, map_flow))
}
