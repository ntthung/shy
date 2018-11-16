#' Annual to daily disaggregation
#'
#' Disaggregate Q1 by sampling from Q2 using (modified) k-nearest-neighbour resampling (Nowalk et al, 2011). Leap years are handled.
#' @param Q1 An annual dataframe (year, Qa).
#' @param Q2 A daily dataframe (year, month, day, Qd).
#' @param K Number of nearest neighbours.
#' @return A data frame (year, nn, month, day, Qd) (nn for the sampled one of the k nearest neighbours).
#' @references Nowak, K., J. Prairie, B. Rajagopalan, and U. Lall (2010), A nonparametric stochastic approach for multisite isaggregation of annual to daily streamflow, Water Resour. Res., 46, W08529, doi:10.102/2009WR008530.
#' @export
knn_annual_to_daily <- function(Q1, Q2, K) {

    # Incomplete years are not sampled
    Q2a <- Q2 %>% group_by(year) %>% summarise(Qa = sum(Qd)) %>% ungroup() %>% filter(!is.na(Qa))
    Q2  <- Q2 %>% group_by(year) %>% mutate(index = Qd / sum(Qd)) %>% ungroup()
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
        Q.sub <- filter(Q2, year == y)
        n     <- nrow(Q.sub)
        q1    <- v['Qa'] * Q.sub$index # calculate monthly flow

        return(data.frame(year = rep(v['year'], n),
                          nn = rep(y, n),
                          month = Q.sub$month,
                          day = Q.sub$day,
                          Qd = q1))
    }

    # Apply map_flow to each row of Q1 (i.e. each year in Q1)
    bind_rows(apply(Q1, 1, map_flow))
}
