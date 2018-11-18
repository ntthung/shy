#' Oceanic ENSO Index
#'
#' @format A data.table with 3 variables
#' \describe{
#'     \item{year}{From 1950 to 2015}
#'     \item{month}{A factor with 12 levels from Jan to Dec. Each month is the average of 3 consecutive months, i.e, Jan denotes DJF average}
#'     \item{ONI}{Oceanic ENSO Index}
#' }
#' @source NOAA [http://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php]
'ONI'

#' Angat Reservoir inflow
#'
#' Inflow to Angat Reservoir in cubic meter per second.
#' @format A data.frame with 3 variables
#' \describe{
#'     \item{year}{From 1988 to 2014}
#'     \item{month}{A factor with 12 levels from Jan to Dec}
#'     \item{Q}{Inflow to the reservoir in cubic meter per second}
#' }
#' @source [http://202.90.134.59/riverflow/01_avg30yr.aspx]
'Angat'
