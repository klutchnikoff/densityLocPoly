#' Risk associated to polynomial densities on polynomial sector
#'
#' @description
#' Simulated data
#' TBC
#'
#' @format ## `risk_poly`
#' A data frame with 960,000 rows and 6 columns:
#' \describe{
#'   \item{rep}{Replication number}
#'   \item{method}{Estimation method: sparr, lp0, lp1}
#'   \item{k}{Degree of the polynomial sector}
#'   \item{n}{Number of observed data}
#'   \item{h}{Bandwidth}
#'   \item{value}{Value of the squared error between f(0,0) and the estimation}
#' }
"risk_poly"

#' @rdname risk_poly
#' @format ## `risk_norm`
#' A data frame with 960,000 rows and 6 columns.
#' Normal densities
"risk_norm"
