#' @title LognormalParams
#'
#' @description
#'
#' This function will translate mean and CV values into corresponding mu and sigma values for a lognormal distribution.
#'
#' @param Mean The mean is a positive numeric value
#' @param CV The CV is a positive numeric value
#'
#' @details
#' Add some remarks about the parameterization.
#'
#' @return Returns a named list with elements for mu and sigma
#'
#' @examples
#' LognormalParams(10e3, 1.5)
#'
#' @export
LognormalParams <- function(Mean, CV){

  if (missing(Mean)) stop("No mean was specified")
  if (missing(CV)) stop("No CV was specified")

  if (!is.numeric(Mean)) stop("Mean must be numeric")
  if (!is.numeric(CV)) stop("CV must be numeric")

  if (Mean <= 0) stop("Mean is strictly positive")
  if (CV <= 0) stop("CV is strictly positive")

  sigma <- log(sqrt(1 + CV^2))
  mu <- log(Mean) - sigma^2/2

  list(mu = mu, sigma = sigma)
}

#' @title GammaParams
#'
#' @description
#'
#' This function will translate mean and CV values into corresponding alpha and beta value for a gamma distribution.
#'
#' @param Mean The mean is a positive numeric value
#' @param CV The CV is a positive numeric value
#'
#' @details
#'
#' Add some remarks about the parameterization.
#'
#' @return Returns a named list with elements for alpha and beta
#'
#' @examples
#' GammaParams(10e3, 1.5)
#'
#' @export
GammaParams <- function(Mean, CV){

  if (missing(Mean)) stop("No mean was specified")
  if (missing(CV)) stop("No CV was specified")

  if (!is.numeric(Mean)) stop("Mean must be numeric")
  if (!is.numeric(CV)) stop("CV must be numeric")

  if (Mean <= 0) stop("Mean is strictly positive")
  if (CV <= 0) stop("CV is strictly positive")

  beta = 1 / (mean * CV^2)
  alpha = 1 / CV^2

  list(alpha = alpha, beta = beta)

}