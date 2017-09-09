LognormalLikelihood <- function(x, mu, sigma){
  z <- (log(x) - mu) / sigma

  lnL <- -z^2/2 - log(x) - log(sigma)

  sum(lnL)
}

#' @title CalcLognormalLikelihood
#'
#' @param data A vector of sample observations
#' @param mean_range A vector with values of the mean
#' @param CV_range A vector with CV values to be tested
#' @param length.out If the parameter ranges
#'
#' @details
#'
#' The ranges for mean and CV may be of any arbitrary length. If they are a given as a vector of length 2, the function will
#' assume that this represents the bounding range of a sequence of values. In this instance, a sequence of length equal to the
#' length.out parameter will be generated.
#'
#' @include Params.R
#'
#' @examples
#' my_likelihood <- CalcLognormalLikelihood(somedata, mean_range = c(50e3, 150e3), CV_range = c(0.3, 1.8))
#'
#' @export
#'
#' @importFrom purrrlyr by_row
#'
CalcLognormalLikelihood <- function(data, mean_range, CV_range, length.out = 100){

  if (!is.numeric(data)) stop("data input must be numeric")
  if (any(data <= 0)) stop("data input must be greater than zero")
  if (any(mean_range <= 0 )) stop("mean_range must be greater than zero")
  if (any(CV_range <= 0 )) stop("CV_range must be greater than zero")

  if (length(mean_range) == 2) {
    mean_range <- seq(from = mean_range[1], to = mean_range[2], length.out = length.out)
  }
  if (length(CV_range) == 2) {
    CV_range <- seq(from = CV_range[1], to = CV_range[2], length.out = length.out)
  }

  dfL <- expand.grid(mean_range, CV_range)
  rownames(dfL) <- NULL
  names(dfL) <- c("Mean", "CV")

  dfL <- purrrlyr::by_row(
      dfL
    , .to = "Likelihood"
    , .collate = "rows"
    , function(x){
      lstParams <- LognormalParams(x$Mean, x$CV)
      LognormalLikelihood(data, lstParams$mu, lstParams$sigma)
  })

  dfL
}
