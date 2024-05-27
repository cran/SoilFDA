#' Computation of Fractal Dimension
#' @description
#' The function computes the fractal dimension of particle size distribution based on soil mass
#'
#' @param Mass Numerical vector containing the mass of each particle size class
#' @param Size Numerical vector containing the average of each particle size class
#' @param TM Numerical value containing the total mass of soil
#' @param dmax Numerical value containing the average of maximum particle size class
#'
#' @return Fractal Dimension
#' @export
#' @import stats
#' @references Tyler S. W., Wheatcraft S. W. (1992). Fractal scaling of soil particle size distribution: Analysis and limitations. Soil Sci. Soc. Am. J., 56: 362.
#'
#' @examples
#' data <- data.frame(Mass = c(15.1,28.9,11.3,14.6,7.9,22.2),
#'                    Size = c(0.053,0.1765,0.4,1.25,3.375,4.75))
#' TM <- 100
#' dmax <- 4.75
#' attach(data)
#' fractdim(Mass = Mass, Size = Size, TM = TM, dmax = dmax)
fractdim <- function(Mass, Size, TM, dmax){
  csum <- cumsum(Mass)
  New_data <- csum/TM
  New_Size <- Size/dmax
  model <- stats::lm(log(New_data)~log(New_Size))
  coeff <- model$coefficients[2]
  D <- (3-coeff)
  names(D) <- "Fractal Dimension"
  return(D)
}
