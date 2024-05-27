#' Geometric Mean Diameter
#' @description
#' The function computes the geometric mean  diameter of particle size distribution
#'
#' @param Mass Numerical vector containing the mass of each particle size class
#' @param Size Numerical vector containing the average of each particle size class
#'
#' @return Geometric Mean Diameter
#' @export
#' @references Perfect E, Rasiah V, Kay BD. (1992). Fractal Dimension of soil aggregate size distributions calculated on number and mass. Soil Science Society of America Journals, 56: 1407.
#'
#' @examples
#' data <- data.frame(Mass = c(15.1,28.9,11.3,14.6,7.9,22.2),
#'                    Size = c(0.053,0.1765,0.4,1.25,3.375,4.75))
#' attach(data)
#' GMD(Mass = Mass, Size = Size)
GMD <- function(Mass, Size){
  product <- Mass*log(Size)
  sumproduct <- sum(product)
  gmd <- exp(sumproduct/sum(Mass))
  names(gmd) <- "Geometric Mean  Diameter"
  return(gmd)
}
