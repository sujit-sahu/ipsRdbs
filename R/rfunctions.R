#' Draws a butterfly as on the front cover of the book 
#' @param color This is the color to use in the plot. It can take any value 
#' that R can use for color, e.g. 1, 2, "blue" etc. 
#' @param a Parameter controlling the shape of the butterfly
#' @param b Second parameter controlling the shape of the butterfly
#' @return No return value, called for side effects. It generates a plot
#' whose colour and shape are determined by the supplied parameters.   
#' @examples 
#' butterfly(color = 6)
#' old.par <- par(no.readonly = TRUE)
#' par(mfrow=c(2, 2))
#' butterfly(color = 6)
#' butterfly(a=5, b=5, color=2)
#' butterfly(a=10, b=1.5, color = "seagreen")
#' butterfly(a=20, b=4, color = "blue")
#' par(old.par) # par(mfrow=c(1, 1))
#' @export
butterfly <- function(color = 2, a=2, b=4) {
  theta <- seq(from=0.0, to=24 * pi, len = 2000)
  radius <- exp(cos(theta)) - a * cos(b * theta)
  radius <- radius + sin(theta/12)
  x <- radius * sin(theta)
  y <-  - radius * cos(theta)
  plot(x, y, type = "l", axes = FALSE, xlab = "", ylab = "", col = color)
}
