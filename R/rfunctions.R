#' Draws a butterfly as in the front cover of the book 
#' @param color This is the color to use in the plot. It can take any value 
#' that R can use for color, e.g. 1, 2, "blue" etc. 
#' @param p1 Parameter controlling the shape of the butterfly
#' @param p2 Second parameter controlling the shape of the butterfly
#' @return Plots a butterfly whose colour and shape are determined by the
#' supplied parameters.   
#' @examples 
#' butterfly(color = 6)
#' par(mfrow=c(2, 2))
#' butterfly(color = 6)
#' butterfly(p1=5, p2=5, color=2)
#' butterfly(p1=10, p2=1.5, color = "seagreen")
#' butterfly(p1=20, p2=4, color = "blue")
#' par(mfrow=c(1, 1))
#' @export
butterfly <- function(color = 2, p1=2, p2=4) {
  theta <- seq(from=0.0, to=24 * pi, len = 2000)
  radius <- exp(cos(theta)) - p1 * cos(p2 * theta)
  radius <- radius + sin(theta/12)
  x <- radius * sin(theta)
  y <-  - radius * cos(theta)
  plot(x, y, type = "l", axes = FALSE, xlab = "", ylab = "", col = color)
}
