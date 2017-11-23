#' Plot data ellipses in ggplot2 based on MyEllipsefit
#' 
#' @description Calculates an elliptic fit of data as a \code{ggplot2} layer. Calculated using \code{MyEllipsefit::Ellipsefit}
# '@inheritParams StatMyEll
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_ellipse
#' @seealso  \code{Ellipsefit}, \code{conicfit}, \code{ggplot2}
#' @examples
#' mydata <- data.frame(x = c(5.92, 5.37, 3.16, 0.71, -0.29, -1.14, -0.8291667, 4.14, 10.74, 18.97, 21.66,  21.57, 21.56, 23.15, 24.17, 24.10, 23.26, 19.39, 12.31, 6.11, 7.49, 5.79, 2.66, 1.01),
#'                      y = c(0.14, 0.14, 0.10, 0.08, 0.08, 0.08, 0.12, 0.22, 0.36, 0.43, 0.42, 0.42, 0.43, 0.42, 0.37, 0.32, 0.26, 0.20, 0.12, 0.10, 0.14, 0.11, 0.07, 0.05))
#' library(ggplot2)
#' p <- ggplot(mydata, aes(x = x, y = y))
#'  p <- p + geom_point()
#'  p <- p + stat_myell()
#' p
#'
#' # comparison with ggplot2:stat_ellipse
#' p + stat_ellipse(colour = "red") 
#' @export stat_myell

stat_myell <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatMyEll, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatMyEll <- ggplot2::ggproto("StatMyEll", ggplot2::Stat, 
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales) {
    ell <- Ellipsefit(data, data$x, data$y, coords = TRUE)
    ell$Coord
    }
)
