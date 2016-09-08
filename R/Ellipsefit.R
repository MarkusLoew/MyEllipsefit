#' Calculate an Ellipsefit using conicfit package
#' 
#' @param data Name of data frame
#' @param x name of x-vector in data
#' @param y names of y-vector in data 
#' @param coords Logical. If TRUE, function returns a list of ellipse fit parameters and coordinates of the resulting ellipse. If FALSE, returns the ellipse fit parameters only. Default is FALSE.
#' @param bbox Logical. If TRUE, function returns the extremes of the ellipse coordinates. These coordinares can be used to draw a bounding box around the ellipse. Only available when coords = TRUE. Default is FALSE.
#' @return Either a data frame with the fit parameters for the ellipse (Default). Or a list with two data frames, the fit parameters and the coordinates to draw the ellipse. Fit parameters are:  The X coordinate of the center of the ellipse. The Y coordinate of the center of the ellipse. The distance from the center to the perimenter along the major axis. The distance from the center to the perimenter along the minor axis. The tilt angle of the ellipse. The area of the ellipse. If bbox is set to TRUE, in addition returns a data frame with the extreme values of the coordinates of the bounding box for the ellipse.
#' @examples 
#' \dontrun{
#' Ellipsefit(eg.hour, temp.hour, Pp.hour, coords = TRUE)
#' Ellipsefit(eg.hour, temp.hour, Pp.hour)
#' }
#' mydata <- data.frame(x = c(5.92, 5.37, 3.16, 0.71, -0.29, -1.14, -0.8291667, 4.14, 10.74, 18.97, 21.66,  21.57, 21.56, 23.15, 24.17, 24.10, 23.26, 19.39, 12.31, 6.11, 7.49, 5.79, 2.66, 1.01),
#'                     y = c(0.14, 0.14, 0.10, 0.08, 0.08, 0.08, 0.12, 0.22, 0.36, 0.43, 0.42, 0.42, 0.43, 0.42, 0.37, 0.32, 0.26, 0.20, 0.12, 0.10, 0.14, 0.11, 0.07, 0.05))
#' ell <- Ellipsefit(mydata, x, y, coords = TRUE, bbox = TRUE)
#' coord <- ell$Coord
#' bbox <- ell$Bbox
#' plot(y ~ x, data = mydata, ylim = c(0, 0.5), xlim = c(-2, 25))
#' lines(y ~ x, data = coord, col = "blue")
#' abline(v = bbox$x, h = bbox$y, col = "red")
#' # Calculate bounding box area to compare to ellipse area
#' bbox.area <- (bbox$x[2] - bbox$x[1]) * (bbox$y[2] - bbox$y[1])
#' area.ratio <- ell$Para$Area / bbox.area
#' @export

Ellipsefit <- function(data, x, y, coords = FALSE, bbox = FALSE) {
   arguments <- as.list(match.call())
   x = eval(arguments$x, data)
   y = eval(arguments$y, data)
   # assemble a matrix from x, y
   xy <- as.matrix(stats::na.omit(data.frame(x, y)))
   
   if (nrow(xy) > 0) {
   
   # from conicfit help:
   # applies the algebraic ellipse fit method by Fitzgibbon-Pilu-Fisher
   # returns vector for fitting ellipse: ax^2 + bxy + cy^2 +dx + ey + f = 0
   ellipara <- try(conicfit::EllipseDirectFit(xy))
   
   if (inherits(ellipara, "try-error")) {
	geopara.out <- as.data.frame(t(rep(NA, 6)))
	names(geopara.out) <- c("Centerpoint_X", "Centerpoint_Y", 
			        "Axis_A", "Axis_B", "Angle", "Area")
	return(geopara.out)
   } else {
   
	# AtoG converts algebraic parameters (A, B, C, D, E, F) 
	# to geometric parameters (Center(1:2), Axes(1:2), Angle)
	geopara <- conicfit::AtoG(ellipara)$Par
	geopara.out <- as.data.frame(t(geopara))
	
	#    The X coordinate of the center of the ellipse.
	#    The Y coordinate of the center of the ellipse.
	#    The distance from the center to the perimeter along the major axis.
	#    The distance from the center to the perimeter along the minor axis.
	#    The tilt angle of the ellipse (counter-clockwise from X axis). 
	#    Area of the ellipse pi * major axis * minor axis
	names(geopara.out) <- c("Centerpoint_X", "Centerpoint_Y", 
	                        "Axis_A", "Axis_B", "Angle")
	geopara.out$Area <- pi * geopara.out$Axis_A * geopara.out$Axis_B
	#print(geopara.out)
	
	if (coords == FALSE) {
	   return(geopara.out)
	   } else {
   
		# http://math.stackexchange.com/questions/793289/plotting-an-ellipse-after-an-ellipse-fit
		# calculate ellipse from the geometric parameters
		# returns matrix of coordinates
		xycoord <- conicfit::calculateEllipse(geopara[1], 
				            geopara[2], 
				            geopara[3], 
				            geopara[4], 
				            180 / pi * geopara[5])
		xycoord <- as.data.frame(xycoord)
		names(xycoord) <- c("x", "y")
		# create return object, as list with parameters and coordinates
		out <- list(geopara.out, xycoord)
		names(out) <- c("Para", "Coord")
				
		if (bbox == TRUE) {
			max.x <- max(xycoord$x)
			min.x <- min(xycoord$x)
			max.y <- max(xycoord$y)
			min.y <- min(xycoord$y)
			bottom.left <- c(min.x, min.y)
			bottom.right <- c(max.x, min.y)
			top.left <- c(min.x, max.y)
			top.right <- c(max.x, max.y)
			#bbox.list <- c(bottom.left, bottom.right, top.left, top.right)
			#names(bbox.list) <- c("bottom.left.x", "bottom.left.y", "bottom.right.x", "bottom.right.y", "top.left.x", "top.left.y", "top.right.x", "top.right.y")
			bbox.df <- data.frame(x = c(min.x, max.x),
			                      y = c(min.y, max.y))
                        rownames(bbox.df) <- c("Minima", "Maxima")
			bbox.out <- list(geopara.out, xycoord, bbox.df)
			names(bbox.out) <- c("Para", "Coord", "Bbox")
		        return(bbox.out)
		} else {
		return(out)
		}
	   }
   	} 
   } else {
   warning("nrow(xy) is not > 0")
   geopara.out <- as.data.frame(t(rep(NA, 6)))
   names(geopara.out) <- c("Centerpoint_X", "Centerpoint_Y", 
                            "Axis_A", "Axis_B", "Angle", "Area")
   return(geopara.out)
   }
}
