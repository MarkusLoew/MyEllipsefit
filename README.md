# MyEllipsefit

[![Build Status](https://travis-ci.org/MarkusLoew/MyEllipsefit.svg?branch=master)](https://travis-ci.org/MarkusLoew/MyEllipsefit)

R package to fit an ellipse to data points. Convience wrapper around the [conicfit](https://cran.r-project.org/web/packages/conicfit/index.html) package to provide ellipsis parameters (axes, angle, area, bounding box, and more). Offers an option to draw the ellipse as layer in ggplot2.

See 

        help(package = "MyEllipsefit") 

for details on the function provided by this package.

See 
        help(package = "conicfit")

for details on the *conicfit* package that provides the actual calculcations.

Function returns either a data frame with the fit parameters for the ellipse (Default). Or a list with two data frames, the fit parameters and the coordinates to draw the ellipse. 

Fit parameters are:
* The X coordinate of the center of the ellipse. 
* The Y coordinate of the center of the ellipse. 
* The distance from the center to the perimenter along the major axis. 
* The distance from the center to the perimenter along the minor axis. 
* The tilt angle of the ellipse. 
* The area of the ellipse. 

If *coords = TRUE*, returns a data frame with the coordinates of the ellipse in addition.

If *bbox = TRUE*, in addition, returns a data frame with the extreme values of the coordinates of the bounding box for the ellipse.

Items are returned as a named list. See example below.

### Installation

Installation straight from github (if package "devtools" is already installed) via

```{r}
devtools::install_github("MarkusLoew/MyEllipsefit")
```

### Example session
```{r}
library(MyEllipsefit)

mydata <- data.frame(x = c(5.92, 5.37, 3.16, 0.71, -0.29, -1.14, -0.8291667, 4.14, 10.74, 18.97, 21.66,  21.57, 21.56, 23.15, 24.17, 24.10, 23.26, 19.39, 12.31, 6.11, 7.49, 5.79, 2.66, 1.01),
                     y = c(0.14, 0.14, 0.10, 0.08, 0.08, 0.08, 0.12, 0.22, 0.36, 0.43, 0.42, 0.42, 0.43, 0.42, 0.37, 0.32, 0.26, 0.20, 0.12, 0.10, 0.14, 0.11, 0.07, 0.05))

ell <- Ellipsefit(mydata, x, y, coords = TRUE, bbox = TRUE)

coord <- ell$Coord
bbox <- ell$Bbox
plot(y ~ x, data = mydata, ylim = c(0, 0.5), xlim = c(-2, 25))
lines(y ~ x, data = coord, col = "blue")
abline(v = bbox$x, h = bbox$y, col = "red")

# Calculate bounding box area to compare to ellipse area
bbox.area <- (bbox$x[2] - bbox$x[1]) * (bbox$y[2] - bbox$y[1])
area.ratio <- ell$Para$Area / bbox.area


# comparison with ggplot2::stat_ellipse
library(ggplot2)

p <- ggplot(mydata, aes(x = x, y = y))
 p <- p + geom_point()
 p <- p + stat_myell()
 p <- p + stat_ellipse(colour = "red")
p
```
