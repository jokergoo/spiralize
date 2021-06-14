\name{spiral_initialize}
\alias{spiral_initialize}
\title{
Initialize the spiral
}
\description{
Initialize the spiral
}
\usage{
spiral_initialize(start = 360, end = 360*5, xlim = c(0, 1),
    scale_by = c("angle", "curve_length"),
    flip = c("none", "vertical", "horizontal", "both"), reverse = FALSE,
    polar_lines = TRUE, polar_lines_by = 30, polar_lines_gp = gpar(col = "grey", lty = 3),
    padding = unit(5, "mm"), newpage = TRUE)
}
\arguments{

  \item{start}{Start of the spiral, in degree.}
  \item{end}{End of the spiral, in degree.}
  \item{xlim}{Range on x coordinates.}
  \item{scale_by}{How scales on x-axis are equally intepolated. The values can be one of "angle" and "curve_length". If the value is "angle", equal angle difference corresponds to equal difference of data. In this case, outer circle, the scales have larger distance than the inter circle, although the difference of the data are the same. If the value is "curve_length", equal curve length difference corresponds to the equal difference of the data.}
  \item{flip}{How the flip the spiral curve. By default, the spiral starts from the center of the coordinate and grows reverseclockwisely. The argument can control the orientation of the spiral.}
  \item{reverse}{By default, the most inside of the spiral corresponds to the lower boundary of x coordinate. Setting the value to \code{FALSE} can reverse the direction.}
  \item{polar_lines}{Whether draw the polar guiding lines.}
  \item{polar_lines_by}{Increment of the polar lines. Measures in degree.}
  \item{polar_lines_gp}{Graphics parameters for the polar lines.}
  \item{padding}{Padding of the spiral. The value can be a \code{\link[grid]{unit}} of length of one to two.}
  \item{newpage}{Whether to apply \code{\link[grid]{grid.newpage}} befoer making the plot?}

}
\examples{
spiral_initialize(); spiral_track()
spiral_initialize(start = 180, end = 360+180); spiral_track()
spiral_initialize(flip = "vertical"); spiral_track()
spiral_initialize(flip = "horizontal"); spiral_track()
spiral_initialize(flip = "both"); spiral_track()
spiral_initialize(); spiral_track(); spiral_axis()
spiral_initialize(scale_by = "curve_length"); spiral_track(); spiral_axis()

# the following example shows the difference of `scale_by` more clearly:
make_plot = function(scale_by) {
    n = 100
    require(circlize)
    col = circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
    spiral_initialize(xlim = c(0, n), scale_by = scale_by)
    spiral_track(height = 0.9)

    x = runif(n)
    spiral_rect(1:n - 1, 0, 1:n, 1, gp = gpar(fill = col(x), col = NA))
}
make_plot("angle")
make_plot("curve_length")
}
