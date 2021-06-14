\name{spiral_raster}
\alias{spiral_raster}
\title{
Add image to a track
}
\description{
Add image to a track
}
\usage{
spiral_raster(x, y, image, width = NULL, height = NULL,
    facing = c("downward", "inside", "outside"),
    track_index = current_track_index())
}
\arguments{

  \item{x}{X coordinates of the center of the image.}
  \item{y}{Y coordinates of the center of the image.}
  \item{image}{A vector of file paths of images. The format of the image is inferred from the suffix name of the image file. NA values or empty strings in the vector means no image to drawn. Supported formats are png/svg/pdf/eps/jpeg/jpg/tiff.}
  \item{width}{Width of the image. The value should be a \code{\link[grid]{unit}} object.}
  \item{height}{Height of the image. The value should be a \code{\link[grid]{unit}} object. It is suggested to only set one of \code{width} and \code{height}, the other dimension will be automatically calcualted from the aspect ratio of the image.}
  \item{facing}{Facing of the image.}
  \item{track_index}{Index of the track. }

}
\examples{
image = system.file("extdata", "Rlogo.png", package = "circlize")
x = seq(0.1, 0.9, length = 10)

spiral_initialize()
spiral_track()
spiral_raster(x, 0.5, image)

spiral_initialize()
spiral_track()
spiral_raster(x, 0.5, image, facing = "inside")
}
