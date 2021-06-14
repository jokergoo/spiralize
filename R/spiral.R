### a reference class for a spiral object

create_spiral = function(start = 360, end = 360*5, xlim = c(0, 1), scale_by = "angle", 
	flip = "none", reverse = FALSE) {
			
	b = 1/2/pi  # the radical distance between two neighbouring circles is 1

	start = start/180*pi
	end = end/180*pi

	xrange = xlim[2] - xlim[1]
	theta_lim = c(start, end)
	theta_range = end - start
	curve = local({
		b = b
		function(theta) {
			b*theta
		}
	})
	
	spiral_length = local({
		b = b
		function(theta) {
			b/2*(theta*sqrt(1+theta*theta) + log(theta+sqrt(1+theta*theta)))
		}
	})
	spiral_length_lim = spiral_length(c(start, end))
	spiral_length_range = spiral_length_lim[2] - spiral_length_lim[1]
	
    max_radius = max(b*end)
    dist = b * 2*pi

    spiral(
		b = b,
		xlim = xlim,
		xrange = xrange,
		theta_lim = theta_lim,
		theta_range = theta_range,
		curve = curve,
		spiral_length = spiral_length,
		spiral_length_lim = spiral_length_lim,
		spiral_length_range = spiral_length_range,
		max_radius = max_radius,
		dist = dist,
		scale_by = scale_by,
		flip = flip,
		reverse = reverse
	)
}

spiral = setRefClass("spiral",
	fields = list(
		b = "numeric",
		xlim = "numeric",
		xrange = "numeric",
		theta_lim = "numeric",
		theta_range = "numeric",
		curve = "function",  ## given theta, output r
		spiral_length = "function", ## given theta, output length
		spiral_length_lim = "numeric",
		spiral_length_range = "numeric",
		max_radius = "numeric",
		dist = "numeric",
		scale_by = "character",
		flip = "character",
		reverse = "logical"
	),
	methods = list(
		show = function() {
			qqcat(
"An Archimedean spiral (r = b*theta) with the following parameters:
  b: @{.self$b}
  distance between two neighbouring circles: @{.self$dist}
  xlim: [@{.self$xlim[1]}, @{.self$xlim[2]}] @{ifelse(.self$reverse, '(from outside of the curve to the inside)', '')}
  range of theta (in degree): [@{round(as.degree(.self$theta_lim[1], scale = FALSE))}, @{round(as.degree(.self$theta_lim[2], scale = FALSE))}]
  The spiral curve is equally intepolated by @{.self$scale_by}.
")
			if(.self$flip == "horizontal") {
				cat("  spiral is flipped horizontally.\n")
			} else if(.self$flip == "vertical") {
				cat("  spiral is flipped vertically.\n")
			} else if(.self$flip == "both") {
				cat("  spiral is flipped both horizontally and vertically.\n")
			}
		}
	)
)
