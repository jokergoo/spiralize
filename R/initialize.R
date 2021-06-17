
# == title
# Initialize the spiral
#
# == param
# -xlim Range on x-locations.
# -start Start of the spiral, in degree. ``start`` and ``end`` should be positive and ``start`` should be smaller than ``end``.
# -end End of the spiral, in degree.
# -scale_by How scales on x-axis are equally interpolated? The values can be one of "angle" and "curve_length". If
#      the value is "angle", equal angle difference corresponds to equal difference of data. In this case, in outer loops,
#      the scales are longer than in the inner loops, although the difference on the data are the same. If
#      the value is "curve_length", equal curve length difference corresponds to the equal difference of the data.
# -flip How to flip the spiral? By default, the spiral starts from the origin of the coordinate and grows reverseclockwisely.
#       The argument controls the growing direction of the spiral.
# -reverse By default, the most inside of the spiral corresponds to the lower boundary of x-location. Setting the value to ``FALSE``
#        can reverse the direction.
# -polar_lines Whether draw the polar guiding lines.
# -polar_lines_by Increment of the polar lines. Measured in degree.
# -polar_lines_gp Graphics parameters for the polar lines.
# -padding Padding of the plotting region. The value can be a `grid::unit` of length of one to two.
# -newpage Whether to apply `grid::grid.newpage` before making the plot?
#
# == example
# spiral_initialize(); spiral_track()
# spiral_initialize(start = 180, end = 360+180); spiral_track()
# spiral_initialize(flip = "vertical"); spiral_track()
# spiral_initialize(flip = "horizontal"); spiral_track()
# spiral_initialize(flip = "both"); spiral_track()
# spiral_initialize(); spiral_track(); spiral_axis()
# spiral_initialize(scale_by = "curve_length"); spiral_track(); spiral_axis()
#
# # the following example shows the difference of `scale_by` more clearly:
# make_plot = function(scale_by) {
#     n = 100
#     require(circlize)
#     col = circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
#     spiral_initialize(xlim = c(0, n), scale_by = scale_by)
#     spiral_track(height = 0.9)
# 
#     x = runif(n)
#     spiral_rect(1:n - 1, 0, 1:n, 1, gp = gpar(fill = col(x), col = NA))
# }
# make_plot("angle")
# make_plot("curve_length")
spiral_initialize = function(xlim = c(0, 1), start = 360, end = 360*5, 
	scale_by = c("angle", "curve_length"), 
	flip = c("none", "vertical", "horizontal", "both"), reverse = FALSE,
	polar_lines = TRUE, polar_lines_by = 30, polar_lines_gp = gpar(col = "grey", lty = 3), 
	padding = unit(5, "mm"), newpage = TRUE) {

	spiral_clear(check_vp = FALSE)

	if(length(xlim) != 2) {
		stop_wrap("Length of `xlim` should be two.")
	}

	if(start < 0 || end < 0) {
		stop_wrap("`start` or `end` cannot be negative.")
	}
	if(start >= end) {
		stop_wrap("`start` should be smaller than `end`.")
	}

	scale_by = match.arg(scale_by)[1]
	flip = match.arg(flip)[1]
	spiral = create_spiral(start = start, end = end, xlim = xlim, scale_by = scale_by, flip = flip, reverse = reverse)
	spiral_env$spiral = spiral
	spiral_env$i_spiral = spiral_env$i_spiral + 1

	# push a viewport
	theta = seq(spiral$theta_lim[1], spiral$theta_lim[2], by = 2/180*pi)

	r = spiral$b*theta
	dist = spiral$dist

	df1 = polar_to_cartesian(theta, r)
	df2 = polar_to_cartesian(theta, r + dist)

	x = c(df1$x, rev(df2$x))
	y = c(df1$y, rev(df2$y))
	abs_max_x = max(abs(x))
	abs_max_y = max(abs(y))

	if(length(padding) == 1) {
		padding = rep(padding, 2)
	}
	if(newpage) {
		grid.newpage()
	}
	pushViewport(viewport(name = paste0("spiral_", spiral_env$i_spiral),
		width = unit(1, "snpc") - padding[1], 
		height = unit(1, "snpc") - padding[2],
		xscale = c(-abs_max_x, abs_max_x), yscale = c(-abs_max_y, abs_max_y)))

	if(polar_lines) {
		d = seq(0, 360, by = polar_lines_by)
		if(d[length(d)] == 360) d = d[-length(d)]
		dm = matrix(nrow = length(d), ncol = 4)
		for(i in seq_along(d)) {
			r0 = max(r + dist)*1.1
			dm[i, ] = c(0, 0, cos(d[i]/180*pi)*r0, sin(d[i]/180*pi)*r0)
		}
		grid.segments(dm[, 1], dm[, 2], dm[, 3], dm[, 4], default.units = "native", gp = polar_lines_gp)
	}
}

# == title
# Initialize the spiral from Date object
#
# == param
# -x A Date object.
# -xlim Range of the dates.
# -start Start of the spiral, in degrees. By default it is automatically calculated.
# -end End of the spiral, in degrees. By default it is automatically calculated.
# -year_per_loop How many years to put in a loop?
# -... All pass to `spiral_initialize`.
#
# == details
# Each year (actually 364 days) is in a loop.
#
# == example
# spiral_initialize_by_date(xlim = as.Date(c("2014-01-01", "2021-06-17")))
# spiral_track(height = 0.6)
# spiral_axis()
spiral_initialize_by_date = function(x = NULL, xlim = range(x), start = NULL, end = NULL,
	year_per_loop = 1, ...) {

	if(is.null(x) && missing(xlim)) {
		stop_wrap("You need to specify one of `x` and `xlim`.")
	}
	
	if(is.character(xlim)) {
		xlim = as.Date(xlim)
	}

	date_start = xlim[1]
	date_end = xlim[2]

	get_numeric_x = local({
		date_start = date_start
		function(x) {
			if(is.numeric(x)) {
				return(x)
			}
			if(is.character(x)) {
				x = as.Date(x)
			}
			as.double(x - date_start, "days")
		}
	})

	get_original_x = local({
		date_start = date_start
		function(x) {
			as.character(date_start + x)
		}
	})

	
	n = as.double(date_end - date_start, "days")
	xlim = as.double(xlim - date_start, "days") - 0.5

	start_of_the_year = as.Date("2021-01-01")
	year(start_of_the_year) = year(date_start)

	if(is.null(start)) {
		start = (1 - as.double(date_start - start_of_the_year, "days")/364)*360
	}

	if(is.null(end)) {
		end = 360*(n/364/year_per_loop) + start
	}

	spiral_initialize(xlim = xlim, start = start, end = end, ...)

	spiral = current_spiral()
	spiral$xclass = "Date"
	spiral$get_numeric_x = get_numeric_x
	spiral$get_original_x = get_original_x

	invisible(NULL)
}


spiral_initialize_by_time = function(x, xlim, period = NULL, by = NULL, multiply = 1, ...) {
	day = lubridate::day(x)
	hour = lubridate::hour(x)
	min = lubridate::minute(x)
}

# spiral_initialize_by_granges = function(x = NULL, xlim = range(start(x), end(x)), ...) {

# 	if(is.null(x) && missing(xlim)) {
# 		stop_wrap("You need to specify one of `x` and `xlim`.")
# 	}

# 	get_original_x = function(x) {
# 		x2 = character(length(x))
# 		l1 = x >= 1e6
# 		x2[l1] = paste(x[l1]/1000000, "MB", sep = "")
# 		l2 = x >= 1e3 & !l1
# 		x2[l2] = paste(x[l2]/1000, "KB", sep = "")
		
# 		l3 = !(l1 | l2)
# 		x2[l3] = paste(x[l3], "bp", sep = "")
# 		x2
# 	}

# 	spiral_initialize(xlim = xlim, ...)

# 	spiral = current_spiral()
# 	spiral$xclass = "GRanges"
# 	spiral$get_original_x = get_original_x

# 	invisible(NULL)
# }
