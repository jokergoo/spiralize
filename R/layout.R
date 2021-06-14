
# == title
# Initialize the spiral
#
# == param
# -start Start of the spiral, in degree.
# -end End of the spiral, in degree.
# -xlim Range on x coordinates.
# -scale_by How scales on x-axis are equally intepolated. The values can be one of "angle" and "curve_length". If
#      the value is "angle", equal angle difference corresponds to equal difference of data. In this case, outer circle,
#      the scales have larger distance than the inter circle, although the difference of the data are the same. If
#      the value is "curve_length", equal curve length difference corresponds to the equal difference of the data.
# -flip How the flip the spiral curve. By default, the spiral starts from the center of the coordinate and grows reverseclockwisely.
#       The argument can control the orientation of the spiral.
# -reverse By default, the most inside of the spiral corresponds to the lower boundary of x coordinate. Setting the value to ``FALSE``
#        can reverse the direction.
# -polar_lines Whether draw the polar guiding lines.
# -polar_lines_by Increment of the polar lines. Measures in degree.
# -polar_lines_gp Graphics parameters for the polar lines.
# -padding Padding of the spiral. The value can be a `grid::unit` of length of one to two.
# -newpage Whether to apply `grid::grid.newpage` befoer making the plot?
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
spiral_initialize = function(start = 360, end = 360*5, xlim = c(0, 1), 
	scale_by = c("angle", "curve_length"), 
	flip = c("none", "vertical", "horizontal", "both"), reverse = FALSE,
	polar_lines = TRUE, polar_lines_by = 30, polar_lines_gp = gpar(col = "grey", lty = 3), 
	padding = unit(5, "mm"), newpage = TRUE) {

	spiral_clear(check_vp = FALSE)

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
# Clear the spiral curve
#
# == param
# -check_vp Whether to check the viewport.
#
# == details
# It basically sets the internally spiral object to NULL, and reset all global options.
spiral_clear = function(check_vp = TRUE) {
	spiral_env$spiral = NULL
	track_env$track_data = empty_track_data
	track_env$current_track = 0
	spiral_opt(RESET = TRUE)

	if(check_vp) {
		vp = current_spiral_vp()
		while(1) {
			if(current.viewport()$name == vp) {
				popViewport()
				break
			}
			if(current.viewport()$name == "ROOT") {
				break
			}
			popViewport()
		}
	}
}

# == title
# Add a track or go to an existed track
#
# == param
# -ylim Data range of the y coordinates.
# -height Height of the track. The value should be the fraction of the distance of the two neighbouring circles.
# -background Whether draw the background of the track, i.e. border and filled color of background.
# -background_gp Graphics parameters of the background.
# -track_index Index of the track. 
#
# == details
# If the track is already existed, the function simply mark the track as the current track and does nothing else.
#
# == example
# spiral_initialize()
# spiral_track(height = 0.8)
#
# spiral_initialize()
# spiral_track(height = 0.4, background_gp = gpar(fill = "red"))
# spiral_track(height = 0.2, background_gp = gpar(fill = "green"))
# spiral_track(height = 0.1, background_gp = gpar(fill = "blue"))
spiral_track = function(ylim = c(0, 1), height = 0.5, background = TRUE, 
	background_gp = gpar(fill = "#EEEEEE"), track_index = current_track_index() + 1) {

	spiral = spiral_env$spiral
	dist = spiral$dist

	if(track_existed(track_index)) {
		# only reset current track
		set_current_track(track_index)
	} else {
		# a new track
		if(!track_existed(track_index - 1)) {
			stop_wrap(qq("There are only @{n_tracks()} existed. The value of `track_index` should not be larger than @{n_tracks() + 1}."))
		} else {
			sum_height = sum(track_env$track_data[, "rel_height"])
			new_track_data = data.frame(i = track_index, 
				ymin = ylim[1], ymax = ylim[2], 
				rmin = sum_height*dist, rmax = (sum_height + height)*dist, 
				rel_height = height)
			add_track(track_index, new_track_data)
		}

		if(background) {
			spiral_rect(spiral$xlim[1], get_track_data("ymin"), spiral$xlim[2], get_track_data("ymax"), gp = background_gp)
		}
	}
}

# == title
# Information of the current spiral curve
#
# == details
# It prints some information of the current spiral.
spiral_info = function() {
	spiral = spiral_env$spiral
	if(is.null(spiral)) {
		cat("No spiral has been initialized.\n")
	} else {
		print(spiral_env$spiral)

		cat("\n")
		nt = n_tracks()
		if(nt < 1) {
			cat("  No track has been created.\n")
		} else {
			for(i in seq_len(nt)) {
				qqcat("  track @{i}:\n")
				qqcat("    ylim: [@{get_track_data('ymin', i)}, @{get_track_data('ymax', i)}]\n")
				qqcat("    height: @{get_track_data('rel_height', i)} (fraction of the distance of two neighbour circles)\n")
				if(i < nt) cat("\n")
			}
		}
	}
}

