
# == title
# Convert coordinate from data coordinate to the canvas coordinate
#
# == param
# -x X coordinates of the data points.
# -y Y coordinates of the data points.
# -track_index Index of the track. 
#
# == details
# The canvas coordinate system corresponds to the "native unit" coordinate of the viewport where the graphics are to be drawn.
#
# Note different settings of ``flip`` and ``reverse`` set in `spiral_initialize` affect the conversion.
#
# == value
# A data frame with two columns: x and y.
xy_to_cartesian = function(x, y, track_index = current_track_index()) {
	df_polar = xy_to_polar(x, y, track_index = track_index)
	polar_to_cartesian(df_polar$theta, df_polar$r)
}

# == title
# Convert coordinate from data coordinate to polar coordinate
#
# == param
# -x X coordinates of the data points.
# -y Y coordinates of the data points.
# -track_index Index of the track. 
#
# == details
# Note different settings of ``flip`` and ``reverse`` set in `spiral_initialize` affect the conversion.
#
# == value
# A data frame with two columns: theta and r.
xy_to_polar = function(x, y, track_index = current_track_index()) {

	if(track_index == 0) {
		stop_wrap("No track has been created. Maybe you need to call `spiral_track()` first.")
	}

	spiral = spiral_env$spiral

	if(spiral$scale_by == "angle") {
		theta = (x - spiral$xlim[1]) * spiral$theta_range/spiral$xrange + spiral$theta_lim[1]
	} else if(spiral$scale_by == "curve_length") {
		len = (x - spiral$xlim[1]) * spiral$spiral_length_range/spiral$xrange + spiral$spiral_length_lim[1]
		n = length(len)
		theta = numeric(n)
		for(i in seq_len(n)) {
			theta[i] = uniroot(function(theta, a) {
				spiral$spiral_length(theta) - a
			}, interval = spiral$spiral_length_lim, a = len[i])$root
		}
	}

	if(spiral$reverse) {
		theta = spiral$theta_lim[2] - (theta - spiral$theta_lim[1])
	}

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	rmin = get_track_data("rmin", track_index)
	rmax = get_track_data("rmax", track_index)
	d = (y - ymin) * (rmax - rmin)/(ymax - ymin) + rmin
	r = spiral$curve(theta) + d

	spiral = spiral_env$spiral
	if(spiral$flip == "horizontal") {
		theta = 2*pi - theta 
	} else if(spiral$flip == "vertical") {
		theta = pi - theta
	} else if(spiral$flip == "both") {
		theta = 2*pi - theta 
		theta = pi - theta
	}

	data.frame(theta = theta, r = r)
}

polar_to_cartesian = function(theta, r) {
	x = cos(theta)*r
	y = sin(theta)*r
	
	data.frame(x = x, y = y)
}

spiral_lines_expand = function(x, y, d = spiral_opt$min_segment_len, track_index = current_track_index()) {

	df = xy_to_polar(x, y, track_index = track_index)
	n = nrow(df)

	## here x and y are theta and r
	x = df$theta
	y = df$r
	nx = x[1]
	ny = y[1]
	for(i in seq_len(n)) {
		if(i == 1) {
			next
		}
		if(is.na(x[i]) || is.na(y[i])) {
            nx = c(nx, NA)
            ny = c(ny, NA)
            next
        }
        if(is.na(x[i-1]) || is.na(y[i-1])) {
            next
        }

        if(abs(x[i] - x[i-1]) <= d) {
        	nc = 2
        } else {
        	nc = ceiling(abs(x[i] - x[i-1])/d)
        }

        nx = c(nx, seq(x[i-1], x[i], length.out = nc)[-1])
        ny = c(ny, seq(y[i-1], y[i], length.out = nc)[-1])
	}

	polar_to_cartesian(nx, ny)
}


radical_extend = function(x, y, offset, track_index = current_track_index()) {
	df = xy_to_polar(x, y, track_index = track_index)
	offset_x = convertWidth(offset*cos(df$theta), "native", valueOnly = TRUE)
	offset_y = convertHeight(offset*sin(df$theta), "native", valueOnly = TRUE)
	df = polar_to_cartesian(df$theta, df$r)
	df2 = df
	df2$x = df2$x + offset_x
	df2$y = df2$y + offset_y

	df2
}

circular_extend = function(x, y, offset, track_index = current_track_index(), interval_extend = 0) {
	
	spiral = spiral_env$spiral

	if(is.unit(offset)) {
		offset = convertWidth(offset, "native", valueOnly = TRUE)
	}

	if(spiral$scale_by == "curve_length") {
		x_offset = offset/(spiral$spiral_length(spiral$theta_lim[2]) - spiral$spiral_length(spiral$theta_lim[1]))*(spiral$xlim[2] - spiral$xlim[1])
		xy_to_cartesian(x + x_offset, y, track_index = track_index)
	} else {
		df = xy_to_polar(x, y, track_index = track_index)
		t = get_theta_from_len(spiral$spiral_length(df$theta) + offset, c(spiral$theta_lim[1] - spiral$theta_range*interval_extend, spiral$theta_lim[2] + spiral$theta_range*interval_extend))
		r = spiral$curve(t) + (y - get_track_data("ymin", track_index))/get_track_data("yrange", track_index)*get_track_data("rrange", track_index)
		for(i in seq_len(track_index - 1)) {
			r = r + get_track_data("rrange", i)
		}
		polar_to_cartesian(t, r)
	}
}

convert_to_y = function(offset, track_index = current_track_index()) {
	offset = convertWidth(offset, "native", valueOnly = TRUE)
	offset/get_track_data("rrange", track_index)*get_track_data("yrange", track_index) + get_track_data("ymin", track_index)
}



