
# == title
# Convert data coordinates to the canvas coordinates
#
# == param
# -x X-locations of the data points.
# -y Y-locations of the data points.
# -track_index Index of the track. 
#
# == details
# The canvas coordinates correspond to the "native" coordinates of the viewport where the graphics are to be drawn.
#
# Note different settings of ``flip`` and ``reverse`` in `spiral_initialize` affect the conversion.
#
# == value
# A data frame with two columns: x and y.
xy_to_cartesian = function(x, y, track_index = current_track_index()) {
	df_polar = xy_to_polar(x, y, track_index = track_index)
	polar_to_cartesian(df_polar$theta, df_polar$r)
}

# == title
# Convert data coordinates to polar coordinates
#
# == param
# -x X-locations of the data points.
# -y Y-locations of the data points.
# -track_index Index of the track. 
#
# == details
# Note different settings of ``flip`` and ``reverse`` in `spiral_initialize` affect the conversion.
#
# == value
# A data frame with two columns: theta (in radians) and r (the radius).
xy_to_polar = function(x, y, track_index = current_track_index()) {

	if(track_index == 0) {
		stop_wrap("No track has been created. Maybe you need to call `spiral_track()` first.")
	}

	spiral = spiral_env$spiral

	if(spiral$scale_by == "angle") {
		theta = (x - spiral$xlim[1]) * spiral$theta_range/spiral$xrange + spiral$theta_lim[1]

		if(spiral$reverse) {
			theta = spiral$theta_lim[2] - (theta - spiral$theta_lim[1])
		}
	} else if(spiral$scale_by == "curve_length") {
		if(spiral$reverse) {
			len = (spiral$xlim[2] - x + spiral$xlim[1]) * spiral$spiral_length_range/spiral$xrange + spiral$spiral_length_lim[1]
			n = length(len)
			theta = numeric(n)
			for(i in seq_len(n)) {
				theta[i] = uniroot(function(theta, a) {
					spiral$spiral_length(theta) - a
				}, interval = spiral$spiral_length_lim, a = len[i])$root
			}
		} else {
			len = (x - spiral$xlim[1]) * spiral$spiral_length_range/spiral$xrange + spiral$spiral_length_lim[1]
			n = length(len)
			theta = numeric(n)
			for(i in seq_len(n)) {
				theta[i] = uniroot(function(theta, a) {
					spiral$spiral_length(theta) - a
				}, interval = spiral$spiral_length_lim, a = len[i])$root
			}
		}
	}

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	rmin = get_track_data("rmin", track_index)
	rmax = get_track_data("rmax", track_index)
	d = (y - ymin) * (rmax - rmin)/(ymax - ymin) + rmin
	r = spiral$curve(theta) + d

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

# note here polar can not be directly used in spirals
cartesian_to_polar = function(x, y) {
	theta = atan(y/x)
	r = sqrt(x*x + y*y)

	data.frame(theta = theta, r = r)
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

circular_extend = function(x, y, offset, track_index = current_track_index(), interval_extend = 1,
	coordinate = "cartesian") {
	
	spiral = spiral_env$spiral

	if(is.unit(offset)) {
		offset = convertWidth(offset, "native", valueOnly = TRUE)
	}

	if(spiral$scale_by == "curve_length") {
		x_offset = offset/spiral$spiral_length_range*spiral$xrange
		if(coordinate == "cartesian") {
			xy_to_cartesian(x + x_offset, y, track_index = track_index)
		} else if(coordinate == "polar") {
			xy_to_polar(x + x_offset, y)
		} else {
			data.frame(x = x + x_offset, y = y)
		}
	} else {
		df = xy_to_polar(x, y, track_index = track_index)
		v_offset = (df$r - get_track_data("rmin"))/spiral$dist
		t = get_theta_from_len(spiral$spiral_length(df$theta, v_offset) + offset, 
				c(spiral$theta_lim[1] - spiral$theta_range, 
				  spiral$theta_lim[2] + spiral$theta_range))
		r = spiral$curve(t) + (y - get_track_data("ymin", track_index))/get_track_data("yrange", track_index)*get_track_data("rrange", track_index)
		for(i in seq_len(track_index - 1)) {
			r = r + get_track_data("rrange", i)
		}
		if(coordinate == "cartesian") {
			polar_to_cartesian(t, r)
		} else if(coordinate == "polar") {
			data.frame(theta = t, r = r)
		} else {
			data.frame(x = polar_to_x(t), y = y)
		}
	}
}

# in xy coordinates
circular_extend_on_x = function(x, y, offset, track_index = track_index, coordinate = "polar") {
	spiral = spiral_env$spiral

	if(is.unit(offset)) {
		offset = convertWidth(offset, "native", valueOnly = TRUE)
	}

	if(spiral$scale_by == "curve_length") {
		x_offset = offset/spiral$spiral_length_range*spiral$xrange
		if(coordinate == "polar") {
			xy_to_polar(x + x_offset, y)$theta
		} else {
			x + x_offset
		}
	} else {
		df = xy_to_polar(x, y, track_index = track_index)
		v_offset = (y - get_track_data("ymin", track_index))*get_track_data("yrange", track_index)*(get_track_data("rmax", track_index) - get_track_data("rmin", track_index))
		if(track_index > 1) {
			for(i in 1:(track_index - 1))
			v_offset = v_offset + get_track_data("rmax", i) - get_track_data("rmin", i)
		}
		v_offset = v_offset/spiral$dist

		t = get_theta_from_len(spiral$spiral_length(df$theta, v_offset) + offset, 
				c(spiral$spiral_length(-spiral$theta_lim[1], max(v_offset)), 
				  spiral$spiral_length(2*spiral$theta_lim[2], max(v_offset))),
				v_offset)
		if(coordinate == "polar") {
			t
		} else {
			polar_to_x(t)
		}
	}
}

polar_to_x = function(theta) {

	spiral = spiral_env$spiral

	if(spiral$scale_by == "angle") {
		x = (theta - spiral$theta_lim[1])/spiral$theta_range*spiral$xrange + spiral$xlim[1]
	} else {
		x = (spiral$spiral_length(theta) - spiral$spiral_length_lim[1])/spiral$spiral_length_range*spiral$xrange + spiral$xlim[1]
	}

	if(spiral$reverse) {
		x = spiral$xlim[2] - x + spiral$xlim[1]
	}
	x
}

convert_to_y = function(offset, track_index = current_track_index()) {
	offset = convertWidth(offset, "native", valueOnly = TRUE)
	offset/get_track_data("rrange", track_index)*get_track_data("yrange", track_index) + get_track_data("ymin", track_index)
}

convert_height_to_y = convert_to_y


get_theta_from_x = function(x) {
	spiral = spiral_env$spiral

	(x - spiral$xlim[1])/spiral$xrange*spiral$theta_range + spiral$theta_lim[1]
}

# from the spiral length
get_theta_from_len = function(len, interval, offset = 0) {
	spiral = spiral_env$spiral
	n = length(len)
	t = numeric(n)

	offset = offset
	if(length(offset) == 1) offset = rep(offset, n)
	for(i in seq_len(n)) {
		t[i] = uniroot(function(theta, a) {
			spiral$spiral_length(theta, offset[i]) - a
		}, interval = interval, a = len[i])$root
	}
	t
}

