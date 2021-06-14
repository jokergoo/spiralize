


# == title
# Add points to a track
#
# == param
# -x X coordinates of the data points.
# -y Y coordinates of the data points.
# -pch Point type.
# -size Size of the points. Value should be a `grid::unit` object.
# -gp Graphical parameters.
# -track_index Index of the track. 
#
# == example
# spiral_initialize()
# spiral_track()
# spiral_points(x = runif(1000), y = runif(1000))
spiral_points = function(x, y, pch = 1, size = unit(0.4, "char"), gp = gpar(), 
	track_index = current_track_index()) {

	df = xy_to_cartesian(x, y, track_index = track_index)
	x = unit(df$x, "native")
	y = unit(df$y, "native")

	grid.points(x, y, pch = pch, size = size, gp = gp)
}

# == title
# Add lines to a track
#
# == param
# -x X coordinates of the data points.
# -y Y coordinates of the data points.
# -type Type of the line. Value should be one of "l" and "h". When the value is "h", vertical lines relative to the baseline will be drawn.
# -gp Graphical parameters.
# -baseline Baseline used when ``type`` is ``"l"`` or ``area`` is ``TRUE``.
# -area Whether to draw the area under the lines? Note ``gpar(fill)`` controls the filled of the areas.
# -track_index Index of the track. 
#
# == example
# x = sort(runif(1000))
# y = runif(1000)
# spiral_initialize()
# spiral_track()
# spiral_lines(x, y)
#
# spiral_initialize()
# spiral_track()
# spiral_lines(x, y, type = "h")
#
# spiral_initialize()
# spiral_track()
# spiral_lines(x, y, area = TRUE, gp = gpar(fill = "red", col = NA))
spiral_lines = function(x, y, type = "l", gp = gpar(),
	baseline = "bottom", area = FALSE, track_index = current_track_index()) {

	if(baseline == "bottom") {
		baseline = get_track_data("ymin", track_index)
	} else if(baseline == "top") {
		baseline = get_track_data("ymax", track_index)
	}

	if(type == "l") {
		if(area) {
			n = length(x)
			x = c(x, x[n], x[1])
			y = c(y, baseline, baseline)
			spiral_polygon(x, y, gp = gp, track_index = track_index)
		} else {
			df = spiral_lines_expand(x, y, track_index = track_index)
			grid.lines(df$x, df$y, default.units = "native", gp = gp)
		}
	}
	if(type == "h") {
		df1 = xy_to_cartesian(x, y, track_index = track_index)
		df2 = xy_to_cartesian(x, baseline, track_index = track_index)
		if(!"lineend" %in% names(gp)) {
			gp$lineend = "butt"
		}
		grid.segments(df1$x, df1$y, df2$x, df2$y, default.units = "native", gp = gp)
	}

}

# == title
# Add segments to a track
#
# == param
# -x0 X coordinates of the start points of the segments.
# -y0 Y coordinates of the start points of the segments.
# -x1 X coordinates of the end points of the segments.
# -y1 Y coordinates of the end points of the segments.
# -gp Graphical parameters.
# -track_index Index of the track. 
# -buffer Number of segments to buffer.
#
# == example
# n = 1000
# x0 = runif(n)
# y0 = runif(n)
# x1 = x0 + runif(n, min = -0.01, max = 0.01)
# y1 = 1 - y0
#
# spiral_initialize(xlim = range(c(x0, x1)))
# spiral_track()
# spiral_segments(x0, y0, x1, y1, gp = gpar(col = circlize::rand_color(n)))
#
spiral_segments = function(x0, y0, x1, y1, gp = gpar(), 
	track_index = current_track_index(), buffer = 10000) {
	
	n = length(x0)
	
	if("col" %in% names(gp)) {
		if(length(gp$col) == 1) gp$col = rep(gp$col, n)
	} else {
		gp$col = rep(get.gpar("col")$col, n)
	}
	if("lwd" %in% names(gp)) {
		if(length(gp$lwd) == 1) gp$lwd = rep(gp$lwd, n)
	} else {
		gp$lwd = rep(get.gpar("lwd")$lwd, n)
	}
	if("lty" %in% names(gp)) {
		if(length(gp$lty) == 1) gp$lty = rep(gp$lty, n)
	} else {
		gp$lty = rep(get.gpar("lty")$lty, n)
	}

	k = buffer
	ir = 0
	df_store = data.frame(x0 = rep(NA_real_, k), y0 = rep(NA_real_, k), x1 = rep(NA_real_, k), y1 = rep(NA_real_, k),
		col = vector(k, mode = mode(gp$col)), lwd = vector(k, mode = mode(gp$lwd)), lty = vector(k, mode = mode(gp$lty)))

	for(i in seq_len(n)) {
		df = spiral_lines_expand(c(x0[i], x1[i]), c(y0[i], y1[i]), track_index = track_index)
		nb = nrow(df)
		df2 = data.frame(x0 = numeric(nb - 1), y0 = numeric(nb - 1), x1 = numeric(nb - 1), y1 = numeric(nb - 1))
		df2$x0 = df$x[1:(nb-1)]
		df2$y0 = df$y[1:(nb-1)]
		df2$x1 = df$x[2:nb]
		df2$y1 = df$y[2:nb]
		df2$col = gp$col[i]
		df2$lwd = gp$lwd[i]
		df2$lty = gp$lty[i]

		nb2 = nrow(df2)

		if(ir + nb2 > k) {
			# draw the segments
			df_store = df_store[!is.na(df_store$x0), ]
			grid.segments(df_store$x0, df_store$y0, df_store$x1, df_store$y1, gp = gpar(col = df_store$col, lwd = df_store$lwd, lty = df_store$lty), default.units = "native")

			ir = 0
			df_store = data.frame(x0 = rep(NA_real_, k), y0 = rep(NA_real_, k), x1 = rep(NA_real_, k), y1 = rep(NA_real_, k),
				col = character(k), lwd = integer(k), lty = integer(k))
		}

		if(nb2 > k) {
			grid.segments(df2$x0, df2$y0, df2$x1, df2$y1, gp = gpar(col = df2$col, lwd = df2$lwd, lty = df2$lty), default.units = "native")
			next
		}

		ind = ir + 1:nb2
		df_store$x0[ind] = df2$x0
		df_store$y0[ind] = df2$y0
		df_store$x1[ind] = df2$x1
		df_store$y1[ind] = df2$y1
		df_store$col[ind] = df2$col
		df_store$lwd[ind] = df2$lwd
		df_store$lty[ind] = df2$lty
		ir = ir + nb2
		
	}
	if(ir > 0) {
		df_store = df_store[!is.na(df_store$x0), ]
		grid.segments(df_store$x0, df_store$y0, df_store$x1, df_store$y1, gp = gpar(col = df_store$col, lwd = df_store$lwd, lty = df_store$lty), default.units = "native")
	}
}


spiral_radical_segments = function(x, y, offset, gp = gpar(), track_index = current_track_index()) {
	df = xy_to_cartesian(x, y, track_index = track_index)
	df2 = radical_extend(x, y, offset, track_index = track_index)

	grid.segments(df$x, df$y, df2$x, df2$y, default.units = "native", gp = gp)
}

# == title
# Add rectangles to a track
#
# == param
# -xleft X coordinates of the left bottom angle of the rectangles.
# -ybottom Y coordinates of the left bottom angle of the rectangles.
# -xright X coordinates of the right top angle of the rectangles.
# -ytop Y coordinates of the right top angle of the rectangles.
# -gp Graphical parameters.
# -track_index Index of the track. 
#
# == example
# # to simulate heatmap
# n = 1000
# require(circlize)
# col = circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
# spiral_initialize(xlim = c(0, n))
# spiral_track(height = 0.9)
#
# x1 = runif(n)
# spiral_rect(1:n - 1, 0, 1:n, 0.5, gp = gpar(fill = col(x1), col = NA))
# x2 = runif(n)
# spiral_rect(1:n - 1, 0.5, 1:n, 1, gp = gpar(fill = col(x2), col = NA))
#
spiral_rect = function(xleft, ybottom, xright, ytop, gp = gpar(), 
	track_index = current_track_index()) {
	
	n1 = length(xleft)
    n2 = length(ybottom)
    n3 = length(xright)
    n4 = length(ytop)
    n = max(c(n1, n2, n3, n4))
    if(n1 == 1) xleft = rep(xleft, n)
    if(n2 == 1) ybottom = rep(ybottom, n)
    if(n3 == 1) xright = rep(xright, n)
    if(n4 == 1) ytop = rep(ytop, n)

    x = NULL
	y = NULL
	id = NULL
	id_k = 0
	for(i in seq_len(n)) {
		x = c(x, c(xleft[i], xleft[i], xright[i], xright[i], xleft[i]))
		y = c(y, c(ybottom[i], ytop[i], ytop[i], ybottom[i], ybottom[i]))
		id_k = id_k + 1
		id = c(id, rep(id_k, 5))
	}

	spiral_polygon(x, y, id = id, gp = gp, track_index = track_index)
}

# == title
# Add rectangles to a track
#
# == param
# -pos X coordinates of the left bottom angle of the rectangles.
# -value Y coordinates of the left bottom angle of the rectangles.
# -baseline Baseline of the bars. Note it only works when ``value`` is a simple vector.
# -bar_width X coordinates of the right top angle of the rectangles.
# -gp Graphical parameters.
# -track_index Index of the track. 
#
# == example
# x = seq(1, 1000, by = 1) - 0.5
# y = runif(1000)
# spiral_initialize(xlim = c(0, 1000))
# spiral_track(height = 0.8)
# spiral_barplot(x, y)
#
# y = matrix(runif(3*1000), ncol = 3)
# y = y/rowSums(y)
# spiral_initialize(xlim = c(0, 1000))
# spiral_track(height = 0.8)
# spiral_barplot(x, y, gp = gpar(fill = 2:4, col = NA))
#
spiral_barplot = function(pos, value, baseline = get_track_data("ymin", track_index),
	bar_width = min(diff(pos)), gp = gpar(), track_index = current_track_index()) {

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	if(is.matrix(value)) {
		n = ncol(value)
		for(i in 1:n) {
            if(i == 1) {
                spiral_rect(pos - bar_width/2, ymin, pos + bar_width/2, rowSums(value[, seq_len(i), drop = FALSE]), 
                	gp = subset_gp(gp, i), track_index = track_index)
            } else {
                spiral_rect(pos - bar_width/2, rowSums(value[, seq_len(i-1), drop = FALSE]), pos + bar_width/2, rowSums(value[, seq_len(i), drop = FALSE]), 
                	gp = subset_gp(gp, i), track_index = track_index)
            }
        }
    } else if(is.atomic(value)) {
       spiral_rect(pos - bar_width/2, baseline, pos + bar_width/2, value - baseline, gp = gp, track_index = track_index)
	}
}

# == title
# Add lines to a track
#
# == param
# -x X coordinates of the texts.
# -y Y coordinates of the texts.
# -offset Radical offset of the text. The value should be a `grid::unit` object.
# -text A vector of texts.
# -gp Graphical parameters.
# -facing Facing of the text.
# -nice_facing If it is true, the facing will be automatically adjusted for texts which locate at different positions of the curve.
# -just The justification of the text relative to (x, y).
# -hjust Horizontal justification. Value should be numeric. 0 means the left of the text and 1 means the right of the text.
# -vjust Vertical justification. Value should be numeric. 0 means the bottom of the text and 1 means the top of the text.
# -track_index Index of the track. 
# -... Pass to `grid::grid.text`.
#
# == details
# For the curved text, it only supports one-line text.
#
# == example
# x = seq(0.1, 0.9, length = 26)
# text = strrep(letters, 6)
# spiral_initialize(); spiral_track()
# spiral_text(x, 0.5, text)
#
# spiral_initialize(); spiral_track()
# spiral_text(x, 0.5, text, facing = "inside")
#
# spiral_initialize(); spiral_track()
# spiral_text(x, 0.5, text, facing = "outside")
#
# x = seq(0.1, 0.9, length = 10)
# text = strrep(letters[1:10], 20)
# spiral_initialize(); spiral_track()
# spiral_text(x, 0.5, text, facing = "curved_inside")
#
# spiral_initialize(); spiral_track()
# spiral_text(x, 0.5, text, facing = "curved_outside")
#
spiral_text = function(x, y, text, offset = NULL, gp = gpar(),
	facing = c("downward", "inside", "outside", "curved_inside", "curved_outside"),
	nice_facing = TRUE, just = "centre", hjust = NULL, vjust = NULL,
	track_index = current_track_index(), ...) {

	n1 = length(x)
	n2 = length(y)
	n3 = length(text)
	n = max(n1, n2, n3)
	if(length(x) == 1) x = rep(x, n)
	if(length(y) == 1) y = rep(y, n)
	if(length(text) == 1) text = rep(text, n)

	if(is.null(offset)) {
		df = xy_to_cartesian(x, y, track_index = track_index)
	} else {
		df = radical_extend(x, y, offset, track_index = track_index)
	}

	just = grid::valid.just(just)
	if(!is.null(hjust)) hjust = just[1]
	if(!is.null(vjust)) vjust = just[2]

	facing = match.arg(facing)[1]
	if(facing == "downward") {
		grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, hjust = hjust, vjust = vjust, ...)
	} else if(facing == "inside") {
		df2 = xy_to_polar(x, y, track_index = track_index)
		degree = as.degree(df2$theta) - 90
		if(nice_facing) {
			degree[df$y < 0] = degree[df$y < 0] + 180
		}
		grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, hjust = hjust, vjust = vjust, rot = degree, ...)
	} else if(facing == "outside") {
		df2 = xy_to_polar(x, y, track_index = track_index)
		degree = as.degree(df2$theta) + 90
		if(nice_facing) {
			degree[df$y > 0] = degree[df$y > 0] + 180
		}
		grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, hjust = hjust, vjust = vjust, rot = degree, ...)
	} else if(facing %in% c("curved_inside", "curved_outside")) {
		df2 = xy_to_polar(x, y, track_index = track_index)
		for(i in seq_len(n)) {
			curved_text(x[i], y[i], text[i], df2$theta[i], gp = subset_gp(gp, i), track_index = track_index, facing = facing, nice_facing = nice_facing)
		}
	}
}

curved_text = function(x, y, text, theta, gp = gpar(), track_index = current_track_index(), facing = "curved_inside", nice_facing = FALSE) {

	letters = strsplit(text, "")[[1]]
	n = length(letters)
	letters_len = sapply(1:n, function(i) convertWidth(grobWidth(textGrob(letters[i], gp = gp)), "native", valueOnly = TRUE))
	
	df = data.frame(x = numeric(n), y = numeric(n))
	for(i in seq_along(letters)) {
		offset = sum(letters_len[1:i]) - sum(letters_len)/2
		df2 = circular_extend(x, y, offset, track_index = track_index)
		df[i, 1] = df2[1, 1]	
		df[i, 2] = df2[1, 2]	
	}
	rot = as.degree(atan(df$y/df$x)) + ifelse(facing == "curved_inside", -1, 1)*90
	rot = rot %% 360
	rot[df$x < 0] = rot[df$x < 0] + 180
	if(nice_facing) {
		if(facing == "curved_inside") {
			if(sum(df$y < 0)/nrow(df) > 0.5) {
				rot = rot + 180
			}
		} else {
			if(sum(df$y > 0)/nrow(df) > 0.5) {
				rot = rot + 180
			}
		}
	}
	grid.text(letters, df$x, df$y, default.units = "native", gp = gp, rot = rot)
}


# == title
# Add lines to a track
#
# == param
# -x X coordinates of the data points.
# -y Y coordinates of the data points.
# -id A numeric vector used to separate locations in x and y into multiple polygons.
# -gp Graphical parameters.
# -track_index Index of the track. 
#
# == details
# Note the polygon must be closed, which means, the last data point should overlap to the first one.
spiral_polygon = function(x, y, id = NULL, gp = gpar(), track_index = current_track_index()) {

	n = length(x)
	if(is.null(id)) {
		df = spiral_lines_expand(x, y, track_index = track_index)
	} else {
		df = do.call(rbind, tapply(seq_len(n), id, function(ind) {
			df = spiral_lines_expand(x[ind], y[ind], track_index = track_index)
			df$id = rep(id[ind][1], nrow(df))
			df
		}))
		id = df$id
	}
	grid.polygon(df$x, df$y, id = id, default.units = "native", gp = gp)
}

# == title
# Draw axis along the spiral curve
#
# == param
# -h Position of the axis. The value can be a character of "top" or "bottom", or a numeric value which represents
#     the y location.
# -at Breaks points on axis.
# -major_at Breaks points on axis. It is the same as ``at``.
# -labels The corresponding labels for the break points.
# -minor_ticks Number of minor ticks.
# -major_ticks_length Length of the major ticks. The value should be a `grid::unit` object.
# -minor_ticks_length Length of the minor ticks. The value should be a `grid::unit` object.
# -ticks_gp Graphics parameters for the ticks.
# -labels_gp Graphics parameters for the labels.
# -track_index Index of the track. 
#
# == example
# spiral_initialize(); spiral_track()
# spiral_axis()
#
# # if the spiral is intepolated by the curve length
# spiral_initialize(scale_by = "curve_length"); spiral_track()
# spiral_axis()
#
# spiral_initialize(xlim = c(0, 360*4), start = 360, end = 360*5); spiral_track()
# spiral_axis(major_at = seq(0, 360*4, by = 30))
#
# spiral_initialize(xlim = c(0, 12*4), start = 360, end = 360*5); spiral_track()
# spiral_axis(major_at = seq(0, 12*4, by = 1), labels = c("", rep(month.name, 4)))
#
spiral_axis = function(h = "top",
	at = NULL,
	major_at = at,
	labels = TRUE,
	minor_ticks = 4,
	major_ticks_length = unit(4, "bigpts"),
	minor_ticks_length = unit(2, "bigpts"),
	ticks_gp = gpar(),
	labels_gp = gpar(fontsize = 6), 
	track_index = current_track_index()) {

	if(h == "top") {
		h = get_track_data("ymax", track_index)
	} else if(h == "botton") {
		h = get_track_data("ymin", track_index)
	}

	spiral = spiral_env$spiral

	if(is.null(major_at)) {
		major_by = spiral$xrange/2/20 # the circle have median size is split into 20 sectors
		digits = as.numeric(gsub("^.*e([+-]\\d+)$", "\\1", sprintf("%e", major_by))) - 1
		major_by = round(major_by, digits = -1*digits)
		major_at = seq(floor(spiral$xlim[1]/major_by)*major_by, spiral$xlim[2], by = major_by)
		major_at = c(major_at, major_at[length(major_at)] + major_by)
	} else {
		if(!(identical(labels, NULL) | identical(labels, TRUE) | identical(labels, FALSE))) {
			if(length(labels) != length(major_at)) {
				stop_wrap("Length of `labels` should be the same as the length of `major_at`.")
			}
		}
	}

	l = major_at <= spiral$xlim[2] & major_at >= spiral$xlim[1]
	major_at = major_at[l]
	if(!(identical(labels, NULL) | identical(labels, TRUE) | identical(labels, FALSE))) {
		labels = labels[l]
		labels_gp = subset_gp(labels_gp, l)
	}
		
	spiral_radical_segments(major_at, h, offset = major_ticks_length, track_index = track_index, gp = ticks_gp)

	minor_at = NULL
	if(minor_ticks != 0) {
		for(i in seq_along(major_at)) {
			if(i == 1) next
			k = seq_len(minor_ticks) / (minor_ticks + 1)
			minor_at = c(minor_at, k * (major_at[i] - major_at[i - 1]) + major_at[i - 1])
		}
		spiral_radical_segments(minor_at, h, offset = minor_ticks_length, track_index = track_index, gp = ticks_gp)
	}
	
	### labels
	if(is.null(labels)) labels = FALSE
	if(!identical(labels, FALSE)) {
		if(identical(labels, TRUE)) {
			labels = major_at
		}
		df = xy_to_polar(major_at, h, track_index = track_index)
		degree = as.degree(df$theta)
		rot = ifelse(degree >= 0 & degree <= 180, degree - 90, degree + 90)
		vjust = ifelse(degree >= 0 & degree <= 180, 0, 1)

		spiral_text(major_at, h, labels, offset = major_ticks_length + minor_ticks_length + unit(1, "bigpts"), 
			rot = rot, vjust = vjust, gp = labels_gp, track_index = track_index)
	}
}

# == title
# Draw axis along the spiral curve
#
# == param
# -... All pass to `spiral_axis`.
#
spiral_xaxis = function(...) {
	spiral_axis(...)
}

# == title
# Draw y-axis
#
# == param
# -side On which side of the spiral the axis is drawn?
# -at Break points.
# -labels Corresponding labels for the break points.
# -ticks_length Length of the tick. Value should be a `grid::unit` object.
# -ticks_gp Graphics parameters for ticks.
# -labels_gp Graphics parameters for labels.
# -track_index Index of the track.
#
# == example
# spiral_initialize(); spiral_track(height = 0.8)
# spiral_yaxis("start")
# spiral_yaxis("end", at = c(0, 0.25, 0.5, 0.75, 1), labels = letters[1:5])
spiral_yaxis = function(side = "start", at = NULL, labels = TRUE, ticks_length = unit(2, "bigpts"), 
	ticks_gp = gpar(), labels_gp = gpar(fontsize = 6), 
	track_index = current_track_index()) {
	
	spiral = spiral_env$spiral

	if(side == "start") {
		v = spiral$xlim[1]
	} else {
		v = spiral$xlim[2]
	}

	ylim = c(get_track_data("ymin", track_index), get_track_data("ymax", track_index))
	if(is.null(at)) {
		at = pretty(ylim, n = 3)
        labels = at
	} else {
		if(identical(labels, TRUE)) {
			labels = at
		} else if(identical(labels, NULL)) {
			labels = FALSE
		}
	}

	df1 = xy_to_cartesian(rep(v, length(at)), at, track_index = track_index)
	if(side == "start") {
		df2 = circular_extend(rep(v, length(at)), at, offset = -ticks_length, track_index = track_index, interval_extend = 1)
	} else {
		df2 = circular_extend(rep(v, length(at)), at, offset = ticks_length, track_index = track_index, interval_extend = 1)
	}
	
	grid.segments(df1$x, df1$y, df2$x, df2$y, default.units = "native", gp = ticks_gp)

	if(!identical(labels, FALSE)) {
		if(side == "start") {
			df2 = circular_extend(rep(v, length(at)), at, offset = -ticks_length - unit(1, "bigpts"), track_index = track_index, interval_extend = 1)
			just = "left"
			d = as.degree(atan(df2$y/df2$x))[1]
			if(df2$x[1] < 0) {
				d = d - 180
			}
			rot = d - 90
			if(d >= 0 & d < 180) {
				just = "left"
			} else {
				rot = rot + 180
				just = "right"
			}
		} else {
			df2 = circular_extend(rep(v, length(at)), at, offset = ticks_length + unit(1, "bigpts"), track_index = track_index, interval_extend = 1)
			d = as.degree(atan(df2$y/df2$x))[1]
			if(df2$x[1] < 0) {
				d = d - 180
			}
			rot = d + 90
			if(d >= 0 & d < 180) {
				rot = rot + 180
				just = "right"
			} else {
				just = "left"
			}
		}
		grid.text(labels, df2$x, df2$y, default.units = "native", gp = labels_gp, just = just, rot = rot)
	}
}

# == title
# Draw horizon chart
#
# == param
# -x X coordinates of the data points.
# -y Y coordinates of the data points.
# -n_slices Number of slices.
# -slice_size Size of the slices. The final number of sizes is ``ceiling(max(abs(y)/slice_size)``.
# -pos_fill Colors for positive values. 
# -neg_fill Colors for negative values.
# -use_bars Whether to use bars?
# -bar_width Width of bars.
# -negative_from_top Should negative distribution be drawn from the top?
# -track_index Index of the track. 
#
# == details
# Since the track height is very small in the spiral, horizon chart visualization is a efficient way to visualize
# distribution-like graphics.
#
# == value
# A list of two color mapping functions, one for positive colors and one for negative colors, mainly for generating legends.
#
# == example
# \donttest{
# df = cranlogs::cran_downloads("ggplot2", from="2014-01-01", to = "2020-12-31")
# day_diff = as.numeric(df$date[nrow(df)] - df$date[1])
# v = df$count
# v[v > quantile(v, 0.95)] = quantile(v, 0.95)
# spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/364)) # a circle 52 weeks
# spiral_track(height = 0.9)
# spiral_horizon(1:nrow(df), v)
#
# spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/364))
# spiral_track(height = 0.9)
# spiral_horizon(1:nrow(df), v, use_bars = TRUE)
# }
spiral_horizon = function(x, y, n_slices = 4, slice_size, pos_fill = "#D73027", neg_fill = "#313695",
	use_bars = FALSE, bar_width = min(diff(x)),
	negative_from_top = FALSE, track_index = current_track_index()) {

	if(!(get_track_data("ymin", track_index) == 0 & get_track_data("ymax", track_index) == 1)) {
		stop_wrap("The horizon track must have 'ylim = c(0, 1)'.")
	}

	if(missing(slice_size)) {
		slice_size = max(abs(y))/n_slices
	}
	n_slices = ceiling(max(abs(y))/slice_size)

	if(n_slices == 0) {
		return(invisible(NULL))
	}

	pos_col_fun = colorRamp2(c(0, n_slices), c("white", pos_fill))
	neg_col_fun = colorRamp2(c(0, n_slices), c("white", neg_fill))
	for(i in seq_len(n_slices)) {
		l1 = y >= (i-1)*slice_size & y < i*slice_size
		l2 = y < (i-1)*slice_size
		l3 = y >= i*slice_size
		if(any(l1)) {
			x2 = x
			y2 = y
			y2[l1] = y2[l1] - slice_size*(i-1)
			y2[l3] = slice_size
			x2[l2] = NA
			y2[l2] = NA

			if(use_bars) {
				add_horizon_bars(x2, y2, bar_width = bar_width, slice_size = slice_size, 
					gp = gpar(fill = pos_col_fun(i), col = pos_col_fun(i)), track_index = track_index) 
			} else {
				add_horizon_polygons(x2, y2, slice_size = slice_size, 
					gp = gpar(fill = pos_col_fun(i), col = NA), track_index = track_index)
			}
		}
	}
	y = -y
	for(i in seq_len(n_slices)) {
		l1 = y >= (i-1)*slice_size & y < i*slice_size
		l2 = y < (i-1)*slice_size
		l3 = y >= i*slice_size
		if(any(l1)) {
			x2 = x
			y2 = y
			y2[l1] = y2[l1] - slice_size*(i-1)
			y2[l3] = slice_size
			x2[l2] = NA
			y2[l2] = NA

			if(use_bars) {
				add_horizon_bars(x2, y2, bar_width = bar_width, slice_size = slice_size, from_top = negative_from_top, 
					gp = gpar(fill = neg_col_fun(i), col = neg_col_fun(i)), track_index = track_index)
			} else {
				add_horizon_polygons(x2, y2, slice_size = slice_size, from_top = negative_from_top, 
					gp = gpar(fill = neg_col_fun(i), col = NA), track_index = track_index)
			}
		}
	}

	invisible(list(pos_col_fun = pos_col_fun, neg_col_fun = neg_col_fun))
}

add_horizon_polygons = function(x, y, slice_size = NULL, from_top = FALSE, ...) {
	ltx = split_vec_by_NA(x)
	lty = split_vec_by_NA(y)

	for(i in seq_along(ltx)) {
		x0 = ltx[[i]]
		y0 = lty[[i]]
		if(from_top) {
			x0 = c(x0[1], x0, x0[length(x0)], x0[1])
			y0 = c(slice_size, slice_size - y0, slice_size, slice_size)
		} else {
			x0 = c(x0[1], x0, x0[length(x0)], x0[1])
			y0 = c(0, y0, 0, 0)
		}
		spiral_polygon(x0, y0/slice_size, ...)
	}
}

add_horizon_bars = function(x, y, bar_width, slice_size = NULL, from_top = FALSE, ...) {
	ltx = split_vec_by_NA(x)
	lty = split_vec_by_NA(y)

	for(i in seq_along(ltx)) {
		x0 = ltx[[i]]
		y0 = lty[[i]]
		if(from_top) {
			spiral_barplot(x0, y0/slice_size, bar_width = bar_width, baseline = 1, ...)
		} else {
			spiral_barplot(x0, y0/slice_size, bar_width = bar_width, ...)
		}
	}
}

# https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
split_vec_by_NA = function(x) {
	idx = 1 + cumsum(is.na(x))
	not.na = !is.na(x)
	split(x[not.na], idx[not.na])
}

# == title
# Add image to a track
#
# == param
# -x X coordinates of the center of the image.
# -y Y coordinates of the center of the image.
# -image A vector of file paths of images. The format of the image is inferred from the suffix name of the image file.
#       NA values or empty strings in the vector means no image to drawn. Supported formats are png/svg/pdf/eps/jpeg/jpg/tiff.
# -width Width of the image. The value should be a `grid::unit` object.
# -height Height of the image. The value should be a `grid::unit` object. It is suggested to only set one of ``width`` and ``height``, the
#       other dimension will be automatically calcualted from the aspect ratio of the image.
# -facing Facing of the image.
# -track_index Index of the track. 
#
# == example
# image = system.file("extdata", "Rlogo.png", package = "circlize")
# x = seq(0.1, 0.9, length = 10)
#
# spiral_initialize()
# spiral_track()
# spiral_raster(x, 0.5, image)
#
# spiral_initialize()
# spiral_track()
# spiral_raster(x, 0.5, image, facing = "inside")
#
spiral_raster = function(x, y, image, width = NULL, height = NULL, 
	facing = c("downward", "inside", "outside"),
	track_index = current_track_index()) {

	facing = match.arg(facing)[1]
	
	if(is.character(image) && is.atomic(image)) {
		n1 = length(x)
		n2 = length(y)
		n3 = length(image)
		n = max(n1, n2, n3)

		if(n1 == 1) x = rep(x, n)
		if(n2 == 1) y = rep(y, n)
		if(n3 == 1) {
			img = read_image(image)
			image = vector("list", n)
			for(i in seq_len(n)) {
				image[[i]] = img[[1]]
			}
		} else {
			image = read_image(image)
		}
		if(!is.null(width)) {
			if(length(width) == 1) width = rep(width, n)
		}
		if(!is.null(height)) {
			if(length(height) == 1) height = rep(height, n)
		}

		for(i in seq_len(n)) {
			if(identical(image[[i]], NA)) next
			spiral_raster(x[i], y[i], image[[i]], width = width[i], height = height[i], facing = facing, track_index = track_index)
		}
		return(invisible(NULL))
	}
	
	spiral = spiral_env$spiral

	df = xy_to_polar(x, y, track_index = track_index)
	df2 = polar_to_cartesian(df$theta, df$r)
	theta = as.degree(df$theta)
	
	if(inherits(image, "array")) {
		asp = ncol(image)/nrow(image) # width/height
	} else if(inherits(image, "Picture")) {
		asp = max(image@summary@xscale)/max(image@summary@yscale)
	}

	if(is.null(width) && is.null(height)) {
		height = convertHeight(unit(0.8*spiral$dist * get_track_data("rel_height", track_index), "native"), "mm")
		width = height*asp
	} else if(is.null(width)) {
		width = height*asp
	} else if(is.null(height)) {
		height = 1/asp*width
	}
	
	if(facing == "downward") {
		rot = 0
	} else if(facing == "inside") {
		rot = theta - 90 
	} else if(facing == "outside") {
		rot = theta + 90 
	}

	image_class = attr(image, "image_class")

	pushViewport(viewport(x = df2$x, y = df2$y, width = width, height = height, angle = rot, default.units = "native"))
	if(is.null(image_class)) {
		grid.raster(image)
	} else if(image_class == "raster") {
		grid.raster(image)
	} else if(image_class == "grImport::Picture") {
		grid.picture = getFromNamespace("grid.picture", ns = "grImport")
		grid.picture(image, x = (i-0.5)/n, width = width, height = height)
	} else if(image_class == "grImport2::Picture") {
		grid.picture = getFromNamespace("grid.picture", ns = "grImport2")
		grid.picture(image, x = (i-0.5)/n, width = width, height = height)
	}
	popViewport()
}

# == title
# Draw circular arrows
#
# == param
# -x1 Start of the arrow.
# -x2 End of the arrow.
# -y Position of the arrow on y location.
# -width Width of the arrow.
# -track_index Index of the track. 
# -arrow_head_length Length of the arrow head.
# -arrow_head_width Width of the arrow head.
# -arrow_position Position of the arrow. If the value is ``"end"``, then the arrow head is drawn at ``x = x2``. If the value
#    is ``"start"``, then the arrow head is drawn at ``x = x1``. 
# -tail The shape of the arrow tail.
# -gp Graphics parameters.
#
# == example
# spiral_initialize()
# spiral_track()
# spiral_arrow(0.3, 0.6, gp = gpar(fill = "red"))
# spiral_arrow(0.8, 0.9, gp = gpar(fill = "blue"), tail = "point", arrow_position = "start")
spiral_arrow = function(
	x1, x2, 
	y = get_track_data("ycenter", track_index), 
	width = get_track_data("yrange", track_index)/3, 
	track_index = current_track_index(),
	arrow_head_length = unit(4, "mm"),
	arrow_head_width = width*2, 
	arrow_position = c("end", "start"),
	tail = c("normal", "point"), 
	gp = gpar()) {

	arrow_position = match.arg(arrow_position)[1]
	tail = match.arg(tail)[1]

	if(x2 <= x1) {
		x3 = x1
		x1 = x2
		x2 = x3
		arrow_position = setdiff(c("end", "start"), arrow_position)
	}
	
	spiral = spiral_env$spiral

	if(is.unit(arrow_head_length)) {
		arrow_head_length = convertWidth(arrow_head_length, "native", valueOnly = TRUE)
		arrow_head_length = arrow_head_length/spiral$spiral_length(spiral$theta_lim[2])*(spiral$xlim[2] - spiral$xlim[1])
	}
	if(is.unit(arrow_head_width)) {
		arrow_head_width = convertWidth(arrow_head_width, "native", valueOnly = TRUE)
		arrow_head_width = arrow_head_width/(get_track_data("rmax", track_index) - get_track_data("rmin", track_index))*get_track_data("yrange", track_index)
	}

	if(abs(x2 - x1 - arrow_head_length) < 1e-6) {
		stop_wrap("Arrow head is too long that it is even longer than the arrow itself.")
	}

	if(arrow_position == "end") {
		arrow_head_coor = rbind(c(x2 - arrow_head_length, y + arrow_head_width/2),
			                    c(x2, y),
			                    c(x2 - arrow_head_length, y - arrow_head_width/2))
		if(tail == "normal") {
			arrow_body_coor = rbind(c(x2 - arrow_head_length, y - width/2),
				                    c(x1, y - width/2),
				                    c(x1, y + width/2),
				                    c(x2 - arrow_head_length, y + width/2))
			
		} else {
			arrow_body_coor = rbind(c(x2 - arrow_head_length, y - width/2),
				                    c(x1, y),
				                    c(x2 - arrow_head_length, y + width/2))
			
		}

		coor = rbind(arrow_body_coor, arrow_head_coor)
	} else {
		
		arrow_head_coor = rbind(c(x1 + arrow_head_length, y + arrow_head_width/2),
			                    c(x1, y),
			                    c(x1 + arrow_head_length, y - arrow_head_width/2))
		if(tail == "normal") {
			arrow_body_coor = rbind(c(x1 + arrow_head_length, y - width/2),
				                    c(x2, y - width/2),
				                    c(x2, y + width/2),
				                    c(x1 + arrow_head_length, y + width/2))
		} else {
			arrow_body_coor = rbind(c(x1 + arrow_head_length, y - width/2),
				                    c(x2, y),
				                    c(x1 + arrow_head_length, y + width/2))
		}
		coor = rbind(arrow_body_coor, arrow_head_coor)
	}
	coor = rbind(coor, coor[1, ])
	
	spiral_polygon(coor[, 1], coor[, 2], gp = gp, track_index = track_index)
}


# == title
# Highlight segments in the curve
#
# == param
# -x1 Start of the segments.
# -x2 End of the segments.
# -type Type of the highlighting. "rect" means drawing transparent rectangles covering the whole track.
#      "line" means drawing annotation lines on top of the track or at the bottom of it.
# -line_side If the highlight type is "line", it controls which side of the track to draw the lines.
# -line_width Width of the annotation line. Value should be a `grid::unit` object.
# -gp Graphics parameters.
# -track_index Index of the track.
#
# == example
# spiral_initialize(); spiral_track()
# spiral_highlight(0.4, 0.6)
# spiral_highlight(0.1, 0.2, type = "line", gp = gpar(col = "blue"))
# spiral_highlight(0.7, 0.8, type = "line", line_side = "outside")
spiral_highlight = function(x1, x2, type = c("rect", "line"), 
	line_side = c("inside", "outside"), line_width = unit(1, "bigpts"),
	gp = gpar(fill = "red"), track_index = current_track_index()) {

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)

	type = match.arg(type)[1]
	if(type == "rect") {
		if("fill" %in% names(gp)) {
			gp$fill = add_transparency(gp$fill, 0.75)
		}
		if(!"col" %in% names(gp)) {
			gp$col = NA
		}
	
		spiral_rect(x1, get_track_data("ymin", track_index),
			        x2, get_track_data("ymax", track_index),
			        gp = gp, track_index =  track_index)
	} else {
		line_side = match.arg(line_side)[1]
		if(!"col" %in% names(gp)) {
			gp$col = gp$fill
		}
		if("col" %in% names(gp) && !("fill" %in% names(gp))) {
			gp$fill = gp$col
		}
		offset = convert_to_y(line_width, track_index = track_index)
		if(line_side == "inside") {
			spiral_rect(x1, ymin, x2, ymin - offset, gp = gp, track_index = track_index)
		} else {
			spiral_rect(x1, ymax, x2, ymax + offset, gp = gp, track_index = track_index)
		}
	}
}

# == title
# Highlight a sector in the curve
#
# == param
# -theta1 Start of the sector in the circle. Values are measured in degrees.
# -theta2 End of the sector in the circle. Values are measured in degrees.
# -gp Graphics parameters.
#
# == example
# spiral_initialize(); spiral_track()
# spiral_highlight_by_theta(30, 60)
spiral_highlight_by_theta = function(theta1, theta2, gp = gpar(fill = "red")) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360

	if(theta1 > theta2) {
		theta2 = theta2 + 360
	}

	spiral = spiral_env$spiral
	max_r = spiral$max_radius + spiral$dist

	theta = seq(theta1, theta2, by = 0.5)/180*pi
	r = rep(max_r, length(theta))
	df = polar_to_cartesian(theta, r)

	if("fill" %in% names(gp)) {
		gp$fill = add_transparency(gp$fill, 0.75)
	}
	if(!"col" %in% names(gp)) {
		gp$col = NA
	}

	grid.polygon(c(df$x, 0), c(df$y, 0), gp = gp, default.units = "native")
}


add_transparency = function (col, transparency = 0){
	col1 = col2rgb(col, alpha = TRUE)
	col1[4, col1[4, ] == 255] = round((1 - transparency)*255)
    rgb(t(col1/255), alpha = col1[4, ]/255)
}

