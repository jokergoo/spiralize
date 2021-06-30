
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
# -vp_param A list of parameters sent to `grid::viewport`.
#
# == value
# No value is returned.
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
	polar_lines = scale_by == "angle", polar_lines_by = 30, 
	polar_lines_gp = gpar(col = "#808080", lty = 3), 
	padding = unit(5, "mm"), newpage = TRUE, vp_param = list()) {

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

	vp_param2 = list(name = paste0("spiral_", spiral_env$i_spiral),
		width = unit(1, "snpc") - padding[1], 
		height = unit(1, "snpc") - padding[2],
		xscale = c(-abs_max_x, abs_max_x), yscale = c(-abs_max_y, abs_max_y))
	if("width" %in% names(vp_param)) {
		vp_param2$width = NULL
	}
	if("height" %in% names(vp_param)) {
		vp_param2$height = NULL
	}
	vp_param = vp_param[!names(vp_param) %in% c("xscale", "yscale")]
	vp_param2 = c(vp_param2, vp_param)
	pushViewport(do.call(viewport, vp_param2))

	if(polar_lines) {
		d = seq(0, 360, by = polar_lines_by)
		if(d[length(d)] == 360) d = d[-length(d)]
		dm = matrix(nrow = length(d), ncol = 4)
		for(i in seq_along(d)) {
			r0 = max(r + dist) + convertWidth(max(padding), "native", valueOnly = TRUE)
			dm[i, ] = c(0, 0, cos(d[i]/180*pi)*r0, sin(d[i]/180*pi)*r0)
		}
		grid.segments(dm[, 1], dm[, 2], dm[, 3], dm[, 4], default.units = "native", gp = polar_lines_gp)
	}
}

# == title
# Initialize the spiral from time objects
#
# == param
# -xlim Range of the time. The value can be time object such as ``Date``, ``POSIXlt`` or ``POSIXct``. The value can also be characters and it is converted to time objects automatically.
# -start Start of the spiral, in degrees. By default it is automatically calculated.
# -end End of the spiral, in degrees. By default it is automatically calculated.
# -unit_on_axis Units on the axis.
# -period Which period to use?
# -period_per_loop How many periods to put in a loop?
# -polar_lines_by By default different value of ``polar_lines_by`` is set for different ``period``. E.g. 360/7 is set if ``period`` is "weeks" or 360/24 is set if ``peroid`` is set to "hours".
# -verbose Whether to print messages?
# -... All pass to `spiral_initialize`.
#
# == details
# "start" and "end" are automatically calculated for different "unit_on_axis" and "period". For example, if "unit_on_axis" is "days" and "period" is "years", then
# the first day of each each year is always put on theta = 0 + 2*pi*k where k is the index of loops.
#
# == value
# No value is returned.
#
# == example
# spiral_initialize_by_time(xlim = c("2014-01-01", "2021-06-17"))
# spiral_track(height = 0.6)
# spiral_axis()
#
# spiral_initialize_by_time(xlim = c("2021-01-01 00:00:00", "2021-01-05 00:00:00"))
# spiral_track(height = 0.6)
# spiral_axis()
#
# spiral_initialize_by_time(xlim = c("2021-01-01 00:00:00", "2021-01-01 00:10:00"),
#     unit_on_axis = "secs", period = "mins")
# spiral_track(height = 0.6)
# spiral_axis()
spiral_initialize_by_time = function(xlim, start = NULL, end = NULL,
	unit_on_axis = c("days", "months", "weeks", "hours", "mins", "secs"), 
	period = c("years", "months", "weeks", "days", "hours", "mins"),
	period_per_loop = 1, polar_lines_by = NULL, verbose = TRUE, ...) {

	xlim_is_date = inherits(xlim, "Date")
	xlim = as.POSIXlt(xlim)

	if(missing(unit_on_axis)) {
		if(xlim_is_date) {
			unit_on_axis = "days"
		} else {
			td = time_difference(xlim[1], xlim[2], "days")
			if(td >= 180) {
				unit_on_axis = "days"
			} else {
				td = time_difference(xlim[1], xlim[2], "hours")
				if(td > 100) {
					unit_on_axis = "hours"
				} else {
					td = time_difference(xlim[1], xlim[2], "mins")
					if(td > 100) {
						unit_on_axis = "mins"
					} else {
						unit_on_axis = "secs"
					}
				}
			}
		}
		if(verbose) qqcat("'unit_to_axis' is set to '@{unit_on_axis}'.\n")
	} else {
		unit_on_axis = match.arg(unit_on_axis)[1]
	}
	if(missing(period)) {
		if(unit_on_axis %in% c("days", "months", "weeks")) {
			period = "years"
		} else if(unit_on_axis %in% c("hours", "mins")) {
			period = "days"
		} else if(unit_on_axis == "secs") {
			period = "hours"
		}
		if(verbose) qqcat("'period' is set to '@{period}'.\n")
	} else {
		period = match.arg(period)[1]
	}

	## check whether the value of unit and period make sense
	if(unit_on_axis == "days") {
		if(!period %in% c("years", "months", "weeks")) {
			stop_wrap("If 'unit_on_axis' is 'days', 'period' can only be one of 'years', 'months' and 'weeks'.")
		}
	}
	if(unit_on_axis == "months") {
		if(!period %in% c("years")) {
			stop_wrap("If 'unit_on_axis' is 'months', 'period' can only be 'years'.")
		}
	}
	if(unit_on_axis == "weeks") {
		if(!period %in% c("years")) {
			stop_wrap("If 'unit_on_axis' is 'weeks', 'period' can only be 'years'.")
		}
	}
	if(unit_on_axis == "hours") {
		if(!period %in% c("months", "weeks", "days")) {
			stop_wrap("If 'unit_on_axis' is 'hours', 'period' can only be one of 'months', 'weeks' and 'days'.")
		}
	}
	if(unit_on_axis == "mins") {
		if(!period %in% c("days", "hours")) {
			stop_wrap("If 'unit_on_axis' is 'mins', 'period' can only be one of 'days' and 'hours'.")
		}
	}
	if(unit_on_axis == "secs") {
		if(!period %in% c("hours", "mins")) {
			stop_wrap("If 'unit_on_axis' is 'secs', 'period' can only be one of 'hours' and 'mins'.")
		}
	}

	if(is.null(polar_lines_by)) {
		if(period == "years") {
			polar_lines_by = (360/12)/period_per_loop
		} else if(period == "weeks") {
			polar_lines_by = (360/7)/period_per_loop
		} else if(period == "days") {
			polar_lines_by = (360/24)/period_per_loop
		} else {
			polar_lines_by = 30
		}
	}

	time_start = xlim[1]
	time_end = xlim[2]

	get_numeric_x = local({
		time_start = time_start
		function(x) {
			if(is.numeric(x)) {
				return(x)
			}
			x = as.POSIXlt(x)
			time_difference(time_start, x, unit_on_axis)
		}
	})

	get_data_x = local({
		time_start = time_start
		function(x) {
			add_time(time_start, x, unit_on_axis)
		}
	})

	if(unit_on_axis == "days") {
		get_character_x = local({
			time_start = time_start
			function(x) {
				as.character(time_start + days(x))
			}
		})
	} else if(unit_on_axis == "months") {
		get_character_x = local({
			time_start = time_start
			function(x) {
				t = time_start %m+% months(x)
				format(t, "%Y-%m")
			}
		})
	} else if(unit_on_axis == "weeks") {
		get_character_x = local({
			time_start = time_start
			function(x) {
				t = time_start + weeks(x)
				format(t, "%Y-%m-%d")
			}
		})
	} else if(unit_on_axis == "hours") {
		get_character_x = local({
			time_start = time_start
			in_a_same_year = year(time_end) == year(time_start)
			in_a_same_day = day(time_end) == day(time_start)
			function(x) {
				t = time_start + hours(x)
				if(in_a_same_year && in_a_same_day) {
					format(t, "%Hh")
				} else if(in_a_same_year) {
					format(t, "%m-%d %Hh")
				} else {
					format(t, "%Y-%m-%d %Hh")
				}
			}
		})
	} else if(unit_on_axis == "mins") {
		get_character_x = local({
			time_start = time_start
			in_a_same_year = year(time_end) == year(time_start)
			in_a_same_day = day(time_end) == day(time_start)
			function(x) {
				t = time_start + minutes(x)
				if(in_a_same_year && in_a_same_day) {
					format(t, "%H:%M")
				} else if(in_a_same_year) {
					format(t, "%m-%d %H:%M")
				} else {
					format(t, "%Y-%m-%d %H:%M")
				}
			}
		})
	} else if(unit_on_axis == "secs") {
		get_character_x = local({
			time_start = time_start
			in_a_same_year = year(time_end) == year(time_start)
			in_a_same_day = day(time_end) == day(time_start)
			function(x) {
				t = time_start + seconds(x)
				if(in_a_same_year && in_a_same_day) {
					format(t, "%H:%M:%S")
				} else if(in_a_same_year) {
					format(t, "%m-%d %H:%M:%S")
				} else {
					format(t, "%Y-%m-%d %H:%M:%S")
				}
			}
		})
	}

	xlim = get_numeric_x(xlim)

	if( (is.null(start) && !is.null(end)) || (!is.null(start) && is.null(end)) ) {
		stop_wrap("You should both set 'start' and 'end'.")
	}

	start_of_the_year = as.POSIXlt("2021-01-01")
	year(start_of_the_year) = year(time_start)

	if(is.null(start)) {
		if(unit_on_axis == "months") {
			start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/12/period_per_loop)*360
			end = 360*(time_difference(time_start, time_end, unit_on_axis)/12/period_per_loop) + start
		} else if(unit_on_axis == "weeks") {
			start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/52/period_per_loop)*360
			end = 360*(time_difference(time_start, time_end, unit_on_axis)/52/period_per_loop) + start
		} else if(unit_on_axis == "days") {
			if(period == "years") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/(52*7)/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/(52*7)/period_per_loop) + start
			} else if(period == "months") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/30/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/30/period_per_loop) + start
			} else if(period == "weeks") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/7/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/7/period_per_loop) + start
			} 
		} else if(unit_on_axis == "hours") {
			if(period == "months") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/(30*24)/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/(30*24)/period_per_loop) + start
			} else if(period == "weeks") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/(7*24)/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/(7*24)/period_per_loop) + start
			} else if(period == "days") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/24/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/24/period_per_loop) + start
			}
		} else if(unit_on_axis == "mins") {
			if(period == "days") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/(24*60)/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/(24*60)/period_per_loop) + start
			} else if(period == "hours") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/60/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/60/period_per_loop) + start
			}
		} else if(unit_on_axis == "secs") {
			if(period == "hours") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/(60*60)/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/(60*60)/period_per_loop) + start
			} else if(period == "mins") {
				start = (1 + time_difference(start_of_the_year, time_start, unit_on_axis)/60/period_per_loop)*360
				end = 360*(time_difference(time_start, time_end, unit_on_axis)/60/period_per_loop) + start
			}
		}
	}

	p = 1/(xlim[2] - xlim[1])
	start = start - (end - start)*p/2
	end = end + (end - start)*p/2
	xlim[1] = xlim[1] - 0.5
	xlim[2] = xlim[2] + 0.5
	spiral_initialize(xlim = xlim, start = start, end = end, polar_lines_by = polar_lines_by, ...)

	spiral = current_spiral()
	spiral$xclass = "Time"
	spiral$get_numeric_x = get_numeric_x
	spiral$get_character_x = get_character_x

	spiral$other = list(unit_on_axis = unit_on_axis, 
		                period = period)

	invisible(NULL)
}



# unit: months, weeks, days, hours, mins, secs
time_difference = function(x, y, unit = "days") {
	n = max(length(x), length(y))
	if(length(x) == 1) x = rep(x, n)
	if(length(y) == 1) y = rep(y, n)

	if(unit %in% c("days", "weeks", "hours", "mins", "secs")) {
		as.double(y - x, unit)
	} else if(unit == "months") {
		year_1 = lubridate::year(x)
		month_1 = lubridate::month(x)

		year_2 = lubridate::year(y)
		month_2 = lubridate::month(y)

		l1 = year_1 == year_2
		l2 = year_1 < year_2
		l3 = year_1 > year_2

		d = numeric(length(x))
		if(any(l1)) d[l1] = month_2[l1] - month_1[l1]
		if(any(l2)) d[l2] = 12 - month_1[l2] + month_2[l2] + (year_2[l2] - year_1[l2] - 1)*12
		if(any(l3)) d[l3] = 12 - month_2[l3] + month_1[l3] + (year_1[l3] - year_2[l3] - 1)*12

		d
	}
}

add_time = function(t, diff, unit) {
	if(unit == "days") {
		t + lubridate::days(diff)
	} else if(unit == "weeks") {
		t + lubridate::weeks(diff)
	} else if(unit == "hours") {
		t + lubridate::hours(diff) 
	} else if(unit == "mins") {
		t + lubridate::minutes(diff)
	} else if(unit == "secs") {
		t + lubridate::seconds(diff)
	} else if(unit == "months") {
		month2 = (month(t) + diff - 1) %% 12 + 1
		year2 = year(t) + floor((month(t) + diff - 1)/12)
		year(t) = year2
		month(t) = month2
		t
	}
}

# == title
# Initialize the spiral with genomic coordinates
#
# == param
# -xlim Range of the genomic coordinates.
# -scale_by For genomic plot, axis is linearly scaled by the curve length.
# -... All pass to `spiral_initialize`.
#
# == details
# It is basically the same as `spiral_initialize`. The only difference is the axis labels are automatically
# formated for genomic coordinates.
#
# == value
# No value is returned.
#
# == example
# spiral_initialize_by_gcoor(c(0, 1000000000))
# spiral_track()
# spiral_axis()
spiral_initialize_by_gcoor = function(xlim, scale_by = "curve_length", ...) {

	get_character_x = function(x) {
		x2 = character(length(x))
		l1 = x >= 1e6
		x2[l1] = paste(x[l1]/1000000, "MB", sep = "")
		l2 = x >= 1e3 & !l1
		x2[l2] = paste(x[l2]/1000, "KB", sep = "")
		
		l3 = !(l1 | l2)
		x2[l3] = paste(x[l3], "bp", sep = "")
		x2
	}

	spiral_initialize(xlim = xlim, scale_by = scale_by, ...)

	spiral = current_spiral()
	spiral$xclass = "Genomic positions"
	spiral$get_character_x = get_character_x

	invisible(NULL)
}
