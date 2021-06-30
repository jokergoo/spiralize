
# == title
# Clear the spiral curve
#
# == param
# -check_vp Whether to check the viewport.
#
# == details
# It basically sets the internally spiral object to NULL, and reset all the global options.
#
# == value
# No value is returned.
#
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
# Add a new track or go to an existed track
#
# == param
# -ylim Data range of the y-locations.
# -height Height of the track. The value should be the fraction of the distance of the two neighbour loops.
# -background Whether to draw the background of the track, i.e. border and filled color of background.
# -background_gp Graphics parameters of the background.
# -reverse_y Whether reverse the direction of y-axis.
# -track_index Index of the track. 
#
# == details
# If the track is already existed, the function simply mark the track as the current track and does nothing else.
#
# == value
# No value is returned.
#
# == example
# spiral_initialize()
# spiral_track(height = 0.8)
#
# spiral_initialize()
# spiral_track(height = 0.4, background_gp = gpar(fill = "red"))
# spiral_track(height = 0.2, background_gp = gpar(fill = "green"))
# spiral_track(height = 0.1, background_gp = gpar(fill = "blue"))
spiral_track = function(ylim = c(0, 1), height = 0.8, background = TRUE, 
	background_gp = gpar(col = NA, fill = "#EEEEEE"), reverse_y = FALSE,
	track_index = current_track_index() + 1) {

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
			if(reverse_y) {
				ylim = rev(ylim)
			}
			new_track_data = data.frame(i = track_index, 
				ymin = ylim[1], ymax = ylim[2], 
				rmin = sum_height*dist, rmax = (sum_height + height)*dist, 
				rel_height = height)
			add_track(track_index, new_track_data)
		}

		if(spiral$theta_range/2/pi > 30) { # if there are more than 30 loops
			if(missing(background)) {
				background = FALSE
			}
		}

		if(background) {
			if(!"col" %in% names(background_gp)) {
				background_gp$col = NA
			}
			if(!"fill" %in% names(background_gp)) {
				background_gp$fill = "#EEEEEE"
			}
			spiral_rect(spiral$xlim[1], get_track_data("ymin"), spiral$xlim[2], get_track_data("ymax"), gp = background_gp)
		}
	}
}

# == title
# Information of the current spiral
#
# == details
# It prints information of the current spiral.
#
# == value
# No value is returned.
#
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
				qqcat("    height: @{get_track_data('rel_height', i)} (fraction of the distance of two neighbour loops)\n")
				if(i < nt) cat("\n")
			}
		}
	}
}

