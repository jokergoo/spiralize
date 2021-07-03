
track_env = new.env()

track_env$current_track = 0
empty_track_data = data.frame(
	i = integer(0), 
	ymin = numeric(0),
	ymax = numeric(0),
	rmin = numeric(0),
	rmax = numeric(0),
	rel_height = numeric(0),
	reverse_y = logical(0)
)

track_env$track_data = empty_track_data

# == title
# Current track index
#
# == value
# An integer of the index of the current track.
current_track_index = function() {
	track_env$current_track
}

# == title
# Set current track
#
# == param
# -track_index The index of the track.
#
# == value
# No value is returned.
#
set_current_track = function(track_index) {
	if(!track_existed(track_index)) {
		stop_wrap(qq("Track @{track_index} does not exist."))
	}
	track_env$current_track = track_index
}

add_track = function(i, new_track_data) {

	height_remain = 1 - sum(track_env$track_data[, "rel_height"])
	if(new_track_data$rel_height > height_remain) {
		stop_wrap(qq("There is no space for the new track (height = @{new_track_data$rel_height}). The value should be smaller than @{height_remain}. Note 'height' is a relative fraction between 0 and 1."))
	}

	track_env$track_data = rbind(track_env$track_data, new_track_data)
	track_env$current_track = i
	set_current_track(i)
}

# == title
# Get meta-data of a track
#
# == param
# -field Name, see Details section.
# -track_index Which track?
#
# == details
# There are following fields that can be retrieved for a given track:
#
# - ymin Lower boundary of the data.
# - ymax Upper boundary of the data.
# - ycenter ``(ymin + ymax)/2``
# - ylim ``c(ylim, ymax)``
# - yrange ``ymax - ymin``
# - height Height of the track, measured as the fraction of the distance between two neighbouring circles.
#
# It is more suggested to directly use `TRACK_META` to retrieve meta data for the current track.
# 
# == value
# A numeric value of the corresponding field.
get_track_data = function(field, track_index = current_track_index()) {

	if(!track_existed(track_index)) {
		stop_wrap(qq("Track @{track_index} does not exist."))
	}

	if(field == "ycenter") {
		(track_env$track_data[track_index, "ymin"] + track_env$track_data[track_index, "ymax"])/2
	} else if(field == "ylim") {
		c(track_env$track_data[track_index, "ymin"], track_env$track_data[track_index, "ymax"])
	} else if(field == "yrange") {
		abs(track_env$track_data[track_index, "ymax"] - track_env$track_data[track_index, "ymin"])
	} else if(field == "rrange") {
		track_env$track_data[track_index, "rmax"] - track_env$track_data[track_index, "rmin"]
	} else if(field == "height") {
		track_env$track_data[track_index, "rel_height"]
	} else {
		track_env$track_data[track_index, field]
	}
}

track_existed = function(track_index) {
	track_index <= nrow(track_env$track_data)
}

# == title
# Number of tracks
#
# == value
# An integer of the number of available tracks.
n_tracks = function() {
	nrow(track_env$track_data)
}


# == title (variable:TRACK_META)
# Get meta data in the current track
#
# == details
# The variable ``TRACK_META`` can only be used to get meta data from the "current" track. If the current track
# is not the one you want, you can first use `set_current_track` to set the current track.
#
# Don't directly use ``TRACK_META``. The value of `TRACK_META` itself is meaningless. Always use in form of ``TRACK_META$name``.
#
# There are following meta data for the current track:
#
# -``xlim``: Data range on x-axis.
# -``xmin``: ``xlim[1]``.
# -``xmax``: ``xlim[2]``.
# -``xrange``: ``xlim[2] - xlim[1]``.
# -``xcenter``: ``mean(xlim)``.
# -``theta_lim``: Range of the angles on the spiral, measured in radians.
# -``theta_min``: ``theta_lim[1]``.
# -``theta_max``: ``theta_lim[2]``.
# -``theta_range``: ``theta_lim[2] - theta_lim[1]``.
# -``theta_center``: ``mean(theta_lim)``.
# -``ylim``: Data range on y-axis.
# -``ymin``: ``ylim[1]``.
# -``ymax``: ``ylim[2]``.
# -``yrange``: ``ylim[2] - ylim[1]``.
# -``ycenter``: ``mean(ylim)``.
# -``rel_height``: Fraction of height of the track to the distance between two neighbouring loops.
# -``abs_height``: The height of the track, which is ``rel_height`` multiplied by the distance between two neighbouring loops.
# -``track_index``: Current track index.
#
TRACK_META = NA
class(TRACK_META) = "TRACK_META"

# == title
# Names of all supported meta data
#
# == param
# -x Always use ``TRACK_META``.
#
# == value
# A vector of characters.
#
# == example
# names(TRACK_META)
names.TRACK_META = function(x) {
	
	nm = c("xlim", "xmin", "xmax", "xcenter", "xrange",
		   "theta_lim", "theta_min","theta_max", "theta_center", "theta_range",
		   "ylim", "ymin", "ymax", "ycenter", "yrange",
		   # "radius_lim", "radius_min", "radius_max", "radius_center", "radius_range",
		   "abs_height", "rel_height", "track_index")

	return(nm)
}

# == title
# Get meta data in the current track
#
# == param
# -x Always use ``TRACK_META``.
# -name Name of the meta name. For all supported names, type ``names(TRACK_META)``.
#
# == details
# The variable ``TRACK_META`` can only be used to get meta data from the "current" track. If the current track
# is not the one you want, you can first use `set_current_track` to set the current track.
#
# There are following meta data for the current track:
#
# -``xlim``: Data range on x-axis.
# -``xmin``: ``xlim[1]``.
# -``xmax``: ``xlim[2]``.
# -``xrange``: ``xlim[2] - xlim[1]``.
# -``xcenter``: ``mean(xlim)``.
# -``theta_lim``: Range of the angles on the spiral, measured in radians.
# -``theta_min``: ``theta_lim[1]``.
# -``theta_max``: ``theta_lim[2]``.
# -``theta_range``: ``theta_lim[2] - theta_lim[1]``.
# -``theta_center``: ``mean(theta_lim)``.
# -``ylim``: Data range on y-axis.
# -``ymin``: ``ylim[1]``.
# -``ymax``: ``ylim[2]``.
# -``yrange``: ``ylim[2] - ylim[1]``.
# -``ycenter``: ``mean(ylim)``.
# -``rel_height``: Fraction of height of the track to the distance between two neighbouring loops.
# -``abs_height``: The height of the track, which is ``rel_height`` multiplied by the distance between two neighbouring loops.
# -``track_index``: Current track index.
#
# == value
# The corresponding value.
"$.TRACK_META" = function(x, name) {
	spiral = spiral_env$spiral

	if(is.null(spiral)) {
		stop_wrap("No spiral has been initialized.")
	}
	if(n_tracks() == 0) {
		stop_wrap("No track has been created.")
	}

	if(name == "xlim") {
		spiral$get_data_x(spiral$xlim)
	} else if(name == "xmin") {
		spiral$get_data_x(spiral$xlim[1])
	} else if(name == "xmax") {
		spiral$get_data_x(spiral$xlim[2])
	} else if(name == "xrange") {
		spiral$xlim[2] - spiral$xlim[1]
	} else if(name == "xcenter") {
		mean(spiral$xlim)
	} else if(name == "theta_lim") {
		spiral$theta_lim
	} else if(name == "theta_min") {
		spiral$theta_lim[1]
	} else if(name == "theta_max") {
		spiral$theta_lim[2]
	} else if(name == "theta_range") {
		spiral$theta_lim[2] - spiral$theta_lim[1]
	} else if(name == "theta_center") {
		mean(spiral$theta_lim)
	} else if(name == "ylim") {
		c(get_track_data("ymin"), get_track_data("ymax"))
	} else if(name == "ymin") {
		get_track_data("ymin")
	} else if(name == "ymax") {
		get_track_data("ymax")
	} else if(name == "yrange") {
		get_track_data("ymax") - get_track_data("ymin")
	} else if(name == "ycenter") {
		(get_track_data("ymin") + get_track_data("ymax"))/2
	} else if(name == "abs_height") {
		get_track_data("rel_height")*( get_track_data("rmax") - get_track_data("rmin") )
	} else if(name == "rel_height") {
		get_track_data("rel_height")
	} else if(name == "track_index") {
		current_track_index()
	} else {
		stop_wrap(qq("'@{name}' is not supported in TRACK_META."))
	}
}

# == title
# Print TRACK_META
#
# == param
# -x The ``TRACK_META`` object.
# -... Additional parameters.
#
# == value
# No value is returned.
#
print.TRACK_META = function(x, ...) {
	cat("Please use in form of `TRACK_META$name`. Type `names(TRACK_META)` for supported names.\n")
}

# == title
# Test whether points are in a track
#
# == param
# -x X-location of data points.
# -y Y-location of data points.
# -track_index Index of track.
#
# == value
# A logical vector.
#
is_in_track = function(x, y, track_index = current_track_index()) {
	s = spiral_env$spiral
	xlim = s$xlim
	ylim = get_track_data("ylim", track_index)
	ylim = sort(ylim)

	x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2]
}
