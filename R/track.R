
track_env = new.env()

track_env$current_track = 0
empty_track_data = data.frame(
	i = integer(0), 
	ymin = numeric(0),
	ymax = numeric(0),
	rmin = numeric(0),
	rmax = numeric(0),
	rel_height = numeric(0)
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

set_current_track = function(i) {
	track_env$current_track = i
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
# == value
# A numeric value of the corresponding field.
get_track_data = function(field, track_index = current_track_index()) {

	if(field == "ycenter") {
		(track_env$track_data[track_index, "ymin"] + track_env$track_data[track_index, "ymax"])/2
	} else if(field == "ylim") {
		c(track_env$track_data[track_index, "ymin"] + track_env$track_data[track_index, "ymax"])
	} else if(field == "yrange") {
		track_env$track_data[track_index, "ymax"] - track_env$track_data[track_index, "ymin"]
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
# An integer of the number of avaiable tracks.
n_tracks = function() {
	nrow(track_env$track_data)
}
