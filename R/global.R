

# == title
# Global options
#
# == param
# -... Arguments for the parameters, see "details" section.
# -RESET Whether to reset to default values.
# -READ.ONLY Please ignore.
# -LOCAL Please ignore.
# -ADD Please ignore.
# 
# == details
# There are following global parameters:
# 
# -``min_segment_len`` Minimal length of the segment that partitions a curve.
#
# To access the value of an option: ``spiral_opt$name`` where ``name`` is the name of the option. To set a new value
# for an option: ``spiral_opt$name = new_value``.
#
# == value
# A list of options.
spiral_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
spiral_opt = setGlobalOptions(
	min_segment_len = 1/180*pi,
	help = TRUE
)

spiral_env = new.env()
spiral_env$i_spiral = 0
spiral_env$spiral = NULL


# == title
# Viewport name of current spiral
#
# == value
# A string of the viewport name.
current_spiral_vp = function() {
	paste0("spiral_", spiral_env$i_spiral)
}
