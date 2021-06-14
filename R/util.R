
as.degree = function(x, scale = TRUE) {
	if(scale) {
		(x/pi * 180) %% 360
	} else {
		(x/pi * 180)
	}
}



stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

message_wrap = function (...)  {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}

subset_gp = function (gp, i) {
    gp = lapply(gp, function(x) {
        if (length(x) == 1)
            x
        else x[i]
    })
    class(gp) = "gpar"
    return(gp)
}


read_image = function(image) {

	image[is.na(image)] = ""
	l = grepl("^\\s*$", image)
	image[l] = ""
	
	allowed_image_type = c("png", "svg", "pdf", "eps", "jpeg", "jpg", "tiff")

	if(inherits(image, "character")) { ## they are file path
		image_type = tolower(gsub("^.*\\.(\\w+)$", "\\1", image))
		if(! all(image_type[image_type != ""] %in% allowed_image_type)) {
			stop_wrap("image file should be of png/svg/pdf/eps/jpeg/jpg/tiff.")
		}
	} else {
		stop_wrap("`image` should be a vector of path.")
	}

	n_image = length(image)
	image_list = vector("list", n_image)
	image_class = vector("character", n_image)
	for(i in seq_along(image)) {
		if(image[i] == "") {
			image_list[[i]] = NA
			image_class[i] = NA
		} else if(image_type[i] == "png") {
			if(!requireNamespace("png")) {
				stop_wrap("Need png package to read png images.")
			}
			image_list[[i]] = png::readPNG(image[i])
			attr(image_list[[i]], "image_class") = "raster"
		} else if(image_type[i] %in% c("jpeg", "jpg")) {
			if(!requireNamespace("jpeg")) {
				stop_wrap("Need jpeg package to read jpeg/jpg images.")
			}
			image_list[[i]] = jpeg::readJPEG(image[i])
			attr(image_list[[i]], "image_class") = "raster"
		} else if(image_type[i] == "tiff") {
			if(!requireNamespace("tiff")) {
				stop_wrap("Need tiff package to read tiff images.")
			}
			image_list[[i]] = tiff::readTIFF(image[i])
			attr(image_list[[i]], "image_class") = "raster"
		} else if(image_type[i] %in% c("pdf", "eps")) {
			if(!requireNamespace("grImport")) {
				stop_wrap("Need grImport package to read pdf/eps images.")
			}
			temp_file = tempfile()
			getFromNamespace("PostScriptTrace", ns = "grImport")(image[[i]], temp_file)
			image_list[[i]] = grImport::readPicture(temp_file)
			file.remove(temp_file)
			attr(image_list[[i]], "image_class") = "grImport::Picture"
		} else if(image_type[i] == "svg") {
			if(!requireNamespace("grImport2")) {
				stop_wrap("Need grImport2 package to read svg images.")
			}
			# if(!requireNamespace("rsvg")) {
			# 	stop_wrap("Need rsvg package to convert svg images.")
			# }
			temp_file = tempfile()
			# get it work on bioconductor build server
			oe = try(getFromNamespace("rsvg_svg", ns = "rsvg")(image[i], temp_file))
			if(inherits(oe, "try-error")) {
				stop_wrap("Need rsvg package to convert svg images.")
			}
			image_list[[i]] = grImport2::readPicture(temp_file)
			file.remove(temp_file)
			attr(image_list[[i]], "image_class") = "grImport2::Picture"
		}
	}
	image_list
}

get_theta_from_len = function(len, interval) {
	spiral = spiral_env$spiral
	n = length(len)
	t = numeric(n)
	for(i in seq_len(n)) {
		t[i] = uniroot(function(theta, a) {
			spiral$spiral_length(theta) - a
		}, interval = interval, a = len[i])$root
	}
	t
}
