

# == title
# Visualize git commits
#
# == param
# -repo Path of the git repo. The value can be a single repo or a vector of repos.
# -show_legend Whether to show the legend.
# -start Start date. By default it is the first date of the commit. The value can be a string such as "2022-01-01" or a ``Date`` object.
# -end End date. By default it is the current date. The value can be a string such as "2022-01-01" or a ``Date`` object.
# -pt_range Range of the point sizes.
# -commits_range Range of the numbers of commits.
# -type Type of the plot.
# -colors If type is the heatmap, it controls the list of colors.
#
spiral_git_commits = function(repo = ".", show_legend = TRUE,
    start = NULL, end = Sys.Date(),
    pt_range = c(2, 16), commits_range = c(2, 20),
    type = c("points", "heatmap"),
    colors = c("#3288BD", "#99D594", "#E6F598", "#FFFFBF", "#FEE08B", "#FC8D59", "#D53E4F")) {

    type = match.arg(type)[1]

    df_all = list()
    for(rp in repo) {
        if(!file.exists(paste0(rp, "/.git/"))) {
            stop_wrap(qq("'@{rp}' is not a git repository."))
        }

        rp = normalizePath(rp)

        od = getwd()
        oe = try({
            setwd(rp)
            df = read.table(pipe("git log --date=short --pretty=format:%ad | sort | uniq -c"))
        }, silent = TRUE)
        setwd(od)

        if(inherits(oe, "try-error")) {
            stop(oe)
        }
        colnames(df) = c("commits", "date")

        df$date = as.Date(df$date)
        df_all[[rp]] = df
    }

    if(length(repo) == 1) {
        repo_name = basename(normalizePath(repo))
    } else {
        repo_name = paste0(length(repo), " packages")
    }

    df_all = do.call(rbind, df_all)

    if(is.null(start)) {
        start = min(df_all$date)
    } else {
        start = as.Date(start)
    }
    if(is.null(end)) {
        end = max(df_all$date)
    } else {
        end = as.Date(end)
    }

    df_all = df_all[df_all$date >= start & df_all$date <= end, , drop = FALSE]

    start_year = year(start)
    end_year = year(end)

    d = start + seq(1, end - start + 1) - 1
    n = numeric(length(d))

    for(i in seq_len(nrow(df_all))) {
        ind = as.double(difftime(df_all[i, "date"], start), "days") + 1
        n[ind] = n[ind] + df_all[i, "commits"]
    }
    if(type == "points") {
        l = n > 0
        n = n[l]
        d = d[l]
    }

    pt_range = pt_range + 0
    commits_range = commits_range + 0
    calc_pt_size = function(x) {
        pt_size = (pt_range[2] - pt_range[1])/(commits_range[2] - commits_range[1])*(x - commits_range[1]) + pt_range[1]
        pt_size[x > commits_range[2]] = pt_range[2]
        pt_size[x < commits_range[1]] = pt_range[1]
        pt_size
    }

    col_fun = circlize::colorRamp2(seq(max(min(n), commits_range[1]), min(max(n), commits_range[2]), length = length(colors)), colors)

    spiral_initialize_by_time(c(start, end), verbose = FALSE, normalize_year = TRUE)
    spiral_track()
    if(type == "points") {
        spiral_points(d, 0.5, pch = 16, size = unit(calc_pt_size(n), "pt"))
    } else {
        spiral_rect(d-0.5, 0, d+0.5, 1, gp = gpar(fill = col_fun(n), col = NA))
    }
    
    for(y in start_year:end_year) {
        spiral_text(paste0(y, "-01-01"), 0.5, y, gp = gpar(fontsize = 8, col = ifelse(type == "points", "#808080", "black")), facing = "inside")
    }

    upViewport()
    grid.text(repo_name, x = unit(0, "npc") + unit(4, "pt"), y = unit(1, "npc") - unit(4, "pt"), just = c("left", "top"), gp = gpar(fontsize = 14))

    if(show_legend) {
        breaks = pretty(commits_range, 3)
        if(breaks[1] == 0) {
            breaks[1] = 1
        }
        breaks = unique(breaks)
        labels = breaks
        if(max(n) > commits_range[2]) {
            labels[length(labels)] = paste0("[", labels[length(labels)], ", ", max(n), "]")
        }
        if(type == "points") {
            lgd = ComplexHeatmap::Legend(title = "#commits", at = breaks, labels = labels, type = "points", pch = 16, size = unit(calc_pt_size(breaks), "pt"))
        } else {
            lgd = ComplexHeatmap::Legend(title = "#commits", at = breaks, labels = labels, col_fun = col_fun)
        }
        ComplexHeatmap::draw(lgd, x = unit(0, "npc") + unit(4, "pt"), y = unit(1, "npc") - unit(24, "pt"), just = c("left", "top"))
    }

}

