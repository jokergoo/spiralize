


spiral_layout()
spiral_axis()



n = 1000
v = runif(n)

spiral_layout(xlim = c(0, n), start = 360, height = 0.8, end = 360*6)

spiral_barplot(v, pos = 1:n - 0.5)


df = cranlogs::cran_downloads("ggplot2", from="2015-01-01")
day_diff = as.numeric(df$date[nrow(df)] - df$date[1])

year_mean = tapply(df$count, lubridate::year(df$date), function(x) mean(x[x > 0]))

df$diff = log2(df$count/year_mean[as.character(lubridate::year(df$date))])
df$diff[is.infinite(df$diff)] = 0
q = quantile(abs(df$diff), 0.99)
df$diff[df$diff > q] = q
df$diff[df$diff < -q] = -q

p1 = grid.grabExpr({
spiral_initialize(xlim = c(0, nrow(df)) + 0.5, start = 360, end = 360*(day_diff/364) + 360, padding = unit(2, "cm"), flip = "horizontal")
spiral_track(height = 0.8, background_gp = gpar(col = NA, fill = "#EEEEEE"))
spiral_horizon(1:nrow(df) - 0.5, df$diff, use_bars = TRUE)
days_in_a_year = c(365, 366, 365, 365, 365, 366, 365)
for(i in seq_along(days_in_a_year)) {
	if(i == 1) {
		spiral_highlight(0.5, days_in_a_year[1] + 0.5, gp = gpar(col = i + 1), type = "line")
	} else {
		spiral_highlight(sum(days_in_a_year[1:(i-1)]) + 0.5, min(sum(days_in_a_year[1:i]), nrow(df)) + 0.5, gp = gpar(col = i + 1), type = "line")
	}
}

d = seq(-15, -360, by = -30) %% 360
for(i in seq_along(d)) {
	foo = polar_to_cartesian(d[i]/180*pi, (spiral_env$spiral$max_radius + 1)*1.05)
	grid.text(month.name[i], x = foo[1, 1], y = foo[1, 2], default.unit = "native",
		rot = ifelse(d[i] > 0 & d[i] < 180, d[i] - 90, d[i] + 90), gp = gpar(fontsize = 10))
}


lgd = packLegend(
	Legend(title = "Year", at = 2015:2021, type = "lines", legend_gp = gpar(col = 2:12, lwd = 2), background = "white"),
	Legend(title = "Relative difference to year mean", at = c("Lower", "higher"), legend_gp = gpar(fill = c("#313695", "#D73027")))
)

draw(lgd, x = unit(1, "npc") + unit(1, "cm"), just = "left")

grid.text("log2(difference to year mean downloads)\nggplot2, 2015-01-01 ~ 2021-06-16", gp = gpar(fontface = "bold"),
	x = unit(1, "npc") + unit(1, "cm"),
	y = unit(1, "npc") - unit(10, "mm"), just = c("left", "top"))


}, width = 5, height = 6.5)

p2 = grid.grabExpr({
spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/365 + 1), padding = unit(2, "cm"))
v = df$count
v[v > quantile(v, 0.95)] = quantile(v, 0.95)
spiral_track(height = 0.9)
spiral_horizon(1:nrow(df), v)

d = seq(15, 360, by = 30)
for(i in seq_along(d)) {
	foo = polar_to_cartesian(d[i]/180*pi, (.env$max_radius + 1)*1.05)
	grid.text(month.name[i], x = foo[1, 1], y = foo[1, 2], default.unit = "native",
		rot = ifelse(d[i] > 0 & d[i] < 180, d[i] - 90, d[i] + 90))
}
grid.text("absolute downloads per day\nggplot2, 2014-01-01 ~ 2020-12-31", y = unit(1, "npc") + unit(10, "mm"), just = "bottom")
}, width = 5, height = 6.5)


library(cowplot)

plot_grid(p2, p1)



spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/365 + 1), padding = unit(2, "cm"))
spiral_track(height = 0.9)
n = nrow(df)
col = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
spiral_rect(1:n - 1, 0, 1:n, 1, gp = gpar(fill = col(df$diff), col = NA))




spiral_initialize()
spiral_track()
for(x in seq(0.1, 0.9, by = 0.1)) {
	spiral_raster(x, 0.5, image, facing = "downward", )
}

image = sample(dir("~/Downloads/IcoMoon-Free-master/PNG/64px", full.names = TRUE), 10)


k = 100
dend = as.dendrogram(hclust(dist(runif(k))))

spiral_initialize(xlim = c(0, k))
spiral_track(height = 0.4)
spiral_dendrogram(dend, facing = "outside")
spiral_track(height = 0.4)
spiral_dendrogram(dend, facing = "inside")


spiral_initialize()
spiral_track()

n = 1000
x0 = runif(n)
y0 = runif(n)
x1 = x0 + runif(n, min = -0.01, max = 0.01)
y1 = 1 - y0

spiral_segments(x0, y0, x1, y1, gp = gpar(col = rand_color(n)))


n_nodes = nrow(edge) + 1
n_leaves = obj$Nnode + 1

node_pos = rep(-1, n_nodes)
node_pos[1:n_leaves] = 1:n_leaves

e = new.env(parent = emptyenv())
while(1) {
	e$flag = TRUE
	tapply(1:nrow(edge), edge[, 1], function(ind) {
		m = edge[ind, , drop = FALSE]
		if(node_pos[ m[1, 1] ] < 0) {
			if(all(node_pos[ m[, 2] ] > 0)) {
				node_pos[ m[1, 1] ] <<- mean(node_pos[ m[, 2] ])
				qqcat("get value for node @{m[1, 1]}\n")
			} else {
				e$flag = FALSE
			}
		}
	})
	if(e$flag) break
}


node_height = rep(-1, n_nodes)
node_height[1:n_leaves] = 0

while(1) {
	e$flag = TRUE
	tapply(1:nrow(edge), edge[, 1], function(ind) {
		m = edge[ind, , drop = FALSE]
		if(node_height[ m[1, 1] ] < 0) {
			if(all(node_height[ m[, 2] ] >= 0)) {
				node_height[ m[1, 1] ] <<- max(node_height[ m[, 2] ] + edge.length[ind])
				qqcat("get value for node @{m[1, 1]}\n")
			} else {
				e$flag = FALSE
			}
		}
	})
	if(e$flag) break
}

plot(NULL, xlim = c(0, max(node_height)), ylim = c(0, n_leaves))
for(i in seq_len(nrow(edge))) {
	i1 = edge[i, 1]
	i2 = edge[i, 2]
	segments(node_height[i1], node_pos[i1], node_height[i1], node_pos[i2])
	segments(node_height[i1], node_pos[i2], node_height[i2], node_pos[i2])
}

pdf("~/test.pdf", width = 10, height = 10)
spiral_initialize(xlim = c(0, 50645), start = 60, end = 360*8+90)
spiral_track(border = FALSE)

d2 = phylo_to_dendrogame(d)
cl = cutree(d2, h = 100)
tb = table(cl)
x = cumsum(tb)
x = c(0, x)
x1 = x[-length(x)]
x2 = x[-1]
l = x2 - x1 > 100
x1 = x1[l]
x2 = x2[l]
spiral_rect(x1, 0, x2, 1, gp = gpar(fill = rand_color(length(x1), transparency = 0.5), col = NA))

spiral_phylo(d, log = T, gp = gpar(lwd = 0.2, linejoin = "square", lend = "butt"), facing = "outside")

dev.off()



load(system.file("extdata", "chinese_dynasty.RData", package = "HilbertCurve"))
n = nrow(chinese_dynasty)
spiral_initialize(xlim = range(c(chinese_dynasty$start, chinese_dynasty$end)))
spiral_track(height = 0.6)
spiral_axis(major_at = seq(-1000, 1900, by = 100))
h = runif(n, min = 0.2, max = 0.8)
spiral_rect(chinese_dynasty$start, 0.5 - h/2, chinese_dynasty$end, 0.5 + h/2, gp = gpar(fill = rand_color(n, luminosity = "light"), col = NA))
spiral_text( (chinese_dynasty$start + chinese_dynasty$end)/2, 0.5, chinese_dynasty$name, gp = gpar(fontsize = 8, fontfamily = fontfamily))



spiral_initialize()
spiral_track()
x = seq(0, 1, length = 24)
spiral_text(x, 0.5, strrep(letters[x], 10), facing = "outside")

spiral_initialize()
spiral_track()
spiral_yaxis()


#############


x0 = sort(runif(200))
x0 = matrix(x0, ncol = 2, byrow = TRUE)
x1 = sort(runif(200))
x1 = matrix(x1, ncol = 2, byrow = TRUE)

spiral_initialize()
spiral_track()

for (i in 1:100) {
    pt1 = circlize:::get_bezier_points(x0[i, 1], 0, x1[i, 1], 1, xlim = c(0, 1), ylim = c(0, 1))
    pt2 = circlize:::get_bezier_points(x0[i, 2], 0, x1[i, 2], 1, xlim = c(0, 1), ylim = c(0, 1))

    spiral_polygon(c(x0[i, 1], x0[i, 2], pt2[, 1], rev(pt1[, 1]), x0[i, 1]),
            c(0, 0, pt2[, 2], rev(pt1[, 2]), 0), 
        gp = gpar(fill = rand_color(1, luminosity = "bright", transparency = 0.5), col = NA))
}

###########
tree = readRDS("~/project/development/spiralize/inst/extdata/life_tree_Nat_Microbiol_2016.rds")

cate1 = gsub("^([^_]+)_.*$", "\\1", tree$tip.label)
nlevel1 = length(unique(cate1))
cate2 = gsub("^.*?_(.*?_.*?_).*$", "\\1", tree$tip.label)
nlevel2 = length(unique(cate2))

cate1_col = structure(rand_color(nlevel1), names = unique(cate1))
cate2_col = structure(rand_color(nlevel2), names = unique(cate2))

n = length(x1)

spiral_initialize(xlim =c(0, n), scale_by = "curve_length", reverse = TRUE)
spiral_track()
spiral_phylo(tree)



r2 = rle(cate2)

for(i in seq_along(r2$lengths)) {
    if(i == 1) {
        spiral_highlight(0, r2$lengths[1], gp = gpar(fill = cate2_col[r2$values[1]]))
    } else {
        spiral_highlight(sum(r2$lengths[1:(i-1)]), sum(r2$lengths[1:i]), gp = gpar(fill = cate2_col[r2$values[i]]))
    }
}

r1 = rle(cate1)

for(i in seq_along(r1$lengths)) {
    if(i == 1) {
        spiral_highlight(0, r1$lengths[1], gp = gpar(col = cate1_col[r1$values[1]]), type = "line", line_width = unit(1, "mm"))
    } else {
        spiral_highlight(sum(r1$lengths[1:(i-1)]), sum(r1$lengths[1:i]), gp = gpar(col = cate1_col[r1$values[i]]), type = "line", line_width = unit(1, "mm"))
    }
}


#######
df = readRDS("~/project/development/spiralize/inst/extdata/CNS_tumour_classification.rds")
n = nrow(df)
spiral_initialize(xlim = c(0, n))
spiral_track(height = 0.2)
spiral_rect(1:n - 1, 0, 1:n, 1, gp = gpar(fill = df$meth_col, col = NA))
spiral_track(height = 0.2)
spiral_rect(1:n - 1, 0, 1:n, 1, gp = gpar(fill = df$tumor_col, col = NA))
spiral_track(height = 0.3, background = FALSE)

r1 = rle(as.vector(df$tumor_type))

for(i in seq_along(r1$lengths)) {
    if(i == 1) {
        spiral_text(r1$lengths[1]/2, 0.5, r1$values[1], facing = "curved_inside")
    } else {
        spiral_text( (sum(r1$lengths[1:(i-1)]) + sum(r1$lengths[1:i]))/2, 0.5, r1$values[i], facing = "curved_inside")
    }
}


draw_tangent = function(theta) {
	s = spira_env$spiral
	a = s$tangent_slope(theta)
	r = s$curve(theta)
	df = polar_to_cartesian(theta, r)

	x = df$x
	y = df$y

	b = y - a*x

	x0 = seq(x - 1, x + 1, length = 100)
	y0 = a*x0 + b

	grid.lines(x0, y0, default.units = "native") 
}


spiral_initialize(flip = "horizontal")
s = current_spiral()
theta = pi*seq(2, 3, length = 10)
len = s$spiral_length(theta)
solve_theta_from_spiral_length(len) 



p1 = grid.grabExpr({
	spiral_initialize(c(0,5000), end = 365*30, scale_by = "curve")
	spiral_track(height = 1, background = F)
	spiral_points(1:5000-0.5, 0.5, pch = 16, size = unit((x1+1)/5, "mm"), gp = gpar(col = x1 + 1))
})

p2 = grid.grabExpr({
	spiral_initialize(c(0,5000), end = 365*30, scale_by = "curve")
	spiral_track(height = 1, background = F)
	spiral_points(1:5000-0.5, 0.5, pch = 16, size = unit((x2+1)/5, "mm"), gp = gpar(col = x2 + 1))
})

plot_grid(p1, p2)

