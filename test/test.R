


spiral_layout()
spiral_axis()



n = 1000
v = runif(n)

spiral_layout(xlim = c(0, n), start = 360, height = 0.8, end = 360*6)

spiral_barplot(v, pos = 1:n - 0.5)


df = cranlogs::cran_downloads("ggplot2", from="2014-01-01", to = "2020-12-31")
day_diff = as.numeric(df$date[nrow(df)] - df$date[1])

year_mean = tapply(df$count, lubridate::year(df$date), function(x) mean(x[x > 0]))

df$diff = log2(df$count/year_mean[as.character(lubridate::year(df$date))])
df$diff[is.infinite(df$diff)] = 0

p1 = grid.grabExpr({
spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/365 + 1), padding = unit(2, "cm"))
spiral_track(height = 0.9)
spiral_horizon(1:nrow(df), df$diff)

d = seq(15, 360, by = 30)
for(i in seq_along(d)) {
	foo = polar_to_cartesian(d[i]/180*pi, (.env$max_radius + 1)*1.05)
	grid.text(month.name[i], x = foo[1, 1], y = foo[1, 2], default.unit = "native",
		rot = ifelse(d[i] > 0 & d[i] < 180, d[i] - 90, d[i] + 90))
}
grid.text("log2(difference to year mean)\nggplot2, 2014-01-01 ~ 2020-12-31", y = unit(1, "npc") + unit(10, "mm"), just = "bottom")

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


