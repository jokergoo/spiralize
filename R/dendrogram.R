construct_dend_segments = function(dend, gp = gpar()) {

    if(is.null(attr(dend, "x"))) {
        dend = adjust_dend_by_x(dend)
    }

    x_is_unit = inherits(attr(dend, "x"), "unit")
    height_is_unit = inherits(attr(dend, "height"), "unit")

    env = new.env(parent = emptyenv())
    env$x0 = NULL
    env$y0 = NULL
    env$x1 = NULL
    env$y1 = NULL
    env$col = NULL
    env$lty = NULL
    env$lwd = NULL

    generate_children_dendrogram_segments = function(dend, env = NULL) {

        if(is.leaf(dend)) {
            
            return(NULL)
        }

        height = attr(dend, "height")
        height_is_zero = abs(height - 0) < 1e-10
        
        nc = length(dend)
        
        xl = lapply(seq_len(nc), function(i) attr(dend[[i]], "x"))
        yl = lapply(seq_len(nc), function(i) attr(dend[[i]], "height"))
        xl = unlist(xl)
        yl = unlist(yl)
        
        max_x = max(xl)
        min_x = min(xl)
        mid_x = (max_x + min_x)*0.5

        # graphic parameters for current branch
        edge_gp_list = lapply(seq_len(nc), function(i) as.list(attr(dend[[i]], "edgePar")))
        for(i in c(setdiff(seq_len(nc), c(1, nc)), c(1, nc))) {
            for(gp_name in c("col", "lwd", "lty")) {
                # gp for two segments
                if(is.null(edge_gp_list[[i]][[gp_name]])) {
                    gpa = rep(get.gpar(gp_name)[[gp_name]], 2)
                } else {
                    gpa = rep(edge_gp_list[[i]][[gp_name]], 2)
                }

                env[[gp_name]] = c(env[[gp_name]], gpa)
            }
           
            if(height_is_zero) {
                
                env$x0 = c(env$x0, xl[i])
                env$x1 = c(env$x1, mid_x)
                env$y0 = c(env$y0, height)
                env$y1 = c(env$y1, height)
                
            } else {
                env$x0 = c(env$x0, xl[i], xl[i])
                env$x1 = c(env$x1, xl[i], mid_x)
                env$y0 = c(env$y0, yl[i], height)
                env$y1 = c(env$y1, height, height)
            }
        }
    }

    # per depth
    if(is.leaf(dend)) {
        return(list())
    }

    dend_list = list(dend)

    env$node_x = c(env$node_x, attr(dend, "x"))
    env$node_y = c(env$node_y, attr(dend, "height"))

    while(1) {

        if(length(dend_list) == 0) break

        for(i in seq_along(dend_list)) {
            generate_children_dendrogram_segments(dend_list[[i]], env)
        }

        # on their children nodes for non-leaf nodes
        dend_list = dend_list[ !sapply(dend_list, is.leaf) ]
        dend_list2 = list()
        for(i in seq_along(dend_list)) {
            dend_list2 = append(dend_list2, dend_list[[i]])
        }
        dend_list = dend_list2
    }
    
    lt = as.list(env)

    if("col" %in% names(gp)) {
        lt$col = gp$col
    }
    if("lwd" %in% names(gp)) {
        lt$lwd = gp$lwd
    }
    if("lty" %in% names(gp)) {
        lt$lty = gp$lty
    }
    return(lt)

}

# == title
# Draw dendrogram
#
# == param
# -dend A ``stats::dendrogram`` object.
# -facing Facing of the dendrogram.
# -gp Graphics parameters of the dendrogram edges.
# -track_index Index of the track. 
#
# == details
# Note the dendrogram edges can be rendered with the `dendextend::dendextend` package.
#
# == example
# k = 500
# dend = as.dendrogram(hclust(dist(runif(k))))
# spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
# spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))
# spiral_dendrogram(dend, facing = "inside")
#
# \donttest{
# require(dendextend)
# dend = color_branches(dend, k = 4)
# spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
# spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))
# spiral_dendrogram(dend, facing = "inside")
# }
spiral_dendrogram = function(dend, facing = c("inside", "outside"), gp = gpar(), 
    track_index = current_track_index()) {

    lt = construct_dend_segments(dend, gp)

    xlim = range(lt[c("x0", "x1")])
        
    ymax = get_track_data("ymax", track_index)

    ymax2 = max(lt$y0, lt$y1)

    lt$y0 = lt$y0/ymax2 * ymax
    lt$y1 = lt$y1/ymax2 * ymax

    facing = match.arg(facing)[1]
    if(facing %in% c("outside")) {
        lt$y0 = ymax - lt$y0
        lt$y1 = ymax - lt$y1
    }    
    spiral_segments(lt$x0, lt$y0, lt$x1, lt$y1, gp = gpar(lwd = lt$lwd, lty = lt$lty, col = lt$col), track_index = track_index)      
}


############# for phylo object ############

construct_phylo_segments = function(obj) {
    if(!inherits(obj, "phylo")) {
        stop_wrap("The input should be a 'phylo' object.")
    }

    edge = obj$edge
    edge.length = obj$edge.length
    n_nodes = nrow(edge) + 1
    n_leaves = length(obj$tip.label)
    n_edges = nrow(edge)

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
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    node_height = rep(-1, n_nodes)
    node_height[1:n_leaves] = 0

    edge.length[is.na(edge.length)] = 0

    while(1) {
        e$flag = TRUE
        tapply(1:nrow(edge), edge[, 1], function(ind) {
            m = edge[ind, , drop = FALSE]
            if(node_height[ m[1, 1] ] < 0) {
                if(all(node_height[ m[, 2] ] >= 0)) {
                    node_height[ m[1, 1] ] <<- max(node_height[ m[, 2] ] + edge.length[ind])
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    lt = list(x0 = numeric(n_edges*2), y0 = numeric(n_edges*2), x1 = numeric(n_edges*2), y1 = numeric(n_edges*2))
    for(i in seq_len(n_edges)) {
        i1 = edge[i, 1]
        i2 = edge[i, 2]

        lt$x0[2*i + c(-1, 0)] = c(node_pos[i1], node_pos[i2])
        lt$y0[2*i + c(-1, 0)] = c(node_height[i1], node_height[i1])
        lt$x1[2*i + c(-1, 0)] = c(node_pos[i2], node_pos[i2])
        lt$y1[2*i + c(-1, 0)] = c(node_height[i1], node_height[i2])
    }

    lt

}

# == title
# Draw phylogenetic tree
#
# == param
# -obj A ``phylo`` object.
# -facing Facing of the tree.
# -gp Graphics parameters of the tree edges.
# -log Whether the height of the tree should be log-transformed (log10(x + 1))?
# -reverse Whether the tree should be reversed?
# -track_index Index of the track. 
#
# == example
# require(ape)
# data(bird.families)
# n = length(bird.families$tip.label)
# spiral_initialize(xlim = c(0, n), start = 360, end = 360*3)
# spiral_track(height = 0.8)
# spiral_phylo(bird.families)
spiral_phylo = function(obj, facing = c("inside", "outside"), gp = gpar(), 
    log = FALSE, reverse = FALSE, track_index = current_track_index()) {

    lt = construct_phylo_segments(obj)

    if(log) {
        lt$y0 = log10(lt$y0 + 1)
        lt$y1 = log10(lt$y1 + 1)
        lt$y0[is.infinite(lt$y0)] = 0
        lt$y1[is.infinite(lt$y1)] = 0
        lt$y0[lt$y0 < 0] = 0
        lt$y1[lt$y1 < 0] = 0
    }

    ymax = get_track_data("ymax", track_index)
    
    ymax2 = max(lt$y0, lt$y1)

    lt$y0 = lt$y0/ymax2 * ymax
    lt$y1 = lt$y1/ymax2 * ymax

    facing = match.arg(facing)[1]
    if(facing %in% c("outside")) {
        lt$y0 = ymax - lt$y0
        lt$y1 = ymax - lt$y1
    }

    lt$x0 = lt$x0 - 0.5
    lt$x1 = lt$x1 - 0.5

    if(reverse) {
        spiral = spiral_env$spiral
        lt$x0 = spiral$xlim[2] - lt$x0
        lt$x1 = spiral$xlim[2] - lt$x1
    }

    spiral_segments(lt$x0, lt$y0, lt$x1, lt$y1, gp = gp, track_index = track_index)
}

# == title
# Convert a phylo object to a dendrogram object
#
# == param
# -obj A ``phylo`` object.
# -log Whether the height of the phylogenetic tree should be log-transformed (log10(x + 1)).
#
# == details
# The motivation is that phylogenetic tree may contain polytomies, which means at a certain node,
# there are more than two children branches. Available tools that do the conversion only support binary trees.
#
# The returned ``dendrogram`` object is not in its standard format which means it can not be properly
# drawn by the ``plot.dendrogram`` function. However, you can still apply dendextend::`dendextend::cutree` to the returned
# ``dendrogram`` object with no problem and the dendrogram can be properly drawn with the ComplexHeatmap package.
#
# == example
# require(ape)
# data(bird.families)
# d = phylo_to_dendrogram(bird.families)
#
# require(ComplexHeatmap)
# grid.dendrogram(d, test = TRUE)
phylo_to_dendrogram = function(obj, log = FALSE) {
    if(!inherits(obj, "phylo")) {
        stop_wrap("The input should be a 'phylo' object.")
    }

    edge = obj$edge
    edge.length = obj$edge.length
    n_nodes = nrow(edge) + 1
    n_leaves = length(obj$tip.label)
    n_edges = nrow(edge)

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
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    node_height = rep(-1, n_nodes)
    node_height[1:n_leaves] = 0

    edge.length[is.na(edge.length)] = 0

    while(1) {
        e$flag = TRUE
        tapply(1:nrow(edge), edge[, 1], function(ind) {
            m = edge[ind, , drop = FALSE]
            if(node_height[ m[1, 1] ] < 0) {
                if(all(node_height[ m[, 2] ] >= 0)) {
                    node_height[ m[1, 1] ] <<- max(node_height[ m[, 2] ] + edge.length[ind])
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    if(log) {
        node_height = log10(node_height + 1)
        node_height[is.infinite(node_height)] = 0
        node_height[node_height < 0] = 0
    }

    lt_ind = vector("list", length = nrow(edge) + 1)
    dend = list()
    attributes(dend) = list(
        class = "dendrogram",
        node_id = edge[1, 1],
        height = node_height[edge[1, 1]],
        x = node_pos[edge[1, 1]],
        members = n_leaves,
        code = "0"
    )
    for(i in seq_len(nrow(edge))) {
        parent = edge[i, 1]
        children = edge[i, 2]
        if( parent == edge[1, 1] ) {
            i_children = length(dend)
        } else {
            i_children = length(dend[[ lt_ind[[parent]] ]])
        }

        lt_ind[[ children ]] = c(lt_ind[[parent]], i_children + 1)
        if(children <= n_leaves) {
            dend[[ lt_ind[[ children ]] ]]= children
            attributes(dend[[ lt_ind[[ children ]] ]]) = list(
                class = "dendrogram",
                leaf = TRUE,
                node_id = children,
                height = node_height[ children ],
                x = node_pos[ children ],
                members = 1,
                label = children,
                code = paste(c(0, lt_ind[[ children ]]), collapse = "")
            )
        } else {
            dend[[ lt_ind[[ children ]] ]]= list()
            attributes(dend[[ lt_ind[[ children ]] ]]) = list(
                class = "dendrogram",
                node_id = children,
                height = node_height[ children ],
                x = node_pos[ children ],
                code = paste(c(0, lt_ind[[ children ]]), collapse = "")
            )
        }
    }

    dend
}
