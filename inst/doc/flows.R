## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.height = 6
)

## -----------------------------------------------------------------------------
library(flows)
# Import data
nav <- read.csv(system.file("csv/nav.csv", package = "flows"))
head(nav, 4)
# Prepare data
mat <- prepare_mat(x = nav, i = "i", j = "j", fij = "fij")
mat[1:4, 1:4]

## ----fig.width=5, fig.height=5------------------------------------------------
# Get statistics about the matrix
stat_mat(mat = mat, output = "none", verbose = TRUE)

# Plot Lorenz curve only
stat_mat(mat = mat, output = "lorenz", verbose = FALSE)

## ----fig.width=7, fig.height=7------------------------------------------------
# Graphics only
stat_mat(mat = mat, output = "all", verbose = FALSE)

# Statistics only
mystats <- stat_mat(mat = mat, output = "none", verbose = FALSE)
str(mystats)
# Sum of flows
mystats$sumflows

## ----fig.height = 4, fig.width=4----------------------------------------------
# Remove the matrix diagonal
diag(mat) <- 0

# Selection of flows > 500
mat_sel_1 <- select_flows(mat = mat, method = "xfirst", k = 500, global = TRUE)

# Selection of flows > 1000
mat_sel_2 <- select_flows(mat = mat, method = "xfirst", k = 1000, global = TRUE)

# Compare initial matrix and selected matrices
compare_mat(mat1 = mat, mat2 = mat * mat_sel_1, digits = 0)
compare_mat(mat1 = mat, mat2 = mat * mat_sel_2, digits = 0)

## ----fig.height = 6, fig.width=6----------------------------------------------
# Percentage of each outgoing flows
mat_p <- mat / rowSums(mat) * 100

# Select flows that represent at least 20% of the sum of outgoing flows for
# each urban area.
mat_p_sel <- select_flows(mat = mat_p, method = "xfirst", k = 20)

# Compare initial and selected matrices
compare_mat(mat1 = mat, mat2 = mat * mat_p_sel, digits = 2)

## -----------------------------------------------------------------------------
res <- nodal_flows(mat)

compare_mat(mat1 = mat, mat2 = res)

## ----fig.height=5.77, fig.width=6, eval = TRUE--------------------------------
library(sf)
library(mapsf)
UA <- st_read(system.file("gpkg/GE.gpkg", package = "flows"),
  layer = "urban_area", quiet = TRUE
)
GE <- st_read(system.file("gpkg/GE.gpkg", package = "flows"),
  layer = "region", quiet = TRUE
)
mf_map(GE, col = "#c6deba", border = NA, expandBB = c(0, 0, 0, .25))
out <- map_nodal_flows(
  mat = mat, x = UA,
  col_node = c("red", "orange", "yellow"),
  col_flow = "grey30",
  leg_pos = "topright",
  leg_flow = "Nb. of commuters",
  breaks = c(4, 100, 1000, 2500, 8655),
  lwd = c(1, 4, 8, 16), add = TRUE
)
mf_label(out$nodes[out$nodes$w > 6000, ],
  var = "name",
  halo = TRUE, overlap = FALSE
)
mf_title("Dominant Flows of Commuters")
mf_credits("INSEE, 2011")
mf_scale()
head(out$nodes[order(out$nodes$w, decreasing = TRUE), 2:3, drop = TRUE])

