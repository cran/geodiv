## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("geodiv")

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("bioXgeo/geodiv")

## ----package code for show, eval = FALSE--------------------------------------
#  library(geodiv)
#  library(raster)
#  library(rasterVis)
#  library(mapdata)
#  library(maptools)
#  library(rgeos)
#  library(ggplot2)
#  library(tidyverse)
#  library(parallel)
#  library(sf)
#  library(rasterVis)
#  library(ggmap)
#  library(corrplot)
#  library(gridExtra)
#  library(cowplot)
#  library(factoextra)
#  library(cluster)

## ----run packages, results='asis', echo=FALSE, include=FALSE------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
library(geodiv)
library(raster)
library(rasterVis)
library(mapdata)
library(maptools)
library(rgeos)
library(ggplot2)
library(tidyverse)
library(parallel)
library(sf)
library(rasterVis)
library(ggmap)
library(corrplot)
library(gridExtra)
library(cowplot)
library(factoextra)
library(cluster)

## ---- echo=TRUE---------------------------------------------------------------
# Load the orforest data into your active session.
data(orforest)
# Check out the properties of the orforest raster layer.
orforest

## ---- echo=TRUE---------------------------------------------------------------
# Plot orforest without the trend removed.
eviCols <- colorRampPalette(c('lightyellow1', 'darkgreen'))(100)
eviTheme <- rasterTheme(region = eviCols)
(orig_ndvi <- rasterVis::levelplot(orforest, margin = F,
                                   par.settings = eviTheme, xlab = 'Longitude',
                                   ylab = 'Latitude', main='orforest original'))

## ---- echo=TRUE---------------------------------------------------------------
# Remove a polynomial trend.
orfor_rem <- remove_plane(orforest)

## ---- echo=TRUE---------------------------------------------------------------
# Calculate global metrics over the entire orforest image.
(sa <- sa(orforest)) # average roughness
(sbi <- sbi(orforest)) # surface bearing index
(std <- std(orforest, create_plot = FALSE, option = 1))

## ---- eval = TRUE-------------------------------------------------------------
# Texture image creation using 'focal_metrics' function.
window <- matrix(1, nrow = 7, ncol = 7)
system.time(
output_raster <- focal_metrics(orforest, window,
                                metrics = list('sa'),
                                progress = TRUE)
)

print(output_raster)

# Texture image creation using 'texture_image' function.
system.time(output_raster2 <- texture_image(orforest, window_type = 'square', 
                                            size = 7, in_meters = FALSE, 
                                            metric = 'sa', parallel = FALSE, 
                                            nclumps = 100))
print(output_raster2)

## ---- eval=TRUE, fig.width=7.5, fig.height=6----------------------------------
rasterVis::levelplot(output_raster2, margin = F, par.settings = eviTheme,
                       ylab = NULL, xlab = NULL, main = 'Sa')

## ---- eval = TRUE-------------------------------------------------------------
# Download data from figshare. 
fs_data <- list("https://ndownloader.figshare.com/files/24366086",
                 "https://ndownloader.figshare.com/files/28259166")

# Set timeout option to 1000s to make sure downloads succeed.
options(timeout = 1000)

# Function to download rasters, including setting a tempfile.
get_raster <- function(rasts) {
  tf <- tempfile()
  download.file(rasts, destfile = tf, mode = 'wb')
  outrast <- raster(tf)
  return(outrast)
}


## ---- warning=FALSE-----------------------------------------------------------
# Download data from figshare.
evi <- get_raster(fs_data[[1]]) * 0.0001

## ---- warning=FALSE-----------------------------------------------------------
evi <- raster::aggregate(evi, fact = 4)

## ---- warning=FALSE-----------------------------------------------------------
state <- maps::map(database = 'state', regions = 'oregon',
                   fill = TRUE, plot = FALSE)
statePoly <- map2SpatialPolygons(state, IDs = state$names,
                                 proj4string = CRS(proj4string(evi)))
evi_masked <- mask(x = evi, mask = statePoly)

## ---- warning=FALSE-----------------------------------------------------------
# plot maximum growing season EVI for Oregon
rasterVis::levelplot(evi_masked, margin = F, par.settings = eviTheme,
                     ylab = NULL, xlab = NULL,
                     main = 'Maximum Growing Season EVI')

## ---- warning=FALSE-----------------------------------------------------------
evi_masked <- remove_plane(evi_masked)

# plot again to see what the new raster looks like
rasterVis::levelplot(evi_masked, margin = F, par.settings = eviTheme,
                     ylab = NULL, xlab = NULL, main = 'EVI without Trend')

## ---- eval=TRUE---------------------------------------------------------------
# Calculate EVI 'sa' texture image for state of Oregon.
system.time(outrast <- texture_image(evi_masked, window_type = 'square', 
                                     size = 5, in_meters = FALSE, metric = 'sa', 
                                     parallel = FALSE, nclumps = 100))


## ---- eval = FALSE------------------------------------------------------------
#  m_list <- list('sa', 'sq', 's10z', 'sdq', 'sdq6', 'sdr', 'sbi', 'sci', 'ssk',
#                 'sku', 'sds', 'sfd', 'srw', 'std', 'svi', 'stxr', 'ssc', 'sv',
#                 'sph', 'sk', 'smean', 'spk', 'svk', 'scl', 'sdc')
#  
#  outrasts <- list()
#  system.time(for (i in 1:length(m_list)) { # figure out 16, 24 error
#      outrasts[[i]] <- texture_image(evi_masked, window_type = 'square',
#                                     size = 15, in_meters = FALSE,
#                                     metric = m_list[[i]], parallel = TRUE,
#                                     nclumps = 100)})
#  outrasts <- stack(unlist(outrasts))
#  
#  data_evi <- data.frame(x = coordinates(outrasts)[, 1],
#                         y = coordinates(outrasts)[, 2])
#  for (i in 1:34) {
#    data_evi[, i + 2] <- outrasts[[i]][]
#  }
#  names(data_evi) <- c('x', 'y', 'Sa', 'Sq', 'S10z', 'Sdq', 'Sdq6', 'Sdr', 'Sbi',
#                       'Sci', 'Ssk', 'Sku', 'Sds', 'Sfd', 'Srw', 'Srwi', 'Shw',
#                       'Std', 'Stdi', 'Svi', 'Str 0.2', 'Str 0.3', 'Ssc', 'Sv',
#                       'Sp', 'Sk', 'Smean', 'Spk', 'Svk', 'Scl min 0.2',
#                       'Scl max 0.2', 'Scl min 0.3', 'Scl max 0.3', 'Sdc 0-5%',
#                       'Sdc 50-55%', 'Sdc 80-85%')

## ---- warning=FALSE-----------------------------------------------------------
# The list of figshare files was completed above, so grab the appropriate files
# for the csv of all texture image outputs for Oregon.
tf <- tempfile()
download.file(fs_data[[2]], destfile = tf, mode = 'wb')
data_evi <- read.csv(tf, stringsAsFactors = FALSE)
unlink(tf)


## ---- warning=FALSE-----------------------------------------------------------
for (i in c(9, 10, 18, 6)) {
  raster::hist(data_evi[, i], breaks = 30, xlab = names(data_evi)[i], main = '')
}

## ---- warning=FALSE, fig.width=7.5, fig.height=8------------------------------
# New names for plots
plt_names <- data.frame(old = names(data_evi)[3:ncol(data_evi)],
                        new = c('Sa', 'Sq', 'S10z', 'Sdq', 'Sdq6', 'Sdr',
                                'Sbi', 'Sci', 'Ssk', 'Sku', 'Sds',
                                'Sfd', 'Srw', 'Srwi', 'Shw', 'Std', 'Stdi',
                                'Svi', 'Str (0.2)', 'Str (0.3)', 'Ssc', 'Sv', 
                                'Sp', 'Sk', 'Smean', 'Spk', 'Svk', 
                                'Scl - min (0.2)', 'Scl - max (0.2)', 
                                'Scl - max (0.3)', 'Scl - max (0.3)',
                                'Sdc 0-5%', 'Sdc 50-55%', 'Sdc 80-85%'))

create_maps <- function(df, r, theme) {
  maps_list <- list()
  for (i in seq(3, ncol(df))) {
    temp <- setValues(r, df[, i])
    temp[is.na(r)] <- NA
    goodname <- as.character(plt_names$new[plt_names$old == names(df)[i]])
    maps_list[[i - 2]] <- rasterVis::levelplot(temp, margin = F,
                                               par.settings = theme,
                                               ylab = NULL, xlab = NULL,
                                               main = goodname)
    maps_list[[i - 2]]$par.settings$layout.heights[
      c( 'bottom.padding',
         'top.padding',
         'key.sub.padding',
         'axis.xlab.padding',
         'key.axis.padding',
         'main.key.padding') ] <- 1
    maps_list[[i - 2]]$aspect.fill <- TRUE
    names(maps_list)[i - 2] <- goodname
  }
  return(maps_list)
}

# Create plots of all possible surface gradient metrics that geodiv calculates
# for EVI.
evi_maps <- create_maps(data_evi, evi_masked, eviTheme)

# Create map panels.
grid.arrange(grobs = evi_maps[1:12], nrow = 4, ncol = 3) # 850x800
grid.arrange(grobs = evi_maps[13:24], nrow = 4, ncol = 3) # 850x800
grid.arrange(grobs = evi_maps[25:34], nrow = 4, ncol = 3) # 850x800


## ---- warning=FALSE-----------------------------------------------------------
# Convert the rasters to dataframe format and add value to dataframe with
# metric values.
sp_df <- function(r, df) {
  pixdf <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  df$value <- pixdf[, 1]
  return(df)
}

data_evi <- sp_df(evi, data_evi)


## ---- warning=FALSE-----------------------------------------------------------

# Create new dataframe of values along a latitudinal transect.
get_transect <- function(r, df) {
  # Crop raster to center transect (+/- 7 pixels North or South).
  center_row <- round(nrow(r) / 2)
  r_crop <- crop(r, extent(r, center_row - 7, center_row + 7, 1, ncol(r)))

  # Get 8th latitudinal coordinate (center latitude) from the cropped raster.
  central_y <- unique(coordinates(r_crop)[, 2])[8]

  # Get the closest latitude in the dataframe to the central raster coordinate.
  central_y <- unique(df$y[near(df$y, central_y, 0.01)])[1]

  # Extract mean EVI and elevation values by raster column.
  r_means <- colMeans(as.matrix(r_crop), na.rm = TRUE)

  # Now limit the dataframe to the central row across the transect.
  transect_vals <- df[df$y == central_y,]

  # Add column means to dataframe.
  transect_vals$value <- r_means

  return(transect_vals)
}

transect_evi <- get_transect(evi, data_evi)


## ---- warning=FALSE-----------------------------------------------------------
# Get all metrics on same scale (0-1).
scale_mets <- function(df) {
  for (i in 3:ncol(df)) {
    df[,i] <- (df[, i] - min(df[, i], na.rm = TRUE)) /
      (max(df[, i], na.rm = TRUE) - min(df[, i], na.rm = TRUE))
  }
  return(df)
}

transect_evi <- scale_mets(transect_evi)

## ---- warning=FALSE-----------------------------------------------------------

# Remove NA values from the metric columns.
rm_nas <- function(df) {
  for (i in 3:ncol(df)) {
    df <- df[!is.na(df[, i]),]
  }
  return(df)
}

transect_evi <- rm_nas(transect_evi)


## ---- warning=FALSE-----------------------------------------------------------
### Plot optimal number of clusters
plot_gap <- function(df) {
  # enhanced k-means clustering
  res.km <- clusGap(t(df)[3:(ncol(df) - 1), ], stats::kmeans, K.max = 10,
                    B = 100, nstart = 25)
  # gap statistic plot
  fviz_gap_stat(res.km)
}

plot_gap(transect_evi)

### Dendrogram and scatterplot of clusters
get_clusters <- function(df, nclust) {
  # Enhanced hierarchical clustering using optimal # of clusters.
  res.hc <- eclust(t(df)[3:(ncol(df) - 1),],
                   "hclust", k = nclust)

  return(res.hc)
}

plot_dendrogram <- function(res.hc, nclust){
  # Plot colors
  plt_cols <- c('lightgoldenrod1', 'lightblue', 'grey', 'lightsteelblue4')

  # Dendrogram plot
  fviz_dend(res.hc, rect = FALSE, k_colors = plt_cols[1:nclust],
            lwd = 1, label_cols = 'black', cex = 0.8, main = "", ylab = "",
            type = 'rectangle', horiz = TRUE, labels_track_height = 14) +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
}

res.hc_evi <- get_clusters(transect_evi, nclust = 3)

plot_dendrogram(res.hc_evi, nclust = 3)


## ---- warning=FALSE-----------------------------------------------------------

# Create gathered (long) version of dataframe for the clustering analysis.
gather_data <- function(df) {
  df <- df %>% gather(key = 'var', value = 'value',
                      names(df[, seq(3, ncol(df))]))

  # Order variables.
  df <- df[order(df$var),]

  return(df)
}

gathered_evi <- gather_data(transect_evi)

## ---- warning=FALSE, , fig.width=7.5, fig.height=10---------------------------

# Plot metrics along transect, with cluster labeled.
plot_transect_mets <- function(df, res.hc, varname) {
  # Map colors to cluster or variable names.
  col_map <- c("1" = "lightgoldenrod1", "2" = "lightblue", "3" = "grey",
               "4" = "lightsteelblue4", "EVI" = "white", "Elev" = "white")

  # Create a dataframe to match variable names with cluster number.
  clust_df <- data.frame(var = res.hc$labels, clust = res.hc$cluster)
  clust_df <- clust_df[order(clust_df$clust),]

  # Convert var to character.
  clust_df$var <- as.character(clust_df$var)

  # Join cluster number with main dataframe to get cluster labels for plotting.
  df <- left_join(df, clust_df, by = 'var')

  # Anything not labeled with a cluster (i.e., the actual value) gets labeled.
  df$clust[is.na(df$clust)] <- varname

  # Change 'value' label to actual variable name.
  df$var[df$var == 'value'] <- varname

  # Convert cluster names to factors and match with colors.
  df$clust <- as.factor(df$clust)
  df$var <- factor(df$var, levels = c(clust_df$var, varname))
  cols_to_use <- col_map[names(col_map) %in% df$clust]

  ggplot(df, aes(x = x, y = value)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                  fill = clust)) +
    geom_line(lwd = 0.7) +
    xlab('Longitude') +
    facet_grid(var~., switch = 'y') +
    scale_fill_manual(values = cols_to_use, name = 'Cluster') +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text.y.left = element_text(face = 'bold', size = 11, angle = 0),
          legend.position = 'none',
          axis.title.x = element_text(face = 'bold', size = 11))
}

plot_transect_mets(gathered_evi, res.hc_evi, "EVI")


## -----------------------------------------------------------------------------
# Get data ready for PCA by removing NA values.
clean_data <- function(df) {
  # Remove columns with very large numbers of NAs.
  NAs <- sapply(df, function(x) sum(is.na(x)))
  rm_cols <- which(NAs >= 90000)
  df <- df[, -rm_cols]
  # Remove NAs from remaining columns.
  df <- na.omit(df)
  return(df)
}

data_evi_noNA <- clean_data(data_evi)


## -----------------------------------------------------------------------------
# Calculate the principal components.
evi_prc <- prcomp(data_evi_noNA[,3:23], center = TRUE, scale = TRUE)
summary(evi_prc)

## ---- fig.width=7.5, fig.height=4---------------------------------------------
# Take a look at the components using a screeplot.
plot_scree <- function(pc_dat) {
  screeplot(pc_dat, type = "l", npcs = 15,
            main = "Screeplot of the first 10 PCs")
  abline(h = 1, col = "red", lty = 5)
  legend("topright", legend = c("Eigenvalue = 1"),
         col = c("red"), lty = 5, cex = 0.6)
}

plot_scree(evi_prc)

## ---- , fig.width=7.5, fig.height=4-------------------------------------------
# Look at how much variance is explained using a cumulative variance plot.
plot_cvar <- function(pc_dat) {
  # Get cumulative variance explained.
  cumpro <- summary(pc_dat)$importance[3, ][1:16]

  # Create plot of cumulative variance, marking the 5th component as the cutoff.
  plot(cumpro, xlab = "PC #", ylab = "Amount of explained variance",
       main = "Cumulative variance plot")
  abline(v = 5, col = "blue", lty = 5)
  abline(h = cumpro[5], col = "blue", lty = 5)
  legend("topleft", legend = c("Cut-off @ PC5"),
         col = c("blue"), lty = 5, cex = 0.6)
}

plot_cvar(evi_prc)


## ---- fig.width=7.5, fig.height=4---------------------------------------------

# Map components across state.
map_comps <- function(pc_dat, noNA_df, full_df, r, theme) {
  # Add pc values to no-NA dataframe.
  for (i in 1:5) {
    colname <- paste0('prc', i)
    noNA_df[, colname] <- pc_dat$x[, i]
  }

  # Add PCA results back to full raster dataframe.
  full_dat <- full_df %>% left_join(noNA_df)
  # Cut to only the prc columns.
  full_dat <- full_dat[, grep('prc', names(full_dat))]

  # Create rasters and maps with principle component values.
  out_maps <- list()
  for (i in 1:5) {
    new_rast <- setValues(r, full_dat[, i])
    pc_map <- rasterVis::levelplot(new_rast, margin = F,
                                   par.settings = theme,
                                   ylab = NULL, xlab = NULL,
                                   main = paste0('PC', i))
    pc_map$par.settings$layout.heights[c( 'bottom.padding',
                                          'top.padding',
                                          'key.sub.padding',
                                          'axis.xlab.padding',
                                          'key.axis.padding',
                                          'main.key.padding') ] <- 1
    pc_map$aspect.fill <- TRUE
    out_maps[[i]] <- pc_map
  }

  # Plot in a grid.
  grid.arrange(grobs = out_maps, nrow = 2, ncol = 3)
}

map_comps(evi_prc, data_evi_noNA, data_evi, evi, eviTheme)


## ---- fig.width=7.5, fig.height=4---------------------------------------------

# Plot principal component loadings.
plot_loadings <- function(pc_dat) {
  # Get rotation for top 5 components.
  loadings <- pc_dat$rotation[, 1:5]

  # Figure out the relative loadings.
  aload <- abs(loadings)
  rel <- sweep(aload, 2, colSums(aload), "/")

  # Convert relative loadings to dataframe.
  rel <- as.data.frame(rel)
  # Get good variable names (from dataframe created earlier).
  rel$var <- plt_names$new[match(rownames(rel), plt_names$old)]

  # Create importance plots.
  imp_plts <- list()
  for (i in 1:5) {
    temp <- rel
    # Determine whether component loading is postive or negative.
    temp$sign <- factor(sapply(loadings[, i], FUN = function(x) x / abs(x)),
                        levels = c(-1, 1))

    # Order loadings by value.
    temp <- temp[order(temp[, i]),]

    temp$var <- factor(temp$var, levels = temp$var)

    temp_plt <- ggplot(temp, aes(x = temp[, i], y = var)) +
      geom_point(size = 3, aes(pch = sign)) +
      scale_shape_manual(name = element_blank(),
                         breaks = c(1, -1),
                         values = c(19, 8),
                         labels = c("Positive", "Negative")) +
      xlab(paste0('PC', i)) +
      ylab('Metric') +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_blank(),
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 12))

    imp_plts[[i]] <- temp_plt
  }

  # Return grid of first three components.
  grid.arrange(grobs = imp_plts[1:3], ncol = 3)
}

plot_loadings(evi_prc)

