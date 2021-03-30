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
#  library(rfigshare)

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
library(rfigshare)

## ---- echo=TRUE---------------------------------------------------------------
# Load the orforest data into your active session.
data(orforest)
# check out the properties of the orforest raster layer.
orforest

## ---- echo=TRUE---------------------------------------------------------------
# Plot orforest without the trend removed.
eviCols <- colorRampPalette(c('lightyellow1', 'darkgreen'))(100)
eviTheme <- rasterVis::rasterTheme(region = eviCols)
(orig_ndvi <- rasterVis::levelplot(orforest, margin = F, 
                                  par.settings = eviTheme, xlab = 'Longitude', 
                                  ylab = 'Latitude', main='orforest original'))

## ---- echo=TRUE---------------------------------------------------------------
# Plot orforest with the trend removed.
orfor_rem <- remove_plane(orforest)
(orig_rem_plot <- rasterVis::levelplot(orfor_rem, margin = F, 
                                      par.settings = eviTheme, 
                                      xlab = 'Longitude', ylab = 'Latitude', 
                                      main='orforest trend removed'))

## ---- echo=TRUE---------------------------------------------------------------
# Plot the actual satellite image.
coords <- coordinates(orforest)
lon <- mean(coords[, 1], na.rm = TRUE)
lat <- mean(coords[, 2], na.rm = TRUE)
api_key <- NA # ADD AN API KEY HERE

## ----private API key, echo = FALSE--------------------------------------------
# api_key <- # PRIVATE API KEY FOR VIGNETTE

## ----remainder of mapping, echo = TRUE, eval = FALSE--------------------------
#  # Register a Google API key.
#  register_google(api_key)
#  
#  # Get a satellite image at the specified longitude and latitude.
#  img <- get_map(c(lon, lat), zoom = 13, maptype = 'satellite',
#                 source = "google")
#  
#  # Create a figure with the satellite image using ggplot.
#  satimg <- ggmap(img) + coord_fixed(1.7) + xlab('Longitude') +
#    ylab('Latitude') +
#    theme(axis.title.x = element_text(size = 12),
#          axis.title.y = element_text(size = 12),
#          plot.margin = unit(c(0, 0, 0, 0), "cm"))
#  
#  # Plot the satellite image next to the original NDVI image.
#  plot_grid(satimg, orig_ndvi, ncol = 2, nrow = 1, scale = c(0.84, 1))

## ----figurename, echo=FALSE, out.width = '90%'--------------------------------
knitr::include_graphics("orig_ndvi_figure.png")

## ---- echo=TRUE---------------------------------------------------------------
# Calculate global metrics over the entire orforest image.
(sa <- sa(orforest)) # average roughness
(sbi <- sbi(orforest)) # surface bearing index
(std <- std(orforest, create_plot = FALSE, option = 1))

## ---- eval = FALSE------------------------------------------------------------
#  # Texture image creation using 'focal_metrics' function.
#  window <- matrix(1, nrow = 7, ncol = 7)
#  system.time(
#  output_rasters <- focal_metrics(orforest, window,
#                                  metrics = list('sa', 'sbi'),
#                                  progress = TRUE)
#  )
#  
#  print(output_rasters)
#  
#  # Texture image creation using 'texture_image' function.
#  metric_list <- c('sa', 'sbi', 'std')
#  system.time(output_rasters2 <- lapply(metric_list, FUN = function(m) {
#    texture_image(orforest, window_type = 'square', size = 7,
#                                  in_meters = FALSE, metric = m,
#                                  parallel = TRUE, nclumps = 100)}))

## ---- eval = TRUE-------------------------------------------------------------
# Download rasters from figshare. The list of data returned here will be used
# throughout the vignette.
url <- fs_download(12834896,  urls_only = TRUE, mine = FALSE, session = NULL,
                   version = 4)

# Set tempfile for rasters
get_raster <- function(url) {
  tf <- tempfile()
  download.file(url, destfile = tf, mode = 'wb')
  outrast <- raster(tf)
  return(outrast)
}

output_rasters2 <- list()
output_rasters2[[1]] <- get_raster(url[[5]])
output_rasters2[[2]] <- get_raster(url[[6]])
output_rasters2[[3]] <- get_raster(url[[7]])


## ---- eval=TRUE, fig.width=7.5, fig.height=6----------------------------------
# Create list of plots.
names <- c('Sa', 'Sbi', 'Std')
rast_list <- unlist(output_rasters2) 
plts <- lapply(seq(1, 3), FUN = function(i) {
  rasterVis::levelplot(rast_list[[i]], margin = F, par.settings = eviTheme, 
                       ylab = NULL, xlab = NULL, main = names[i])
})

# Arrange plots in the list into a grid.
grid.arrange(grobs = plts, nrow = 2)

## ---- warning=FALSE-----------------------------------------------------------
# Download data from figshare. 
elev <- get_raster(url[[4]])
evi <- get_raster(url[[3]]) * 0.0001

## ---- warning=FALSE-----------------------------------------------------------
elev <- aggregate(elev, fact = 8)
evi <- aggregate(evi, fact = 8)

## ---- warning=FALSE-----------------------------------------------------------
state <- maps::map(database = 'state', regions = 'oregon', 
                   fill = TRUE, plot = FALSE)
statePoly <- map2SpatialPolygons(state, IDs = state$names, 
                                 proj4string = CRS(proj4string(evi)))
evi_masked <- mask(x = evi, mask = statePoly)
elev_masked <- mask(x = elev, mask = statePoly)

## ---- warning=FALSE-----------------------------------------------------------
# plot maximum growing season EVI for Oregon
rasterVis::levelplot(evi_masked, margin = F, par.settings = eviTheme, 
                     ylab = NULL, xlab = NULL, 
                     main = 'Maximum Growing Season EVI')

# plot elevation (in meters) for Oregon
elevCols <- colorRampPalette(c('grey7', 'grey93'))(100)
elevTheme <- rasterVis::rasterTheme(region = elevCols)
rasterVis::levelplot(elev_masked, margin = F, par.settings = elevTheme, 
                     ylab = NULL, xlab = NULL, main = 'Elevation (m)')

## ---- warning=FALSE-----------------------------------------------------------
evi_masked <- remove_plane(evi_masked)
elev_masked <- remove_plane(elev_masked) # there was no trend

# plot again to see what the new raster looks like
rasterVis::levelplot(evi_masked, margin = F, par.settings = eviTheme, 
                     ylab = NULL, xlab = NULL, main = 'EVI without Trend')
rasterVis::levelplot(elev_masked, margin = F, par.settings = elevTheme, 
                     ylab = NULL, xlab = NULL, main = 'Elevation without Trend')

## ---- eval=FALSE--------------------------------------------------------------
#  # Calculate elevation sa texture image for state of Oregon
#  system.time(outrast <- texture_image(elev_masked, window_type = 'square', size = 7,
#                           in_meters = FALSE, metric = 'sa', parallel = TRUE,
#                           ncores = 4, nclumps = 20))

## ---- eval=FALSE--------------------------------------------------------------
#  # get raster values
#  vals <- outrast[]
#  
#  # Convert output raster of sa metrics (outrast) to a dataframe for
#  # easier use in subsequent analyses
#  coords <- coordinates(outrast)
#  sa_data_elev <- data.frame(x = coords[, 1], y = coords[, 2], v = vals)

## ---- warning=FALSE-----------------------------------------------------------
# The list of figshare files was completed above, so grab the appropriate files
# for the csv's of all texture image outputs for Oregon.
tf <- tempfile()
download.file(url[[2]], destfile = tf, mode = 'wb')
data_evi <- read.csv(tf, stringsAsFactors = FALSE)
unlink(tf)

tf <- tempfile()
download.file(url[[1]], destfile = tf, mode = 'wb')
data_elev <- read.csv(tf, stringsAsFactors = FALSE)
unlink(tf)


## ---- warning=FALSE-----------------------------------------------------------
for (i in c(9, 10, 18, 6)) {
  hist(data_elev[, i], breaks = 30, xlab = names(data_elev)[i], main = '')
}

## ---- warning=FALSE, fig.width=7.5, fig.height=8------------------------------
# New names for plots
plt_names <- data.frame(old = names(data_elev)[3:ncol(data_elev)],
                        new = c('Shw', 'Srw', 'Srwi', 'Std', 'Stdi', 'S10z',
                                'Sa', 'Sbi', 'Sci', 'Sdc 50-55%', 'Sdc 80-85%',
                                'Sdc 0-5%', 'Sdq6', 'Sdr', 'Sds', 'Sfd', 'Sk',
                                'Sku', 'Smean', 'Sph', 'Spk', 'Sq', 'Ssc',
                                'Ssk', 'Sv', 'Svi', 'Svk'))

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
# for elevation and EVI.
elev_maps <- create_maps(data_elev, elev_masked, elevTheme)
evi_maps <- create_maps(data_evi, evi_masked, eviTheme)

# Make sure that order of maps is the same for both EVI and Elevation.
new_order <- match(plt_names$new, names(evi_maps)) # get order according to names table
evi_maps <- evi_maps[new_order]

# Create map panels (3 each for EVI and elevation).
for (l in list(elev_maps, evi_maps)) {
  grid.arrange(grobs = l[1:12], nrow = 4, ncol = 3) # 850x800
  grid.arrange(grobs = l[13:24], nrow = 4, ncol = 3) # 850x800
  grid.arrange(grobs = l[25:27], nrow = 4, ncol = 3) # 850x800
}


## ---- warning=FALSE-----------------------------------------------------------
# Convert the rasters to dataframe format and add value to dataframe with 
# metric values.
sp_df <- function(r, df) {
  pixdf <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  df$value <- pixdf[, 1]
  return(df)
}

data_elev <- sp_df(elev, data_elev)
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

transect_elev <- get_transect(elev, data_elev)
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

transect_elev <- scale_mets(transect_elev)
transect_evi <- scale_mets(transect_evi)

## ---- warning=FALSE-----------------------------------------------------------

# Remove NA values from the metric columns.
rm_nas <- function(df) {
  for (i in 3:ncol(df)) {
    df <- df[!is.na(df[, i]),]
  }
  return(df)
}

transect_elev <- rm_nas(transect_elev)
transect_evi <- rm_nas(transect_evi)


## ---- warning=FALSE-----------------------------------------------------------
### Plot optimal number of clusters
plot_gap <- function(df) {
  # enhanced k-means clustering
  res.km <- clusGap(t(df)[3:(ncol(df) - 1), ], stats::kmeans, K.max = 10, 
                    B = 100, nstart = 25)
  # gap statistic plot
  factoextra::fviz_gap_stat(res.km)
}

plot_gap(transect_evi)
plot_gap(transect_elev)

### Dendrogram and scatterplot of clusters
get_clusters <- function(df, nclust) {
  # Enhanced hierarchical clustering using optimal # of clusters.
  res.hc <- factoextra::eclust(t(df)[3:(ncol(df) - 1),], 
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

plot_scatter <- function(res.hc) {
  # Scatterplot
  fviz_cluster(res.hc)
}

res.hc_elev <- get_clusters(transect_elev, nclust = 4)
res.hc_evi <- get_clusters(transect_evi, nclust = 3)

plot_dendrogram(res.hc_elev, nclust = 4)
plot_dendrogram(res.hc_evi, nclust = 3)

plot_scatter(res.hc_elev)
plot_scatter(res.hc_evi)


## ---- warning=FALSE-----------------------------------------------------------

# Create gathered (long) version of dataframe for the clustering analysis.
gather_data <- function(df) {
  df <- df %>% tidyr::gather(key = 'var', value = 'value', 
                             names(df[, seq(3, ncol(df))]))
  
  # Order variables.
  df <- df[order(df$var),]
  
  return(df)
}

gathered_elev <- gather_data(transect_elev)
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

plot_transect_mets(gathered_elev, res.hc_elev, "Elev")
plot_transect_mets(gathered_evi, res.hc_evi, "EVI")



## -----------------------------------------------------------------------------
# Get data ready for PCA by removing NA values.
clean_data <- function(df) {
  # Remove columns with very large numbers of NAs.
  NAs <- sapply(df, function(x) sum(is.na(x)))
  rm_cols <- which(NAs >= 20000)
  df <- df[, -rm_cols]
  # Remove NAs from remaining columns.
  df <- na.omit(df)
  return(df)
}

data_elev_noNA <- clean_data(data_elev)
data_evi_noNA <- clean_data(data_evi)


## -----------------------------------------------------------------------------
# Calculate the principal components.
elev_prc <- prcomp(data_elev_noNA[,3:22], center = TRUE, scale = TRUE)
evi_prc <- prcomp(data_evi_noNA[,3:22], center = TRUE, scale = TRUE)
summary(elev_prc)
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

plot_scree(elev_prc)
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

plot_cvar(elev_prc)
plot_cvar(evi_prc)


## -----------------------------------------------------------------------------

# Create scatterplots to look at relationships among principal components.
plot(elev_prc$x[, 1], elev_prc$x[, 2], xlab = "PC1 (27.83%)", 
     ylab = "PC2 (16.13%)", main = "PC1 / PC2 - plot")
plot(elev_prc$x[, 1], elev_prc$x[, 3], xlab = "PC1 (27.83%)", 
     ylab = "PC3 (12.54%)", main = "PC1 / PC3 - plot")
plot(elev_prc$x[, 2], elev_prc$x[, 3], xlab = "PC2 (16.13%)", 
     ylab = "PC3 (12.54%)", main = "PC2 / PC3 - plot")

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

map_comps(elev_prc, data_elev_noNA, data_elev, elev, elevTheme)


## ---- fig.width=7.5, fig.height=4---------------------------------------------
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

plot_loadings(elev_prc)

## ---- fig.width=7.5, fig.height=4---------------------------------------------
plot_loadings(evi_prc)

