#The development version:
#library(pacman)
#p_load(devtools, terra, tiff, viridis, raster)

# Libraries ---------------------------------------------------------------
{library(pacman)
p_load(devtools, 
       terra, 
       tiff, 
       viridis, 
       raster,
       sf,
       tidyverse)
devtools::install_github("carlos-alberto-silva/ForestGapR")
}

# set working directory
my_input_dir <- file.path('G:/Temp/B/Uchino/data/chm')
my_input_dir <- file.path('E:/data/chm_tiles_solling')

# list all tif files
my_tif_files <- list.files(my_input_dir,
                           pattern = '*.tif',
                           full.names = T)

# extract desired files only on the right-side of the forest
# trials

s <- seq(20, 30)
s

s[grep("20|21|22|23", s)]

(files <- list.files(my_input_dir, pattern = "_54[7-9]_|(550)_$\\.tif", full.names = TRUE))
(files <- list.files(my_input_dir, pattern = "_54[7-9]_|550_$\\.tif", full.names = TRUE))

# wrong
(files <- list.files(my_input_dir, pattern = "_54[7-9]_|550_572[3-9]", full.names = T))
# AI but still wrong
(files <- list.files(my_input_dir, pattern = "_54[7-9]_|550_(572[3-9]|573[0-1])\\.tif$", full.names = T))

# developed but still wrong
(files <- list.files(my_input_dir, 
                     pattern = c("_54[7-9]_|550_", "(572[3-9]|573[0-1])\\.tif$"), full.names = T))

cat(5723:5731, sep = "|")

# extract area with the red dots on QGIS (where data from early 2024 available)
# so far still long but the most efficient way
(right_side_tif_files <- 
    list.files(my_input_dir,  
               pattern = "_(547|548|549|550)_(5723|5724|5725|5726|5727|5728|5729|5730|5731)\\.tif$", 
               full.names = TRUE))

# assign CRS
chm.2 <- rast(my_tif_files[2])
crs(chm.2) <- "EPSG:25832"

#################### for chm.1 ############################
# {
# # read one raster CHM
# chm.1 <- rast(my_tif_files[1])
# chm.1 # resolution = 0.5 m x 0.5 m = 50 cm2
# rast
# 
# crs(chm.1) <- "EPSG:25832"
# chm.1
# terra::size(chm.1)
# 
# # plotting
# terra::plot(chm.1)# forest canopy gap ----
# 
# # Plotting chm
# plot(chm.1, col=viridis(10))
# }
# gaps_duc <- ForestGapR::getForestGaps(chm_layer = chm.1, threshold=threshold, size=size)
# ?getForestGaps
# 
# plot(gaps_duc, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)
# 
###########################################################

# Setting height thresholds (e.g. 10 meters)
threshold<- 20
size<-c(1,1000) # m2. The max and min size to include. Outside this range is not 
# considered as a 'gap'

# plotting chm.2
plot(chm.2, col = viridis(10))

# gap encoding, plot the map & gaps, get stats at the same time
{
gaps_chm.2 <- ForestGapR::getForestGaps(chm_layer = chm.2, threshold = 15, size = size)
plot(chm.2, col = viridis(10))
plot(gaps_chm.2, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)
gaps_stats_chm.2 <- ForestGapR::GapStats(gap_layer = gaps_chm.2, chm_layer = chm.2)
print(gaps_stats_chm.2)
}


# for (i in nthresholds) {
#   gaps_i <- ForestGapR::getForestGaps(chm_layer = chm.2, threshold = i, size = size)
#   names(gaps_i) <- paste0("gaps_", i, "m")
#   gaps_stack <- raster::stack(gaps_stack, gaps_i)
# } # 2024/5/6 maybe not needed?

# Create an empty list to store raster layers ----
gaps_layers <- list()

# get gaps with ForestGapR()
nthresholds <- c(10, 12, 15, 20, 25)

for (i in nthresholds) {
  gaps_i <- ForestGapR::getForestGaps(chm_layer = chm.2, threshold = i, size = size)
  names(gaps_i) <- paste0("gaps_", i, "m")
  
  # Add the current forest gap raster layer to the list
  gaps_layers[[length(gaps_layers) + 1]] <- gaps_i
}

str(gaps_layers) #see the outcome of the list of layers

# plot gaps
{
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))

plot(chm.2, col = viridis(10), main = "Height threshold 10m")
plot(gaps_layers[[1]], col = "red", add = TRUE, legend = FALSE)


plot(chm.2, col = viridis(10), main = "Height threshold 12m")
plot(gaps_layers[[2]], col = "red", add = TRUE, legend = FALSE)

plot(chm.2, col = viridis(10), main = "Height threshold 15m")
plot(gaps_layers[[3]], col = "red", add = TRUE, legend = FALSE)

plot(chm.2, col = viridis(10), main = "Height threshold 20m")
plot(gaps_layers[[4]], col = "red", add = TRUE, legend = FALSE)

plot(chm.2, col = viridis(10), main = "Height threshold 25m")
plot(gaps_layers[[5]], col = "red", add = TRUE, legend = FALSE)

plot(chm.2, col = viridis(10), main = "Template")
}

par(oldpar)

# get gap stats ----
{
stats.1 <- ForestGapR::GapStats(gap_layer = gaps_layers[[1]], chm_layer = chm.2)
stats.2 <- ForestGapR::GapStats(gap_layer = gaps_layers[[2]], chm_layer = chm.2)
stats.3 <- ForestGapR::GapStats(gap_layer = gaps_layers[[3]], chm_layer = chm.2)
stats.4 <- ForestGapR::GapStats(gap_layer = gaps_layers[[4]], chm_layer = chm.2)
stats.5 <- ForestGapR::GapStats(gap_layer = gaps_layers[[5]], chm_layer = chm.2)
}

  # for loop version (under development)
stats_list <- list()
# for (i in length(gaps_layers)) { # fix required
#  stats_list[i] <- ForestGapR::GapStats(gap_layer = gaps_layers[[i]], chm_layer = chm.2)
# } 

stats.1 <- as.data.frame(stats.1)

sum(stats.1$gap_area)

# save as csv
write.csv2(stats.1, "stats_tif_2_10m.csv", row.names = F)
write.csv2(stats.2, "stats_tif_2_12m.csv", row.names = F)
write.csv2(stats.3, "stats_tif_2_15m.csv", row.names = F)
write.csv2(stats.4, "stats_tif_2_20m.csv", row.names = F)
write.csv2(stats.5, "stats_tif_2_25m.csv", row.names = F)


# how to see what exactly functions are doing -----------------------------
View(nlm)
View(sum)
View(ForestGapR::GapStats)
View(ForestGapR::getForestGaps)
View(rnorm)

rnorm(20, 3)
max(rnorm(20, 3))
tapply(1e3, 20, rnorm)

# getStats()->develop-------------------------------------------------

# if the $chm.min is 0, maybe the $chm.max is smaller (which leads to potential inclusion of 'trivial' gaps)
stats.1

min.0 <- stats.1$chm_max[stats.1$chm_min == 0]
min.all <- stats.1$chm_max

mean(min.0) # 5.73
mean(min.all) # 7.55

# is this significantly different?
max(min.0) # 10 m
# so this means that gaps btwn canopy with height 10 m & tree stem on the ground are also recognised as 'gaps'
# Idk if this is desirable

# data exploration of right side tif files

# overlay chm-tif & copernicus-tif ---------------------------------------
## 5/24 data exploration of right side tif files ---------------------------
con_1_tif <- rast(right_side_tif_files[8])
str(con_1_tif)

terra::plot(con_1_tif, col = viridis(10))

con_1_shp <- st_read("E:/Uchino_gast/con_1.shp")

# prj <- "E:/Uchino_gast/con_1.prj"
# con_1_prj <- readLines(prj)
# 
# # Adjust Coordinates
# con_1_crs <- st_crs(con_1_prj) # now it works 
# str(con_1_prj)
# 
# con_1_shp <- st_transform(con_1_tif, crs = con_1_crs) # st_transform can't be used for .tif files / SpatRaster
# # rather it should be a .shp file
# con_1_shp <- st_transform(con_1_shp, crs = con_1_crs) # works 
# con_1_shp <- st_transform(con_1_shp, crs = crs(con_1_tif)) # works 
# 
# # another way (short cut?)
# con_1_shp <- st_transform(con_1_shp, crs(con_1_tif)) # another way (maybe better?) 
# str(con_1_tif)

# assign crs to the tif file
# crs(con_1_tif) <- con_1_crs # not working
crs(con_1_tif) <- st_crs(con_1_shp)$prof4string # good

# set the same extent for tif and shp
ext(con_1_tif) <- ext(con_1_shp)
ext(con_1_tif) <- ext(st_bbox(con_1_shp)) # alternative (making sure that it's a square shape?)

# crop 
new_con_1_tif <- crop(con_1_tif, ext(con_1_shp)) # now it works, but before the extents did not overlap
plot(new_con_1_tif) # here the plot is with coordinates that are more common (N 51.72 & E 9.68)
# the shape gets slimmer for some reason
# ? is it really cropped?

plot(con_1_shp)

## FUTURE choose plots to get coniferous & BL stands from QGIS ------------------------------



# github setup ------------------------------------------------------------
p_load(usethis)
edit_git_config()
use_git()

# to crop BL / coniferous stands




