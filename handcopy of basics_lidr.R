# load 'lidR' package
library(lidR)
library(terra)
library(tiff)

# define input path to laz files
my_input_dir <- file.path('G:/Temp/B/Uchino/data/laz_normalized')

# list all laz files
?list.files
my_laz_files <- list.files(my_input_dir,  # base R package for listing file names/directories in a specific folder
                        pattern = '*.laz', # file type
                        full.names = T)

# read one laz file
laz.1 <- lidR::readLAS(my_laz_files[1])
laz.1
laz_keep_first <- readLAS(my_laz_files[1], filter = "-keep_first")
laz_keep_first
 # read only the first returns
# ? is it like reading only the first layer?

laz.2 <- lidR::readLAS(my_laz_files[2])
laz.2

# plot
lidR::plot(laz.1)
lidR::plot(laz_keep_first)



#----

my_input_dir <- file.path('G:/Temp/B/Uchino/data/chm')

# list all tif files
my_tif_files <- list.files(my_input_dir,
                           pattern = '*.tif',
                           full.names = T)

# read one raster CHM
chm.1 <- rast(my_tif_files[1])
chm.1

chm.2 <- rast(my_tif_files[2])
chm.2


# assign CRS
crs(chm.1) <- "EPSG:25832"
chm.1

crs(chm.2) <- "EPSG:25832"
chm.2

# plotting
terra::plot(chm.1)
terra::plot(chm.2)



# forest canopy gap ----

#Loading terra and viridis library
library(ForestGapR)
library(viridis)

# ALS-derived CHM over Adolpho Ducke Forest Reserve - Brazilian tropical forest
data(ALS_CHM_DUC)
str(ALS_CHM_DUC)

# Plotting chm
plot(ALS_CHM_DUC, col=viridis(10))

# Setting height thresholds (e.g. 10 meters)
threshold<- 20
size<-c(1,1000) # m2

# Detecting forest gaps
gaps_duc<-ForestGapR::getForestGaps(chm_layer=ALS_CHM_DUC, threshold=threshold, size=size)
# ?getForestGaps

# Plotting gaps
plot(gaps_duc, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)


# convert SpatRaster to RasterLayer 
rast
chm_layer.1 <- readTIFF('G:/Temp/B/Uchino/data/chm/2023_06_solling_32_546_5729.tif')

# with the given raster data
gaps_duc<-ForestGapR::getForestGaps(chm_layer = chm.1, threshold=threshold, size=size)
# ?getForestGaps
plot(gaps_duc)
