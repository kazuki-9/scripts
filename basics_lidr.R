# load 'lidR' package
library(lidR)
library(terra)

# define input path to laz files
input_dir <- file.path('G:/Temp/B/Uchino/data/laz_normalized')

# list all laz files
laz_files <- list.files(input_dir,
                        pattern = '*.laz',
                        full.names = T)

# read one laz file
laz <- lidR::readLAS(laz_files[1])
laz

# read one laz file (only first returns)
laz_keep_first <- readLAS(laz_files[1], filter = "-keep_first")
laz_keep_first

# plot
lidR::plot(laz_keep_first)




#---

input_dir <- file.path('G:/Temp/B/Uchino/data/chm')

# list all tif files
tif_files <- list.files(input_dir,
                        pattern = '*.tif',
                        full.names = T)

# read one raster CHM
chm <- terra::rast(tif_files[1])
chm

# assign CRS
terra::crs(chm) <- "EPSG:25832"
chm

# plotting
terra::plot(chm)



