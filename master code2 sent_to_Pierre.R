# Term paper: Modern concepts and methods in macroecology and biogeography
# Topic: Elevation ranges of plants in the Pyrénées
# Yeji Lee & Kazuki Uchino

# Libraries ----
{
  library(tidyverse)
  library(sf)
  library(sp)
  library(raster)  
  library(dplyr)
  library(terra)
  library(RColorBrewer)
  library(car)
  library(ggplot2)
  library(viridis)
  library(cowplot)
}

# Read the Pyrenee plant species data
pyre <- read.csv("data/Pyrenees_procNames.csv", header = T)
head(pyre)
str(pyre)

# Select only necessary columns
pyre_select <- pyre %>% dplyr::select(species, alti_min_data, alti_max_data)
pyre_select <- dplyr::select(pyre, species, alti_min_data, alti_max_data)

# Rename the columns names
names(pyre_select)[2] <- "min"
names(pyre_select)[3] <- "max"

# Remove outliers
pyre_select$max[pyre_select$max >= 3404] <- NA # 3404m is the highest elevation. No species exists higher than that. 

# Remove NAs
pyre_NAsremoved <- na.omit(pyre_select)
str(unique(pyre_NAsremoved$species)) # 4756 species 

rownames(pyre_NAsremoved) <- c(pyre_NAsremoved$species)

# Transposed Data Frame
tran_pyre <- t(pyre_NAsremoved)
tran_pyre <- tran_pyre[-1, ] # Remove 'species' row
colnames(tran_pyre) <- c(pyre_NAsremoved$species) # name columns with species' names

tran_pyre <- data.frame(apply(tran_pyre, 2, as.numeric)) # make the values back to numeric

# ---
duplicated_species <- pyre_NAsremoved$species[duplicated(pyre_NAsremoved$species)]
dp_df <- as.data.frame(table(duplicated_species))

unique(dp_df)
# Create a data frame with duplicated species and their counts
duplicated_species_df <- data.frame(
  species = duplicated_species,
  count = table(duplicated_species)
)

unique(pyre_NAsremoved)
pyre_NAsremoved[unique(pyre_NAsremoved)]
as.data.frame(pyre_NAsremoved[unique(pyre_NAsremoved)])
str(pyre_NAsremoved)

# Find non-unique species names (appear more than once)
species_counts <- table(pyre_NAsremoved$species)
spc.df <- as.data.frame(species_counts)
non_unique_species <- names(species_counts[species_counts > 1])
str(non_unique_species)

unique_species <- names(species_counts[species_counts == 1])
str(unique_species)

which(spc.df$Freq == 5)

spc.df[2579,]

which(pyre_NAsremoved$species == "Lotus corniculatus")

pyre_NAsremoved[2763, ]
pyre_NAsremoved[2764, ]
pyre_NAsremoved[2765, ]
pyre_NAsremoved[2766, ]
pyre_NAsremoved[2767, ]

min(pyre_NAsremoved$min)
which(pyre_NAsremoved$min < 0)

# ---



# Create empty rows
empty_rows <- data.frame(matrix(NA, nrow = 36, ncol = ncol(tran_pyre)))
# max elevation is 3404, that is why nrow = 36
colnames(empty_rows) <- colnames(tran_pyre) # name the columns with the same name 
                          # as the tran_pyre data frame so that we can combine

tran_pyre <- rbind(tran_pyre, empty_rows) # combine

# Change row names 
rownames(tran_pyre)[3:38] <- as.character(seq(0, 3500, 100)) 

# make a range for belts
range_min <- as.numeric(rownames(tran_pyre))
range_max <- range_min + 99

# go back -----
# add the ranges columns
tran_pyre_range <- cbind(range_min, range_max, tran_pyre)

str(tran_pyre_range)
class(tran_pyre_range)

ncol(tran_pyre) # 5099 individuals
nrow(tran_pyre_range) # 36 rows (excluding min,max ) 

# Assign 0 or 1 to NAs
tran_pyre_range$'Abies alba'[3:38] <- ifelse(
  tran_pyre_range$`Abies alba`[1] <= tran_pyre_range$range_max & tran_pyre_range$range_min  <= tran_pyre_range$`Abies alba`[2],
  ifelse(is.na(tran_pyre_range$'Abies alba'), 1, tran_pyre_range$'Abies alba'),
  ifelse(is.na(tran_pyre_range$'Abies alba'), 0, tran_pyre_range$'Abies alba')
)[3:38]

# Assign 0 or 1 to NAs (presence / absence table) -------------------------------------
for (i in 3:ncol(tran_pyre_range)){
tran_pyre_range[3:38, i] <- ifelse(
  tran_pyre_range[1,i] <= tran_pyre_range$range_max & tran_pyre_range$range_min <= tran_pyre_range[2,i],
  ifelse(is.na(tran_pyre_range[ ,i]), 1, tran_pyre_range[ ,i]),
  ifelse(is.na(tran_pyre_range[ ,i]), 0, tran_pyre_range[ ,i])
)[3:38]
}

View(tran_pyre_range)

tran_pyre_range <- tran_pyre_range[1:38, ] # remove NA rows

# Alpha diversity per elevation
tran_pyre_range_alpha <- tran_pyre_range

tran_pyre_range_alpha <- tran_pyre_range_alpha[3:nrow(tran_pyre_range_alpha), ] # delete occurence range rows
tran_pyre_range_alpha$alpha <- rowSums(tran_pyre_range_alpha[ , 3:ncol(tran_pyre_range_alpha)], na.rm = TRUE)

# Delete species columns
tran_pyre_range_alpha <-   
  dplyr::select(tran_pyre_range_alpha, range_min, range_max, alpha)
 
# Alpha diversity
plot(tran_pyre_range_alpha$alpha, xlab = "elevation", ylab = "alpha" ) 

# We can plot this
boxplot(tran_pyre_range_alpha$alpha)
hist(tran_pyre_range_alpha$alpha, col = "grey",
     main = expression(paste(alpha, " richness")), 
     xlab = "Number of species") # either so low or many # uneven distribution of abundances

# Species turnover ----
tran_pyre_range1 <- tran_pyre_range[-1 , ]
tran_pyre_range1 <- tran_pyre_range1[-1 , ]
tran_pyre_range2 <- tran_pyre_range1[, 3:ncol(tran_pyre_range1)]

turnover <- data.frame()
for (j in 1:35) {
  for (i in 1:5099) {
    turnover[j, i] <- tran_pyre_range2[j, i] != tran_pyre_range2[j+1, i]
  } 
}

rowSums(turnover)
turnover <- c(rowSums(turnover))

plot(turnover) 

# raster data -------------------------------------------------------------
# Read shape file
pyre_shp <- st_read("data/shape/Pyrenees.shp")
str(pyre_shp)
plot(st_geometry(pyre_shp), border = "darkgrey")

# Adjust Coordinate
prj <- "data/shape/Pyrenees.prj"
prj_content <- readLines(prj)
prj_crs <- st_crs(prj_content)
pyre_shp <- st_transform(pyre_shp, crs = prj_crs)

head(pyre_shp)

# Read elevation, temperature, precipitation raster data
elev <- rast("data/mx30_grd") + 0
bio1 <- rast("data/CHELSA_bio10_01.tif") # temperature*10
bio12 <- rast("data/CHELSA_bio10_12.tif") # precipitation
plot(bio1, main="Annual mean temperature (10 x C°)", col = inferno(255))

elev <- crop(elev, ext(pyre_shp)) # crop raster layers to smaller extent
# elev <- elev * bio1 / bio1 # to remove ocean and seas 
 # this operation sometimes causes crush on R so I commented out. This 'elev' 
 #is already clopped at least, and it is masked with pyre_shp when plotted, so it may be skipped

bio1 <- crop(bio1, ext(pyre_shp))
bio12 <- crop(bio12, ext(pyre_shp))

# plotting the 3 
par(mfrow = c(1, 1))
plot(elev, main = "Elevation a.s.l. (m)", col = terrain.colors(777))
plot(bio1, main = "Annual mean temperature (10 x C°)", col = inferno(255))
plot(bio12, main = "Mean annual precipitation (mm)", col = blues9)

# with mask()
par(mfrow = c(3, 1))
plot(mask(elev, pyre_shp), main = "Elevation a.s.l. (m)", col = terrain.colors(255))
plot(mask(bio1, pyre_shp), main = "Annual mean temperature (10 x C°)", col = inferno(255))
plot(mask(bio12, pyre_shp), main = "Mean annual precipitation (mm)", col = blues9)

# for loop for a new data frame per belt----------------------------------
# first, make an empty dataframe
{df_belt <- data.frame(matrix(NA, nrow = 6, ncol = length(seq(0, 3400, 100))))
row.names(df_belt) <- c("temp_mean", "temp_sd", "prec_mean", "prec_sd", "area", "alpha")
colnames(df_belt) <- seq(0, 3400, 100)}

elev_values <- values(elev) # second, make this object

for (i in 0:34) { 
  mask <- !is.na(elev_values) & (i*100 <= elev_values) & (elev_values < (i+1)*100)
  xy_coords <- xyFromCell(elev, which(mask))
  
  temp_belt.dummy <- bio1[cellFromXY(bio1, xy_coords)]
  prec_belt.dummy <- bio12[cellFromXY(bio12, xy_coords)]
  
  df_belt[1, i+1] <- mean(temp_belt.dummy$CHELSA_bio10_01, na.rm = TRUE)/10
  df_belt[2, i+1] <- sd(temp_belt.dummy$CHELSA_bio10_01, na.rm = TRUE)/10
  df_belt[3, i+1] <- mean(prec_belt.dummy$CHELSA_bio10_12, na.rm = TRUE)
  df_belt[4, i+1] <- sd(prec_belt.dummy$CHELSA_bio10_12, na.rm = TRUE)
  df_belt[5, i+1] <- sum(ncell(elev_values[mask])) / 1000 # unit: 1000 km^2
}

# add 6th row for alpha per belt
df_belt[6,] <- c(t(tran_pyre_range_alpha$alpha[1:35]))
df_belt.2 <- as.data.frame(t(df_belt))

# alpha per km2
df_belt.2$alpha_per_km2 <- df_belt.2$alpha / df_belt.2$area / 1000 # /1000 for fixing the unit
View(df_belt.2)

# glm -------------------------------------------------------

# lm.1 <- lm(alpha ~ temp_mean * prec_mean * area, data = as.data.frame(df_belt.02))
  # we shouldn't use lm() cos it might give negative values
  # instead we should use glm() with family = poisson
m1 <- glm(alpha ~ temp_mean * prec_mean * area, data = as.data.frame(df_belt.2))
summary(m1)

m2 <- glm(alpha ~ temp_mean + prec_mean + area, data = as.data.frame(df_belt.2))
summary(m2)

m3 <- glm(alpha_per_km2 ~ temp_mean * prec_mean * area, data = as.data.frame(df_belt.2))
summary(m3)

m4 <- glm(alpha_per_km2 ~ temp_mean + prec_mean + area, data = as.data.frame(df_belt.2))
summary(m4) # prec_mean always has a high significance, and interaction doesn't seem to be relevant

# Plots of alpha and estimators -------------------------------------------------------------------
# species richness * elevation
df_belt.2 %>%
  ggplot(aes(x = as.numeric(rownames(df_belt.2)), y= alpha, color = area))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", aes(x = as.numeric(rownames(df_belt.2)), y= alpha, fill = area)) +
  labs(fill = "", x= "Elevation", y="Species richness", title="Species richness")+
  guides(color=guide_legend(title="area [1000 km^2]"))

# Precipitation * area plot
df_belt.2 %>%
  ggplot(aes(x = prec_mean, y= alpha, color = area))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", aes(x = prec_mean, y= alpha, fill = area)) +
  labs(fill = "", x= "Mean annual precipitation [mm]", y="Species richness", title="Species richness")+
  guides(color=guide_legend(title="area [1000 km^2]"))

# temperature * area plot
df_belt.2 %>%
  ggplot(aes(x = temp_mean, y= alpha, color = area))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", aes(x = temp_mean,  y= alpha, fill = area)) +
  labs(fill = "", x= "Mean annual temperature [°C]", y="Species richness", title="Species richness")+
  guides(color=guide_legend(title="area [1000 km^2]"))

# alpha_per_area 
 # * elevation
df_belt.2 %>%
  ggplot(aes(x = as.numeric(rownames(df_belt.2), y= alpha_per_km2, color = temp_mean)))+
  geom_point(size = 2, y = df_belt.2$alpha_per_km2)+
  geom_smooth(method = "lm", aes(x = as.numeric(rownames(df_belt.2)), y= alpha_per_km2, fill = temp_mean)) +
  labs(fill = "", x= "Elevation", y="Species richness per km^2", title="Species richness")+
  guides(color=guide_legend(title="Mean annual temperature [°C]"))

# Create a grid raster (like the one we used in the practical)----------
# Set the desired resolution for the grid (adjust as needed)
grid_resolution <- 0.008333333 

# Create the grid based on the extent of the region shapefile
grid_raster <- raster(extent(pyre_shp), resolution = grid_resolution, vals = 0)

# Clip the grid to the region's outline
clipped_grid <- mask(grid_raster, pyre_shp)

plot(clipped_grid)

df_clipped <- as.data.frame(clipped_grid, xy = TRUE)
df_clipped <- subset(df_clipped, select = -layer)

prj <- "data/shape/Pyrenees.prj"
prj_content <- readLines(prj)
prj_crs <- st_crs(prj_content)

# Convert df_clipped to a spatial points object
coordinates(df_clipped) <- ~ x + y

# Convert df_clipped to a spatial data frame (sf object)
df_clipped_sf <- st_as_sf(df_clipped, coords = c("x", "y"))

# Set the coordinate reference system (CRS) for df_clipped_sf
st_crs(df_clipped_sf) <- prj_crs


x_coord <- c()
y_coord <- c()

for (k in 1:1206) {
x_coord[k] <- df_clipped$x[k]
y_coord[k] <- df_clipped$y[k]

coordinate_matrix <- cbind(x_coord, y_coord)
}

# Extract values at the given x and y coordinates
temp_value <- extract(bio1, df_clipped_sf)
prec_value <- extract(bio12, df_clipped_sf)
elev_value <- extract(elev, df_clipped_sf)

df_clipped_sf$temp_value <- temp_value$CHELSA_bio10_01
df_clipped_sf$prec_value <- prec_value$CHELSA_bio10_12
df_clipped_sf$elev <- elev_value$mx30_grd
 # some values are correctly assigned, but there are some NAs :/

# Mapping alpha per belt --------------------------------------------------------
 # Loop for plotting each belt (elevation) (this is not really what we want, but
 # it can be modified and utilised later)
for (i in seq_along(breaks)) {
  # Create a mask for the current increment
  mask <- values(elev) >= breaks[i] & values(elev) < breaks[i] + 100
  
  # Create a new raster with the masked values
  masked_raster <- raster(elev)
  masked_raster[!mask] <- NA
  
  # Plot the masked raster using base R plotting
  plot(masked_raster, col = colors[i], main = paste("Range:", breaks[i], "-", breaks[i] + 100))
}

# loop for making 0-99 -> "0", 100-199 -> "100", and so on.
# not necessarily needed, but just to make the process and codes simpler for plotting
elev_2 <- elev
elev_2$alpha <- NA

for (i in 0:34) {
  mask <- !is.na(elev_values) & (i*100 <= elev_values) & (elev_values < (i+1)*100)
  values(elev_2$mx30_grd)[mask] <- i*100
}

# loop for assigning alpha to each belt
for (i in 0:34) { # working but not doing right
  mask <- values(elev_2$mx30_grd) == i*100
  elev_2$alpha[elev_2$mx30_grd==mask] <- rep(df_belt.2$alpha[i], length(elev_2$alpha[mask]))
}

View(as.data.frame(elev_2)) # to check if the values are assigned correctly or not
# sadly at the moment, it seems that the last for loop didn't work so well as commented above :/
