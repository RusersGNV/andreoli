
install.packages("sp")
install.packages("dismo")
install.packages("rgdal")
install.packages("dplyr")
install.packages("raster")
install.packages("openxlsx")


library(sp)
library(dismo)
library(rgdal)
library(dplyr)
library(raster)
library(openxlsx)


######################################################################################################################
##Native and Non-native Range Presence Records
#####################################################################################################################

### First let's read the USGS-NAS query already in CSV form. 
NAS_Data<-read.csv("2016-12-05_Joe_Andreoli_select_species.csv", stringsAsFactors = FALSE)
head(NAS_Data)
### Begin subsetting the data to remove failed introductions, extirpated introductions 
### (defined by USGS-NAS as a population dying on its own without human intervention)   
unique(NAS_Data$Status)
rows.drop <- c("failed","extirpated")
### note the exclamation point which means to keep Statuses Not in Rows.Drop
NAS_Data<-subset(NAS_Data, !Status%in%rows.drop)
unique(NAS_Data$Status)
### Moving on let's now look at the unique values of lat_long source
unique(NAS_Data$lat_long_source)
### Remove records where the lat_long source is NOT reported.So map-derived, GNIS, and 
### Calculated by GIS, US Census Bureau, and Calculated by GIS since most are 
### centorids of HUCs or states
rows.drop <- c("Map derived","Calculated by GIS", "GNIS", "US Census Bureau")
NAS_Data<-subset(NAS_Data, !lat_long_source%in%rows.drop)
NAS_Data
### Remove records where the lat_long_accuracy is approximate, approximate is defined 
### as a poor description of locality, or generalized description. Centroid is also 
### removed since it is the centroid of a county or state
unique(NAS_Data$lat_long_accuracy)
### Only keep accurate
rows.drop<-("Approximate")
NAS_Data<-subset(NAS_Data, !lat_long_accuracy%in%rows.drop)
NAS_Data
str(NAS_Data)

NAS_Data<- subset(NAS_Data, select=c("common_name", "GISLatDD", "GISLongDD"))
colnames(NAS_Data)<-c("Species", "Latitude", "Longitude")
str(NAS_Data)

### Now make this csv a spatial point dataframe for use in Maxent
coordinates(NAS_Data)<-c("Longitude", "Latitude")
### Setting coordinates cannot be done on Spatial Objects where they have already been set, so if you get order wrong
### Rerun read.csv and flip the order of the coordinates line
spplot(NAS_Data, "Species")
### NAS_USGS is now a spatial points dataframe, so you can get a master csv of species, lat and long
class (NAS_Data)
plot(NAS_Data)

### Vector 04: Convert from .csv to shapefile in R
### Convert into lat and long from Northing and Easting. It should be in UTM 17N (Florida)
Gandy_Data<-read.xlsx("gandy data for joe.xlsx")
write.csv(Gandy_Data, "Gandy_Data.csv")
Gandy_Data<-read.csv("Gandy_Data.csv", stringsAsFactors = FALSE)
str(Gandy_Data)
Gandy_Data<-subset(Gandy_Data, select=c ("Species.presence", "UTM.easting", "UTM.northing"))
colnames(Gandy_Data)<-c("Species", "Easting", "Northing")
head(Gandy_Data)
str(Gandy_Data)
names(Gandy_Data)
unique(Gandy_Data$Species)
head(Gandy_Data$Easting)
head(Gandy_Data$Northing)
Gandy_Data[,2:3]
### We have coordinate values in our data.frame but in order to convert data.frame to a spatial points data frame, we
### need a CRS associated with these Northings and Eastings. 
Gandy_Data_crs<-CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
Gandy_Data<-SpatialPointsDataFrame(Gandy_Data[,2:3], Gandy_Data, proj4string = Gandy_Data_crs)
Gandy_Data<-spTransform(Gandy_Data, CRS("+proj=longlat"))
latlong<-coordinates(Gandy_Data)
latlong<-as.data.frame(latlong)
Species<-Gandy_Data$Species
Gandy_Data<-cbind(Species, latlong)
colnames(Gandy_Data)<-c("Species", "Longitude", "Latitude")
coordinates(Gandy_Data)<-c("Longitude", "Latitude")
spplot(Gandy_Data, "Species")

### Two columns already exist, rename scientific to common names change names of columns so that they are consistent 
### with the NAS csv file column names 

### Will need to run an additional filter for GBIF data, noticing some points that
### are clearly of non-established species. Use lower lethal limits?
GBIF_Data<-read.xlsx("concatonated gbif data for joe.xlsx")
write.csv(GBIF_Data, "GBIF_Data.csv")
GBIF_Data<-read.csv("GBIF_Data.csv", stringsAsFactors = FALSE)
colnames(GBIF_Data)<-c("Number", "Species", "Latitude", "Longitude", "Year")
GBIF_Data
head(GBIF_Data)
unique(GBIF_Data$Species)
GBIF_Data$Species[GBIF_Data$Species == "Astronotus ocellatus"] <- "Oscar"
GBIF_Data$Species[GBIF_Data$Species == "Cichlasoma bimaculatum"] <- "Black Acara"
GBIF_Data$Species[GBIF_Data$Species == "Sarotherodon melanotheron"] <- "Blackchin Tilapia"
GBIF_Data$Species[GBIF_Data$Species == "Tilapia mariae"] <- "Spotted Tilapia"
GBIF_Data$Species[GBIF_Data$Species == "Clarias batrachus (Linnaeus"] <- "Walking Catfish"
GBIF_Data$Species[GBIF_Data$Species == "Oreochromis aureus "] <- "Blue Tilapia"
GBIF_Data$Species[GBIF_Data$Species == "Monopterus albus"] <- "Asian Swamp Eel"
GBIF_Data$Species[GBIF_Data$Species == "Belonesox belizanus"] <- "Pike Killifish"
GBIF_Data$Species[GBIF_Data$Species == "Cichla ocellaris Bloch & Schneider"] <- "Butterfly Peacock Bass"
GBIF_Data$Species[GBIF_Data$Species == "Cichlasoma urophthalmus"] <- "Mayan Cichlid"
GBIF_Data$Species[GBIF_Data$Species == "Hemichromis letourneuxi"] <- "African Jewelfish"
GBIF_Data$Species[GBIF_Data$Species == "Hoplosternum littorale"] <- "Brown Hoplo"

unique(GBIF_Data$Species)
GBIF_Data<- subset(GBIF_Data, select=c("Species", "Latitude", "Longitude"))
GBIF_Data
coordinates(GBIF_Data)<-c("Longitude", "Latitude")
### Setting coordinates cannot be done on Spatial Objects where they have already been set, so if you get order wrong
### Rerun read.csv and flip the order of the coordinates line
spplot(GBIF_Data, "Species")

### We have no manipulated three sources of presence records. Now let's bring them 
### together into a csv

nnfish<-rbind(NAS_Data, GBIF_Data, Gandy_Data)
spplot(nnfish)
nnfish
### withhold 25% of presence records to later use as an external measure of omission 
### (false negative rate)
fold<-kfold(nnfish, k=4)
nnfish_test<-nnfish[fold==1,]
spplot(nnfish_test)
nnfish_test
write.csv(nnfish_test, "NNFish_Testing.csv")
nnfish_test<-read.csv("NNFish_Testing.csv", stringsAsFactors = FALSE)
nnfish_test<-subset(nnfish_test, select=c("Species", "Latitude", "Longitude"))
### This is our testing data set to use for calculating omission rate
write.csv(nnfish_test, "NNFish_Testing.csv")

nnfish_train<-nnfish[fold!=1,]
spplot(nnfish_train)
nnfish_train
write.csv(nnfish_train, "NNFish_Training.csv")
nnfish_train<-read.csv("NNFish_Training.csv", stringsAsFactors = FALSE)
nnfish_train<-subset(nnfish_train, select=c("Species", "Latitude", "Longitude"))
### This is our training dataset used to build the model, and used for internal 
### testing using Area Under the Curve (AUC)
write.csv(nnfish_train, "NNFish_Training.csv")

################################################################################################
###Environmental Layers
################################################################################################
### Create directory to store raster data later
dir.create("florida_data")

### Bring in the shapefile of the polygon you want to crop raster data with
### Give your layers meaningful names for when you present in front of a group
FLA <- readOGR(dsn = "C:/Users/andreoli5978690/Desktop/Andreoli_R_Meetup/florida_boundary", layer = "idk")
projection(FLA) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

### Stack the worldclim ascii files you already have. 
### bioclim_current is now a rasterstack 
setwd("C:/Users/andreoli5978690/Desktop/Andreoli_R_Meetup/Current")
bioclim_current<-stack(list.files(pattern="\\.asc$", full.names=TRUE))

## Crop bioclim data by extent of state subset (the state of Florida)
## Crop returns a geographic subset of an object as specified by an Extent 
## object (or object from which an extent object can be extracted/created)
florida.crop <- crop(bioclim_current, FLA, snap="out")

## We now identify those pixels of our bioclim rasters that lie within 
## the boundaries of Florida.
florida.raster<-rasterize(FLA, florida.crop)

###Create a new Raster* object that has the same values as x
florida_bioclim <- mask(x=florida.crop, mask=florida.raster)

plot(florida_bioclim)
## Looks good, now let's save them as GeoTIFFS

## Save as SpatialPixelsDataFrame because the above rasterlayer object can't
## be written as a GeoTIFF  


## Convert the Environmental Layers to ASCII Format using R

## GDAL grid map written, no data values written in once we changed the default
## from mvFlag=NA to mvFlag=-9999. From http://www.gdal.org/frmt_various.html
## AAIGrid is  the ASCII interchange format for Arc/Info Grid, and takes the 
## form of an ASCII file, and associated .prj files.

setwd("C:/Users/andreoli5978690/Desktop/Andreoli_R_Meetup/florida_data")
for (i in 1:19){
  plot(florida_bioclim[[i]])
  dev.copy(tiff, paste('bioclim', i, '.tif', sep=''))
  dev.off()
  sp <- as(bioclim_current[[i]], "SpatialPixelsDataFrame")
  writeGDAL(sp, paste('bioclim', i, '.asc', sep=''), drivername="AAIGrid", mvFlag=-9999)
}

