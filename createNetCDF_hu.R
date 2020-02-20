# Instructions ------------------------------------------------------------

# IMPORTANT:
# Data formats can be .dat, .csv, .txt, .Rdata, .shp (requires rgdal)
# Please provide column names in datasets for every
# variable (variable_name) (including metadata)
# as specified in lists at https://automatix.muk.uni-hannover.de/net4/
# and other attributes as specified in UC2Datenstandard_1.2.pdf

# TODO:
# change metadata and attributes in the first sections to your needs!
# name latitude in your dataset "lat" and longitude "lon"
# name UTM easting in your dataset "E_UTM"
# name UTM northing in your dataset "N_UTM"
# name datetime column "date"



# 0 LIBRARIES
#------------------------------------------------------------------------------
# Dependencies NCO,
library(RNetCDF)
library(lubridate)
# library(rgdal)
library(sp)

# Set your working directory to the folder containing your data set,
# e.g. "D:/data/uc2" or use absolute paths for "input_data" and
setwd("~/scripts/uc2")
input_name <- "iop4_test.txt"
output_name <- "iop4_test" # without the ending .nc

# Not needed for .Rdata format:
skip_lines <- 0    # number of lines to skip after header line
header_line <- 1    # line number of header information
#  Separator of data in data set,
#  e.g. "\t" (for tabs), "," (for commas), ";" (for semicolons)
separator <- ","

#-------------------------------------------------------------------------------
# 1 Metadata -------------------------------------------------------------------
#-------------------------------------------------------------------------------

height_ag <- 1                # station height above ground/height of measurement device above ground
height_amsl <- 40.3           # surface height above mean sea level (only needed for station type)
measurementCount <- 1         # number of stations or trajectories
height <-  height_amsl + height_ag
origin_lon <- 13.53           # station longitude
origin_lat <- 52.42           # station latitude
origin_z <- 0                 # height of reference point from vrs (0 for every fetureType)
time_step <- 1                # timestep in data, will be written as such in NetCDF-file (in seconds)
origin_time <- "2018-07-17 06:00:00" # in UTC; only for validation runs
epsg_utm <- 25833             # UTM Zone of data (25833 for Berlin (UTM zone 33N)) (also acceptable 25832, 25831 (UTN zones 32N, 31N))
epsg_lonlat <- 4258           # either 4326 (datum wgs84) or 4258 (datum grs80)
fill_value <- -9999.          # should be -9999.

#-------------------------------------------------------------------------------
# Global Attribtues ------------------------------------------------------------
# change for every data set ----------------------------------------------------
#-------------------------------------------------------------------------------

acronym <- "HUBgeo"           # get from list in https://automatix.muk.uni-hannover.de/net4/ (Humboldt)
author <- "Schmidt, Benjamin, ben.schmidt@hu-berlin.de; Steger, David, david.steger@geo.hu-berlin.de" # script author
campaign <- "VALR02"             # get from list in https://automatix.muk.uni-hannover.de/net4/ (LTO = Long time observation, IOP.. = IOP number)
contact_person <- "Fritz, Sabine, sabine.fritz@hu-berlin.de" # contact person
data_content <- "airmeteo"       # get from list in https://automatix.muk.uni-hannover.de/net4/
feature_type <- "trajectory"   # get from list in https://automatix.muk.uni-hannover.de/net4/ (time series)
institution <- "Humboldt-Universität zu Berlin, Geographisches Institut"
keywords <- "Berlin, Ultra Fine Particles, Strasse des 17. Juni"
location <- "B"               # Berlin; get from list in https://automatix.muk.uni-hannover.de/net4/
site <- "tumathsouth"        # get from list in https://automatix.muk.uni-hannover.de/net4/ (Geography Institute, Adlershof)
source <- "Lufft WS600; GRIMM EDM 465 UFPC"               # GRIMM Messding
title <- "UFP_IOP1_tumathsouth" # short title for data set
version <- 1                # version number of the netCDF data set


#-------------------------------------------------------------------------------
# Global Attribtues ------------------------------------------------------------
# change only if necessary -----------------------------------------------------
#-------------------------------------------------------------------------------
# for information about global attributes, see UC2_Datenstandard_1.2 (or newer version): http://www.klima.tu-berlin.de/KB/index.php?KB=KB-UC2

history <- "Data processed and corrected with R Statistical Software"
comment <- "Ultrafine particles with particle sizes 7 - 2000 nm"
conventions <- "CF-1.7"
dependencies <- "no dependencies"
licence <- "[UC]2 Open Licence; see [UC]2 data policy available at www.uc2-program.org/uc2_data_policy.pdf"
references <- " " # Publications on data or methods (separate with ";", space is important)
rot_angle <- 0

# 2 Load data set
#------------------------------------------------------------------------------
# this function checks if data format is OK to continue and gives an error message if not.
input_split <- strsplit(input_name, ".", fixed = T)[[1]] # split input filename/ separate ending
read_table_formats <- list("txt", "csv", "dat")  # data formats readable by function read.table
if (input_split[length(input_split)] %in% read_table_formats) {
  # test if given data format is readable by read.table
  header <-
    read.table(
      input_name,
      sep = separator,
      header = T,
      skip = (header_line - 1),
      nrows = 1
    )
  data <-
    read.table(
      input_name,
      sep = separator,
      header = F,
      skip = (header_line + skip_lines)
    )
  names(data) <- names(header)
  remove(header)
} else if (input_split[length(input_split)] %in% "Rdata") {
  load_obj <- function(f) {
    env <- new.env()
    nm <- load(f, env, verbose = T)[1]
    env[[nm]]
  }
  data <- load_obj(input_name)
} else if (input_split[length(input_split)] %in% "shp") {
  data <- rgdal::readOGR(input_name)
} else {
  stop(
    "Data format not readable by this script. Optimize script or change data format to .txt, .csv, .dat, .shp or .Rdata"
  )
}

#-------------------------------------------------------------------------------
# VARIABLES --------------------------------------------------------------------
# change according to specific data set ----------------------------------------
#-------------------------------------------------------------------------------

# variable short names get from list at ***insert new homepage here***
# insert names from list at ***insert new homepage here***
data_names <- c("id", "date", "lon", "lat", "mcpm", "mcpm10", "mcpm2p5", "mcpm1")

# rename column names in data
names(data) <- data_names

# UTC Conversion
data$date <- format(ymd_hms(data$date, tz = "Europe/Berlin"), tz = "UTC")

variable_table <- read.table("variable.csv", header = T, sep = ",")  # read variable names table from local folder
tryCatch({
  # check for matching variables in data and names table
  info <- variable_table[match(names(data), variable_table$variable_name), ]
}, error = function(e) {
  stop("Error fetching the variable name from table variable.csv")
})
info <- info[!is.na(info[, 1]), ]                                      # get rid of non matching lines
long_name <- as.character(info$long_name)
standard_name <- as.character(info$standard_name)
variable_name <- as.character(info$variable_name)
units <- as.character(info$units)

# grid mapping (coordinate reference system)
grid_mapping <- rep("crs", length(standard_name))

# aggregation/integration method (dimension and method)
cell_methods <- rep("time: mean", length(standard_name))

# Calculate coordinates (rgdal and sp required) -----------------------
# conversion from lonlat WGS84 to UTM

epsg_utm <- as.character(epsg_utm)
epsg_lonlat <- as.character(epsg_lonlat)
e_epsg_longlat <- errorCondition("Illegal epsg code for lon/lat.\n Allowed: 4326 or 4258")
if (epsg_lonlat == "4326") {
  sp_ref_lonlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
} else if (epsg_lonlat == "4258") {
  sp_ref_lonlat <- "+proj=longlat +ellps=GRS80 +no_defs"
} else {
  stop(e_epsg_longlat)
}
spat_ref_utm <- paste0("+proj=utm +zone=", substr(epsg_utm, start = 4, stop = 5), " +ellps=GRS80 +units=m +no_defs")

if (!exists("E_UTM", data) & !exists("N_UTM", data)) {
  if (exists("origin_lon") & exists("origin_lat")) {
    origin_utm <- data.frame(lon = origin_lon, lat = origin_lat)           # station easting in meters (Coordinates)
    coordinates(origin_utm) <- c("lon", "lat")
    proj4string(origin_utm) <- CRS(sp_ref_lonlat)
    origin_utm <- as.data.frame(spTransform(origin_utm, CRS(spat_ref_utm)))
    names(origin_utm) <- c("x", "y")
    origin_x <- origin_utm[1, "x"]
    origin_y <- origin_utm[1, "y"]
  }
  if (exists("lon", data) & exists("lat", data)) {
    coords <- data[, c("lon", "lat")]
    coordinates(coords) <- c("lon", "lat")
    proj4string(coords) <- CRS(sp_ref_lonlat)
    utm_coords <- as.data.frame(spTransform(coords, CRS(spat_ref_utm)))
    data[,c("E_UTM", "N_UTM")] <- as.data.frame(spTransform(coords, CRS(spat_ref_utm)))
    rm(coords)
  }
} else if (!exists("lon", data) & !exists("lat", data)) {
  coords <- data[, c("E_UTM", "N_UTM")]
  coordinates(coords) <- c("E_UTM", "N_UTM")
  proj4string(coords) <- CRS(spat_ref_utm)
  geo_coords <- as.data.frame(spTransform(coords, CRS(sp_ref_lonlat)))
  names(geo_coords) <- c("lon", "lat")
  data <- cbind(data, geo_coords)
}

if (!exists("origin_lon") & !exists("origin_x")) {
  stop(
    "origin coordinates are missing! Please provide origin_lon and origin_lat or origin_x and origin_y."
  )
}

if (feature_type == "trajectory") {
  feature_type_dim <- "traj"
  typename_long <- "trajectory name"
  typename <- "traj_name"
  data$x <- data[, "E_UTM"] - origin_x            # distance to origin_x in m
  data$y <- data[, "N_UTM"] - origin_y            # distance to origin_y in m
  data$z <- height_amsl + height_ag - origin_z
} else if (feature_type == "station") {
  typename_long <- "station name"
  typename <- "station_name"
  x <- 0            # distance to origin_x in m
  y <- 0            # distance to origin_y in m
} else {
  stop("Unknown Measurement Type passed. Try trajectory or station!")
}

# Calculate time variables  ----------------------------------------------------
# start <- as.POSIXct(min(as.character(data$date)), tz = "UTC")
# end <- as.POSIXct(max(as.character(data$date)), tz = "UTC")
# start1 <- as.POSIXct(max(as.character(data[1, "date"])), tz = "UTC")
data$date <- ymd_hms(data$date, tz = "UTC")
if (campaign == "LTO") {
  origin_time <- floor_date(min(data$date), unit = "month")
} else if (grepl("VAL", campaign)) { # for validation time periods
  origin_time <- ymd_hms(origin_time, tz = "UTC")
} else {
  origin_time <- floor_date(min(data$date), unit = "day")
}

creation_time <- as.character(as.POSIXlt(Sys.time(), tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))

if (feature_type == "trajectory") {
  cf_role <- "trajectory_id"
  count <- c(length(data$time), 1)
  var_dim <- c("ntime", "traj_name")
} else if (feature_type == "station") {
  featureType <- "timeSeries"
  feature_type_dim <- "station"
  cf_role <- "timeseries_id"
  count <- c(length(data$time), 1)
  var_dim <- c("ntime", "station")
}


# handle time -------------------------------------------------------------

data$date <- as.POSIXct(data$date, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")   # to make sure POSIXct format of date column
data$time <- as.integer(seconds(interval(origin_time, data$date)))
data$time_bounds_l <- data$time - time_step
data$time_bounds_h <- data$time

#-------------------------------------------------------------------------------
# CREATE NetCDF-FILE  ----------------------------------------------------------
#-------------------------------------------------------------------------------

fn <- as.character(paste0(output_name, ".nc"))
nc <- create.nc(fn, clobber = T, format = "netcdf4")
missval.flag <- 0

# Dimensions  ------------------------------------------------------------------
dim.def.nc(nc, feature_type_dim, dimlength = measurementCount, unlim = F)        # Anz. Stationen
dim.def.nc(nc, "ntime", dimlength = length(data$time), unlim = F)                # Anz. Messzeiten
dim.def.nc(nc, "nv", dimlength = 2, unlim = F)                                   # Anz. Mess-Variablen
dim.def.nc(nc, "max_name_len", dimlength = 32, unlim = F)                        # Stationsnamenlaenge

# Time variables  --------------------------------------------------------------
var.def.nc(nc, "time", "NC_INT", c("ntime", feature_type_dim))
att.put.nc(nc, "time", "long_name", "NC_CHAR", "time")
att.put.nc(nc, "time", "standard_name", "NC_CHAR", "time")
att.put.nc(nc, "time", "units", "NC_CHAR", paste("seconds since", as.character(origin_time), "+00"))
att.put.nc(nc, "time", "calendar", "NC_CHAR", "proleptic_gregorian")
att.put.nc(nc, "time", "axis", "NC_CHAR", "T")
att.put.nc(nc, "time", "bounds", "NC_CHAR", "time_bounds")
var.put.nc(nc, "time", data$time, count = c(length(data$time), 1))

var.def.nc(nc, "time_bounds", "NC_INT", c("nv", "ntime", feature_type_dim))
var.put.nc(nc, "time_bounds", data$time_bounds_l, start = c(1, 1, 1), count = c(1, length(data$time), 1))
var.put.nc(nc, "time_bounds", data$time_bounds_h, start = c(2, 1, 1), count = c(1, length(data$time), 1))

# Data variables  --------------------------------------------------------------

for (i in 1:length(variable_name)) {
  name <- paste(variable_name[i])
  var.def.nc(nc, variable_name[i], "NC_FLOAT", c("ntime", feature_type_dim))
  att.put.nc(nc,
             variable_name[i],
             "long_name",
             "NC_CHAR",
             paste(long_name[i]))
  if (standard_name[i] != "") {
    att.put.nc(nc,
               variable_name[i],
               "standard_name",
               "NC_CHAR",
               standard_name[[i]])
  }
  att.put.nc(nc, variable_name[i], "units", "NC_CHAR", units[[i]])
  att.put.nc(nc, variable_name[i], "_FillValue", "NC_FLOAT", fill_value)
  att.put.nc(nc, variable_name[i], "coordinates", "NC_CHAR", paste("lon lat E_UTM N_UTM x y z time", typename)
  )
  att.put.nc(nc, variable_name[i], "grid_mapping", "NC_CHAR", grid_mapping[[i]])
  att.put.nc(nc, variable_name[i], "cell_methods", "NC_CHAR", cell_methods[[i]])
  var.put.nc(nc, variable_name[i], data[, variable_name[i]], count = c(length(data[, variable_name[i]]), 1))
}

# Reference System  ------------------------------------------------------------

var.def.nc(nc, "crs", "NC_INT", NA)
att.put.nc(nc, "crs", "long_name", "NC_CHAR", "coordinate reference system")
att.put.nc(nc, "crs", "grid_mapping_name", "NC_CHAR", "transverse_mercator")
att.put.nc(nc, "crs", "semi_major_axis", "NC_DOUBLE", 6378137.0)
att.put.nc(nc, "crs", "inverse_flattening", "NC_DOUBLE", 298.257222101)
att.put.nc(nc, "crs", "longitude_of_prime_meridian", "NC_DOUBLE", 0.0)
att.put.nc(nc, "crs", "longitude_of_central_meridian", "NC_DOUBLE", 15.0)
att.put.nc(nc, "crs", "scale_factor_at_central_meridian", "NC_DOUBLE", 0.9996)
att.put.nc(nc, "crs", "latitude_of_projection_origin", "NC_DOUBLE", 0.0)
att.put.nc(nc, "crs", "false_easting", "NC_DOUBLE", 500000.0)
att.put.nc(nc, "crs", "false_northing", "NC_DOUBLE", 0.0)
att.put.nc(nc, "crs", "units", "NC_CHAR", "m")
att.put.nc(nc, "crs", "epsg_code", "NC_CHAR", paste0("EPSG:", epsg_utm))

var.def.nc(nc, "vrs", "NC_INT", NA)
att.put.nc(nc, "vrs", "long_name", "NC_CHAR", "vertical reference system")
att.put.nc(nc, "vrs", "system_name", "NC_CHAR", "DHHN2016")

var.def.nc(nc, typename, "NC_CHAR", c("max_name_len", feature_type_dim))
att.put.nc(nc, typename, "long_name", "NC_CHAR", typename_long)
att.put.nc(nc, typename, "standard_name", "NC_CHAR", "platform_name")
att.put.nc(nc, typename, "cf_role", "NC_CHAR", cf_role)
var.put.nc(nc, typename, title, count = c(32, 1))

if (feature_type == "station") {
  var.def.nc(nc, "station_h", "NC_FLOAT", feature_type_dim)
  att.put.nc(nc, "station_h", "long_name", "NC_CHAR", "surface altitude")
  att.put.nc(nc, "station_h", "standard_name", "NC_CHAR", "surface_altitude")
  var.put.nc(nc, "station_h", height_amsl, count = 1)

  var.def.nc(nc, "height", "NC_FLOAT", feature_type_dim)
  att.put.nc(nc, "height", "long_name", "NC_CHAR", "height above surface")
  att.put.nc(nc, "height", "standard_name", "NC_CHAR", "height")
  att.put.nc(nc, "height", "units", "NC_CHAR", "m")
  att.put.nc(nc, "height", "_FillValue", "NC_FLOAT",fill_value)
  var.put.nc(nc, "height", height_ag, count = 1)
} else if (feature_type == "trajectory") {
  # if (length(unique(data$z[!is.na(data$z)])) == 1) {
  #   var.def.nc(nc, "height", "NC_DOUBLE", feature_type_dim)
  #   count <- 1
  # } else {
    var.def.nc(nc, "height", "NC_DOUBLE", c("ntime", feature_type_dim))
    count <- c(length(data$time), 1)
  # }
  var.def.nc(nc, "station_h", "NC_DOUBLE", feature_type_dim)
  att.put.nc(nc, "station_h", "long_name", "NC_CHAR", "surface altitude")
  att.put.nc(nc,
             "station_h",
             "standard_name",
             "NC_CHAR",
             "surface_altitude")
  var.put.nc(nc, "station_h", height_amsl, count = 1)

  att.put.nc(nc, "height", "long_name", "NC_CHAR", "height above surface")
  att.put.nc(nc, "height", "standard_name", "NC_CHAR", "height")
  att.put.nc(nc, "height", "units", "NC_CHAR", "m")
  # att.put.nc(nc, "height", "_FillValue", "NC_FLOAT",fill_value)
  var.put.nc(nc, "height", data$z, count = count)
}
if (feature_type != "trajectory") {
  var.def.nc(nc, "lat", "NC_DOUBLE", feature_type_dim)
  var.def.nc(nc, "lon", "NC_DOUBLE", feature_type_dim)
  var.def.nc(nc, "x", "NC_DOUBLE", feature_type_dim)
  var.def.nc(nc, "y", "NC_DOUBLE", feature_type_dim)
  var.def.nc(nc, "z", "NC_DOUBLE", feature_type_dim)
  var.def.nc(nc, "N_UTM", "NC_DOUBLE", feature_type_dim)
  var.def.nc(nc, "E_UTM", "NC_DOUBLE", feature_type_dim)
} else if (feature_type == "trajectory") {
  var.def.nc(nc, "lat", "NC_DOUBLE", c("ntime", feature_type_dim))
  var.def.nc(nc, "lon", "NC_DOUBLE", c("ntime", feature_type_dim))
  var.def.nc(nc, "x", "NC_DOUBLE", c("ntime", feature_type_dim))
  var.def.nc(nc, "y", "NC_DOUBLE", c("ntime", feature_type_dim))
  var.def.nc(nc, "z", "NC_DOUBLE", c("ntime", feature_type_dim))
  var.def.nc(nc, "N_UTM", "NC_DOUBLE", c("ntime", feature_type_dim))
  var.def.nc(nc, "E_UTM", "NC_DOUBLE", c("ntime", feature_type_dim))
}

att.put.nc(nc, "lat", "long_name", "NC_CHAR", "latitude")
att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
att.put.nc(nc, "lat", "_FillValue", "NC_DOUBLE",fill_value)

att.put.nc(nc, "lon", "long_name", "NC_CHAR", "longitude")
att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
att.put.nc(nc, "lon", "_FillValue", "NC_DOUBLE",fill_value)

att.put.nc(nc, "x", "long_name", "NC_CHAR", "distance to origin in x-direction")
att.put.nc(nc, "x", "units", "NC_CHAR", "m")
att.put.nc(nc, "x", "axis", "NC_CHAR", "X")
att.put.nc(nc, "x", "_FillValue", "NC_DOUBLE",fill_value)

att.put.nc(nc,
           "y",
           "long_name",
           "NC_CHAR",
           "distance to origin in y-direction")
att.put.nc(nc, "y", "units", "NC_CHAR", "m")
att.put.nc(nc, "y", "axis", "NC_CHAR", "Y")
att.put.nc(nc, "y", "_FillValue", "NC_DOUBLE",fill_value)

att.put.nc(nc, "z", "long_name", "NC_CHAR", "height above origin")
att.put.nc(nc,
           "z",
           "standard_name",
           "NC_CHAR",
           "height_above_mean_sea_level")
att.put.nc(nc, "z", "units", "NC_CHAR", "m")
att.put.nc(nc, "z", "axis", "NC_CHAR", "Z")
att.put.nc(nc, "z", "positive", "NC_CHAR", "up")
att.put.nc(nc, "z", "_FillValue", "NC_DOUBLE", fill_value)

att.put.nc(nc, "N_UTM", "long_name", "NC_CHAR", "northing")
att.put.nc(nc,
           "N_UTM",
           "standard_name",
           "NC_CHAR",
           "projection_y_coordinate")
att.put.nc(nc, "N_UTM", "units", "NC_CHAR", "m")
att.put.nc(nc, "N_UTM", "_FillValue", "NC_DOUBLE", fill_value)

att.put.nc(nc, "E_UTM", "long_name", "NC_CHAR", "easting")
att.put.nc(nc,
           "E_UTM",
           "standard_name",
           "NC_CHAR",
           "projection_x_coordinate")
att.put.nc(nc, "E_UTM", "units", "NC_CHAR", "m")
att.put.nc(nc, "E_UTM", "_FillValue", "NC_DOUBLE", fill_value)

if (feature_type == "station") {
  var.put.nc(nc, "x", x, count = 1)
  var.put.nc(nc, "y", y, count = 1)
  var.put.nc(nc, "z", height_amsl + height_ag, count = 1)
  var.put.nc(nc, "lat", origin_lat, count = 1)
  var.put.nc(nc, "lon", origin_lon, count = 1)
  var.put.nc(nc, "N_UTM", origin_y, count = 1)
  var.put.nc(nc, "E_UTM", origin_x, count = 1)
} else if (feature_type == "trajectory") {
  var.put.nc(nc, "x", data$x, count = c(length(data$time), 1))
  var.put.nc(nc, "y", data$y, count = c(length(data$time), 1))
  var.put.nc(nc, "z", data$z, count = c(length(data$time), 1))
  var.put.nc(nc, "lat", data$lat, count = c(length(data$time), 1))
  var.put.nc(nc, "lon", data$lon, count = c(length(data$time), 1))
  var.put.nc(nc, "N_UTM", data$N_UTM, count = c(length(data$time), 1))
  var.put.nc(nc, "E_UTM", data$E_UTM, count = c(length(data$time), 1))
}


# Global Attributes  -----------------------------------------------------------

att.put.nc(nc, "NC_GLOBAL", "Conventions", "NC_CHAR", conventions)
att.put.nc(nc, "NC_GLOBAL", "licence", "NC_CHAR", licence)

att.put.nc(nc, "NC_GLOBAL", "campaign", "NC_CHAR", campaign)
att.put.nc(nc, "NC_GLOBAL", "location", "NC_CHAR", location)
att.put.nc(nc, "NC_GLOBAL", "source", "NC_CHAR", source)
att.put.nc(nc, "NC_GLOBAL", "featureType", "NC_CHAR", feature_type)

att.put.nc(nc, "NC_GLOBAL", "institution", "NC_CHAR", institution)
att.put.nc(nc, "NC_GLOBAL", "acronym", "NC_CHAR", acronym)
att.put.nc(nc, "NC_GLOBAL", "author", "NC_CHAR", author)
att.put.nc(nc, "NC_GLOBAL", "contact_person", "NC_CHAR", contact_person)

att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", history)
att.put.nc(nc, "NC_GLOBAL", "comment", "NC_CHAR", comment)

att.put.nc(nc, "NC_GLOBAL", "rotation_angle", "NC_FLOAT", rot_angle)

att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", title)
att.put.nc(nc, "NC_GLOBAL", "keywords", "NC_CHAR", keywords)

att.put.nc(nc, "NC_GLOBAL", "site", "NC_CHAR", site)
att.put.nc(nc, "NC_GLOBAL", "data_content", "NC_CHAR", data_content)
att.put.nc(nc, "NC_GLOBAL", "origin_time", "NC_CHAR", paste(as.character(origin_time), "+00"))

att.put.nc(nc, "NC_GLOBAL", "dependencies", "NC_CHAR", dependencies)
att.put.nc(nc, "NC_GLOBAL", "references", "NC_CHAR", references)

att.put.nc(nc, "NC_GLOBAL", "origin_lon", "NC_DOUBLE", origin_lon)
att.put.nc(nc, "NC_GLOBAL", "origin_lat", "NC_DOUBLE", origin_lat)

att.put.nc(nc, "NC_GLOBAL", "origin_x", "NC_DOUBLE", origin_x)
att.put.nc(nc, "NC_GLOBAL", "origin_y", "NC_DOUBLE", origin_y)
att.put.nc(nc, "NC_GLOBAL", "origin_z", "NC_DOUBLE", origin_z)

att.put.nc(nc,
           "NC_GLOBAL",
           "creation_time",
           "NC_CHAR",
           paste(creation_time, '+00'))
att.put.nc(nc, "NC_GLOBAL", "version", "NC_INT", version)

#-------------------------------------------------------------------------------
close.nc(nc)
#-------------------------------------------------------------------------------
