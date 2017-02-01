


# chunk 1
options(max.print = 1000, scipen = 999, width = 90)
library(RevoScaleR)
rxOptions(reportProgress = 1) # reduces the amount of output RevoScaleR produces
library(tidyverse)
library(lubridate)
library(stringr)
options(dplyr.print_max = 2000)
options(dplyr.width = Inf) # shows all columns of a tbl_df object
library(rgeos) # spatial package
library(maptools) # spatial package
library(ggmap)
library(gridExtra) # for putting plots side by side
library(ggrepel) # avoid text overlap in plots
library(seriation) # package for reordering a distance matrix




# chunk 2
col_classes <- c(
  'pickup_datetime'       = "character",
  'dropoff_datetime'      = "character",
  'passenger_count'       = "integer",
  'trip_distance'         = "numeric",
  'pickup_longitude'      = "numeric",
  'pickup_latitude'       = "numeric",
  'rate_code_id'          = "factor",
  'dropoff_longitude'     = "numeric",
  'dropoff_latitude'      = "numeric",
  'payment_type'          = "factor",
  'fare_amount'           = "numeric",
  'extra'                 = "numeric",
  'mta_tax'               = "numeric",
  'tip_amount'            = "numeric",
  'tolls_amount'          = "numeric",
  'improvement_surcharge' = "numeric",
  'total_amount'          = "numeric")




# chunk 3
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
# we take a chunk of the data and load it as a data.frame (good for testing things)
nyc_sample <- read.csv(input_csv, nrows = 1000, colClasses = col_classes)
head(nyc_sample)




# chunk 4
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016.xdf')
library(lubridate)
most_recent_date <- ymd("2016-07-01") # the day of the months is irrelevant

# because we keep appending to the same file, we can't run this in parallel
st <- Sys.time()
for(ii in 1:6) { # get each month's data and append it to the first month's data
  file_date <- most_recent_date - months(ii)
  input_csv <- sprintf('yellow_tripsample_%s.csv', substr(file_date, 1, 7))
  input_csv <- file.path(data_dir, input_csv)
  append <- if (ii == 1) "none" else "rows"
  rxImport(input_csv, input_xdf, colClasses = col_classes, overwrite = TRUE, 
    append = append)
  print(input_csv)
}
Sys.time() - st # stores the time it took to import




# chunk 5
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016.xdf')
nyc_xdf <- RxXdfData(input_xdf)
system.time(rxsum_xdf <- rxSummary( ~ fare_amount, nyc_xdf))
rxsum_xdf




# chunk 6
# we can only use one month's data unless we join the CSVs
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
# point to CSV file and provide column info
nyc_csv <- RxTextData(input_csv, colClasses = col_classes)
system.time(rxsum_csv <- rxSummary( ~ fare_amount, nyc_csv))
rxsum_csv




# chunk 7
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016-01.xdf')
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')

st <- Sys.time()
## convert CSV to XDF here
jan_2016_xdf <- RxXdfData(input_xdf)
## summarize XDF file here
rt_xdf <- Sys.time() - st

st <- Sys.time()
jan_2016_csv <- RxTextData(input_csv, colClasses = col_classes)
## summarize CSV file here
rt_csv <- Sys.time() - st

file.remove(input_xdf) # remove the file to keep folder clean

## compare runtimes rt_xdf and rt_csv
## compare results sum_xdf and sum_csv




# chunk 8
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016-01.xdf')
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')

st <- Sys.time()
rxImport(input_csv, input_xdf, colClasses = col_classes, overwrite = TRUE)
jan_2016_xdf <- RxXdfData(input_xdf)
sum_xdf <- rxSummary( ~ ., jan_2016_xdf)
rt_xdf <- Sys.time() - st # runtime for XDF file

file.remove(input_xdf) # remove the file to keep folder clean




# chunk 9
st <- Sys.time()
jan_2016_csv <- RxTextData(input_csv, colClasses = col_classes)
sum_csv <- rxSummary( ~ ., jan_2016_csv)
rt_csv <- Sys.time() - st # runtime for CSV file




# chunk 10
rt_xdf - rt_csv




# chunk 11
sum_xdf$categorical[[2]]
sum_csv$categorical[[2]]




# chunk 12
sum_xdf$sDataFrame[5, ]
sum_csv$sDataFrame[5, ]




# chunk 13
rxGetInfo(nyc_xdf, getVarInfo = TRUE, numRows = 5)




# chunk 14
rxDataStep(nyc_xdf, nyc_xdf,
  transforms = list(
    tip_percent = ifelse(fare_amount > 0 & tip_amount < fare_amount, 
                         round(tip_amount * 100 / fare_amount, 0), 
                         NA)),
  overwrite = TRUE)
rxSummary( ~ tip_percent, nyc_xdf)




# chunk 15
rxSummary( ~ tip_percent2, nyc_xdf,
           transforms = list(
            tip_percent2 = ifelse(fare_amount > 0 & tip_amount < fare_amount, 
                                  round(tip_amount * 100 / fare_amount, 0), 
                                  NA)))




# chunk 16
rxCrossTabs( ~ month:year, nyc_xdf,
             transforms = list(
               year = as.integer(substr(pickup_datetime, 1, 4)),
               month = as.integer(substr(pickup_datetime, 6, 7)),
               year = factor(year, levels = 2014:2016),
               month = factor(month, levels = 1:12)))




# chunk 17
rxCrossTabs( ~ month:year, nyc_xdf,
             transforms = list(
               date = ymd_hms(pickup_datetime),
               year = factor(year(date), levels = 2014:2016),
               month = factor(month(date), levels = 1:12)),
             transformPackages = "lubridate")




# chunk 18
# transformation function for extracting some date and time features
xforms <- function(data) {
  
  wlabels <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
  cut_levels <- c(1, 5, 9, 12, 16, 18, 22)
  hour_labels <- c('1AM-5AM', '5AM-9AM', '9AM-12PM', '12PM-4PM', 
                   '4PM-6PM', '6PM-10PM', '10PM-1AM')
  
  pickup_datetime <- ymd_hms(data$pickup_datetime, tz = "UTC")
  pickup_hour <- addNA(cut(hour(pickup_datetime), cut_levels))
  pickup_dow <- factor(wday(pickup_datetime), levels = 1:7, labels = wlabels)
  levels(pickup_hour) <- hour_labels
  
  dropoff_datetime <- ymd_hms(data$dropoff_datetime, tz = "UTC")
  dropoff_hour <- addNA(cut(hour(dropoff_datetime), cut_levels))
  dropoff_dow <- factor(wday(dropoff_datetime), levels = 1:7, labels = wlabels)
  levels(dropoff_hour) <- hour_labels
  
  data$pickup_hour <- pickup_hour
  data$pickup_dow <- pickup_dow
  data$dropoff_hour <- dropoff_hour
  data$dropoff_dow <- dropoff_dow
  data$trip_duration <- as.integer(as.duration(dropoff_datetime - pickup_datetime))
  
  data
}




# chunk 19
library(lubridate)
Sys.setenv(TZ = "US/Eastern") # not important for this dataset
head(xforms(nyc_sample)) # test the function on a data.frame




# chunk 20
head(rxDataStep(nyc_sample, transformFunc = xforms, transformPackages = "lubridate"))




# chunk 21
st <- Sys.time()
rxDataStep(nyc_xdf, nyc_xdf, overwrite = TRUE, transformFunc = xforms, 
           transformPackages = "lubridate")
Sys.time() - st




# chunk 22
rxs1 <- rxSummary( ~ pickup_hour + pickup_dow + trip_duration, nyc_xdf)
# we can add a column for proportions next to the counts
rxs1$categorical <- lapply(rxs1$categorical, 
  function(x) cbind(x, prop = round(prop.table(x$Counts), 2)))
rxs1




# chunk 23
rxs2 <- rxSummary( ~ pickup_dow:pickup_hour, nyc_xdf)
rxs2 <- tidyr::spread(rxs2$categorical[[1]], key = 'pickup_hour', value = 'Counts')
row.names(rxs2) <- rxs2[ , 1]
rxs2 <- as.matrix(rxs2[ , -1])
rxs2




# chunk 24
levelplot(prop.table(rxs2, 2), cuts = 4, xlab = "", ylab = "", 
          main = "Distribution of taxis by day of week")




# chunk 25
library(rgeos)
library(maptools)

nyc_shapefile <- readShapePoly('ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp')
library(stringr)
mht_shapefile <- subset(nyc_shapefile, str_detect(CITY, 'New York City-Manhattan'))

mht_shapefile@data$id <- as.character(mht_shapefile@data$NAME)
library(ggplot2)
mht.points <- fortify(gBuffer(mht_shapefile, byid = TRUE, width = 0), region = "NAME")
library(dplyr)
mht.df <- inner_join(mht.points, mht_shapefile@data, by = "id")

library(dplyr)
  mht.cent <- mht.df %>%
  group_by(id) %>%
  summarize(long = median(long), lat = median(lat))

library(ggrepel)
  ggplot(mht.df, aes(long, lat, fill = id)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme(legend.position = "none") +
  geom_text_repel(aes(label = id), data = mht.cent, size = 3)




# chunk 26
# take only the coordinate columns, and replace NAs with 0
data_coords <- data.frame(
  long = ifelse(is.na(nyc_sample$pickup_longitude), 0, nyc_sample$pickup_longitude),
  lat = ifelse(is.na(nyc_sample$pickup_latitude), 0, nyc_sample$pickup_latitude)
  )
# we specify the columns that correspond to the coordinates
coordinates(data_coords) <- c('long', 'lat')
# returns the neighborhoods based on coordinates
nhoods <- over(data_coords, nyc_shapefile)
# rename the column names in nhoods
names(nhoods) <- paste('pickup', tolower(names(nhoods)), sep = '_')
# combine the neighborhood information with the original data
nyc_sample <- cbind(nyc_sample, nhoods)
head(nyc_sample)




# chunk 27
find_nhoods <- function(data) {
  # extract pick-up lat and long and find their neighborhoods
  # add only the pick-up neighborhood and city columns to the data
  # extract drop-off lat and long and find their neighborhoods
  # add only the drop-off neighborhood and city columns to the data
  # return the data with the new columns added in
}




# chunk 28
# test the function on a data.frame using rxDataStep
head(rxDataStep(nyc_sample, 
                transformFunc = find_nhoods, 
                transformPackages = c("sp", "maptools"), 
                transformObjects = list(shapefile = mht_shapefile)))




# chunk 29
rxDataStep(nyc_xdf, nyc_xdf,
  transforms = list(
    rate_code_id = as.factor(rate_code_id),
    rate_code_id = factor(rate_code_id, 
                          levels = 1:6, 
                          labels = c('standard', 'JFK', 'Newark', 'Nassau or Westchester', 
                                     'negotiated', 'group ride')),
    payment_type = factor(payment_type, levels = 1:2, labels = c('card', 'cash'))
  ),
  overwrite = TRUE)




# chunk 30
nhoods <- over(data_coords, mht_shapefile)
str(nhoods)




# chunk 31
find_nhoods <- function(data) {
  # extract pick-up lat and long and find their neighborhoods
  pickup_longitude <- ifelse(is.na(data$pickup_longitude), 0, data$pickup_longitude)
  pickup_latitude <- ifelse(is.na(data$pickup_latitude), 0, data$pickup_latitude)
  data_coords <- data.frame(long = pickup_longitude, lat = pickup_latitude)
  coordinates(data_coords) <- c('long', 'lat')
  nhoods <- over(data_coords, shapefile)
  # add only the pick-up neighborhood and city columns to the data
  data$pickup_nhood <- nhoods$NAME
  data$pickup_borough <- nhoods$CITY
  # extract drop-off lat and long and find their neighborhoods
  dropoff_longitude <- ifelse(is.na(data$dropoff_longitude), 0, data$dropoff_longitude)
  dropoff_latitude <- ifelse(is.na(data$dropoff_latitude), 0, data$dropoff_latitude)
  data_coords <- data.frame(long = dropoff_longitude, lat = dropoff_latitude)
  coordinates(data_coords) <- c('long', 'lat')
  nhoods <- over(data_coords, shapefile)
  # add only the drop-off neighborhood and city columns to the data
  data$dropoff_nhood <- nhoods$NAME
  data$dropoff_borough <- nhoods$CITY
  # return the data with the new columns added in
  data
}




# chunk 32
# test the function on a data.frame using rxDataStep
head(rxDataStep(nyc_sample, transformFunc = find_nhoods, 
                transformPackages = c("sp", "maptools"), 
                transformObjects = list(shapefile = nyc_shapefile)))




# chunk 33
st <- Sys.time()
rxDataStep(nyc_xdf, nyc_xdf, overwrite = TRUE, 
           transformFunc = find_nhoods, 
           transformPackages = c("sp", "maptools", "rgeos"), 
           transformObjects = list(shapefile = nyc_shapefile))

Sys.time() - st
rxGetInfo(nyc_xdf, numRows = 5)




# chunk 34
system.time(rxs_all <- rxSummary( ~ ., nyc_xdf) )




# chunk 35
head(rxs_all$sDataFrame)




# chunk 36
nhoods_by_borough <- rxCrossTabs( ~ pickup_nhood:pickup_borough, nyc_xdf)
nhoods_by_borough <- nhoods_by_borough$counts[[1]]
nhoods_by_borough <- as.data.frame(nhoods_by_borough)

# get the neighborhoods by borough
lnbs <- lapply(names(nhoods_by_borough), 
               function(vv) subset(nhoods_by_borough, nhoods_by_borough[ , vv] > 0, 
                                   select = vv, drop = FALSE))
lapply(lnbs, head)




# chunk 37
manhattan_nhoods <- as.character(mht_shapefile@data$NAME)

refactor_columns <- function(data) {
  data$pickup_nb = factor(data$pickup_nhood, levels = nhoods_levels)
  data$dropoff_nb = factor(data$dropoff_nhood, levels = nhoods_levels)
  data
}

rxDataStep(nyc_xdf, nyc_xdf, overwrite = TRUE, 
           transformFunc = refactor_columns, 
           transformObjects = list(nhoods_levels = manhattan_nhoods))

rxs_pickdrop <- rxSummary( ~ pickup_nb:dropoff_nb, nyc_xdf)
head(rxs_pickdrop$categorical[[1]])




# chunk 38
rxHistogram( ~ trip_distance, nyc_xdf, startVal = 0, endVal = 25, 
            histType = "Percent", numBreaks = 20)




# chunk 39
rxs <- rxSummary( ~ pickup_nhood:dropoff_nhood, nyc_xdf, 
                 rowSelection = (trip_distance > 15 & trip_distance < 22))
library(dplyr)
head(arrange(rxs$categorical[[1]], desc(Counts)), 10)




# chunk 40
# outFile argument missing means we output to data.frame
odd_trips <- rxDataStep(nyc_xdf, 
  rowSelection = (u < .05 & ( # we can adjust this if the data gets too big
                  (trip_distance > 20 | trip_distance <= 0) |
                  (passenger_count > 5 | passenger_count == 0) |
                  (fare_amount > 1000 | fare_amount <= 0))), 
  transforms = list(u = runif(.rxNumRows)))

print(dim(odd_trips))




# chunk 41
library(ggplot2)
odd_trips %>%
filter(trip_distance > 20) %>%
ggplot() -> p

p + geom_histogram(aes(x = fare_amount, fill = trip_duration <= 10*60), binwidth = 10) +
  xlim(0, 500) + 
  coord_fixed(ratio = .5)




# chunk 42
rxHistogram( ~ trip_distance, nyc_xdf, startVal = 0, endVal = 25, 
            histType = "Percent", numBreaks = 20)




# chunk 43
cut(8.9, breaks = c(-Inf, 0, 5, 10, Inf), labels = c("0", "<5", "5-10", "10+"))




# chunk 44
rxHistogram( ~ trip_distance | pickup_hour + payment_type, nyc_xdf, startVal = 0, 
            endVal = 25, histType = "Percent", numBreaks = 20)




# chunk 45
rxHistogram( ~ trip_dist | pickup_hour + payment_type, nyc_xdf, 
            histType = "Percent", 
            transforms = list(trip_dist = cut(trip_distance, 
                                              breaks = c(-Inf, 0, 5, 10, Inf), 
                                              labels = c("0", "<5", "5-10", "10+"))))




# chunk 46
input_xdf <- file.path(data_dir, 'yellow_tripdata_2016_clean.xdf')
mht_xdf <- RxXdfData(input_xdf)

rxDataStep(nyc_xdf, mht_xdf,
  rowSelection = (passenger_count > 0 &
                  trip_distance >= 0 & trip_distance < 30 &
                  trip_duration > 0 & trip_duration < 60*60*24 &
                  str_detect(pickup_borough, 'Manhattan') &
                  str_detect(dropoff_borough, 'Manhattan') &
                  !is.na(pickup_nb) &
                  !is.na(dropoff_nb) &
                  fare_amount > 0),
  transformPackages = "stringr",
  varsToDrop = c('extra', 'mta_tax', 'improvement_surcharge', 'total_amount', 
                 'pickup_borough', 'dropoff_borough', 'pickup_nhood', 'dropoff_nhood'),
  overwrite = TRUE)




# chunk 47
mht_sample <- rxDataStep(mht_xdf, rowSelection = (u < .01), transforms = list(u = runif(.rxNumRows)))

dim(mht_sample)




# chunk 48
library(ggmap)
map_13 <- get_map(location = c(lon = -73.98, lat = 40.76), zoom = 13)
map_14 <- get_map(location = c(lon = -73.98, lat = 40.76), zoom = 14)
map_15 <- get_map(location = c(lon = -73.98, lat = 40.76), zoom = 15)

q1 <- ggmap(map_14) +
  geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), data = mht_sample, 
             alpha = 0.15, na.rm = TRUE, col = "red", size = .5) +
  theme_nothing(legend = TRUE)

q2 <- ggmap(map_15) +
  geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), data = mht_sample, 
             alpha = 0.15, na.rm = TRUE, col = "red", size = .5) +
  theme_nothing(legend = TRUE)

library(gridExtra)
grid.arrange(q1, q2, ncol = 2)




# chunk 49
library(dplyr)
xydata <- transmute(mht_sample, 
                    long_std = dropoff_longitude / -74, 
                    lat_std = dropoff_latitude / 40)

start_time <- Sys.time()
rxkm_sample <- kmeans(xydata, centers = 300, iter.max = 2000, nstart = 50)
Sys.time() - start_time

# we need to put the centroids back into the original scale for coordinates
centroids_sample <- rxkm_sample$centers %>%
  as.data.frame %>%
  transmute(long = long_std*(-74), lat = lat_std*40, size = rxkm_sample$size)

head(centroids_sample)




# chunk 50
start_time <- Sys.time()
rxkm <- rxKmeans( ~ long_std + lat_std, data = mht_xdf, outFile = mht_xdf, 
                 outColName = "dropoff_cluster", centers = rxkm_sample$centers, 
                 transforms = list(long_std = dropoff_longitude / -74, 
                                   lat_std = dropoff_latitude / 40), 
                 blocksPerRead = 1, overwrite = TRUE, maxIterations = 100, 
                 reportProgress = -1)
Sys.time() - start_time

clsdf <- cbind(
transmute(as.data.frame(rxkm$centers), long = long_std*(-74), lat = lat_std*40),
size = rxkm$size, withinss = rxkm$withinss)

head(clsdf)




# chunk 51
centroids_whole <- cbind(transmute(as.data.frame(rxkm$centers), 
                                   long = long_std*(-74), lat = lat_std*40), 
                         size = rxkm$size, 
                         withinss = rxkm$withinss)

q1 <- ggmap(map_15) +
  geom_point(data = centroids_sample, aes(x = long, y = lat, alpha = size), 
             na.rm = TRUE, size = 3, col = 'red') +
  theme_nothing(legend = TRUE) +
  labs(title = "centroids using sample data")

q2 <- ggmap(map_15) +
  geom_point(data = centroids_whole, aes(x = long, y = lat, alpha = size), 
             na.rm = TRUE, size = 3, col = 'red') +
  theme_nothing(legend = TRUE) +
  labs(title = "centroids using whole data")

library(gridExtra)
grid.arrange(q1, q2, ncol = 2)




# chunk 52
nclus <- 50
kmeans_nclus <- kmeans(xydata, centers = nclus, iter.max = 2000, nstart = 1)
sum(kmeans_nclus$withinss)




# chunk 53
nclus_seq <- seq(20, 1000, by = 50)




# chunk 54
find_wss <- function(nclus, ...) {
st <- Sys.time()
res <- sum(kmeans(centers = nclus, ...)$withinss)
print(sprintf("nclus = %d, runtime = %3.2f seconds", nclus, Sys.time() - st))
res
}

find_wss(nclus = 10, x = xydata, iter.max = 500, nstart = 1)




# chunk 55
wss <- sapply(nclus_seq, find_wss, x = xydata, iter.max = 500, nstart = 1)

library(ggplot2)
ggplot(aes(x = x, y = y), data = data.frame(x = nclus_seq, y = wss)) +
  geom_line() +
  xlab("number of clusters") +
  ylab("within clusters sum of squares")




# chunk 56
rxct <- rxCrossTabs(trip_distance ~ pickup_nb:dropoff_nb, mht_xdf)
res <- rxct$sums$trip_distance / rxct$counts$trip_distance

library(seriation)
res[which(is.nan(res))] <- mean(res, na.rm = TRUE)
nb_order <- seriate(res)




# chunk 57
rxc1 <- rxCube(trip_distance ~ pickup_nb:dropoff_nb, mht_xdf)
rxc2 <- rxCube(minutes_per_mile ~ pickup_nb:dropoff_nb, mht_xdf, 
               transforms = list(minutes_per_mile = (trip_duration/60)/trip_distance))
rxc3 <- rxCube(tip_percent ~ pickup_nb:dropoff_nb, mht_xdf)
library(dplyr)
res <- bind_cols(list(rxc1, rxc2, rxc3))
res <- res[ , c('pickup_nb', 'dropoff_nb', 'trip_distance', 'minutes_per_mile', 'tip_percent')]
head(res)




# chunk 58
library(ggplot2)
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = trip_distance), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)




# chunk 59
newlevs <- levels(res$pickup_nb)[unlist(nb_order)]
res$pickup_nb <- factor(res$pickup_nb, levels = unique(newlevs))
res$dropoff_nb <- factor(res$dropoff_nb, levels = unique(newlevs))

ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = trip_distance), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)




# chunk 60
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = minutes_per_mile), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)




# chunk 61
res %>%
  mutate(tip_color = cut(tip_percent, c(0, 8, 12, 15, 100))) %>%
  ggplot(aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = tip_color)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_fixed(ratio = .9)




# chunk 62
# first way of reordering the factor levels
rxDataStep(inData = mht_xdf, outFile = mht_xdf, 
  transforms = list(
    pickup_nb = factor(pickup_nb, levels = newlevels), 
    dropoff_nb = factor(dropoff_nb, levels = newlevels)), 
  transformObjects = list(newlevels = unique(newlevs)), overwrite = TRUE)




# chunk 63
# second way of reordering the factor levels
rxFactors(mht_xdf, outFile = mht_xdf, 
  factorInfo = list(
    pickup_nb = list(newLevels = unique(newlevs)), 
    dropoff_nb = list(newLevels = unique(newlevs))), 
  overwrite = TRUE)




# chunk 64
rxc <- rxCube( ~ pickup_nb:dropoff_nb, mht_xdf)
rxc <- as.data.frame(rxc)

rxc %>%
  filter(Counts > 0) %>%
  mutate(pct_all = Counts/sum(Counts) * 100) %>%
  group_by(pickup_nb) %>%
  mutate(pct_by_pickup_nb = Counts/sum(Counts) * 100) %>%
  group_by(dropoff_nb) %>%
  mutate(pct_by_dropoff_nb = Counts/sum(Counts) * 100) %>%
  group_by() %>%
  arrange(desc(Counts)) -> rxcs

head(rxcs)




# chunk 65
ggplot(rxcs, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = pct_all), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "black") +
  coord_fixed(ratio = .9)




# chunk 66
ggplot(rxcs, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = pct_by_pickup_nb), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)




# chunk 67
ggplot(rxcs, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = pct_by_dropoff_nb), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = .9)




# chunk 68
rxcs %>%
  select(pickup_nb, dropoff_nb, pct = pct_by_pickup_nb) %>%
  arrange(pickup_nb, desc(pct))




# chunk 69
nb_name <- "West Village" # a neighborhood of our choosing
nb_drop <- ## pull the most common destinations for this neighborhood from `rxcs_tops`

pickup_df <- rxDataStep(mht_xdf, # we leave out outFile and store results in pickup_df
  rowSelection = ## select the relevant subset of the data
  varsToKeep = c("dropoff_nb", "pickup_datetime"),
  transformObjects = ## a list, used to pass `nb_name` and `nb_drop` to rowSelection
  )




# chunk 70
library(lubridate)
pickup_df %>%
  mutate(pickup_hour = hour(ymd_hms(pickup_datetime, tz = "UTC"))) %>%
  ggplot(aes(x = pickup_hour, fill = dropoff_nb)) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))




# chunk 71
rxcs %>%
  select(pickup_nb, dropoff_nb, pct = pct_by_pickup_nb) %>%
  arrange(pickup_nb, desc(pct)) %>%
  group_by(pickup_nb) %>%
  mutate(cumpct = cumsum(pct)) %>%
  filter(pct > 5 & (cumpct <= 50 | (cumpct > 50 & lag(cumpct) <= 50))) %>%
  as.data.frame -> rxcs_tops




# chunk 72
nb_name <- "West Village"
nb_drop <- subset(rxcs_tops, pickup_nb == nb_name, select = "dropoff_nb", drop = TRUE)

pickup_df <- rxDataStep(mht_xdf,
  rowSelection = pickup_nb == nb & dropoff_nb %in% top_drop_for_nb,
  varsToKeep = c("dropoff_nb", "pickup_datetime"),
  transformObjects = list(nb = nb_name, top_drop_for_nb = nb_drop))




# chunk 73
library(scales)
library(lubridate)
pickup_df %>%
  mutate(pickup_hour = hour(ymd_hms(pickup_datetime, tz = "UTC"))) %>%
  ggplot(aes(x = pickup_hour, fill = dropoff_nb)) +
  geom_bar(position = "fill", stat = "count") +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  ylab("percent")




# chunk 74
res1 <- rxCube(tip_percent ~ pickup_dow:pickup_hour, mht_xdf)
res2 <- rxCube(fare_amount/(trip_duration/60) ~ pickup_dow:pickup_hour, mht_xdf)
names(res2)[3] <- 'fare_per_minute'
res <- bind_cols(list(res1, res2))
res <- res[ , c('pickup_dow', 'pickup_hour', 'fare_per_minute', 'tip_percent', 'Counts')]

ggplot(res, aes(pickup_dow, pickup_hour)) +
  geom_tile(aes(fill = fare_per_minute), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = sprintf('%dK riders\n (%d%% tip)', 
                                signif(Counts/1000, 2), round(tip_percent, 0))), 
            size = 2.5) +
  coord_fixed(ratio = .9)




# chunk 75
form_1 <- as.formula(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour)
rxlm_1 <- rxLinMod(form_1, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)




# chunk 76
rxs <- rxSummary( ~ pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
ll <- lapply(rxs$categorical, function(x) x[ , 1])
names(ll) <- c('pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow')
pred_df_1 <- expand.grid(ll)
pred_df_1 <- rxPredict(rxlm_1, data = pred_df_1, computeStdErrors = TRUE, writeModelVars = TRUE)
names(pred_df_1)[1:2] <- paste(c('tip_pred', 'tip_stderr'), 1, sep = "_")
head(pred_df_1, 10)




# chunk 77
library(ggplot2)
ggplot(pred_df_1, aes(x = pickup_nb, y = dropoff_nb)) +
  geom_tile(aes(fill = tip_pred_1), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = .9)




# chunk 78
ggplot(pred_df_1, aes(x = pickup_dow, y = pickup_hour)) +
  geom_tile(aes(fill = tip_pred_1), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = .9)




# chunk 79
form_2 <- as.formula(tip_percent ~ pickup_nb:dropoff_nb)
rxlm_2 <- rxLinMod(form_2, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)
pred_df_2 <- rxPredict(rxlm_2, data = pred_df_1, computeStdErrors = TRUE, writeModelVars = TRUE)
names(pred_df_2)[1:2] <- paste(c('tip_pred', 'tip_stderr'), 2, sep = "_")

library(dplyr)
pred_df <- pred_df_2 %>%
  select(starts_with('tip_')) %>%
  cbind(pred_df_1) %>%
  arrange(pickup_nb, dropoff_nb, pickup_dow, pickup_hour) %>%
  select(pickup_dow, pickup_hour, pickup_nb, dropoff_nb, starts_with('tip_pred_'))

head(pred_df)




# chunk 80
ggplot(data = pred_df) +
  geom_density(aes(x = tip_pred_1, col = "complex")) +
  geom_density(aes(x = tip_pred_2, col = "simple")) +
  facet_grid(pickup_hour ~ pickup_dow)




# chunk 81
dfq <- data.frame(probs = seq(0, 1, by = .05))
dfq$tip_percent <- rxQuantile("tip_percent", data = mht_xdf, probs = dfq$probs)

ggplot(aes(x = tip_percent, y = probs), data = dfq) +
  geom_line()




# chunk 82
pred_df %>%
  mutate_each(funs(cut(., c(-Inf, 8, 12, 15, 18, Inf))), tip_pred_1, tip_pred_2) %>%
  ggplot() +
  geom_bar(aes(x = tip_pred_1, fill = "complex"), alpha = .5) +
  geom_bar(aes(x = tip_pred_2, fill = "simple"), alpha = .5) +
  facet_grid(pickup_hour ~ pickup_dow) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# chunk 83
form_3 <- ## formula described above goes here
rxlm_3 <- ## build a linear model based on the above formula




# chunk 84
rxs <- rxSummary( ~ payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
ll <- lapply(rxs$categorical, function(x) x[ , 1])
names(ll) <- c('payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow')
pred_df <- expand.grid(ll)




# chunk 85
ggplot(data = pred_all) +
  geom_bar(aes(x = p1, fill = "model 1", group = payment_type, alpha = .5)) +
  geom_bar(aes(x = p3, fill = "model 3", group = payment_type, alpha = .5)) +
  facet_grid(pickup_hour ~ pickup_dow) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# chunk 86
form_3 <- as.formula(tip_percent ~ payment_type + pickup_nb:dropoff_nb + pickup_dow:pickup_hour)
rxlm_3 <- rxLinMod(form_3, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)




# chunk 87
rxs <- rxSummary( ~ payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
ll <- lapply(rxs$categorical, function(x) x[ , 1])
names(ll) <- c('payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow')
pred_df <- expand.grid(ll)

pred_df_1 <- rxPredict(rxlm_1, data = pred_df, computeStdErrors = TRUE, writeModelVars = TRUE)
pred_df_3 <- rxPredict(rxlm_3, data = pred_df, computeStdErrors = TRUE, writeModelVars = TRUE)

pred_df %>%
  cbind(select(rename(pred_df_1, p1 = tip_percent_Pred), p1)) %>%
  cbind(select(rename(pred_df_3, p3 = tip_percent_Pred), p3)) %>%
  mutate_at(vars(p1, p3), funs(cut(., c(-Inf, 8, 12, 15, 18, Inf)))) -> pred_all




# chunk 88
ggplot(data = pred_all) +
  geom_bar(aes(x = p1, fill = "model 1", alpha = .5)) +
  geom_bar(aes(x = p3, fill = "model 3", alpha = .5)) +
  facet_grid(~ payment_type) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# chunk 89
library(scales)
pred_df %>%
  cbind(select(rename(pred_df_1, p1 = tip_percent_Pred), p1)) %>%
  cbind(select(rename(pred_df_3, p3 = tip_percent_Pred), p3)) %>%
  mutate_at(vars(p1, p3), funs(rescale(., to = c(0, 20)))) %>%
  mutate_at(vars(p1, p3), funs(cut(., c(-Inf, 8, 12, 15, 18, Inf)))) -> pred_all

ggplot(data = pred_all) +
  geom_bar(aes(x = p1, fill = "model 1"), alpha = .5) +
  geom_bar(aes(x = p3, fill = "model 3"), alpha = .5) +
  facet_grid(~ payment_type) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# chunk 90
dir.create(file.path(data_dir, 'output'), showWarnings = FALSE)
rx_split_xdf <- function(xdf = mht_xdf, split_perc = 0.75, output_path = "output", ...) {
  # first create a column to split by
  outFile <- tempfile(fileext = 'xdf')
  rxDataStep(inData = xdf, outFile = xdf, transforms = list(
    split = factor(ifelse(rbinom(.rxNumRows, size = 1, prob = splitperc), "train", "test"))),
    transformObjects = list(splitperc = split_perc), overwrite = TRUE, ...)

  # then split the data in two based on the column we just created
  splitDS <- rxSplit(inData = xdf,
  outFilesBase = file.path(output_path, "train"),
  splitByFactor = "split",
  overwrite = TRUE)
  return(splitDS)
}

# we can now split to data in two
mht_split <- rx_split_xdf(xdf = mht_xdf, 
  varsToKeep = c('payment_type', 'fare_amount', 'tip_amount', 'tip_percent', 
                 'pickup_hour', 'pickup_dow', 'pickup_nb', 'dropoff_nb'))
names(mht_split) <- c("train", "test")




# chunk 91
system.time(linmod <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour, data = mht_split$train, reportProgress = 0))
system.time(dtree <- rxDTree(tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + pickup_hour, data = mht_split$train, pruneCp = "auto", reportProgress = 0))
system.time(dforest <- rxDForest(tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + pickup_hour, data = mht_split$train, nTree = 30, importance = TRUE, useSparseCube = TRUE, reportProgress = 0))




# chunk 92
trained.models <- list(linmod = linmod, dtree = dtree, dforest = dforest)
save(trained.models, file = 'trained_models.Rdata')




# chunk 93
pred_df <- expand.grid(ll[2:5])
pred_df_1 <- rxPredict(trained.models$linmod, data = pred_df, predVarNames = "pred_linmod")
pred_df_2 <- rxPredict(trained.models$dtree, data = pred_df, predVarNames = "pred_dtree")
pred_df_3 <- rxPredict(trained.models$dforest, data = pred_df, predVarNames = "pred_dforest")
pred_df <- do.call(cbind, list(pred_df, pred_df_1, pred_df_2, pred_df_3))

observed_df <- rxSummary(tip_percent ~ pickup_nb:dropoff_nb:pickup_dow:pickup_hour, mht_xdf)
observed_df <- observed_df$categorical[[1]][ , c(2:6)]
pred_df <- inner_join(pred_df, observed_df, by = names(pred_df)[1:4])

ggplot(data = pred_df) +
  geom_density(aes(x = Means, col = "observed average"), size = 1) +
  geom_density(aes(x = pred_linmod, col = "linmod"), size = 1) +
  geom_density(aes(x = pred_dtree, col = "dtree"), size = 1) +
  geom_density(aes(x = pred_dforest, col = "dforest"), size = 1) +
  xlim(-1, 30) +
  xlab("tip percent")




# chunk 94
rxPredict(trained.models$linmod, data = mht_split$test, outData = mht_split$test, predVarNames = "tip_percent_pred_linmod", overwrite = TRUE)
rxPredict(trained.models$dtree, data = mht_split$test, outData = mht_split$test, predVarNames = "tip_percent_pred_dtree", overwrite = TRUE)
rxPredict(trained.models$dforest, data = mht_split$test, outData = mht_split$test, predVarNames = "tip_percent_pred_dforest", overwrite = TRUE)

rxSummary(~ SSE_linmod + SSE_dtree + SSE_dforest, data = mht_split$test,
  transforms = list(
    SSE_linmod = (tip_percent - tip_percent_pred_linmod)^2,
    SSE_dtree = (tip_percent - tip_percent_pred_dtree)^2,
    SSE_dforest = (tip_percent - tip_percent_pred_dforest)^2))




# chunk 95
rxc <- rxCor( ~ tip_percent + tip_percent_pred_linmod + tip_percent_pred_dtree + tip_percent_pred_dforest, data = mht_split$test)
print(rxc)




# chunk 96
# we re-build the linear model without payment_type for comparison
linmod_1 <- rxLinMod(## formula goes here
                     data = mht_split$train, reportProgress = 0)
# we build a linear model with payment_type
linmod_2 <- rxLinMod(## formula goes here
                     data = mht_split$train, reportProgress = 0)
# we build a decision tree with payment_type
dtree <- rxDTree(## formula goes here
                 data = mht_split$train, pruneCp = "auto", reportProgress = 0)

trained.models <- list(linmod_1 = linmod_1, linmod_2 = linmod_2, dtree = dtree)




# chunk 97
pred_df <- expand.grid(ll)
## predict payment_type on pred_df by each of three models
# pred_df_1 stores predictions made by linmod_1 into a column called pred_linmod_1
# pred_df_2 stores predictions made by linmod_2 into a column called pred_linmod_2
# pred_df_3 stores predictions made by dtree into a column called pred_dtree
pred_df <- bind_cols(pred_df, pred_df_1, pred_df_2, pred_df_3)
head(pred_df)




# chunk 98
ggplot(data = pred_df) +
  geom_density(aes(x = pred_linmod_1, col = "linmod_1"), size = 1) +
  geom_density(aes(x = pred_linmod_2, col = "linmod_2"), size = 1) +
  geom_density(aes(x = pred_dtree, col = "dtree"), size = 1) +
  xlim(-10, 40) +
  xlab("tip percent") +
  facet_grid(payment_type ~ ., scales = "free")




# chunk 99
test_df <- rxDataStep(mht_split$test, 
  varsToKeep = c('tip_percent', 'payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow'), 
  maxRowsByCols = 10^9)
test_df_1 <- ## predict for the first model, call the predictions `tip_pred_linmod`
test_df_2 <- ## predict for the second model, call the predictions `tip_pred_dtree`
test_df_3 <- ## predict for the third model, call the predictions `tip_pred_dforest`




# chunk 100
test_df <- do.call(cbind, list(test_df, test_df_1, test_df_2, test_df_3))

rxSummary(~ SSE_linmod + SSE_dtree + SSE_dforest, data = test_df,
  transforms = list(
    SSE_linmod = (tip_percent - tip_pred_linmod)^2,
    SSE_dtree = (tip_percent - tip_pred_dtree)^2,
    SSE_dforest = (tip_percent - tip_pred_dforest)^2))




# chunk 101
linmod_1 <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour, data = mht_split$train, reportProgress = 0)
linmod_2 <- rxLinMod(tip_percent ~ payment_type + pickup_nb:dropoff_nb + pickup_dow:pickup_hour, data = mht_split$train, reportProgress = 0)
dtree <- rxDTree(tip_percent ~ payment_type + pickup_nb + dropoff_nb + pickup_dow + pickup_hour, data = mht_split$train, pruneCp = "auto", reportProgress = 0)

trained.models <- list(linmod_1 = linmod_1, linmod_2 = linmod_2, dtree = dtree)




# chunk 102
pred_df <- expand.grid(ll)
pred_df_1 <- rxPredict(trained.models$linmod_1, data = pred_df, predVarNames = "pred_linmod_1")
pred_df_2 <- rxPredict(trained.models$linmod_2, data = pred_df, predVarNames = "pred_linmod_2")
pred_df_3 <- rxPredict(trained.models$dtree, data = pred_df, predVarNames = "pred_dtree")
pred_df <- bind_cols(pred_df, pred_df_1, pred_df_2, pred_df_3)
head(pred_df)




# chunk 103
ggplot(data = pred_df) +
  geom_density(aes(x = pred_linmod_1, col = "linmod_1"), size = 1) +
  geom_density(aes(x = pred_linmod_2, col = "linmod_2"), size = 1) +
  geom_density(aes(x = pred_dtree, col = "dtree"), size = 1) +
  xlim(-10, 40) +
  xlab("tip percent") +
  facet_grid(payment_type ~ ., scales = "free")




# chunk 104
test_df <- rxDataStep(mht_split$test, varsToKeep = c('tip_percent', 'payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow'), maxRowsByCols = 10^9)
test_df_1 <- rxPredict(trained.models$linmod_1, data = test_df, predVarNames = "tip_pred_linmod_1")
test_df_2 <- rxPredict(trained.models$linmod_2, data = test_df, predVarNames = "tip_pred_linmod_2")
test_df_3 <- rxPredict(trained.models$dtree, data = test_df, predVarNames = "tip_pred_dtree")
test_df <- do.call(cbind, list(test_df, test_df_1, test_df_2, test_df_3))
head(test_df)




# chunk 105
ggplot(data = test_df) +
  geom_density(aes(x = tip_pred_linmod_1, col = "linmod w/o payment type")) +
  geom_density(aes(x = tip_pred_linmod_2, col = "linmod w/ payment type")) +
  geom_density(aes(x = tip_pred_dtree, col = "dtree with payment type")) +
  xlab("tip percent") # + facet_grid(pickup_hour ~ pickup_dow)




# chunk 106
rxSummary(~ SSE_linmod_1 + SSE_linmod_2 + SSE_dtree, data = test_df,
  transforms = list(
    SSE_linmod_1 = (tip_percent - tip_pred_linmod_1)^2,
    SSE_linmod_2 = (tip_percent - tip_pred_linmod_2)^2,
    SSE_dtree = (tip_percent - tip_pred_dtree)^2))




# chunk 107
rxc <- rxCor( ~ tip_percent + tip_pred_linmod_1 + tip_pred_linmod_2 + tip_pred_dtree, data = test_df)
print(rxc)




# chunk 108
input_xdf <- file.path(data_dir, 'yellow_tripdata_2016_clean.xdf')
nyc_xdf <- RxXdfData(input_xdf)

sqlConnString <- sprintf("Driver=SQL Server;Server=%s;Database=RDB;Uid=ruser;Pwd=ruser", SQLSERVERNAME)
sqlRowsPerRead <- 100000
sqlTable <- "NYCTaxiSmall"

nyc_sql <- RxSqlServerData(connectionString = sqlConnString, rowsPerRead = sqlRowsPerRead, table = sqlTable)




# chunk 109
system.time(
  rxDataStep(nyc_xdf, nyc_sql, overwrite = TRUE, varsToDrop = c("long_std", "lat_std"))
)




# chunk 110
rate_levels <- c("standard", "JFK", "Newark", "Nassau or Westchester", "negotiated", "group ride")

ccColInfo <- list(
pickup_datetime    = list(type = "character"),
dropoff_datetime   = list(type = "character"),
passenger_count    = list(type = "integer"),
trip_distance      = list(type = "numeric"),
pickup_longitude   = list(type = "numeric"),
pickup_latitude    = list(type = "numeric"),
dropoff_longitude  = list(type = "numeric"),
dropoff_latitude   = list(type = "numeric"),
rate_code_id       = list(type = "factor", levels = as.character(1:6), newLevels = rate_levels),
store_and_fwd_flag = list(type = "factor", levels = c("Y", "N")),
payment_type       = list(type = "factor", levels = as.character(1:2), newLevels = c("card", "cash")),
fare_amount        = list(type = "numeric"),
tip_amount         = list(type = "numeric"),
total_amount       = list(type = "numeric")
)




# chunk 111
weekday_labels <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
hour_labels <- c('1AM-5AM', '5AM-9AM', '9AM-12PM', '12PM-4PM', '4PM-6PM', '6PM-10PM', '10PM-1AM')

ccColInfo$pickup_dow <- list(type = "factor", levels = weekday_labels)
ccColInfo$pickup_hour <- list(type = "factor", levels = hour_labels)
ccColInfo$dropoff_dow <- list(type = "factor", levels = weekday_labels)
ccColInfo$dropoff_hour <- list(type = "factor", levels = hour_labels)




# chunk 112
library(maptools)
nyc_shapefile <- readShapePoly('ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp')
library(stringr)
mht_shapefile <- subset(nyc_shapefile, str_detect(CITY, 'New York City-Manhattan'))
manhattan_nhoods <- as.character(mht_shapefile@data$NAME)

ccColInfo$pickup_nb <- list(type = "factor", levels = manhattan_nhoods)
ccColInfo$dropoff_nb <- list(type = "factor", levels = manhattan_nhoods)




# chunk 113
nyc_sql <- RxSqlServerData(connectionString = sqlConnString, table = sqlTable, rowsPerRead = sqlRowsPerRead, colInfo = ccColInfo)




# chunk 114
rxGetInfo(nyc_sql, getVarInfo = TRUE, numRows = 3)




# chunk 115
system.time(
  rxsum_sql <- rxSummary( ~ fare_amount, nyc_sql)
)




# chunk 116
# Set ComputeContext. Needs a temp directory path to serialize R objects back and forth
sqlShareDir <- paste("C:/AllShare/", Sys.getenv("USERNAME"), sep = "")
sqlWait <- TRUE
sqlConsoleOutput <- TRUE
sqlCC <- RxInSqlServer(connectionString = sqlConnString, shareDir = sqlShareDir,
wait = sqlWait, consoleOutput = sqlConsoleOutput)




# chunk 117
rxSetComputeContext(sqlCC)
system.time(
  rxsum_sql <- rxSummary( ~ fare_amount, nyc_sql)
)




# chunk 118
system.time(linmod <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour,
  data = nyc_sql, reportProgress = 0, rowSelection = (split == "train")))




# chunk 119
sqlTable <- "NYCTaxiScore"
nyc_score <- RxSqlServerData(connectionString = sqlConnString, rowsPerRead = sqlRowsPerRead, table = sqlTable)

rxPredict(linmod, data = nyc_sql, outData = nyc_score, predVarNames = "tip_percent_pred_linmod", overwrite = TRUE)




# chunk 120
nyc_score <- nyc_sql
nyc_score@table <- "NYCTaxiScore"




# chunk 121
rxPredict(linmod, data = nyc_sql, outData = nyc_score, predVarNames = "tip_percent_pred_linmod", overwrite = TRUE, extraVarsToWrite = c("pickup_datetime", "dropoff_datetime"))




# chunk 122
summary(linmod)
modelbin <- serialize(linmod, NULL)
modelbinstr <- paste(modelbin, collapse = "")

library(RODBC)
odbcCloseAll()
conn <- odbcDriverConnect(sqlConnString)
q <- paste("EXEC PersistModel @m='", modelbinstr,"'", sep = "")
sqlQuery(conn, q)




# chunk 123
input <- "N' SELECT top 1000 * FROM NYCTaxiSmall'"
q <- paste("EXEC PredictTipBatchMode @inquery = ", input, sep = "")
scoredData <- sqlQuery(conn, q)
head(scoredData)




# chunk 124
qt <- data.frame(percentile = seq(1, 99, by = 1))
num_vars <- c('fare_amount', 'tip_percent')
qt[ , num_vars] <- lapply(num_vars, function(var) rxQuantile(var, nyc_sql, probs = qt$percentile / 100))
library(ggplot2)
q1 <- ggplot(aes(x = percentile, y = fare_amount), data = qt) + geom_line()
q2 <- ggplot(aes(x = percentile, y = tip_percent), data = qt) + geom_line()

library(gridExtra)
grid.arrange(q1, q2, ncol = 2)




# chunk 125
nyc_sample <- rxDataStep(nyc_sql, rowSelection = (u < .01), transforms = list(u = runif(.rxNumRows)))

library(ggplot2)
ggplot(data = nyc_sample, aes(x = log(trip_distance), y = log(trip_duration))) + 
  geom_point()




# chunk 126
nyc_sample_sql <- nyc_sql
nyc_sample_sql@table <- NULL
nyc_sample_sql@sqlQuery <- 'select * from RDB.dbo.NYCTaxiSmall tablesample (1 percent)'
nyc_sample <- rxImport(nyc_sample_sql)

library(ggplot2)
ggplot(data = nyc_sample, aes(x = log(trip_distance), y = log(trip_duration))) + 
  geom_point()




# chunk 127
scatterPlot <- function(inDataSource) {
  ds <- rxImport(inDataSource)
  require(ggplot2)
  pl <- ggplot(data = ds, aes(x = log(trip_distance), y = log(trip_duration)))
  pl <- pl + geom_point()
  return(list(myplot = pl))
}

scatterPlot(nyc_sample_sql) # this works, but it's not in-database




# chunk 128
rxSetComputeContext(sqlCC)
myplots <- rxExec(scatterPlot, nyc_sample_sql, timesToRun = 1, packagesToLoad = 'ggplot2')
plot(myplots[[1]][["myplot"]]) # only the plot object is returned to us for display

