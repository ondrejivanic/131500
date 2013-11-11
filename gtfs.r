library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)
library(plyr)
library(hexbin)
library(doMC)
library(reshape2)
library(RColorBrewer)

gtfs.load <- function(path) {

  WGS84toUTMzone56 <- function (latlon) {
    coordinates(latlon) <- ~ stop_lon + stop_lat
    proj4string(latlon) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    utm <- as.data.frame(spTransform(latlon, CRS("+proj=utm +zone=56 ellps=WGS84")))
    names(utm) <- c("lon", "lat")
    utm
  }
  
  read.gtfs.file <- function (name, ...) {
    read.csv(file.path(path, name), na.strings = c("", "NA"))
  }

  list(
    "agency" = read.gtfs.file("agency.txt"),
    "calendar" = transform(
      read.gtfs.file("calendar.txt"),
      start_date = ymd(start_date),
      end_date = ymd(end_date)
    ),
    "calendar_dates" = transform(
      read.gtfs.file("calendar_dates.txt"),
      date =  ymd(date)
    ),
    "routes" = transform(
      read.gtfs.file("routes.txt"),
      route_type = factor(
        route_type, 
        levels = 0:7, 
        labels = c("Tram", "Metro", "Train", "Bus", "Ferry", "Cable car", "Gondola", "Funicular")
      )
    ),
    "shapes" = read.gtfs.file("shapes.txt"),
    "stop_times" = transform(
      read.gtfs.file("stop_times.txt"),
      stop_sequence = factor(stop_sequence),
      pickup_type = factor(pickup_type),
      drop_off_type = factor(drop_off_type)
    ),
    "stops" = transform(
      read.gtfs.file("stops.txt"),
      utm = WGS84toUTMzone56(data.frame(stop_lat, stop_lon))
    ),
    "trips" = read.gtfs.file("trips.txt")
  )  
}

gtfs.bbox <- function(gtfs, type = c("bbox", "convex")) {
  type <- match.arg(type)

  points <- gtfs[["stops"]][, c("stop_lat", "stop_lon")]
  coordinates(points) <- ~ stop_lon + stop_lat # x, then y
  proj4string(points) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # assing coordinate reference system
  box <- gConvexHull(points) # str() is your friend
  
  if(type == "bbox") {
    box@bbox      
  } else {
    data.frame(box@polygons[[1]]@Polygons[[1]]@coords)
  }
}

gtfs.trips <- function(gtfs, date = Sys.time()) {
  d <- trunc(date, "days")

  is.between <- function(x, a, b) { 
    (x >= a) & (b >= x) 
  }
  
  is.available <- function(data, date) {
    data[[tolower(weekdays(date))]] == 1
  }
  
  calendar.dates <- gtfs[["calendar_dates"]]
  calendar <- gtfs[["calendar"]]
  trips <- gtfs[["trips"]]
  stop.times <- gtfs[["stop_times"]]
  routes <- gtfs[["routes"]]
  
  services <- setdiff(
    union(
      calendar.dates[calendar.dates$date == d & calendar.dates$exception_type == 1, c("service_id")],
      calendar[is.available(calendar, date) & is.between(date, calendar$start_date, calendar$end_date), c("service_id")]
    ),
    calendar.dates[calendar.dates$date == d & calendar.dates$exception_type == 2, c("service_id")]
  )
  
  t <- trips[trips$service_id %in% services, c("trip_id")]
  r <- unique(trips[trips$service_id %in% services, c("trip_id", "route_id")])
  r <- merge(r, routes[, c("route_id", "route_type")], all.x = T)[, c("trip_id", "route_type")]
  
  ret <- transform(stop.times[stop.times$trip_id %in% t, ],
    trip_id = trip_id,
    arrival_time =  d + time.to.seconds(arrival_time),
    departure_time = d + time.to.seconds(departure_time),
    pickup_type = factor(pickup_type),
    drop_off_type = factor(drop_off_type)
  )

  merge(ret, r, all.x = T)
}

gtfs.by.mode <- function(gtfs, m) {
  routes <- gtfs[["routes"]]
  routes <- droplevels(routes[routes$route_type == m, ])
  
  agency <- gtfs[["agency"]]
  agency <- droplevels(agency[agency$agency_id %in% routes$agency_id, ])
  
  trips <- gtfs[["trips"]]
  trips <- droplevels(trips[trips$route_id %in% routes$route_id, ])
  
  calendar <- gtfs[["calendar"]]
  calendar <- droplevels(calendar[calendar$service_id %in% trips$service_id, ])

  calendar_dates <- gtfs[["calendar_dates"]]
  calendar_dates <- droplevels(calendar_dates[calendar_dates$service_id %in% trips$service_id, ])
  
  shapes <- gtfs[["shapes"]]
  shapes <- droplevels(shapes[shapes$shape_id %in% trips$shape_id, ])
  
  stop_times <- gtfs[["stop_times"]]
  stop_times <- droplevels(stop_times[stop_times$trip_id %in% trips$trip_id, ])
  
  stops <- gtfs[["stops"]]
  stops <- droplevels(stops[stops$stop_id %in% stop_times$stop_id, ])
  
  list(
    "routes" = routes,
    "agency" = agency,
    "trips" = trips,
    "calendar" = calendar,
    "calendar_dates" = calendar_dates,
    "shapes" = shapes,
    "stop_times" = stop_times,
    "stops" = stops
  )
}

gtfs.trips.summary <- function(trips) {
  #trips <- old.singleday
  s <- trips[order(trips$trip_id, trips$stop_sequence), ]
  s$n <- 1:nrow(s)
  s$n1 <- s$n - 1

  # Warning message can be ignored: column name ‘n’ is duplicated in the result
  # 's' is ordered by trip and stop sequence. Hence previous row to current row contains
  # previous stop in actual trip or last stop from different trip.
  ss <- merge(s, s, by.x = "n", by.y = "n1", all.x = T, sort = F, suffixes = c("", ".x"))
  # we only need first or last stop for each trip
  cols <- c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "shape_dist_traveled", "n", "route_type")
  ss <- ss[ss$stop_sequence == 1 | ss$stop_sequence.x == 1 | is.na(ss$stop_sequence.x), cols]
  # compute summary
  trips.summary <- with(
    # ss looks like this:
    # trip_id=1 <info about first stop>
    # trip_id=1 <info about last stop>
    # ...
    # we need structure like this:
    # trip_id=1 <info about first stop> <info about last stop>
    merge(ss[ss$stop_sequence == 1, ], ss[ss$stop_sequence != 1, ], by = "trip_id", all.x = T, all.y = F, suffixes = c(".s", ".e")), {
    stops_levels <- levels(factor(union(stop_id.s, stop_id.e)))
    data.frame(
      trip_id = factor(trip_id),
      stop_from = factor(stop_id.s, levels = stops_levels),
      stop_to = factor(stop_id.e, levels = stops_levels),
      start_time = arrival_time.s,
      end_time = departure_time.e,
      travel_time = as.numeric(difftime(arrival_time.e, arrival_time.s, units = "secs")),
      distance = shape_dist_traveled.e / 1000, # in km
      stops = n.e - n.s + 1,
      route_type = route_type.s
    )}
  )
  
#
#   this takes ages...
#   
#   trips <- ddply(gtfs[["stop_times"]], .(trip_id), function(x) {
#     x <- x[order(x$stop_sequence), ]
#     first <- head(x, 1)
#     last <- tail(x, 1)
# 
#     data.frame(
#       stop_from = first$stop_id,
#       stop_to = last$stop_id,
#       distnace = last$shape_dist_traveled,
#       travel_time = time.to.seconds(last$departure_time) - time.to.seconds(first$arrival_time),
#       stops = nrow(x)
#     )    
#   }, .progress = "text")  
}

compute.hex.bins <- function(x, xbnds, ybnds, lat.bin.width, lon.bin.width) {
  
  bins <- hexbin(
    x = x$stop_lon, y = x$stop_lat, 
    xbins = diff(xbnds) / lon.bin.width,
    shape = lon.bin.width / lat.bin.width, # "square" bins
    xbnds = xbnds, ybnds = ybnds, 
    IDs = T
  )
  
  merge(
    data.frame(hcell2xy (bins),  ID = bins@cell),
    ddply(data.frame(ID = bins@cID, trip_id = x$trip_id, time = x$time), .(ID, time), function(x) { 
      data.frame(count = length(unique(x$trip_id)))
    }, .parallel = T),
    all.x = T
  )
}

truncDate <- function(x, i = 900) {
  x - (as.double(x) %% i)
}

truncDiffTime <- function(s, e, i = 900) {
  floor(as.numeric(difftime(e, s, units = "secs")) / i) * i
}

time.to.seconds <- function(t) {
  t <- strsplit(as.character(t), ":")
  sapply(t, function(y) sum(as.numeric(y) * c(3600, 60, 1)))
}


