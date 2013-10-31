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
    "agency" = read.csv(file.path(path, "agency.txt")),
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
    "stop_times" = read.gtfs.file("stop_times.txt"),
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
  
  as.seconds <- function(t) {
      t <- strsplit(as.character(t), ":")
      sapply(t, function(y) sum(as.numeric(y) * c(3600, 60, 1)))
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
    arrival_time =  d + as.seconds(arrival_time),
    departure_time = d + as.seconds(departure_time),
    pickup_type = factor(pickup_type),
    drop_off_type = factor(drop_off_type)
  )

  merge(ret, r, all.x = T)
}

truncDate <- function(x, i = 600) {
  x - (as.double(x) %% i)
}
