load.gtfs <- function(path) {
  parse.gtfs.date <- function(d) strptime(d, "%Y%m%d")
  
  WGS84toUTMzone56 <- function (latlon) {
    coordinates(latlon) <- ~ stop_lon + stop_lat
    proj4string(latlon) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    utm <- as.data.frame(spTransform(latlon, CRS("+proj=utm +zone=56 ellps=WGS84")))
    names(utm) <- c("lon", "lat")
    utm
  }

  list(
    "agency" = read.csv(file.path(path, "agency.txt")),
    "calendar" = transform(
      read.csv(file.path(path, "calendar.txt")),
      start_date = parse.gtfs.date(start_date),
      end_date = parse.gtfs.date(end_date)
    ),
    "calendar_dates" = transform(
      read.csv(file.path(path, "calendar_dates.txt")),
      date =  parse.gtfs.date(date)
    ),
    "routes" = read.csv(file.path(path, "routes.txt")),
    "shapes" = read.csv(file.path(path, "shapes.txt")),
    "stop_times" = read.csv(file.path(path, "stop_times.txt")),
    "stops" = transform(
      read.csv(file.path(path, "stops.txt")),
      utm = WGS84toUTMzone56(data.frame(stop_lat, stop_lon))
    ),
    "trips" = read.csv(file.path(path, "trips.txt"))
  )  
}

