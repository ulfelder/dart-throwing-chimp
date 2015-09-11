# Functions to fetch, parse, and map Phoenix event data daily files singly or in groups by date range. For example:
# X <- fetchPhoenix(Sys.Date() - 1)
# X <- buildPhoenix("2015-09-01", Sys.Date() - 1)
# X2 <- parsePhoenix(X, start="2015-09-03", country=c("GRC", "SYR"), issue="REFUGEES")
# mapPhoenix(X2)
# The only package required for the downloading and parsing functions is 'downloader'. The mapping function depends
# on 'leaflet' and 'maps'.

fetchPhoenix <- function(date) {
  require(downloader)
  date <- gsub("-", "", date)
  url <- paste0("https://s3.amazonaws.com/openeventdata/current/events.full.", date, ".txt.zip")
  file <- paste0("events.full.", date, ".txt")
  tmp <- tempfile()
  download(url, tmp)
  Dailies <- read.delim(unz(tmp, file), header=FALSE, stringsAsFactors=FALSE)
  names(Dailies) <- c("EventID", "Date", "Year", "Month", "Day", "SourceActorFull",
    "SourceActorEntity", "SourceActorRole", "SourceActorAttribute",
    "TargetActorFull", "TargetActorEntity", "TargetActorRole",
    "TargetActorAttribute", "EventCode", "EventRootCode", "QuadClass",
    "GoldsteinScore", "Issues", "ActionLat", "ActionLong",
    "LocationName", "GeoCountryName", "GeoStateName", "SentenceID", "URLs",
    "NewsSources")
  return(Dailies)
}

buildPhoenix <- function(start, end) {
  dateset <- seq(as.Date(start), as.Date(end), by="days")
  List <- lapply(dateset, fetchPhoenix)
  DF <- do.call(rbind, List)
  return(DF)
}

parsePhoenix <- function(data, start="1900-01-01", end=Sys.Date(),
  country="any", sourcerole="any", targetrole="any", rootcode="any", eventcode="any", issue="any") {

  DF = data

  DF$Date2 = with(DF, paste(substr(Date, 1, 4), substr(Date, 5, 6), substr(Date, 7, 8), sep="-"))
  DF$Date2 = as.Date(DF$Date2)

  # filter by date range
  if (start!="1900-01-01") {
    DF <- DF[which(DF[,"Date2"] >= as.Date(start)),]
  }
  if (end!=Sys.Date()) {
    DF <- DF[which(DF[,"Date2"] <= as.Date(end)),]
  }

  # pick country or countries
  if (("any" %in% country)==FALSE) {
    DF <- DF[DF[,"GeoStateName"] %in% country,]
  }

  # pick source actor role(s)
  if (("any" %in% sourcerole)==FALSE) {
    DF <- DF[DF[,"SourceActorRole"] %in% sourcerole,]
  }

  # pick target actor role(s)
  if (("any" %in% targetrole)==FALSE) {
    DF <- DF[DF[,"TargetActorRole"] %in% targetrole,]
  }

  # pick event category or categories
  if (("any" %in% rootcode)==FALSE) {
    DF <- DF[DF[,"EventRootCode"] %in% rootcode,]
  }

  # pick event type(s)
  if (("any" %in% eventcode)==FALSE) {
    DF <- DF[DF[,"EventCode"] %in% eventcode,]
  }

  # pick issue(s)
  List <- strsplit(gsub(";", ",", DF$Issues), split=",")
  Index <- sapply(1:nrow(DF), function(x) as.logical(issue %in% List[[x]]))
  if (("any" %in% issue)==FALSE) {
    DF <- DF[which(Index),]
  }

  DF$Date2 <- NULL

  return(DF)

}

# quick-and-dirty interactive map of Phoenix events using 'leaflet' to render it in a browser, e.g.:
# X <- parsePhoenix(buildPhoenix("2015-07-01", "2015-09-10"), country="TUR", rootcode=c(18,19))
# mapPhoenix(X)
mapPhoenix <- function(PhoenixData) {
  require(maps)
  require(leaflet)
  lmap <- leaflet(PhoenixData)
  lmap <- addTiles(lmap)
  lmap <- addCircleMarkers(lmap, lng = PhoenixData[,"ActionLong"], lat = PhoenixData[,"ActionLat"],
    popup = paste(PhoenixData[,"Date"], PhoenixData[,"SourceActorFull"], PhoenixData[,"TargetActorFull"], sep = " "),
    stroke = FALSE)
  print(lmap)
}
