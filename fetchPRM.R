url <- "http://www.uni-heidelberg.de/md/politik/personal/croissant/forschung/prm_dataset_-_2016-10.zip"
target.file <- "military influence_TS.CSV"

tmp <- paste0(tempfile(), ".zip") # Create tempfile connection
download.file(url, tmp) # Download the .zip file to that connection
tmp2 <- tempfile() # Create another connection
tmp2 <- unzip(zipfile = tmp, files = target.file, exdir = tempdir()) # Unzip the specified file from the download to that slot
PRM <- read.csv(tmp2, stringsAsFactors = FALSE)
unlink(c(tmp, tmp2))

# kill a few columns that are all NAs
PRM$IDENT <- PRM$INDEX <- PRM$RAW <- NULL
