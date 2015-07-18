# This script will download, unzip, prep, and merge ACLED's Version 5 historical data (1997-2014) and its realtime
# data. The downside is that it will need to be modified as the realtime data are updated, or if the url or filename for
# the historical data changes. The upside is that those changes only need to be made in the "File info" section that
# starts on line 15. Unless the structure of the files or their variable names change, the rest should keep working.

# Function to get zip file and extract csv using vector of two string objects and returning data frame
getfile <- function(vector) {
  temp <- tempfile()
  download.file(vector[1], temp)
  df <- read.csv(unz(temp, vector[2]), stringsAsFactors=FALSE)
  unlink(temp)
  return(df)
}

# File info, copied from http://www.acleddata.com/data/ pages and 
past.url <- "http://www.acleddata.com/wp-content/uploads/2015/06/ACLED-Version-5-All-Africa-1997-2014_dyadic_Updated_csv-no-notes.zip"
past.file <- "ACLED-Version-5-All-Africa-1997-2014_dyadic_Updated_no_notes.csv"

realtime.url <- "http://www.acleddata.com/wp-content/uploads/2015/07/ACLED-All-Africa-File_20150101-to-20150711_csv.zip"
realtime.file <- "ACLED All Africa File_20150101 to 20150711_csv.csv"

# Data fetching
ACLED.targets <- list(c(past.url, past.file), c(realtime.url, realtime.file))
ACLED.list <- lapply(ACLED.targets, getfile)
names(ACLED.list[[1]]) <- sub("GEO_PRECIS", "GEO_PRECISION", names(ACLED.list[[1]])) # Fix names of location vars to match Version 5
names(ACLED.list[[2]]) <- gsub("ADM_LEVEL_", "ADMIN", names(ACLED.list[[2]])) # Fix names of location vars to match Version 5
ACLED <- Reduce(function(...) merge(..., all=TRUE), ACLED.list)
names(ACLED) <- tolower(make.names(names(ACLED)))

str(ACLED)
