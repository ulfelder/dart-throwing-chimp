# Function to read Penn World Tables into R using version number as string, e.g.
# PWT <- fetchPWT("9.0")
# Only tested on Windows 10 machine; last tested 2016-10-14

fetchPWT <- function(version) {
    
  require(readxl)
    
  vs <- sub("\\.", "", version)
    
  url <- paste0("http://www.rug.nl/research/ggdc/data/pwt/v", vs, "/pwt", vs, ".xlsx")
  
  temp <- paste0(tempfile(), ".xlsx")
  
  download.file(url, destfile = temp, mode = "wb")
  
  PWT <- read_excel(path = temp, sheet = 3)
  
  return(PWT)

}
