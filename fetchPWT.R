# Function to read Penn World Tables into R using version number only
# For example, for Version 8.1, run fetchPWT(8.1)
# Only tested on Windows machine.

fetchPWT <- function(version) {
    require(readxl)
    vs <- sub("\\.", "", version)
    url <- paste0("http://www.rug.nl/research/ggdc/data/pwt/v", vs, "/pwt", vs, ".xlsx")
    temp <- paste0(tempfile(), ".xlsx")
    download.file(url, destfile = temp, mode = "wb")
    pwt <- read_excel(path = temp, sheet = 3)
    return(pwt)
}
