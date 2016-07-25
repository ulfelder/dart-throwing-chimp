library(dplyr)
library(lubridate)

ACLED <- lapply(seq(1997, year(Sys.Date())), function(x) {

  read.csv(sprintf("http://acleddata.com/api/acled/read.csv?year=%d", x),
           stringsAsFactors = FALSE)

})

ACLED <- rbind_all(ACLED) %>%
  arrange(as.Date(event_date), gwno, event_type, actor1, actor2)
