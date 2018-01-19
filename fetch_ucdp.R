fetch_ucdp <- function(dataset = "ucdpprioconflict", version = "17.2") {

	require(jsonlite)

	link <- sprintf("http://ucdpapi.pcr.uu.se/api/%s/%s?pagesize=1", dataset, version)

	ping <- fromJSON(link)

	pages <- ping$TotalPages

	raw <- lapply(seq(0, floor(pages/1000)), function(x) {
	
		url <- sprintf("http://ucdpapi.pcr.uu.se/api/%s/%s?pagesize=1000&page=%s",
		               dataset, version, x)
	
		X <- fromJSON(url)
	
 		Y <- flatten(X$Result)
	
	})

	table <- do.call(rbind, raw)

	return(table)

}
