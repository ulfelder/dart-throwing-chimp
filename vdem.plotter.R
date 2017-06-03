# a function to make a nice plot of v-dems electoral democracy index
# this function assumes the V-Dem dataset is in the local environment as a data frame
# named 'VDem'

vdem.plot <- function(id,
                      start = 1970,
                      end = as.numeric(substr(Sys.Date(), 1, 4)),
                      show.median = FALSE) {

    require(dplyr)
    require(ggplot2)
    require(countrycode)

    name <- countrycode(id, "iso3c", "country.name")

    p <- VDem %>%
        filter(country_text_id == id, year >= start, year <= end) %>%
        ggplot(aes(x = year, y = v2x_polyarchy)) +
            geom_line() +
            ylim(0,1) +
            geom_ribbon(aes(ymin = v2x_polyarchy_codelow, ymax = v2x_polyarchy_codehigh), alpha = 1/4) + 
            labs(x = "", y = "v-dem electoral democracy index", title = name) +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.minor = element_blank())

    if (show.median == TRUE) {

        m <- VDem %>%
            group_by(year) %>%
            summarise(median = median(v2x_polyarchy, na.rm = TRUE)) %>%
            filter(year >= start, year <= end)

        p <- p + geom_line(inherit.aes = FALSE,
                           mapping = aes(x = year, y = median),
                           data = m,
                           alpha = 2/3,
                           linetype = "dotted")
                          
    }

    print(p)

}

# examples:
# vdem.plot(countrycode("Venezuela", "country.name", "iso3c"), start = 1960, show.median = TRUE)
# vdem.plot("MKD", start = 1990, show.median = TRUE)
