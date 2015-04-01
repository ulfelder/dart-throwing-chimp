# This script creates a function that produces a time-series cross-sectional file of valid country months between two
# specified dates, where "valid" means the country in question actually existed in the specified month. It is limited to
# countries with populations larger than 500,000 as of 2014. I use it as scaffolding for the construction of country-month
# analysis files from other sources.
#
# The function requires the package 'DataCombine'.
#
# The function is called f.yrmorack, and you call it like this:
#
# DataSetYouWantToMake <- f.yrmorack(<start date>, <end date>)
#
# where start date and end date take the form "yyyy-mm-dd", with the quotation marks included. For example:
#
# Frame <- f.yrmorack("1960-01-01", "2013-12-31")
#
# makes a country-month data set ranging from Jan 1960 to Dec 2013. The result includes three columns:
#
# country (str) -- country name, e.g., "Afghanistan"
# year (int) -- the year, e.g., 1960
# month (int) -- the month, e.g., 1
#
# Country list and most dates sourced to:
# Wikipedia: http://en.wikipedia.org/wiki/List_of_sovereign_states_by_date_of_formation
# CIA Factbook: https://www.cia.gov/library/publications/the-world-factbook/fields/2088.html
#
# For dates of independence, the moment when the former metropole recognized the new state's independence was preferred
# to the date when independence was declared. Countries that existed before 1000 AD were all given 1000-01-01 as their
# birth date.

f.yrmorack <- function(startdate, enddate) {

     # First, we need a function to make the stacks for individual countries
     kuntry <- function(country, birthdate, deathdate) {
          require(DataCombine)
          start <- ifelse(birthdate > startdate, birthdate, startdate)
          end <- ifelse(deathdate < enddate, deathdate, enddate)
          Data <- data.frame(country = rep(country, 2), date = as.Date(c(start, end)), stringsAsFactors = FALSE)
          EData <- TimeExpand(Data, GroupVar = "country", TimeVar = "date", by = "month")
          EData$year <- as.integer(substr(EData$date, 1, 4))
          EData$month <- as.integer(substr(EData$date, 6, 7))
          EData <- VarDrop(EData, "date")
          return(EData)
     }

     # AFRICA

     Algeria <- kuntry("Algeria", "1962-07-05", enddate)
     Angola <- kuntry("Angola", "1975-11-11", enddate)
     Benin <- kuntry("Benin", "1960-08-01", enddate)
     Botswana <- kuntry("Botswana", "1966-09-30", enddate)
     BurkinaFaso <- kuntry("Burkina Faso", "1960-08-05", enddate)
     Burundi <- kuntry("Burundi", "1962-07-01", enddate) 
     Cameroon <- kuntry("Cameroon", "1960-01-01", enddate)
     CapeVerde <- kuntry("Cape Verde", "1975-07-05", enddate)
     CAR <- kuntry("Central African Republic", "1960-08-13", enddate)
     Chad <- kuntry("Chad", "1960-08-11", enddate)
     Comoros <- kuntry("Comoros", "1975-07-06", enddate)
     DROC <- kuntry("Congo-Kinshasa", "1960-06-30", enddate)
     Congo <- kuntry("Congo-Brazzaville", "1960-08-15", enddate)
     IvoryCoast <- kuntry("Ivory Coast", "1960-08-07", enddate)
     Djibouti <- kuntry("Djibouti", "1977-06-27", enddate)
     Egypt <- kuntry("Egypt", "1953-06-18", enddate)
     EquatorialGuinea <- kuntry("Equatorial Guinea", "1968-10-12", enddate)
     Eritrea <- kuntry("Eritrea", "1993-05-24", enddate)
     Ethiopia <- kuntry("Ethiopia", "1000-01-01", enddate) # Really more like 1000 BC, but that's a hassle
     Gabon <- kuntry("Gabon", "1960-08-17", enddate)
     Gambia <- kuntry("Gambia", "1965-02-18", enddate)
     Ghana <- kuntry("Ghana", "1957-03-06", enddate)
     Guinea <- kuntry("Guinea", "1958-10-02", enddate)
     GuineaBissau <- kuntry("Guinea-Bissau", "1974-09-10", enddate)
     Kenya <- kuntry("Kenya", "1963-12-12", enddate)
     Lesotho <- kuntry("Lesotho", "1966-10-04", enddate)
     Liberia <- kuntry("Liberia", "1847-07-26", enddate)
     Libya <- kuntry("Libya", "1951-12-24", enddate)
     Madagascar <- kuntry("Madagascar", "1960-06-26", enddate)
     Malawi <- kuntry("Malawi", "1964-07-06", enddate)
     Mali <- kuntry("Mali", "1960-09-22", enddate)
     Mauritania <- kuntry("Mauritania", "1960-11-28", enddate)
     Mauritius <- kuntry("Mauritius", "1968-03-12", enddate)
     Morocco <- kuntry("Morocco", "1956-03-02", enddate)
     Mozambique <- kuntry("Mozambique", "1975-06-25", enddate)
     Namibia <- kuntry("Namibia", "1990-03-21", enddate)
     Niger <- kuntry("Niger", "1960-08-03", enddate)
     Nigeria <- kuntry("Nigeria", "1960-10-01", enddate)
     Rwanda <- kuntry("Rwanda", "1962-07-01", enddate)
     SaoTome <- kuntry("Sao Tome and Principe", "1975-07-12", enddate)
     Senegal <- kuntry("Senegal", "1960-08-20", enddate)
     SierraLeone <- kuntry("Sierra Leone", "1961-04-27", enddate)
     Somalia <- kuntry("Somalia", "1960-07-01", enddate)
     SouthAfrica <- kuntry("South Africa", "1931-12-11", enddate)
     SouthSudan <- kuntry("South Sudan", "2011-07-09", enddate)
     Sudan <- kuntry("Sudan", "1956-01-01", enddate)
     Swaziland <- kuntry("Swaziland", "1968-09-06", enddate)
     Tanzania <- kuntry("Tanzania", "1961-12-09", enddate)
     Togo <- kuntry("Togo", "1960-04-27", enddate)
     Tunisia <- kuntry("Tunisia", "1956-03-20", enddate)
     Uganda <- kuntry("Uganda", "1962-10-09", enddate)
     Zambia <- kuntry("Zambia", "1964-10-24", enddate)
     Zimbabwe <- kuntry("Zimbabwe", "1980-04-18", enddate)

     africa <- rbind(Algeria, Angola, Benin, Botswana, BurkinaFaso, Burundi, Cameroon, CapeVerde, CAR,
          Chad, Comoros, DROC, Congo, IvoryCoast, Djibouti, Egypt, EquatorialGuinea,
          Eritrea, Ethiopia, Gabon, Gambia, Ghana, Guinea, GuineaBissau, Kenya,
          Lesotho, Liberia, Libya, Madagascar, Malawi, Mali, Mauritania, Mauritius,
          Morocco, Mozambique, Namibia, Niger, Nigeria, Rwanda, Senegal, SierraLeone,
          Somalia, SouthAfrica, SouthSudan, Sudan, Swaziland, Tanzania, Togo, Tunisia,
          Uganda, Zambia, Zimbabwe)

     # AMERICAS

     Argentina <- kuntry("Argentina", "1816-07-09", enddate)
     Bolivia <- kuntry("Bolivia", "1825-08-06", enddate)
     Brazil <- kuntry("Brazil", "1825-08-29", enddate)
     Canada <- kuntry("Canada", "1867-07-01", enddate)
     Chile <- kuntry("Chile", "1844-04-25", enddate)
     Colombia <- kuntry("Colombia", "1819-08-07", enddate)
     CostaRica <- kuntry("Costa Rica", "1821-09-15", enddate)
     Cuba <- kuntry("Cuba", "1898-12-10", enddate)
     DominicanRepublic <- kuntry("Dominican Republic", "1865-03-03", enddate)
     Ecuador <- kuntry("Ecuador", "1830-05-13", enddate)
     ElSalvador <- kuntry("El Salvador", "1841-01-01", enddate)
     Guatemala <- kuntry("Guatemala", "1839-04-17", enddate)
     Guyana <- kuntry("Guyana", "1966-05-26", enddate)
     Haiti <- kuntry("Haiti", "1804-01-01", enddate)
     Honduras <- kuntry("Honduras", "1838-10-26", enddate)
     Jamaica <- kuntry("Jamaica", "1962-08-06", enddate)
     Mexico <- kuntry("Mexico", "1821-08-24", enddate)
     Nicaragua <- kuntry("Nicaragua", "1838-11-05", enddate)
     Panama <- kuntry("Panama", "1903-11-03", enddate)
     Paraguay <- kuntry("Paraguay", "1811-05-14", enddate)
     Peru <- kuntry("Peru", "1879-01-01", enddate)
     Trinidad <- kuntry("Trinidad and Tobago", "1962-08-31", enddate)
     USA <- kuntry("United States", "1783-09-03", enddate)
     Uruguay <- kuntry("Uruguay", "1828-08-27", enddate)
     Venezuela <- kuntry("Venezuela", "1830-01-13", enddate)

     americas <- rbind(Argentina, Bolivia, Brazil, Canada, Chile,
          Colombia, CostaRica, Cuba, DominicanRepublic, Ecuador, ElSalvador,
          Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua,
          Panama, Paraguay, Peru, Trinidad, USA, Uruguay, Venezuela)

     # ASIA

     Afghanistan <- kuntry("Afghanistan", "1747-01-01", enddate)
     Bahrain <- kuntry("Bahrain", "1971-08-15", enddate)
     Bangladesh <- kuntry("Bangladesh", "1971-03-26", enddate)
     Bhutan <- kuntry("Bhutan", "1885-01-01", enddate)
     Cambodia <- kuntry("Cambodia", "1953-09-09", enddate)
     China <- kuntry("China", "1000-01-01", enddate) # Earlier, I know; this is a convenience.
     India <- kuntry("India", "1947-08-15", enddate)
     Indonesia <- kuntry("Indonesia", "1949-12-27", enddate)
     Iran <- kuntry("Iran", "1000-01-01", enddate) # Placeholder
     Iraq <- kuntry("Iraq", "1932-10-03", enddate)
     Israel <- kuntry("Israel", "1948-05-14", enddate)
     Japan <- kuntry("Japan", "1000-01-01", enddate) # Yeah, yeah, yeah...
     Jordan <- kuntry("Jordan", "1946-05-26", enddate)
     Kuwait <- kuntry("Kuwait", "1961-06-19", enddate)
     Laos <- kuntry("Laos", "1949-07-19", enddate)
     Lebanon <- kuntry("Lebanon", "1943-11-22", enddate)
     Malaysia <- kuntry("Malaysia", "1957-08-31", enddate)
     Mongolia <- kuntry("Mongolia", "1911-12-29", enddate)
     Myanmar <- kuntry("Myanmar", "1948-01-04", enddate)
     Nepal <- kuntry("Nepal", "1768-12-21", enddate)
     NorthKorea <- kuntry("North Korea", "1948-09-09", enddate)
     Oman <- kuntry("Oman", "1650-01-26", enddate)
     Pakistan <- kuntry("Pakistan", "1947-08-14", enddate)
     Philippines <- kuntry("Philippines", "1898-06-12", enddate)
     Qatar <- kuntry("Qatar", "1971-09-03", enddate)
     SaudiArabia <- kuntry("Saudi Arabia", "1927-05-20", enddate)
     Singapore <- kuntry("Singapore", "1965-08-09", enddate)
     SouthKorea <- kuntry("South Korea", "1948-08-15", enddate)
     SriLanka <- kuntry("Sri Lanka", "1972-05-22", enddate)
     Syria <- kuntry("Syria", "1946-04-17", enddate)
     Taiwan <- kuntry("Taiwan", "1949-01-01", enddate)
     Thailand <- kuntry("Thailand", "1000-01-01", enddate) # Again...
     TimorLeste <- kuntry("Timor Leste", "2002-05-20", enddate)
     UAE <- kuntry("United Arab Emirates", "1971-12-02", enddate)
     Vietnam <- kuntry("Vietnam", "1976-07-02", enddate)
     Yemen <- kuntry("Yemen", "1990-05-22", enddate)

     asia <- rbind(Afghanistan, Bahrain, Bangladesh, Bhutan, Cambodia, China, India,
          Indonesia, Iran, Iraq, Israel, Japan, Jordan, Kuwait, Laos, Lebanon,
          Malaysia, Mongolia, Myanmar, Nepal, NorthKorea, Oman, Pakistan,
          Philippines, Qatar, SaudiArabia, Singapore, SouthKorea, SriLanka,
          Syria, Taiwan, Thailand, TimorLeste, UAE, Vietnam, Yemen) 

     # EUROPE

     Albania <- kuntry("Albania", "1912-11-28", enddate)
     Austria <- kuntry("Austria", "1918-11-12", enddate)
     Belarus <- kuntry("Belarus", "1991-08-25", enddate)
     Belgium <- kuntry("Belgium", "1830-10-04", enddate)
     Bosnia <- kuntry("Bosnia and Herzegovina", "1992-03-03", enddate)
     Bulgaria <- kuntry("Bulgaria", "1908-09-22", enddate)
     Croatia <- kuntry("Croatia", "1991-10-08", enddate)
     Cyprus <- kuntry("Cyprus", "1960-08-16", enddate)
     CzechRepublic <- kuntry("Czech Republic", "1993-01-01", enddate)
     Denmark <- kuntry("Denmark", "1000-01-01", enddate)
     Estonia <- kuntry("Estonia", "1991-08-25", enddate)
     Finland <- kuntry("Finland", "1918-01-03", enddate)
     France <- kuntry("France", "1000-01-01", enddate)
     Germany <- kuntry("Germany", "1990-10-03", enddate)
     Greece <- kuntry("Greece", "1832-03-04", enddate)
     Hungary <- kuntry("Hungary", "1849-01-01", enddate)
     Ireland <- kuntry("Ireland", "1922-12-06", enddate)
     Italy <- kuntry("Italy", "1861-03-17", enddate)
     Kosovo <- kuntry("Kosovo", "2008-02-17", enddate)
     Latvia <- kuntry("Latvia", "1991-08-25", enddate)
     Lithuania <- kuntry("Lithuania", "1991-08-25", enddate)
     Macedonia <- kuntry("Macedonia", "1991-09-08", enddate)
     Moldova <- kuntry("Moldova", "1991-08-25", enddate)
     Montenegro <- kuntry("Montenegro", "2006-06-03", enddate)
     Netherlands <- kuntry("Netherlands", "1648-05-15", enddate)
     Norway <- kuntry("Norway", "1905-06-07", enddate)
     Poland <- kuntry("Poland", "1918-11-11", enddate)
     Portugal <- kuntry("Portugal", "1143-10-05", enddate)
     Romania <- kuntry("Romania", "1878-07-13", enddate)
     Serbia <- kuntry("Serbia", "2006-06-08", enddate)
     Slovakia <- kuntry("Slovakia", "1993-01-01", enddate)
     Slovenia <- kuntry("Slovenia", "1991-06-25", enddate)
     Spain <- kuntry("Spain", "1512-01-01", enddate)
     Sweden <- kuntry("Sweden", "1523-06-06", enddate)
     Switzerland <- kuntry("Switzerland", "1291-01-01", enddate)
     Turkey <- kuntry("Turkey", "1923-10-29", enddate)
     Ukraine <- kuntry("Ukraine", "1991-08-25", enddate)
     UnitedKingdom <- kuntry("United Kingdom", "1707-05-01", enddate)
     Armenia <- kuntry("Armenia", "1991-08-25", enddate)
     Azerbaijan <- kuntry("Azerbaijan", "1991-08-25", enddate)
     Georgia <- kuntry("Georgia", "1991-08-25", enddate)
     Kazakhstan <- kuntry("Kazakhstan", "1991-08-25", enddate)
     Kyrgyzstan <- kuntry("Kyrgyzstan", "1991-08-25", enddate)
     Russia <- kuntry("Russia", "1991-08-25", enddate)
     Tajikistan <- kuntry("Tajikistan", "1991-08-25", enddate)
     Turkmenistan <- kuntry("Turkmenistan", "1991-08-25", enddate)
     Uzbekistan <- kuntry("Uzbekistan", "1991-08-25", enddate)

     europe <- rbind(Albania, Austria, Belgium, Bosnia, Bulgaria, Croatia, Cyprus,
          CzechRepublic, Denmark, Finland, France, Germany, Greece, Hungary,
          Ireland, Italy, Macedonia, Montenegro, Netherlands, Norway, Poland,
          Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Switzerland,
          Sweden, Turkey, UnitedKingdom)

     fsu <- rbind(Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan,
          Kyrgyzstan, Latvia, Lithuania, Moldova, Russia, Tajikistan,
          Turkmenistan, Ukraine, Uzbekistan) 

     # OCEANIA

     Australia <- kuntry("Australia", "1901-01-01", enddate)
     Fiji <- kuntry("Fiji", "1970-10-10", enddate)
     NewZealand <- kuntry("New Zealand", "1907-09-26", enddate)
     PapuaNewGuinea <- kuntry("Papua New Guinea", "1975-09-16", enddate)
     SolomonIslands <- kuntry("Solomon Islands", "1978-07-07", enddate)

     oceania <- rbind(Australia, Fiji, NewZealand, PapuaNewGuinea, SolomonIslands)

     # DEFUNCT

     Czechoslovakia <- kuntry("Czechoslovakia", "1918-11-11", "1992-12-31")
     Yugoslavia <- kuntry("Yugoslavia", "1918-11-11", "1992-01-15")
     FedRepYugoslavia <- kuntry("Federal Republic of Yugoslavia", "1992-01-15", "2003-02-04")
     SerbiaMontenegro <- kuntry("Serbia and Montenegro", "2003-02-04", "2006-06-08")
     WestGermany <- kuntry("West Germany", "1945-08-02", "1990-10-03")
     EastGermany <- kuntry("East Germany", "1945-08-02", "1990-10-03")
     USSR <- kuntry("Soviet Union", "1922-12-28", "1991-08-25")
     NorthYemen <- kuntry("North Yemen", "1918-11-01", "1990-05-22")
     SouthYemen <- kuntry("South Yemen", "1967-11-30", "1990-05-22")
     NorthVietnam <- kuntry("North Vietnam", "1954-07-21", "1976-07-02")
     SouthVietnam <- kuntry("South Vietnam", "1954-07-21", "1976-07-02")

     defunct <- rbind(Czechoslovakia, Yugoslavia, FedRepYugoslavia, SerbiaMontenegro,
          WestGermany, EastGermany, USSR, NorthYemen, SouthYemen,
          NorthVietnam, SouthVietnam)

     # Aggregate
     rack <- as.data.frame(rbind(africa, americas, asia, europe, fsu, oceania, defunct),
          stringsAsFactors = FALSE)
     
     print(unique(rack$country))
     rack$iso3c <- countrycode::countrycode(rack$country, "country.name", "iso3c")
     rack[rack$country=="North Yemen", "iso3c"] <- "YEM"
     rack[rack$country=="South Yemen", "iso3c"] <- "YMD"
     rack[rack$country=="North Vietnam", "iso3c"] <- "VDR"
     rack[rack$country=="Serbia and Montenegro", "iso3c"] <- "YMD"
     rack[rack$country=="Soviet Union", "iso3c"] <- "SUN"
     rack[rack$country=="Serbia and Montenegro", "iso3c"] <- "SCG"

     tmpdate <- as.Date(lubridate::ymd(paste0(rack$year, "-", rack$month, "-01")))
     rack <- rack[tmpdate >= startdate,]
     print(unique(rack$country))
     return(rack)

}
