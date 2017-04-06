# For fun, a function to check the gender of a first name, using the public API at Genderize.io. The 
# functionality could be expanded to let users control the locale or language, but you would either need 
# to use dropdown menus or write code to handle improperly formatted parameters.
#
# Try: namecheck("Chris")

namecheck <- function(name) {

  require(jsonlite)

  query <- sprintf("https://api.genderize.io/?name=%s&country_id=us", tolower(name))

  X <- fromJSON(query)

  capname <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))

  sprintf("Someone named %s is probably %s (%s percent)", capname, X$gender, 100 * X$probability)

}
