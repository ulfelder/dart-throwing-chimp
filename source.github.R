# Function to source R scripts from GitHub (so meta), where `u` is the URL obtained by clicking on the Raw button
# when viewing a script on GitHub; see:
# http://stackoverflow.com/questions/8229859/sourcing-an-r-script-from-github-for-global-session-use-from-within-a-wrapper

source_github <- function(u) {
  require(RCurl)
  script <- getURL(u, ssl.verifypeer=FALSE)
  eval(parse(text = script), envir=.GlobalEnv)
}
