########################################################################################################################
#
#   Loads Created Functions Found in Library directory
#   Author: Raffaele Vardavas
#   Last Update: 11/19/2015
#
########################################################################################################################
# Alternate Method Commented Out with "###"
### LIB <- new.env()
# Defines directory with files
library.dir   <- "R/"
### LIB$library.dir <- library.dir
# Creates list of files at directory
l <- list.files(path = library.dir)
# Removes non-R files
l <- l[grep("\\.R", l)]
# Checks for tilde's
tilde <- grep("\\.R~", l)
# If tilde's exist, files are removed from list
if (length(tilde) > 0) {
  l <- l[-tilde]
}
# Removes this script from list
l <- l[l != "library.R"]
# l <- l[l != "StatEllipse.R"]
# For every script in list, loads the file to the global environment
s <- sapply(l, FUN = function(x, library.dir) {
  sys.source(file.path(library.dir,x), .GlobalEnv)
}, library.dir)
### s <- sapply(l, FUN = function(x,library.dir, env) {
###   sys.source(file.path(library.dir,x), env)
###   }, library.dir, env = LIB)
# Cleans memory
rm(s,l,tilde)
