#Dependencies need to be installed before analysis can begin
#RDieharder contains the test suite that will be used and RJson allows for the reading of JSON files
install.packages("RDieHarder")
install.packages("rjson")

library(RDieHarder)
library(rjson)

#The working directory is set to allow access to stored JSON files
setwd("D:/Github/PROJ518")

result <- fromJSON(file = "input.json")