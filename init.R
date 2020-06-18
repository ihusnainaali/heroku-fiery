install.packages("plumber", repos="https://cloud.r-project.org/", force=FALSE)
library(plumber)
r <- plumb("endpoint.R")
r$run(port=8080)
