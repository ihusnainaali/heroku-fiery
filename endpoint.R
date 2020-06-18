# endpoint.R

#* @get /
function() {
	"Hello World! This is the homepage of my little R service!"
}

#* @get /add
addTwo <- function(a, b) {
  
  library(rvest)
  library(stats)
  library(jsonlite)
  library(dplyr, warn.conflicts = FALSE)
  
  # Start Scraping
  print(a)
  print(b)
  trails_webpage <- read_html(a)
  
  print("1")
  
  ouputName <- trails_webpage %>% html_nodes(b) %>% html_text()
  
  index = NULL
  for (value in 1:length(ouputName)) {
    if(ouputName[value] == "\n") {
      index = append(index, value)
    }
  }
  
  Name <- ouputName[ouputName != "\n"]
  datFrame <- data.frame(Name)
  
  jsonData <- toJSON(datFrame)
  return(jsonData)
}
