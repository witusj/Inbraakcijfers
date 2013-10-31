library("RCurl")
library("XML")

setwd("C:/Users/NB/Google Drive/Data Analysis/Dairy")

# Function to grab data from internet site according to given year
getCrimeData <- function(town) {
  
  town <- as.character(town)
  # Build url string
  urlStr <- paste0("http://www.politie.nl/misdaad-in-kaart/lijst?geoquery=",town,",%20Nederland&categorie=1&categorie=2&pageSize=500")
  
  # Get table
  CrimeData <- getURL(urlStr, ssl.verifypeer = FALSE)
  
  # Parse HTML into data frame and add variable names
  doc <- htmlParse(CrimeData)
  tableNodes <- getNodeSet(doc, "//table")
  tb <- readHTMLTable(tableNodes[[1]])
  
  tb
}

data <- getCrimeData("Den Haag")
summary(data)
head(data)
tail(data)