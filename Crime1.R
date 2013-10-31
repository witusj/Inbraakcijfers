library("RCurl")
library("XML")


# Function to retrieve specific number from text
getCrimeNumber <- function(text) {
  
  # Get table
  CrimeData <- text
  CrimeData
  # Parse HTML into data frame and add variable names
  r <- regexpr("<h2 class=\"listHeader\">(.*?)&nbsp", CrimeData)
  
  CrimeNumber <- regmatches(CrimeData, r)
  CrimeNumber <- as.numeric(gsub("h2|[^0-9]", "", CrimeNumber, ignore.case = TRUE))
}


# Function to grab data from internet site according to given year
getCrimeData <- function(town) {
  
  town <- as.character(town)
  # Build url string
  urlStr <- paste0("http://www.politie.nl/misdaad-in-kaart/lijst?geoquery=",town,",%20Nederland&categorie=1&categorie=2&pageSize=500")
  
  # Get table
  CrimeData <- getURL(urlStr, ssl.verifypeer = FALSE)
  
  # Parse HTML into data frame
  doc <- htmlParse(CrimeData)
  tableNodes <- getNodeSet(doc, "//table")
  tb <- readHTMLTable(tableNodes[[1]])
  
  # Retrieve number of burglaries
  
  CrimeCount <- getCrimeNumber(CrimeData)
  i = 500
  p = 2
  
  while (i < CrimeCount) {
    
    # Build url string
    urlStr <- paste0("http://www.politie.nl/misdaad-in-kaart/lijst?geoquery=",town,",%20Nederland&categorie=1&categorie=2&pageSize=500&page=", p)
    
    # Get table
    CrimeData2 <- getURL(urlStr, ssl.verifypeer = FALSE)
    
    doc2 <- htmlParse(CrimeData2)
    tableNodes2 <- getNodeSet(doc2, "//table")
    tb2 <- readHTMLTable(tableNodes2[[1]])
    
    tb <- rbind(tb, tb2)
    
    i = i + 500
    p = p + 1
  }

  tb$Plaats <- rep(town,CrimeCount)
  
  tb  
  
}
newdata <- c()
for (t in c("Amsterdam", "Den Haag", "Rotterdam", "Utrecht", "Eindhoven", "Tilburg", "Groningen", "Breda", "Apeldoorn", "Nijmegen")){
  data <- getCrimeData(t)
  newdata <- rbind(newdata, data)
}
