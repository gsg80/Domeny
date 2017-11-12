library(XML)
library(RCurl)
library(httr)
library(R.utils)

setwd("/Users/gsg/R Projects/domeny")

f = "domeny_web.csv" #link do pliku
ver <- "1.0"


#funkcja ograniczająca czas sprawdzania url
checkURL <- function(url) {
  e <- FALSE
  e <- url.exists(url, .opts = list(timeout = 60, maxredirs = 2, verbose = FALSE))
  
  #	if(e == FALSE){
  #	  e <- withTimeout(url.exists(url, useragent="curl/7.39.0 Rcurl/1.95.4.5"), timeout=10, onTimeout="silent")
  #	}
  #print(paste(i, "checkURL ok"))
  return(e)
}

#funkcja ograniczajace czas pobrania linków
getLinks <- function(url) {
  ln <- ""
  #setTimeLimit(elapsed=60, transient=TRUE)
  ln <- getHTMLLinks(url)
  #print("getLinks ok")
  return(ln)
}

#funkcja ograniczająca czas pobrania htmla
getHTML <- function(url){
  h <- ""
  #setTimeLimit(elapsed=60, transient=TRUE)
  
  #h <- htmlParse(url)
  h <- htmlParse(rawToChar(GET(url)$content))
  #print('getHTML ok')
  return(h)	
}

danecsv <- read.csv2(f)
lmax <- length(danecsv[, "domena"])

if(lmax > 100) { 
  lmax <- 100
} 

#definicja wektorów z danymi zwrotnymi
eniro <- vector(mode="numeric", length=lmax)
fachowcy <- vector(mode="numeric", length=lmax)
sunrise <-vector(mode="numeric", length=lmax)
dataCrawling <- vector(mode="numeric", length=lmax)
wersja <- vector(mode="numeric", length=lmax)


for(i in 1 : lmax)
{
  #print(paste(i, "start"))
  danemain <- ""
  html <- ""
  
  url <- paste ("http://", danecsv[i,"domena"], sep="")
  print(paste(i, url))
  
  if(checkURL(url)){
    #print(paste(i, "url OK"))
    try({html <- getHTML(url)
    danemain <- xpathSApply(html, "//div", xmlValue)
    if(length(danemain) == 0) {
      danemain <- xpathSApply(html, "//p", xmlValue)
    } else {
      if(length(danemain) == 0) {
        danemain <- xpathSApply(html, "//body", xmlValue)
      }
    }
    }, silent=T)
  }    
    #WEB BY ENIRO	
    if(length(grep(pattern = "Website by Eniro Polska", danemain)) > 0) {
      eniro[i] <- 1
    } else {
      if(length(grep(pattern = "Stwórz własną stronę www z Panoramą Firm", danemain)) > 0){
        eniro[i] <- 1
      } else {
        eniro[i] <- 0
      }
    }
    
    #REALIZACJA fachowcy.pl	
    if(length(grep(pattern = "Fachowcy.pl Ventures", danemain)) > 0) {
      fachowcy[i] <- 1
    } else {
      fachowcy[i] <- 0
    }
    
    #SUNRISE 
    if(length(grep(pattern = "odpowiada Sunrise System", danemain)) > 0) {
      sunrise[i] <- 1
    } else {
      sunrise[i] <- 0
    } 
  
  wersja[i] <- ver
  #Data
  dataCrawling[i] <- as.character(Sys.Date())	
  if(i %% 100 == 0) {
    tabelaOut <- data.frame(id_domena = head(danecsv["id_domena"], lmax)
                            , domena = head(danecsv["domena"], lmax)
                            , webEniro = eniro
                            , webFachowcy = fachowcy
                            , sunrise = sunrise
                            , version = wersja
                            , data = dataCrawling
    )		
    
    write.csv(tabelaOut, "domeny_web_out.csv", fileEncoding = "UTF-8")
    
  }
  print(paste(i, "koniec"))		
}			

#danecsv2 <- read.csv("domeny_out.csv")
#n <- rbind(danecsv2[ , !(names(danecsv2) %in% 'X')], tabelaOut)			 
tabelaOut <- data.frame(id_domena = head(danecsv["id_domena"], lmax)
                        , domena = head(danecsv["domena"], lmax)
                        , webEniro = eniro
                        , webFachowcy = fachowcy
                        , sunrise = sunrise
                        , version = wersja
                        , data = dataCrawling							
)		

write.csv(tabelaOut, "domeny_web_out.csv", fileEncoding = "UTF-8")


