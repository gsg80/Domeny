#fraza <- 'stomatolog'
#miasto <- 'częstochowa'

library(XML)
library(RCurl)

url <- "https://www.google.pl/#q=stomatolog+częstochowa"
h <- htmlParse(url)
h


site <- getForm("http://www.google.com/search", hl="en",
                lr="", q="stomatolog częstochowa", btnG="Search")
htmlTreeParse(site)
