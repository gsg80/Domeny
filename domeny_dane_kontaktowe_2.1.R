  library(XML)
  library(RCurl)
  library(httr)
  #library(R.utils)
  
  setwd("/Users/gsg/R Projects/domeny")
  
  f = "domeny.csv" #link do pliku
  ver <- "2.12"
  
  allLinks <- ""
  links2match <- c(".pl",".eu",".net",".biz",".com")
  
  parsePhoneFraza <- function(text){
    fraza <- ""  
    
    if(length(grep(pattern = "\\+48", text)) > 0){
      fraza <- "\\+48"
    } else {
      if(length(grep(pattern = "0048", text)) > 0){
        fraza <- "0048"
    } else {
      if(length(grep(pattern = "telkom", text)) > 0){
        if(length(grep(pattern = "telkomórkowy", text)) > 0){
          fraza <- "telkomórkowy"
        } else {
          fraza <- "telkom"
        }
      } else {
        if(length(grep(pattern = "telfax", text)) > 0){
          fraza <- "telfax"		
        } else {
          if(length(grep(pattern = "tel", text)) > 0){
            if(length(grep(pattern = "telefonu", text)) > 0){
              fraza <- "telefonu"
            } else {
              if(length(grep(pattern = "telefon", text)) > 0){
                fraza <- "telefon"
              } else {
                fraza <- "tel"	
              }
            } 
          } else {
            if(length(grep(pattern = "zadzwoń", text)) > 0) {
              fraza <- "zadzwoń"  
            }
          }
        }
      }
    }
    }  
  return(fraza)
  }
  
  #funkcja wyszukujaca telefon w tekście
  parsePhone <- function(text) {
  	ptel <- ""
  	fraza <- ""
  	text <- tolower(text)
  	
  	loopOut <- 0
  	while(loopOut == 0){
  	  fraza <- parsePhoneFraza(text)
  	  dl <- nchar(fraza)
  	  if(dl == 0){
  	    loopOut <- 1
  	  }
  	  z <- 0
  	
  	  zer <- substring(text, regexpr(fraza, text)+dl, regexpr(fraza, text)+dl)
  	  #print(zer)
  	  if( zer == "0"){
  	    z <- 1
  	  }
  	
  	  if(fraza == "\\+48")  {
  	    dl <- dl - 2
  	  } 
  	  
  	  ptel <- substring(text, regexpr(fraza, text)+dl+z, regexpr(fraza, text)+dl+z+8)
  	
  	  if(length(grep(pattern = "[0-9]{9}", ptel)) == 0 || nchar(ptel) != 9){
  	    text <- substring(text, regexpr(fraza, text)+dl+z, nchar(text))
  	    ptel <- ""
  	  }	else {
  	    loopOut <- 1
  	  }
  	}
  return(ptel)
  }
  
  #funkcja do wyjęcia telefonu z atrybutu "phone"
  parsePhoneAttribute <- function(text){
    ptel <- "0"
    try({  
      if(length(xpathSApply(html, "//li[@class='phone']", xmlValue)) > 0){
        tellist <- xpathSApply(html, "//li[@class='phone']", xmlValue)
        tellist <- gsub(" ","", tellist, fixed = TRUE)
        tellist <- substring(tellist, nchar(tellist) - 8, nchar(tellist))
        ptel <- tellist[min(grep(pattern = "[0-9]{9}", tellist))]
      }
    }, silent=T)
    return(ptel)
  }
  
  #funkcja do wyjęcia telefonu z tekstu na stronie
  parsePhoneGSM <- function(text){
    ptel <- "0"
    st <- 0
    if(length(grep(pattern = "[0-9]{3}[ |-][0-9]{3}[ |-][0-9]{3}", text) > 0)){
      st <- regexpr(pattern = "[0-9]{3}[ |-][0-9]{3}[ |-][0-9]{3}", text) 
    } else {
      if(length(grep(pattern = "[0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", text)) > 0){
        st <- regexpr(pattern = "[0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", text) 
      }
    }
    
      ptel <- substr(text, st, st + attr(st, "match.length"))
      
      ptel <- gsub("\r", "", ptel, fixed = TRUE)	
      ptel <- gsub("\n", "", ptel, fixed = TRUE)		
      ptel <- gsub("\t", "", ptel, fixed = TRUE)  
      ptel <- gsub(" ","", ptel, fixed = TRUE)
      ptel <- gsub("-","", ptel, fixed = TRUE)
      
      if(nchar(ptel) > 9){
        ptel <- substring(ptel, 1, 9)
      }
    
    return(ptel)
  }
  
  #funkcja do wyjęcia telefonu stcjonarnego z tekstu na stronie
  parsePhoneStationary <- function(text){
    ptel <- "0"
    st <- 0
    if(length(grep(pattern = "[0-9]{2}[ |)][0-9]{7}", text)) > 0){
      st <- regexpr(pattern = "[0-9]{2}[ |)][0-9]{7}", text) 
      ptel <- substr(text, st, st + attr(st, "match.length"))   
    } else {
      if(length(grep(pattern = "[0-9]{2}[ |)][0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}", text)) > 0){
        st <- regexpr(pattern = "[0-9]{2}[ |)][0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}", text) 
        ptel <- substr(text, st, st + attr(st, "match.length"))
      } else {
        if(length(grep(pattern = "[0-9]{2}[ |)][0-9][ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", text)) > 0){
          st <- regexpr(pattern = "[0-9]{2}[ |)][0-9][ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", text) 
          ptel <- substr(text, st, st + attr(st, "match.length"))
        }
      }
    }
      ptel <- gsub("\r", "", ptel, fixed = TRUE)	
      ptel <- gsub("\n", "", ptel, fixed = TRUE)		
      ptel <- gsub("\t", "", ptel, fixed = TRUE)  
      ptel <- gsub(" ","", ptel, fixed = TRUE)
      ptel <- gsub("-","", ptel, fixed = TRUE)
      
      if(nchar(ptel) > 9){
        ptel <- substring(ptel, 1, 9)
      }
    
    return(ptel)
  }
  
  #funkcja do wyszukiwania w tekscie adresow e-mail
  parseMail <- function(text){
  	adresEmail <- ""
  	              
  	if(length(grep(pattern = "e-mail", text)) > 0){
  	  em <- regexpr('e-mail', text)
      sufiks <- ''
      r <- 0
      
  	  pl <- regexpr('.pl', text) 
  	  eu <- regexpr('.eu', text)
  	  com <- regexpr('.com', text)
  	  #fm <- regexpr('.fm', text)
  	  
  	  if(pl < 0) pl <- 100000
  	  if(eu < 0) eu <- 100000
  	  if(com < 0) com <- 100000
  	  #if(fm < 0) fm <- 100000
  	  
  	  if (pl < eu) {
  	    if (pl < com){
  	      r <- pl
  	      sufiks <- 'pl'
  	    } else {
  	      r <- com
  	      sufiks <- 'com'
  	    }
  	  } else {
  	    if (eu < com){
  	      r <- eu
  	      sufiks <- 'eu'
  	    } else {
  	      r <- com
  	      sufiks <- 'com'
  	    }
  	  }
  	  
  		if(r > em){
  			adresEmail <- substring(text, em+6, r + if(sufiks=='com') 3 else 2 )	
  		}
  	} else {
  		if(length(grep(pattern = "email", text)) > 0){
  		  em <- regexpr('email', text)
  		  sufiks <- ''
  		  r <- 0
  		  
  		  pl <- regexpr('.pl', text) 
  		  eu <- regexpr('.eu', text)
  		  com <- regexpr('.com', text)
  		  if(pl < 0) pl <- 100000
  		  if(eu < 0) eu <- 100000
  		  if(com < 0) com <- 100000
  		  
  		  if (pl < eu) {
  		    if (pl < com){
  		      r <- pl
  		      sufiks <- 'pl'
  		    } else {
  		      r <- com
  		      sufiks <- 'com'
  		    }
  		  } else {
  		    if (eu < com){
  		      r <- eu
  		      sufiks <- 'eu'
  		    } else {
  		      r <- com
  		      sufiks <- 'com'
  		    }
  		  }
  		  
  		  if(r > em){
  		    adresEmail <- substring(text, em+5, r + if(sufiks=='com') 3 else 2 )	
  		  }
  		} 
  			  		
  	}
  	return(adresEmail)
  }
  
  #funkcja usuwajaca zbedne znaki przed wyszukaniem telefonu
  clearText <- function(itext, typ="TEL") {
  	ctext <- itext
  
  	ctext <- gsub("*","", ctext, fixed = TRUE)
  	ctext <- gsub(" ","", ctext, fixed = TRUE)
  	ctext <- gsub("(","", ctext, fixed = TRUE)
  	ctext <- gsub(")","", ctext, fixed = TRUE)
  	ctext <- gsub("\u00A0", "", ctext, fixed = TRUE)	
  	ctext <- gsub("\r", "", ctext, fixed = TRUE)	
  	ctext <- gsub("\n", "", ctext, fixed = TRUE)		
  	ctext <- gsub("\t", "", ctext, fixed = TRUE)	
  	ctext <- gsub(",", "", ctext, fixed = TRUE)
  	ctext <- gsub(":", "", ctext, fixed = TRUE)	
  	ctext <- gsub("/", "", ctext, fixed = TRUE)
  	ctext <- gsub("<strong>","", ctext, fixed=TRUE)
  	ctext <- gsub("<li>","", ctext, fixed=TRUE)
  	
  	if(typ == "NIP") {
  	  ctext <- gsub("PL", "", ctext, fixed = TRUE)
  	}
  	  
  	if(typ == "TEL" || typ == "NIP") {
  		ctext <- gsub(".", "", ctext, fixed = TRUE)	
  		ctext <- gsub("-","", ctext, fixed = TRUE)			
  	}
  #	if(typ == "TEL") {
  #		ctext <- gsub("+48", "", ctext, fixed = TRUE)	
  #		ctext <- gsub("+ 48", "", ctext, fixed = TRUE)
  #		ctext <- gsub("0048", "", ctext, fixed = TRUE)			
  #	}
  
  	return(ctext)
  }
  
  #funkcja usuwajaca zbedne znaki przed wyszukaniem telefonu
  clearKierunkowy <- function(itext) {
    ctext <- itext
    
    ctext <- gsub("+48", "", ctext, fixed = TRUE)	
    ctext <- gsub("+ 48", "", ctext, fixed = TRUE)
    ctext <- gsub("0048", "", ctext, fixed = TRUE)			
   
    return(ctext)
  }
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
  
  #funkcja do wyszukania linków do zdefioniowanych serwisów/social media
  getSocialMediaLinks <- function(v){
    
    vSocial <- c("facebook.com/","instagram.com/","linkedin.com/","allegro.pl/",".otomoto.pl")
    vFound <- c("","","","","")
    for(i in 1:5){
      if(is.na(v[min(grep(pattern = vSocial[i], v))]) == TRUE){
        vFound[i] <- ""
      } else {
        vFound[i] <- v[min(grep(pattern = vSocial[i], v))]
      }
    }
    
    return(vFound)
  } 
  
  danecsv <- read.csv2(f)
  
  lmax <- length(danecsv[, "domena"])
  
  if(lmax > 10000) { 
      lmax <- 10000
      } 
  
  #definicja wektorów z danymi zwrotnymi
  urlWorks <- vector(mode="numeric", length=lmax)
  urlKontakt <- vector(mode="character", length=lmax)
  urlRegulamin <- vector(mode="character", length=lmax)
  
  textG <- vector(mode="character", length=lmax)
  textN <- vector(mode="character", length=lmax)
  textK <- vector(mode="character", length=lmax)
  textE <- vector(mode="character", length=lmax)
  textP <- vector(mode="character", length=lmax)
  textU <- vector(mode="character", length=lmax)
  textTitle <- vector(mode="character", length=lmax)
  textFB <- vector(mode="character", length=lmax)
  textInsta <- vector(mode="character", length=lmax)
  textLinked <- vector(mode="character", length=lmax)
  textAllegro <- vector(mode="character", length=lmax)
  textOtomoto <- vector(mode="character", length=lmax)
  
  tel <- vector(mode="numeric", length=lmax)
  eniro <- vector(mode="numeric", length=lmax)
  nip <-vector(mode="numeric", length=lmax)
  krs <-vector(mode="numeric", length=lmax)
  mail <-vector(mode="numeric", length=lmax)
  pna <-vector(mode="numeric", length=lmax)
  ulica <-vector(mode="numeric", length=lmax)
  wersja <-vector(mode="numeric", length=lmax)
  sunrise <-vector(mode="numeric", length=lmax)
  rzetelna <- vector(mode="numeric", length=lmax)
  koszyk <- vector(mode="numeric", length=lmax)
  dataCrawling <- vector(mode="numeric", length=lmax)
  
  fkoszyk <- c('Koszyk','Basket','koszyk','basket','sklep internetowy','Sklep internetowy','internetowy sklep','księgarnia internetowa','ksiêgarnia internetowa','Ksiêgarnia internetowa')
  fniekoszyk <- c('Koszykówka','koszykówka','koszykowej','Koszykowej','Basketball','basketball')
  
  for(i in 1 : lmax)
  {
  	#print(paste(i, "start"))
  	m <- 0
  	mN <- 0
  	mK <- 0
  	mE <- 0
  	mP <- 0
  	mU <- 0
  	opcja <- 0
  	dane <- ""
  	danem <- ""
  	danemain <- ""
  	daneTitle <- ""
  	danep <- ""
  	html <- ""
  	links <- ""
  	links_v2 <- ""
  	dkoszyk <- c(0,0,0,0,0,0,0,0)
  	dmkoszyk <- c(0,0,0,0,0,0,0,0)
  	soclinks <- ""
  	
  	url <- paste ("http://", danecsv[i,"domena"], sep="")
  	print(paste(i, url))
  	
  	if(checkURL(url)){
  		#print(paste(i, "url OK"))
  		urlWorks[i] <- 1
  		try(links <- getLinks(url), silent=T)
  		if(danecsv[i, "czy_linki"] == 0){ 
  		  allLinks <- c(allLinks, substring(links, 1, 255))
  		}
  		
  		kontakt <-links[grep(pattern = "(K|k|C|c)onta(k|c)t", links)]
  		kontakt <- kontakt[grep(pattern = "mailto", kontakt, invert=TRUE)]
  		kontakt <- kontakt[grep(pattern = "home.pl", kontakt, invert=TRUE)]	

  		try({html <- getHTML(url)
  		#print(paste(i, "html"))
  		daneTitle <- xpathSApply(html, "//head/title", xmlValue)  
  		#print(paste(i, "title"))		
  		
  		danem <- xpathSApply(html, "//meta", xmlGetAttr, 'content') 
  		#print(paste(i, "danetitle"))
  		danemain <- xpathSApply(html, "//div", xmlValue)
  		#print(paste(i, "danemain"))
  		links_v2 <- ""
  		links_v2 <- xpathSApply(html, "//a", xmlGetAttr, "href")
  		if(length(danemain) == 0) {
  		  danemain <- xpathSApply(html, "//p", xmlValue)
  		} else {
  		  if(length(danemain) == 0) {
  		    danemain <- xpathSApply(html, "//body", xmlValue)
  		  }
  		}
  		}, silent=T)
      
  		#print(paste(i, kontakt[1]))
  		
  		if(length(kontakt) > 0){
  			if(checkURL(links[min(grep(pattern = "(K|k|C|c)onta(k|c)t", links))])){
  				urlKontakt[i] <- links[min(grep(pattern = "(K|k|C|c)onta(k|c)t", links))]
  				#print(paste(url, "1", sep=" - "))	
  				}
  			else{
  				urlKontakt[i] <- paste(url,links[min(grep(pattern = "(K|k|C|c)onta(k|c)t", links))],sep="")
  				if(!(checkURL(urlKontakt[i]))){
  					urlKontakt[i] <- paste(url,"/",links[min(grep(pattern = "(K|k|C|c)onta(k|c)t", links))],sep="")		
  				} 
  				#print(paste(url, "2", sep=" - "))
  			}
  		} else {
  		    #print("links_v2")
  		    dl <- 0
  		    try(dl <- length(grep(pattern="(K|k|C|c)onta(k|c)t", links_v2)), silent=T)
  		      if(dl > 0){
  	
  		        urlKontakt[i] <- links_v2[[min(grep(pattern="(K|k|C|c)onta(k|c)t",links_v2))]]
  		      } else {
  				    urlKontakt[i] <- url
  		      }
  				#print(paste(url, "3", sep=" - "))
  		}
  		if(checkURL(urlKontakt[i]) == FALSE){
  		  if(substring(urlKontakt[i], 1, 1) == "/"){
  		    urlKontakt[i] = paste(url,urlKontakt[i], sep="")
  		  } else {
  		    urlKontakt[i]= paste(url,"/", urlKontakt[i], sep="")		    
  		  }
  		}
  	  print(paste(i, urlKontakt[i]))
  		html <- ""
  	
  		try({
  			html <- getHTML(urlKontakt[i])	
  			dane <- xpathSApply(html, "//div", xmlValue)
  			if(length(dane) == 0) {
  			  dane <- xpathSApply(html, "//p", xmlValue)
  			} else {
  			  dane <- c(dane, xpathSApply(html, "//p", xmlValue))
  			}
  			
  			if(length(dane) == 0) {
  			   dane <- xpathSApply(html, "//body", xmlValue)
  			}
  			
  			#print(html)
  			}, silent=T)	
  		#print(paste(i, "html+dane ok"))
  		
  		if(danecsv[i, "czy_telefon"] == 0){ 
    		if(length(grep(pattern = "T|tel(\\.|:|[:space:])", dane)) > 0){
    		  danetel <- dane[grep(pattern = "T|tel(\\.|:|[:space:])", dane)]
    		  j <- 1
    		  while(tel[i] == 0 && j <= length(danetel)) {
    		    textG[i] <- try(clearText(danetel[j]), silent=T)
    		    textG[i] <- try(clearKierunkowy(textG[i]), silent=T)
    		    tel[i] <- parsePhone(textG[i]) 
    		    
    		    if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
    		      tel[i] <- 0
    		      }
    		    j <- j + 1
    		  }
    		} 
    		#print(paste(i, "tel 'tel'"))
    		
    		if(tel[i] == "0") {
    		  if(length(grep(pattern = "\\+48", dane)) > 0){
    		    #print ("tak")
    		    danetel <- dane[grep(pattern = "\\+48", dane)]
    		    j <- 1
    		    while(tel[i] == "0" && j <= length(danetel)) {
    		      textG[i] <- try(clearText(danetel[j]), silent=T)
    		      #text[i] <- try(clearKierunkowy(text[i]), silent=T)
    		      tel[i] <- parsePhone(textG[i]) 
    		      
    		      if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
    		        tel[i] <- 0
    		      }
    		      j <- j + 1
    		    }
    		  }
    		}
    		#print(paste(i, "tel '+48'"))
    		
    		if(tel[i] == "0") {
    		  if(length(grep(pattern = "Z|zadzwoń", dane)) > 0){
    		    #print ("tak")
    		    danetel <- dane[grep(pattern = "Z|zadzwoń", dane)]
    		    j <- 1
    		    while(tel[i] == "0" && j <= length(danetel)) {
    		      textG[i] <- try(clearText(danetel[j]), silent=T)
    		      #text[i] <- try(clearKierunkowy(text[i]), silent=T)
    		      tel[i] <- parsePhone(textG[i]) 
    		      
    		      if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
    		        tel[i] <- "0"
    		      }
    		      j <- j + 1
    		    }
    		  }
    		}		
    		#print(paste(i, "tel 'zadzwoń'"))
    		
    		if(tel[i] == "0") {
    		    tel[i] <- parsePhoneAttribute(html)
    		    if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
    		      tel[i] <- "0"
    		    }
    		}
    		#print(paste(i, "tel 'attribute'"))
    		
    		#tel GSM
    		if(tel[i] == "0") {
    		  imin <- 0
    		  if(length(grep(pattern = "[0-9]{3}[ |-][0-9]{3}[ |-][0-9]{3}", dane)) > 0){
    		    imin <- max(grep(pattern = "[0-9]{3}[ |-][0-9]{3}[ |-][0-9]{3}", dane))
    		  } else {
    		    if(length(grep(pattern = "[0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", dane)) > 0){
    		      imin <- max(grep(pattern = "[0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", dane))		    
    		      }
    		  }
    		  if(imin > 0){
    		    tel[i] <- parsePhoneGSM(dane[imin])
    		  }
    		  if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
    		    tel[i] <- "0"
    		  }
    		}		
    		#print(paste(i, "tel 'GSM'"))
    		
    		#Stacjonarny
    		if(tel[i] == "0") {
    		  imin <- 0
    		  if(length(grep(pattern = "[0-9]{2}[ |)][0-9]{7}", dane)) > 0){
    		    imin <- max(grep(pattern = "[0-9]{2}[ |)][0-9]{7}", dane))  
    		  } else {
    		    if(length(grep(pattern = "[0-9]{2}[ |)][0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}", dane)) > 0){
    		      imin <- max(grep(pattern = "[0-9]{2}[ |)][0-9]{3}[ |-][0-9]{2}[ |-][0-9]{2}", dane)) 
    		    } else {
    		      if(length(grep(pattern = "[0-9]{2}[ |)][0-9][ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", dane)) > 0){
    		        imin <- max(grep(pattern = "[0-9]{2}[ |)][0-9][ |-][0-9]{2}[ |-][0-9]{2}[ |-][0-9]{2}", dane)) 		        
    		      }
    		    }
    		  }
    		  if(imin > 0) {
    		      tel[i] <- parsePhoneStationary(dane[imin])
    		  }
    		  if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
    		    tel[i] <- "0"
    		  }
    		}
    		#print(paste(i, "tel 'Stationary'"))
    		
    # #href tel	
     		if(tel[i]=="0"){
     		  if(length(grep(pattern = "tel:", links_v2)) > 0){
     		    tel[i] <- links_v2[[min(grep(pattern = "tel:", links_v2))]]
     		    #tel[i] <- links_v2[[min(grep(pattern="tel:", xpathSApply(html,"//a", xmlGetAttr, "href")))]]
     		    tel[i] <- substring(tel[i], nchar(tel[i])-8, nchar(tel[i]))
     		  }
     		  
     		  if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
     		    tel[i] <- "0"
     		  }	
  		  #print(paste(i, "tel 'href'"))
     		}
  			#print(paste(i, tel[i]))
  		}	
  		
  # NIP
  			if(length(grep(pattern = "(N|n)(I|i)(P|p)", dane)) > 0){
  				mN <- max(grep(pattern = "(N|n)(I|i)(P|p)", dane))
  				if(mN > 0) { 
  			  	textN[i] <- try(clearText(dane[mN], typ="NIP"), silent=T)
  			  
  			  	if(length(grep(pattern = "NIP", textN[i])) > 0){
  			  		nip[i] <- substring(textN[i], regexpr('NIP', textN[i])+3, regexpr('NIP', textN[i])+12)	
  			  		} else {
  			  			if(length(grep(pattern = "nip", textN[i])) > 0){
  			  			nip[i] <- substring(textN[i], regexpr('nip', textN[i])+3, regexpr('nip', textN[i])+12)	
  			  				} 
  			  		
  			 			}
  				}
  			}			
  			#print(paste(i, "NIP"))
    		if(length(grep(pattern = "[0-9]{10}", nip[i])) == 0 || nchar(nip[i]) != 10){
    		  nip[i] <- 0
    		}
  
  # KRS
  		if(length(grep(pattern = "(K|k)(R|r)(S|s)", dane)) > 0){
  		  mK <- max(grep(pattern = "(K|k)(R|r)(S|s)", dane))
  		  if(mK > 0) { 
  		    textK[i] <- try(clearText(dane[mK], typ="NIP"), silent=T)
  		    
  		    if(length(grep(pattern = "KRS", textK[i])) > 0){
  		      krs[i] <- substring(textK[i], regexpr('KRS', textK[i])+3, regexpr('KRS', textK[i])+12)	
  		    } else {
  		      if(length(grep(pattern = "krs", textK[i])) > 0){
  		        krs[i] <- substring(textK[i], regexpr('krs', textK[i])+3, regexpr('krs', textK[i])+12)	
  		      } 
  		      
  		    }
  		  }
  		}			
  		#print(paste(i, "KRS"))
  		if(length(grep(pattern = "[0-9]{10}", krs[i])) == 0 || nchar(krs[i]) != 10){
  		  krs[i] <- 0
  		}		 
  		
  		  
        if(nip[i] == "0") {
          regulamin <-links[grep(pattern = "(R|r)egulamin", links)]
          
          if(length(regulamin) == 0){
            regulamin <-links[grep(pattern = "(C|c)onditions", links)]  
          } 
          if(length(regulamin) == 0){
            regulamin <-links[grep(pattern = "(O|o)firmie", links)]  
          }
          
          if(length(regulamin) > 0){
            if(checkURL(regulamin)){
              urlRegulamin[i] <- regulamin
            } else {
              urlRegulamin[i] <- paste(url,regulamin,sep="")
              if(!(checkURL(urlRegulamin[i]))){
                urlRegulamin[i] <- paste(url,"/",regulamin,sep="")		
              }
            }
          }
          
          daneR <- ""
          
          try({
            htmlR <- getHTML(urlRegulamin[i])	
            daneR <- xpathSApply(htmlR, "//div", xmlValue)
          }, silent=T)	
          
          if(length(grep(pattern = "(N|n)(I|i)(P|p)", daneR)) > 0){
            mN <- max(grep(pattern = "(N|n)(I|i)(P|p)", daneR))
            if(mN > 0) { 
              textN[i] <- try(clearText(daneR[mN], typ="NIP"), silent=T)
              
              if(length(grep(pattern = "NIP", textN[i])) > 0){
                nip[i] <- substring(textN[i], regexpr('NIP', textN[i])+3, regexpr('NIP', textN[i])+12)	
              } else {
                if(length(grep(pattern = "nip", textN[i])) > 0){
                  nip[i] <- substring(textN[i], regexpr('nip', textN[i])+3, regexpr('nip', textN[i])+12)	
                } 
                
              }
            }
          }		
        }			
  #E-MAIL	
        if(danecsv[i, "czy_mail"] == 0){  		
    			if(length(grep(pattern = "(E|e)?-?(M|m)(A|a)(I|i)(L|l)", dane)) > 0){
    				mE <- max(grep(pattern = "(E|e)?-?(M|m)(A|a)(I|i)(L|l)", dane))
    				if(mE > 0) { 
    			  	textE[i] <- try(clearText(dane[mE], typ="MAIL"), silent=T)
    			  	mail[i] <- substring(parseMail(textE[i]), 1, 256)
    				}
    			}
    			#print(paste(i, "MAIL"))
        }
  
  #PNA		
  			if(length(grep(pattern = "[0-9][0-9]-[0-9][0-9][0-9]", dane)) > 0){
  			  mP <- max(grep(pattern = "[0-9][0-9]-[0-9][0-9][0-9]", dane))
  			  if(mP > 0) { 
  			    textP[i] <- try(dane[mP], silent=T)
  			    pna[i] <- substring(textP[i], regexpr("[0-9][0-9]-[0-9][0-9][0-9]", textP[i]),
  			                        regexpr("[0-9][0-9]-[0-9][0-9][0-9]", textP[i])+5)            
  			  }
  			}
  			#print(paste(i, "PNA"))			
  
  #ULICA
  		if(length(grep(pattern = "ul.", dane)) > 0){	
  		  mU<- max(grep(pattern = "ul.", dane))
  		  if(mU > 0){
  		    textU[i] <- try(dane[mU], silent=T)  
  		    ulica[i] <- try(substring(textU[i], regexpr("ul.", textU[i], fixed = TRUE)+3, nchar(textU[i])), silent=TRUE)
  		    ulica[i] <- substring(ulica[i], 1, regexpr("[0-9]", ulica[i]) - 1)
  		    
  		    if(nchar(ulica[i]) > 50){
  		      ulica[i] <- ""
  		    } else {
  		      ulica[i] <- gsub("\r", "", ulica[i], fixed = TRUE)	
  		      ulica[i] <- gsub("\n", "", ulica[i], fixed = TRUE)		
  		      ulica[i] <- gsub("\t", "", ulica[i], fixed = TRUE)
  		      
  		      if(substring(ulica[i], 1, 1) == " ")
  		        ulica[i] <- substring(ulica[i], 2, nchar(ulica[i]))
  		      if(substring(ulica[i], nchar(ulica[i]), nchar(ulica[i])) == " ")
  		        ulica[i] <- substring(ulica[i], 1, nchar(ulica[i]) - 1)	
  		    }
  		  }
  		}
  		
  #WEB BY ENIRO	
  			if(length(grep(pattern = "Website by Eniro Polska", dane)) > 0) {
  				eniro[i] <- 1
  			} else {
  				if(length(grep(pattern = "Stwórz własną stronę www z Panoramą Firm", dane)) > 0){
  					eniro[i] <- 1
  				} else {
  					eniro[i] <- 0
  				}
  			}
  			
  #SUNRISE 
  			if(length(grep(pattern = "Za pozycjonowanie tego serwisu odpowiada Sunrise System", dane)) > 0) {
  			  sunrise[i] <- 1
  			} else {
  			  sunrise[i] <- 0
  			  } 
  
  #RZETELNA FIRMA 
  			if(length(grep(pattern = "wizytowka.rzetelnafirma.pl", dane)) > 0) {
  			  rzetelna[i] <- 1
  			} else {
  			  rzetelna[i] <- 0
  			} 
  						
  
  #KOSZYK
  		  dkoszyk <- c(0,0,0,0,0,0,0,0)
  		  dmkoszyk <- c(0,0,0,0,0,0,0,0)
  		  for(ik in 1:length(fkoszyk)){
  		    dkoszyk[ik] <- length(grep(pattern = fkoszyk[ik], danemain)) 
  		    dmkoszyk[ik] <- length(grep(pattern = fkoszyk[ik], danem)) 
  		  }
  		  
  		  #print(max(dkoszyk))
  		  #print(max(dmkoszyk))
  		  
  		  if(max(dkoszyk) > 0) {
  		    koszyk[i] <- 1
  		  } else {
  		    if(max(dmkoszyk) > 0) {
  		      koszyk[i] <- 1
  		    } else {
  		      if(length(grep(pattern = "koszyk", danemain)) > 0) {
  		        koszyk[i] <- 1
  		      } else {
  		        koszyk[i] <- 0
  		      }
  		    }
  		  }
        
  		  if(koszyk[i] > 0){
  		    dkniekoszyk <- c(0,0,0,0,0,0)
  		    dmniekoszyk <- c(0,0,0,0,0,0)
  		    for(ik in 1:length(fniekoszyk)){
  		      dkniekoszyk[ik] <- length(grep(pattern = fniekoszyk[ik], danemain)) 
  		      dmniekoszyk[ik] <- length(grep(pattern = fniekoszyk[ik], danem)) 
  		    }
  		    if(max(dkniekoszyk) + max(dmniekoszyk) > 0){
  		      koszyk[i] <- 0
  		    }
  		  }
  		  
  # PAGE TITLE
  		  if(length(daneTitle) == 0) {
  		    daneTitle <- ""
  		  }		
  		  
  		  daneTitle[1] <- gsub("\r?\n|\r", " ", daneTitle[1])
  		  try(textTitle[i] <- substring(daneTitle[1], 1, 255), silent = TRUE)
  		  textTitle[i] <- gsub("\t", "", textTitle[i])
  		  textTitle[i] <- gsub("\n", "", textTitle[i])
  		  textTitle[i] <- gsub("\r", "", textTitle[i])		  
  		  textTitle[i] <- gsub('"', '', textTitle[i])
    		
  #SOCIAL MEDIA / e-Commerce Accounts
  		  soclinks <- getSocialMediaLinks(links_v2)
  		  textFB[i] <- substring(soclinks[1], 1, 255)	
  		  textInsta[i] <- substring(soclinks[2], 1, 255)	
  		  textLinked[i] <- substring(soclinks[3], 1, 255)	
  		  textAllegro[i] <- substring(soclinks[4], 1, 255)	
  		  textOtomoto[i] <- substring(soclinks[5], 1, 255)	
  		  
# FACEBOOK
#  		    if(length(facebook) == 0) {
#  		      facebook <- ""
#  		    }
  		  
#          textFB[i] <- substring(facebook[1], 1, 255)		  
  		    
  #		}
  	}
  	if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
  		tel[i] <- 0
  	}
  
  	if(length(grep(pattern = "[0-9]{10}", nip[i])) == 0 || nchar(nip[i]) != 10){
  	  nip[i] <- 0
  	}
  	
  	if(length(grep(pattern = "@", mail[i])) == 0){
  	  if(length(grep(pattern = "@", links)) > 0){
  	    mail[i] <- links[min(grep(pattern = "@", links))]
  	  } else {
  		  mail[i] <- ""
  	  }
  	} 
  	
  	if(length(grep(pattern = "<script", mail[i])) > 0){
  	  mail[i] <- ""
  	}
  	
  	mail[i] <- gsub('\"','', mail[i], fixed = TRUE)
  	mail[i] <- gsub(',','', mail[i], fixed = TRUE)
  	mail[i] <- gsub("\t", "", mail[i])
  	mail[i] <- gsub("[\r\n]", "", mail[i])
  	mail[i] <- gsub('"', '', mail[i])
  	
  	wersja[i] <- ver
  	#Data
  	dataCrawling[i] <- as.character(Sys.Date())	
  	if(i %% 50 == 0) {
  				tabelaOut <- data.frame(id_domena = head(danecsv["id_domena"], lmax)
  				        , domena = head(danecsv["domena"], lmax)
  								, czy_dziala = urlWorks
  								, telefon = tel
  								, nip = nip
  								, krs = krs
  								, webEniro = eniro
  								, PNA = pna
  								, version = wersja
  								, sunrise = sunrise
  								, rzetelna = rzetelna
  								, koszyk = koszyk
  								, data = dataCrawling
  					      , facebook = textFB
  								, instagram = textInsta
  								, linkedin = textLinked
  							  , allegro = textAllegro	
  								, otomoto = textOtomoto
  								, title = textTitle	
  								, mail = mail
  								, ulica = ulica								
  								)		
  								
  		write.csv(tabelaOut, "domeny_out.csv", fileEncoding = "UTF-8")
  		
  		lnks <- unique (grep(links2match, allLinks, value=TRUE))
  		linksOut <- data.frame(url2 <- lnks)
  		write.csv(linksOut, "linki.csv", fileEncoding = "UTF-8")
  	}
  print(paste(i, "koniec"))		
  }			
  
  #danecsv2 <- read.csv("domeny_out.csv")
  #n <- rbind(danecsv2[ , !(names(danecsv2) %in% 'X')], tabelaOut)			 
  		tabelaOut <- data.frame(id_domena = head(danecsv["id_domena"], lmax)
  		            , domena = head(danecsv["domena"], lmax)
  								, czy_dziala = urlWorks
  								, telefon = tel
  								, nip = nip
  								, krs = krs
  								, webEniro = eniro
  								, PNA = pna
  								, version = wersja					
  								, sunrise = sunrise
  								, rzetelna = rzetelna	
  								, koszyk = koszyk
  								, data = dataCrawling
  								, facebook = textFB
  								, instagram = textInsta
  								, linkedin = textLinked
  								, allegro = textAllegro	
  								, otomoto = textOtomoto				
  								, title = textTitle								
  								, mail = mail		
  								, ulica = ulica									
  								)		
  								
  		write.csv(tabelaOut, "domeny_out.csv", fileEncoding = "UTF-8")
  		
  		lnks <- unique (grep(links2match, allLinks, value=TRUE))
  		linksOut <- data.frame(url2 <- lnks)
  		write.csv(linksOut, "linki.csv", fileEncoding = "UTF-8")
  
