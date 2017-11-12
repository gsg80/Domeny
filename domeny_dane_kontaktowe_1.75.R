library(XML)
library(RCurl)

setwd("/Users/gsg/R Projects/domeny")

f = "domeny.csv" #link do pliku
ver <- "1.75"

allLinks <- ""
links2match <- c(".pl",".eu",".net",".biz",".com")

#funkcja wyszukujaca telefon w tekście
parsePhone <- function(text) {
	ptel <- ""
	fraza <- ""
	
	# flist <- c('tel','Tel','TelKom','telkom','TelKomórkowy','telkomórkowy','telefon','Telefon','telfax','TELFAX','komórka','Komórka','+48')
	# fmatch <- vector(mode="numeric", length=length(flist))
	# for(i in 1: length(fmatch)) {
	#   fmatch[i] <- length(grep(pattern = flist[i], text))
	#   }
	# 
	# print(fmatch)
	
	if(length(grep(pattern = "telkom", text)) > 0){
		if(length(grep(pattern = "telkomórkowy", text)) > 0){
		  fraza <- "telkomórkowy"
		} else {
	    fraza <- "telkom"
	  }
	} else {
	  if(length(grep(pattern = "TelKom", text)) > 0){
	    if(length(grep(pattern = "TelKomórkowy", text)) > 0){	
	      fraza <- "TelKomórkowy"	      
	    } else {
	      fraza <- "TelKom"	 	      
	    }
	  } else {
	    if(length(grep(pattern = "Telkom", text)) > 0){
	      if(length(grep(pattern = "Telkomórkowy", text)) > 0){	
	        fraza <- "Telkomórkowy"	      
	      } else {
	        fraza <- "Telkom"	 	      
	      }
	    } else {
	    if(length(grep(pattern = "telfax", text)) > 0){
	      fraza <- "telfax"		
	    }
	    else {
	      if(length(grep(pattern = "tel", text)) > 0){
	        fraza <- "tel"			
	      }
	      else {
	        if(length(grep(pattern = "Tel", text)) > 0){
	          fraza <- "Tel"		
	        } else {
	          if(length(grep(pattern = "\\+48", text)) > 0) {
	            fraza <- "+48"
	          }
	        }	
	      }
	    }
	    }
	  }
	}
	dl <- nchar(fraza)
	z <- 0
	
	zer <- substring(text, regexpr(fraza, text)+dl, regexpr(fraza, text)+dl)
	#print(zer)
	if( zer == "0"){
	  z <- 1
	}
	
	if(fraza == "+48")  {
	  dl <- dl - 1
	} 
	  
	ptel <- substring(text, regexpr(fraza, text)+dl+z, regexpr(fraza, text)+dl+z+8)
	
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

	ctext <- gsub(" ","", ctext, fixed = TRUE)
	ctext <- gsub("(","", ctext, fixed = TRUE)
	ctext <- gsub(")","", ctext, fixed = TRUE)
	ctext <- gsub("\u00A0", "", ctext, fixed = TRUE)	
	ctext <- gsub("\r", "", ctext, fixed = TRUE)	
	ctext <- gsub("\n", "", ctext, fixed = TRUE)		
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
	#setTimeLimit(elapsed=60, transient=TRUE)
	e <- url.exists(url, .opts = list(timeout = 60, maxredirs = 2, verbose = FALSE))
	#print("checkURL ok")
	return(e)
}

#funkcja ograniczajace czas pobrania linków
getLinks <- function(url) {
	ln <- ""
	#setTimeLimit(elapsed=120, transient=TRUE)
	ln <- getHTMLLinks(url)
	#print("getLinks ok")
	return(ln)
}

#funkcja ograniczająca czas pobrania htmla
getHTML <- function(url){
	h <- ""
  #setTimeLimit(elapsed=120, transient=TRUE)
	h <- htmlParse(url)
	#print('getHTML')
	return(h)	
	
}

danecsv <- read.csv2(f)

#definicja wektorów z danymi zwrotnymi
urlWorks <- vector(mode="numeric", length=length(danecsv[, "domena"]))
urlKontakt <- vector(mode="character", length=length(danecsv[, "domena"]))
urlRegulamin <- vector(mode="character", length=length(danecsv[, "domena"]))

text <- vector(mode="character", length=length(danecsv[, "domena"]))
textN <- vector(mode="character", length=length(danecsv[, "domena"]))
textE <- vector(mode="character", length=length(danecsv[, "domena"]))
textP <- vector(mode="character", length=length(danecsv[, "domena"]))
textTitle <- vector(mode="character", length=length(danecsv[, "domena"]))

tel <- vector(mode="numeric", length=length(danecsv[, "domena"]))
eniro <- vector(mode="numeric", length=length(danecsv[,"domena"]))
nip <-vector(mode="numeric", length=length(danecsv[,"domena"]))
mail <-vector(mode="numeric", length=length(danecsv[,"domena"]))
pna <-vector(mode="numeric", length=length(danecsv[,"domena"]))
wersja <-vector(mode="numeric", length=length(danecsv[,"domena"]))
sunrise <-vector(mode="numeric", length=length(danecsv[,"domena"]))
rzetelna <- vector(mode="numeric", length=length(danecsv[,"domena"]))
koszyk <- vector(mode="numeric", length=length(danecsv[,"domena"]))
dataCrawling <- vector(mode="numeric", length=length(danecsv[,"domena"]))

fkoszyk <- c('Koszyk','Basket','koszyk','basket','sklep internetowy','Sklep internetowy','internetowy sklep','księgarnia internetowa','ksiêgarnia internetowa','Ksiêgarnia internetowa')

for(i in 1 : length(danecsv[, "domena"]))
{
	#print(paste(i, "start"))
	m <- 0
	mN <- 0
	mE <- 0
	mP <- 0
	opcja <- 0
	dane <- ""
	danem <- ""
	danemain <- ""
	daneTitle <- ""
	danep <- ""
	html <- ""
	links <- ""
	dkoszyk <- c(0,0,0,0,0,0,0,0)
	dmkoszyk <- c(0,0,0,0,0,0,0,0)
	
	url <- paste ("http://", danecsv[i,"domena"], sep="")
	print(paste(i, url))
	
	if(checkURL(url)){
		#print(paste(i, "url OK"))
		urlWorks[i] <- 1
		try(links <- getLinks(url), silent=T)
		#print(paste(i, "links"))
		allLinks <- c(allLinks, links)
		kontakt <-links[grep(pattern = "kontakt", links)]
		kontakt <- grep(pattern = "mailto", kontakt, invert=TRUE)		
		
		try({html <- getHTML(url)
		daneTitle <- xpathSApply(html, "//head/title", xmlValue)  
		#Encoding(daneTitle) <- "UTF-8"
		danem <- xpathSApply(html, "//meta", xmlGetAttr, 'content') 
		danemain <- xpathSApply(html, "//div", xmlValue)
		}, silent=T)
  
		
		html <- ""
		#print(paste(i, kontakt[1]))
		
		if(length(kontakt) > 0){
			if(checkURL(links[min(grep(pattern = "kontakt", links))])){
				urlKontakt[i] <- links[min(grep(pattern = "kontakt", links))]
				#print(paste(url, "1", sep=" - "))	
				}
			else{
				urlKontakt[i] <- paste(url,links[min(grep(pattern = "kontakt", links))],sep="")
				if(!(checkURL(urlKontakt[i]))){
					urlKontakt[i] <- paste(url,"/",links[min(grep(pattern = "kontakt", links))],sep="")		
				} 
				#print(paste(url, "2", sep=" - "))
			}
		} 
		else{
				urlKontakt[i] <- url
				#print(paste(url, "3", sep=" - "))
			}
		#print(paste(i, "urlKontakt ok"))

		try({
			html <- getHTML(urlKontakt[i])	
			dane <- xpathSApply(html, "//div", xmlValue)
			
			#print(html)
			}, silent=T)	
		#print(paste(i, "html+dane ok"))
		
		
		if(length(grep(pattern = "T|tel(\\.|:|[:space:])", dane)) > 0){
		  danetel <- dane[grep(pattern = "T|tel(\\.|:|[:space:])", dane)]
		  j <- 1
		  while(tel[i] == 0 && j <= length(danetel)) {
		    text[i] <- try(clearText(danetel[j]), silent=T)
		    text[i] <- try(clearKierunkowy(text[i]), silent=T)
		    tel[i] <- parsePhone(text[i]) 
		    
		    if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
		      tel[i] <- 0
		      }
		    j <- j + 1
		  }
		} 
		
		if(tel[i] == "0")
		{
		  if(length(grep(pattern = "\\+48", dane)) > 0){
		    #print ("tak")
		    danetel <- dane[grep(pattern = "\\+48", dane)]
		    j <- 1
		    while(tel[i] == "0" && j <= length(danetel)) {
		      text[i] <- try(clearText(danetel[j]), silent=T)
		      #text[i] <- try(clearKierunkowy(text[i]), silent=T)
		      tel[i] <- parsePhone(text[i]) 
		      
		      if(length(grep(pattern = "[0-9]{9}", tel[i])) == 0 || nchar(tel[i]) != 9){
		        tel[i] <- 0
		      }
		      j <- j + 1
		    }
		  }
		}
		

			#print(paste(i, "Phone"))
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
          }
          else{
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
			if(length(grep(pattern = "(E|e)?-?(M|m)(A|a)(I|i)(L|l)", dane)) > 0){
				mE <- max(grep(pattern = "(E|e)?-?(M|m)(A|a)(I|i)(L|l)", dane))
				if(mE > 0) { 
			  	textE[i] <- try(clearText(dane[mE], typ="MAIL"), silent=T)
			  	mail[i] <- parseMail(textE[i])
				}
			}
			#print(paste(i, "MAIL"))

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
		  for(ik in 1:length(fkoszyk))
		  {
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

# PAGE TITLE
		  if(length(daneTitle) == 0) {
		    daneTitle <- ""
		  }		
		  
		  daneTitle[1] <- gsub("\r?\n|\r", " ", daneTitle[1])
		  textTitle[i] <- daneTitle[1]
		  textTitle[i] <- gsub("\t", "", textTitle[i])
		  textTitle[i] <- gsub("\n", "", textTitle[i])
		  textTitle[i] <- gsub('"', '', textTitle[i])
		  
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
	if(i %% 10 == 0) {
				tabelaOut <- data.frame(id_domena = danecsv["id_domena"]
								, domena = danecsv["domena"]
								, czy_dziala = urlWorks
								, telefon = tel
								, nip = nip
								, webEniro = eniro
								, PNA = pna
								, version = wersja
								, sunrise = sunrise
								, rzetelna = rzetelna
								, koszyk = koszyk
								, data = dataCrawling
								, title = textTitle		
								, mail = mail
								)		
								
		write.csv(tabelaOut, "domeny_out.csv")
		
		lnks <- unique (grep(links2match, allLinks, value=TRUE))
		linksOut <- data.frame(url <- lnks)
		write.csv(linksOut, "linki.csv")
	}
print(paste(i, "koniec"))		
}			

#danecsv2 <- read.csv("domeny_out.csv")
#n <- rbind(danecsv2[ , !(names(danecsv2) %in% 'X')], tabelaOut)			 
		tabelaOut <- data.frame(id_domena = danecsv["id_domena"]
								, domena = danecsv["domena"]
								, czy_dziala = urlWorks
								, telefon = tel
								, nip = nip
								, webEniro = eniro
								, PNA = pna
								, version = wersja					
								, sunrise = sunrise
								, rzetelna = rzetelna	
								, koszyk = koszyk
								, data = dataCrawling
								, title = textTitle								
								, mail = mail								
								)		
								
		write.csv(tabelaOut, "domeny_out.csv")
		
		lnks <- unique (grep(links2match, allLinks, value=TRUE))
		linksOut <- data.frame(url <- lnks)
		write.csv(linksOut, "linki.csv")

