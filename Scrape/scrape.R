
## Load necessary packages
library("dplyr")
library("rvest")

## The Function
# Month should be two digits (03 or 11)
# Year defaults to 2020
# Pages defaults to one. Max 2 or 3, otherwise running time increases dramatically
getTitle <- function (month,year=2020,pages=1){
  webpagelist <- list()
  daylist <- c('01','02','03','04','05','06','07',
               '08','09','10','11','12','13','14',
               '15','16','17','18','19','20','21',
               '22','23','24','25','26','27','28',
               '29')
  # daylist <- c('30','31') #range of days
  for (i in daylist) { 
    for (t in 1:pages) { #number of pages 
      halaman <- sprintf("https://indeks.kompas.com/?site=nasional&date=%s-%s-%s&page=%s",year,month,i,t) #for March 2020, manually change to other months. now irrelevant
      webpagelist[[length(webpagelist) + 1]] <- halaman
    }
  }
  readTitle <- function(webpage){
    webpage <- as.character(webpage)
    kompas <- read_html(webpage)
    judul <- kompas %>% html_nodes(".article__link")%>% html_text()
  }
  titlelist <- list()
  date <- list()
  for (r in 1:length(webpagelist)) {
    judul <- readTitle(webpagelist[[r]])
    for (q in 1:15) { # because every page is 15 results
      titlelist[[length(titlelist)+1]] <- judul[q]
      date[[length(date)+1]] <- webpagelist[[r]]
    }
  }
  hasil <- do.call(rbind, Map(data.frame, alamat=date, judul=titlelist)) #https://stackoverflow.com/questions/28630024/combine-two-lists-in-a-dataframe-in-r
  return(hasil)
}

## The procedure. Change this part as necessary
hasiljan2020 <- getTitle(01)
write.csv(hasiljan2020,'hasiljan2020.csv')

## update, tambahin tgl 30-31
# mei, juli, desember
hasil3031mei2020 <- getTitle(05,pages=5)
write.csv(hasil3031mei2020,'hasil3031mei2020.csv')

hasil3031juli2020 <- getTitle(07,pages=5)
write.csv(hasil3031juli2020,'hasil3031juli2020.csv')

hasil3031des2020 <- getTitle(12,pages=5)
write.csv(hasil3031des2020,'hasil3031des2020.csv')

hasil3031agu2020 <- getTitle(08,pages=5)
write.csv(hasil3031agu2020,'hasil3031agu2020.csv')
