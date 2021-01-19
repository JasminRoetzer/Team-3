


library(readxl)  ##load Excel Data
Umsatzdaten_Excel <- read_excel("raw_data/Umsatzdaten_Excel.xlsx")

Wetter_Excel <- read_excel("raw_data/Wetter_Excel.xlsx")

Schiffsverkehr <- read_excel("raw_data/Schiffsverkehr.xlsx")

Uebernachtungen_Kiel_Mai_August_2019_new <- read_excel("raw_data/Uebernachtungen_Kiel_Mai-August_2019_new.xlsx")

Besucher_Kiwo <- read_excel("raw_data/Besucher_Kiwo.xlsx")

kiwo_Excel <- read_excel("raw_data/kiwo_Excel.xlsx")


## merging

m1 <- merge(x=Umsatzdaten_Excel,y= Wetter_Excel,by= "Datum", all.x=TRUE) ## merge Umsatzdaten mit Wetter

m2 <- merge(x=m1, y= Schiffsverkehr, by="Datum", all.x = TRUE) ## merge m1 mit Schiffsverkehr

m3 <- merge(x=m2, y= Uebernachtungen_Kiel_Mai_August_2019_new, all.x = TRUE) ## merge m2 mit Übernachtungen

m4 <- full_join(x=m3, y= Besucher_Kiwo) ## merge m3 mit Besucherzahlen

m5 <- full_join(x= m4, y= kiwo_Excel) ## merge m4 mit Kiwo


library(plyr)
m_final <- ddply(m5,"Datum",numcolwise(sum)) ## merge Datumsangaben


for(i in 1:ncol(m_final)){
  m_final[is.na(m_final[,i]), i] <- mean(m_final[,i], na.rm = TRUE)
  
}
