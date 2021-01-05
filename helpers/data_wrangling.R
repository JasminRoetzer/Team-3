

# .............................................................................. ----  
# style [Style Regeln, damit unser Code gleich aussieht und von allen gelesen werden kann] ---- 

# First of all some style rules. Headers get # and a line # ...... ---- above
# it, Subheaders get ##, subsubheaders get ###. All headers get ---- at the end
# so that they are foldable. Collapse all with Alt+o to see its beauty
# (Alt+Shift+o to expand all afterwards) Regular comments that are not headers
# get a single #

## about spacing

#bad
# good


## Two blank lines before each header you can fold and one afterwards ----

# one blank line after each header. No blank lines after a normal comment
bad<-"bad"
good <- "good"

# one blank before each comment.


# .............................................................................. ----
# load packages
if(!require("tidyverse")){install.packages("tidyverse")}; library("tidyverse")
if(!require("ggplot2")){install.packages("ggplot2")}; library("ggplot2") # plots
if(!require("lubridate")){install.packages("lubridate")}; library("lubridate") # Umgang mit Datum
if(!require("readr")){install.packages("readr")}; library("readr") # Einlesen von Daten
if(!require("dplyr")){install.packages("dplyr")}; library("dplyr") # Einlesen von 
if(!require("e1071")){install.packages("e1071")}; library("e1071") # Support Vektor Machines
if(!require("Metrics")){install.packages("Metrics")}; library("Metrics") # Support Vektor Machines


# .............................................................................. ----
# Daten einlesen ----
umsatzdaten <- read_csv("raw_data/umsatzdaten_gekuerzt.csv")
kiwo <- read_csv("raw_data/kiwo.csv")
wetter <- read_csv("raw_data/wetter.csv")


# .............................................................................. ----
# Variable Umsatz am Vortag einfügen ----
# Umsatz des Vortages zufügen, hier habe ich erst versucht eine Funktion zu bauen. Das ging aber aufgrund von mehrfach Variablen Bezug und anderem Kram immer wieder schief. Jetzt bruteforce ich es.


## Vorbereitung ----
# umsatzdaten in anderer Variable zum Bearbeiten speichern
data <- umsatzdaten

# Leere Zeile hinzufügen
data$umsatz_day_before <- rep(NA, length(data$Datum))


# .............................................................................. ----
## Liste mit sub Datensätzen erstellen ---- 
# nach bestimmter Sparte gruppieren, in unserem Fall Warengruppe
data_grouped <- data %>% group_by(Warengruppe ) 

# Zählen wieviele Gruppen exisiteren
groupnumber <- as.numeric(length(count(data_grouped)$Warengruppe))

# Liste zum abspeichern der Dataframes
data_list <- vector(mode = "list", 0)

for (i in 1:groupnumber) {
 # Erstelle Datensatz für Warengruppe und speichere diesen
  data_list <- c(data_list, list(subset(data, Warengruppe == i )))
}

# Namen zu Liste hinzufügen
names(data_list) <- c("data_1", "data_2", "data_3", "data_4", "data_5", "data_6")


# .............................................................................. ----
## Umsatz beim Vortag als Variable beifügen ----

# Für alle sechs Datensätze
for (j in 2: as.numeric(length(data_list$data_1$Umsatz))){
  if(length(data_list$data_1$Umsatz[data_list$data_1$Datum == data_list$data_1$Datum[j] - 1 ])>0)
    data_list$data_1$umsatz_day_before[j] <- data_list$data_1$Umsatz[data_list$data_1$Datum == data_list$data_1$Datum[j] - 1 ]
}

for (j in 2: as.numeric(length(data_list$data_2$Umsatz))){
  if(length(data_list$data_2$Umsatz[data_list$data_2$Datum == data_list$data_2$Datum[j] - 1 ])>0)
    data_list$data_2$umsatz_day_before[j] <- data_list$data_2$Umsatz[data_list$data_2$Datum == data_list$data_2$Datum[j] - 1 ]
}


for (j in 2: as.numeric(length(data_list$data_3$Umsatz))){
  if(length(data_list$data_3$Umsatz[data_list$data_3$Datum == data_list$data_3$Datum[j] - 1 ])>0)
    data_list$data_3$umsatz_day_before[j] <- data_list$data_3$Umsatz[data_list$data_3$Datum == data_list$data_3$Datum[j] - 1 ]
}


for (j in 2: as.numeric(length(data_list$data_4$Umsatz))){
  if(length(data_list$data_4$Umsatz[data_list$data_4$Datum == data_list$data_4$Datum[j] - 1 ])>0)
    data_list$data_4$umsatz_day_before[j] <- data_list$data_4$Umsatz[data_list$data_4$Datum == data_list$data_4$Datum[j] - 1 ]
}


for (j in 2: as.numeric(length(data_list$data_5$Umsatz))){
  if(length(data_list$data_5$Umsatz[data_list$data_5$Datum == data_list$data_5$Datum[j] - 1 ])>0)
    data_list$data_5$umsatz_day_before[j] <- data_list$data_5$Umsatz[data_list$data_5$Datum == data_list$data_5$Datum[j] - 1 ]
}


for (j in 2: as.numeric(length(data_list$data_6$Umsatz))){
  if(length(data_list$data_6$Umsatz[data_list$data_6$Datum == data_list$data_6$Datum[j] - 1 ])>0)
    
    data_list$data_6$umsatz_day_before[j] <- data_list$data_6$Umsatz[data_list$data_6$Datum == data_list$data_6$Datum[j] - 1 ]
}


# .............................................................................. ----
## diese Datensätze werden nun gemergt und dann der alte überschrieben----
data <- data_list[[1]]
for(k in 2:(groupnumber)){
  data <- merge( data_list[[k]], data  , by = c('Warengruppe', "Umsatz", "Datum", "umsatz_day_before"), all = TRUE)
}

umsatzdaten <- data
# .............................................................................. ----
## check for NAS ----
sum(is.na(data$umsatz_day_before))


# .............................................................................. ----
# Wochentag und Wetter hinzufügen ----
# Erweitern des Umsatzdaten Datensates mit Wochentagen
umsatzdaten$weekday <- wday(umsatzdaten$Datum) # 1 ist Sonntag. Meine ich.

# Wetter an den Datensatz anschließen
umsatzdaten <- merge(umsatzdaten, wetter, by = "Datum")

write.csv(umsatzdaten, "data/umsatzdaten.csv")

