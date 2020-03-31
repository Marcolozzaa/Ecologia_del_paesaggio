# ANALISI POINT PATTERNS

# metto subito ed indico i pacchetti che ho dovuto scaricare per l'esercitazione
install.packages("ggplot2") 
install.packages("spatstat")

library(ggplot2)
library(spatstat)

# importo la working directory, con mec faccio Import dataset
setwd("C:/lab")     

# rinomino covid_agg in covid

covid <- read.table("covid_agg.csv", head=T)  # per importare dati di una tabella
covid <- covid_agg
head(covid)

#  plot mi fa un grafico con due variabili 

plot(covid$country,covid$cases)    # $ collega un pezzo ad un altro, in questo caso collega la colonna al proprio dataset altrimenti non riconosce la colonna
plot(covid$country,covid$cases,las=0)  # per metterlo verticale
plot(covid$country,covid$cases,las=1)  # las=1 le etichette di asse Y diventano orizzontali
plot(covid$country,covid$cases,las=2)  # las=2 le etichette della asse X sono verticali
plot(covid$country,covid$cases,las=3)  # laberls verticali

# "cex.axis" per rimpicciolire le scritte di asse X
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5)


# ggplot2

install.packages("ggplot2") 
library(ggplot2)
# mpg è un dataset di prassi già all'interno di ggplt2
data(mpg)
head(mpg)

# Quello che serve a ggplot2 per creare un grafico è 1. data set 2. aestetics, cioè le variabili 3. La geometria con cui si vuole visualizzare

ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
# prendo come dataset=covid, variabili=longitudine e latitudine
# aggiungo size=cases per aggiungere i puntini al grafico riguardanti i casi nei diversi continenti 

ggplot(covid,aes(x=lon,y=lat)) + geom_point()    
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point() 

# Programma per fare grafic SPATSTAT
install.packages("spatstat")
library(spatstat)

# density intende la densità dei punti in una mappa
# create dataset for spatstat
# "ppp" devo mettere (variabile x, var.y, rage della x, range della y)
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

# grafico DENSITÀ

d <- density(covids)
plot(d)
points(covids)
