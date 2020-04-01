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

# Lezione 2 (1 Aprile)

# imposto i dati di ieri
# con RStudio è gia tuto salvato
setwd("~/lab/")
load("point_pattern.RData")
# carico la libreria spatstat
library(spatstat)

plot(d)

# voglio cambiare la gamma di colori del grafico di densità=palette di colori
# do un nome alla palette, creo una gamma(arrey) di colori dopo la parentesi (c('colore1','colore2',...)
# cl= colour (lo ho scleto io per comodità)
# si usa una virgoletta
# qunati mini livelli tra un colore all'altro? piu ne ho meglio è cosi ho una maggior gradazione es. (100) = 100 gradazioni
cl <- colorRampPalette(c('yellow','orange','red'))(100)

# Faccio il grafico di denistà e gli specifico che colori volgio(quelli che ho fatto io)
plot(d,col=cl)

# ESERCIZIO, plot della mappa dal verde al blu

cl1 <- colorRampPalette(c('green','blue','purple'))(100)
plot(d,col=cl1)

# con pionts posso poi inserire i punti di ieri del COVIDS
point(covids)
# posso anche mettere i confini degli altri stati
# coastlines = nome del nuovo file
# readOGR è parte di una libreria GDAL (libreia Geospaziale che permette di leggere qualsiasi tipo di file raster o vettoriale)
# rGDAL libreria di R
# installo con le virgolette perche devo uscire da GitHub
install.packages("rgdal")
library(rgdal)
# i file devono essere nella cartella LAB liberi
coastlines <- readOGR("ne_10m_coastline.shp")
# adesso plotto i dati 
plot(coastlines,add=T)

# ESERCIZIOplot della mappa con nuova colorazione ed aggiunta della coastlines
cl1 <- colorRampPalette(c('blue','purple','light green','yellow'))(100)
plot(d,col=cl1)
plot(coastlines,add=T)
# col=yellow, setto i confini dei continenti col giallo 
plot(coastlines,add=T,col=yellow)























