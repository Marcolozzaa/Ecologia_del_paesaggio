# Argomento = land cover 

setwd("~/Desktop/Eco del Paesaggio/LAB")

install.packages("RStoolbox")
library(RStoolbox)
library(raster)

# brick= impila i dati e portali su R

p224r63_2011 <- brick("p224r63_2011_masked.hdr")

# con le tre componenti, red=4 del vicino infrarosso,green=nel rosso, blue

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# della nostra immagine le classi sembrano 4
# p224r63_2011c = "c" nel senso classificato

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# plotto la mappa       $map = mappa generata
# valori pixel da 1 a 4

plot(p224r63_2011c$map)

# ma cambio i colori(che me li ha messi di standard R)

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 

plot(p224r63_2011c$map, col=clclass)

# proviamo a mettere le classi uguali a 2 invece che 4

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

# in funzione del numero di classi aumenta l'incertezza
# possiamo fare piu mappe con piu o meno classi per vedere le difernze tra i cluster dei pixel 
# con due classi incertezza bassa


# PARTE 2 (22 Aprile)

# ricarico il grafico dei POINT PATTERN per poter fare l'interpolazione sui valori dei casi del covid

library(spatstat)
library(rgdal) # for the coastlines

setwd("~/lab/")
load("point_pattern.RData")
ls()

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)



# interpolazione

# 1a cosa da fare. Guardare la tabella dei dati e vedere quale variabile mi interessa
# head(covid)   a me interessa colonna "cases"
# funzione marks = valori che do ai dati del point pattern e lo associo alla colonna cases

marks(covids) <- covid$cases

# s ( per stima )
# smooth dei punti spaziali del covid

s <- Smooth(covids)

plot(s)

# aggiungo titolo, colori diversi(colourpalette), aggiungo i punti e aggiungo coastiles al plot s

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# GRFICO = stima dei casi. Molto alta verso DX(zona cina) 

text(covids)    # per aggiungere il valore dei punti 


# MAPPA FINALE
# paragonare i due grafici plottati, quello sulla densita e quello sull interpolazione

par(mfrow=c(2,1))

# densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

 


# TESI SAN MARINO
# seto la working directory
# prima cosa carico i dati 

load("Tesi.Rdata")
ls()  # per vedere cosa c'è dentro
head(Tesi)

# grafico densita dei punti perche come varibili ho le coordinate

library(spatstat)  # per usare ppp

# devo fare un ppp per crearlo mi serve coordinata x, coord.y, c(xmin,xmax),c(ymin,ymax) cioe i miniti della x e della y
# uso funzione che si chiama summery che mi dice i valori min e max delle coordinate
# x va da 12.42 a 12.46 (mettero in realta 12.41 e 12.47 per stare un po piu larghi)
# y da 43.91 a 43.93 ( aumento di 1 per estendere)

attach(Tesi) # super importante

summary(Tesi)

Tesippp <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.94))

# grafico della densità

dT <- density(Tesippp)

plot(dT)

points(Tesippp,col="green")



# SAN MARINO parte 2 

setwd("~/Desktop/Eco del Paesaggio/LAB")
load("SAN MARINO.RData")

ls() # cosi vedo che dati ho all'interno
     # dT = è la density map
     # Tesi = era un dataset
     # Tesippp = point pattern

library(spatstat)
     
plot(dT)
points(Tesippp, col="green") # grafico che avevo gia otteuto con densita piu elevata verso il centro

# quale è la ricchezza specifica? Per fare INTERPOLAZIONE
# prima faccio head e vado a vedere che dati ho
# campo "species richness"
head(Tesi)

# marks, valori della variabile che voglio interpolare associata ai valori del PPP(point pattern che adesso non è associato a nulla)

marks(Tesippp) <- Tesi$Species_richness

# FUNZIONE SMOOTH = INTERPOLATORE!! mappa raster a partite da volori di punti 
# due punti misurati, calcolo la media tra i due e vado avanti cosi

interpol <- Smooth(Tesippp)

plot(interpol)
points(TESI,col="green") # per aggiungere i punti

# i valori di ricchezza specifica sono distribuiti diversamente dalla densità
# valori piu alti nella parte centrale e sud-est

# AGGIUNGO FILE VETTORIALE SI SAN MARINO 

# library RGDAL per leggere immagini vettoriali 
library(rgdal)
# OGR = file vettoriale
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino) # è un poligono
# aggiungo i punti di interpol sopra a sanmarino
# aggiungo anche i punti
plot(interpol,add=T) # add=T per aggiungere dei pezzi alla mappa precedente
points(Tesippp,col="green")
plot(sanmarino,add=T) # per sovraporre e vedere i confini di San Marino

# UNISCO I DUE GRAFICI (densità ed interpolazione) con una colonna e due file

par(mfrow=c(2,1))

plot(dT, main="Density of points")
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

# non cosidero pero l'uso del suolo, es, vicino c'è zona urbana dove avro una bassa ricchezza specifica

# STESSI FILE UNITI MA SU DUE COLONNE E UNA RIGA
par(mfrow=c(1,2))

plot(dT, main="Density of points")
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")















