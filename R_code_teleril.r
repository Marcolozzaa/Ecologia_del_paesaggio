# CODICE R PER ANALISI DI IMMAGINI SATELLITARI 

install.package("raster")

library(raster)

# setto working directory 

setwd("~/Desktop/Eco del Paesaggio/LAB")


# Importo immagine all'interno di R => p224r63 (con il path e la row)
# associo l'immgaine alla funzione BRICK

p224r63 <- brick("p224r63_2011_masked.grd")

plot(p224r63)

# Ho B1,B2,... cioe diverse bande con diverse riflettanze nelle diverse lunghezze d'onda
# in tutto 7 sensori 


