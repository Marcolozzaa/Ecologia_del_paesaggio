### ESERCIZIO CON COMPERNICUS
# caricare file assieme
# differenza tra immagini
# mappa di previsione

setwd("~/Desktop/Eco del Paesaggio/LAB")

install.packages("ncdf4")

library(ncdf4)
library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

plot(snowmay,col=cl)

# Scarico zip file 
# Importo adesso i dati dalla zip.snow delle varie immagini in diversi anni
# IMPOSTO la cartella SNOW come working directory per poter importare insieme tutte le immagini perche per importarsi insieme devono essere tutti dentro ad una cartella
# USO funzione lapply

setwd("~/Desktop/Eco del Paesaggio/LAB/SNOW")

rlist <- list.files(pattern=".tif", full.names=T)

# lapply apllica dei comandi a degli interi lista di file. Nel nostro caso è la funzione raster
list_rast <- lapply(rlist, raster)

# vogliamo creare uno stack
snow.multitemp <- stack(list_rast)        # snow.multitemp (come time snow nel tempo)

plot(snow.multitemp)       # colori a caso, li cambio con una colourramp palette

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snow.multitemp,col=cl)




####### evidenzio le differnze tra le immagini e poi faccio delle previsioni
# plotto con Par la prima e l'ultima immagine 
# PRIMA IMMAGINE <- plotto i file snow.multitemp e lo lego col $ all'immagine 2000
# SECONDA IMMAGINE <- plotto i file snow.multitemp e lo lego col $ all'immagine 2020

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# valore nella legenda sono diversi però, uno arriva a 250 e l'LTRO A 200
# uso funzione ZLIM (limiti assi)

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))    # adesso sono visualmente comparabili

####### faccio adesso una DIFFERENZA tra le due mappe con la funzione DIF e metto una color ramp 

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 

dev.off() # per eliminare il par di prima 

plot(difsnow,col=cldif)

####### faccio adesso una PREVISONE(SCENARIO) multimtemporale per vedere nl 2025 come sara la copertura nevosa di una determinata misura
# dati basati sullo scarto quadratico medio (linea che unisce i miei dati e puo essere lineare, curva,seno-coseno,...)
# facile se c'è una variazione lineare da un tempo all'altro 
# magari i dati formano una curva a differenziale negativo
# magari i dati sono ciclici (funz.seno-coseno), es.stagionali, o glaciaioni(eventi ciclici)

# per fare la predizione ho scricato da IOL file prediction.r e salvata nella cartella SNOW

# funzione SOURCE per fare girare uno scrip su R prelevandolo dal desktop (cariare dati dall'esterno)

source("prediction.r")  # impiega un casino di tempo

# prof aveva creato file predicted.snow.2025.TIF e lo scarico e lo metto nella cartella SNOW per risparmiare tempo
# SCENARIO 2025

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
 
plot(predicted.snow.2025.norm, col=cl)
 

