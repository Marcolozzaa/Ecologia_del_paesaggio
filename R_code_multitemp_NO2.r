# PER ANALIZZARE LE CONCENTRAZIONI DI CO2
# preso i dati dal file zip (dati di NO2 dall'ESA) e ho messo i dati senza cartella nella cartella LAB

setwd("~/Desktop/Eco del Paesaggio/LAB")
# importo poi le immagini con la funzione raster
# importo la prima immagine 

EN01 <- raster("EN_0001.png")
plot(EN01)    # SE HO GRAFICO PICCOLO SCIRVO    dev.off()

# importo le altre
# se metto lo zero prima del numero me le mette tutte in ordine

EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")


cl <- colorRampPalette(c("red","orange","yellow"))(100)
plot(EN01,col=cl)
plot(EN13,col=cl)


par(mfrow=c(1,2))
plot(EN01,col=cl)
plot(EN13,col=cl)    

# CON DEV.OFF() TORGLO LA PAR

# faccio una differenza 

diff <- EN13-EN01
cldif <- colorRampPalette(c("blue","black","yellow"))(100)
plot(diff,col=cldif)

# plotto tutte le immagini, 13 pero è un numero primo e per farle stare tutte in un PAR devo fare 4x4

par(mfrow=c(4,4))
plot(EN01,col=cl)
plot(EN02,col=cl)
plot(EN03,col=cl)
plot(EN04,col=cl)
plot(EN05,col=cl)
plot(EN06,col=cl)
plot(EN07,col=cl)
plot(EN08,col=cl)
plot(EN09,col=cl)
plot(EN10,col=cl)
plot(EN11,col=cl)
plot(EN12,col=cl)
plot(EN13,col=cl)

plot(EN01,EN02,col=cl)


########### DAY 2

# come faccio ad imporatre tutti ifile allo stesso tempo? SE HO TANTI FILES!!
# devo creare una cartella all'interno della cartella LAB e cambiare la working directory
# metto tutte le immagini nella nuova cartella "esa_no2" e la imposto come working directory

library(raster)
setwd("~/Desktop/Eco del Paesaggio/LAB/esa_no2")
rlist <- list.files(pattern=".png")
rlist         # appare la lista di tutti i files con estensione .png

# funzione lapply (che si legge l applay) applica una funzione su una serie di elementi(una lista di files)
# la funzione che volgio applicare sarà brick(per importare piu layers) oppure raster(per un singolo layer) per caricare le mie immagini
# (rlist,raster) funzione raster applicata alla lista che ho creato prima

listafinale <- lapply(rlist,raster)
listafinale   # visualizzo 13 elementi in lista con le singole bande

# adesso faccio il plot col par(4,4)
# PER PRIMO DEVO USARE funzione stack per unire tutte le bande per creare un pacchetto di dati (e per poter fare poi PLOT)
# unisco le immagini dove ogni banda equivale a un TEMPO diverso

EN <- stack(listafinale)

cl <- colorRampPalette(c("red","orange","yellow"))(100)
plot(EN,col=cl)


######### DAY 3 (12 amggio)

library(sp)
library(raster)
setwd("~/Desktop/Eco del Paesaggio/LAB/esa_no2")  # "esa_no2" sarà la mia working direcorty
rlist <- list.files(pattern=".png")

 # adesso faccio la differenza dei pixel tra le immagini EN01 ed EN13 e lo plotto

difEN <- EN$EN0013 - EN$EN0001
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(difEN, col=cld)

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)   # per plottare poi tutta la lista di immagini raster

# FUNZ. STAT. BOX PLOT = individua il range dei dati con mediana e media
# ogni immagine EN avra il proprio boxplot con i suoi valori di mediana e di max e min
# cambiamento contingente sui valori massimi! I valori medi sono diminuiti di poco
boxplot(EN)
boxplot(EN, horizontal=T) # per avere i box in orizzontale
boxplot(EN, horizontal=T,outline=F)  # per rimuovere gli OUTLAINER (punti lontani dalla media)
boxplot(EN, horizontal=T,outline=F,axes=T) # aggiungo gli assi (anche se ci dovrebbero essere di default)












