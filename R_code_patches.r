##########
# setto la workinf directory 

setwd("~/Desktop/Eco del Paesaggio/LAB")

library(raster)

# per caricare i dati raster, 2 funzioni:
# funzione BRICK : per caricare tutte le bande
# funzione RASTER : per caricare un singolo livello
# non uso lapply perche sono solo due mappe 

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

# plottiamo i due file per vedere la loro composizione
# metto due colori(es.una per la foresta e una per campi agricoloi)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c,col=cl)
plot(d2c,col=cl)     # ma è sbagliato il colore, sono invertiti, allora inverto <- colorRampPalette(c('black','green'))(100) 

cl <- colorRampPalette(c('black','green'))(100) 

#### la foresta è la classe numero 2, agriculture è la classe numero 1
# adesso voglio che la classe 1(agricoltura) abbia un valore nullo(annullando tutto quello che non è foresta)estraendo solo la foresta
# cosi da poter fare calcoli solo con i dati della foresta
# funzione RECLASSIFY(paccheto raster)= riassegna dei valori con cbind (classe 1 divemta NA)
# d1c.for = foresta

d1c.for <- reclassify(d1c,cbind(1,NA))
# rilancio una par 

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) 
plot(d1c,col=cl)
plot(d1c.for)     # di colore giallo

plot(d1c.for,col=cl)   # cambio il colore in verde con la classe agricola nulla in bianco

# annullo la classe 1 anche sulla seconda immagine

d2c.for <- reclassify(d2c,cbind(1,NA))

par(mfrow=c(1,2))
plot(d1c)
plot(d2c)   # due mappe solo con le foreste 


################ Calcolo il numero di PATCHES

# funzione CLUMP
# la applico ad esempio alla prima mappa

install.packages("igraph")
library(igraph)

d1c.for.pacthes <- clump(d1c.for)
d2c.for.pacthes <- clump(d2c.for)

writeRaster(d1c.for.pacthes, "d1c.for.patches.tif") # salvo dei dati verso l'esterno nella cartella LAB
writeRaster(d2c.for.pacthes, "d2c.for.patches.tif")

par(mfrow=c(1,2))
plot(d1c.for.pacthes)
plot(d2c.for.pacthes) # colori un po brutti

# creiamo un'altra color ramp palette per aumentare un po la differenziazione

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.pacthes, col=clp)
plot(d2c.for.pacthes, col=clp)

# se lancio su R solo "d1c.for.pacthes" mi da le informazioni

d1c.for.pacthes # posso andare a vedere i valori minimi e massimi (che sono il numero dei patch)

d1c.for.pacthes = 301 patches
d2c.for.pacthes = 1212 patches

time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")











































