# CODICE R PER ANALISI DI IMMAGINI SATELLITARI 

install.packages("raster")

library(raster)

# setto working directory 

setwd("~/Desktop/Eco del Paesaggio/LAB")


# Importo immagine all'interno di R => p224r63 (con il path e la row)
# associo l'immgaine alla funzione BRICK

p224r63 <- brick("p224r63_2011_masked.grd")

plot(p224r63)

# Ho B1,B2,... cioe diverse bande con diverse riflettanze nelle diverse lunghezze d'onda
# in tutto 7 sensori 
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# PARTE 2
# se ho chiuso R e devo ricaricare i dati 

setwd("~/Desktop/Eco del Paesaggio/LAB")

load("Lezione 7 Aprile.RData")

p224r63_2011 <- p224r63


plot("p224r63_2011")

# non me lo da perche bisogna caricare la libreria "raster"

# cambio la colorazioe del Plot con "ColorRampPalette". Metto poi con c le colorazioni che voglio 

cl <- colorRampPalette(c('black','grey','light grey'))(100)

plot(p224r63_2011,col=cl)

# con 5 i colori appaiono molto piu sgranati 

cllow <- colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011,col=cllow)

# PLOT della banda del blu
# names per vedere tutti i nomi degli oggetti che stiamo utilizzando

clb <- colorRampPalette(c('dark blu','blue','light blue'))(5)

# attach(dataframe) non funziona con il pacchetto raster e allora devo usare il $ per B1_rse(cioe il sensore banda blu)

plot(p224r63_2011$B1_rse,col=clb)

# ESERCIZIO con B4_sre e range di colori rosso,arancione e giallo

clnir <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_2011$B4_sre, col=clnir)


# Faccio un multiframe di 4 bande con funzione "par" che mi permette di dividere in un pannello i vari grafici
# mfrow,c(2,2) = cioe due righe e due colonne 
par(mfrow,c(2,2))

# blue (in alto a sx)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)

# green (in alto a dx)
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_2011$B2_sre, col=clg)

# red (in basso a sx)
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_2011$B3_sre, col=clr)

# Infrarosso
clnir <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_2011$B4_sre, col=clnir)

# i valori sono molto alti nel NIR ed idica un elevata presenza di piante
# i valori del blu e del rosso invece sono valori bassi (perche assorbite dalle inate per la fotosintesi)

# Imaggine come la vederebbe un occhio umano, unendo le bande rossa,blu e verde

dev.off() # per chiudere le immagini che ho appena plottato 

# natural colours (come occhio umano), componenti R,G, e B ( computer plotta 3 bande per volta)

plotRGB(p224r63_2011, r=3, g=2, b=1)

# appare nera l'immagine e allora devo strecchare i colori e l'immagine 
# stretch=lin cioè lineare

plotRGB(p224r63_2011, r=3, g=2, b=1,stretch="Lin")

# immagine come la vedrebbe occhio umano ma la vegetazione fitta puo essere confusa con ombre
# allora aggiungo anche NIR ( tramite r=4 ) 4= infrarosso vicino 
# scalo tutti i colori di uno per formare FALSE COLOR

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")

# IMMAGINE formata : le piante diventeranno di colore rosso 

par(mfrow,c(1,2)) # 2 immagini una sotto all'altra
plotRGB(p224r63_2011, r=3, g=2, b=1,stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")

plotRGB(p224r63_2011, r=3, g=4, b=2,stretch="Lin") # per avere l'immagine dell'infrarosso nel verde, cosi da vedere la vegetazione




# PARTE 3
# ricarico tutti i dati
install.packages("raster")

library(raster)

setwd("~/Desktop/Eco del Paesaggio/LAB")

load("teleril.RData")

# posso fare una lista per vedere tutti i dati salvati precedentemente
# a me interessa p224r63_2011
 ls()

# importo adesso il file del 1988 per 
# uso comando brick per importare tutte le varie bande dell'immagine satellitare

p224r63_1988 <- brick("p224r63_1988_masked.grd")

# uso ancora par,mfrow per arere piu immagini nella stessa frame
# uso poi colorramp palette

par(mfrow,c(2,2))

# blue (in alto a sx)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_1988$B1_sre, col=clb)

# green (in alto a dx)
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_1988$B2_sre, col=clg)

# red (in basso a sx)
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_1988$B3_sre, col=clr)

# Infrarosso
clnir <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_1988$B4_sre, col=clnir)


# Imaggine come la vederebbe un occhio umano, unendo le bande rossa,blu e verde

dev.off() # per chiudere le immagini che ho appena plottato 

# natural colours (come occhio umano), componenti R,G, e B ( computer plotta 3 bande per volta)

plotRGB(p224r63_1988, r=3, g=2, b=1)

# appare nera l'immagine e allora devo strecchare i colori e l'immagine 
# stretch=lin cioè lineare

plotRGB(p224r63_1988, r=3, g=2, b=1,stretch="Lin")

# immagine come la vedrebbe occhio umano ma la vegetazione fitta puo essere confusa con ombre
# allora aggiungo anche NIR ( tramite r=4 ) 4= infrarosso vicino 
# scalo tutti i colori di uno per formare FALSE COLOR

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")

# IMMAGINE formata : le piante diventeranno di colore rosso 

# adesso faccio un plot delle due immagini( 2011 e 1988) in un multiprame(par mfrow(multiframe row))
# main (è per aggiungere un titolo ai grafici)
par(mfrow=c(2,1))

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_1988, r=4, g=3, b=2,stretch="Lin")

# SPECTRAL INDEX
# posso calcolare un indice per vedere come stà la vegetazione
# pinata sana riflette molto nell'INFRAROSSO (alta riflettanza) e poco nel blu e rosso
# uso l'indice chiamato DVI (different vegetation index)
# se sottraggo i valori delle due bande (Nir - Rosso) 
# pinata sana (Nir=90, R=10 -> DIV=80 ) pinata malata o poche pinate (Nir=70, R=30 -> DIV=40)
# dvi1988= nir1988-red1988
# $ per collegare le bande all'immagine

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre

cldvi <- colorRampPalette(c('light blue','light green','green'))(100) 
plot(dvi2011,col=cldvi)

# posso anche vedere la differenza tra diversi DVI, se è un valore positivo= MIGLIORAMENTO se è negativo=PEGGIORAMENTO
# es. dvi2011-dvi1988= 40 - 80 = - 40 (indice negativo)

difdvi <- dvi2011-dvi1988

plot(difdvi)
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) 
plot(difdvi,cl=cldifdvi)
# tutte le zone dove è stata tagliata la vegetzione è in colore rosso
# colore blu invece pinate che stanno meglio 
# "binaco" situzione stabile 

# voglio 3 immagini di fila [mfrow=c(3,1)], immagine del 2011, del 1988 e l'immagine di confronto del DVI

par(mfrow=c(3,1))

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_1988, r=4, g=3, b=2,stretch="Lin")
plot(difdvi,cl=cldifdvi)

dev.off() # per chiudere

# cambio la risoluzione, cambio la diensione dei pixel con funzione "aggregate"= (cosa cambio, fact(quanto la cambio, se metto es. 10 aumenta i pixel di 10 volte))
# lr (low resolution)

p224r63_2011 # vedo tutte le info tra cui anche la risoluzione (che è 30m)
p224r63_2011lr # invece ha 300 al posto che 30 (perche è stata moltiplicata per 10)

p224r63_2011lr <- aggregate(p224r63_2011,fact=10) # immagine che sembra essere miopi

# metto a confronto le due immagini a 30 m e a 300m
par(mfrow=c(2,1))

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2,stretch="Lin")

# provo a diminuitre ancora di piu la risoluzione (risoluzione 1500m con fattore=50)

p224r63_2011lr50 <- aggregate(p224r63_2011,fact=50)

par(mfrow=c(3,1)) # plotto le 3 immagini a diverse risoluzioni

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2,stretch="Lin")

dev.off()

# calcolo il dvi per il low resolution del 2011

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre)
plot(dvi2011lr50)

# Diminuisco la risoluzione dell'immagine del 1988 (risoluzione 1500m con fattore=50)

p224r63_1988lr50 <- aggregate(p224r63_1988,fact=50)

dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre)

# differenza dvi a low resolution del 2011 e 1988 e lo plotto 

difdvilr50 <- dvi2011lr50-dvi1988lr50
plot(difdvilr50, col=coldifdvi)

# multiframe con due risultati
# perche? perche bisogna utilizzare la grana/scala giusta!!

par(mfrow=c(2,1))
plot(difdvi,cl=cldifdvi)
plot(difdvilr50, col=coldifdvi)

















































