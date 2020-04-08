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





















