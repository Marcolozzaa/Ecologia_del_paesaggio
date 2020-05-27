# R_code_exam.r



# 1. R_code_first.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial2.r
# 4. R_code_point_pattern   
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r   





########################################################################################################
########################################################################################################
########################################################################################################





### 1. R code first




# INIZIO 

install.packages("sp")

# richiamo il pacchetto 

library(sp)

data(meuse)

# se digito solo meuse avro tutta la tabella

meuse 
# visualizzo data set solo nelle prime righe
head(meuse)
# names = nome delle variabili
names(meuse)

# summery= si puo fare un abstact delle info del dataset e delle funzioni che contiene
summary(meuse)

# pairs= funzione che crea grafico mostruoso= correlazione tra le varie variabili(tutte insieme)
pairs(meuse)

# c'è il modo di ridurre il numero di variabili nella funzione pairs
# ~ = TILDE, è un simbolo che significa UGUALE

pairs(~ cadmium + copper + lead , data = meuse) 

# EXERCISE 

# [,3:6]= vuol fare un subset ([]), la "," vuol dire parti da, e 3:6 vuol dire dalla colonna 3 alla 6
# pairs(meuse[,3:6]) = pairs(~cadmium+copper+lead+zinc,data = meuse)
# " " si usano per argomento di testo

pairs(meuse[,3:6],
      col = "red",                                         # Cambio colore dei punti
      pch = 19,                                            # Cambio la forma dei punti, pch=point character
      labels = c("var1", "var2", "var3"),                  # Change labels of diagonal
      main = "This is a nice pairs plot in R")             # Add a main title
      
      
# AGGIUNGO ELEVATION (che è la 7ima variabile)
pairs(meuse[,3:7],
# il resto poi tutto uguale


# FUNZIONI PANNEL

# <- = dai nomi alle punzioni, ho chiamato con un certo nome un blocco di codici
# faccio indice di correlazione tra x e y (tra due variabili)

# PRIMA FUNZIONE
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)

     usr <- par("usr"); on.exit(par(usr))
     par(usr = c(0, 1, 0, 1))
     
 
# SECONDA FUNZIONE -> panel.smoothing = È un plot di punti con "lowes" = smoother locale per mostrare una linea di correlazione tra variabili

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
   cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
   points(x, y, pch = pch, col = col, bg = bg, cex = cex)


# TERZA FUNZIONE -> panel.histograms = ISTOGRAMMI,fa un istogramma delle distrubuzioni di frequenza

panel.histograms <- function(x, ...)
usr <- par("usr"); on.exit(par(usr))


# Decido cosa mettere nei vari spazi del grafico (upper pannel,lower,diagonale(cioe centrale))

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)


EXERCISE: mettere come lower panel lo smoothing, come diagonal apnel gli istogrammi e come upper panel le correlazioni 

pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)


# FUNZIONE PLOT

# come sono relazionati tra loro cadmio e rame?
# $ = in R collega un pezzo con un altro, nostro caso collega la colonna col proprio data set

plot(meuse$cadmium,meuse$copper)

# oppure allego il dataframe con attach= spiega a R che utilizzeremo sempre quel dataset per le alre funzuoni (cosi non uso $)

attach(meuse)

plot(cadmium,copper) 

plot(cadmium, copper, pch=17, col="green", main="primo plot")

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame") 

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2) 



########################################################################################################
########################################################################################################
########################################################################################################



### 2. R code spatial





# R spaziale : funzioni spaziali in Ecologia del Paesaggio

install.packages("sp")

# richiamo il pacchetto sp 
library(sp)

# richiamo i dati "meuse"
data(meuse)

head(meuse)

#plot cadium e lead, devo allegare database

attach(meuse)

#plotto e coloro e uso diverso carattere(pch=point character) e aumento dimensione(cex=character exageration)

plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise : plot di copper e zinco con simbolo triangolo e colore verde

plot(copper,zinc,col="green",pch=17,cex=2)

# " " le uso quando ho un testo

# cambiare le etichette nel grafico (x e y label)

plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

# multiframe (per mettere piu di un grafico nella stessa finestra), par è la funzione (poi decido se i grafici li volgio in riga o in colonna)
# c(1,2) = una riga e due colonne , nele due colonne ho messo i due grafici sullo stesso piano, la stessa riga
# sotto copio e incollo i due grafici da unire , (puo essere utile per es.analisi multitemporale (t0 e t1))

par(mfrow=c(1,2))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# adesso due righe e una colonna

par(mfrow=c(2,1))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# multiframe automatico
install.packages("GGalli")

# prendo il dataset Meuse e lo mando nella funzione scaricata "ggpairs"
# [,3:6] = dalla terza colonna alla sesta colonna per avere tutti gli elementi (li vedo da head(meuse))
# grafico= sull asse diagonale le singole variabili e la distribuzione dei dati 
# coefficienti di correlazione, 0.92 = molto correlati

library(GGally)
ggpairs(meuse[,3:6])

# Spatial, devo spiegare a R che "meuse" ha delle coordinate x e y
# head(meuse) vedo le coordinate

head(meuse)

# inserire per primo il dataset e poi speigo che ho x y , gli ho spiegato quali sono le mie coordinate
# uso ~ per gruppo di coordinate

coordinates(meuse)=~x+y

plot(meuse)

# inserisco nella funzione il dataset per creare grafico spaziale grazie a spplot (plotto i dati spazialmente)
# grafico= come si ditribuisce lo zinco attorno al fiume, vicino all'acqua (giallo) valori molto alti, lontani dal fiume puntini neri valore basso

spplot(meuse,"zinc")




########################################################################################################
########################################################################################################
########################################################################################################



### 3. R code spatial 2




# INIZIO
# installo "sp" se non lo ho ancora
# se lo ho gia lo carico 

library(sp)

# carico poi dataset meuse

data(meuse)

# coordinate dataset x e y nel dataset meuse

coordinates(meuse)=~x+y

# poi creo sp plot(funzione della libreria sp) con le varibili con le virgolette

spplot(meuse,"zinc")

# ESERCIZIO: stessa cosa ma col rame 
# il grafico del rame ha dei valori piuttosto alti nella parte vicino al fume e piu bassi allontanadosi dal fiume 
# il fiume è molto inquinato 

spplot(meuse,"copper")

# funzione bubble nel pacchetto sp, rappresento uguale a spplot MA i valori piu alti hanno bolle piu grandi 

bubble(meuse,"zinc")

# ESERCIZO: bubble di rame colorato di rosso

bubble(meuse,"copper",col="red")

# ESERCIZIO, creo due oggetti e li plotto insieme

# Lucia: foraminiferi con diversi plot
10,20,35,55,67,80
# questo è un array e si scrive con c
c(10,20,35,55,67,80)
# lo chiamo foram

foram <- c(10,20,35,55,67,80)

# ho creato un oggetto

# MARCO plot per carbon sequestro
5,15,30,70,85,99

carbon <- c(5,15,30,70,85,99)

# ci siamo inventati un data frame e li incollo su R entrambi 
# faccio un plot
# vedo che c'è una stretta relazione tra l'abbondanza dei foraminiferi e il sequestro di CO2

plot(foram,carbon,col="green",cex=2,pch=3)

# ESERCIZIO
# faccio dialogare R con dati dall'estreno, cioe col computer
# in questo caso dati del covid 19, creo la cartella
# dico ad R quale cartella col comando "setworkingdirectoy" = setwd

setwd("Deckstop/LAB")
# su Mac no funzia... allora basta andare su files scegliere la cartella e fare import data set

# leggere la tabella
# la tabella prima ha i titoli delle colonne e non i dati, quindi gli spiego che ho un Header(testa)
# con head=T  (TRUE)  , cioè so che c'è un titolo 

covid <- read.table("covid_agg.csv",head=TRUE)

NON FUNZIA D.C!!!


########################################################################################################
########################################################################################################
########################################################################################################


### 4. R code piont pattern




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


########################################################################################################
########################################################################################################
########################################################################################################


### 5. R code teleril



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



########################################################################################################
########################################################################################################
########################################################################################################



### 7. R code multitemp




install.packages("gridExtra")
install.packages("RStoolbox")
library(ggplot2)
library(raster)

# Ananisi multitemporali con terreno suddiviso in varie classi di copertura del suolo 

setwd("~/Desktop/Eco del Paesaggio/LAB")

library(raster)

# uso BRICK per caricare tutte le singole bande di immagini satellitari 
# carico le immagini riguardanti le deforestazioni 1 e 2 

defor1 <- brick("defor1_.png")
defor2 <- brick("defor2_.png")

# DEFOR1 ho tre bande, metto Infrarosso vicino alla banda R, nella componente G inserisco la banda R, nella componente R inserisco la banda G

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin") # immagine della foresta pluviale dove le piante sono in rosso
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# faccio due classi per classificare tutto quello che è forsta
# funzione unsuperclass è per creare le classi nonn supervisionate (non gli diamo un imput)
# devo caricare perô RStoolbox

install.packages("RStoolbox")
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2) # è una $map

d1c
# d1c$map è la mia mappa

plot(d1c$map)

# cambio i colori 

cl <- colorRampPalette(c('black','green'))(100) # ho la foresta in verde e tutto il resto in nero
plot(d1c$map, col=cl)


# Classifico anche la seconda immagine

d2c <- unsuperClass(defor2, nClasses=2) 
d2c
# d2c$map è la mia mappa

plot(d2c$map)
cl <- colorRampPalette(c('black','green'))(100) # ho la foresta in verde e tutto il resto in nero
plot(d2c$map, col=cl)

dev.off()

plotto le due immagine appena ottenute
par(mfrow=c(2,1))
plot(d2c$map, col=cl)
plot(d1c$map, col=cl)


# QUANTIFICO ADESSO LA PERCENTUALE di foresta persa (in base al numero di pixels appartenenti ad ogni classe)
# MAPPA 1
freq(d1c$map) # mi conta i pixel per ogni classe
              # n.di pixel area foresta = 305095
              # n.di pixel area aperta = 36197
              
# calcolo il totale e poi le proporzioni (che x100 mi da la percentuale) tra le due classi
# freq = freq della mappa per 100/il totale

totd1 <- 305095+36197
totd1   # mi da 341292

percent1 <- freq(d1c$map) * 100 / totd1
percent1   # mi mostra le percentuali (89.4% e 10.6%)

# MAPPA 2

freq(d2c$map) # frequenza 2 è la classe della foresta
totd2 <- 178625+164101
totd1 
percent2 <- freq(d2c$map) * 100 / totd2
percent2 # che è 47.8% e 52.2%(di foresta)

# CREO UN DATAFRAME, una picoola tabella con i vari volri di percentuali

cover <- c("Agriculture","Forest")
before <- c(10.6,89.4)
after <- c(47.8,52.2)
# creo le colonne con la cover, prima del disboscamento e dopo il disb.

output <- data.frame(cover,before,after)

# ADESSO devo plottare i valori
# richimao "ggplot2"

library(gglpot2)

# GIORNO DUE
# ricarico dati RData e Working directory

load("~/Desktop/Eco del Paesaggio/LAB/defor.RData")
setwd("~/Desktop/Eco del Paesaggio/LAB")
ls()   # d1c e d2c sono i due grafici che mi interessano

# ri visualizzo i grafici dell'altra volta
par(mfrow=c(2,1))
cl <- colorRampPalette(c('black','green'))(100)
plot(d2c$map, col=cl)
plot(d1c$map, col=cl)



library(ggplot2)
# grafico con ggplot = ISTOGRAMMA DELLA % DI FOREST COVER 
# aes= aestetichs
# colore basato sulla cover 

# p1 DEFORESTAZIONE BEFORE

p1 <- ggplot(output, aes(x=cover,y=before,color=cover)) + geom_bar(stat="identity",fill="white")
plot(p1)

# p2 DEF.AFTER

p2 <- ggplot(output, aes(x=cover,y=after,color=cover)) + geom_bar(stat="identity",fill="white")
plot(p2)

install.packages("gridExtra")
library("gridExtra")

# grid.arrange(plot1,plot2,nrow=1) = due grafici nella stessa finestra 

grid.arrange(p1,p2,nrow=1)


# AGGIUNGO UN LIMITE SULL'ASSE Y per visualizzare meglio i grafici con YLIM

p1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

p2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)



########################################################################################################
########################################################################################################
########################################################################################################



### 8. R code multitemp NO





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




########################################################################################################
########################################################################################################
########################################################################################################




### 9. R code SNOW




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
 

########################################################################################################
########################################################################################################
########################################################################################################




### 10. R code patches




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






