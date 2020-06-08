# R_code_exam.r



# 1. R_code_intro.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial2.r
# 4. R_code_point_pattern   
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r   
# 11. R_code_crop.r
# 12. R_code_species_distribution_modeling.r



########################################################################################################
########################################################################################################
########################################################################################################





### 1. R_code_intro




# INIZIO 

install.packages("sp")

# M.L : richiamo il pacchetto 

library(sp)

data(meuse)

# M.L : se digito solo meuse avrò tutta la tabella

meuse 
# M.L : visualizzo data set solo nelle prime righe
head(meuse)
# M.L : names = nome delle variabili
names(meuse)

# M.L : summery = si puo fare un abstact delle info del dataset e delle funzioni che contiene
summary(meuse)

# M.L : pairs= funzione che crea grafico mostruoso= correlazione tra le varie variabili(tutte insieme)
pairs(meuse)

# M.L : c'è il modo di ridurre il numero di variabili nella funzione pairs
# M.L : ~ = TILDE, è un simbolo che significa UGUALE

pairs(~ cadmium + copper + lead , data = meuse) 

# EXERCISE 

# M.L : [,3:6]= vuol fare un subset ([]), la "," vuol dire parti da, e 3:6 vuol dire dalla colonna 3 alla 6
# M.L : pairs(meuse[,3:6]) = pairs(~cadmium+copper+lead+zinc,data = meuse)
# M.L : " " si usano per argomento di testo

pairs(meuse[,3:6],
      col = "red",                                         # Cambio colore dei punti
      pch = 19,                                            # Cambio la forma dei punti, pch=point character
      labels = c("var1", "var2", "var3"),                  # Change labels of diagonal
      main = "This is a nice pairs plot in R")             # Add a main title
      
      
# M.L : AGGIUNGO ELEVATION (che è la 7ima variabile)

pairs(meuse[,3:7])
# M.L : il resto poi tutto uguale
# M.L : FUNZIONI PANNEL
# M.L : <- = dai nomi alle funzioni, ho chiamato con un certo nome un blocco di codici
# M.L : faccio indice di correlazione tra x e y (tra due variabili)

# M.L : PRIMA FUNZIONE
      
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)

     usr <- par("usr"); on.exit(par(usr))
     par(usr = c(0, 1, 0, 1))
     
 
# M.L : SECONDA FUNZIONE -> panel.smoothing = È un plot di punti con "lowes" = smoother locale per mostrare una linea di correlazione tra variabili

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
   cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
   points(x, y, pch = pch, col = col, bg = bg, cex = cex)


# M.L : TERZA FUNZIONE -> panel.histograms = ISTOGRAMMI,fa un istogramma delle distrubuzioni di frequenza

panel.histograms <- function(x, ...)
usr <- par("usr"); on.exit(par(usr))


# M.L : Decido cosa mettere nei vari spazi del grafico (upper pannel,lower,diagonale(cioe centrale))

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)


EXERCISE: mettere come lower panel lo smoothing, come diagonal apnel gli istogrammi e come upper panel le correlazioni 

pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)


# M.L : FUNZIONE PLOT
# M.L : come sono relazionati tra loro cadmio e rame?
# M.L : $ = in R collega un pezzo con un altro, nostro caso collega la colonna col proprio data set

plot(meuse$cadmium,meuse$copper)

# M.L : oppure allego il dataframe con attach= spiega a R che utilizzeremo sempre quel dataset per le alre funzuoni (cosi non uso $)

attach(meuse)

plot(cadmium,copper) 

plot(cadmium, copper, pch=17, col="green", main="primo plot")

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame") 

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2) 



########################################################################################################
########################################################################################################
########################################################################################################



### 2. R_code_spatial

install.packages("sp")
install.packages("GGalli")
library(sp)
library(GGally)
      
# M.L : R spaziale : funzioni spaziali in Ecologia del Paesaggio

install.packages("sp")

# M.L : richiamo il pacchetto sp 
library(sp)

# M.L : richiamo i dati "meuse"
data(meuse)

head(meuse)

# M.L : plot cadium e lead, devo allegare database

attach(meuse)

# M.L : plotto e coloro e uso diverso carattere(pch=point character) e aumento dimensione(cex=character exageration)

plot(cadmium,lead,col="red",pch=19,cex=2)

# M.L : exercise : plot di copper e zinco con simbolo triangolo e colore verde

plot(copper,zinc,col="green",pch=17,cex=2)

# M.L : " " le uso quando ho un testo

# M.L : cambiare le etichette nel grafico (x e y label)

plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

# M.L : multiframe (per mettere piu di un grafico nella stessa finestra), par è la funzione (poi decido se i grafici li volgio in riga o in colonna)
# M.L : c(1,2) = una riga e due colonne , nelle due colonne ho messo i due grafici sullo stesso piano, la stessa riga
# M.L : sotto copio e incollo i due grafici da unire ,(puo essere utile per es.analisi multitemporale (t0 e t1))

par(mfrow=c(1,2))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# M.L : adesso due righe e una colonna

par(mfrow=c(2,1))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# M.L : multiframe automatico
install.packages("GGalli")

# M.L : prendo il dataset Meuse e lo mando nella funzione scaricata "ggpairs"
# M.L : [,3:6] = dalla terza colonna alla sesta colonna per avere tutti gli elementi (li vedo da head(meuse))
# M.L : grafico= sull asse diagonale le singole variabili e la distribuzione dei dati 
# M.L : coefficienti di correlazione, 0.92 = molto correlati

library(GGally)
ggpairs(meuse[,3:6])

# M.L : Spatial, devo spiegare a R che "meuse" ha delle coordinate x e y
# M.L : head(meuse) vedo le coordinate

head(meuse)

# M.L : inserire per primo il dataset e poi speigo che ho x y , gli ho spiegato quali sono le mie coordinate
# M.L : uso ~ per gruppo di coordinate

coordinates(meuse)=~x+y

plot(meuse)

# M.L : inserisco nella funzione il dataset per creare grafico spaziale grazie a spplot (plotto i dati spazialmente)
# M.L : grafico= come si ditribuisce lo zinco attorno al fiume, vicino all'acqua (giallo) valori molto alti, lontani dal fiume puntini neri valore basso

spplot(meuse,"zinc")




########################################################################################################
########################################################################################################
########################################################################################################



### 3. R_code_spatial_2

install.packages("sp")
library(sp)

# INIZIO
# M.L : installo "sp" se non lo ho ancora
# M.L : se lo ho gia lo carico 

library(sp)

# M.L : carico poi dataset meuse

data(meuse)

# M.L : coordinate dataset x e y nel dataset meuse

coordinates(meuse)=~x+y

# M.L : poi creo sp plot(funzione della libreria sp) con le varibili con le virgolette

spplot(meuse,"zinc")

# M.L : ESERCIZIO: stessa cosa ma con il rame 
# M.L : il grafico del rame ha dei valori piuttosto alti nella parte vicino al fume e piu bassi allontanandosi dal fiume 
# M.L : il fiume è molto inquinato 

spplot(meuse,"copper")

# M.L : funzione bubble nel pacchetto sp, rappresento uguale a spplot MA i valori piu alti hanno bolle piu grandi 

bubble(meuse,"zinc")

# ESERCIZO: bubble di rame colorato di rosso

bubble(meuse,"copper",col="red")

# ESERCIZIO, creo due oggetti e li plotto insieme

# M.L : Lucia: foraminiferi con diversi plot
10,20,35,55,67,80

# M.L : questo è un array e si scrive con c
c(10,20,35,55,67,80)

# M.L : lo chiamo foram

foram <- c(10,20,35,55,67,80)

# M.L : ho creato un oggetto

# M.L : MARCO plot per carbon sequestro
5,15,30,70,85,99

carbon <- c(5,15,30,70,85,99)

# M.L : ci siamo inventati un data frame e li incollo su R entrambi 
# M.L : faccio un plot
# M.L : vedo che c'è una stretta relazione tra l'abbondanza dei foraminiferi e il sequestro di CO2

plot(foram,carbon,col="green",cex=2,pch=3)

# ESERCIZIO
# M.L : faccio dialogare R con dati dall'estreno, cioe con il computer
# M.L : in questo caso dati del covid 19, creo la cartella
# M.L : dico ad R quale cartella col comando "setworkingdirectoy" = setwd

setwd("~/Desktop/Eco del Paesaggio/LAB") 


# M.L : leggere la tabella
# M.L : la tabella prima ha i titoli delle colonne e non i dati, quindi gli spiego che ho un Header(testa)
# M.L : con head=T  (TRUE)  , cioè so che c'è un titolo 

covid <- read.table("covid_agg.csv",head=TRUE)



########################################################################################################
########################################################################################################
########################################################################################################


### 4. R_code_piont_pattern
      

install.packages("ggplot2") 
install.packages("spatstat")
install.packages("rgdal")

library(ggplot2)
library(spatstat)
library(rgdal)
      

# ANALISI POINT PATTERNS

# M.L : metto subito ed indico i pacchetti che ho dovuto scaricare per l'esercitazione
install.packages("ggplot2") 
install.packages("spatstat")

library(ggplot2)
library(spatstat)

# M.L : importo la working directory
setwd("~/Desktop/Eco del Paesaggio/LAB")  

# M.L : rinomino covid_agg in covid

covid <- read.table("covid_agg.csv", head=T)  # per importare dati di una tabella
covid <- covid_agg
head(covid)

#  M.L : plot mi fa un grafico con due variabili 

plot(covid$country,covid$cases)    # M.L : $ collega un pezzo ad un altro, in questo caso collega la colonna al proprio dataset altrimenti non riconosce la colonna
plot(covid$country,covid$cases,las=0)  # M.L : per metterlo verticale
plot(covid$country,covid$cases,las=1)  # M.L : las=1 le etichette di asse Y diventano orizzontali
plot(covid$country,covid$cases,las=2)  # M.L : las=2 le etichette della asse X sono verticali
plot(covid$country,covid$cases,las=3)  # lM.L : aberls verticali

# M.L : "cex.axis" per rimpicciolire le scritte di asse X
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5)


# M.L : ggplot2

install.packages("ggplot2") 
library(ggplot2)

# M.L : mpg è un dataset di prassi già all'interno di ggplt2
data(mpg)
head(mpg)

# M.L : Quello che serve a ggplot2 per creare un grafico è 1. data set 2. aestetics, cioè le variabili 3. La geometria con cui si vuole visualizzare

ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

# M.L : ggplot di covid
# M.L : prendo come dataset=covid, variabili=longitudine e latitudine
# M.L : aggiungo size=cases per aggiungere i puntini al grafico riguardanti i casi nei diversi continenti 

ggplot(covid,aes(x=lon,y=lat)) + geom_point()    
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point() 

# M.L : Programma per fare grafic SPATSTAT
install.packages("spatstat")
library(spatstat)

# M.L : density intende la densità dei punti in una mappa
# M.L : create dataset for spatstat
# M.L : "ppp" devo mettere (variabile x, var.y, rage della x, range della y)
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

# M.L : grafico DENSITÀ

d <- density(covids)
plot(d)
points(covids)

# Parte 2 (lezione del 1 Aprile)

# M.L : imposto i dati di ieri
# M.L : con RStudio è gia tuto salvato
setwd("~/lab/")
load("point_pattern.RData")
# M.L : carico la libreria spatstat
library(spatstat)

plot(d)

# M.L : voglio cambiare la gamma di colori del grafico di densità=palette di colori
# M.L : dò un nome alla palette, creo una gamma(arrey) di colori dopo la parentesi (c('colore1','colore2',...)
# M.L : cl= colour (lo ho scleto io per comodità)
# M.L : si usa una virgoletta
# M.L : qunati mini livelli tra un colore all'altro? piu ne ho meglio è cosi ho una maggior gradazione es. (100) = 100 gradazioni
cl <- colorRampPalette(c('yellow','orange','red'))(100)

# M.L : Faccio il grafico di denistà e gli specifico che colori volgio(quelli che ho fatto io)
plot(d,col=cl)

# ESERCIZIO, plot della mappa dal verde al blu

cl1 <- colorRampPalette(c('green','blue','purple'))(100)
plot(d,col=cl1)

# M.L : con points posso poi inserire i punti di ieri del COVIDS
point(covids)
# M.L : posso anche mettere i confini degli altri stati
# M.L : coastlines = nome del nuovo file
# M.L : readOGR è parte di una libreria GDAL (libreia Geospaziale che permette di leggere qualsiasi tipo di file raster o vettoriale)
# M.L : rGDAL libreria di R
# M.L : installo con le virgolette perche devo uscire da GitHub
install.packages("rgdal")
library(rgdal)
# M.L : i file devono essere nella cartella LAB liberi
coastlines <- readOGR("ne_10m_coastline.shp")
# M.L : adesso plotto i dati 
plot(coastlines,add=T)

# ESERCIZIO <- plot della mappa con nuova colorazione ed aggiunta della coastlines

cl1 <- colorRampPalette(c('blue','purple','light green','yellow'))(100)
plot(d,col=cl1)
plot(coastlines,add=T)

# M.L : col=yellow, setto i confini dei continenti col giallo 
plot(coastlines,add=T,col=yellow)


########################################################################################################
########################################################################################################
########################################################################################################


### 5. R_code_teleril
      

      
install.packages("raster")
library(raster)
    
      
      
# CODICE R PER ANALISI DI IMMAGINI SATELLITARI 

install.packages("raster")

library(raster)

# M.L : setto working directory 

setwd("~/Desktop/Eco del Paesaggio/LAB")


# M.L : Importo immagine all'interno di R => p224r63 (con il path e la row)
# M.L : associo l'immgaine alla funzione BRICK

p224r63 <- brick("p224r63_2011_masked.grd")

plot(p224r63)

# M.L : Ho B1,B2,... cioe diverse bande con diverse riflettanze nelle diverse lunghezze d'onda
# M.L : in tutto 7 sensori 
# M.L : B1: blue
# M.L : B2: green
# M.L : B3: red
# M.L : B4: near infrared (nir)
# M.L : B5: medium infrared
# M.L : B6: thermal infrared
# M.L : B7: medium infrared

# PARTE 2
# M.L : se ho chiuso R e devo ricaricare i dati 

setwd("~/Desktop/Eco del Paesaggio/LAB")

load("Lezione 7 Aprile.RData")

p224r63_2011 <- p224r63


plot("p224r63_2011")

# M.L : non me lo da perche bisogna caricare la libreria "raster"
# M.L : cambio la colorazioe del Plot con "ColorRampPalette". Metto poi con c le colorazioni che voglio 

cl <- colorRampPalette(c('black','grey','light grey'))(100)

plot(p224r63_2011,col=cl)

# M.L : con 5 i colori appaiono molto piu sgranati 

cllow <- colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011,col=cllow)

# M.L : PLOT della banda del blu
# M.L : names per vedere tutti i nomi degli oggetti che stiamo utilizzando

clb <- colorRampPalette(c('dark blu','blue','light blue'))(5)

# M.L : attach(dataframe) non funziona con il pacchetto raster e allora devo usare il $ per B1_rse(cioe il sensore banda blu)

plot(p224r63_2011$B1_rse,col=clb)

# ESERCIZIO con B4_sre e range di colori rosso,arancione e giallo

clnir <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_2011$B4_sre, col=clnir)


# M.L : Faccio un multiframe di 4 bande con funzione "par" che mi permette di dividere in un pannello i vari grafici
# M.L : mfrow,c(2,2) = cioe due righe e due colonne 
par(mfrow,c(2,2))

# M.L : blue (in alto a sx)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)

# M.L : green (in alto a dx)
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_2011$B2_sre, col=clg)

# M.L : red (in basso a sx)
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_2011$B3_sre, col=clr)

# M.L : Infrarosso
clnir <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_2011$B4_sre, col=clnir)

# M.L : i valori sono molto alti nel NIR ed idica un elevata presenza di piante
# M.L : i valori del blu e del rosso invece sono valori bassi (perche assorbite dalle inate per la fotosintesi)

# M.L : Imaggine come la vederebbe un occhio umano, unendo le bande rossa,blu e verde

dev.off() # M.L : per chiudere le immagini che ho appena plottato 

# M.L : natural colours (come occhio umano), componenti R,G, e B ( computer plotta 3 bande per volta)

plotRGB(p224r63_2011, r=3, g=2, b=1)

# M.L : appare nera l'immagine e allora devo strecchare i colori e l'immagine 
# M.L : stretch=lin cioè lineare

plotRGB(p224r63_2011, r=3, g=2, b=1,stretch="Lin")

# M.L : immagine come la vedrebbe occhio umano ma la vegetazione fitta puo essere confusa con le ombre
# M.L : allora aggiungo anche NIR ( tramite r=4 ) 4= infrarosso vicino 
# M.L : scalo tutti i colori di uno per formare FALSE COLOR

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")

# M.L : IMMAGINE formata : le piante diventeranno di colore rosso 

par(mfrow,c(1,2)) # M.L : 2 immagini una sotto all'altra
plotRGB(p224r63_2011, r=3, g=2, b=1,stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")

plotRGB(p224r63_2011, r=3, g=4, b=2,stretch="Lin") # M.L : per avere l'immagine dell'infrarosso nel verde, cosi da vedere la vegetazione




# PARTE 3
# M.L : ricarico tutti i dati
      
install.packages("raster")

library(raster)

setwd("~/Desktop/Eco del Paesaggio/LAB")

load("teleril.RData")

# M.L : posso fare una lista per vedere tutti i dati salvati precedentemente
# M.L : a me interessa p224r63_2011
 ls()

# M.L : importo adesso il file del 1988 
# M.L : uso comando brick per importare tutte le varie bande dell'immagine satellitare

p224r63_1988 <- brick("p224r63_1988_masked.grd")

# M.L : uso ancora par,mfrow per arere piu immagini nella stessa frame
# M.L : uso poi colorramp palette

par(mfrow,c(2,2))

# M.L : blue (in alto a sx)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_1988$B1_sre, col=clb)

# M.L : green (in alto a dx)
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_1988$B2_sre, col=clg)

# M.L : red (in basso a sx)
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_1988$B3_sre, col=clr)

# M.L : Infrarosso
clnir <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_1988$B4_sre, col=clnir)


# M.L : Imaggine come la vederebbe un occhio umano, unendo le bande rossa,blu e verde

dev.off() # M.L : per chiudere le immagini che ho appena plottato 

# M.L : natural colours (come occhio umano), componenti R,G, e B ( computer plotta 3 bande per volta)

plotRGB(p224r63_1988, r=3, g=2, b=1)

# M.L : appare nera l'immagine e allora devo strecchare i colori e l'immagine 
# M.L : stretch=lin cioè lineare

plotRGB(p224r63_1988, r=3, g=2, b=1,stretch="Lin")

# M.L : immagine come la vedrebbe occhio umano ma la vegetazione fitta puo essere confusa con ombre
# M.L : allora aggiungo anche NIR ( tramite r=4 ) 4= infrarosso vicino 
# M.L : scalo tutti i colori di uno per formare FALSE COLOR

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")

# M.L : IMMAGINE formata : le piante diventeranno di colore rosso 

# M.L : adesso faccio un plot delle due immagini( 2011 e 1988) in un multiprame(par mfrow(multiframe row))
# M.L : main (è per aggiungere un titolo ai grafici)
par(mfrow=c(2,1))

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_1988, r=4, g=3, b=2,stretch="Lin")

# M.L : SPECTRAL INDEX
# M.L : posso calcolare un indice per vedere come stà la vegetazione
# M.L : pinata sana riflette molto nell'INFRAROSSO (alta riflettanza) e poco nel blu e rosso
# M.L : uso l'indice chiamato DVI (different vegetation index)
# M.L : se sottraggo i valori delle due bande (Nir - Rosso) 
# M.L : pinata sana (Nir=90, R=10 -> DIV=80 ) pinata malata o poche pinate (Nir=70, R=30 -> DIV=40)
# M.L : dvi1988= nir1988-red1988
# M.L : $ per collegare le bande all'immagine

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre

cldvi <- colorRampPalette(c('light blue','light green','green'))(100) 
plot(dvi2011,col=cldvi)

# M.L : posso anche vedere la differenza tra diversi DVI, se è un valore positivo= MIGLIORAMENTO se è negativo=PEGGIORAMENTO
# M.L : es. dvi2011-dvi1988= 40 - 80 = - 40 (indice negativo)

difdvi <- dvi2011-dvi1988

plot(difdvi)
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) 
plot(difdvi,cl=cldifdvi)
# M.L : tutte le zone dove è stata tagliata la vegetzione è in colore rosso
# M.L : colore blu invece pinate che stanno meglio 
# M.L : "binaco" situzione stabile 

# M.L : voglio 3 immagini di fila [mfrow=c(3,1)], immagine del 2011, del 1988 e l'immagine di confronto del DVI

par(mfrow=c(3,1))

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_1988, r=4, g=3, b=2,stretch="Lin")
plot(difdvi,cl=cldifdvi)

dev.off() # M.L : per chiudere

# M.L : cambio la risoluzione, cambio la diensione dei pixel con funzione "aggregate"= (cosa cambio, fact(quanto la cambio, se metto es. 10 aumenta i pixel di 10 volte))
# M.L : lr (low resolution)

p224r63_2011 # M.L : vedo tutte le info tra cui anche la risoluzione (che è 30m)
p224r63_2011lr # M.L : invece ha 300 al posto che 30 (perche è stata moltiplicata per 10)

p224r63_2011lr <- aggregate(p224r63_2011,fact=10) # immagine che sembra essere miopi

# M.L : metto a confronto le due immagini a 30 m e a 300m
par(mfrow=c(2,1))

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2,stretch="Lin")

# M.L : provo a diminuitre ancora di piu la risoluzione (risoluzione 1500m con fattore=50)

p224r63_2011lr50 <- aggregate(p224r63_2011,fact=50)

par(mfrow=c(3,1)) # M.L : plotto le 3 immagini a diverse risoluzioni

plotRGB(p224r63_2011, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2,stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2,stretch="Lin")

dev.off()

# M.L : calcolo il dvi per il low resolution del 2011

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre)
plot(dvi2011lr50)

# M.L : Diminuisco la risoluzione dell'immagine del 1988 (risoluzione 1500m con fattore=50)

p224r63_1988lr50 <- aggregate(p224r63_1988,fact=50)

dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre)

# M.L : differenza dvi a low resolution del 2011 e 1988 e lo plotto 

difdvilr50 <- dvi2011lr50-dvi1988lr50
plot(difdvilr50, col=coldifdvi)

# M.L : multiframe con due risultati
# M.L : perche? perche bisogna utilizzare la grana/scala giusta!!

par(mfrow=c(2,1))
plot(difdvi,cl=cldifdvi)
plot(difdvilr50, col=coldifdvi)


########################################################################################################
########################################################################################################
########################################################################################################



### 6. R_code_landcover



install.packages("RStoolbox")
install.packages("raster")
install.packages("rgdal")
install.packages("spatstat")
library(RStoolbox)
library(raster)
library(rgdal)
library(spatstat)


# M.L : Argomento = land cover 

setwd("~/Desktop/Eco del Paesaggio/LAB")

install.packages("RStoolbox")
library(RStoolbox)
library(raster)

# M.L : brick= impila i dati e portali su R

p224r63_2011 <- brick("p224r63_2011_masked.hdr")

# M.L : con le tre componenti, red=4 del vicino infrarosso,green=nel rosso, blue

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# M.L : della nostra immagine le classi sembrano 4
# M.L : p224r63_2011c = "c" nel senso classificato

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# M.L : plotto la mappa       $map = mappa generata
# M.L : valori pixel da 1 a 4

plot(p224r63_2011c$map)

# M.L : ma cambio i colori(che me li ha messi di standard R)

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 

plot(p224r63_2011c$map, col=clclass)

# M.L : proviamo a mettere le classi uguali a 2 invece che 4

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

# M.L : in funzione del numero di classi aumenta l'incertezza
# M.L : possiamo fare piu mappe con piu o meno classi per vedere le difernze tra i cluster dei pixel 
# M.L : con due classi incertezza bassa


# M.L : PARTE 2 (22 Aprile)

# M.L : ricarico il grafico dei POINT PATTERN per poter fare l'interpolazione sui valori dei casi del covid

library(spatstat)
library(rgdal) # M.L : for the coastlines

setwd("~/lab/")
load("point_pattern.RData")
ls()

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)



# M.L : interpolazione

# M.L : la cosa da fare. Guardare la tabella dei dati e vedere quale variabile mi interessa
# M.L : head(covid)   a me interessa colonna "cases"
# M.L : funzione marks = valori che do ai dati del point pattern e lo associo alla colonna cases

marks(covids) <- covid$cases

# M.L : s ( per stima )
# M.L : smooth dei punti spaziali del covid

s <- Smooth(covids)

plot(s)

# M.L : aggiungo titolo, colori diversi(colourpalette), aggiungo i punti e aggiungo coastiles al plot s

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# M.L : GRFICO = stima dei casi. Molto alta verso DX(zona cina) 

text(covids)    # M.L : per aggiungere il valore dei punti 


# M.L : MAPPA FINALE
# M.L : paragonare i due grafici plottati, quello sulla densita e quello sull interpolazione

par(mfrow=c(2,1))

# M.L : densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# M.L : interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

 


# M.L : TESI SAN MARINO
# M.L : setto la working directory
# M.L : prima cosa carico i dati 

load("Tesi.Rdata")
ls()  # M.L : per vedere cosa c'è dentro
head(Tesi)

# M.L : grafico densita dei punti perche come varibili ho le coordinate

library(spatstat)  # M.L : per usare ppp

# M.L : devo fare un ppp per crearlo mi serve coordinata x, coord.y, c(xmin,xmax),c(ymin,ymax) cioe i miniti della x e della y
# M.L : uso funzione che si chiama summery che mi dice i valori min e max delle coordinate
# M.L : x va da 12.42 a 12.46 (mettero in realta 12.41 e 12.47 per stare un po piu larghi)
# M.L : y da 43.91 a 43.93 ( aumento di 1 per estendere)

attach(Tesi) # M.L : super importante

summary(Tesi)

Tesippp <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.94))

# M.L : grafico della densità

dT <- density(Tesippp)

plot(dT)

points(Tesippp,col="green")



# M.L : SAN MARINO parte 2 

setwd("~/Desktop/Eco del Paesaggio/LAB")
load("SAN MARINO.RData")

ls() # M.L : cosi vedo che dati ho all'interno
     # M.L : dT = è la density map
     # M.L : Tesi = era un dataset
     # M.L : Tesippp = point pattern

library(spatstat)
     
plot(dT)
points(Tesippp, col="green") # M.L : grafico che avevo gia otteuto con densita piu elevata verso il centro

# M.L : quale è la ricchezza specifica? Per fare INTERPOLAZIONE
# M.L : prima faccio head e vado a vedere che dati ho
# M.L : campo "species richness"
head(Tesi)

# M.L : marks, valori della variabile che voglio interpolare associata ai valori del PPP(point pattern che adesso non è associato a nulla)

marks(Tesippp) <- Tesi$Species_richness

# M.L : FUNZIONE SMOOTH = INTERPOLATORE!! mappa raster a partite da volori di punti 
# M.L : due punti misurati, calcolo la media tra i due e vado avanti cosi

interpol <- Smooth(Tesippp)

plot(interpol)
points(TESI,col="green") # M.L : per aggiungere i punti

# M.L : i valori di ricchezza specifica sono distribuiti diversamente dalla densità
# M.L : valori piu alti nella parte centrale e sud-est

# M.L : AGGIUNGO FILE VETTORIALE SI SAN MARINO 

# M.L : library RGDAL per leggere immagini vettoriali 
library(rgdal)

# M.L : OGR = file vettoriale
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino) # M.L : è un poligono
# M.L : aggiungo i punti di interpol sopra a sanmarino
# M.L : aggiungo anche i punti
plot(interpol,add=T) # M.L : add=T per aggiungere dei pezzi alla mappa precedente
points(Tesippp,col="green")
plot(sanmarino,add=T) # M.L : per sovraporre e vedere i confini di San Marino

# M.L : UNISCO I DUE GRAFICI (densità ed interpolazione) con una colonna e due file

par(mfrow=c(2,1))

plot(dT, main="Density of points")
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

# M.L : non cosidero pero l'uso del suolo, es, vicino c'è zona urbana dove avro una bassa ricchezza specifica

# M.L : STESSI FILE UNITI MA SU DUE COLONNE E UNA RIGA
par(mfrow=c(1,2))

plot(dT, main="Density of points")
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")



########################################################################################################
########################################################################################################
########################################################################################################



### 7. R_code_multitemp



install.packages("raster")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("RStoolbox")

library(raster)
library(ggplot2)
library(gridExtra)
library(RStoolbox)


# M.L : Ananisi multitemporali con terreno suddiviso in varie classi di copertura del suolo 

setwd("~/Desktop/Eco del Paesaggio/LAB")

library(raster)

# M.L : uso BRICK per caricare tutte le singole bande di immagini satellitari 
# M.L : carico le immagini riguardanti le deforestazioni 1 e 2 

defor1 <- brick("defor1_.png")
defor2 <- brick("defor2_.png")

# M.L : DEFOR1 ho tre bande, metto Infrarosso vicino alla banda R, nella componente G inserisco la banda R, nella componente R inserisco la banda G

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin") # immagine della foresta pluviale dove le piante sono in rosso
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# M.L : faccio due classi per classificare tutto quello che è forsta
# M.L : funzione unsuperclass è per creare le classi nonn supervisionate (non gli diamo un imput)
# M.L : devo caricare perô RStoolbox

install.packages("RStoolbox")
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2) # è una $map

d1c
# M.L : d1c$map è la mia mappa

plot(d1c$map)

# M.L : cambio i colori 

cl <- colorRampPalette(c('black','green'))(100) # ho la foresta in verde e tutto il resto in nero
plot(d1c$map, col=cl)


# M.L : Classifico anche la seconda immagine

d2c <- unsuperClass(defor2, nClasses=2) 
d2c
# M.L : d2c$map è la mia mappa

plot(d2c$map)
cl <- colorRampPalette(c('black','green'))(100) # M.L :  ho la foresta in verde e tutto il resto in nero
plot(d2c$map, col=cl)

dev.off()

plotto le due immagine appena ottenute
par(mfrow=c(2,1))
plot(d2c$map, col=cl)
plot(d1c$map, col=cl)


# M.L : QUANTIFICO ADESSO LA PERCENTUALE di foresta persa (in base al numero di pixels appartenenti ad ogni classe)
# M.L : MAPPA 1

freq(d1c$map) # M.L : mi conta i pixel per ogni classe
              # M.L : n.di pixel area foresta = 305095
              # M.L : n.di pixel area aperta = 36197
              
# M.L : calcolo il totale e poi le proporzioni (che x100 mi da la percentuale) tra le due classi
# M.L : freq = freq della mappa per 100/il totale

totd1 <- 305095+36197
totd1   # M.L : mi da 341292

percent1 <- freq(d1c$map) * 100 / totd1
percent1   # M.L : mi mostra le percentuali (89.4% e 10.6%)

# M.L : MAPPA 2

freq(d2c$map) # M.L : frequenza 2 è la classe della foresta
totd2 <- 178625+164101
totd1 
percent2 <- freq(d2c$map) * 100 / totd2
percent2 # M.L : che è 47.8% e 52.2%(di foresta)

# M.L : CREO UN DATAFRAME, una picoola tabella con i vari volri di percentuali

cover <- c("Agriculture","Forest")
before <- c(10.6,89.4)
after <- c(47.8,52.2)
# M.L : creo le colonne con la cover, prima del disboscamento e dopo il disb.

output <- data.frame(cover,before,after)

# M.L : ADESSO devo plottare i valori
# M.L : richimao "ggplot2"

library(gglpot2)

# M.L : GIORNO DUE
# M.L : ricarico dati RData e Working directory

load("~/Desktop/Eco del Paesaggio/LAB/defor.RData")
setwd("~/Desktop/Eco del Paesaggio/LAB")
ls()   # M.L : d1c e d2c sono i due grafici che mi interessano

# M.L : ri visualizzo i grafici dell'altra volta
par(mfrow=c(2,1))
cl <- colorRampPalette(c('black','green'))(100)
plot(d2c$map, col=cl)
plot(d1c$map, col=cl)



library(ggplot2)
# M.L : grafico con ggplot = ISTOGRAMMA DELLA % DI FOREST COVER 
# M.L : aes= aestetichs
# M.L : colore basato sulla cover 

# M.L : p1 DEFORESTAZIONE BEFORE

p1 <- ggplot(output, aes(x=cover,y=before,color=cover)) + geom_bar(stat="identity",fill="white")
plot(p1)

# M.L : p2 DEF.AFTER

p2 <- ggplot(output, aes(x=cover,y=after,color=cover)) + geom_bar(stat="identity",fill="white")
plot(p2)

install.packages("gridExtra")
library("gridExtra")

# M.L : grid.arrange(plot1,plot2,nrow=1) = due grafici nella stessa finestra 

grid.arrange(p1,p2,nrow=1)


# M.L : AGGIUNGO UN LIMITE SULL'ASSE Y per visualizzare meglio i grafici con YLIM

p1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

p2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)



########################################################################################################
########################################################################################################
########################################################################################################



### 8. R_code_multitemp_NO




install.packages("raster")
library(raster)




# M.L : PER ANALIZZARE LE CONCENTRAZIONI DI CO2
# M.L : preso i dati dal file zip (dati di NO2 dall'ESA) e ho messo i dati senza cartella nella cartella LAB

setwd("~/Desktop/Eco del Paesaggio/LAB")
# M.L : importo poi le immagini con la funzione raster
# M.L : importo la prima immagine 

EN01 <- raster("EN_0001.png")
plot(EN01)    # M.L : SE HO GRAFICO PICCOLO SCIRVO    dev.off()

# M.L : importo le altre
# M.L : se metto lo zero prima del numero me le mette tutte in ordine

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

# M.L : CON DEV.OFF() TORGLO LA PAR

# M.L : faccio una differenza 

diff <- EN13-EN01
cldif <- colorRampPalette(c("blue","black","yellow"))(100)
plot(diff,col=cldif)

# M.L : plotto tutte le immagini, 13 pero è un numero primo e per farle stare tutte in un PAR devo fare 4x4

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

# M.L : come faccio ad imporatre tutti ifile allo stesso tempo? SE HO TANTI FILES!!
# M.L : devo creare una cartella all'interno della cartella LAB e cambiare la working directory
# M.L : metto tutte le immagini nella nuova cartella "esa_no2" e la imposto come working directory

library(raster)
setwd("~/Desktop/Eco del Paesaggio/LAB/esa_no2")
rlist <- list.files(pattern=".png")
rlist         # M.L : appare la lista di tutti i files con estensione .png

# M.L : funzione lapply (che si legge l applay) applica una funzione su una serie di elementi(una lista di files)
# M.L : la funzione che volgio applicare sarà brick(per importare piu layers) oppure raster(per un singolo layer) per caricare le mie immagini
# M.L : (rlist,raster) funzione raster applicata alla lista che ho creato prima

listafinale <- lapply(rlist,raster)
listafinale   # M.L : visualizzo 13 elementi in lista con le singole bande

# M.L : adesso faccio il plot col par(4,4)
# M.L : PER PRIMO DEVO USARE funzione stack per unire tutte le bande per creare un pacchetto di dati (e per poter fare poi PLOT)
# M.L : unisco le immagini dove ogni banda equivale a un TEMPO diverso

EN <- stack(listafinale)

cl <- colorRampPalette(c("red","orange","yellow"))(100)
plot(EN,col=cl)


######### DAY 3 (12 amggio)

library(sp)
library(raster)
setwd("~/Desktop/Eco del Paesaggio/LAB/esa_no2")  # "esa_no2" sarà la mia working direcorty
rlist <- list.files(pattern=".png")

 # M.L : adesso faccio la differenza dei pixel tra le immagini EN01 ed EN13 e lo plotto

difEN <- EN$EN0013 - EN$EN0001
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(difEN, col=cld)

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)   # M.L : per plottare poi tutta la lista di immagini raster

# M.L : FUNZ. STAT. BOX PLOT = individua il range dei dati con mediana e media
# M.L : ogni immagine EN avrà il proprio boxplot con i suoi valori di mediana e di max e min
# M.L : cambiamento contingente sui valori massimi! I valori medi sono diminuiti di poco

boxplot(EN)
boxplot(EN, horizontal=T) # M.L : per avere i box in orizzontale
boxplot(EN, horizontal=T,outline=F)  # M.L : per rimuovere gli OUTLAINER (punti lontani dalla media)
boxplot(EN, horizontal=T,outline=F,axes=T) # M.L : aggiungo gli assi (anche se ci dovrebbero essere di default)




########################################################################################################
########################################################################################################
########################################################################################################




### 9. R_code_SNOW




install.packages("ncdf4")
install.packages("raster")
library(ncdf4)
library(raster)




### ESERCIZIO CON COMPERNICUS
# M.L : caricare file assieme
# M.L : differenza tra immagini
# M.L : mappa di previsione

setwd("~/Desktop/Eco del Paesaggio/LAB")

install.packages("ncdf4")

library(ncdf4)
library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

plot(snowmay,col=cl)

# M.L : Scarico zip file 
# M.L : Importo adesso i dati dalla zip.snow delle varie immagini in diversi anni
# M.L : IMPOSTO la cartella SNOW come working directory per poter importare insieme tutte le immagini perche per importarsi insieme devono essere tutti dentro ad una cartella
# M.L : USO funzione lapply

setwd("~/Desktop/Eco del Paesaggio/LAB/SNOW")

rlist <- list.files(pattern=".tif", full.names=T)

# M.L : lapply apllica dei comandi a degli interi lista di file. Nel nostro caso è la funzione raster
list_rast <- lapply(rlist, raster)

# M.L : vogliamo creare uno stack
snow.multitemp <- stack(list_rast)        # M.L : snow.multitemp (come time snow nel tempo)

plot(snow.multitemp)       # M.L : colori a caso, li cambio con una colourramp palette

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snow.multitemp,col=cl)




####### M.L : evidenzio le differnze tra le immagini e poi faccio delle previsioni
# M.L : plotto con Par la prima e l'ultima immagine 
# M.L : PRIMA IMMAGINE <- plotto i file snow.multitemp e lo lego col $ all'immagine 2000
# M.L : SECONDA IMMAGINE <- plotto i file snow.multitemp e lo lego col $ all'immagine 2020

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# M.L : valore nella legenda sono diversi però, uno arriva a 250 e l'LTRO A 200
# M.L : uso funzione ZLIM (limiti assi)

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))    # M.L : adesso sono visualmente comparabili

####### M.L : faccio adesso una DIFFERENZA tra le due mappe con la funzione DIF e metto una color ramp 

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 

dev.off() # M.L : per eliminare il par di prima 

plot(difsnow,col=cldif)

####### M.L : faccio adesso una PREVISONE(SCENARIO) multimtemporale per vedere come sarà nel 2025 la copertura nevosa di una determinata misura
# M.L : dati basati sullo scarto quadratico medio (linea che unisce i miei dati e puo essere lineare, curva,seno-coseno,...)
# M.L : facile se c'è una variazione lineare da un tempo all'altro 
# M.L : magari i dati formano una curva a differenziale negativo
# M.L : magari i dati sono ciclici (funz.seno-coseno), es.stagionali, o glaciaioni(eventi ciclici)

# M.L : per fare la predizione ho scricato da IOL file prediction.r e salvata nella cartella SNOW

# M.L : funzione SOURCE per fare girare uno scrip su R prelevandolo dal desktop (cariare dati dall'esterno)

source("prediction.r")  # M.L : impiega un casino di tempo

# M.L : prof aveva creato file predicted.snow.2025.TIF e lo scarico e lo metto nella cartella SNOW per risparmiare tempo
# M.L : SCENARIO 2025

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
 
plot(predicted.snow.2025.norm, col=cl)
 

########################################################################################################
########################################################################################################
########################################################################################################




### 10. R_code_patches




install.packages("raster")
install.packages("igraph")
install.packages("ggplot2")
library(raster)
library(igraph)
library(ggplot2)



##########
# M.L : setto la working directory 

setwd("~/Desktop/Eco del Paesaggio/LAB")

library(raster)

# M.L : per caricare i dati raster, 2 funzioni:
# M.L : funzione BRICK : per caricare tutte le bande
# M.L : funzione RASTER : per caricare un singolo livello
# M.L : non uso lapply perche sono solo due mappe 

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

# M.L : plottiamo i due file per vedere la loro composizione
# M.L : metto due colori(es.una per la foresta e una per campi agricoloi)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c,col=cl)
plot(d2c,col=cl)     # M.L : ma è sbagliato il colore, sono invertiti, allora inverto <- colorRampPalette(c('black','green'))(100) 

cl <- colorRampPalette(c('black','green'))(100) 

#### M.L : la foresta è la classe numero 2, agriculture è la classe numero 1
# M.L : adesso voglio che la classe 1(agricoltura) abbia un valore nullo(annullando tutto quello che non è foresta)estraendo solo la foresta
# M.L : cosi da poter fare calcoli solo con i dati della foresta
# M.L : funzione RECLASSIFY(paccheto raster)= riassegna dei valori con cbind (classe 1 divemta NA)
# M.L : d1c.for = foresta

d1c.for <- reclassify(d1c,cbind(1,NA))
# M.L : rilancio una par 

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) 
plot(d1c,col=cl)
plot(d1c.for)     # M.L : di colore giallo

plot(d1c.for,col=cl)   # M.L : cambio il colore in verde con la classe agricola nulla in bianco

# M.L : annullo la classe 1 anche sulla seconda immagine

d2c.for <- reclassify(d2c,cbind(1,NA))

par(mfrow=c(1,2))
plot(d1c)
plot(d2c)   # M.L : due mappe solo con le foreste 


################ M.L : Calcolo il numero di PATCHES

# M.L : funzione CLUMP
# M.L : la applico ad esempio alla prima mappa

install.packages("igraph")
library(igraph)

d1c.for.pacthes <- clump(d1c.for)
d2c.for.pacthes <- clump(d2c.for)

writeRaster(d1c.for.pacthes, "d1c.for.patches.tif") # M.L : salvo dei dati verso l'esterno nella cartella LAB
writeRaster(d2c.for.pacthes, "d2c.for.patches.tif")

par(mfrow=c(1,2))
plot(d1c.for.pacthes)
plot(d2c.for.pacthes) # M.L : colori un pò brutti

# M.L : creiamo un'altra color ramp palette per aumentare un po la differenziazione

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) 
par(mfrow=c(1,2))
plot(d1c.for.pacthes, col=clp)
plot(d2c.for.pacthes, col=clp)

# M.L : se lancio su R solo "d1c.for.pacthes" mi da le informazioni

d1c.for.pacthes # M.L : posso andare a vedere i valori minimi e massimi (che sono il numero dei patch)

d1c.for.pacthes = 301 patches
d2c.for.pacthes = 1212 patches

time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

library(ggplot2)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")







########################################################################################################
########################################################################################################
########################################################################################################





### 11. R_code_crop




install.packages("ncdf4")
install.packages("raster")
library(ncdf4)
library(raster)





# M.L : setto SNOW come cartella della working directory
setwd("~/Desktop/Eco del Paesaggio/LAB/SNOW")

# M.L : IMPORTO I DATI 
# M.L : assicurarsi di non avere i dati da utilizzare su ICLOUD
rlist <- list.files(pattern=".tif", full.names=T)

# M.L : lapply apllica dei comandi a degli interi lista di file. Nel nostro caso è la funzione raster
list_rast <- lapply(rlist, raster)


# M.L : vogliamo creare uno stack
snow.multitemp <- stack(list_rast)        # M.L : snow.multitemp (come time snow nel tempo)


plot(snow.multitemp)       # M.L : colori a caso, li cambio con una colourramp palette


cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snow.multitemp,col=cl)

# M.L : 1. metodo per fare una zoom con "extension"
snow.multitemp
plot(snow.multitemp$snow2010r, col=cl)

extension <- c(6, 18, 40, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

# M.L : poi la cambio per aggiustare la crop 
extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

# M.L : 2. metodo per fare una zoom con "Drae extent"

plot(snow.multitemp$snow2010r, col=clb)
 

zoom(snow.multitemp$snow2010r, ext=drawExtent())

# M.L : 3. faccio una CROP dell'immagine

extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)

# M.L : effettuo crop su tutto lo stalk di dati
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb) 
# M.L : le legende pero nel plot non sono tutte uguali 
snow.multitemp.italy # M.L : per vedere i valori max e min
# M.L : utilizzo ZLIM
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))

boxplot(snow.multitemp.italy, horizontal=T,outline=F)
 





########################################################################################################
########################################################################################################
########################################################################################################


#### 12. R_code_species_distribution_modeling.r


install.packages(sdm)
library(sdm)

library(raster)
library(rgdal)

# carico il file che mette a disposizione SDM

file <- system.file("external/species.shp", package="sdm")
species <- shapefile(file) # carico tutta la parte grafica
species  # visualizzo le caratteristiche dei dati
species$Occurrence  # visualizzo la struttura del dataset formato da punti di presenza-assenza(1-0)


# plot dataset specie e dentro a tutti i dati in occurance uguali a uno = colore blu, e rossi se sono uguali a 0
# per il primo faccio un plot ma per aggiuungere altri punti devo fare funzione "points"


plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# inserisco le mie varibili ambinetali 

path <- system.file("external", package="sdm") # contine diversi dati 

# faccio una lista di dati con listfiles (tutti i dati che contengono "asc$)
lst <- list.files(path=path,pattern='asc$',full.names = T)
lst  # contiene 4 varibili (elevation,vegetation, temp e precipitation)
 # queste variabili ci serviranno poi per prevedere la distribuzione dei punti

preds <- stack(lst)
cl <- colorRampPalette(c('yellow','orange','red')) (100)
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

# plotto una delle variabili(elevation) e ci metto sopra i punti dell'inizio
plot(preds$elevation, col=cl) 
points(species[species$Occurrence == 1,], pch=16) # punti solo dove è presente
                                                  # si nota come la distribuzione è sopratutto in basse elevations

# plotto una seconda variabile(temp) e ci metto sopra i punti dell'inizio
plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)  # la specie non ama temperature troppo basse

# plotto i punti con la terza variabile(precipitations)
plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16) # situazione intermedia 

# plotto i punti con la quarta variabile(vegetation)
plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16) # preferisce una condizione ombreggiata

# alla fine, elevation bassa, temp alta, precipitaz. intermedie, favorevole alla coprtura della vegetazione

# MODELLO LINEARE generalizzato per predire le aree di probabilita di presenza date dai punti che ho e dalle varie variabili

d <- sdmData(train=species, predictors=preds) # d= dati
d

# creo un modello che ha calcolato nuovi punti in base ai punti inizali che ho dato e in base alle 4 variabili 

m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') # ~ è come scrivere = (ma è piu elegante)
p1 <- predict(m1, newdata=preds) # PREVISIONE

plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16) # mappa di distribuzione prevista per la specie





########################################################################################################
########################################################################################################
########################################################################################################




# EXAM PROJECT
















