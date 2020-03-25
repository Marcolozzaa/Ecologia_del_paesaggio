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
















































