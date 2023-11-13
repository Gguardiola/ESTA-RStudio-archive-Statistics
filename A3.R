#ESTADISTICA DESCRIPTIVA

#EXERCICI1

#Donades unes variables de pes i edat de segons que estudiants fes la frequencia absoluta i relativa de les variables pes i nom.

dades=read.csv("1.csv", sep=";", dec=".")

#TAMBE ES POT LLEGIR COM A FITXER.TXT

taula_pes=table(dades$Pes)
taula_pes

#47 54 55 58 60 65 69 75 77 85 89 
#1  1  1  2  1  2  1  2  1  1  1

prop.table(taula_pes)

#47         54         55         58         60         65         69         75         77         85         89 
#0.07142857 0.07142857 0.07142857 0.14285714 0.07142857 0.14285714 0.07142857 0.14285714 0.07142857 0.07142857 0.07142857 

taula_nom=table(dades$Noms)
taula_nom

#Abby Chomosuke      Gabi    German   Gustavo  Illojuan     Josep    Manuel     Maria     Mihai  Monicacu   Montoya   Mustafa  Zakarias 
#1         1         1         1         1         1         1         1         1         1         1         1         1         1 

prop.table(taula_nom)

# Abby    Chomosuke       Gabi     German    Gustavo   Illojuan      Josep     Manuel      Maria      Mihai   Monicacu    Montoya    Mustafa   Zakarias 
#0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857

#Escriu una instrucció en R que mostri tots els camps dels registres que tenen PES<60.

pes=(dades$Pes)
pes

#[1] 77 58 89 55 47 60 54 58 75 65 69 85 75 65

pes[pes<60]

#[1] 58 55 47 54 58

#Afegeix al data frame una nova variable de tipus factor, i anomena-la Pes2, que re-codifiqui PES segons el codi següent: Seixanta (si és més gran o igual que 60 ), Cinquanta-5 ( si és menor que 55 però més gran o igual que 40), i Quaranta (si és més petit que 40).

dades$pes2[pes>60]="Seixanta"
dades$pes2[pes>40 & pes<=55]="Cinquanta-5"
dades$pes2[pes<=40]="Quaranta"

dades$pes2

#Representa graficament amb un grafic circular i de barres Edat

pie(table(dades$Edat),col=rainbow(7), main = "Diagrama circular per la variable Edat")

barplot(table(dades$Edat), col=rainbow(7), xlab="Edat", ylab="Frequencies absolutes", main ="Diagrama de barres per la variable Edat")

#Representa amb un grafic de barres Pes i Altura

barplot(table(dades$Pes,dades$Altura),col=rainbow(10))

#Representa amb un histograma la variable Edat

hist(table(dades$Edat), col = "purple", main = "Histograma per la variable Edat", xlab="Edat", ylab="Frequencia")

#Representa un histograma de manera textual (grafic de tija i fulles) 
stem(table(dades$Pes))

#The decimal point is at the |
  
#  1 | 00000000
#  1 | 
#  2 | 000

#Un gràfic de tija i fulles, també conegut com a diagrama de tija i fulles és una representació clàssica de la distribució de dades quantitatives, similar a un histograma però en text, on les dades es divideixen en tija (generalment el primer o primers dígits del número) i fulla (l'últim dígit). El gràfic de tija i fulles en R pot ser útil quan es treballa amb poques observacions (15-150 punts de dades).

#Representa amb una caixa de bigotis per la variable edat.

boxplot(dades$Edat,xlab="Edat",main="Caixes i bigotis per la variable edat") 

#Calcula la mitjana i la mediana de cadascuna de les variables (Pes altura i edat)

mean(dades$Pes)

#[1] 66.57143
mean(dades$Edat)

#[1] 30.42857

mean(dades$Altura)

#[1] 1.692857
median(dades$Pes)

#[1] 65

median(dades$Edat)

#[1] 26

median(dades$Altura)

#[1] 1.67

#Mostra els quantils de la variable pes
quantile(dades$Pes)

#  0%  25%  50%  75% 100% 
#  47   58   65   75   89 

#Mostra el resum de la variable pes
summary(dades$Pes)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   47.00   58.00   65.00   66.57   75.00   89.00 

#LES MATEIXES INSTRUCCIONS ES PODEN APLICAR A TOTES LES ALTRES VARIABLES

#Calcula la variança, desviacio estandard i el rang interquartilic de la variable pes

var(dades$Pes,na.rm=TRUE)

#[1] 151.4945

sd(dades$Pes,na.rm=TRUE)

#[1] 12.30831

IQR(dades$Pes,na.rm=TRUE)

#[1] 17

#na.rm indica si els valors que manquen han de ser eliminats abans de l'anàlisi.

#Mostra els valors ordenats 

sort(dades$Pes) 

# [1] 47 54 55 58 58 60 65 65 69 75 75 77 85 89
 
#Calcula el percentatge de cadascuna de les altures segons el total i rodoneix a un decimal
round(prop.table(table(dades$Altura))*100,digits=1)

#1.6 1.62 1.63 1.65 1.69  1.7 1.74 1.78 1.83 1.85 
#7.1  7.1 21.4 14.3  7.1 14.3  7.1  7.1  7.1  7.1

#INFERENCIA ESTADISTICA

#PROBLEMA 2

#D'una certa població s'ha extret una mostra de 69 individus, el valor mitjà dels quals és 969. Se sap per altres experiències del mateix tipus, que la desviació típica val 14. Trobar intervals de confiança per al valor mitjà de la població als nivells de confiança del 0.95 i 0.99.

el=0.05
n=69
variansa=14^2
meida=969
quantil=qnorm(1-el/2)

limon=meida-quantil*sqrt(variansa)/sqrt(n)
limon

#[1] 965.6967

limof=meida+quantil*sqrt(variansa)/sqrt(n)
limof

#[1] 972.3033

#Per tant, es té el 95% de confiança que l'interval [965.6967, 972.3033] contingui el valor mitjà de la població

#P[965.6967 ≤ μ ≤ 972.3033 ] = 0.95


#Per al nivell de confiança del 0.99, l'única cosa que hem de canviar és el valor de α

el=0.01
quantil=qnorm(1-el/2)

limon=meida-quantil*sqrt(variansa)/sqrt(n)
limon

#[1] 964.6587

limof=meida+quantil*sqrt(variansa)/sqrt(n)
limof

#[1] 973.3413

#Per tant, es té el 99% de confiança que l'interval [964.6587, 973.3413] contingui el valor mitjà de la població

#P[964.6587 ≤ μ ≤ 973.3413 ] = 0.99

#PROBLEMA 3 

#En una mostra de 9 temperatures mesurades en 9 dies dúna casa. Suposat que es distribueix segons una distribució Normal de variància desconeguda. Es demana:

dades1=read.table("3.txt", header = TRUE) #ES POT LLEGIR TANT EN TXT COM EN CSV
dades1

#Contingut
#1        24
#2        20
#3        21
#4        19
#5        22
#6        23
#7        16
#8        23
#9        21

n=length(dades1$Contingut)
n
#[1] 9

#Estimar el contingut mitjà de vitamina C del suc de tomàquet

media=mean(dades1$Contingut)
media

#[1] 21

#Calcular l'interval de confiança al 96%

elalfa = 0.04

desvst=sd(dades1$Contingut)

quantil=qt((1-elalfa/2),8,lower.tail=T)

limon=media-quantil*desvst/sqrt(n)
limon

#[1] 19.00041

limof=media+quantil*desvst/sqrt(n)
limof

#[1] 22.99959

#P[19.00041  ≤ μ ≤  22.99959] = 0.96  

#Per tant, hi ha un 96% de confiança que l'interval [19.00041, 22.99959] contingui el contingut mitjà en vitamina C del suc de tomàquet.

#Alternativament es pot fer:

t.test(dades1$Contingut,conf.level=0.96)

#One Sample t-test

#data:  dades1$Contingut
#t = 25.72, df = 8, p-value = 5.601e-09
#alternative hypothesis: true mean is not equal to 0
#96 percent confidence interval:
#  19.00041 22.99959
#sample estimates:
#mean of x 
#21

#PROBLEMA 4
#Se sap que la temperatura màxima que resisteixen  uns  certs  tipus de plastics segueix una distribució Normal. Es prenen mostres aleatòries de dos tipus de plastics (A i B), amb 18 elements per eltipus  A (Polipropilè) i 22 per al B ( Politetrafluoretilè ). S’obtenen  les  mitjanes  mostrals  150oC  i  260oC,  respectivament;  i  les variàncies mostrals 19 graus i 38 graus.
#Calculeu un interval de confiança al 95% per al quocient de variàncies.

alpha=0.05

Ha=qf(1-alpha/2,18-1,22-1)
Ja=qf(alpha/2,18-1,22-1)

limon=1/Ha*sqrt(19)^2/sqrt(38)^2
limon

#[1] 0.2013463

limoff=1/Ja*sqrt(19)^2/sqrt(38)^2
limoff

#[1] 1.29999

#[0.2013463,1.29999]

#Tot admetent que que la variància poblacional de cada tipus de plastic, calculeu un interval de confiança al 98% per a la diferència de mitjanes de temperatures màximes.

alpha=0.02

kekw=(sqrt(19)^2/18+sqrt(38)^2/22)^2/(sqrt(19)^4/(18^2*(18-1))+sqrt(38)^4/(22^2*(22-1)))
ta=qt(1-alpha/2,kekw)

Ga=ta*sqrt(sqrt(19)^2/18+sqrt(38)^2/22)

limon=150-260-Ga
limon

#[1] -114.0546

limoff=150-260+Ga
limoff

#[1] -105.9454

#[-114.0546,-105.9454]

#PROBLEMA 5
#XAVIER MARTINEZ I MIQUEL TORRES 

#ESTADISTICA DESCRIPTIVA

#EXERCICI1

#Donades unes variables de pes i edat de segons que estudiants fes la frequencia absoluta i relativa de les variables pes i nom.

dades=read.csv("1.csv", sep=";", dec=".")

#TAMBE ES POT LLEGIR COM A FITXER.TXT

taula_pes=table(dades$Pes)
taula_pes

#47 54 55 58 60 65 69 75 77 85 89 
#1  1  1  2  1  2  1  2  1  1  1

prop.table(taula_pes)

#47         54         55         58         60         65         69         75         77         85         89 
#0.07142857 0.07142857 0.07142857 0.14285714 0.07142857 0.14285714 0.07142857 0.14285714 0.07142857 0.07142857 0.07142857 

taula_nom=table(dades$Noms)
taula_nom

#Abby Chomosuke      Gabi    German   Gustavo  Illojuan     Josep    Manuel     Maria     Mihai  Monicacu   Montoya   Mustafa  Zakarias 
#1         1         1         1         1         1         1         1         1         1         1         1         1         1 

prop.table(taula_nom)

# Abby    Chomosuke       Gabi     German    Gustavo   Illojuan      Josep     Manuel      Maria      Mihai   Monicacu    Montoya    Mustafa   Zakarias 
#0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857 0.07142857

#Escriu una instrucció en R que mostri tots els camps dels registres que tenen PES<60.

pes=(dades$Pes)
pes

#[1] 77 58 89 55 47 60 54 58 75 65 69 85 75 65

pes[pes<60]

#[1] 58 55 47 54 58

#Afegeix al data frame una nova variable de tipus factor, i anomena-la Pes2, que re-codifiqui PES segons el codi següent: Seixanta (si és més gran o igual que 60 ), Cinquanta-5 ( si és menor que 55 però més gran o igual que 40), i Quaranta (si és més petit que 40).

dades$pes2[pes>60]="Seixanta"
dades$pes2[pes>40 & pes<=55]="Cinquanta-5"
dades$pes2[pes<=40]="Quaranta"

dades$pes2

#Representa graficament amb un grafic circular i de barres Edat

pie(table(dades$Edat),col=rainbow(7), main = "Diagrama circular per la variable Edat")

barplot(table(dades$Edat), col=rainbow(7), xlab="Edat", ylab="Frequencies absolutes", main ="Diagrama de barres per la variable Edat")

#Representa amb un grafic de barres Pes i Altura

barplot(table(dades$Pes,dades$Altura),col=rainbow(10))

#Representa amb un histograma la variable Edat

hist(table(dades$Edat), col = "purple", main = "Histograma per la variable Edat", xlab="Edat", ylab="Frequencia")

#Representa un histograma de manera textual (grafic de tija i fulles) 
stem(table(dades$Pes))

#The decimal point is at the |
  
#  1 | 00000000
#  1 | 
#  2 | 000

#Un gràfic de tija i fulles, també conegut com a diagrama de tija i fulles és una representació clàssica de la distribució de dades quantitatives, similar a un histograma però en text, on les dades es divideixen en tija (generalment el primer o primers dígits del número) i fulla (l'últim dígit). El gràfic de tija i fulles en R pot ser útil quan es treballa amb poques observacions (15-150 punts de dades).

#Representa amb una caixa de bigotis per la variable edat.

boxplot(dades$Edat,xlab="Edat",main="Caixes i bigotis per la variable edat") 

#Calcula la mitjana i la mediana de cadascuna de les variables (Pes altura i edat)

mean(dades$Pes)

#[1] 66.57143
mean(dades$Edat)

#[1] 30.42857

mean(dades$Altura)

#[1] 1.692857
median(dades$Pes)

#[1] 65

median(dades$Edat)

#[1] 26

median(dades$Altura)

#[1] 1.67

#Mostra els quantils de la variable pes
quantile(dades$Pes)

#  0%  25%  50%  75% 100% 
#  47   58   65   75   89 

#Mostra el resum de la variable pes
summary(dades$Pes)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   47.00   58.00   65.00   66.57   75.00   89.00 

#LES MATEIXES INSTRUCCIONS ES PODEN APLICAR A TOTES LES ALTRES VARIABLES

#Calcula la variança, desviacio estandard i el rang interquartilic de la variable pes

var(dades$Pes,na.rm=TRUE)

#[1] 151.4945

sd(dades$Pes,na.rm=TRUE)

#[1] 12.30831

IQR(dades$Pes,na.rm=TRUE)

#[1] 17

#na.rm indica si els valors que manquen han de ser eliminats abans de l'anàlisi.

#Mostra els valors ordenats 

sort(dades$Pes) 

# [1] 47 54 55 58 58 60 65 65 69 75 75 77 85 89
 
#Calcula el percentatge de cadascuna de les altures segons el total i rodoneix a un decimal
round(prop.table(table(dades$Altura))*100,digits=1)

#1.6 1.62 1.63 1.65 1.69  1.7 1.74 1.78 1.83 1.85 
#7.1  7.1 21.4 14.3  7.1 14.3  7.1  7.1  7.1  7.1

#INFERENCIA ESTADISTICA

#PROBLEMA 2

#D'una certa població s'ha extret una mostra de 69 individus, el valor mitjà dels quals és 969. Se sap per altres experiències del mateix tipus, que la desviació típica val 14. Trobar intervals de confiança per al valor mitjà de la població als nivells de confiança del 0.95 i 0.99.

el=0.05
n=69
variansa=14^2
meida=969
quantil=qnorm(1-el/2)

limon=meida-quantil*sqrt(variansa)/sqrt(n)
limon

#[1] 965.6967

limof=meida+quantil*sqrt(variansa)/sqrt(n)
limof

#[1] 972.3033

#Per tant, es té el 95% de confiança que l'interval [965.6967, 972.3033] contingui el valor mitjà de la població

#P[965.6967 ≤ μ ≤ 972.3033 ] = 0.95


#Per al nivell de confiança del 0.99, l'única cosa que hem de canviar és el valor de α

el=0.01
quantil=qnorm(1-el/2)

limon=meida-quantil*sqrt(variansa)/sqrt(n)
limon

#[1] 964.6587

limof=meida+quantil*sqrt(variansa)/sqrt(n)
limof

#[1] 973.3413

#Per tant, es té el 99% de confiança que l'interval [964.6587, 973.3413] contingui el valor mitjà de la població

#P[964.6587 ≤ μ ≤ 973.3413 ] = 0.99

#PROBLEMA 3 

#En una mostra de 9 temperatures mesurades en 9 dies dúna casa. Suposat que es distribueix segons una distribució Normal de variància desconeguda. Es demana:

dades1=read.table("3.txt", header = TRUE) #ES POT LLEGIR TANT EN TXT COM EN CSV
dades1

#Contingut
#1        24
#2        20
#3        21
#4        19
#5        22
#6        23
#7        16
#8        23
#9        21

n=length(dades1$Contingut)
n
#[1] 9

#Estimar el contingut mitjà de vitamina C del suc de tomàquet

media=mean(dades1$Contingut)
media

#[1] 21

#Calcular l'interval de confiança al 96%

elalfa = 0.04

desvst=sd(dades1$Contingut)

quantil=qt((1-elalfa/2),8,lower.tail=T)

limon=media-quantil*desvst/sqrt(n)
limon

#[1] 19.00041

limof=media+quantil*desvst/sqrt(n)
limof

#[1] 22.99959

#P[19.00041  ≤ μ ≤  22.99959] = 0.96  

#Per tant, hi ha un 96% de confiança que l'interval [19.00041, 22.99959] contingui el contingut mitjà en vitamina C del suc de tomàquet.

#Alternativament es pot fer:

t.test(dades1$Contingut,conf.level=0.96)

#One Sample t-test

#data:  dades1$Contingut
#t = 25.72, df = 8, p-value = 5.601e-09
#alternative hypothesis: true mean is not equal to 0
#96 percent confidence interval:
#  19.00041 22.99959
#sample estimates:
#mean of x 
#21

#PROBLEMA 4
#Se sap que la temperatura màxima que resisteixen  uns  certs  tipus de plastics segueix una distribució Normal. Es prenen mostres aleatòries de dos tipus de plastics (A i B), amb 18 elements per eltipus  A (Polipropilè) i 22 per al B ( Politetrafluoretilè ). S’obtenen  les  mitjanes  mostrals  150oC  i  260oC,  respectivament;  i  les variàncies mostrals 19 graus i 38 graus.
#Calculeu un interval de confiança al 95% per al quocient de variàncies.

alpha=0.05

Ha=qf(1-alpha/2,18-1,22-1)
Ja=qf(alpha/2,18-1,22-1)

limon=1/Ha*sqrt(19)^2/sqrt(38)^2
limon

#[1] 0.2013463

limoff=1/Ja*sqrt(19)^2/sqrt(38)^2
limoff

#[1] 1.29999

#[0.2013463,1.29999]

#Tot admetent que que la variància poblacional de cada tipus de plastic, calculeu un interval de confiança al 98% per a la diferència de mitjanes de temperatures màximes.

alpha=0.02

kekw=(sqrt(19)^2/18+sqrt(38)^2/22)^2/(sqrt(19)^4/(18^2*(18-1))+sqrt(38)^4/(22^2*(22-1)))
ta=qt(1-alpha/2,kekw)

Ga=ta*sqrt(sqrt(19)^2/18+sqrt(38)^2/22)

limon=150-260-Ga
limon

#[1] -114.0546

limoff=150-260+Ga
limoff

#[1] -105.9454

#[-114.0546,-105.9454]

#PROBLEMA 5

#La següent taula proporciona dades sobre la temperatura total registrada en 14 dies de dues províncies espanyoles (Barcelona i Tarragona). Suposant independència i normalitat. Calcular un interval de confiança a un nivell de confiança del 88% per al quocient de variàncies en totes dues poblacions.

dades2=read.csv("5.csv", sep=";", dec=".")

#En primer lloc determinem l'interval de confiança per al quocient de variàncies, per a això utilitzem la funció var.test. El primer que hem de fer per a aplicar la funció var.test és separar en dues variables les dades relatives a les temperatures realitzades en cada província.

A=dades2$Barcelina
A

#[1] 12 14 11 11 10 17 14 15 16 18 12 11 13

B=dades2$Tarragona
B

#[1] 13 11 13 15 11 13 13 10 11 12 12 14 12

var.test(A,B,conf.level=0.88)


#F test to compare two variances

#data:  A and B
#F = 3.3851, num df = 12, denom df = 12, p-value = 0.04437
#alternative hypothesis: true ratio of variances is not equal to 1
#88 percent confidence interval:
#  1.332576 8.599241
#sample estimates:
#  ratio of variances 
#3.385135 
#La següent taula proporciona dades sobre la temperatura total registrada en 14 dies de dues províncies espanyoles (Barcelona i Tarragona). Suposant independència i normalitat. Calcular un interval de confiança a un nivell de confiança del 88% per al quocient de variàncies en totes dues poblacions.

dades2=read.csv("5.csv", sep=";", dec=".")

#En primer lloc determinem l'interval de confiança per al quocient de variàncies, per a això utilitzem la funció var.test. El primer que hem de fer per a aplicar la funció var.test és separar en dues variables les dades relatives a les temperatures realitzades en cada província.

A=dades2$Barcelina
A

#[1] 12 14 11 11 10 17 14 15 16 18 12 11 13

B=dades2$Tarragona
B

#[1] 13 11 13 15 11 13 13 10 11 12 12 14 12

var.test(A,B,conf.level=0.88)


#F test to compare two variances

#data:  A and B
#F = 3.3851, num df = 12, denom df = 12, p-value = 0.04437
#alternative hypothesis: true ratio of variances is not equal to 1
#88 percent confidence interval:
#  1.332576 8.599241
#sample estimates:
#  ratio of variances 
#3.385135 