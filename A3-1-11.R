# Exemple d'activitat A3-1
# Sols poso les instruccions
# Les preguntes s'han de contestar mirant els resultats

#EX1
A1<-read.csv("A11.csv",stringsAsFactors = T)
sapply(A1,class)
A1[A1$ID==100175,]
A1$NOTA[A1$PAAU<6]<-"Acceptable"
A1$NOTA[A1$PAAU<8 & A1$PAAU>=6]<- "Bona"
A1$NOTA[A1$PAAU<10 & A1$PAAU>=8]<- "Notable"
A1$NOTA[A1$PAAU<=14 & A1$PAAU>=10]<- "Extra"

#EX2
freq=table(A1$NOTA)
rfreq=round(prop.table(freq)*100,digits=2)
barplot(rfreq)
# re-ordenem mirant el resultat del gràfic
barplot(rfreq[c(1,2,4,3)])
rfreq

#EX3
summary(A1$PAAU)
sd(A1$PAAU)

quantile(A1$PAAU[A1$ESTUDIS=="M"])
 


#EX4
summary(A1$PAAU)
length(A1$PAAU)
hist(A1$PAAU, breaks=seq(4,13,by=0.6))
# agafem 4 i 13 per tal que hi hagi un nombre enter de caixes ((13-4)/0.6))
# el nombre de caixes és acceptable (15~ sqrt(235))
 
boxplot(A1$CAPA~A1$ESTUDIS)
 