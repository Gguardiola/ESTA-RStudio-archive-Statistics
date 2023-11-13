#EX1
A3<-read.csv("model19.csv",stringsAsFactors = T) #llegim el fitxer
sapply(A3,class) #variables i tipus
A3[A3$ALT>=202,]
A3$QALT[A3$ALT>=190]<- "Qalt"
A3$QALT[A3$ALT<190 & A3$ALT>=180]<- "Qmig"
A3$QALT[A3$ALT<180]<- "Qbaix"
A3$QALT <- as.factor(A3$QALT) #passem la nova variable a factor

#EX2
freq=table(A3$POS)
rfreq=round(prop.table(freq)*100,digits=1)
#gràfic:
barplot(rfreq)
#per saber el percentatge d'alers:
rfreq

#EX3
summary(A3$ALT)
sd(A3$ALT)

quantile(A3$ALT[A3$POS=="Base"])

#EX4
summary(A3$ALT)
length(A3$ALT)
hist(A3$ALT, breaks=seq(168,208,by=2))
#agafo 168 i 208 per tal que hi hagi un nombre enter de caixes
sqrt(542)

#Gràfic caixa i bogotis
boxplot(A3$ALT~A3$POS)
