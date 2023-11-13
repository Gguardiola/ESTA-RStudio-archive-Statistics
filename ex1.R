##exercici 1
A3 <- read.csv("model34.csv",stringsAsFactors = T)
sapply(A3,class)##tipus
View(A3)


A3$Bib <- as.numeric(format(round((1/(A3$Bibliotecaris+A3$Auxiliars)*100),1),nsmall = 1))

A3$QBib[A3$Bib<=20.0]<-"Deficient"
A3$QBib[A3$Bib>20.0 & A3$Bib<=40.0]<-"Millorable"
A3$QBib[A3$Bib>40.0]<-"Suficient"


##2
freq=table(A3$QBib)##contar las veces que se repite  
rfreq=round(prop.table(freq)*100,digits=2)
barplot(rfreq)
rfreq["Suficient"]
##3
summary(A3$Bib)
sd(A3$Bib)
## mean - mitjana = 11.1
## median - mediana = 5.9
##q1 = 1.81
##q3 = 14.28
quantile(A3$Comarca[A3$Auxiliars >2])
quantile(A3$Auxiliars>2)
summary(A3$Comarca[A3$Auxiliars >2])

##4
sqrt(length(A3$Bib))
hist(A3$Bib, breaks=seq(0,50,by=5))

boxplot(A3$Altres~A3$QBib)
  