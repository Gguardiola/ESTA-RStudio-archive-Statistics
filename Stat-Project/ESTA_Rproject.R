##---
##  title: "ACTIVITAT A3-2 ESTAD�STICA"
#date: "29/12/2021"
##
#---
#  

#
# Estad�stica descriptiva

#  ### Ejercicio 1
#  
#  Importa el archivo "plantilla.csv"
#Haz la FA (frecuencia absoluta) y la FR (frecuencia relativa) de las variables "Edad" y "Sueldo"
#Informaci�n adicional sobre el csv:
#  Es la plantilla de una empresa donde consta el nombre, edad, sueldo (en euros), y a�os de antiguedad. El sueldo no tiene porqu� ser directamente propocional a su antiguedad, #ya que cada uno ocupa diferentes puestos y responsabilidades.
#
#
plantilla = read.csv("plantilla.csv", sep=";");
plantilla

#
#FA de "Edad":
#  
edadT = table(plantilla$Edad);#FA
edadT
#
#FR de "Edad":
#  
edadP = prop.table(edadT);##FR
edadP
#
#FA de "Sueldo":
#  
sueldoT = table(plantilla$Sueldo);#FA
sueldoT
#
#FR de "Sueldo":
#  
sueldoP = prop.table(sueldoT);#FR
sueldoP
#
#Muestra el Nombre de las personas menores de 20 a�os
#
menores = plantilla$Nombre[plantilla$Edad<20]
menores
#
#A�ade al DataFrame una variable nueva de tipo Factor que se llame "Categoria" y que recodifique "antiguedad" seg�n el siguiente codigo:
#  Freshman (si tiene 1 a�o de antiguedad)
#  Sophomore (si tiene 2 a�os de antiguedad)
#  Junior (si tiene 3 a�os de antiguedad)
#  Senior (si tiene 4 a�os o M�S de antiguedad)

#  
plantilla$Categoria[plantilla$Antiguedad==1] = "Freshman"
plantilla$Categoria[plantilla$Antiguedad==2] = "Sophomore"
plantilla$Categoria[plantilla$Antiguedad==3] = "Junior"
plantilla$Categoria[plantilla$Antiguedad>=4] = "Senior"

plantilla
#

#Representa en un gr�fico de barras la variable Categoria.
#
barplot(table(plantilla$Categoria),col=rainbow(4),xlab="Categorias",main="Cantidad de categorias")
#

#Representa un gr�fico circular para la variable Edad
#
pie(table(plantilla$Edad),col=rainbow(20), main = "Gr�fico circular para la variable Edad")
#

#Representa en un histograma, la variable Sueldos
#
hist(table(plantilla$Sueldo), col = "cyan", main = "Histograma de sueldos", xlab="Sueldos")
#
#<br>
#  Representa en un gr�fico de Caja y bigotes, la variable Sueldo
#
#
boxplot(plantilla$Sueldo,xlab="Sueldo",main="Caja y bigotes de Sueldo") 
#

#Haz el c�lculo de la mediana y media de las variables Edad y Sueldo.

#
mean(plantilla$Edad)
#
#
median(plantilla$Edad)
#
#
mean(plantilla$Sueldo)
#
#
median(plantilla$Sueldo)
#
#Muestra el summary de Sueldo.
#
summary(plantilla$Sueldo)
#
#Muestra los quantils de la variable Sueldo.
#
quantile(plantilla$Sueldo)
#
#Muestra la desviaci�n estandar, el rango intercuartilico y la variancia de la variable Sueldo.
#
variancia = var(plantilla$Sueldo)
variancia
#
#
desviacio = sd(plantilla$Sueldo)
desviacio
#
#
rangI = IQR(plantilla$Sueldo)
rangI
#
#Muestra el porcentaje de la variable Antiguedad y redondea a 2 decimales.
#
total = round(prop.table(table(plantilla$Antiguedad))*100,digits=2)
total
#
#Muestra las edades ordenadas.
#
sort(plantilla$Edad)

#  Inferencia estad�stica



#  ### Ejercicio 2

#  De una tienda de coches de segunda mano, queremos encontrar el int�rvalo de confianza para un nivel del 95%. La tienda consta de una muestra de 150 coches, con un valor medio de 1200. Se conoce por su historial de coches vendidos que la desviaci�n t�pica es de 10.

#
alfa = 0.05
qn = qnorm(1-alfa/2)
n = 150
var = 10^2
med = 1200
inter1 = med-qn*(var)/sqrt(n)
inter2 = med+qn*(var)/sqrt(n)
sprintf("[%s , %s]",inter1,inter2)

#
#En conclusi�n, el int�rvalo de confianza para un nivel del 95% es [1183.99696107882 , 1216.00303892118]

#  ### Ejercicio 3
#  Seg�n una muestra de 25 personas donde se mide la cantidad de minutos que han tardado en ser atendidos para hacerse un test PCR, determina la media y el int�rvalo de confianza al nivel 98%
#
muestra = read.csv("ej3.csv", sep=";");
muestra
#
#Media de minutos en ser atendidos:
#  
medi = mean(muestra$Minutos.en.espera)
medi
#
#IC al 98%:
#  
alfa = 0.02
desvi = sd(muestra$Minutos.en.espera)
qt = qt((1-alfa/2),24,lower.tail = T)

inter1 = medi-qt*(desvi)/sqrt(length(muestra$Minutos.en.espera))
inter2 = medi+qt*(desvi)/sqrt(length(muestra$Minutos.en.espera))
sprintf("[%s , %s]",inter1,inter2)

#
#En conclusi�n, el int�rvalo de confianza para un nivel del 95% es [5.6730354167592 , 10.8069645832408]

#  Gr�fico circular de los minutos de espera:
#  
pie(table(muestra$Minutos.en.espera),col=rainbow(25), main = "Gr�fico circular de minutos en espera")
#


#En conclusi�n, en el int�rvalo [5.6730354167592 , 10.8069645832408] hay un 98% de confianza.
#  Otra forma m�s sencilla y directa de hacerlo:
#  
t.test(muestra$Minutos.en.espera,conf.level = 0.98)
#
#### Problema 4 - ANOVA
#En una empresa de paqueter�a hay 5 b�sculas y para optimizar el ritmo de trabajo se necesita que las 5 b�sculas hayan pesado una cantidad similar entre ellas al acabar el d�a. #Por la bascula 1 han pasado 52 paquetes, por la bascula 2, 31, por la bascula 3, 45, por la bascula 4, 52, y por la bascula 5, 27.

#  Omite los valores vacios "NA" y guarda cada columna en una variable.
#
datos = read.csv("pesos.csv",stringsAsFactors = T)
b1 = na.omit(datos$Bascula1)
b2 = na.omit(datos$Bascula2)
b3 = na.omit(datos$Bascula3)
b4 = na.omit(datos$Bascula4)
b5 = na.omit(datos$Bascula5)
#
#Genera un gr�fico de caja y bigotes para analizar previamente cada b�scula.
#
boxplot(b1,b2,b3,b4,b5,ylab="peso(kg)",col=rainbow(5),names=c("Bascula 1","Bascula 2","Bascula 3","Bascula 4","Bascula 5"))
#
#  Calcula los grados de libertat y las medianas.
#
m = 5
n1 = length(b1)
n2 = length(b2)
n3 = length(b3)
n4 = length(b4)
n5 = length(b5)
n = n1+n2+n3+n4+n5
n
#
#Medianas:
#  
mb1 = mean(b1)
mb2 = mean(b2)
mb3 = mean(b3)
mb4 = mean(b4)
mb5 = mean(b5)
totalm=1/n*(sum(b1)+sum(b2)+sum(b3)+sum(b4)+sum(b5))
sprintf("Mediana total: %s",(totalm))
#
#Calcula la suma de cuadrados.
#
VT = sum((b1-totalm)^2)+sum((b2-totalm)^2)+sum((b3-totalm)^2)+sum((b4-totalm)^2)+sum((b5-totalm)^2)
VR = sum((b1-mb1)^2)+sum((b2-mb2)^2)+sum((b3-mb3)^2)+sum((b4-mb4)^2)+sum((b5-mb5)^2)
VE = n1*(mb1-totalm)^2+n2*(mb2-totalm)^2+n3*(mb3-totalm)^2+n4*(mb4-totalm)^2+n5*(mb5-totalm)^2
#
#Calcula las variancias.
#
sT2 = VT/(n-1)
sR2 = VR/(n-m)
sE2 = VE/(m-1)
#
#Calcula l'estadistic mostral.
#
z = sE2/sR2
#

#Tabla ANOVA:
#
anovaDF = data.frame(
  SS = c(VE,VR,VT),
  DF = c((m-1),(n-m),(n-1)),
  S2 = c(sE2,sR2,sT2),
  Z = c(NA,NA,z)
)
anovaDF
#

#Calcula la RR con un nivel de confianza del 95%.
#
alfa = 0.05
Za = qf(1-alfa,m-1,n-m)
RR = sprintf("RR = (%s, +inf)",Za)
print(RR)
#

#C�mo Z < RR, se acepta la hipotesis.









