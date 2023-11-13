
#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################
##########################################################################
#####################################
## IC
##########################################################################
#####################################
#SIGMA CONEGUDA
#####################################
#####################################
n=50
x = 6.5 ##mitjana mostral
sigma = 1.5##desviacio tipica
alfa = 0.03
#Formula
# z*alfa/2 = F^-1sub(N(0,1)) (1 - alfa/2) = qnorm(0.985) = 2.17

qn = qnorm(1-alfa/2)
qn
##IC = [X - Aalfa, X + Aalfa] =
# IC = [X - sigma/sqrt(n)*z*alfa/2, X + sigma/sqrt(n)*z*alfa/2] =
## IC = [X - sigma/sqrt(n)*qnorm(1-alfa/2),X + sigma/sqrt(n)*qnorm(1-alfa/2)]
inter1 = x-qn*(sigma)/sqrt(n)
inter2 = x+qn*(sigma)/sqrt(n)
sprintf("[%s , %s]",inter1,inter2)


#####################################
#####################################
#SIGMA DESCONEGUDA (mitjana mostral student)
#####################################
#####################################



n=25
x = 3 ##mitjana mostral
S =  0.7#desviacio estandar
alfa = 0.02
#Formula
# t*alfa/2 = F^-1sub(t n-1) (1 - alfa/2) = qt(0.99,24) = 2.49

qt = qt((1-alfa/2),n-1)
qt
##IC = [X - Balfa, X + Balfa] =
# IC = [X - S/sqrt(n)*t*alfa/2, X + S/sqrt(n)*t*alfa/2] =
## IC = [X - S/sqrt(n)*qt(1-alfa/2,n-1),X + S/sqrt(n)*qt(1-alfa/2,n-1)]
inter1 = x-qt*(S)/sqrt(n)
inter2 = x+qt*(S)/sqrt(n)
sprintf("[%s , %s]",inter1,inter2)



#####################################
#####################################
#IC cuando piden VARIANCIA (chi cuadrat)
#####################################
#####################################



n=10
x = 39.2 ##mitjana mostral
S =  3.5#desviacio estandar
alfa = 0.03
#Formula
#PRIMEROS SACAMOS Calfa Dalfa
#Calfa = Fsub(X^2sub(n-1)) (n-1*Calfa) = 1-alfa/2 = (qchisq(1-alfa/2,n-1))/n-1
#Dalfa = Fsub(X^2sub(n-1)) (n-1*Dalfa) = alfa/2 = (qchisq(alfa/2,n-1))/n-1 ##OJOOO ALFA ENTRE 2
Ca = (qchisq(0.985,9))/9
Da = (qchisq(0.015,9))/9
Ca 
Da
##calculamos los extremos
inter1 = S^2/Ca
inter2 = S^2/Da
sprintf("[%s , %s]",inter1,inter2)

#####################################
#####################################
#IC de PROPORCIO
#####################################
#####################################


n=150
x = 3 ##mitjana mostral
pest = 3/150 #estimador p
pest
alfa = 0.05
#Formula
# IC = [estimador_p - Kalfa, estimador_p - Kalfa] =
# IC = Fsub(N(0,1)) (Kalfa /sqrt((estimador_p*(1-estimador_p))/n) = 1-alfa/2

# Kalfa = sqrt((estimador_p*(1-estimador_p))/n) * qnorm(1-alfa/2) = Kalfa
Kalfa = sqrt((pest*(1-pest))/n)

KalfaT = Kalfa*qnorm(1-alfa/2)

#calculamos extremos
# IC = [estimador_p - Kalfa, estimador_p - Kalfa] =
inter1 = pest - KalfaT
inter2 = pest + KalfaT
sprintf("[%s , %s]",inter1,inter2)
##OJO NO PUEDE SER NEGATIVO CUANDO HACEMOS PROPOCIO
#####################################
########COMPOSTAS####################
#####################################
#####################################
#####################################
##IC para mux-muy (caso sigma^2x = sigma^2x) Student nx + ny -2
#####################################
#####################################
n1 = 15
n2 = 18
x1 = 8.73
x2 = 8.68
s1 = 0.35
s2 = 0.40
alfa = 0.05
#X - Y
resXY = x1-x2
resXY

#Fsub(tsub(n1+n2-2)) (Ealfa / ~S*sqrt((1/n1)+(1/n2))) = 1 - alfa/2 =
# qt(1-alfa/2,n1+n2-2) = ALGO1
algo1=qt(1-alfa/2,n1+n2-2)
algo1
#formula num 3
#~S^2 = ((n1-1)*s1 + (n2-1)*s2) / (n1 + n2 -2)
S2 = ((n1-1)*s1 + (n2-1)*s2) / (n1 + n2 -2)
S2
#Ealfa = ALGO1 * sqrt(~S) * sqrt(1/n1 + 1/n2)
Ealfa = algo1 * sqrt(S2) * sqrt(1/n1 + 1/n2)
Ealfa

#Calculamos los extremos
#VER FORMULARIO PAGINA 2 MARCADO CON UN 1
inter1 = resXY - Ealfa
inter2 = resXY + Ealfa
sprintf("[%s , %s]",inter1,inter2)
#sumXY +- Ealfa

#####################################
#####################################
##IC para mux-muy (caso sigma^2x != sigma^2x) Student k
#####################################
#####################################



n1 = 15
n2 = 20
x1 = 300
x2 = 305
s1 = 16
s2 = 49
alfa = 0.05
#el IC es
#ver formula con un 4
# IC = [x1 - x2 - Galfa, x1 - x2 + Galfa]
resXY = x1-x2
resXY
#calculamos k
kappa1 = ((s1^2/n1)+(s2^2/n2))^2
kappa2 = (s1^4/((n1^2)*(n1-1)))
kappa3 = (s2^4/((n2^2)*(n2-1)))

kappaT = kappa1 /(kappa2+kappa3)
kappaT
#Galfa / sqrt(s1/n1 + s2/n2) = qt(1-alfa/2,kappa)
#Galfa = resultado qt * sqrt(s1/n1 + s2/n2) = Galfa
qt = qt(1-alfa/2,kappa)
qt
Galfa = qt * sqrt(s1/n1 + s2/n2)
Galfa
#Calculamos los extremos
#VER FORMULARIO PAGINA 2 MARCADO CON UN 4
inter1 = resXY - Galfa
inter2 = resXY + Galfa
sprintf("[%s , %s]",inter1,inter2)
#ResXY +- Galfa


#####################################
#####################################
##IC para sigma2/sigma2 (caso S2x / S2y)FISHER #VARIABILITAT
#####################################
#####################################


n1 = 15
n2 = 20
x1 = 300
x2 = 305
s1 = 16
s2 = 49
alfa = 0.1

#S^2x | S^2y
sumS = (s1)/(s2)
sumS

#Fsub(F14,17) (Halfa) = 1-alfa/2 = 
#Halfa = qf(1-alfa/2,n1-1,n2-1)

#Fsub(F14,17) (Jalfa) = alfa/2 = 
#Jalfa = qf(alfa/2,n1-1,n2-1)

Halfa = qf(1-alfa/2,n1-1,n2-1)
Halfa
Jalfa = qf(alfa/2,n1-1,n2-1)
Jalfa

#Calculamos los extremos
#VER FORMULARIO PAGINA 2 MARCADO CON UN 2
inter1 = sumS / Halfa
inter2 = sumS / Jalfa
sprintf("[%s , %s]",inter1,inter2)


#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################
##########################################################################
#####################################
##TEST DE HIPOTESIS
#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################

##########################################################################
#####################################
##TH SIGMA CONEGUDA
######################
#####################################
#####################################
##????


#####################################
#####################################
#TH SIGMA DESCONEGUDA - student n-1
#####################################
#####################################


n = 10
x = 39.2
s = 3.5
alfa = 0.01
mu0 = 45
##OJO: TENER EN CUENTA LAS HIPOTESIS DE MARCAN CON EL SIMBOLO DE LA IZQ DEL FORMULARIO, ESTE CASO ES MU

#H0: mu = mu0
#H1: mu < mu0 

#Ra(Zalfa,inf)

#Zalfa = Fsub(tsub(n-1)) (1-alfa) = Fsub(tsub(n-1)) (1- alfa) = qt(1-alfa,n-1)
qt = qt(1-alfa,n-1)
qt

#calculamos z normal
#z = (x-mu0)/(s/sqrt(n))
z = (x-mu0)/(s/sqrt(n))
z
# RR    2.82  RA
#-|------|-----------
# -5.24
#
#Como z pertenece a RR, rechazamos H0

##PVALOR CRITIC

#p = P(tsub(n-1)<z) = pt(z,n-1)
p = pt(z,n-1)
p
#como p < alfa, rebutjem H0 también


#####################################
#####################################
#TH S^2 - Chi cuadrat
#####################################
#####################################



n = 12
x = 4.95
s = 0.07
alfa = 0.05
sigma20 = 0.25^2 ##IMPORTANTE

#H0: sigma2 = sigma20
#H1: sigma2 < sigma20

#RA = (Zalfa, inf)

#Zalfa = Fsub(X^2sub(n-1)) (Zalfa) = alfa = qchisq(alfa,n-1)
Zalfa = qchisq(alfa,n-1)
Zalfa
#Z = ((n-1)*s^2)/sigma20 = zalfa = chi^2sub(n-1)
z= ((n-1)*s^2)/sigma20
z




# RR    4.47  RA
#-|------|-----------
# 0.86
#

#z está en RR, rebutjem

#####################################
#####################################
#TH p - N(0,1)
#####################################
#####################################
## prob 5
# El nombre de 6 que s'obtenen ???s una Bi(n,p)
# que aproximem per una N(n*p,sqrt(n*p*(1-p)))
# i la proporcio de 6 es llavors la normal
# P=N(p,sqrt(p*(1-p)/n))
# de manera que Z=(P-p)/sqrt(p*(1-p)/n) es una N(0,1)
alfa=0.02
p0=1/6
# (a)
n=100
x=19
pest = x/n
# H0: p=1/6
# H1: p>1/6
# estadestic
z=((pest)-p0)/sqrt(p0*(1-p0)/n)
# la RA es (-inf,Za) amb P(Z>Za)=0.05
Za=qnorm(1-alfa)
# Com que z=0.626 < Za= 2.057, acceptem H0 
# i per tant que p=1/6 i el dau no esta trucat
# a favor del 6.


# RA    2.05  RR
#-|------|-----------
# 0.62
#

#z está en RA -> aceptem H0


# el p-valor es P(N(0,1)>z)
1-pnorm(z)
#
# com que surt p=0.266 >> 0.02 mantenim H0

#####################################
#####################################
#PVALOR CON H1 != PARA FISHER nx-1,ny-1
#####################################
#####################################

n1=25
n2=38
tx1=3
s1=0.7
tx2=3.5
s2=1.2

alfa=0.02
a=1-alfa/2
ta=qt(a,n1-1)
# l'IC es [L1,L2] amb
L1=tx1-ta*s1/sqrt(n1)
L2=tx1+ta*s1/sqrt(n1)
# (b)
# H0: var1/var2 = 1
# H1: var1/var2 != 1
# estadestic mostral
z=s1^2/s2^2
kk=pf(z,n1-1,n2-1)
p=2*min(kk,1-kk)
# com que p =0.007 < 0.02, rebutgem la H0 i
# per tant decidim que les vari???ncies s???n diferents.



#####################################
#####################################
#TH CON H1 != PARA FISHER nx-1,ny-1
#####################################
#####################################


n1=25
n2=35
x1=38.26
x2=41.136
s1=5.41
s2=4.55
alfa=0.02
#H0: sigma21/sigma22=1
#H1: sigma21/sigma22>1

#RA: (-inf,Za)

#Zalfa = Fsub(n1-1,n2-1) (1-alfa)= qf(1-alfa,n1-1,n2-1)
Zalfa = qf(1-alfa,n1-1,n2-1)
Zalfa

#z = s1^2/s2^2
z = s1^2/s2^2
z


# RA    2.15  RR
#-|------|-----------
# 1.41
#

#z está en RA -> aceptem H0


##PVALOR

#p = P(Fsub(24,34) >= 1.41) = pf(z,n1-1,n2-1)
p = 1-pf(z,n1-1,n2-1)
p

# p > alfa -> aceptem H0


#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################
##########################################################################
#####################################
##ANOVA
#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################


# m = 4
# n1 = length(b1)
# n2 = length(b2)
# n3 = length(b3)
# n4 = length(b4)
# n5 = length(b5)
# n = 32
# n
# 
# mb1 = mean(b1)
# mb2 = mean(b2)
# mb3 = mean(b3)
# mb4 = mean(b4)
# mb5 = mean(b5)
# totalm=1/n*(sum(b1)+sum(b2)+sum(b3)+sum(b4)+sum(b5))
# sprintf("Mediana total: %s",(totalm))
# 
# VT = sum((b1-totalm)^2)+sum((b2-totalm)^2)+sum((b3-totalm)^2)+sum((b4-totalm)^2)+sum((b5-totalm)^2)
# VR = sum((b1-mb1)^2)+sum((b2-mb2)^2)+sum((b3-mb3)^2)+sum((b4-mb4)^2)+sum((b5-mb5)^2)
# VE = n1*(mb1-totalm)^2+n2*(mb2-totalm)^2+n3*(mb3-totalm)^2+n4*(mb4-totalm)^2+n5*(mb5-totalm)^2
# 
# sT2 = VT/(n-1)
# sR2 = VR/(n-m)
# sE2 = VE/(m-1)
# 
# z = sE2/sR2
# 
# anovaDF = data.frame(
#   SS = c(VE,VR,VT),
#   DF = c((m-1),(n-m),(n-1)),
#   S2 = c(sE2,sR2,sT2),
#   Z = c(NA,NA,z)
# )
# anovaDF

alfa = 0.05
Za = qf(1-alfa,m-1,n-m)
RR = sprintf("RR = (%s, +inf)",Za)
print(RR)

m=4
n1=35
n2=40
n3=30
n4=38
VT=2400.5
VE=520.2
n=n1+n2+n3+n4
# (a)
VR=VT-VE
sR2=VR/(n-m)
sE2=VE/(m-1)
# (b)
alpha=0.02
CL=1-alpha
z=sE2/sR2
# la RR es de la forma (Za,inf),
# on Za es tal que P(Z>Za)=alpha
# amb Z una Fischer(m-1,n-m)
Za=qf(CL,m-1,n-m)
# com que z=12.82 > Za=3.39, tenim que z esta a la RR
# i rebutgem per tant H0, eeractaments.

# El p-valor es igual a la P(Z>z):
1-pf(z,m-1,n-m)
# surt p=1.9e-7, i per tant aixe confirma que rebutgem H0

#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################
##########################################################################
#####################################
##REGRESSIO LINEAL
#####################################
#####################################
#####################################
##########################################################################
##########################################################################
#####################################
n=20
tx=2.475
ty=23.89
covxy=-0.2256
varx=0.0875
# (a)
b1=covxy/varx
b0=ty-b1*tx
# el model lineal ???s y^*=b0+b1*x*
# i per a x*=2.6 tenim y* igual a
b0+b1*2.6
# (b)
sR=0.1629
VE=11.05
# El contrast ???s 
# H0: el model no explica les dades
# H1: el model explica les dades
# estad???stic z=VE/sR^2
z=VE/sR^2
# La VA ???s Z=F(1,n-2), i el p-valor 
# ???s P(Z>z):
p=1-pf(z,1,n-2)
# Com que p=6.86e-14<<<< 0.02, rebutgem H0 i acceptem
# que el model lineal ???s bo.
# 
# x = 2.475
# y = 23.84
# cov(x,y) = -0.2256
# var(x) = 0.0875
# 
# estimador betita 1 = cov(x,y)/var(x) = talx/talz = -2.58
# estimador beta 0 = y - betita1 = 23.94 - (-2.58) * 2.475 = 30.27
# 
# y = 30.27 - 2.58x
# 
# yx = 30.27 - 2.58*2.6 = 23.57
# 
# z = VE/SR2 = 11.05/0.1624^2 = 416.41
# 
# p = 1-Fsub(Fsub(1,18)) (416.41) = 6.8x10^-14


############################################################################################################
############################################################################################################
################################################FIN#########################################################
############################################################################################################
############################################################################################################

#ESTO ES CACA

##Calcular IC

alfa = 0.05
qn = qnorm(1-alfa/2)
n = 9
var = 25
med = 1014
inter1 = med-qn*(var)/sqrt(n)
inter2 = med+qn*(var)/sqrt(n)
sprintf("[%s , %s]",inter1,inter2)

##Calcular mida minima mostra con error < 5

alfa = 0.05
qn = qnorm(1-alfa/2)
n#?
var = 25
med = 1014
error = 5
n = qn * (var)/error
n = n^2
n

qnorm(1-0.02/2)
0.7/sqrt(25)
##sqrt(n) = qnorm(1-alfa/2)*var/error
pnorm(1.41) #normal inversa (-1)

## prob 3 - student, sigma desconeguda
n1=25
n2=38
tx1=3
s1=0.7
tx2=3.5
s2=1.2

# (a)
alfa=0.02
a=1-alfa/2
ta=qt(a,n1-1)
ta
# l'IC ???s [L1,L2] amb
L1=tx1-ta*s1/sqrt(n1)
L2=tx1+ta*s1/sqrt(n1)

L1
L2

#ANOVA
## prob 4
m=4
n1=35
n2=40
n3=30
n4=38
VT=2400.5
VE=520.2
n=n1+n2+n3+n4
# (a)
VR=VT-VE
##SI QUEREMOS SACAR VT SERIA VT = VR + VE
sR2=VR/(n-m)
sE2=VE/(m-1)
# (b)
alfa=0.02
CL=1-alfa
z=sE2/sR2
# la RR ???s de la forma (Za,inf),
# on Za ???s tal que P(Z>Za)=alfa
# amb Z una Fischer(m-1,n-m)
Za=qf(CL,m-1,n-m)
# com que z=12.82 > Za=3.39, tenim que z est??? a la RR
# i rebutgem per tant H0, ???s a dir, acceptem que hi
# ha difer???ncia entre els tractaments.

# El p-valor ???s igual a la P(Z>z):
PF= 1-pf(z,m-1,n-m)

PF
# surt p=1.9e-7, i per tant aix??? confirma que rebutgem H0

# prob 5
# El nombre de 6 que s'obtenen ???s una Bi(n,p)
# que aproximem per una N(n*p,sqrt(n*p*(1-p)))
# i la proporci??? de 6 ???s llavors la normal
# P=N(p,sqrt(p*(1-p)/n))
# de manera que Z=(P-p)/sqrt(p*(1-p)/n) ???s una N(0,1)
alfa=0.02
CL=1-alfa
p=1/6
# (a)


#TH con n(0,1) y estimador p


n=2000
x=370
tp=x/n
# H0: p=1/6
# H1: p>1/6
# estad???stic
z=(tp-p)/sqrt(p*(1-p)/n)
z
# la RR ???s (Za,inf) amb P(Z>Za)=0.05
Za=qnorm(CL)
Za
# Com que z=0.626 < Za= 2.057, acceptem H0 
# i per tant que p=1/6 i el dau no est??? trucat
# a favor del 6.

# el p-valor ???s P(N(0,1)>z)
pnorm(z,lower.tail=F)
# com que surt p=0.266 >> 0.02 mantenim H0

# (b)
n=2000
x=370
tp=x/n
# H0: p=1/6
# H1: p>1/6
# estad???stic
z=(tp-p)/sqrt(p*(1-p)/n)
# la RR ???s (Za,inf) amb P(Z>Za)=0.02
Za=qnorm(CL)
# Com que z=2.2 > Za= 2.057, rebutgem H0 
# i per tant que p=1/6 i acceptem que el dau est??? trucat
# a favor del 6.
pnorm(z,lower.tail=F)
# d???na p=0.0139 <0.02 i per tant rebutgem H0. 
# Mantindr???em H0 si el nivell de significaci??? l'agaf???ssim
# m???s petit que 0.0139:
Zal=qnorm(1-0.01389) 
Zal

##EN CASO DE QUE CON OTRAS TIRADAS DEL DADO NOS DE QUE REBUTJEM:

# En els dos casos la proporci??? experimental de 6 
# ???s superior al valor no trucat 1/6=0.167, per??? en 
# el segon cas el valor  0.185, malgrat ser inferior
# al 0.19 del primer cas, est??? soportat per molts m???s
# llan???aments que fan que sigui molt m???s improbable 
# per a un dau no trucat a fvor del 6.
