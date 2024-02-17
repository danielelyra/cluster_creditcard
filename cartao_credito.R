library(cluster)
library(stringr)
library(dummies)
library(dplyr)
library(plyr)
library(outliers)

# cria a variável roubo
cc <- CC.GENERAL
summary(cc)


# Análise das variáveis

# BALANCE
summary(cc$BALANCE)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$BALANCE, main="BALANCE", col = 11)
hist(cc$BALANCE, main="BALANCE", col = 11)
sum(cc$BALANCE>5000)
sum(cc$BALANCE<=5000)

# BALANCE_FREQUENCY
summary(cc$BALANCE_FREQUENCY)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$BALANCE_FREQUENCY, main="BALANCE_FREQUENCY", col = 11)
hist(cc$BALANCE_FREQUENCY, main="BALANCE_FREQUENCY", col = 11)
count(cc$BALANCE_FREQUENCY)

# PURCHASES
summary(cc$PURCHASES)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$PURCHASES, main="PURCHASES", col = 11)
hist(cc$PURCHASES, main="PURCHASES", col = 11)
sum(cc$PURCHASES >=5000)
sum(cc$PURCHASES <5000)

# ONEOFF_PURCHASES
summary(cc$ONEOFF_PURCHASES)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$ONEOFF_PURCHASES, main="ONEOFF_PURCHASES", col = 11)
hist(cc$ONEOFF_PURCHASES, main="ONEOFF_PURCHASES", col = 11)
sum(cc$ONEOFF_PURCHASES >=1000)
sum(cc$ONEOFF_PURCHASES <1000)

# INSTALLMENTS_PURCHASES
summary(cc$INSTALLMENTS_PURCHASES)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$INSTALLMENTS_PURCHASES, main="INSTALLMENTS_PURCHASES", col = 11)
hist(cc$INSTALLMENTS_PURCHASES, main="INSTALLMENTS_PURCHASES", col = 11)
sum(cc$INSTALLMENTS_PURCHASES >=1000)
sum(cc$INSTALLMENTS_PURCHASES <1000)

#CASH_ADVANCE
summary(cc$CASH_ADVANCE)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$CASH_ADVANCE, main="CASH_ADVANCE", col = 11)
hist(cc$CASH_ADVANCE, main="CASH_ADVANCE", col = 11)
sum(cc$CASH_ADVANCE >=5000)
sum(cc$CASH_ADVANCE <5000)
count(cc$CASH_ADVANCE)

#PURCHASES_FREQUENCY
summary(cc$PURCHASES_FREQUENCY)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$PURCHASES_FREQUENCY, main="PURCHASES_FREQUENCY", col = 11)
hist(cc$PURCHASES_FREQUENCY, main="PURCHASES_FREQUENCY", col = 11)
count(cc$PURCHASES_FREQUENCY)

#ONEOFF_PURCHASES_FREQUENCY
summary(cc$ONEOFF_PURCHASES_FREQUENCY)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$ONEOFF_PURCHASES_FREQUENCY, main="ONEOFF_PURCHASES_FREQUENCY", col = 11)
hist(cc$ONEOFF_PURCHASES_FREQUENCY, main="ONEOFF_PURCHASES_FREQUENCY", col = 11)
count(cc$ONEOFF_PURCHASES_FREQUENCY)

#PURCHASES_INSTALLMENTS_FREQUENCY
summary(cc$PURCHASES_INSTALLMENTS_FREQUENCY)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$PURCHASES_INSTALLMENTS_FREQUENCY, main="PURCHASES_INSTALLMENTS_FREQUENCY", col = 11)
hist(cc$PURCHASES_INSTALLMENTS_FREQUENCY, main="PURCHASES_INSTALLMENTS_FREQUENCY", col = 11)
count(cc$PURCHASES_INSTALLMENTS_FREQUENCY)

#CASH_ADVANCE_FREQUENCY
summary(cc$CASH_ADVANCE_FREQUENCY)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$CASH_ADVANCE_FREQUENCY, main="CASH_ADVANCE_FREQUENCY", col = 11)
hist(cc$CASH_ADVANCE_FREQUENCY, main="CASH_ADVANCE_FREQUENCY", col = 11)

#CASH_ADVANCE_TRX
summary(cc$CASH_ADVANCE_TRX)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$CASH_ADVANCE_TRX, main="CASH_ADVANCE_TRX", col = 11)
hist(cc$CASH_ADVANCE_TRX, main="CASH_ADVANCE_TRX", col = 11)
count(cc$CASH_ADVANCE_TRX >1)

#PURCHASES_TRX
summary(cc$PURCHASES_TRX)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$PURCHASES_TRX, main="PURCHASES_TRX", col = 11)
hist(cc$PURCHASES_TRX, main="PURCHASES_TRX", col = 11)
count(cc$PURCHASES_TRX == 0)
count(cc$PURCHASES_TRX <= 20)

#CREDIT_LIMIT
summary(cc$CREDIT_LIMIT)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$CREDIT_LIMIT, main="CREDIT_LIMIT", col = 11)
hist(cc$CREDIT_LIMIT, main="CREDIT_LIMIT", col = 11)
count(cc$CREDIT_LIMIT == 0) # registro NAs
count(cc$CREDIT_LIMIT >=15000)
count(cc$CREDIT_LIMIT <= 10000)

#PAYMENTS
summary(cc$PAYMENTS)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$PAYMENTS, main="PAYMENTS", col = 11)
hist(cc$PAYMENTS, main="PAYMENTS", col = 11)
count(cc$PAYMENTS >= 5000)

#MINIMUM_PAYMENTS
summary(cc$MINIMUM_PAYMENTS)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$MINIMUM_PAYMENTS, main="MINIMUM_PAYMENTS", col = 11)
hist(cc$MINIMUM_PAYMENTS, main="MINIMUM_PAYMENTS", col = 11)
count(cc$MINIMUM_PAYMENTS <= 10000)
count(cc$MINIMUM_PAYMENTS <= 5000)

#PRC_FULL_PAYMENT
summary(cc$PRC_FULL_PAYMENT)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$PRC_FULL_PAYMENT, main="PRC_FULL_PAYMENT", col = 11)
hist(cc$PRC_FULL_PAYMENT, main="PRC_FULL_PAYMENT", col = 11)
count(cc$PRC_FULL_PAYMENT == 1)

#TENURE
summary(cc$TENURE)
par(mfrow=c(1,2)) #  p/ plotar os dois gráficos juntos
b=boxplot(cc$TENURE, main="TENURE", col = 11)
hist(cc$TENURE, main="TENURE", col = 11)
count(cc$TENURE)

# excluindo variáveis de frequência e ClientID que não iremos usar 
# variáveis excluidas: CUST_ID,BALANCE_FREQUENCY, PURCHASES_FREQUENCY, ONEOFF_PURCHASES_FREQUENCY,
# PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,TENURE
names(cc)
cc.new=cc[,-c(1,3,8,9,10,11,18)]
names(cc.new)

# tratanto os campos nulos 
# CREDIT_LIMIT- NA's   :1 e  MINIMUM_PAYMENTS- NA's   :313
# como são poucos campos resolvi tratar ele com zero ao invés de atribui
# um valor como a média que poderia "atribuir" um determinado tipo de comportamento
summary(cc.new$CREDIT_LIMIT)
cc.new$CREDIT_LIMIT[is.na(cc.new$CREDIT_LIMIT)] <- 0

summary(cc.new$MINIMUM_PAYMENTS)
cc.new$MINIMUM_PAYMENTS[is.na(cc.new$MINIMUM_PAYMENTS)] <- 0

# verificando e tratando os outliers
outlier(cc.new$BALANCE) # nível inferior
outlier(cc.new$BALANCE, opposite = TRUE) # nível superior

# setting the bench mark para tratar os outliers
# usando a técnica de Winsorizing
summary(cc.new)

# BALANCE
bench_balance <- 2054.1 + 1.5*IQR(cc.new$BALANCE) #Q3 + 1.5*IQR
cc.new$BALANCE[cc.new$BALANCE > bench_balance]
cc.new$BALANCE[cc.new$BALANCE > bench_balance] <- bench_balance
summary(cc$BALANCE)
summary(cc.new$BALANCE)
boxplot(cc.new$BALANCE,main="BALANCE", col = 11)
hist(cc.new$BALANCE, main="BALANCE", col = 11)

# PURCHASES
summary(cc.new$PURCHASES)
bench_PURCHASES <- 1110.13 + 1.5*IQR(cc.new$PURCHASES) #Q3 + 1.5*IQR
cc.new$PURCHASES[cc.new$PURCHASES > bench_PURCHASES] <- bench_PURCHASES
boxplot(cc.new$PURCHASES,main="PURCHASES", col = 11)
hist(cc.new$PURCHASES, main="PURCHASES", col = 11)

#ONEOFF_PURCHASES
summary(cc.new$ONEOFF_PURCHASES)
bench_ONEOFF_PURCHASES <- 577.4 + 1.5*IQR(cc.new$ONEOFF_PURCHASES) #Q3 + 1.5*IQR
cc.new$ONEOFF_PURCHASES[cc.new$ONEOFF_PURCHASES > bench_ONEOFF_PURCHASES] <- bench_ONEOFF_PURCHASES
boxplot(cc.new$ONEOFF_PURCHASES,main="ONEOFF_PURCHASES", col = 11)
hist(cc.new$ONEOFF_PURCHASES, main="ONEOFF_PURCHASES", col = 11)

# INSTALLMENTS_PURCHASES
summary(cc.new$INSTALLMENTS_PURCHASES)
bench_INSTALLMENTS_PURCHASES <- 468.6+ 1.5*IQR(cc.new$INSTALLMENTS_PURCHASES) #Q3 + 1.5*IQR
cc.new$INSTALLMENTS_PURCHASES[cc.new$INSTALLMENTS_PURCHASES > bench_INSTALLMENTS_PURCHASES] <- bench_INSTALLMENTS_PURCHASES
boxplot(cc.new$INSTALLMENTS_PURCHASES,main="INSTALLMENTS_PURCHASES", col = 11)
hist(cc.new$INSTALLMENTS_PURCHASES, main="INSTALLMENTS_PURCHASES", col = 11)

#CASH_ADVANCE
summary(cc.new$CASH_ADVANCE)
bench_CASH_ADVANCE <- 1113.8+ 1.5*IQR(cc.new$CASH_ADVANCE) #Q3 + 1.5*IQR
cc.new$CASH_ADVANCE[cc.new$CASH_ADVANCE > bench_CASH_ADVANCE] <- bench_CASH_ADVANCE
boxplot(cc.new$CASH_ADVANCE,main="CASH_ADVANCE", col = 11)
hist(cc.new$CASH_ADVANCE, main="CASH_ADVANCE", col = 11)

#CASH_ADVANCE_TRX
summary(cc.new$CASH_ADVANCE_TRX)
bench_CASH_ADVANCE_TRX <- 4.000 + 1.5*IQR(cc.new$CASH_ADVANCE_TRX) #Q3 + 1.5*IQR
cc.new$CASH_ADVANCE_TRX[cc.new$CASH_ADVANCE_TRX > bench_CASH_ADVANCE_TRX] <- bench_CASH_ADVANCE_TRX
boxplot(cc.new$CASH_ADVANCE_TRX,main="CASH_ADVANCE_TRX", col = 11)
hist(cc.new$CASH_ADVANCE_TRX, main="CASH_ADVANCE_TRX", col = 11)

# PURCHASES_TRX
summary(cc.new$PURCHASES_TRX)
bench_PURCHASES_TRX <- 17.00 + 1.5*IQR(cc.new$PURCHASES_TRX) #Q3 + 1.5*IQR
cc.new$PURCHASES_TRX[cc.new$PURCHASES_TRX > bench_PURCHASES_TRX] <- bench_PURCHASES_TRX
boxplot(cc.new$PURCHASES_TRX,main="PURCHASES_TRX", col = 11)
hist(cc.new$PURCHASES_TRX, main="PURCHASES_TRX", col = 11)

#CREDIT_LIMIT
summary(cc.new$CREDIT_LIMIT)
bench_CREDIT_LIMIT <- 6500 + 1.5*IQR(cc.new$CREDIT_LIMIT) #Q3 + 1.5*IQR
cc.new$CREDIT_LIMIT[cc.new$CREDIT_LIMIT > bench_CREDIT_LIMIT] <- bench_CREDIT_LIMIT
boxplot(cc.new$CREDIT_LIMIT,main="CREDIT_LIMIT", col = 11)
hist(cc.new$CREDIT_LIMIT, main="CREDIT_LIMIT", col = 11)

#PAYMENTS
summary(cc.new$PAYMENTS)
bench_PAYMENTS <- 1901.1 + 1.5*IQR(cc.new$PAYMENTS) #Q3 + 1.5*IQR
cc.new$PAYMENTS[cc.new$PAYMENTS > bench_PAYMENTS] <- bench_PAYMENTS
boxplot(cc.new$PAYMENTS,main="PAYMENTS", col = 11)
hist(cc.new$PAYMENTS, main="PAYMENTS", col = 11)

#MINIMUM_PAYMENTS
summary(cc.new$MINIMUM_PAYMENTS)
bench_MINIMUM_PAYMENTS<- 788.7 + 1.5*IQR(cc.new$MINIMUM_PAYMENTS) #Q3 + 1.5*IQR
cc.new$MINIMUM_PAYMENTS[cc.new$MINIMUM_PAYMENTS > bench_MINIMUM_PAYMENTS] <- bench_MINIMUM_PAYMENTS
boxplot(cc.new$MINIMUM_PAYMENTS,main="MINIMUM_PAYMENTS", col = 11)
hist(cc.new$MINIMUM_PAYMENTS, main="MINIMUM_PAYMENTS", col = 11)

# PRC_FULL_PAYMENT
summary(cc.new$PRC_FULL_PAYMENT)
bench_PRC_FULL_PAYMENT<- 0.1429 + 1.5*IQR(cc.new$PRC_FULL_PAYMENT) #Q3 + 1.5*IQR
cc.new$PRC_FULL_PAYMENT[cc.new$PRC_FULL_PAYMENT > bench_PRC_FULL_PAYMENT] <- bench_PRC_FULL_PAYMENT
boxplot(cc.new$PRC_FULL_PAYMENT,main="PRC_FULL_PAYMENT", col = 11)
hist(cc.new$PRC_FULL_PAYMENT, main="PRC_FULL_PAYMENT", col = 11)

# tentei tratar os outliers com o log, mas manteve eles.
# o log não ajudou por isso não usei ele
log_balance <- log(cc$BALANCE)
boxplot(log_balance)
hist(log_balance, main="log_balance", col = 11)

# correlação das 11 variáveis que restaram
round(cor(cc.new),2) 
par(mfrow=c(1,1))
library(corrplot)
cor <- round(cor(cc.new),2) 
corrplot(cor, type="upper", method="number")
corrplot(cor,method="number")

# separa apenas as variáveis drives
# BALANCE,PURCHASES,ONEOFF_PURCHASES,INSTALMMENTS_PURCHASES,CASH_ADVANCE, PURCHASES_TRX, CREDIT_LIMIT,PAYMENTS e MINIMUM_PAYMENTS. 
names(cc.new)
cc.cluster=cc.new[,-c(6,11)]
names(cc.cluster)

# Pré-Processamento p/ o Cluster 

cc.scale=scale(cc.cluster) #padroniza os dados 
head(cc.cluster)

cc.dist=dist(cc.scale) # matriz de distâncias

# hierárquica aglomerativa, usando WARD
hc=hclust(cc.dist, method = "ward.D2")
plot(hc, hang = -1)
abline(h=11.5, col=2)

# escolhendo k=3
cc.cluster$hcl=cutree(hc, 3)
table(cc.cluster$hcl)

#para analisar e descrever/batizar os clusters utilizamos as drivers
par(mfrow=c(1,5)) #  p/ plotar os dois gráficos juntos
boxplot(cc.cluster$BALANCE~cc.cluster$hcl, main= 'BALANCE', col=rainbow(4))
boxplot(cc.cluster$PURCHASES~cc.cluster$hcl, main= 'PURCHASES', col=rainbow(4))
boxplot(cc.cluster$ONEOFF_PURCHASES~cc.cluster$hcl, main= 'ONEOFF_PURCHASES', col=rainbow(4))
boxplot(cc.cluster$INSTALLMENTS_PURCHASES~cc.cluster$hcl, main= 'INSTALLMENTS_PURCHASES', col=rainbow(4))
boxplot(cc.cluster$CASH_ADVANCE~cc.cluster$hcl, main= 'CASH_ADVANCE', col=rainbow(4))
boxplot(cc.cluster$PURCHASES_TRX~cc.cluster$hcl, main= 'PURCHASES_TRX', col=rainbow(4))
boxplot(cc.cluster$CREDIT_LIMIT~cc.cluster$hcl, main= 'CREDIT_LIMIT', col=rainbow(4))
boxplot(cc.cluster$PAYMENTS~cc.cluster$hcl, main= 'PAYMENTS', col=rainbow(4))
boxplot(cc.cluster$MINIMUM_PAYMENTS~cc.cluster$hcl, main= 'MINIMUM_PAYMENTS', col=rainbow(4))

# definir o número de cluster
library(NbClust)
nb = NbClust(data=cc.scale, diss=cc.dist, distance = NULL,min.nc = 2,max.nc = 8,method = "ward.D2", index="all")
 nb

 #analisando as demais vars (descritivas)
 table(cc.new$CASH_ADVANCE_TRX, cc.cluster$hcl, dnn=c('CASH_ADVANCE_TRX', 'hcl'))
 table(cc$TENURE, cc.cluster$hcl, dnn=c('TENURE', 'hcl'))

################################################################
#kmedoid com as numericas
library(fpc)
set.seed(11)
kmd=pamk(cc.dist, diss = T, k=2:5, criterion="ch",critout = T)
kmd

#utiliza criterio silhouette para avaliar k
kmd$nc # número de cluster
cc.cluster$kmd=kmd$pamobject$clustering
table(cc.cluster$kmd)

#para analisar e descrever/batizar os clusters utilizamos as drivers
par(mfrow=c(1,5)) #  p/ plotar os dois gráficos juntos
boxplot(cc.cluster$BALANCE~cc.cluster$kmd, main= 'BALANCE', col=rainbow(4))
boxplot(cc.cluster$PURCHASES~cc.cluster$kmd, main= 'PURCHASES', col=rainbow(4))
boxplot(cc.cluster$ONEOFF_PURCHASES~cc.cluster$kmd, main= 'ONEOFF_PURCHASES', col=rainbow(4))
boxplot(cc.cluster$INSTALLMENTS_PURCHASES~cc.cluster$kmd, main= 'INSTALLMENTS_PURCHASES', col=rainbow(4))
boxplot(cc.cluster$CASH_ADVANCE~cc.cluster$kmd, main= 'CASH_ADVANCE', col=rainbow(4))
boxplot(cc.cluster$PURCHASES_TRX~cc.cluster$kmd, main= 'PURCHASES_TRX', col=rainbow(4))
boxplot(cc.cluster$CREDIT_LIMIT~cc.cluster$kmd, main= 'CREDIT_LIMIT', col=rainbow(4))
boxplot(cc.cluster$PAYMENTS~cc.cluster$kmd, main= 'PAYMENTS', col=rainbow(4))
boxplot(cc.cluster$MINIMUM_PAYMENTS~cc.cluster$kmd, main= 'MINIMUM_PAYMENTS', col=rainbow(4))

#analisando as demais vars (descritivas)
table(cc.new$CASH_ADVANCE_TRX, cc.cluster$kmd, dnn=c('CASH_ADVANCE_TRX', 'kmd'))
table(cc$TENURE, cc.cluster$kmd, dnn=c('TENURE', 'kmd'))


# Comparar os clusters
table(cc.cluster$hcl,cc.cluster$kmd) # tabela
library(gmodels)
CrossTable(cc.cluster$hcl,cc.cluster$kmd, prop.c = F, prop.chisq = F, prop.t = F)



