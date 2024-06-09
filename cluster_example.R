rm(list = ls())

#### load data ####  

packs <- c('readxl', 'psych', 'ggplot2', 'gmodels', 'vcd', 'varhandle', 'factoextra', 'dplyr', 'fpc')
if (!require(packs)) install.packages(packs)

library(readxl)
library(psych)
library(ggplot2)
library(gmodels)
library(vcd)
library(varhandle)
library(factoextra)
library(dplyr)
library(fpc)

data<-read_xlsx('lite_dataS.xlsx')

#### Data Understanding ####

# Dims, vars y types

dim(data)
head(data)
colnames(data)
str(data)

# describe data

describe(data)
describeBy(data,data$grupo_de_cliente)

cv <- function(var){(sd(var)/mean(var))*100}
cv(data$No_tran)

#### Boxplots, histograms and scatterplots ####

# boxplots - overall

par(mfrow=c(2,2))

boxplot(data[,3])
boxplot(data[,4:7],names = c(colnames(data[,4:7])),las=2, cex.axis=0.7,cex=0.5)
boxplot(data[,8:10],names = c(colnames(data[,8:10])),las=2, cex.axis=0.7,cex=0.5)
boxplot(data[,20:25],names = c(colnames(data[,20:25])),las=2, cex.axis=0.7,cex=0.5)

dev.off()

ggplot(data,aes(prom_tran))+
  geom_histogram(bins = 20,fill="gray", color="white")+
  theme_bw()+
  ggtitle('avrg amount per transaction')+
  ylab('Frequency')+
  xlab('Amount in COP')+
  theme(plot.title = element_text(hjust = 0.5))

qqnorm(data$No_tran)

ggplot(data,aes(prom_tran))+
  geom_boxplot()+
  theme_bw()+
  ggtitle('avrg amount per transaction')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data,aes(Percentage_nacional_total))+
  geom_histogram(bins = 10,fill="gray", color="white")+
  theme_bw()+
  ggtitle('Nacional % (Total)')+
  ylab('Frequency')+
  xlab('Percentage')+
  theme(plot.title = element_text(hjust = 0.5))

data$grupo_de_cliente<-as.factor(data$grupo_de_cliente)

ggplot(data,aes(x=grupo_de_cliente,y=prom_tran))+
  geom_boxplot()+
  theme_bw()+
  ggtitle('avrg amount per transaction')+
  theme(plot.title = element_text(hjust = 0.5))

par(mfrow=c(2,3))

hist(data$No_tran, xlab="Number of Transactions", 
     ylab="Frequency", main="Histogram \nNo of trans")
hist(data$prom_tran, xlab = "avrg transaction", ylab="Frequency", 
     main="Histogram \navrg transactions")
hist(data$porc_masNa, xlab = "Percentage MasterCard", ylab="Frequency", 
     main="Histogram \nPercentage use MasterCard")
hist(data$Percentage_nacional_total, xlab = "Percentage nacional", ylab="Frequency", 
     main="Histogram \nPercentage nacional")
hist(data$porcSABADO, xlab = "Percentage use sabado", ylab="Frequency", 
     main="Histogram \nPercentage Sabado")

dev.off()

# Tabla  variables categoricas

sitios<-sort(table(data$Sitio_consumo_masfrecuente))
prop.table(sitios)

grupos<-sort(table(data$grupo_de_cliente))
prop.table(grupos)

median(data$porc_mana)
median(data$porc_tar)
median(data$Percentage_noche)

##### stage 3: Data preparation #####

# correaltions

cor_table<-data[,-1:-2]
cor_table<-cor_table[,-24]
corr_marix<-cor(cor_table)
#write.csv(corr_marix,'corr_marix.csv')

data_es<-cbind(data$No_tran,
                data$prom_tran,
                data$porc_visNa,
                data$porc_masNa,
                data$porc_mana,
                data$porc_tar)

data_es<-apply(data_es, 2, log1p)
data_es<-as.data.frame(scale(data_es))
colnames(data_es)<-c('No_tran',
                      'prom_tran',
                      'porc_masNa',
                      'porc_visNa',
                      'porc_mana',
                      'porc_tar')

# Boxplots scaled

ggplot(data,aes(x=grupo_de_cliente,y=prom_tran))+
  geom_boxplot()+
  theme_bw()+
  ggtitle('Boxplot monto promedio por transacci?n')+
  theme(plot.title = element_text(hjust = 0.5))

compara_box<-cbind(data$grupo_de_cliente,data_es)

ggplot(compara_box,aes(x=`data$grupo_de_cliente`,y=prom_tran))+
  geom_boxplot()+
  theme_bw()+
  ggtitle('Boxplot monto promedio por transacci?n')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data,aes(x=grupo_de_cliente,y=No_tran))+
  geom_boxplot()+
  theme_bw()+
  ggtitle('Boxplot monto promedio por transacci?n')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(compara_box,aes(x=`data$grupo_de_cliente`,y=No_tran))+
  geom_boxplot()+
  theme_bw()+
  ggtitle('Boxplot monto promedio por transacci?n')+
  theme(plot.title = element_text(hjust = 0.5))

par(mfrow=c(2,2))

hist(data$No_tran, xlab="Numero de transacciones", 
     ylab="Frequency", main="Histogram \nNo de transacciones")
hist(data_es$No_tran, xlab="Numero de transacciones", 
     ylab="Frequency", main="Histogram \nNo de transacciones")

hist(data$prom_tran, xlab="promedio por transacciones", 
     ylab="Frequency", main="Histogram \npromedio por transacciones")
hist(data_es$prom_tran, xlab="promedio por transacciones", 
     ylab="Frequency", main="Histogram \npromedio por transacciones")


dev.off()

##### stage 4: Modeling ####

wss <- (nrow(data_es)-1)*sum(apply(data_es,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(data_es,
                                     centers=i, nstart=10)$withinss)
#Elbow chart

sumas <- as.data.frame(cbind(wss, k = seq(1,15, by=1)))

sumas %>% ggplot(aes(x=k, y=wss)) +
  geom_point() + 
  geom_line() +
  labs(x = "N?mero de clusters", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#silhouette

sampled_data<- as.data.frame(data_es[sample(1:nrow(data_es), 25000), ])
fviz_nbclust(sampled_data,kmeans,method = 'silhouette')

## Kmeans estimation 

cluster_sol<-kmeans(data_es,centers=5,nstart=10,iter.max=20)
cluster_sol$size
cluster_sol$iter


data$cluster <- cluster_sol$cluster

## explore sols

mosaic(~cluster + grupo_de_cliente ,data=data, 
       legend=TRUE, shade=TRUE)

boxplot(log(No_tran)~cluster, data=data, main='Panel a. boxplot ajustado numero de transacciones')
boxplot(log(prom_tran)~cluster, data=data)
boxplot(sqrt(porc_mana)~cluster, data=data, main='Panel b. boxplot ajustado use Percentage ma?ana')
boxplot(sqrt(porc_tar)~cluster, data=data, main='Panel c. boxplot ajustado use Percentage tarde')
boxplot(sqrt(Percentage_noche)~cluster, data=data)

lugares<-data%>%filter(Sitio_consumo_masfrecuente=='SUPMER'|
                          Sitio_consumo_masfrecuente=='AMCEN'|
                          Sitio_consumo_masfrecuente=='VEST'|
                          Sitio_consumo_masfrecuente=='OTRO'|
                          Sitio_consumo_masfrecuente=='GAS'|
                          Sitio_consumo_masfrecuente=='TELCO')

mosaic(~cluster + Sitio_consumo_masfrecuente ,data=lugares, 
       legend=TRUE, shade=TRUE, main = 'Panel d. Mosaico lugares frecuentes')

## Solidez de los clusters

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 1)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 2)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 3)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 4)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 5)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 6)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 7)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 8)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 9)
kclusters$bootmean

kclusters <- clusterboot(data_es,B=20,clustermethod=kmeansCBI,k=5,seed = 10)
kclusters$bootmean


### centers

centrosg <- as.data.frame(cluster_sol$centers)
centrosg$cluster <- as.factor(rownames(centrosg))
centrosheat <- reshape2::melt(centrosg)
colnames(centrosheat) <- c("Cluster","variable","centroide")
centrosheat %>% 
  ggplot(aes(x=Cluster,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

