###################################################
### Loading the Data into R
###################################################
library(DMwR)
#head(dataset)
setwd("D://datamining1")
dataset <- read.table('Analysis.txt',
          header=F,
          dec='.',
          col.names=c('season','size','speed','mxPH','mnO2','Cl',
          'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
          'a5','a6','a7'),
          na.strings=c('XXXXXXX'))

colum <- list('season','size','speed','mxPH','mnO2','Cl',
            'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
            'a5','a6','a7')
###################################################
### Data Visualization and Summarization
###################################################
sink("Summary.txt")   #新建文件summary.txt
summary(dataset)      #Summarization
sink()                #保存文件

library(car)
for (i in 4:18)
{
a <- colum[[i]]

jpeg(file=paste('Histogram of ',a,'.jpg') )
#par(mfrow=c(1,2))
hist(dataset[,i], prob=T, xlab='',main=paste('Histogram of ',a))
lines(density(dataset[,i],na.rm=T))
rug(jitter(dataset[,i]))
dev.off( )
jpeg(file=paste('QQ of ',a,'.jpg') )
qq.plot(dataset[,i],main=paste('Normal QQ plot of ',a),ylab= as.character(a))
dev.off( )


jpeg(file=paste('box of ',a,'.jpg') )
#par(mfrow=c(1,1))
boxplot(dataset[,i],ylab= as.character(a))
rug(jitter(dataset[,i]),side=2)
abline(h=mean(dataset[,i],na.rm=T),lty=2)
dev.off( )
#identify(dataset[,i])
}

library(lattice)
for (i in 12:18)
{
a <- colum[[i]]
jpeg(file=paste('Season and ',a,'.jpg') )
print(bwplot(season ~ dataset[,i], data=dataset,ylab='Season',xlab=paste('Algal ',a)))
dev.off( )
jpeg(file=paste('River Size and ',a,'.jpg') )
print(bwplot(size ~ dataset[,i], data=dataset,ylab='River Size',xlab=paste('Algal ',a)))
dev.off( )
jpeg(file=paste('River Speed and ',a,'.jpg') )
print(bwplot(speed ~ dataset[,i], data=dataset,ylab='River Speed',xlab=paste('Algal ',a)))
dev.off( )
}

###################################################
### Unkwnon Values
###################################################
#dataset <- dataset[-manyNAs(dataset),]
###0,orginal
write.csv(dataset,file = "Analysis_Orginal.csv",na = "XXXXXXX")

###1,Delete
dataset <- na.omit(dataset)
write.csv(dataset,file = "Analysis_Delete.csv",na = "XXXXXXX")

###2,most Frequency
dataset <- read.table('Analysis.txt',
                    header=F,
                    dec='.',
                    col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))
for (i in 4:11)
{
a <- colum[[i]]
as.numeric(names(table(dataset[,as.character(a)])))[which.max(table(dataset[,as.character(a)]))]
dataset[is.na(dataset[,i]),as.character(a)] <- as.numeric(names(table(dataset[,as.character(a)])))[which.max(table(dataset[,as.character(a)]))]

}
write.csv(dataset,file = "Analysis_Frequency.csv",na = "XXXXXXX") 
###3,Correlation
dataset <- read.table('Analysis.txt',
                    header=F,
                    dec='.',
                    col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))
dataset <- dataset[-manyNAs(dataset),]
sink("CorrelationMatrix.txt")   #把结果保存到CorrelationMatrix.txt
symnum(cor(dataset[,4:18],use="complete.obs"))
sink()  

sink("CorrelationCoefficient.txt")   #把结果保存到CorrelationCoefficient.txt
lm(PO4 ~ oPO4,data=dataset)
lm(NH4 ~ NO3,data=dataset)
lm(Chla ~ mxPH,data=dataset)
lm(mxPH ~Chla ,data=dataset)
lm(mnO2 ~oPO4 ,data=dataset)
lm(Cl ~oPO4 ,data=dataset)
sink()

jpeg(file=paste('season-speed-size of mnO2','.jpg') )
stripplot(~mnO2|season *speed *size,data=dataset,jitter=T)
dev.off()
jpeg(file=paste('season-speed-size of Cl','.jpg') )
stripplot(~Cl|season *speed *size,data=dataset,jitter=T)
dev.off()
jpeg(file=paste('season-speed-size of Chla','.jpg') )
stripplot(~Chla|season *speed *size,data=dataset,jitter=T)
dev.off()
jpeg(file=paste('season-speed-size of mxPH','.jpg') )
stripplot(~mxPH|season *speed *size,data=dataset,jitter=T)
dev.off()


fillPO4 <- function(oP) {
  if (is.na(oP)) return(NA)
  else return(42.897 + 1.293 * oP)
}
dataset[is.na(dataset$PO4),'PO4'] <- sapply(dataset[is.na(dataset$PO4),'oPO4'],fillPO4)

fillChla <- function(mP) {
  if (is.na(mP)) return(NA)
  else return(abs(-139.4 + 19 * mP))
}
dataset[is.na(dataset$Chla),'Chla'] <- sapply(dataset[is.na(dataset$Chla),'mxPH'],fillChla)

fillmxPH <- function(Ch) {
  if (is.na(Ch)) return(NA)
  else return(7.92896 + 0.01047 * Ch)
}
dataset[is.na(dataset$mxPH),'mxPH'] <- sapply(dataset[is.na(dataset$mxPH),'Chla'],fillmxPH)

fillmnO2 <- function(oP) {
  if (is.na(oP)) return(NA)
  else return(9.93341 -0.01093 * oP)
}
dataset[is.na(dataset$mnO2),'mnO2'] <- sapply(dataset[is.na(dataset$mnO2),'oPO4'],fillmnO2)

fillCl <- function(oP) {
  if (is.na(oP)) return(NA)
  else return(28.4771 + 0.1992 * oP)
}
dataset[is.na(dataset$Cl),'Cl'] <- sapply(dataset[is.na(dataset$Cl),'oPO4'],fillCl)

#dataset[is.na(dataset$mnO2),"mnO2"] <- mean(dataset$mnO2,na.rm = T)

write.csv(dataset,file = "Analysis_Correlation.csv",na = "XXXXXXX") 

###4,Similarity
dataset <- read.table('Analysis.txt',
                    header=F,
                    dec='.',
                    col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))
dataset <- knnImputation(dataset,k=10)
write.csv(dataset,file = "Analysis_Similarity.csv",na = "XXXXXXX") 
