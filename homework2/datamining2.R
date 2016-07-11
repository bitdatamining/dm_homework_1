library ( Matrix )  
library ( arules )

setwd("H:\\DMhomework\\HomeWork2\\")

###################################################
###Read Data
###################################################
diagnosis <- read.table('diagnosis.data',
                         header=F,
                         dec = ",",
                         sep='\t',
                         fileEncoding="UTF-16LE",
                         as.is=TRUE,
                         col.names=c('Temperature','Nausea','LumbarPain','UrinePush','MicturitionPain','BurningOfUrethra','Inflammation','Nephritis')
                        )
colum <- list('Temperature','Nausea','LumbarPain','UrinePush','MicturitionPain','BurningOfUrethra','Inflammation','Nephritis')
###################################################
###1 Preprocess,converted  dataset into forms that are suitable for Mining Association rules
###################################################
str_all=""
for(i in 1:nrow(diagnosis))
{
  if(as.numeric(diagnosis[i,1])<=36.9){line='normal'}
  else if(as.numeric(diagnosis[i,1])<=37.9){line='low'}
  else if(as.numeric(diagnosis[i,1])<=39.5){line='midde'}
  else if(as.numeric(diagnosis[i,1])<=42.0){line='high'}
 
  for(j in 2:ncol(diagnosis))
  {
    if(diagnosis[i,j]=="yes"){line <- paste(line,colum[[j]],sep=',')}
  }
  if(str_all==''){str_all=line}
  else{str_all <- paste(str_all,line,sep='\r')}
}
write(str_all,'PreprocessData.csv')


###################################################
###2 Find the frequent items ,default supp=0.1,conf=0.8
###################################################
data <- read.transactions('PreprocessData.csv',format = 'basket',sep=',')
sink("frequent_items.txt")
frequentsets=eclat(data,parameter=list(support=0.1,maxlen=8,minlen=1,target = "frequent itemsets"))  
inspect(frequentsets)
###itemsets_apr = apriori (data, parameter = list (support=0.1,confidence=0.5,maxlen=8,target = "frequent itemsets"),control=list(sort=-1)) 
###inspect(itemsets_apr) 
sink()


###################################################
###3 Export rules, calculate its support and confidence
###################################################
sink("rules.txt")
rules = apriori (data, parameter = list (maxlen=8,minlen=2,support=0.1,confidence=0.5), appearance= list(rhs=c('Inflammation','Nephritis'),default="lhs"),control=list(sort=-1)) 
inspect(rules) 
sink()


###################################################
###4 Delete redundant rules X->Y ,Y!=bladder or nephritis
###################################################
subset.matrix<-is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
rules.pruned<-rules[!redundant]

sink("rules_delete_redundant.txt")
inspect(rules.pruned) 
sink()


###################################################
###5 Evaluated rules, use the Lift
###################################################
sink("rules_delete_redundant_sorted_lift.txt")
rules.pruned.sorted_lift = sort(rules.pruned,by='lift')
inspect(rules.pruned.sorted_lift) 
sink()


###################################################
###6 Visualization, display rules
###################################################
library(grid)
library (arulesViz)

#Scatter chart
plot(rules.pruned.sorted_lift) 
#Bubble chart
plot(rules.pruned.sorted_lift,method="grouped") 

plot(rules.pruned.sorted_lift, method="graph")

















