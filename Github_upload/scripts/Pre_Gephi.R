library(psych)
library(reshape2)
library(Hmisc)
library(dplyr)



rm(list = ls())
asv <- read.table("gephi_asv_BW.txt",sep = "\t", header = T,row.names = 1)
asv = t(asv)


tax <- read.table("taxa.txt",sep = "\t",header = T)
names(tax)[1] <- "Id"


abundance = 0.1
asv_a <- asv[,colSums(asv)/sum(asv) >= (abundance/100)]


occor = rcorr(asv_a,type = "spearman")
r_matrix = occor$r
p_matrix = occor$P


r.cut = 0.60
p.cut = 0.05
r_matrix[p_matrix > p.cut|abs(r_matrix)<r.cut] = 0

p_value = melt(p_matrix)
r_value = melt(r_matrix)

r_value = cbind(r_value,p_value$value)
r_value = subset(r_value,r_value[,3]!= 0 )
r_value = na.omit(r_value)

abs = abs(r_value$value)
linktype = r_value$value
linktype[linktype > 0] = 1
linktype[linktype < 0] = -1
r_value = cbind(r_value,abs,linktype)

names(r_value) <- c("Source","Target","r_value","p_value","abs_value","linktype")
names(p_value) <- c("Souece","Target","p_value")

node_asv <- as.data.frame(as.data.frame(r_value[,1])[!duplicated(
  as.data.frame(r_value[,1])),])
names(node_asv)="Id"

list <- rbind(node_asv)
list = subset(tax,Id %in% list$Id)
list$Label <- list$Id



write.csv(list,file = "BW_not.csv",row.names = F)
write.csv(r_value,file = "BW_edge.csv",row.names = F)


write.csv(list,file = "node_list.csv",row.names = F)
write.csv(r_matrix,file = "corr_matrix.csv",row.names = F)
write.csv(r_value,file = "r_value.csv",row.names = F)
write.csv(p_value,file = "p_value.csv",row.names = F)
