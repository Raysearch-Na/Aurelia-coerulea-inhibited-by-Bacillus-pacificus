library(reshape2)
library(ggplot2)
library(ggprism)
library(plyr)
library(wesanderson)
library(colortools)
library(ggpubr)


rm(list=ls())
class <-  read.table(file="bacteria_genus.txt",sep="\t",header=T,row.names = 1)
head(class)

class_per <- as.data.frame(lapply(class,function(x)x/sum(x)))
row.names(class_per) <- row.names(class)

class_ave <- apply(class_per,1,FUN = mean)
class2 <- cbind(class_per,class_ave)[order(-class_ave),]
head(class_ave)

class2 <- subset(class2[1:25,],select = -class_ave)
class2 <- rbind(class2,others = apply(class2,2,function(x){1-sum(x)}))

class2 <- cbind(classID = row.names(class2),class2)

library(reshape2)

class2$classID <- factor(class2$classID,levels = rev(class2$classID))
classgg <- melt(class2,id.vars = "classID",variable.name = "SampledID",
                value.name = "Abundance")

classgg$group <- c(rep("Genus Level"))



ggbarplot(classgg,x="SampledID",y="Abundance",color = "black",fill = "classID",
        legend = "right", size = 0.4,width = 0.8,lab.size = 4,
        title = "",
        legend.title = "Top ASV",
        font.main = c(14,"bold","orange"),font.x=c(22,"bold"),
                      font.y = c (12,"bold"),
        font.title = c(size= 12,"bold"))+
          theme_bw()+
  theme(axis.text.x = element_text(size = 11,face = "bold"),
        axis.text.y = element_text(size = 10,face = "bold"),
        axis.title  = element_text(size = 20),
        legend.justification = c("right"),
        legend.box = "vertical",
        legend.key.size = unit(0.5,"cm"),
        legend.text = element_text(size = 6,face = "bold.italic"),
        legend.title = element_text(size = 6,face = "bold"))+
          rotate_x_text()+
 scale_fill_manual(values = c("gray","rosybrown2",rev(wheel("purple3")[1:4]),rev(wheel("burlywood4")[4:8]),rev(wheel("orangered3")[1:5]),rev(wheel("salmon3")[1:10])))+
          facet_grid(~group,scales = "free_x",space = "free")+
          labs(x= "Sample",y = "Relative Abundance")+
          theme(axis.title = element_text(size = 16, vjust = 0.1))

ggsave("plot.pdf",width = 8,height = 8,plot = last_plot(),device = "pdf",limitsize = F)


 
 