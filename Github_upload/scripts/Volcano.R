library(ggplot2)
library(ggrepel)
library(dplyr)
library(gt)




rm (list = ls())

data <- read.table("BMCM_vocano2.txt",sep = "\t",header = T)
colnames(data) <- c("metab_name","Pvalue","FDR","VIP_OPLS","VIP_PLS","FC","log2FC","Sig","Regulate","log10FDR","log10pvalue")
head(data)

up_top_5 <- bind_rows(
  data %>%
    filter(Regulate == "up") %>%
    arrange(Pvalue,desc(abs(log2FC))) %>%
    head(5))
down_top_5 <- bind_rows(
  data %>%
    filter(Regulate == "down") %>%
    arrange(Pvalue, desc(abs(log2FC))) %>%
    head(5))

p1 <- ggplot(data,aes(x = log2FC, y = log10pvalue,color = Regulate))+
  geom_point(alpha = 0.75,size = 2,shape = 18)+
  scale_color_manual(values = c("#546de5","#d2dae2","#ff4757"))+ xlim(c(-9,9))+
  geom_vline(xintercept = c(0,1,-1),lty = 4,color = "black",lwd = 0.8,alpha = 0.5)+
  geom_hline(yintercept = -log10(0.05),lty = 4, color = "black",lwd = 0.8,alpha = 0.5)+
  labs(x = "Log2FC", y = "-Log10 Pvalue")+
  ggtitle("BT vs CT")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title = element_blank())+
  geom_text_repel(data = up_top_5,
        aes(label = metab_name), size = 3,
        box.padding = unit(1,"lines"),
        point.padding = unit(0.8,"lines"),segment.color = "black", show.legend = F)+
  geom_text_repel(data = down_top_5,
                  aes(label = metab_name), size = 3,
                  box.padding = unit(1,"lines"),
                  point.padding = unit(0.8,"lines"),segment.color = "black", show.legend = F)

p1

p2 <- p1+
  geom_label_repel(data = up_top_5,
                          aes(log2FC,log10pvalue,label = metab_name),
                          size = 3, fill = "#ffcccc",show.legend = F, alpha = 0.65,color = "black")+
  geom_label_repel(data = down_top_5,
                  aes(log2FC,log10pvalue,label = metab_name),
                  size = 3, fill = "#ccffff", alpha = 0.65,color = "black")

p2

ggsave("P.pdf",width = 8,height = 8,plot = last_plot(),device = "pdf",limitsize = F)

