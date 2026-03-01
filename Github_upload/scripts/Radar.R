library(tidyverse)
library(ggiraphExtra) 
library(readxl)  
library(ggh4x) 
library(scales)
library(cowplot)
library(ggradar)
library(ggradar2)

rm(list = ls())

data <- read.table("radar_tissue.txt",header = T)
col <- c("#73946B","#273F4F", "#ed8431","#B66065", "#9083AA","#ed8431")


ggRadar(data = data,
        aes(color = metabolites, fill = metabolites),
        rescale = FALSE,
        size = 1,
        alpha = 0.2,)+
  facet_wrap2(~metabolites,nrow = 1)+
  scale_fill_manual(values = col)+
  scale_color_manual(values = col)+
  scale_y_continuous(breaks = c(0.33,0.67,1))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(linewidth = 0.5) ,
        strip.background = element_rect(linewidth = 0.4, color = "#ffffff"),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 4, color = "black"),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),)

ggsave("radar_tissue.pdf",width = 10,height = 10)
