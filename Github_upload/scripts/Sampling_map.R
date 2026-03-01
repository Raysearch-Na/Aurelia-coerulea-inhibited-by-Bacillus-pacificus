
rm (list = ls())

install.packages("Rtools")


library(ggplot2)
library(maps)
library(ggmapcn)

data_collect <- data.frame(
  longtitue = 121,
  latitude = 37
)

ggplot()+
  geom_mapcn()+
  geom_mapcn(filter_attribute = "name_en",filter = c("Shandong"),fill = "lightblue")
  theme_minimal()

ggplot()+
  geom_mapcn(filter_attribute = "name_en",filter = c("Shandong"))+
  theme_minimal()


ggsave("china.pdf",width = 8,height = 8,plot = last_plot(),device = "pdf",limitsize = F)
ggsave("shandong.pdf",width = 8,height = 8,plot = last_plot(),device = "pdf",limitsize = F)
