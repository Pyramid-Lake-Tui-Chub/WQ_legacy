library(reshape2)
library(ggplot2)
library(dplyr)
library(chron)
library(lubridate)
library(plotrix)
library(RColorBrewer)
library(ggrepel)
library(directlabels)
library(tidyverse)

#lets make things easy...
data <- PLQChla_combo
#subset for just station 93 and 96
data_sta <- subset(data, station %in% c("93", "96"))

#convert date to usable forms, make month column
data_sta$new_date <- mdy(data_sta$date)
data_sta$month <- month(mdy(data_sta$date))

#average and SE for Chla by month
avg_mn_cla <- aggregate(chla~month + station, data_sta, mean)
SE_mn_cla <- aggregate(chla~month + station, data_sta, std.error)

chla_mn<- data.frame(Mean=avg_mn_cla, std.error=SE_mn_cla)

#simple plot of average Chla by month
chla_mn_plot <- ggplot(data= chla_mn, aes(x=Mean.month, y=Mean.chla, color=factor(Mean.station), group=Mean.station)) + 
  geom_point(alpha=0.8)+
  geom_line(alpha=0.8)+
  geom_errorbar(aes(ymin=Mean.chla-std.error.chla, ymax=Mean.chla+std.error.chla), width=.2, position=position_dodge(0.05), alpha=0.5) +
  ggtitle("Mean Chlorophyll A (1989-2015) by Station") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels=c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(panel.background= element_rect(fill= "grey95", color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=12),
        axis.text.x= element_text(size=12, angle=45, hjust=1))+
  labs( x= "", y= "Mean ChlA (ug/L)", color= "Station") +
  scale_color_brewer(palette= "Dark2")
chla_mn_plot

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "chla_station.png", units = "in", width = 8, height = 6, res=300)
chla_mn_plot
dev.off()