#load packages!
library(reshape2)
library(ggplot2)
library(dplyr)
library(chron)
library(lubridate)
library(plotrix)
library(plotly)
library(RColorBrewer)
library(ggrepel)
library(directlabels)
library(tidyverse)
library(cowplot)
library(ggridges)
theme_set(theme_cowplot())

#### READ DATA ----
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\PLQWQ_datavis")
data <-read.csv("PLQWQ_trimmed.csv")

#convert date to usable forms
# make month column
data$new_date <- mdy(data$date)
data$month <- month(mdy(data$date))
data$year <- year(mdy(data$date))
data$depth <- as.numeric(data$depth)
data$tempC <- as.numeric(data$tempC)

# make depth bins
data <- data %>% mutate(bin = case_when(depth <= 20 ~ 1,
                                        depth > 20 & depth <= 40 ~ 2,
                                        depth > 40 & depth <= 60 ~ 3,
                                        depth > 60 & depth <= 80 ~ 4,
                                        depth > 80 ~ 5))

#make frame to attach data to
year <- seq(1985, 2022, 1)
bin <- c(1,2,3,4,5)
month <- c(4,5,6,7,8,9)
frame <- expand.grid(year,bin,month)
colnames(frame)<- c("year", "bin", "month")

#attach temp data to frame
data_summer <- left_join(frame, data, by=c("year","bin", "month"))

data_sum <- data_summer %>% group_by(bin, year, month) %>% summarise(mean = mean(tempC),
                                                                     min = min(tempC),
                                                                     max = max(tempC))
# create month names for labels
mon_names <- c("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", 
               "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")

tuiTemp <- ggplot(data_sum) +
  geom_ribbon(aes(ymin=min, ymax=max, x=year, fill = factor(bin), group=bin), alpha=0.89)+
  geom_hline(yintercept=17.5, linetype="dashed", size=1, alpha=0.5) +
  geom_hline(yintercept=23, linetype="dashed", size=1, alpha=0.5) +
  theme(panel.background= element_rect(color="black"),
        plot.title = element_text(hjust=0.5),
        legend.position = "right", 
        axis.title.y= element_text(size=14),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  labs(x=" ", y="Temp (C)", fill="Depth Range") +
  scale_fill_brewer(palette= "RdBu", labels = c("< 20m", "< 20-40m", "< 40-60m", "< 60-80m", "> 80m"))+
  facet_wrap(~month, labeller=as_labeller(mon_names))
tuiTemp

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "tui_temp.png", units = "in", width = 8, height = 6, res=600)
tuiTemp
dev.off()

################################################################################
#####                         OLD STUFF                                    #####
################################################################################
#average and SE for 10 m temp by month
avg_mn_temp <- aggregate(tempC~month, data_10, mean)
SE_mn_temp <- aggregate(tempC~month, data_10, std.error)

temp_mn<- data.frame(Mean=avg_mn_temp, std.error=SE_mn_temp)

#simple plot of average 10 m temp by month
temp_mn_plot <- ggplot(data= temp_mn, aes(x=Mean.month, y=Mean.tempC)) + 
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_hline(yintercept=15, linetype="dashed", size=0.5, alpha=0.5) +
  geom_hline(yintercept=22, linetype="dashed", size=0.5, alpha=0.5) +
  geom_errorbar(aes(ymin=Mean.tempC-std.error.tempC, ymax=Mean.tempC+std.error.tempC), width=.2, position=position_dodge(0.05), alpha=0.5) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels=c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs( x= "", y= "Temp (C)")
temp_mn_plot

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "10temp_month.png", units = "in", width = 8, height = 6, res=300)
temp_mn_plot
dev.off()

#data rearranging for by year comparison
data_10$year <- year(data_10$new_date)
temp_year <- filter(data_10, year %in% c(1986, 1996, 2004, 2015, 2018))
temp_year <- aggregate(temp_year,tempC~month+year, mean)
####Adding elevation labels for tds, not sure if needed here
#temp_year <- mutate(temp_year, labels= c(if(tds_year$year == 1986) {"3810 masl"}))
#temp_year <- mutate(temp_year, lab = cut(year, 
                                       breaks=unique(c(1986, 1995, 2004, 2013, 2018, max(year, na.rm=TRUE))),
                                       c("3810 masl", "3793 masl", "3808 masl", "3802 masl", "3803 masl"), inclue.lowest=T))
#tds_year <- mutate(tds_year, lab = cut(year, breaks=unique(5),
                                       #labels= c("3810 masl", "3793 masl", "3808 masl", "3802 masl", "3803 masl"), include.highest=T))
#tds_year[45:53,9] <- "3802 masl"

#tds_year <- subset(tds_year, select=-labels)
####

#multi line plot of temp across 5 years
temp_year$year < as.factor(temp_year$year)
temp_year_plot <- ggplot(temp_year, aes(x= month, y=tempC, color=factor(year), group=year)) +
  geom_point(size=1)+
  geom_line(size=1)+
  scale_y_continuous(breaks = seq(0, 25, 2)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels= mon_names) +
  ggtitle("Temperature Yearly Variation") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=14),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  geom_hline(yintercept=15, linetype="dashed", size=1, alpha=0.5) +
  geom_hline(yintercept=22, linetype="dashed", size=1, alpha=0.5) +
  labs(x=" ", y="Temp (C)", color="Years") +
  scale_color_brewer(palette= "Dark2")
temp_year_plot

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "Temp_multiline.png", units = "in", width = 8, height = 6, res=300)
temp_year_plot
dev.off()

# depth profiles by month FACET WRAP BABY
# average Temp by depth at each station (wrapped by month)
data$depth <- ceiling(data$depth)
avg_depth_temp <- aggregate(tempC ~ depth + month, data, mean)
SE_depth_temp <- aggregate(tempC ~ depth + month, data, std.error)
temp_depth <- data.frame(avg_depth_temp, SE_depth_temp)

#plot across months, good code for depth profiles

temp_depth_plot <- ggplot(temp_depth, aes(x=tempC, y=depth)) +
  facet_wrap(~month, labeller=as_labeller(mon_names)) +
  geom_point(alpha=0.5) +
  scale_y_reverse()+
  geom_errorbar(aes(xmin=tempC-2*tempC.1, xmax=tempC+2*tempC.1), width=.2, position=position_dodge(0.05), alpha=0.5)+
  scale_x_continuous(position="top", 
                     labels = paste(seq(0, 25, by = 5)), 
                     breaks=seq(0, 25, by=5),
                     limits = c(0,25))+
  geom_vline(xintercept=15, linetype="dashed", size=1, alpha=0.8) +
  geom_vline(xintercept=23, linetype="dashed", size=1, alpha=0.8) +
  theme(plot.title = element_text(hjust=0.5), )+
  labs( x= "Mean Temp (C)", y= "Depth (m)") 
temp_depth_plot

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "temp_depth_tuirange.png", units = "in", width = 8, height = 6, res=300)
temp_depth_plot
dev.off()