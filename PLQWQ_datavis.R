#### LOAD PACKAGES ----
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

################################################################################
#### READ DATA ----
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\PLQWQ_datavis")
data <-read.csv("PLQWQ_trimmed.csv")

#convert date to usable forms, make month column
data$new_date <- mdy(data$date)#lol turned out to not really be necessary but useful for future, need package lubridate
data$month <- month(mdy(data$date))
data$year <- year(mdy(data$date))

#subset for just station 93 and 96
data_sta <- subset(data, station %in% c("93", "96"))

#average and SE for TDS by month
avg_mn_tds <- aggregate(tds~month, data_sta, mean)
SE_mn_tds <- aggregate(tds~month, data_sta, std.error)

tds_mn<- data.frame(Mean=avg_mn_tds, std.error=SE_mn_tds)

#simple plot of average TDS by month
tds_mn_plot <- ggplot(data= tds_mn, aes(x=Mean.month, y=Mean.tds)) + 
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_errorbar(aes(ymin=Mean.tds-std.error.tds, ymax=Mean.tds+std.error.tds), width=.2, position=position_dodge(0.05), alpha=0.5) +
  ggtitle("Mean TDS (1985-2022) by Month") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                    labels=c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs( x= "", y= "Mean TDS (mg/L)")
tds_mn_plot

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "tds_month_UGLY.png", units = "in", width = 8, height = 6, res=300)
tds_mn_plot
dev.off()

# average TDS by depth at each station (wrapped by month)
data_sta$depth <- ceiling(data_sta$depth)
avg_depth_tds <- aggregate(tds ~ depth + station + month, data_sta, mean)
SE_depth_tds <- aggregate(tds ~ depth + station +month, data_sta, std.error)
tds_depth <- data.frame(avg_depth_tds, SE_depth_tds)

#plot across months, good code for depth profiles
mon_names <- c("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", 
               "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
tds_depth_plot <- ggplot(data= subset(avg_depth_tds, station == 96), aes(x=tds, y=depth)) +
  facet_wrap(~month, labeller=as_labeller(mon_names)) +
  geom_point(alpha=0.5) +
  scale_y_reverse()+
  geom_smooth(orientation = "y", color = "darkseagreen", alpha=0.5) +
  scale_x_continuous(position="top")+
  geom_hline(yintercept=10, linetype="dashed", size=0.5, alpha=0.5) +
  theme(plot.title = element_text(hjust=0.5))+
  labs( x= "Mean TDS (mg/L)", y= "Depth (m)") 
tds_depth_plot

#export
png(filename = "tds_depth.png", units = "in", width = 8, height = 6, res=300)
tds_depth_plot
dev.off()

#data rearranging for by year comparison
data$year <- year(data$new_date)
tds_year <- filter(data, station == 96 & year %in% c(1986, 1995, 2004, 2013, 2018) & depth == 10)
tds_year <- mutate(tds_year, labels= c(if(tds_year$year == 1986) {"3810 masl"}))
tds_year <- mutate(tds_year, lab = cut(year, 
                                          breaks=unique(c(1986, 1995, 2004, 2013, 2018, max(year, na.rm=TRUE))),
                                          c("3810 masl", "3793 masl", "3808 masl", "3802 masl", "3803 masl"), include.lowest=T))
tds_year <- mutate(tds_year, lab = cut(year, breaks=unique(5),
                                       labels= c("3810 masl", "3793 masl", "3808 masl", "3802 masl", "3803 masl"), include.highest=T))
tds_year[43:50,4] <- "3802 masl"
tds_year <- aggregate(tds_year,tds~month+year, mean)
tds_year <- subset(tds_year, select=-labels)

#multi line plot of tds across 5 years 
tds_year$year < as.factor(tds_year$year)
tds_year_plot <- ggplot(tds_year, aes(x= month, y=tds, color=factor(year), group=year)) +
  geom_point(size=1, alpha= 0.75)+
  geom_line(size=1, alpha= 0.75)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels= mon_names) +
  ggtitle("TDS Yearly Variation") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=12),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  geom_dl(aes(label = lab), method = list(dl.trans(x = x + 0.2),
                                           "angled.boxes", cex = 0.8, fontface='bold'))+
  labs(x=" ", y="TDS (mg/L)", color="Years") +
  geom_vline(xintercept=3.5, color="#D95F02", size=1.3, linetype="longdash", alpha=0.5)+
  geom_vline(xintercept=4.0, color="#7570B3", size=1.3, linetype="longdash", alpha=.5)+
  geom_vline(xintercept=5.0, color="#E7298A", size=1.3, linetype="longdash", alpha=.5)+
  geom_vline(xintercept=4.2, color="#66A61E", size=1.3, linetype="longdash", alpha=.5)+
  scale_color_brewer(palette= "Dark2") +
  annotate("text", label="Dashed lines show peak Truckee discharge of the corresponding trend line", x=8, y=4780)
tds_year_plot

brewer.pal(n=5, name="Dark2")
#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "Tds_multiline.png", units = "in", width = 8, height = 6, res=300)
tds_year_plot
dev.off()