####################################
#PRESIDENTIAL APPROVAL RATINGS
#Name: Adam Walker
#Date: August 2017
####################################

rm(list=ls())
gc()

options(scipen=999)

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(grid)
library(gridExtra)
library(purrr)
library(ggrepel)

###############################
#Load and format data
###############################

pres<-read_csv(file=paste0("/Users/walkerag/Documents/pres_approval/pres_approval_dat.csv"))
pres<-read_csv(file=paste0("/Users/walkerag/Documents/pres_approval/pres_approval_dat_v2.csv"))

head(pres)

#Add inauguration dates
pres$inauguration_date<-as.Date("2018-01-01")
pres[pres$Pres=="Truman","inauguration_date"]<-as.Date("1945-04-12")
pres[pres$Pres=="Eisenhower","inauguration_date"]<-as.Date("1953-01-20")
pres[pres$Pres=="JFK","inauguration_date"]<-as.Date("1961-01-20")
pres[pres$Pres=="LBJ","inauguration_date"]<-as.Date("1963-11-22")
pres[pres$Pres=="Nixon","inauguration_date"]<-as.Date("1969-01-20")
pres[pres$Pres=="Ford","inauguration_date"]<-as.Date("1974-08-09")
pres[pres$Pres=="Carter","inauguration_date"]<-as.Date("1977-01-20")
pres[pres$Pres=="Reagan","inauguration_date"]<-as.Date("1981-01-20")
pres[pres$Pres=="Bush1","inauguration_date"]<-as.Date("1989-01-20")
pres[pres$Pres=="Clinton","inauguration_date"]<-as.Date("1993-01-20")
pres[pres$Pres=="Bush2","inauguration_date"]<-as.Date("2001-01-20")
pres[pres$Pres=="Obama","inauguration_date"]<-as.Date("2009-01-20")
pres[pres$Pres=="Trump","inauguration_date"]<-as.Date("2017-01-20")

#Add election result
#Use Trump for Trump to make plots a little easier
pres$election_result<-'Unknown'
pres[pres$Pres=="Truman","election_result"]<-'Won'
pres[pres$Pres=="Eisenhower","election_result"]<-'Won'
pres[pres$Pres=="JFK","election_result"]<-'Died'
pres[pres$Pres=="LBJ","election_result"]<-'Won'
pres[pres$Pres=="Nixon","election_result"]<-'Won'
pres[pres$Pres=="Ford","election_result"]<-'Lost'
pres[pres$Pres=="Carter","election_result"]<-'Lost'
pres[pres$Pres=="Reagan","election_result"]<-'Won'
pres[pres$Pres=="Bush1","election_result"]<-'Lost'
pres[pres$Pres=="Clinton","election_result"]<-'Won'
pres[pres$Pres=="Bush2","election_result"]<-'Won'
pres[pres$Pres=="Obama","election_result"]<-'Won'
pres[pres$Pres=="Trump","election_result"]<-'Trump'

#Calculate day based on days since inauguration
pres$DayDate<-pres$Day+pres$inauguration_date
head(pres)

#Check days are distinct
length(unique(pres$DayDate))
#pres %>% group_by(DayDate) %>% mutate(daytotal=n()) %>% filter(daytotal>1)
#No overlap

#Add each President's re-election date
pres$election_date<-as.Date("2050-01-01")
pres[pres$Pres=="Truman","election_date"]<-as.Date("1948-11-02")
pres[pres$Pres=="Eisenhower","election_date"]<-as.Date("1956-11-06")
pres[pres$Pres=="LBJ","election_date"]<-as.Date("1964-11-03")
pres[pres$Pres=="Nixon","election_date"]<-as.Date("1972-11-07")
pres[pres$Pres=="Ford","election_date"]<-as.Date("1976-11-02")
pres[pres$Pres=="Carter","election_date"]<-as.Date("1980-11-04")
pres[pres$Pres=="Reagan","election_date"]<-as.Date("1984-11-06")
pres[pres$Pres=="Bush1","election_date"]<-as.Date("1992-11-03")
pres[pres$Pres=="Clinton","election_date"]<-as.Date("1996-11-05")
pres[pres$Pres=="Bush2","election_date"]<-as.Date("2004-11-02")
pres[pres$Pres=="Obama","election_date"]<-as.Date("2012-11-06")
pres[pres$Pres=="Trump","election_date"]<-as.Date("2020-11-03")
head(pres)

#LBJ won his election less than a year after assuming office
#Ford had a short run up too due to Nixon resignation

#Summary stats
summ<-pres %>% group_by(Pres) %>% summarise(
  median=median(Value)
  ,mean=mean(Value)
  ,sd=sd(Value)
  ,low=min(Value)
  ,high=max(Value)
  ,range=high-low
  ,days=max(Day)
)

View(summ)

#Convert to factor for ordering in legend
pres[pres$Pres=="Bush1","Pres"]<-"H.W. Bush"
pres[pres$Pres=="Bush2","Pres"]<-"W. Bush"
pres$Pres2<-factor(pres$Pres,level=c("Truman","Eisenhower","JFK","LBJ","Nixon","Ford"
                                             ,"Carter","Reagan","H.W. Bush","Clinton","W. Bush","Obama","Trump"
))

################################
#Incumbent election result plots
################################

#Select Presidents who ran for re-election
#Comparison is iffy for LBJ and Ford vs Trump due to circumstance of taking office
election<-pres[pres$election_result %in% c('Won','Lost','Trump'),]

#Get days until election
election$days_until_election<-election$election_date-election$DayDate
#Make numeric
election$days_until_election<-as.numeric(election$days_until_election)

#Only include days before election
election<-election[election$days_until_election>0,]

#Winners
ggplot(election[election$election_result %in% c("Won","Trump"),]
       ,aes(days_until_election, Value, group=Pres,color=election_result)) +
  geom_line(lwd=1.92) +
  geom_text_repel(data=election %>% filter(days_until_election==1 &
                                             election_result %in% c("Won","Trump"))
                  ,aes(label=Pres2)
                  ,color="black",size=7.5
                  ,nudge_x=43
                  ) +
  scale_x_reverse(limits=c(1400,-100),breaks=rev(c(0,200,400,600,800,1000,1200,1400))) +
  labs(x="Days Until Election", y="Approval Rating") +
  ggtitle("Incumbent Winners") +
  scale_y_continuous(labels=scales::percent
                     ,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)
  ) +
  theme(text = element_text(size = 28)
        ,legend.key.size = unit(2,"line")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.position="none"
  ) +
  scale_colour_manual(values=c('#E41A1C','#00BFC4'))

#Losers
ggplot(election[election$election_result %in% c("Lost","Trump"),]
       ,aes(days_until_election, Value,group=Pres,color=election_result)) +
  geom_line(lwd=1.92) +
  geom_text_repel(data=election %>% filter(days_until_election==1 &
                                             election_result %in% c("Lost","Trump"))
                  ,aes(label=Pres2)
                  ,color="black",size=7.5
                  ,nudge_x=20
  ) +
  scale_x_reverse(limits=c(1400,-100),breaks=rev(c(0,200,400,600,800,1000,1200,1400))) +
  labs(x="Days Until Election", y="Approval Rating") +
  ggtitle("Incumbent Losers") +
  scale_y_continuous(labels=scales::percent
                     ,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)
  ) +
  theme(text = element_text(size = 28)
        ,legend.key.size = unit(2,"line")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.position="none"
  ) + scale_color_manual(values=c('#8DA0CB','#E41A1C'))


##################################
#OVERALL TREND LINE
##################################

#Helpful S.O. answer for this segment
#https://stackoverflow.com/questions/43006606/dividing-long-time-series-in-multiple-panels-with-ggplot2

#Convert to factor for ordering in legend
pres[pres$Pres=="Bush1","Pres"]<-"H.W. Bush"
pres[pres$Pres=="Bush2","Pres"]<-"W. Bush"
pres$Pres2<-factor(pres$Pres,level=c("Truman","Eisenhower","JFK","LBJ","Nixon","Ford"
                                     ,"Carter","Reagan","H.W. Bush","Clinton","W. Bush","Obama","Trump"
))

#Function to separate legend from plot
# http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

#Fool legend by adding president to each plot half
plot.dat<-subset(pres,select=c(Pres2,Value,DayDate))
all_pres<-unique(plot.dat$Pres2)
p1<-data.frame(Pres2=all_pres,Value=rep(-1,length(all_pres)),DayDate=rep(as.Date('1950-08-15'),length(all_pres)))
p1<-p1[p1$Pres2!="Truman",]
p2<-data.frame(Pres2=all_pres,Value=rep(-1,length(all_pres)),DayDate=rep(as.Date('2017-08-15'),length(all_pres)))
p2<-p2[p2$Pres2!="Trump",]
plot.dat<-rbind(plot.dat,p1,p2)

#Create plot
pl = map(list(c(as.Date('1945-06-06'),as.Date('1981-02-01')
                ,as.Date("1944-01-01"),as.Date("1980-01-01")), 
              c(as.Date('1981-02-02'),as.Date('2017-08-15')
                ,as.Date("1980-01-01"),as.Date("2016-01-01"))), 
  ~ ggplot(plot.dat %>% filter(DayDate >= .x[1], DayDate <= .x[2]), 
         aes(colour=Pres2)) +
  geom_line(aes(DayDate, Value), alpha=1,lwd=1.5) +
  scale_x_date(
    date_labels = "%Y"
    ,breaks=seq(from=as.Date(.x[3]),as.Date(.x[4]),"4 years")
  ) +
  scale_y_continuous(labels=scales::percent
                     ,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  labs(x="", y="", colour="") +
  theme(strip.background=element_blank(),
        strip.text=element_blank(),
        axis.title=element_blank(),
        text = element_text(size = 23)
        ,legend.key.size = unit(2,"line")
        ) +
    guides(colour = guide_legend(override.aes = list(lwd=3))) +
    scale_colour_manual(values=c(brewer.pal(8, "Dark2"),brewer.pal(5, "Set1")[2:5],brewer.pal(5, "Set1")[1]))
  )

# Extract legend as a separate graphics object
leg = g_legend(pl[[1]])

grid.arrange(arrangeGrob(grobs=map(pl, function(p) p + guides(colour=FALSE)), ncol=1),
             leg, ncol=2, widths=c(8.9,1)
             , left=textGrob("Approval Rating",rot=90,gp=gpar(fontsize=23))
             , bottom=textGrob("Year",gp=gpar(fontsize=23))
             )

####################################
#RECESSIONS
####################################

#Breaks for background rectangles
rects <- data.frame(
  xstart =c(as.Date("1945-02-01")
            ,as.Date("1948-11-01")
            ,as.Date("1953-07-01")
            ,as.Date("1957-08-01")
            ,as.Date("1960-04-01")
            ,as.Date("1969-12-01")
            ,as.Date("1973-11-01")
            ,as.Date("1980-01-01")
            ,as.Date("1981-07-01")
            ,as.Date("1990-07-01")
            ,as.Date("2001-03-01")
            ,as.Date("2007-12-01")
            ), 
  xend =c(as.Date("1945-11-30")
          ,as.Date("1949-10-30")
          ,as.Date("1954-05-30")
          ,as.Date("1958-04-30")
          ,as.Date("1961-02-28")
          ,as.Date("1970-11-30")
          ,as.Date("1975-03-30")
          ,as.Date("1980-07-30")
          ,as.Date("1982-11-30")
          ,as.Date("1991-03-30")
          ,as.Date("2001-11-30")
          ,as.Date("2009-06-30")
          )
  )

pl = map(list(c(as.Date('1945-06-06'),as.Date('1981-02-01')
                ,as.Date("1944-01-01"),as.Date("1980-01-01")), 
              c(as.Date('1981-02-02'),as.Date('2017-08-15')
                ,as.Date("1980-01-01"),as.Date("2016-01-01"))), 
         ~ ggplot(plot.dat %>% filter(DayDate >= .x[1], DayDate <= .x[2]), 
                  aes(colour=Pres2)) +
           geom_line(aes(DayDate, Value), alpha=1,lwd=1.5) +
           geom_rect(data = rects %>% filter(xstart >= .x[1], xend <= .x[2])
                     , aes(xmin = xstart, xmax = xend, 
                                       ymin = -Inf, ymax = Inf), alpha = 0.4,color="grey") +
           scale_x_date(
             date_labels = "%Y"
             ,breaks=seq(from=as.Date(.x[3]),as.Date(.x[4]),"4 years")
           ) +
           scale_y_continuous(labels=scales::percent
                              ,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
           labs(x="", y="", colour="") +
           theme(strip.background=element_blank(),
                 strip.text=element_blank(),
                 axis.title=element_blank(),
                 text = element_text(size = 23
                 )
                 ,legend.key.size = unit(2,"line")
           ) +
           guides(colour = guide_legend(override.aes = list(lwd=3))) +
           scale_colour_manual(values=c(brewer.pal(8, "Dark2"),brewer.pal(5, "Set1")[2:5],brewer.pal(5, "Set1")[1]))
)

# Extract legend as a separate graphics object
leg = g_legend(pl[[1]])

grid.arrange(arrangeGrob(grobs=map(pl, function(p) p + guides(colour=FALSE)), ncol=1),
             leg, ncol=2, widths=c(8.9,1)
             , left=textGrob("Approval Rating",rot=90,gp=gpar(fontsize=23))
             , bottom=textGrob("Year",gp=gpar(fontsize=23))
)

