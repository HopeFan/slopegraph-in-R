## Import libraries
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

##Read the data file
life <- read.csv("LifeExpectancy.csv",header=TRUE)

## Rearrange the data as Country|1990|2013 for slopegraph. Filter the life expectancy data fo
## 1990 and 2013 into 2 temp variables and then extract the values into year1990 and year2013.
## All countries are extracted to variable group.
## Create a new data frame named 'a' with the 3 variables year1990,year2013, countries
## Set the variable years which will be used for x-axis as 23 (2013-1990)
tmp1990 <- life %>% filter(Year==1990)
tmp2013 <- life %>% filter(Year==2013)
year1990 <- tmp1990$Value
year2013 <- tmp2013$Value
group <- tmp2013$Country
years <- 23
a <- data.frame(year1990,year2013,group)

## Create the labels for the 2 ends of the slopegraph
## Left end (1990) will say COuntry,Value
## Right end(2013) will say value
## Round the values to get rid of the decimals
lab1990 <- paste(a$group, round(a$year1990),sep=",")
lab2013 <- paste(round(a$year2013), a$group, sep=",")

## Draw the initial plot

p <- ggplot(a) + 
  geom_segment(aes(x=0,xend=years,
                   y=year1990,yend=year2013),
               size=0.5)+
  ggtitle("Life Expectancy At Birth, 1990-2013")+theme(plot.title = element_text(face="bold",size=20,color="blue"))

## Set the theme background, grids, ticks, text and borders to blank
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())

## Adding extra space around the graph to accomodate the labels
p <- p+ xlim(-8,(years+9))
p <- p+ylim(min(a$year2013,a$year1990),max(a$year2013,a$year1990)+7)

## Set the Y axis title
p<-p+xlab("")+ylab("Life Expectancy")+
  theme(axis.title.y = element_text(size = 15, angle = 90))


## Set the labels on the slopegraph
## x variable is repeated with value as years (right end)
## hjust of 0, aligns text to left
## Size is the font size
p<-p+geom_text(label=lab2013,y=a$year2013,x=rep.int(years,length(a$year2013)),hjust=0, vjust=0.5,size=4)
p<-p+geom_text(label=lab1990,y=a$year1990,x=rep.int(0,length(a$year2013)),hjust=1, vjust=0.5,size=4)

## Set the label for the 2 ends of the slopegraph.
## Y value needs to be slightly greater than the max value of the data, to avoid overlap. 
## This spacing needs to be adjusted in the ylim above
p<-p+geom_text(label="1990",x=0,y=(1.05*(max(a$year2013,a$year1990))),hjust=0.2,size=5)
p<-p+geom_text(label="2013",x=years,y=(1.05*(max(a$year2013,a$year1990))),hjust=0,size=5)


## Show the plot on screen
print(p)

## Add footnote
grid.newpage()
footnote <- "Data Source: UN Data, Life expectancy at birth, total (years)"
g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)