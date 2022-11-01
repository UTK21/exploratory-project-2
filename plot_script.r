if(!dir.exists("./data")){dir.create("./data")}
if(!file.exists("./data/NEIdata.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",destfile = "./data/NEIdata.zip")
}
if(!file.exists("./data/Source_Classification_Code.rds")| !file.exists("./data/summarySCC_PM25.rds")){
  unzip("./data/NEIdata.zip",exdir = "./data")}


NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")


#PLOT 1

totalemissions <- tapply(NEI$Emissions,NEI$year,sum)
barplot(totalemissions,
        main = "Total emissions in US" ,
        xlab= "year",
        ylab = "emissions(tons)")
dev.copy(png,file="plot1.png")
dev.off



#PLOT 2
Baltimoredata <- subset(NEI,NEI$fips =="24510")
data2 <-tapply(Baltimoredata$Emissions,Baltimoredata$year,sum)
barplot(data2,
        main = "Total emissions in Baltimore City, Maryland" ,
        xlab= "year",
        ylab = "emissions(tons)")
dev.copy(png,file = "plot2.png")
dev.off()


#PLOT 3
data3 <- aggregate(Emissions ~ year + type,Baltimoredata, sum)
library(ggplot2)
g <- ggplot(data3, aes(year, Emissions, color = type))
g + geom_line()+ labs(title=" Total Emissions in Baltimore City, Maryland")
dev.copy(png,file = "plot3.png")
dev.off()


#PLOT 4
SCC.coal = SCC[(grepl(x = SCC$Short.Name, pattern = "Coal", ignore.case=TRUE)),]
NEI.coal = merge(NEI, SCC.coal, by = "SCC")
Emissions.coal <- aggregate(Emissions ~ year,NEI.coal,sum)
Emissions.coal$group=rep("coal",nrow(Emissions.coal))
ggplot(Emissions.coal, aes(year,Emissions,label = round(Emissions)))+
  geom_line(lty =2)+
  geom_point(pch=19) + 
  geom_text(check_overlap = TRUE,vjust = 1, hjust = 1.2)+
  labs(title="Total Coal-sourced emissions") +
  xlim(1996,2011)
dev.copy(png,file = "plot4.png")
dev.off()


#PLOT 5
BaltimoreRoad <- subset(Baltimoredata,type == "ON-ROAD")
data5 <- aggregate(Emissions ~ year, BaltimoreRoad ,sum)
ggplot(data5,aes(year,Emissions,label = round(Emissions)))+
  geom_line(lty =2) +
  geom_point() +
  geom_text(vjust = 1, hjust = .8) +
  labs(title = "Emissions in Baltimore from motor vehicle sources")
dev.copy(png,file = "plot5.png")
dev.off()


#PLOT 6
LosAngelesroad <-subset(NEI,NEI$fips =="06037" & type == "ON-ROAD")
data6 <- aggregate(Emissions ~ year, LosAngelesroad ,sum)
data5$city<- "Baltimore"
data6$city<- "LA"
twodata <- rbind(data5,data6)
ggplot(twodata,aes(year,Emissions,label = round(Emissions),group=factor(city),col=city,label=Emissions))+
  geom_line() +
  geom_point() +
  geom_text(vjust=1) +
  labs(title = "Emissions in Baltimore City and Los Angeles  from motor vehicle")
dev.copy(png,file = "plot6.png")
dev.off()