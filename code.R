##FINAL PROJECT:
library(dplyr)
db<-read.csv("world-happiness-data.csv")
View(db)

##let's consider that data doesnot has any error(so we remove standard error column)
db<-db[,-5]

### we want all region in which country is find:
distinct(db,Region)

###let's create a new column continent for classification
db$continent<-NA

##australia
db$continent[which(db$Region %in% c("Australia and New Zealand"))]<-"Australia"
##north-america
db$continent[which(db$Region %in% c("North America"))]<-"North America"
##europe
db$continent[which(db$Region %in% c("Western Europe","Central and Eastern Europe"))]<-"Europe"
##africa
db$continent[which(db$Region %in% c("Sub-Saharan Africa","Middle East and Northern Africa"))]<-"Africa"
##asia
db$continent[which(db$Region %in% c("Eastern Asia","Southern Asia","Southeastern Asia"))]<-"Asia"
db$continent[which(db$Region %in% c("Latin America and Caribbean"))]<-"South America"

###let's create an average of all the numerical data continent wise:
hp<-aggregate(db[,4:11],list(db$continent),mean)
View(hp)

library(ggplot2)
library(corrgram)
library(corrplot)

##summary
summary(db)
head(db,10)
View(head(db,10))
tail(db,10)
View(tail(db,10))

##let's graph the mean data of all continent
plot<-ggplot(hp,aes(x=Group.1,y=Happiness.Score,fill=Group.1))
plot+geom_bar(stat="identity")+ggtitle("Happiness Score of each continent")+ylab("happiness score")+xlab("continent")

##let's find correlation in avriables
col<-sapply(db,is.numeric)#is.numeric is command it convert data in vector or metrix in place of list
cor.data<-cor(db[,col])
corrplot(cor.data,method="square",type="upper")
corrplot(cor.data,method="number",type="upper")

##let's create boxplot region wise
box<-ggplot(db,aes(x=Region,y=Happiness.Score,color=Region))
box+geom_boxplot()+geom_jitter(aes(color=Country),size=1.0)+ggtitle("Happiness score for Region and county")+coord_flip()+theme(legend.position = "none")

##boxplot for continents
box<-ggplot(db,aes(x=continent,y=Happiness.Score,color=continent))
box+geom_boxplot()+ggtitle("Happiness score for continent")

box<-ggplot(db,aes(x=Health..Life.Expectancy.,y=Happiness.Score))
box+geom_point(aes(color=continent),size=3,alpha=0.8)+geom_smooth(aes(color=continent),method="lm", fullrange=T)+facet_wrap(~continent)+theme_bw()+ggtitle("scatter plot for life expetancy")

##scatter plot for economy
box<-ggplot(db,aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))
box+geom_point(aes(color=continent),size=3,alpha=0.8)+geom_smooth(aes(color=continent),method="lm", fullrange=T)+facet_wrap(~continent)+theme_bw()+ggtitle("scatter plot for economy")
##all the graphs having positive corelation having highest corelation with south america

##for freedom:
box<-ggplot(db,aes(x=Freedom,y=Happiness.Score))
box+geom_point(aes(color=continent),size=3,alpha=0.8)+geom_smooth(aes(color=continent),method="lm", fullrange=T)+facet_wrap(~continent)+theme_bw()+ggtitle("scatter plot for freedom")

##for family:
box<-ggplot(db,aes(x=Family,y=Happiness.Score))
box+geom_point(aes(color=continent),size=3,alpha=0.8)+geom_smooth(aes(color=continent),method="lm", fullrange=T)+facet_wrap(~continent)+theme_bw()+ggtitle("scatter plot for family")


##for trust in government:
box<-ggplot(db,aes(x=Trust..Government.Corruption.,y=Happiness.Score))
box+geom_point(aes(color=continent),size=3,alpha=0.8)+geom_smooth(aes(color=continent),method="lm", fullrange=T)+facet_wrap(~continent)+theme_bw()+ggtitle("scatter plot for trust in government")




##box plot coclusion:
box<-ggplot(db,aes(x=Region,y=Happiness.Score,color=Region))
box+geom_boxplot()+geom_jitter(aes(color=Country),size=1.0)+ggtitle("Happiness score for Region and county")+coord_flip()+theme(legend.position = "none")

###classify all of the data based on happiest neutral and least happy region:
db$happinessmeter<-NA
db$happinessmeter[which(db$Region %in% c("Australia and New Zealand","Western Europe","North America"))]<-"Happiest"
db$happinessmeter[which(db$Region %in% c("Sub-Saharan Africa"))]<-"Least Happiest"
db$happinessmeter[which(db$Region %in% c("Latin America and Caribbean","Eastern Asia","Southern Asia","Southeastern Asia","Middle East and Northern Africa","Central and Eastern Europe"))]<-"Neutral"


##plot regression for all three region:
plot<-ggplot(db,aes(x=Health..Life.Expectancy.,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for life expetency")

##scatter plot for economy
plot<-ggplot(db,aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for economy")


##for freedom:
plot<-ggplot(db,aes(x=Freedom,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for freedom")


##for family:
plot<-ggplot(db,aes(x=Family,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for family")


##for trust in government:
plot<-ggplot(db,aes(x=Trust..Government.Corruption.,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for trust in government")

##for generosity:
plot<-ggplot(db,aes(x=Generosity,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for trust in generosity")

##for Dystopia residual:
plot<-ggplot(db,aes(x=Dystopia.Residual,y=Happiness.Score))
plot+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)+geom_smooth(aes(color=happinessmeter),method="lm", fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("scatter plot for dystopia residual")

##PLOT THE GDP AND HEALTH EXPENTANCY ON ON wORLD MAP
library(rworldmap)
d<-data.frame(Country=db$Country, value=db$Economy..GDP.per.Capita.)
n<-joinCountryData2Map(d,joinCode="NAME",nameJoinColumn = "Country") 
mapCountryData(n,nameColumnToPlot = "value",mapTitle = "World Map for GDP 2015 ")


#map for health expectency
d<-data.frame(Country=db$Country, value=db$Health..Life.Expectancy.)
n<-joinCountryData2Map(d,joinCode="NAME",nameJoinColumn = "Country")
mapCountryData(n,nameColumnToPlot = "value",mapTitle = "World Map for Health Expectancy 2015 ")