rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(RCurl)
library(rgdal)
library(viridis)
library(countrycode)
library(sysfonts)
library(tidyverse)
library(sp)
library(data.table)
library(animation)
library(showtext)
library(directlabels)
library(rnaturalearth)

font_add_google(name="PT Sans", family="PT Sans")

#Get data from GISAH database
download1017 <- getURL("https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:2016;YEAR:2015;YEAR:2014;YEAR:2013;YEAR:2012;YEAR:2011;YEAR:2010&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv")
data1017 <- read.csv(text=download1017)
download0009 <- getURL("https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:2009;YEAR:2008;YEAR:2007;YEAR:2006;YEAR:2005;YEAR:2004;YEAR:2003;YEAR:2002;YEAR:2001;YEAR:2000&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv")
data0009 <- read.csv(text=download0009)
download8099 <- getURL("https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1999;YEAR:1998;YEAR:1997;YEAR:1996;YEAR:1995;YEAR:1994;YEAR:1993;YEAR:1992;YEAR:1991;YEAR:1990;YEAR:1989;YEAR:1988;YEAR:1987;YEAR:1986;YEAR:1985;YEAR:1984;YEAR:1983;YEAR:1982;YEAR:1981;YEAR:1980&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv")
data8099 <- read.csv(text=download8099)
download6079 <- getURL("https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1979;YEAR:1978;YEAR:1977;YEAR:1976;YEAR:1975;YEAR:1974;YEAR:1973;YEAR:1972;YEAR:1971;YEAR:1970;YEAR:1969;YEAR:1968;YEAR:1967;YEAR:1966;YEAR:1965;YEAR:1964;YEAR:1963;YEAR:1962;YEAR:1961;YEAR:1960&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv")
data6079 <- read.csv(text=download6079)

#Header=True doesn't work with this approach, so manually tidy data
colnames(data1017)=data1017[1,]
colnames(data0009)=data0009[1,]
colnames(data8099)=data8099[1,]
colnames(data6079)=data6079[1,]

#remove weird duplicate Nauru
data1017 <- subset(data1017, `3`!="  ")
data0009 <- subset(data0009, `3`!="  ")
data8099 <- subset(data8099, `3`!="  ")
data6079 <- subset(data6079, `2`!="  ")
data1017 <- data1017[-1,-2]
data0009 <- data0009[-1,-2]
data8099 <- data8099[-1,-2]
data6079 <- data6079[-1,-2]
names(data1017)[1] <- c("Country")
names(data0009)[1] <- c("Country")
names(data8099)[1] <- c("Country")
names(data6079)[1] <- c("Country")
names(data1017)[2] <- c("BevType")
names(data0009)[2] <- c("BevType")
names(data8099)[2] <- c("BevType")
names(data6079)[2] <- c("BevType")

#Stitch years together
data <- merge(data1017, data0009,by=c("Country", "BevType"), all=TRUE)
data <- merge(data, data8099,by=c("Country", "BevType"), all=TRUE)
data <- merge(data, data6079,by=c("Country", "BevType"), all=TRUE)

write.csv(data, file="Data/fullGISAH.csv")

#If no internet connection
#data <- fread("fullGISAH.csv", header=TRUE)

#Generate long data
data_long <- gather(data, key="Year", value="PCC", -one_of("Country", "BevType"))
data_long$Country <- as.character(data_long$Country)

#compare UK and US
tiff("Outputs/UKUSAPCC.tiff", units="in", width=8, height=6, res=300)
ggplot(subset(data_long, (Country=="United States of America" | 
                Country=="United Kingdom of Great Britain and Northern Ireland") & BevType==" All types"),
       aes(x=as.numeric(Year), y=PCC, colour=Country))+
  geom_line()+
  theme_classic()+
  scale_x_continuous(name="Year", breaks=seq(1960,2015, by=5))+
  scale_y_continuous(name="Per Capita Consumption (litres of alcohol per year)",
                     limits=c(0,12))+
  scale_colour_manual(guide=FALSE, values=c("#EB2055", "#57E0DE"))+
  geom_dl(aes(label=rep(c("UK", "USA"), times=114/2)),method="last.points")+
  labs(title="Per Capita Alcohol Consumption in the UK and USA", caption="Data from WHO GISAH database")
dev.off()

tiff("Outputs/UKUSAPCCBevType.tiff", units="in", width=12, height=6, res=300)
ggplot(subset(data_long, (Country=="United States of America" | 
                            Country=="United Kingdom of Great Britain and Northern Ireland") & 
                BevType!=" All types" & BevType!=" Other alcoholic beverages"),
       aes(x=as.numeric(Year), y=PCC, colour=Country))+
  geom_line()+
  theme_classic()+
  scale_x_continuous(name="Year", breaks=seq(1960,2015, by=5))+
  scale_y_continuous(name="Per Capita Consumption (litres of alcohol per year)")+
  scale_colour_manual(guide=FALSE, values=c("#EB2055", "#57E0DE"))+
  #geom_dl(aes(label=rep(c("UK", "USA"), times=456/2)),method="last.points")+
  facet_wrap(~BevType)+
  labs(title="Per Capita Consumption by drink type", caption="Data from WHO GISAH database")
dev.off()


#Fix countries that don't have complete records because of boundary changes etc.
#Firstly missing countries
#South Sudan - allocate consumption from Sudan
Country <- c(rep("S. Sudan", 5*57)) 
BevType <- c(rep(c(" All types", " Beer", " Wine", "Spirits", " Other alcoholic beverages"), 57))
Year <- c(rep(seq(from=1960, to=2016, by=1), each=5))
extra <- data.frame(Country, BevType, Year)
extra <- merge(extra, select(subset(data_long, Country=="Sudan"), -one_of("Country")), by=c("BevType", "Year"))

#Kosovo - allocate consumption from North Macedonia
Country <- c(rep("Kosovo", 5*57)) 
BevType <- c(rep(c(" All types", " Beer", " Wine", "Spirits", " Other alcoholic beverages"), 57))
Year <- c(rep(seq(from=1960, to=2016, by=1), each=5))
extra2 <- data.frame(Country, BevType, Year)
extra2 <- merge(extra2, select(subset(data_long, Country=="Republic of North Macedonia"), -one_of("Country")), by=c("BevType", "Year"))

#Somaliland - allocate consumption from Somalia
Country <- c(rep("Somaliland", 5*57)) 
BevType <- c(rep(c(" All types", " Beer", " Wine", "Spirits", " Other alcoholic beverages"), 57))
Year <- c(rep(seq(from=1960, to=2016, by=1), each=5))
extra3 <- data.frame(Country, BevType, Year)
extra3 <- merge(extra3, select(subset(data_long, Country=="Somalia"), -one_of("Country")), by=c("BevType", "Year"))

#Western Sahara - allocate consumption from Morocco
Country <- c(rep("W. Sahara", 5*57)) 
BevType <- c(rep(c(" All types", " Beer", " Wine", "Spirits", " Other alcoholic beverages"), 57))
Year <- c(rep(seq(from=1960, to=2016, by=1), each=5))
extra4 <- data.frame(Country, BevType, Year)
extra4 <- merge(extra4, select(subset(data_long, Country=="Morocco"), -one_of("Country")), by=c("BevType", "Year"))

#Liechtenstein - allocate consumption from Switzerland
Country <- c(rep("Liechtenstein", 5*57)) 
BevType <- c(rep(c(" All types", " Beer", " Wine", "Spirits", " Other alcoholic beverages"), 57))
Year <- c(rep(seq(from=1960, to=2016, by=1), each=5))
extra5 <- data.frame(Country, BevType, Year)
extra5 <- merge(extra5, select(subset(data_long, Country=="Switzerland"), -one_of("Country")), by=c("BevType", "Year"))

#Append new countries
data_long <- rbind(data_long, extra)
data_long <- rbind(data_long, extra2)
data_long <- rbind(data_long, extra3)
data_long <- rbind(data_long, extra4)
data_long <- rbind(data_long, extra5)

#Next countries missing in some years
#subset out missing years
data_miss <- subset(data_long, is.na(PCC))

data_miss$matchcountry <- case_when(
  data_miss$Country=="Andorra" & data_miss$Year<2000 ~ "France",
  data_miss$Country=="Armenia" & data_miss$Year<1990 ~ "Russian Federation",
  data_miss$Country=="Azerbaijan" & data_miss$Year<1990 ~ "Russian Federation",
  data_miss$Country=="Barbados" & data_miss$Year<1990 ~ "United Kingdom of Great Britain and Northern Ireland",
  data_miss$Country=="Bosnia and Herzegovina" & data_miss$Year<1990 ~ "Hungary",
  data_miss$Country=="Cambodia" & data_miss$Year<1982 ~ "Viet Nam",
  data_miss$Country=="Cook Islands" & data_miss$Year<1990 ~ "New Zealand",
  data_miss$Country=="Croatia" & data_miss$Year<1987 ~ "Hungary",
  data_miss$Country=="Dominica" & data_miss$Year<1990 ~ "United Kingdom of Great Britain and Northern Ireland",
  data_miss$Country=="Equatorial Guinea" & data_miss$Year<1983 ~ "Cameroon",
  data_miss$Country=="Estonia" & data_miss$Year<1990 ~ "Russian Federation",
  data_miss$Country=="Georgia" & data_miss$Year<1990 ~ "Russian Federation",
  data_miss$Country=="Grenada" & data_miss$Year<1990 ~ "United Kingdom of Great Britain and Northern Ireland",
  data_miss$Country=="Kazakhstan" & data_miss$Year<1990 ~ "Russian Federation",
  data_miss$Country=="Kosovo" & data_miss$Year<1992 ~ "Hungary",
  data_miss$Country=="Kyrgyzstan" & data_miss$Year<1985 ~ "Russian Federation",
  data_miss$Country=="Latvia" & data_miss$Year<1980 ~ "Russian Federation",
  data_miss$Country=="Lithuania" ~ "Russian Federation",
  data_miss$Country=="Maldives" & data_miss$Year<2000 ~ "Seychelles",
  data_miss$Country=="Montenegro" & data_miss$Year<2006 ~ "Hungary",
  data_miss$Country=="Nauru" & data_miss$Year<1999 ~ "Australia",
  data_miss$Country=="Puerto Rico" & data_miss$Year<2000 ~ "United States of America",
  data_miss$Country=="Republic of Moldova" & data_miss$Year<1992 ~ "Hungary",
  data_miss$Country=="Republic of North Macedonia" & data_miss$Year<1992 ~ "Hungary",
  data_miss$Country=="Niue" & data_miss$Year<1990 ~ "New Zealand",
  data_miss$Country=="Saint Vincent and the Grenadines" & data_miss$Year<1990 ~ "United Kingdom of Great Britain and Northern Ireland",
  data_miss$Country=="Serbia" & data_miss$Year<2006 ~ "Hungary",
  data_miss$Country=="Slovenia" & data_miss$Year<1981 ~ "Hungary",
  data_miss$Country=="Tajikistan" & data_miss$Year<1992 ~ "Russian Federation",
  data_miss$Country=="Timor-Leste" & data_miss$Year<1990 ~ "Indonesia",
  data_miss$Country=="Tonga" & data_miss$Year<1990 ~ "United Kingdom of Great Britain and Northern Ireland",
  data_miss$Country=="Turkmenistan" & data_miss$Year<1992 ~ "Russian Federation",
  data_miss$Country=="Tuvalu" & data_miss$Year<1990 ~ "United Kingdom of Great Britain and Northern Ireland",
  data_miss$Country=="Uzbekistan" & data_miss$Year<1992 ~ "Russian Federation",
  data_miss$Country=="Ukraine" & data_miss$Year<1980 ~ "Russian Federation",
  data_miss$Country=="Belarus" & data_miss$Year<1980 ~ "Russian Federation",
  data_miss$Country=="Belarus" & data_miss$Year<1988 & data_miss$Year>1979 & data_miss$BevType!=" All types" ~ "Russian Federation",
  TRUE ~ data_miss$Country
)

#merge in 'correct' data
data_miss <- merge(data_miss, data_long, by.x=c("matchcountry", "BevType", "Year"), by.y=c("Country", "BevType", "Year"), all.x=TRUE)
data_miss$PCC.x <- ifelse(is.na(data_miss$PCC.x), data_miss$PCC.y, data_miss$PCC.x)
data_miss <- select(data_miss, -one_of("PCC.y"))

#Bring back into main data
data_long <- merge(data_long, data_miss, by=c("Country", "BevType", "Year"), all.x=TRUE)
data_long$PCC <- ifelse(is.na(data_long$PCC), data_long$PCC.x, data_long$PCC)
data_long <- select(data_long, -one_of(c("PCC.x", "matchcountry")))

#Replace missing PCC values for other with 0
data_long$PCC <- ifelse(is.na(data_long$PCC) & data_long$BevType==" Other alcoholic beverages", 0, data_long$PCC)

data_long$Year <- as.integer(as.vector(data_long$Year))

#Repeat with missing years for countries with otherwise complete series
data_miss <- subset(data_long, is.na(PCC) & Year>1963 & Year<2016)
data_miss$matchyear <- case_when(
  data_miss$Country=="Bahrain" & data_miss$Year<1970 ~ 1970,
  data_miss$Country=="Bahrain" & data_miss$Year==1976 ~ 1975,
  data_miss$Country=="Bahrain" & data_miss$Year==1978 ~ 1977,
  data_miss$Country=="Bangladesh" & data_miss$Year==1983 ~ 1982,
  data_miss$Country=="Belize" & data_miss$Year==1989 ~ 1988,
  data_miss$Country=="Bhutan" & (data_miss$Year==2014 | data_miss$Year==2015) ~ 2013,
  data_miss$Country=="Brunei Darussalam" & data_miss$Year==1992 ~ 1991,
  data_miss$Country=="Burundi" & data_miss$Year>2011 ~ 2011,
  data_miss$Country=="Central African Republic" & data_miss$Year==1990 ~ 1989,
  data_miss$Country=="Comoros" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Cook Islands" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Czechia" & data_miss$Year==1990 ~ 1989,
  data_miss$Country=="Dominica" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Eritrea" & data_miss$Year>2011 ~ 2011,
  data_miss$Country=="Eritrea" & data_miss$Year==1975 ~ 1974,
  data_miss$Country=="Hungary" & data_miss$Year==2004 ~ 2003,
  data_miss$Country=="Lesotho" & data_miss$Year==1972 ~ 1971,
  data_miss$Country=="Libya" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Comoros" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Montenegro" & data_miss$Year==2004 ~ 2003,
  data_miss$Country=="Nauru" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Niue" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Puerto Rico" & data_miss$Year>2014 ~ 2014,
  data_miss$Country=="S. Sudan" & (data_miss$Year==2010 | data_miss$Year==2011) ~ 2009,
  data_miss$Country=="S. Sudan" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Serbia" & data_miss$Year==2004 ~ 2003,
  data_miss$Country=="Slovakia" & data_miss$Year==1990 ~ 1989,
  data_miss$Country=="Sudan" & (data_miss$Year==2010 | data_miss$Year==2011) ~ 2009,
  data_miss$Country=="Sudan" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Switzerland" & data_miss$Year<1966 ~ 1966,
  data_miss$Country=="Liechtenstein" & data_miss$Year<1966 ~ 1966,
  data_miss$Country=="Tonga" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="Tuvalu" & data_miss$Year>2013 ~ 2013,
  data_miss$Country=="United Arab Emirates" & data_miss$Year<1972 ~ 1972,
  TRUE ~ as.numeric(data_miss$Year)
  )

#merge in 'correct' data
data_miss <- merge(data_miss, data_long, by.x=c("Country", "BevType", "matchyear"), by.y=c("Country", "BevType", "Year"), all.x=TRUE)
data_miss$PCC.x <- ifelse(is.na(data_miss$PCC.x), data_miss$PCC.y, data_miss$PCC.x)
data_miss <- select(data_miss, -one_of("PCC.y"))

#Bring back into main data
data_long <- merge(data_long, data_miss, by=c("Country", "BevType", "Year"), all.x=TRUE)
data_long$PCC <- ifelse(is.na(data_long$PCC), data_long$PCC.x, data_long$PCC)
data_long <- select(data_long, -one_of(c("PCC.x", "matchyear")))

#Tidy up country codes - have to sort Eswatini manually
data_long$Country <- case_when(
  data_long$Country=="Eswatini" ~ "Swaziland",
  TRUE ~ data_long$Country
)
  
data_long$ISO3_CODE <- countrycode(data_long$Country, origin="country.name", destination="iso3c")
data_long$continent <- countrycode(data_long$Country, origin="country.name", destination="continent")

#fix countries which this doesn't work for
data_long$ISO3_CODE <- ifelse(data_long$Country=="Kosovo", "KOS", data_long$ISO3_CODE)
data_long$ISO3_CODE <- ifelse(data_long$Country=="S. Sudan", "SSD", data_long$ISO3_CODE)
data_long$ISO3_CODE <- ifelse(data_long$Country=="W. Sahara", "SAH", data_long$ISO3_CODE)
data_long$continent <- ifelse(data_long$Country=="Kosovo", "Europe", data_long$continent)
data_long$continent <- ifelse(data_long$Country=="S. Sudan", "Africa", data_long$continent)
data_long$continent <- ifelse(data_long$Country=="W. Sahara", "Africa", data_long$continent)

#generate wide data by bevtype for tricolore
data_wide <- spread(data_long, key=BevType, value=PCC)

#Read in shapefile from Natural Earth
polygons <- ne_download(scale=10, type="countries", category="cultural")
#polygons <- readOGR("Data/ne_10m_admin_0_countries.shp")
polygons$Country <- as.character(polygons$NAME)
polygons$ISO3_CODE <- as.character(polygons$ADM0_A3_IS)
polygons<-fortify(polygons, region="ISO3_CODE")

#Can't make this work with gganimate, so use alternate approach - requires {animation} package and ImageMagick
seq <- 1964:2015
seq <- append(seq,rep(2015, 10))
windows()
showtext_auto()

saveGIF({
for (i in seq) {
  a <- ggplot(subset(data_wide, Year==i))+
    geom_map(aes(map_id=ISO3_CODE, fill=` All types`), map=polygons, colour=alpha(0.0001))+
                  theme_classic()+
                  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
                        legend.position=c(0.08,0.4),text=element_text(family="PT Sans"))+
                  xlim(-170,180)+
                  ylim(-55,80)+
                  scale_fill_viridis(option='magma', direction=-1, name='Litres \nper person \nper year\n ', limits=c(0,25))+
                  labs(title='Changing alcohol consumption around the world',  subtitle=paste('Year: ', i, sep=''),
                  caption='Data from WHO GISAH database | Plot by @VictimOfMaths')+
                  coord_fixed(ratio=1.2)
  print(a)}
}, interval=0.3, movie.name="GISAHppp.gif",ani.width = 1200)


#zoomed in version with Europe only
saveGIF({
  for (i in seq) {
    a <- ggplot(subset(data_wide, Year==i))+
      geom_map(aes(map_id=ISO3_CODE, fill=` All types`), map=polygons, colour=alpha(0.0001))+
      theme_classic()+
      theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
            legend.position=c(0.1,0.25),text=element_text(family="PT Sans"))+
      xlim(-21,40)+
      ylim(35,70)+
      scale_fill_viridis(option='magma', direction=-1, name='Litres \nper person \nper year\n ', limits=c(0,25))+
      labs(title='Changing alcohol consumption across Europe',  subtitle=paste('Year: ', i, sep=''),
           caption='Data from WHO GISAH database | Plot by @VictimOfMaths')
    print(a)}
}, interval=0.3, movie.name="GISAHpppEur.gif", ani.width=600, ani.height=600)

#Messy line graph showing convergence of drinking levels
tiff("Outputs/EurDrinkTrends.tiff", units="in", width=3, height=3, res=200)
ggplot(subset(data_long, BevType==" All types" & continent=="Europe"), aes(x=Year, y=PCC, group=Country))+
  geom_line(colour="#1089F7", size=0.1)+
  theme_classic()+
  scale_y_continuous(name="Average alcohol consumption in litres per person per year")+
  theme(text=element_text(family="Roboto"))+
  labs(title="Changes in drinking levels across Europe", caption="Data from WHO GISAH database | Plot by @VIctimOfMaths")
dev.off()

#Show beverage preferences with a ternary colour scheme using the {tricolore} package
library(tricolore)
library(ggtern)
library(cowplot)

#identify countries we don't want to plot due to high volatility because of low levels of consumption
data_wide$" All types" <- ifelse(is.na(data_wide$" All types"), 0, data_wide$" All types")
data_wide$" Beer" <- ifelse(is.na(data_wide$" Beer"), 0, data_wide$" Beer")
data_wide$" Wine" <- ifelse(is.na(data_wide$" Wine"), 0, data_wide$" Wine")
data_wide$" Spirits" <- ifelse(is.na(data_wide$" Spirits"), 0, data_wide$" Spirits")

data_max <- data_wide %>%
  group_by(Country) %>%
  summarise(max=max(as.numeric(` All types`), na.rm=TRUE))

#cap at 1litre PCC
data_max$include <- ifelse(data_max$max<1, FALSE, TRUE)

data_wide <- merge(data_wide, data_max, by="Country")

tric <- Tricolore(data_wide, p1=" Beer", p2=" Spirits", p3=" Wine", breaks=100, show_data=FALSE)

data_wide$rgb <- tric$rgb

#set missing values to white
data_wide$rgb <- ifelse(is.na(data_wide$rgb), "#FFFFFF", data_wide$rgb)

#set values for low PCC countries to white to stop high volatility
data_wide$rgb <- ifelse(data_wide$include==FALSE, "#FFFFFF", data_wide$rgb)

#static 1965 & 2015 versions
tric1965 <- Tricolore(subset(data_wide, Year==1965), p1=" Beer", p2=" Spirits", p3=" Wine", breaks=100)
tric2015 <- Tricolore(subset(data_wide, Year==2015), p1=" Beer", p2=" Spirits", p3=" Wine", breaks=100)

key1965 <- tric1965$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Split of total consumption \nbetween drink types")+
  theme(tern.axis.title.T = element_text(hjust = -0.8, vjust = 2, angle = 0, size=9),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=9),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=9),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=10),
        axis.text=element_text(size=6))+
scale_x_continuous(name="")+
  scale_y_continuous(name="")

key2015 <- tric2015$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Split of total consumption \nbetween drink types")+
  theme(tern.axis.title.T = element_text(hjust = -0.8, vjust = 2, angle = 0, size=9),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=9),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=9),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=10),
        axis.text=element_text(size=6))+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")

key <- tric$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Split of total consumption \nbetween drink types")+
  theme(tern.axis.title.T = element_text(hjust = -0.8, vjust = 2, angle = 0, size=9),
    tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=9),
    tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=9),
    text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=10),
    axis.text=element_text(size=6))+
  geom_Tline(Tintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Rline(Rintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Lline(Lintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")

#for some reason you need to actually show the key for the plot to use it correctly afterwards
key
key1965
key2015

tiff("Outputs/GISAHTricolore1965.tiff", width=8, height=6, units="in", res=200)
ggdraw()+
  draw_plot(ggplot(subset(data_wide, Year==2015))+
              geom_map(aes(map_id=ISO3_CODE, fill=rgb), map=polygons, colour="Black", size=0.01)+
              theme_classic()+
              theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
                    text=element_text(family="PT Sans"))+
              xlim(-170,180)+
              ylim(-55,80)+
              scale_fill_identity()+
              labs(title="Drinking preferences in 2015",
                   caption="Excluding countries with very low levels of total consumption\n \nData from WHO GISAH database | Plot by @VictimOfMaths")+
              coord_fixed(ratio=1.2))+
 draw_plot(key2015, -0.05, 0.09, 0.4, 0.4)
dev.off()

plot_grid(key1965, key2015, align="V", labels=c("1965", "2015"))

saveGIF({
for (i in seq){
  b <- ggdraw()+
    draw_plot(ggplot(subset(data_wide, Year==i))+
    geom_map(aes(map_id=ISO3_CODE, fill=rgb), map=polygons, colour="Black", size=0.001)+
    theme_classic()+
      theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
            text=element_text(family="PT Sans"))+
      xlim(-170,180)+
      ylim(-55,80)+
      scale_fill_identity()+
      labs(title="The end of the one-drink culture? Changing drink preferences around the world", subtitle=paste('Year: ', i, sep=''),
           caption="Excluding countries with very low levels of total consumption\n \nData from WHO GISAH database | Plot by @VictimOfMaths")+
      coord_fixed(ratio=1.2))+
    draw_plot(key, -0.05, 0.09, 0.45, 0.45)
print(b)}
}, interval=0.3, movie.name="GISAHtricolore.gif", ani.width=1150)
    
  
#Europe-specific plot
saveGIF({
  for (i in seq){
    b <- ggdraw()+
      draw_plot(ggplot(subset(data_wide, Year==i))+
                  geom_map(aes(map_id=ISO3_CODE, fill=rgb), map=polygons, colour="Black", size=0.001)+
                  theme_classic()+
                  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
                        text=element_text(family="PT Sans"))+
                  xlim(-21,40)+
                  ylim(35,70)+
                  scale_fill_identity()+
                  labs(title="The end of the one-drink culture? Changing drink preferences around the world", subtitle=paste('Year: ', i, sep=''),
                       caption="Excluding countries with very low levels of total consumption\n \nData from WHO GISAH database | Plot by @VictimOfMaths")+
                  coord_fixed(ratio=1.2))+
      draw_plot(key, -0.05, 0.09, 0.45, 0.45)
    print(b)}
}, interval=0.3, movie.name="GISAHtricoloreEur.gif", ani.width=1150)


#Ternary plots of keys only for 1965 and 2015
#tric1965 <- Tricolore(data_wide_1965, p1=" Beer", p2=" Spirits", p3=" Wine", breaks=100, show_data=FALSE)
#tric2015 <- Tricolore(data_wide_2015, p1=" Beer", p2=" Spirits", p3=" Wine", breaks=100, show_data=FALSE)

#generate proportions manually as ternary coordinates
data_wide$beerprop <- data_wide$` Beer`/(data_wide$` Beer`+data_wide$` Spirits`+ data_wide$` Wine`)
data_wide$wineprop <- data_wide$` Wine`/(data_wide$` Beer`+data_wide$` Spirits`+ data_wide$` Wine`)
data_wide$spiritsprop <- data_wide$` Spirits`/(data_wide$` Beer`+data_wide$` Spirits`+ data_wide$` Wine`)

tric <- Tricolore(data_wide, p1=" Beer", p2=" Spirits", p3=" Wine", breaks=100, show_data=FALSE)

tiff("Outputs/GBTernPath.tiff", width=3, height=3, units="in", res=200)
tric$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Changes in drink preferences in the UK 1965-2015", 
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")+
  theme(tern.axis.title.T = element_text(hjust = -1, vjust = 2, angle = 0, size=12),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=12),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=12),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=16),
        axis.text=element_text(size=10))+
  geom_Tline(Tintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Rline(Rintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Lline(Lintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")+
  geom_path(data=subset(data_wide, ISO3_CODE=="GBR"), aes(beerprop, spiritsprop, wineprop), 
            arrow=arrow(angle=20, type="closed", length = unit(0.09, "inches")))
dev.off()

tiff("Outputs/EurTernArrow.tiff", width=4, height=4, units="in", res=200)
tric$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Changes in drink preferences across Europe 1965-2015", 
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")+
  theme(tern.axis.title.T = element_text(hjust = -1, vjust = 2, angle = 0, size=12),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=12),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=12),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=16),
        axis.text=element_text(size=10))+
  geom_Tline(Tintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Rline(Rintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Lline(Lintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")+
  geom_path(data=subset(data_wide, Year=="1965" | Year=="2015" & continent=="Europe"), aes(beerprop, spiritsprop, wineprop, group=Country), 
            arrow=arrow(angle=20, type="closed", length = unit(0.09, "inches"), ends="last"))
dev.off()

tiff("Outputs/AfrTernArrow.tiff", width=4, height=4, units="in", res=200)
tric$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Changes in drink preferences across Africa 1965-2015", 
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")+
  theme(tern.axis.title.T = element_text(hjust = -1, vjust = 2, angle = 0, size=12),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=12),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=12),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=16),
        axis.text=element_text(size=10))+
  geom_Tline(Tintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Rline(Rintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Lline(Lintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")+
  geom_path(data=subset(data_wide, Year=="1965" | Year=="2015" & continent=="Africa"), aes(beerprop, spiritsprop, wineprop, group=Country), 
            arrow=arrow(angle=20, type="closed", length = unit(0.09, "inches"), ends="last"))
dev.off()

tiff("Outputs/AmrTernArrow.tiff", width=4, height=4, units="in", res=200)
tric$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Changes in drink preferences across the Americas 1965-2015", 
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")+
  theme(tern.axis.title.T = element_text(hjust = -1, vjust = 2, angle = 0, size=12),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=12),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=12),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=16),
        axis.text=element_text(size=10))+
  geom_Tline(Tintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Rline(Rintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Lline(Lintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")+
  geom_path(data=subset(data_wide, Year=="1965" | Year=="2015" & continent=="Americas"), aes(beerprop, spiritsprop, wineprop, group=Country), 
            arrow=arrow(angle=20, type="closed", length = unit(0.09, "inches"), ends="last"))
dev.off()

tiff("Outputs/AsiaTernArrow.tiff", width=4, height=4, units="in", res=200)
tric$key+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Spirits")+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Beer")+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"), name="Wine")+
  labs(title="Changes in drink preferences across Asia/Oceania 1965-2015", 
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")+
  theme(tern.axis.title.T = element_text(hjust = -1, vjust = 2, angle = 0, size=12),
        tern.axis.title.L = element_text(hjust = 0.2, vjust = 1.1, angle = -60, size=12),
        tern.axis.title.R = element_text(hjust =0.8, vjust = 1.1, angle=60, size=12),
        text=element_text(family="PT Sans"), plot.title = element_text(vjust=-7, size=16),
        axis.text=element_text(size=10))+
  geom_Tline(Tintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Rline(Rintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  geom_Lline(Lintercept=c(0.25, 0.5, 0.75), colour="White", size=0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="")+
  geom_path(data=subset(data_wide, Year=="1965" | Year=="2015" & (continent=="Asia" | continent=="Oceania")), aes(beerprop, spiritsprop, wineprop, group=Country), 
            arrow=arrow(angle=20, type="closed", length = unit(0.09, "inches"), ends="last"))
dev.off()




