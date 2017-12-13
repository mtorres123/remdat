# Load libraries
library(lubridate)
library(plyr)
library(tidyr)
library(stats)
library(ggplot2)
library(grDevices)

# Load remdat data
load("~/CAMP/Data/Packages/remdat/data/dfrEmDat.rda")

# Parse years and months
years = year(dfrEmDat$Start.date)
months = month(dfrEmDat$Start.date)

dfrEmDat$Disaster.type <- factor(dfrEmDat$Disaster.type)

####### country-year

# Group country, year, and disaster type
cdt <- count(dfrEmDat,c("Country","Disaster.type"))
cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
dyrc <- count(dfrEmDat,c("Disaster.type","years","Country"))
cyrdsub <- count(dfrEmDat,c("Country","years","Disaster.type","Disaster.subtype"))

# Plot disasters by year by country
g <- ggplot(cyrd,aes(x=years,y=Disaster.type,color=freq))
g + geom_point() + facet_grid(Country~.) + ylab("Disaster type") + 
  scale_color_distiller(palette="Spectral")

# Parse into individual countries: point data
ghana.yr <- cyrd[which(cyrd$Country == "Ghana"), ]
india.yr <- cyrd[which(cyrd$Country == "India"), ]
kenya.yr <- cyrd[which(cyrd$Country == "Kenya"), ]
tanza.yr <- cyrd[which(cyrd$Country == "Tanzania, United Republic of"), ]
yemen.yr <- cyrd[which(cyrd$Country == "Yemen"), ]
phili.yr <- cyrd[which(cyrd$Country == "Philippines (the)"), ]

# Create time series of point data
ldis <- unlist(levels(dfrEmDat$Disaster.type))
yrvec <- min(years):max(years)
len <- length(yrvec)*length(ldis)
yrbuild <- c()
for (i in seq_along(yrvec)){
  yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
}

# Give colors to disasters
#mycol <- RColorBrewer::brewer.pal(11,"Paired")
  #c('darkgoldenrod1','brown','black','red','blue','green','tan3','darkmagenta','violet','darkgray','darkorange')

## Plot frequency of disasters by year

# Ghana
ghana.cont <- data.frame(
  Country = rep("Ghana",len),
  years = yrbuild,
  Disaster.type = rep(ldis,len/11)
)
ghana.cont <- merge(ghana.cont,ghana.yr,by=c("Country","years","Disaster.type"),all=T)
ghana.cont[is.na(ghana.cont)] <- 0

par(mar = c(4.5,4.5,1.5,1))
ggplot(ghana.cont,aes(x=years)) +
    geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
    scale_color_distiller(palette="Spectral") +
    labs(color="freq",title="Ghana,years") +
    ylab("disaster type") + theme_bw()
# with(ghana.yr,plot(years,freq,pch=16,col=mycol,ylab = "frequency"))
# with(ghana.yr,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
# title('Ghana, years')

# India
india.cont <- data.frame(
  Country = rep("India",len),
  years = yrbuild,
  Disaster.type = rep(ldis,len/11)
)
india.cont <- merge(india.cont,india.yr,by=c("Country","years","Disaster.type"),all=T)
india.cont[is.na(india.cont)] <- 0

par(mar = c(4.5,4.5,1.5,1))
ggplot(india.cont,aes(x=years))+
  geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
  scale_color_distiller(palette="Spectral") +
  labs(color="freq",title="India,years") +
  ylab("disaster type") + theme_bw()
#with(india.yr,plot(years,freq,pch=16,col=mycol,ylab = "frequency"))
#with(india.yr,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
#title('India, years')

# Kenya
kenya.cont <- data.frame(
  Country = rep("Kenya",len),
  years = yrbuild,
  Disaster.type = rep(ldis,len/11)
)
kenya.cont <- merge(kenya.cont,india.yr,by=c("Country","years","Disaster.type"),all=T)
kenya.cont[is.na(kenya.cont)] <- 0

par(mar = c(4.5,4.5,1.5,1))
ggplot(kenya.cont,aes(x=years))+
  geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
  scale_color_distiller(palette="Spectral") +
  labs(color="freq",title="Kenya,years") +
  ylab("disaster type") + theme_bw()

#with(kenya.yr,plot(years,freq,pch=16,col=mycol,ylab = "frequency"))
#with(kenya.yr,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
#title('Kenya, years')

# Tanzania
tanza.cont <- data.frame(
  Country = rep("Tanzania, United Republic of",len),
  years = yrbuild,
  Disaster.type = rep(ldis,len/11)
)
tanza.cont <- merge(tanza.cont,tanza.yr,by=c("Country","years","Disaster.type"),all=T)
tanza.cont[is.na(tanza.cont)] <- 0

par(mar = c(4.5,4.5,1.5,1))
ggplot(tanza.cont,aes(x=years))+
  geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
  scale_color_distiller(palette="Spectral") +
  labs(color="freq",title="Tanzania,years") +
  ylab("disaster type") + theme_bw()

#with(tanza.yr,plot(years,freq,pch=16,col=mycol,ylab = "frequency"))
#with(tanza.yr,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
#title('Tanzania, years')

# Yemen
yemen.cont <- data.frame(
  Country = rep("Yemen",len),
  years = yrbuild,
  Disaster.type = rep(ldis,len/11)
)
yemen.cont <- merge(yemen.cont,yemen.yr,by=c("Country","years","Disaster.type"),all=T)
yemen.cont[is.na(yemen.cont)] <- 0

par(mar = c(4.5,4.5,1.5,1))
ggplot(yemen.cont,aes(x=years))+
  geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
  scale_color_distiller(palette="Spectral") +
  labs(color="freq",title="Yemen,years") +
  ylab("disaster type") + theme_bw()

#with(yemen.yr,plot(years,freq,pch=16,col=mycol,ylab = "frequency"))
#with(yemen.yr,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
#title('Yemen, years')

# Philippines
phili.cont <- data.frame(
  Country = rep("Philippines (the)",len),
  years = yrbuild,
  Disaster.type = rep(ldis,len/11)
)
phili.cont <- merge(phili.cont,phili.yr,by=c("Country","years","Disaster.type"),all=T)
phili.cont[is.na(phili.cont)] <- 0

par(mar = c(4.5,4.5,1.5,1))
ggplot(phili.cont,aes(x=years))+
  geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
  scale_color_distiller(palette="Spectral") +
  labs(color="freq",title="Philippines,years") +
  ylab("disaster type") + theme_bw()

#with(phili.yr,plot(years,freq,pch=16,col=mycol,ylab = "frequency"))
#with(phili.yr,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
#title('Philippines, years')

####### country-month

# Group by country, month, disaster
cmod <- data.frame(
  "Country" = dfrEmDat$Country,
  "month" = months,
  "disaster" = dfrEmDat$Disaster.type
)
cmod1 <- count(cmod,c("Country","month","disaster"))

# Plot disasters by month by country
gg <- ggplot(cmod1,aes(x=month,y=disaster,color=freq))
gg + geom_point() + facet_grid(.~Country) + ylab("Disaster type") +
  scale_colour_distiller(palette="Spectral")

# Parse into individual countries
ghana.mo <- cmod1[which(cmod1$Country == "Ghana"), ]
india.mo <- cmod1[which(cmod1$Country == "India"), ]
kenya.mo <- cmod1[which(cmod1$Country == "Kenya"), ]
tanza.mo <- cmod1[which(cmod1$Country == "Tanzania, United Republic of"), ]
yemen.mo <- cmod1[which(cmod1$Country == "Yemen"), ]
phili.mo <- cmod1[which(cmod1$Country == "Philippines (the)"), ]

## Plot frequency of disasters by month

# Ghana
par(mar = c(4.5,4.5,1.5,1))
with(ghana.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
with(ghana.mo,legend("topright",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
title("Ghana, months")

# India
par(mar = c(4.5,4.5,1.5,1))
with(india.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
with(india.mo,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
title("India, months")

# Kenya
par(mar = c(4.5,4.5,1.5,1))
with(kenya.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
with(kenya.mo,legend("topright",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=0.9,title="disaster type"))
title("Kenya, months")

# Tanzania
par(mar = c(4.5,4.5,1.5,1))
with(tanza.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
with(tanza.mo,legend("topright",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
title("Tanzania, months")

# Yemen
par(mar = c(4.5,4.5,1.5,1))
with(yemen.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
with(yemen.mo,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
title("Yemen, months")

# Philippines
par(mar = c(4.5,4.5,1.5,1))
with(phili.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
with(phili.mo,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
title("Philippines, months")