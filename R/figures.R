#' Produce figures
#'
#' Functions to produce figures for Chui et al 2017
#'
#' plotYear produces a temporal heatmap of disaster frequency by country-year
#' and disaster type.
#'
#' plotMonth produces a temporal heatmap of disaster frequency by country-month
#' and disaster type.  Month values range from 1 to 12.
#'
#' plotGHA produces a line graph of disaster frequency by year and a point cloud
#' of disaster frequency by month for Ghana. Month values range from 1 to 12.
#'
#' plotIND produces a line graph of disaster frequency by year and a point cloud
#' of disaster frequency by month for India. Month values range from 1 to 12.
#' 
#' plotKEN produces a line graph of disaster frequency by year and a point cloud
#' of disaster frequency by month for Kenya. Month values range from 1 to 12.
#' 
#' plotTZA produces a line graph of disaster frequency by year and a point cloud
#' of disaster frequency by month for Tanzania. Month values range from 1 to 12.
#' 
#' plotYEM produces a line graph of disaster frequency by year and a point cloud
#' of disaster frequency by month for Yemen. Month values range from 1 to 12.
#' 
#' plotPHL produces a line graph of disaster frequency by year and a point cloud
#' of disaster frequency by month for Philippines. Month values range from 1 to 12.
#'
#' @param o Character vector specifying output file(s).
#' @param form Character variable specifying output format:
#'   \describe{
#'     \item{pdf}{Adobe Portable document format}
#'     \item{png}{Portable network graphics}
#'     \item{jpeg}{Joint Photographic Experts Group}
#'     \item{eps}{Encapsulated postscript}
#'     \item{tiff}{Tagged image file format}
#'     \item{bmp}{Bitmap}
#' }
#' @return imperative function--writes files to disk and returns \code{NULL}.
#' @name figures
NULL

#' @rdname plotYear
plotYear <- function(o, form='pdf') {
    # Parse years
    years = year(dfrEmDat$Start.date)
    dfrEmDat$Disaster.type <- factor(dfrEmDat$Disaster.type)
    
    # Group country, year, and disaster type
    cdt <- count(dfrEmDat,c("Country","Disaster.type"))
    cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
    
  pdf(o)  
    # Plot disasters by year by country
    g <- ggplot(cyrd,aes(x=years,y=Disaster.type))
    g + geom_point() + facet_grid(Country~.) + ylab("Disaster type")
  dev.off()
}

#' @rdname plotMonth
plotMonth <- function(o, form='pdf') {
  # Parse months
  months = month(dfrEmDat$Start.date)
  # Group by country, month, disaster
  cmod <- data.frame(
    "Country" = dfrEmDat$Country,
    "month" = months,
    "disaster" = dfrEmDat$Disaster.type
  )
  cmod1 <- count(cmod,c("Country","month","disaster"))
  
  pdf(o)
    # Plot disasters by month by country
    gg <- ggplot(cmod1,aes(x=month,y=disaster))
    gg + geom_point() + facet_grid(.~Country) + ylab("Disaster type")
  dev.off()
}

#' @rdname plotGHA
plotGHA <- function(o, form='pdf') {
  # Plot Ghana disaster data by year
  ghana.yr <- cyrd[which(cyrd$Country == "Ghana"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  # For GHA
  ghana.cont <- data.frame(
    Country = rep("Ghana",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  ghana.cont <- merge(ghana.cont,ghana.yr,by=c("Country","years","Disaster.type"),all=T)
  ghana.cont[is.na(ghana.cont)] <- 0
  
  # Plot
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(ghana.cont,aes(x=years)) +
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Ghana,years") +
      ylab("disaster type") + theme_bw()
  dev.off()
  
  # Plot Ghana disaster data by month
  # ghana.mo <- cmod1[which(cmod1$Country == "Ghana"), ]
  # mycol <- RColorBrewer::brewer.pal(11,"Paired")
  # 
  # pdf(o)
  #   par(mar = c(4.5,4.5,1.5,1))
  #   with(ghana.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
  #   with(ghana.mo,legend("topright",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
  #   title("Ghana, months")
  # dev.off()
}

#' @rdname plotIND
plotIND <- function(o, form='pdf') {
  # Plot India disaster data by year
  india.yr <- cyrd[which(cyrd$Country == "India"), ]
  
  # Generate time series from points  
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  # For IND
  india.cont <- data.frame(
    Country = rep("India",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  india.cont <- merge(india.cont,india.yr,by=c("Country","years","Disaster.type"),all=T)
  india.cont[is.na(india.cont)] <- 0
  
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(india.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="India,years") +
      ylab("disaster type") + theme_bw()
  dev.off()
  
  # Plot India disaster data by month
  # india.mo <- cmod1[which(cmod1$Country == "India"), ]
  # mycol <- RColorBrewer::brewer.pal(11,"Paired")
  # 
  # pdf(o)
  #   par(mar = c(4.5,4.5,1.5,1))
  #   with(india.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
  #   with(india.mo,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
  #   title("India, months")
  # dev.off()
}

#' @rdname plotKEN
plotKEN <- function(o, form='pdf') {
  # Plot Kenya disaster data by year
  kenya.yr <- cyrd[which(cyrd$Country == "Kenya"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  # For KEN
  kenya.cont <- data.frame(
    Country = rep("Kenya",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  kenya.cont <- merge(kenya.cont,india.yr,by=c("Country","years","Disaster.type"),all=T)
  kenya.cont[is.na(kenya.cont)] <- 0
  
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(kenya.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Kenya,years") +
      ylab("disaster type") + theme_bw()
  dev_off()
  
  # Plot Kenya disaster data by month
  # kenya.mo <- cmod1[which(cmod1$Country == "Kenya"), ]
  # mycol <- RColorBrewer::brewer.pal(11,"Paired")
  # 
  # pdf(o)
  #   par(mar = c(4.5,4.5,1.5,1))
  #   with(kenya.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
  #   with(kenya.mo,legend("topright",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=0.9,title="disaster type"))
  #   title("Kenya, months")
  # dev.off()
}

#' @rdname plotTZA
plotTZA <- function(o, form='pdf') {
  # Plot Tanzania disaster data by year
  tanza.yr <- cyrd[which(cyrd$Country == "Tanzania, United Republic of"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  # For TZA
  tanza.cont <- data.frame(
    Country = rep("Tanzania, United Republic of",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  tanza.cont <- merge(tanza.cont,tanza.yr,by=c("Country","years","Disaster.type"),all=T)
  tanza.cont[is.na(tanza.cont)] <- 0
  
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(tanza.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Tanzania,years") +
      ylab("disaster type") + theme_bw()
  dev_off()
  
  # Plot Tanzania disaster data by month
  # tanza.mo <- cmod1[which(cmod1$Country == "Tanzania, United Republic of"), ]
  # mycol <- RColorBrewer::brewer.pal(11,"Paired")
  # 
  # pdf(o)
  #   par(mar = c(4.5,4.5,1.5,1))
  #   with(tanza.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
  #   with(tanza.mo,legend("topright",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
  #   title("Tanzania, months")
  # dev.off()
}

#' @rdname plotYEM
plotYEM <- function(o, form='pdf') {
  # Plot Yemen disaster data by year
  yemen.yr <- cyrd[which(cyrd$Country == "Yemen"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  # For YEM
  yemen.cont <- data.frame(
    Country = rep("Yemen",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  yemen.cont <- merge(yemen.cont,yemen.yr,by=c("Country","years","Disaster.type"),all=T)
  yemen.cont[is.na(yemen.cont)] <- 0
  
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(yemen.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Yemen,years") +
      ylab("disaster type") + theme_bw()
  dev.off()
  
  # Plot Yemen disaster data by month
  # yemen.mo <- cmod1[which(cmod1$Country == "Yemen"), ]
  # mycol <- RColorBrewer::brewer.pal(11,"Paired")
  # 
  # pdf(o)
  #   par(mar = c(4.5,4.5,1.5,1))
  #   with(yemen.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
  #   with(yemen.mo,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
  #   title("Yemen, months")
  # dev.off()
}

#' @rdname plotPHL
plotPHL <- function(o, form='pdf') {
  # Plot Philippines disaster data by year
  phili.yr <- cyrd[which(cyrd$Country == "Philippines (the)"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  # For PHL
  phili.cont <- data.frame(
    Country = rep("Philippines (the)",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  phili.cont <- merge(phili.cont,phili.yr,by=c("Country","years","Disaster.type"),all=T)
  phili.cont[is.na(phili.cont)] <- 0
  
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(phili.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Philippines,years") +
      ylab("disaster type") + theme_bw()
  dev_off()
  
  # Plot Philippines disaster data by month
  # phili.mo <- cmod1[which(cmod1$Country == "Philippines (the)"), ]
  # mycol <- RColorBrewer::brewer.pal(11,"Paired")
  # 
  # pdf(o)
  #   par(mar = c(4.5,4.5,1.5,1))
  #   with(phili.mo,plot(month,freq,pch=16,col=mycol,ylab = "frequency"))
  #   with(phili.mo,legend("topleft",legend=levels(dfrEmDat$Disaster.type),col=mycol,pch=16,cex=1,title="disaster type"))
  #   title("Philippines, months")
  # dev.off()
}