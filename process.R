library(lubridate)
library(rnaturalearth)
library(sf)

dfrEmDat <- rbind(read.csv('data-raw/em-dat-GHA-IND-KEN-TZA-YEM.csv'),
                  read.csv('data-raw/em-dat-PHL.csv'))

## sfPhl <- ne_countries(country='Philippines', returnclass='sf')
## sfPhlBbox <- st_make_grid(sfPhl, n=1)

dfrEmDat $Start.date <- gsub('(.*?)/(.*?)/(.*)', '\\3/\\2/\\1', dfrEmDat $Start.date)
dfrEmDat $Start.date <- ymd(dfrEmDat $Start.date, truncated=2)
dfrEmDat $End.date <- gsub('(.*?)/(.*?)/(.*)', '\\3/\\2/\\1', dfrEmDat $End.date)
dfrEmDat $End.date <- ymd(dfrEmDat $End.date, truncated=2)

dfrEmDat $Latitude <- as.numeric(gsub('[[:alpha:]]', '', dfrEmDat $Latitude))
dfrEmDat $Longitude <- as.numeric(gsub('[[:alpha:]]', '', dfrEmDat $Longitude))

geom <- st_sfc(by(dfrEmDat[, c('Latitude', 'Longitude')], 1:nrow(dfrEmDat), function(x) {
    st_point(as.numeric(x))}))

dfrEmDat <- dfrEmDat[, !names(dfrEmDat)%in%c('Latitude', 'Longitude')]
dfrEmDat <- st_sf(dfrEmDat, geom)

save(dfrEmDat, file='data/dfrEmDat.rda')
