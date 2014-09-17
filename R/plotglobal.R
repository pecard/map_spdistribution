#' Construcao dos mapas globais com basemap Openlayers ou Mapa de bits (geotiff)
#' 18/09/2014
# Packages necessarios
kpacks <- c('ggplot2','ggmap', 'rgdal', 'raster')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Diretorias ----------------------------------------------------------------------
wd_data <- 'R/Data'
wd_out  <- 'R/Out'
wd_shp <- 'Shp'
wd_r <- 'D:\\Programacao\\R\\mapas'

save.image(file.path(wd_r, 'mapglobaldata.RData'))


#' Dados Open-Layers ---------------------------------------------------------------
v_mapbox <- c('left' = -180.0, 'bottom' = -85, 'right' = 180, 'top' = 85)
map_loc <- get_map(
  location = v_mapbox, source = 'google', maptype = 'satellite')

#' Dados 2004 True Marble Earth Geotiff --------------------------------------------
#' Data can be obtained from:
#' http://neo.sci.gsfc.nasa.gov/view.php?datasetId=BlueMarbleNG-TB&date=2004-07-01
#' http://www.unearthedoutdoors.net/global_data/true_marble/download
basemap <- brick('S:/Raster/Cartografia/Mundo/BlueMarbleNG-TB_2004-12-01_rgb_1440x720.TIFF')
basemap@crs # check projection parameters

#' Fake data
ptos <- data.frame(x = rnorm(100, 0, 20), y = rnorm(100, 0, 20))
               

#' Plot Mapa com plotRGB
plotRGB(basemap, maxpixels = ncell(basemap))
with(ptos, points(x, y, col = 'yellow', pch = 19))

#' Plot mapa com Openlayers (google satellite ou outro) with ggmap
map <- ggmap(map_loc, extent = 'device', darken = c(.1, "white")) # Mapa base
map + 
  geom_point(data = ptos, aes(x = x, y = y), colour = 'yellow')

