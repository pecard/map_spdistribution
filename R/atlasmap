# Atlas

kpacks <- c("ggplot2", 'plyr', 'maptools', 'ggmap','RCurl',
            'lubridate', 'scales', 'gridExtra')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)


# Grelha
grid500 <- maptools::readShapeSpatial(paste0(wddata, 'Grelha500mCarcaveloswgs84.shp'))
sp::proj4string(grid500) = CRS('+init=epsg:4326')
griddf <- ggplot2::fortify(grid500, region = 'IDQ') #! Dataframe
griddf$region <-griddf$id
ctd <- data.frame(grid500@data, coordinates(grid500))
names(ctd) <- c('id', 'x', 'y')

# Link aos spreadsheets google
# Pontos
tlink = getForm("https://docs.google.com/spreadsheet/pub", 
                hl ="en_US", key = "0AvBPYf4vFckidGxDR2FiLVhfOThfb1d4V3NfTUdLYmc", 
                output = "csv", 
                .opts = list(followlocation = TRUE, verbose = TRUE, ssl.verifypeer = FALSE)) 
ptos_atlas <- read.csv(textConnection(tlink), stringsAsFactors = F)
ptos_atlas$metodo <- 'pto'
# Transectos
#https://docs.google.com/spreadsheet/pub?key=0AvBPYf4vFckidGxSWWhFOVdobkwtdS1FRVlRdC0yQ2c&output=csv
tlink = getForm("https://docs.google.com/spreadsheet/pub", 
                hl ="en_US", key = "0AvBPYf4vFckidGxSWWhFOVdobkwtdS1FRVlRdC0yQ2c", 
                output = "csv", 
                .opts = list(followlocation = TRUE, verbose = TRUE, ssl.verifypeer = FALSE)) 
tran_atlas <- read.csv(textConnection(tlink), stringsAsFactors = F)
tran_atlas$metodo <- 'trans'
# Suplementares
#https://docs.google.com/spreadsheet/pub?key=0AvBPYf4vFckidENqV3NIdUloZHVZU3lnTXFyb0p4ZXc&output=csv
tlink = getForm("https://docs.google.com/spreadsheet/pub", 
                hl ="en_US", key = "0AvBPYf4vFckidENqV3NIdUloZHVZU3lnTXFyb0p4ZXc", 
                output = "csv", 
                .opts = list(followlocation = TRUE, verbose = TRUE, ssl.verifypeer = FALSE)) 
supl_atlas <- read.csv(textConnection(tlink), stringsAsFactors = F)
supl_atlas$metodo <- 'supl'
# Merge all
all_data <- plyr::rbind.fill(ptos_atlas, tran_atlas, supl_atlas)
all_data$tnind[is.na(all_data$tnind)] <- 0
head(all_data)
# Basemap
#basemapgoogle <- qmap(c(lon = mean(griddf$lon), mean(griddf$lat)), source ="google",
#                      zoom=14, extent = "panel", maprange = FALSE)
map_loc <- get_map(location = c(lon = mean(griddf$lon), mean(griddf$lat)),
                   source = 'google', zoom = 14)
map <- ggmap(map_loc, extent = 'device')

# Mapa distribuicao especies
esp_list <- unique(all_data$esp)
pdf(paste0(wddata,'\\mapas\\Atlas Carvavelos 2013.pdf'), paper = "a4r", width = 0, height = 0)
for(i in esp_list) {
  spi <- ddply(subset(all_data, especie == i),
               .(quad), summarise, n = max(tnind, na.rm = T), .drop = T)
  spi <- merge(ctd, spi, by.x = 'id',
               by.y = 'quad', all = T)
  spi$n[is.na(spi$n)] <- 0
  p <-  map +
    #ggplot(aes(map_id = quad), data = spi) +
    geom_map(inherit.aes = F,
             aes(map_id = id, fill = n), color = 'white',
             alpha = .7, map = griddf, data = spi) +
    annotate('text', x = spi$x, y = spi$y, label = spi$n, size = 3) + 
    scale_fill_gradient2(paste0(i,'\nAbundancia'),
                         low = "lightblue",
                         mid = 'blue',
                         high= "red",
                         midpoint = median(range(spi$n, na.rm = T))) +
    expand_limits(x = griddf$long, y = griddf$lat)
  print(p)
  #ggsave(filename = paste0(wddata,'Atlas Carvavelos 2013_', i,'_.pdf'),
  #       plot = last_plot())
}
dev.off()


# Mapa Riqueza especifica
summ <- ddply(subset(all_data, metodo %in% c('pto', 'trans')),
              .(quad), summarise, n = length(unique(especie)))
summ <- merge(ctd, summ, by = 'quad', all.x = T)
gr_summ <- merge(griddf, summ, by.x = 'id', by.y = 'quad', all.x = T, sort = T)
gr_summ <- gr_summ[order(gr_summ$order), ]

basemapgoogle + 
  geom_polygon(aes.inherit = F, aes(x=long, y=lat, group = id, fill = n),
               alpha = 0.6, data = subset(gr_summ, !is.na(n))) +
  scale_fill_gradient2('N Especies',low = "lightblue", mid = 'blue',
                       high= "red", midpoint = mean(summ$n, na.rm = T)) +
  geom_polygon(aes.inherit = F, aes(x=long, y=lat, group = id), fill = NA,
               colour = 'black', data = griddf) +
  theme(#axis.text.x = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 9)) +
  labs(list(title = "Atlas das aves de Carcavelos",
            x = "", y = "")) +
  annotate('text', x = summ$x, y = summ$y, label = summ$n)

# Especies
kDadosSistem <- c('pto', 'trans')
esp <- unique(all_data$especie)
i <- 'Sylv mela'
# Plot ------------------------------------------------------------------------
for(i in esp) { # LOOP 1 - Especie
  sist.i.tmp <- nrow(subset(all_data, especie == i & metodo == 'pto'))
  if (sist.i.tmp!= 0) { # Ha registos Sistematicos, Entao:
    sist.i<- ddply(subset(all_data,
                          especie == i & metodo == 'pto'),
                   .(quad), summarise, n_ind = sum(tnind))
  } else { # Nao ha registos Sistematicos, Entao: 
    sist.i  <- data.frame()
  }
  # Outras Ocorrencias: Suplm
  nsist.tmp.i <- nrow(subset(all_data, especie == i & metodo != 'pto'))
  # Ha registos Suplementares | N. Sist, Entao:
  if (nsist.tmp.i != 0) { 
    nsist.i  <- subset(all_data, especie == i & !metodo != 'pto' & 
                         !quad %in% if (nrow(sist.i) != 0) {
                           subset(sist.i, select = quad)$quad
                         } else NA,
                       select = c(quad, metodo, tnind))
    nsist.i$rank <- rank.nsist$rank[match(nsist.i$cod_tipo, rank.nsist$cod_tipo)] 
    nsist.i <- subset(nsist.i[order(nsist.i$rank), ], !duplicated(quad))
    nsist.i <- subset(nsist.i, select = (-rank))
    nsist.i$cod_tipo[nsist.i$cod_tipo == 'RS1'] <- 'suplementar'
    #subset(nsist.i, !duplicated(quad))
    #<- ddply(nsist.i, .(quad), identity)
  } else {
    # Nao ha registos suplementares e nao kDadosSistems (biblio), Entao:
    nsist.i <- data.frame()
  }
  
  if(nrow(nsist.i) != 0 | nrow(sist.i) != 0) { # Contrucao do mapa com todos os dados
    if(nrow(sist.i) != 0) {sist.i$Origem <- 'Sistem?tica'}
    if(nrow(nsist.i) != 0) {nsist.i$Origem <- nsist.i$cod_tipo}
    # Data.frame
    df.spi <- rbind(sist.i, nsist.i)
    df.spi <- subset(df.spi, !is.na(n_ind))
    df.spi$region <- df.spi$quad
    # Contrucao das Classes de abundancia
    ord.nind <- df.spi$n_ind[order(df.spi$n_ind)]
    nind.interv <- findInterval(df.spi$n_ind[order(df.spi$n_ind)], v, 
                                rightmost.closed = F)
    breaks <- c(ord.nind[which(duplicated(nind.interv) == F)],
                min(v[v > max(df.spi$n_ind)]))
    df.spi$cln <- cut(df.spi$n_ind, breaks = breaks, 
                      lowest = T, right = F)
    
    # Ajustar factor levels
    levels(df.spi$cln) <- c(levels(df.spi$cln),
                            'suplementar', 'bibliografia', 'inquerito')
    if(any(df.spi$cln[df.spi$Origem != 'Sistem?tica']!= 0)) {
      df.spi$cln[df.spi$Origem != 'Sistem?tica']<- nsist.i$cod_tipo
      # LOOP 2 - Estacoes
      ggplot(aes(map_id = region), data = df.spi) +
        geom_map(aes(map_id = region, fill = cln), map = grid500df) +
        coord_equal() +
        expand_limits(x=grid500df$long, y = grid500df$lat) +
        opts.base + # Layout geral do map
        map.compl.lcosta + # Complemento: Linha de costa
        geom_polygon(inherit.aes = F, aes(group = id, x = long, y = lat), 
                     data = subset(grid500df, nsistematica %in% c(0,1)),
                     fill = NA, colour = 'grey80') +
        map.compl.loc + guides(colour = "colorbar") + # Complemento: Localidades
        map.compl.ltext + # Complemento: Labels Localidades
        ggtitle(paste(lista.geral.aves$especie[lista.geral.aves$esp == i],'-',
                      kEstacao[j])) +
        theme() + 
        scale_fill_manual('Legenda',
                          values = f.map.paleta(x), drop = F)
    } else {
      ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100) +
        ggtitle(paste(lista.geral.aves$especie[lista.geral.aves$esp == i],'-',
                      kEstacao[j]))
    } # Fim da contruicao do mapa com todos os dados
    ggsave(file = paste0(pasta.mapas.aves,
                         lista.geral.aves$especie[lista.geral.aves$esp == i],
                         '_', kEstacao[j],'.png'),
           plot = last_plot())
    
  }# LOOP 1 - Especie
  
  
  
