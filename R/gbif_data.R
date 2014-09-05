#' Retrieve and plot data from GBIF portal http://www.gbif.org/
#' Retrieve GBIG data with occ_search
#' taxonKey: 212 # Aves Class
#' Paulo E. Cardoso 09-2014

#'Packages
kpacks <- c("raster", "reshape2", "ggplot2", "rWBclimate", "rgbif", 'ggmap',
            "dplyr")
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)

#' GBIF data
db <- occ_search(country = "MZ",  taxonKey=212, hasCoordinate = T,
                 fields = c('collectionID', 'collectionCode',
                            'scientificName', 'name', 'year', 'class', 'order', 'family', 
                            'orderKey', 'familyKey', 'decimalLongitude',
                            'decimalLatitude', 'associatedMedia'),
                 limit = 1e+5)$data
dat <- subset(db, select=c(name, year, decimalLongitude, decimalLatitude,
                           collectionCode))
dat <- dat[complete.cases(dat), ]
dat <- as.tbl(dat)

#' Summaries
sp_list <- dat %>%
  group_by(name) %>%
  summarise(n = length(name))
sp_list[with(sp_list, order(n, decreasing = T)), ]

#' Plot data
moz_map <- raster::getData(country = 'MOZ', level = 1)
moz_df <- fortify(moz_map)

ctrymap <- qmap('Mozambique', maptype = "roadmap", source = "google",
              extent = 'panel', zoom = 5, darken = c(0.7, "white"))

ctrymap +
  geom_jitter(data = dat,
              aes(decimalLongitude, decimalLatitude,
                  colour = factor(name)),
              alpha = 0.7, size = 3) +
  theme(legend.position = 'none') +
  scale_y_continuous(limits = c(-28, -8)) +
  scale_x_continuous(limits = c(28, 41))

