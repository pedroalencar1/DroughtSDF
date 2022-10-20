
install.packages('kgc')
library(kgc)
library(ggplot2)
library(magrittr)

climatezones

eu_climatezones <- climatezones %>% dplyr::filter(Lat > 34 & Lat < 72) %>%
    dplyr::filter(Lon > -13 & Lon < 45)

ggplot(data = eu_climatezones, aes(x = Lon, y = Lat, fill = Cls))+
    geom_tile()+
    scale_x_continuous(name = 'Longitude', breaks = seq(-20,40,10))+
    scale_y_continuous(name = 'Latitude', breaks = seq(35,70,10))+
    coord_fixed()+
    theme_bw()
