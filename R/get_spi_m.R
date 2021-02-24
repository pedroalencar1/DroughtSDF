get_spi_m <- function(df, k = 3){
  import::from(magrittr, "%>%")

  spi <- SPEI::spi(df[,2], k, na.rm = T)

  series_spi <- data.frame(time = df[,1], spi = spi$fitted)
  colnames(series_spi) <- c('time', 'spi')
  return(series_spi)
}
