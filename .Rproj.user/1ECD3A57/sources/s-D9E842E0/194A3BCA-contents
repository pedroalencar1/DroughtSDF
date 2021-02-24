hist_plot <- function(df, view = T){
  # library(grid)

  melt_df <- data.table::melt(df, id.vars = 'year', value.name = 'value')

  n_bins <- floor(max(diff(range(df$duration)),diff(range(df$severity)))/
                    (2*max(IQR(df$duration),IQR(df$severity))*nrow(df)^(-1/3)))

  ### ploting
  ggplot2::theme_set(theme_minimal())
  plot1 <- ggplot2::ggplot(data = melt_df, aes(x = value, fill = variable))+
    geom_histogram(alpha = 0.5, position = "identity", bins = n_bins)+
    ggtitle(paste('Histograms of drought severity and duration - ',
                  deparse(substitute(df)), sep = ''))


  plot2 <- ggplot2::ggplot(data = df, aes(x = duration))+
    geom_histogram(alpha = 1, position = "identity", bins = n_bins,
                   aes(y = ..density..))+
    geom_density(alpha = 0.5, fill = "lightblue")

  plot3 <- ggplot2::ggplot(data = df, aes(x = severity))+
    geom_histogram(alpha = 1, position = "identity", bins = n_bins,
                   aes(y = ..density..))+
    geom_density(alpha = 0.5, fill = "pink")

  ###plot
  if (view){
    view_aux <-grid::viewport(layout = grid.layout(2, 2))
    grid::pushViewport(view_aux)
    vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
    print(plot1, vp = vplayout(1, 1:2))
    print(plot2, vp = vplayout(2, 1))
    print(plot3, vp = vplayout(2, 2))
  }

  #export
  png(filename=paste(deparse(substitute(df)),'_hist.png', sep = ''), width=600, bg="white")
  par(mar=c(5,11,1,1)+.1)
  grid::pushViewport(view_aux)
  vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
  print(plot1, vp = vplayout(1, 1:2))
  print(plot2, vp = vplayout(2, 1))
  print(plot3, vp = vplayout(2, 2))
  dev.off()

  print('Check file in your work directory')

}
