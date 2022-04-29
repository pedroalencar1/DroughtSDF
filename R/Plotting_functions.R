###############
# plotting functions


#' function to plot heatmap of changes in return period (Tr) over time for selected evetns
#'
#' @param tr_comparison data frame with columns containing the absolute value of the drought features (x and y),
#' the return period of reference (Tr_ref) and the return period of the study (Tr_new). Preferably the output
#' of function `get_comparison_tr()`.
#'
#'@export
#'
plot_tr_comparison <- function(tr_comparison, station_metadata, year_bounds = c(1900,2019),
                               year_break = 1960){

    plot <- tr_comparison |> mutate(diff = Tr_ref - Tr_new,
                                    change = diff/Tr_new) |>
        ggplot(aes(x = as.factor(round(x, 1)), y = as.factor(round(y, 1)), fill = change)) +
        geom_tile() +
        scale_fill_gradient(low = "#ffcccc",high = "#ff2222") +
        # ggtitle('Changes in the return period of reference events')+
        labs(x = 'Severity',
             y = 'Duration (months)',
             title = 'Changes in the return period of reference events',
             subtitle = paste('Station ',station_metadata$name, ' - id:',station_metadata$id,
                              '\nReference period: ',year_bounds[1], '-', (year_break-1),
                              '\nStudy period: ',year_break, '-',year_bounds[2],
                              '\nThe ratio was calculated as (Reference-Study)/Study',
                              sep = ""))

    return(plot)
}


#' function to plot the  changes in the distribution of return period period (Tr) for the domain of events in the reference period.
#'
#' @param tr_dist_comparison The output of function `get_distribution_comparison()`. Contains
#' reference event feature values and the two distributions of TR.
#' @param data_reference data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals for the REFERENCE period of interest. Preferably
#' the output of `get_marginal_distribution()`.
#' @param data_study data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals for the STUDY period of interest. Preferably
#' the output of `get_marginal_distribution()`.
#' @param na.rm logical. Default is True. NA values are filled with `tidyr::fill(direction = 'down')`
#' @param station_metadata data frame with metadata, preferably the output from `get_data_station()`.
#' @param year_bounds vector with two integer values, the years of beginning and end of analysis
#' @param year_break integer, the year used to separate the data into reference and study datasets.
#'
#' @export
#'
plot_tr_distribution_comparison <- function(tr_dist_comparison, data_reference, data_study,
                                            na.rm = T, station_metadata, year_bounds = c(1900,2019),
                                            year_break = 1960){

    if(na.rm){
        tr_dist_comparison %<>% fill(Tr_ref, Tr_2)
    }

    theme_set(theme_bw())
    theme_update(aspect.ratio=1)
    plot <- ggplot2::ggplot(tr_dist_comparison, aes(x = x, y = y))+
        geom_contour_filled(aes(z = Tr_ref), breaks = c(1,2,5,10,20,50,100,200),polygon_outline = FALSE)+
        geom_contour(aes(z = Tr_2), breaks = c(2,5,10,20,50,100), colour = 'darkgrey')+
        geom_text_contour(aes(z = Tr_2), skip = 0,
                          breaks = c(2,5,10,20,50),
                          label.placer = label_placer_n(1),
                          colour = 'darkred')+
        # ggtitle('MECC cumulative probability function')+
        scale_x_continuous(expand = c(0.0,0.1), limits = c(0,max(series1$sev)))+
        scale_y_continuous(expand = c(0.0,0.1), limits = c(0,max(series1$dur)))+
        labs(x = 'Severity (SPEI3)', y = 'Duration (months)',
             title = 'Comparison of Return Period Distributions',
             subtitle = paste('Station ',station_metadata$name, ' - id:',station_metadata$id,
                              '\nReference period (colored curves): ', year_bounds[1], '-', (year_break-1),
                              '\nStudy period (grey curves): ',year_break, '-',year_bounds[2],
                              sep = ""))+
        theme_bw()+
        theme_update(aspect.ratio=1)+
        geom_point(inherit.aes = F, data = series1, aes(x = sev, y = dur), alpha = 0.5)+
        geom_point(inherit.aes = F, data = series2, aes(x = sev, y = dur), alpha = 0.5, col ='red')

    return(plot)
}
