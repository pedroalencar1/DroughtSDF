###############
# plotting functions


#' function to plot heatmap of changes in return period (Tr) over time for selected evetns
#'
#' @param tr_comparison data frame with columns containing the absolute value of the drought features (x and y),
#' the return period of reference (Tr_ref) and the return period of the study (Tr_new). Preferably the output
#' of function `get_comparison_tr()`.
#'
#' @return The function returns a ggplot element.
#'
#' @export
#'
plot_tr_comparison <- function(tr_comparison,
                               station_metadata,
                               year_bounds = c(1900,2019),
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
                              sep = ""))+
        theme_classic()

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
#' @param station_metadata data frame with metadata, preferably the output from `get_data_station()`.
#' @param year_bounds vector with two integer values, the years of beginning and end of analysis
#' @param year_break integer, the year used to separate the data into reference and study datasets.
#' @param na.rm logical. Default is True. NA values are filled with `tidyr::fill(direction = 'down')`
#'
#' @return The function returns a ggplot element.
#'
#' @details The numeric solution may introduce negative return periods in extreme
#' cases (high duration - low severity; and vise-versa). These negative values
#' introduce noise to the plot and can be removed before plotting
#'
#' @export
#'
plot_tr_distribution_comparison <- function(tr_dist_comparison,
                                            data_reference,
                                            data_study,
                                            station_metadata,
                                            year_bounds = c(1900,2019),
                                            year_break = 1960,
                                            na.rm = T){

    if(na.rm){
        tr_dist_comparison %<>% fill(Tr_ref, Tr_2)
    }

    xmax <- max(tr_dist_comparison$x, na.rm = T)
    ymax <- max(tr_dist_comparison$y, na.rm = T)

    theme_set(theme_bw())
    theme_update(aspect.ratio=1)
    plot <- ggplot2::ggplot(tr_dist_comparison, aes(x = x, y = y))+
        geom_contour_filled(aes(z = Tr_ref), breaks = c(1,2,5,10,20,50,100),polygon_outline = FALSE)+
        geom_contour(aes(z = Tr_2), breaks = c(2,5,10,20,50), colour = 'darkgrey')+
        geom_text_contour(aes(z = Tr_2), skip = 0,
                          breaks = c(2,5,10,20,50),
                          label.placer = label_placer_n(1),
                          colour = 'darkred',
                          stroke = =0.15)+
        geom_text_contour(aes(z = Tr_ref), skip = 0,
                          breaks = c(2,5,10,20,50),
                          label.placer = label_placer_n(1),
                          colour = 'darkblue',
                          stroke = 0.15)+
        scale_x_continuous(expand = c(0.0,0.1), limits = c(0,xmax))+
        scale_y_continuous(expand = c(0.0,0.1), limits = c(0,ymax))+
        labs(x = 'Severity (SPEI3)',
             y = 'Duration (months)',
             title = 'Comparison of Return Period Distributions',
             subtitle = paste('Station ',
                              stringr::str_to_title(trimws(station_metadata$name)),
                              ' - id:',station_metadata$id,
                              '\nReference period (colored curves): ',
                              year_bounds[1], '-', (year_break-1),
                              '\nStudy period (grey curves): ',
                              year_break, '-',year_bounds[2],
                              sep = ""))+
        theme_bw()+
        theme_update(aspect.ratio=1)+
        geom_point(inherit.aes = F, data = series1, aes(x = sev, y = dur), alpha = 0.5)+
        geom_point(inherit.aes = F, data = series2, aes(x = sev, y = dur), alpha = 0.5, col ='red')

    return(plot)
}

#' Function to  plot NA (missing values) og the time series
#'
#' @param list_data list with three elements - output of function `get_data_station_MECC()`
#' @param varname character with the name of the variable, corresponding to the default names from DWD
#'
#' @return The function returns a ggplot element.
#'
#' @export
#'
plot_na <- function(list_data, varname = 'RSK'){

    var_names <- colnames(list_data[[1]])

    if (varname %in% var_names){
        data <- as.data.frame(list_data[[1]])
        metadata <- list_data[[2]]
        var_names <- colnames(list_data[[1]])
        na_count <- list_data[[3]]

        id <- which(varname == var_names)

        min_var <- min(data[,id], na.rm = T)
        data$na <- max(data[,id], na.rm = T)*is.na(data[,id]) - min_var

        theme_set(theme_classic())
        theme_update(axis.text.y.left = element_blank(),
                     axis.ticks.y.left = element_blank())
        plot_na <- ggplot(data,aes(x = data[,1], y = data[,id]-min_var))+
            geom_line(colour = '#246faa')+
            geom_point(colour = '#246faa')+
            geom_bar(data,
                     mapping = aes(x = data[,1], y =na,
                                   fill = as.factor(1*is.na(data[,id]))),
                     alpha = ifelse(is.na(data[,id]), 0.5,0),
                     stat = 'identity',
                     width = 86400,
                     show.legend = F)+
            scale_fill_manual(
                values = c("#ffffff","#d23b46"),
                name = "Missing data"
            ) +
            scale_y_continuous(expand = c(0,0),
                               sec.axis = sec_axis(~.+min_var, name = var_names[id]))+
            labs(x = 'Date', y = " ")+
            ggtitle(paste("Distribution of missing values for ", var_names[id]),
                    subtitle=paste(metadata$name,", No. of missing days:", as.numeric(na_count[id+1]),
                                   ' (', format((100*as.numeric(na_count[id+1])/nrow(data)), digits=4),'% of days)', sep = ""))

        return(plot_na)
    } else {cat("Variable name not valid\n")}
}

