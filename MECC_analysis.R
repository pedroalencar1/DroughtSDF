
if(!require(matlabr)){
  install.packages("matlabr")
  install.packages("pacman")
  library(matlabr)
  library(pacman)
}

pacman::p_load('tidyr', 'dplyr', 'ggplot2', 'copula', 'ggpubr', 'plotly', 'metR',
               'rdwd', 'SPEI', 'tibble', 'tibbletime', 'lubridate', 'magrittr',
               'ggforce', 'tictoc', 'Evapotranspiration')

# to process the equations
pacman::p_load('pracma')


# select station
# id_station <- rdwd::findID("Saekularstation", exactmatch=FALSE)
id_station <- rdwd::findID("Cottbus", exactmatch=FALSE)
id_station

# get data
data_station <- get_data_station_MECC(station_id = 880, var_name='kl',
                                      date_bounds = NA)


# Calculate ETP
station_etp <- calculate_ETP_daily_MECC(input_type = 'dwd', list_data = data_station,
                                        method = 'hargreavessamani')

# Calculate SPEI

station_spei <- calculate_SPEI_MECC(Date = station_etp$Date, ETP = station_etp$ETP,
                                    Precipitation = data_station$data$RSK, scale = 3)


# get drought events and maxima series
station_droghts <- get_drought_series(Date = station_spei$Date, SPEI = station_spei$SPEI,
                                      threshold = 0, year_bounds = c(1900,2019))

write.csv(station_droghts$drought_max, 'input_series_1.csv', row.names = F)


## get_entropy_marginals

# 1. get intervals
series1 <- station_droghts$drought_max |>
  filter(year < 1960) |>
  marginal_interval()

series2 <- station_droghts$drought_max |>
  filter(year >= 1960) |>
  marginal_interval()

# 2. get marginal multipliers
mult1 <- get_marginal_multipliers(series1[,4:5])
mult2 <- get_marginal_multipliers(series2[,4:5])

# 3. get ent-based marginals

series1 <- get_marginal_distribution(series1, mult1)
series2 <- get_marginal_distribution(series2, mult2)

## get_copula_multipliers

copula_mult_1 <- get_copula_multipliers(series1)
copula_mult_2 <- get_copula_multipliers(series2)


# get_copula_distribution

copula_dist_1 <- get_copula_distribution(copula_mult_1)
copula_dist_2 <- get_copula_distribution(copula_mult_2)


# get_return_periods

return_periods_1 <- get_return_periods(series1, d = 0.01, marginal_mult = mult1,
                                       copula_mult = copula_mult_1,
                                       p_values = c(0.8,0.9,0.95,0.98,.99))
return_periods_2 <-get_return_periods(series2, d = 0.01, marginal_mult = mult2,
                                      copula_mult = copula_mult_2,
                                      p_values = c(0.8,0.9,0.95,0.98,.99))

# get_comparison_tr

copula_comparison <- get_comparison_tr(data_study = series2,
                                       return_periods_reference = return_periods_1,
                                       d = 0.01)


copula_comparison |> mutate(diff = Tr_ref - Tr_new,
                            change = diff/Tr_new) |>
  ggplot(aes(x = as.factor(round(x, 1)), y = as.factor(round(y, 1)), fill = change)) +
  geom_tile() +
  scale_fill_gradient(low = "#ffcccc",high = "#ff2222") +
  ggtitle('Changes in the return period of reference events')+
  labs(x = 'Severity',
       y = 'Duration (months)')
ggsave('Potsdam_1.png')



# get_distribution_comparison

dist_comparison <- get_distribution_comparison(data_study = series2,
                                               data_reference = series1, d = 0.01)

# dist_comparison1 <- dist_comparison[complete.cases(dist_comparison),]

dist_comparison1 <- dist_comparison|>
  fill(Tr_ref, Tr_2)


theme_set(theme_bw())
theme_update(aspect.ratio=1)
ggplot2::ggplot(dist_comparison1, aes(x = x, y = y))+
  geom_contour_filled(aes(z = Tr_ref), breaks = c(1,2,5,10,20,50,100,200),polygon_outline = FALSE)+
  # geom_text_contour(aes(z = V5), skip = 0,
  #                   breaks = c(2,5,10,20,50,100),
  #                   label.placer = label_placer_n(1),
  #                   colour = 'darkblue')+
  geom_contour(aes(z = Tr_2), breaks = c(2,5,10,20,50,100), colour = 'darkgrey')+
  geom_text_contour(aes(z = Tr_2), skip = 0,
                    breaks = c(2,5,10,20,50),
                    label.placer = label_placer_n(1),
                    colour = 'darkred')+
  ggtitle('MECC cumulative probability function')+
  scale_x_continuous(expand = c(0.0,0.1), limits = c(0,max(series1$sev)))+
  scale_y_continuous(expand = c(0.0,0.1), limits = c(0,max(series1$dur)))+
  labs(x = 'Severity (SPEI3)', y = 'Duration (months)')+
  # coord_cartesian(expand = FALSE) +
  theme_bw()+
  theme_update(aspect.ratio=1)+
  geom_point(inherit.aes = F, data = series1, aes(x = sev, y = dur), alpha = 0.5)+
  geom_point(inherit.aes = F, data = series2, aes(x = sev, y = dur), alpha = 0.5, col ='red')
ggsave('Potsdam_2.png')



#### process with matlab

input_series<-read.csv('input_series.csv')

input_data <- read.csv('input_data1.csv', header = F)

run_matlab_MECC(input_data = input_data,
                input_series = input_series)


