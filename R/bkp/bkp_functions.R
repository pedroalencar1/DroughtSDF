# functions to process MECC for drought analysis


# Function to extract data from file.
# Returns list with two elements (1) data series; (2) data frame with info about station
#' Function to extract data from file.
#'
#' @param station_id DWD station number (can be obtained with `rdwd::findID("StationName")`)
#' @param var_name character with the name of the variable, corresponding to the default names from DWD (e.g. kl, more_precip, solar)
#' @param date_bounds vector with two dates in the format ('yyyy-mm-dd'), indicating beggining and end of study period.
#'
#' @return list with three elements (1) data series; (2) data frame with info about station, (3) vector with `NA` count.
#'
#' @details To avoid errors and NA values in the SPEI and drought computation, preferably keep `date_bounds = NA`
#'
#'@export
#'
get_data_station_MECC2 <- function(station_id, var_name='kl', date_bounds = NA){
    # station_id <- 880
    # var_name <- 'kl'
    # date_bounds <- c("1921-1-1", "2020-12-31")
    # rain_threshold <- 1

    # adding `current = T` it takes longer but makes the function more resilient to updates
    link <- selectDWD(id=station_id, res='daily', var = var_name, per='rh', current = T)

    meta_data <- metaInfo(station_id) #metadata

    # check if variable is available
    if (var_name %in% meta_data$var){
        data <- data.frame()

        if (link[1] == link[2]){
            data <-  readDWD(dataDWD(link[1], read=FALSE), varnames=TRUE, fread = F)
        } else {
            # join data from both recent and historical data sets
            if (grepl('historical', link[2])){
                # without fread it is slower, but more reliable
                data_2 <- readDWD(dataDWD(link[2], read=FALSE), varnames=TRUE, fread = F)
                data <- rbind(data, data_2)
            }
            if (grepl('recent', link[1])){
                # without fread it is slower, but more reliable
                data_1 <- readDWD(dataDWD(link[1], read=FALSE), varnames=TRUE, fread = F)
                data <- rbind(data, data_1)
            }

        }
        # filter data by dates, if bounds are provided
        if (!is.na(date_bounds)){
            data %<>% dplyr::filter(., MESS_DATUM >= date_bounds[1], MESS_DATUM <= date_bounds[2]) %>%
                dplyr::mutate(Date= .$MESS_DATUM) %>% complete(Date= seq(from = as.Date(date_bounds[1]),
                                                                         to = as.Date(date_bounds[2]),
                                                                         by = "day"))
        } else {
            date_bounds[1] <- as.Date(min(data$MESS_DATUM, na.rm = T))
            date_bounds[2] <- as.Date(max(data$MESS_DATUM, na.rm = T))

            date_bounds <- zoo::as.Date(date_bounds)

            data %<>% dplyr::filter(., MESS_DATUM >= date_bounds[1], MESS_DATUM <= date_bounds[2]) %>%
                dplyr::mutate(Date= .$MESS_DATUM) %>% complete(Date= seq(from = as.Date(date_bounds[1]),
                                                                         to = as.Date(date_bounds[2]),
                                                                         by = "day"))
        }

        data <- data[!duplicated(data$Date),] # remove duplicates

        data %<>% setNames(sub("\\..*", "", colnames(.))) %>%#rename columns to more simple name
            filter(!is.na(STATIONS_ID))

        # View(data)

        #Check how many NAs
        na_count <- c(station_id,summarise_all(data,funs(sum(is.na(.)))))|> unlist()

        my_data <- list(data = data,
                        meta = data.frame(name = meta_data$Stationsname[1],
                                          id = station_id,
                                          latitude = meta_data$geoBreite[1],
                                          longitude = meta_data$geoLaenge[1],
                                          height = meta_data$Stationshoehe[1],
                                          county = meta_data$Bundesland[1],
                                          var = var_name),
                        na_count = na_count)

    } else {
        data <- data.frame(val = -999) # error value
        na_count <- c(station_id, rep(-888, 30)) # error value

        my_data <-list(data = data,
                       meta = data.frame(name = meta_data$Stationsname[1],
                                         id = station_id,
                                         latitude = meta_data$geoBreite[1],
                                         longitude = meta_data$geoLaenge[1],
                                         height = meta_data$Stationshoehe[1],
                                         county = meta_data$Bundesland[1],
                                         var = '**VARIABLE NOT AVAILABLE'), # WARNING
                       na_count = na_count)
    }

    return(my_data)
}


#' Function to  calculate ETP
#'
#' @param input_type character string. Either "DWD" (to use the output of function `get_data_station()`) or "MANUAL" to give each variable manually
#' @param list_data if `input_type == "DWD"`, the output of `get_data_station()`, else it is `NA`
#' @param date series of dates (daily) to be of the time series.
#' @param t_max series of maximum daily temperature (Celcius)
#' @param t_min series of minimum daily temperature (Celcius)
#' @param rh series of average daily relative humidity (-)
#' @param rh_max series of maximum daily relative humidity (-)
#' @param rh_min series of minimum daily relative humidity (-)
#' @param n_hours series of daily total number of sunlight hours (hours)
#' @param cloud_cover series of daily degree of cloud coverage (Okta scale)
#' @param mean_wind series of daily average wind speed (m.s-1)
#' @param station_metadata data frame with metadata, preferably the output from `get_data_station()`. Should contain 6 columns (name, id, latitude, longitude, height, county and var; in this exact order and names).
#' @param method carachter string with what equation should be used to estimate Evapotranspiration. Values are *HargreavesSamani* or *PenmanMonteith*.
#'
#' @details
#' All inputs (expect `station_metadata`) are available in the first element of the output of function `get_data_station()`.
#' For clarity, they can be incerted separatly
#'
#' @export
#'
calculate_ETP_daily_MECC2 <- function(input_type = 'dwd', list_data = NA, date = NA, t_max = NA,
                                     t_min = NA, rh = NA, rh_max = NA, rh_min = NA, n_hours = NA,
                                     cloud_cover = NA, mean_wind = NA, station_metadata = NA,
                                     method = 'HargreavesSamani'){
    #####
    ## Uncomment to test function
    # data_id <- get_data_station_MECC(3988, var_name = 'kl',
    #                             date_bounds = c("1900-1-1", "2020-12-31"))
    # data_sol <- get_data_station_MECC(3987, var_name = 'solar',
    #                             date_bounds = c("1900-1-1", "2020-12-31"))
    #
    #
    # data <- cbind(data_id[[1]], FG_STRAHL = data_sol[[1]]$FG_STRAHL)
    # station_metadata <- data_id[[2]]
    #
    # date = data$Date
    # t_max = data$TXK
    # t_min = data$TNK
    # rh = data$UPM
    # n_hours = data$SDK
    # cloud_cover = data$NM
    # mean_wind = data$FM
    # solar_rad = data$FG_STRAHL
    #
    # input_type = 'dwd'
    # list_data = list_data
    #
    # method = 'hargreavessamani'
    #####

    ## 1. Input
    if (tolower(method) == 'hargreavessamani'){
        if (tolower(input_type) == 'dwd'){ #load from output `get_data_station`
            date <- list_data[[1]]$Date
            t_max <- list_data[[1]]$TXK
            t_min <- list_data[[1]]$TNK

            station_metadata <- list_data[[2]]
        } else if(tolower(input_type) == 'manual'){ # load manually each vector
            cat("You selected manual input.\n
        Minimum inputs are the vectors (or columns from dataframe):
          1. date
          2. t_max
          3. tmin")
        } else {
            cat("ERROR: invalid input type!
        Please choose DWD or MANUAL.")
            return(NULL)
        }
    } else if (tolower(method) == 'penmanmonteith') {
        if (tolower(input_type) == 'dwd'){ #load from output `get_data_station`
            date <- list_data[[1]]$Date
            t_max <- list_data[[1]]$TXK
            t_min <- list_data[[1]]$TNK
            rh <- list_data[[1]]$UPM
            n_hours <- list_data[[1]]$SDK
            cloud_cover <- list_data[[1]]$NM
            mean_wind <- list_data[[1]]$FM

            station_metadata <- list_data[[2]]
        } else if(tolower(input_type) == 'manual'){ # load manually each vector
            cat("You selected manual input.\n
        Minimum inputs are the vectors (or columns from dataframe):
          1. date
          2. t_max
          3. tmin
          4. rh (or rh_min and rh_max)
          5. n_hours
          6. cloud_cover
          7. mean_wind
          8. station_metadata")
        } else {
            cat("ERROR: invalid input type!
        Please choose DWD or MANUAL.")
            return(NULL)
        }
    } else {
        cat("ERROR: invalid mehtod!
        Please choose HargreavesSamani or PenmanMonteith.")
        return(NULL)
    }


    ## 2. Calculation

    require(Evapotranspiration)
    data("constants")
    if (tolower(method) == 'penmanmonteith'){

        # if only average daily RH is provided
        if (!is.na(rh[1])){
            rh_min <- rh
            rh_max <- rh
        }

        series <- data.frame(Year = lubridate::year(date), Month = lubridate::month(date),
                             Day = lubridate::day(date),
                             Tmax = t_max, Tmin = t_min, RHmax = rh_max, RHmin = rh_min,
                             n = n_hours, Cd = cloud_cover, uz = mean_wind)

        # load in pre-set constants and variables for the ETP Calculations

        # require(Evapotranspiration)
        # data("constants")
        # load('data/constants.rda')

        constants$lat <- station_metadata$latitude
        constants$lat_rad <- station_metadata$latitude * pi/180
        constants$Elev <- station_metadata$height
        constants$z <- 2 # ?? is it alright?

        var_names <- c('Tmax', 'Tmin', 'RHmax', 'RHmin', 'n', 'Cd', 'uz')

        input <- Evapotranspiration::ReadInputs(varnames = var_names,
                                                climatedata = series, constants = constants,
                                                stopmissing=c(20,20,3), timestep="daily",
                                                message = 'no')

        results1 <- ET.PenmanMonteith(input, constants, ts="daily", solar="sunshine hours",
                                      wind="yes", crop = "short", message="no", AdditionalStats="no", save.csv="no")
        results2 <- ET.PenmanMonteith(input, constants, ts="daily", solar="cloud",
                                      wind="yes", crop = "short", message="no", AdditionalStats="no", save.csv="no")

        etp_series <- data.frame(Date = date, Julian_day = yday(date),
                                 ETP_sunshine = results1$ET.Daily, ETP_cloud = results2$ET.Daily)

    } else  {

        series <- data.frame(Year = lubridate::year(date), Month = lubridate::month(date),
                             Day = lubridate::day(date), Tmax = t_max, Tmin = t_min)

        # data("constants")

        constants$lat <- station_metadata$latitude
        constants$lat_rad <- station_metadata$latitude * pi/180
        constants$Elev <- station_metadata$height
        constants$z <- 2 # ?? is it alright?

        var_names <- c('Tmax', 'Tmin')

        input <- Evapotranspiration::ReadInputs(varnames = var_names,
                                                climatedata = series, constants = constants,
                                                stopmissing=c(20,20,3), timestep="daily",
                                                message = 'no')

        results1 <- ET.HargreavesSamani (input, constants, ts="daily", crop = "short",
                                         message="no", AdditionalStats="no", save.csv="no")

        etp_series <- data.frame(Date = date, Julian_day = yday(date),
                                 ETP = results1$ET.Daily, row.names = NULL)

    }

    ## 3. Export results
    output <- data.frame(etp_series, dplyr::select(series, !c('Year', 'Month', 'Day')), row.names = NULL) |>
        select(Date, Julian_day, Tmax, Tmin, ETP)

    return(output)
}


#' Function to calculate SPEI
#'
#' @param Date series of dates (daily) to be of the time series.
#' @param Precipitation series of daily precipitation (mm)
#' @param ETP series of minimum daily potential evapotranspiration (mm), preferably obtained with `calculate_ETP_daily_MECC()`
#' @param scale integer, the number of months for accumulation computation
#'
#'@export
#'
calculate_SPEI_MECC2 <- function(Date, Precipitation, ETP, scale = 3){

    # uncomment to test function
    # Date = station_etp$Date
    # Precipitation = data_station$data$RSK
    # ETP = station_etp$ETP

    # get monthly accumulation
    series <- tibble::tibble(date = as.POSIXct(Date),
                             prec = Precipitation,
                             etp = ETP) |>
        tibbletime::as_tbl_time(date) |>
        collapse_by('month') |>
        group_by(date) |>
        summarise(prec = sum(.data[["prec"]]),
                  etp = sum(.data[['etp']])) |>
        mutate(deficit = prec - etp) |>
        mutate(spei = as.vector(ts(SPEI::spei(deficit, scale = 3)$fitted)))|>
        stats::setNames(c('Date', 'Precipitation', 'ETP', 'Deficit', 'SPEI'))|>
        as.tibble()

    return(series)

}


#' Function to select drought events
#'
#' @param Date series of dates (monthly) to be of the time series.
#' @param SPEI series of month SPEI (-), preferably obtained with `calculate_SPEI_MECC()`
#' @param threshold scalar, maximum value of SPEI for a period to be classified as droguht
#' @param year_bounds vector with two integer values, the years of beggining and end of analysis
#'
#'@export
#'
get_drought_series2 <- function(Date, SPEI, threshold = 0, year_bounds = c(1900,2019)){

    ## uncomment to test function
    # Date = station_spei$Date
    # SPEI = station_spei$SPEI
    # threshold = 0
    # year_bounds = c(1900,2019)


    series <- data.frame(date = Date, spei = SPEI) |>
        filter(lubridate::year(Date) >= year_bounds[1] & lubridate::year(Date) <= year_bounds[2]) |>
        mutate(drought = (spei <= threshold)*1) |>
        mutate(year = lubridate::year(date), spei_aux = spei*drought,
               drought_aux = with(rle(drought), rep(cumsum(values) * values, lengths)))

    droughts <- series[,c(2:6)] |>
        group_by(drought_aux) |>
        summarise(spei_acc = -sum(.data[["spei_aux"]]),
                  year_acc = max(.data[['year']]),
                  duration = sum(.data[["drought"]])) |>
        dplyr::slice_tail(n = -1) # remove first line filled with zeros by default

    max_drought <- data.frame(year = NULL, sev = NULL, dur = NULL)
    for (d_year in unique(droughts$year_acc)){
        # d_year <- 1928
        droughts_year <- dplyr::filter(droughts, year_acc == d_year) |>
            summarise_all(max) |> dplyr::select(year_acc,spei_acc, duration) |>
            setNames(c('year', 'sev','dur'))
        max_drought <- rbind(max_drought,droughts_year)
    }

    max_drought %<>% tidyr::complete(year = year_bounds[1]:year_bounds[2],
                                     fill = list(sev = 0, dur = 0))

    return(list(drought_series = droughts,
                drought_max = max_drought))

}





######################## ENTROPY FUNCTIONS

#' Function to download MATLAB files from github
#'
#' @details no input is require. The files are saved in the WD.
#'
#' @export
#'
get_matlab_files_MECC2 <- function(){

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/R_Copula.m"
                  , destfile = "R_Copula.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_comparison_tr.m"
                  , destfile = "get_comparison_tr.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_copula.m"
                  , destfile = "get_copula.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_copula_distribution.m"
                  , destfile = "get_copula_distribution.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_copula_multipliers.m"
                  , destfile = "get_copula_multipliers.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_distribution_comparison.m"
                  , destfile = "get_distribution_comparison.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_entropy_marginals.m"
                  , destfile = "get_entropy_marginals.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_return_periods.m"
                  , destfile = "get_return_periods.m")
}

#' function to run MATLAB stript of MECC
#'
#' @param input_data vector with elements in the following order:
#'   d = threshold ratio for transformation (default is 0.01, after Singh and Zhang(2018)
#'   year_separator = indicates the point of breaking between reference na study datasets
#'   export_report = logical - option to export or not a report with informaiton about the copulas
#'   export_copula = logical - option to export or not the cumulative and survival copulas
#'   values_p = a sequence of numbers (until the end of the file) with values in (0,1) of
#'   probabilities of interest to compute the return periods.
#' @param input_series dataframe with 3 columns. Columns are YEARS, X1 (SEVERITY), X2(DURATION)
#'
#' @export
#'
run_matlab_MECC2 <- function(input_data, input_series){

    write.table(input_data, 'input_data.csv', row.names = F, col.names = F)
    write.csv(input_series, 'input_series.csv', row.names = F)


    if (matlabr::have_matlab()){
        matlabr::get_matlab()
        matlabr::run_matlab_script(fname = 'R_Copula.m')
    }

}


#' function to transform marginals into (0,1) interval
#'
#' @param data data frame with three columns containing years, severity and duration. Preferably the output of `get_drought_series()`
#' @param d a small value between zero and one. It is used bind the distribution to a open interval between zero and one, i.e. ]0,1[
#'
#' @details the default value of `d` is 0.01 (Zhang and Singh, 2019)
#'
#' @return a vector with three values, the Lagrangian multipliers *Lambda_0*, *Lambda_1*, and *Lambda_2*
#'
#' @export
#'
marginal_interval2 <- function(data, d = 0.01){

    data_raw <- data %>%
        dplyr::mutate(sev_t = unlist((data[,2] - (1-d)*min(data[,2]))/((1+d)*max(data[,2]) - (1-d)*min(data[,2]))),
                      dur_t = unlist(data[,3] - (1-d)*min(data[,3]))/((1+d)*max(data[,3]) - (1-d)*min(data[,3])))%>%
        stats::setNames(c('year', 'sev', 'dur', 'sev_t', 'dur_t'))

    return(data_raw)
}


#' function to obtain the constrains to compute marginal distribution
#'
#' @param data_column column containing the data of the analysed variable. Preferably the output of `get_drought_series()`
#'
#' @return a vector with 2 values, *c1* and *c2*
#'
#' @export
#'
get_1d_constraints2 <- function(data_column){
    # data_column <- data$sev

    c[1] <- mean(data_column)
    c[2] <- mean(data_column^2)

    return(c)
}


#' function to solve single variable entropy equation (marginal)
#'
#' @param data_series data frame with the columns of the variables to be analysed. Preferably a subset of the output of function `marginal_interval()`
#'
#' @return a data frame with three columns, the Lagrangian multipliers *Lambda_0*, *Lambda_1*, and *Lambda_2*
#'
#' @export
#'
get_marginal_multipliers2 <- function(data_series){

    # data_series <- data[,4:5]

    all_multipliers <- data.frame()

    c <- c(0,0)

    for (i in 1:ncol(data_series)){

        data_column <- pull(data_series, i)

        c[1] <- sum(data_column)/length(data_column)
        c[2] <- sum(data_column^2)/length(data_column)

        # define objective function
        fun_obj <- function(a) pracma::integral(function(x) exp(-a[1]*(x - c[1]) - a[2]*(x^2 - c[2])), 0,1)

        #solve integral
        solution <- pracma::fminsearch(fun_obj, c(-1, 1), method="Nelder-Mead", minimize = T)

        multipliers <- solution$xmin #vector with multipliers

        a0 <- log(pracma::integral(function(x) exp(-multipliers[1]*x - multipliers[2]*x^2),0,1))

        all_multipliers <- rbind(all_multipliers, c(a0, multipliers))
    }

    colnames(all_multipliers) <- c('Lambda_0', 'Lambda_1', 'Lambda_2')
    rownames(all_multipliers) <- c('sev', 'dur')

    return(all_multipliers)
}

#' function to transform marginals into (0,1) interval
#'
#' @param data data frame with five columns containing years, severity and duration, and its transformations. Preferably the output of `marginal_interval()`
#' @param mult data frame with three columns (three multipliers) and two rows (severity and duration). Preferably the output of `get_marginal_multipliers()`
#'
#' @return a vector with three values, the Lagrangian multipliers *Lambda_0*, *Lambda_1*, and *Lambda_2*
#'
#' @export
#'
get_marginal_distribution2 <- function(data, mult){
    ## uncomment to test function
    # data <- series1
    # mult <- mult1

    # set name of new columns
    data$F_sev <- NA
    data$F_dur <- NA

    for (i in 1:nrow(mult)){

        a <-  unlist(mult[i,]) # subset multipliers
        F_var <- function(x) pracma::integral(function(x) exp(-a[1]-a[2]*x - a[3]*x^2),0,x) #cummulative distribution
        data[, i+5] <- as.vector(unlist(lapply(pull(data, i+3), FUN = F_var)))
    }

    return(data)
}


#' function to calculate the copula's Lagrange multipliers
#'
#' @param data data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals. Preferably the output of `get_marginal_distribution()`
#'
#' @return a numeric vector with names and six elements, the Lagrangian multipliers *Lambda_0*,
#' *Lambda_1*, and *Lambda_2*, *Gamma_1*, *Gamma_2*, and *Lambda_3*
#'
#' @export
#'
get_copula_multipliers2 <- function(data){

    #calculate copula constraint based on spearman correlation
    Euv <- cor(x = data[,6], y = data[,7], method = 'spearman')
    Euv <- as.numeric((Euv + 3)/12)

    # define objective function
    fun_obj <- function(a) pracma::integral2(function(x,y)
        exp(-a[1]*(x - 0.5) - a[2]*(x^2 - 0.333333) - a[3]*(y - 0.5) - a[4]*(y^2 - 0.333333) - a[5]*(x*y - Euv)),
        xmin = 0,xmax = 1, ymin = 0, ymax = 1)$Q

    #solve integral
    multipliers <- pracma::fminsearch(fun_obj, c(1,1,1,1,-1), method="Nelder-Mead",
                                      maxiter = 10000)$xmin

    a0 <- pracma::integral2(function(x,y)
        exp(-multipliers[1]*(x) - multipliers[2]*(x^2) - multipliers[3]*(y) -
                multipliers[4]*(y^2) - multipliers[5]*x*y),
        xmin = 0,xmax = 1, ymin = 0, ymax = 1)$Q

    a0 <- log(1/a0)

    copula_multipliers <- c(a0, multipliers) |>
        setNames(c('l0','l1','l2','g1','g2','l3'))

    return(copula_multipliers)
}


#' function to obtain Copula and Survival Copula distributions
#'
#' @param copula_mult vector with the six copula lagrange multipliers (in this orther: *Lambda_0*,
#' *Lambda_1*, and *Lambda_2*, *Gamma_1*, *Gamma_2*, and *Lambda_3*). Preferably the output from `get_copula_multipliers()`
#'
#' @return a 4 column data frame with x and y values for mapping, and the values of Copula and Survival Copula distributions.
#'
#' @export
#'
get_copula_distribution2 <- function(copula_mult){

    copula <- function(x,y) exp(copula_mult[1] -copula_mult[2]*(x) - copula_mult[3]*(x^2) - copula_mult[4]*(y) -
                                    copula_mult[5]*(y^2) - copula_mult[6]*x*y)

    Copula <- function(xm,ym) pracma::integral2(fun = copula, xmin = 0,xmax = xm, ymin = 0, ymax = ym)$Q

    # MATLAB handels well xia nd yi values equal to 1 and 0, but R doesn't. This new interval fixes it.
    xi <- (seq(1,99, length.out = 99)/100)^0.5

    C_values <- crossing(xi,xi) |>
        setNames(c('yi', 'xi'))|>
        select(xi,yi) |>
        mutate(dist = 0, surv = 0)


    for (i in 1:nrow(C_values)){
        tryCatch({ # do to the numerical compuation of the integral, some values are not solvable. They are replaced by NA!
            C_values$dist[i] <- Copula(C_values$xi[i], C_values$yi[i])
            C_values$surv[i] <- -1 + C_values$xi[i] + C_values$yi[i] + Copula(1-C_values$xi[i], 1-C_values$yi[i])

        }, error=function(e){})

        if (C_values$dist[i] == 0){C_values$dist[i] <- NA}
        if (C_values$surv[i] == 0){C_values$surv[i] <- NA}

    }

    return(C_values)
}


# #' function to obtain Copula and Survival Copula distributions
# #'
# #' @param copula_mult vector with the six copula lagrange multipliers (in this orther: *Lambda_0*,
# #' *Lambda_1*, and *Lambda_2*, *Gamma_1*, *Gamma_2*, and *Lambda_3*). Preferably the output from `get_copula_multipliers()`
# #'
# #' @return a 4 column data frame with x and y values for mapping, and the values of Copula and Survival Copula distributions.
# get_distribution_comparison <- function(){}


#' function to obtain the return period of the copula to particular event features (based on the probability of the marginals)
#'
#' @data data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals. Preferably the output of `get_marginal_distribution()`
#' @param d a small value between zero and one. It is used bind the distribution to a open interval between zero and one, i.e. ]0,1[
#' @param marginal_mult data frame with three columns (three multipliers) and two rows (severity and duration).
#' Preferably the output of `get_marginal_multipliers()`
#' @param copula_mult vector with the six copula lagrange multipliers (in this orther: *Lambda_0*,
#' *Lambda_1*, and *Lambda_2*, *Gamma_1*, *Gamma_2*, and *Lambda_3*). Preferably the output from `get_copula_multipliers()`
#' @param p_values vector with values between 0 and 1, they are probabilities of interest. Default = `c(0.8,0.9,0.95,0.98,0.99)`
#'
#'@export
#'
get_return_periods2 <- function(data, d, marginal_mult, copula_mult, p_values){

    # data = series1
    # d = 0.01
    # marginal_mult = mult1
    # copula_mult = copula_mult_1
    # p_values = c(0.8,0.9,0.95,0.98,.99)

    #define marginal distributions
    f_x <- function(x) exp(-marginal_mult[1,1] - marginal_mult[1,2]*x - marginal_mult[1,3]*x^2)
    F_x <- function(xm) pracma::integral(f_x, 0, xm)

    f_y <- function(y) exp(-marginal_mult[2,1] - marginal_mult[2,2]*y - marginal_mult[2,3]*y^2)
    F_y <- function(ym) pracma::integral(f_y, 0, ym)


    # get real values of event features
    limits <- data.frame(sev = c(max(data[,2]), min(data[,2])),
                         dur = c(max(data[,3]), min(data[,3])),
                         row.names = c('max', 'min')) |> t() |>
        as.tibble() |>
        mutate(delta = (1+d)*max - (1-d)*min)


    # get normalized and absolute event values based on probability
    prob_marginals <- data.frame()
    for (i in 1:length(p_values)){

        obj_fun_x <- function(xm) abs(F_x(xm) - p_values[i])
        a_sol_x <- fminbnd(obj_fun_x, a = 0, b = 1)$xmin

        x_raw <- a_sol_x*limits$delta[1] + (1-d)*limits$min[1]

        obj_fun_y <- function(ym) abs(F_y(ym) - p_values[i])
        a_sol_y <- fminbnd(obj_fun_y, a = 0, b = 1)$xmin

        y_raw <- a_sol_y*limits$delta[2] + (1-d)*limits$min[2]

        prob_marginals <- rbind(prob_marginals,
                                c(p_values[i], a_sol_x, a_sol_y, x_raw, y_raw))

    }
    colnames(prob_marginals) <- c('Probability', 'Sev_p', 'Dur_p', 'Sev_abs', 'Dur_abs')

    # define Copula (primitive)

    copula <- function(x,y) exp(copula_mult[1] -copula_mult[2]*(x) - copula_mult[3]*(x^2) - copula_mult[4]*(y) -
                                    copula_mult[5]*(y^2) - copula_mult[6]*x*y)

    Copula <- function(xm,ym) pracma::integral2(fun = copula, xmin = 0,xmax = xm, ymin = 0, ymax = ym)$Q


    p_values_comb <- crossing(p_values,p_values) %>% .[, 1:2] |>
        setNames(c('x_prob', 'y_prob'))

    p_marginals_comb <- crossing(prob_marginals$Sev_abs,prob_marginals$Dur_abs) %>% .[, 1:2] |>
        setNames(c('x_abs', 'y_abs'))

    p_values_comb <- cbind(p_values_comb, p_marginals_comb)
    p_values_comb$TR <- 0

    for (i in 1:nrow(p_values_comb)){
        tryCatch({ # do to the numerical compuation of the integral, some values are not solvable. They are replaced by NA!
            x_i <- p_values_comb$x_prob[i]
            y_i <- p_values_comb$y_prob[i]
            p_values_comb$TR[i] <- (1 - x_i - y_i + Copula(x_i, y_i))^-1

        }, error=function(e){})

        if (p_values_comb$TR[i] == 0){p_values_comb$TR[i] <- NA}
    }

    colnames(p_values_comb) <- c('u = F(x)', 'v = F(y)', 'x', 'y', 'Tr(x,y)')

    return(p_values_comb[order(p_values_comb$`v = F(y)`),])
}

#' function to compare changes in the return period over time
#'
#' @param data_study data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals for the study period of interest. Preferably
#' the output of `get_marginal_distribution()`.
#' @param return_periods_reference data frame with marginal probabilities, absolute values and
#' copula-based return period for the reference period. Preferably the output from get_return_periods
#' @param d a small value between zero and one. It is used bind the distribution to a open interval between zero and one, i.e. ]0,1[
#'
#'@export
#'
get_comparison_tr2 <- function(data_study, return_periods_reference, d){

    data_study <- series2
    d = 0.01
    return_periods_reference <- return_periods_1

    # get marginal distribution of raw data of study area
    marginal_mult = get_marginal_multipliers(data_series = data_study[,4:5])

    #define marginal distributions
    f_x <- function(x) exp(-marginal_mult[1,1] - marginal_mult[1,2]*x - marginal_mult[1,3]*x^2)
    F_x <- function(xm) pracma::integral(f_x, 0, xm)

    f_y <- function(y) exp(-marginal_mult[2,1] - marginal_mult[2,2]*y - marginal_mult[2,3]*y^2)
    F_y <- function(ym) pracma::integral(f_y, 0, ym)

    # limits study
    limits <- data.frame(sev = c(max(data_study[,2]), min(data_study[,2])),
                         dur = c(max(data_study[,3]), min(data_study[,3])),
                         row.names = c('max', 'min')) |> t() |>
        as.tibble() |>
        mutate(delta = (1+d)*max - (1-d)*min)

    #values of interest (raw)
    marginal_values <- data.frame(x_values = return_periods_reference[,3],
                                  y_values = return_periods_reference[,4])

    # values of interest (transformed to between 0 and 1 using scale of study data)
    marginal_values$x_values_transformed <- (marginal_values$x_values - (1-d)*limits$min[1])/limits$delta[1]
    marginal_values$y_values_transformed <- (marginal_values$y_values - (1-d)*limits$min[2])/limits$delta[2]

    for (i in 1:nrow(marginal_values)){
        marginal_values$x_values_transformed_p[i] <- F_x(as.numeric(marginal_values$x_values_transformed[i]))
        marginal_values$y_values_transformed_p[i] <- F_y(as.numeric(marginal_values$y_values_transformed[i]))
    }

    #get copula multipliers *study*
    copula_multipliers <- get_copula_multipliers(data_study)

    # define copula (primitive) *study*
    copula <- function(x,y) exp(copula_multipliers[1] -copula_multipliers[2]*(x) -
                                    copula_multipliers[3]*(x^2) - copula_multipliers[4]*(y) -
                                    copula_multipliers[5]*(y^2) - copula_multipliers[6]*x*y)

    Copula <- function(xm,ym) pracma::integral2(fun = copula, xmin = 0,xmax = xm, ymin = 0, ymax = ym)$Q

    # calculate new probability of reference events

    marginal_values$new_tr <- 0
    for (i in 1:nrow(marginal_values)){
        tryCatch({ # do to the numerical compuation of the integral, some values are not solvable. They are replaced by NA!
            x_i <- F_x(marginal_values$x_values_transformed[i])
            y_i <- F_y(marginal_values$y_values_transformed[i])

            marginal_values$new_tr[i] <- (1 - x_i - y_i + Copula(x_i, y_i))^-1
        }, error=function(e){})

        if (marginal_values$new_tr[i] == 0){marginal_values$new_tr[i] <- NA}
    }

    output <- cbind(return_periods_reference, marginal_values$new_tr) |>
        setNames(c('u = F(x)', 'v = F(y)', 'x', 'y', 'Tr_ref', 'Tr_new'))

    return(output)
}


#' function to compare changes in the returnt period distribution over time
#'
#' @param data_reference data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals for the REFERENCE period of interest. Preferably
#' the output of `get_marginal_distribution()`.
#' @param data_study data frame with seven columns containing years, severity and duration, its
#' transformations and the entropy-based marginals for the STUDY period of interest. Preferably
#' the output of `get_marginal_distribution()`.
#' @param d a small value between zero and one. It is used bind the distribution to a open interval between zero and one, i.e. ]0,1[
#'
#'@export
#'
get_distribution_comparison2 <- function(data_study, data_reference, d){

    # get marginal multipliers
    marg_mult_1 <- get_marginal_multipliers(data_reference[,4:5])
    marg_mult_2 <- get_marginal_multipliers(data_study[,4:5])

    # get copula multipliers and distribution (reference)
    copula_mult_1 <- get_copula_multipliers(data_reference)
    copula_mult_2 <- get_copula_multipliers(data_study)

    copula_dist_1 <- get_copula_distribution(copula_mult_1)

    return_periods_all <- get_return_periods(data = data_reference, d = d, copula_mult = copula_mult_1,
                                             marginal_mult = marg_mult_1, p_values = copula_dist_1$xi[1:99]) |>
        setNames(c('u', 'v', 'x', 'y', 'TR_ref'))

    # return_periods_all$TR_ref[return_periods_all$TR_ref<=0] <- NA #remove numerical errors

    # get max and min of data sets
    # limits_1 <- data.frame(sev = c(max(data_reference[,2]), min(data_reference[,2])),
    #                                dur = c(max(data_reference[,3]), min(data_reference[,3])),
    #                                row.names = c('max', 'min')) |> t() |>
    #   as.tibble() |>
    #   mutate(delta = (1+d)*max - (1-d)*min)

    limits_2 <- data.frame(sev = c(max(data_study[,2]), min(data_study[,2])),
                           dur = c(max(data_study[,3]), min(data_study[,3])),
                           row.names = c('max', 'min')) |> t() |>
        as.tibble() |>
        mutate(delta = (1+d)*max - (1-d)*min)

    return_periods_all$xt_2 <- (return_periods_all$x - limits_2$min[1])/limits_2$delta[1]
    return_periods_all$yt_2 <- (return_periods_all$y - limits_2$min[2])/limits_2$delta[2]

    # define marginals and copula *study*
    F_x_2 = function(xm) integral(function(x) exp(-marg_mult_2[1,1] - marg_mult_2[1,2]*x - marg_mult_2[1,3]*x^2),0,xm);
    F_y_2 = function(ym) integral(function(y) exp(-marg_mult_2[2,1] - marg_mult_2[2,2]*y - marg_mult_2[2,3]*y^2),0,ym);

    Copula_2 = function(xm,ym) integral2(function(x,y) exp(copula_mult_2[1] - copula_mult_2[2]*x -
                                                               copula_mult_2[3]*x^2 - copula_mult_2[4]*y -
                                                               copula_mult_2[5]*y^2 - copula_mult_2[6]*x*y),
                                         0,xm, 0, ym)$Q

    # calculate marginals
    return_periods_all$u_2 <- sapply(return_periods_all$xt_2, F_x_2)
    return_periods_all$v_2 <- sapply(return_periods_all$yt_2, F_y_2)

    # compute new copula distribution
    for (i in 1:nrow(return_periods_all)){
        tryCatch({ # do to the numerical computation of the integral, some values are not solvable. They are replaced by NA!
            x_i <- return_periods_all$u_2[i]
            y_i <- return_periods_all$v_2[i]

            return_periods_all$C_2[i] <- Copula_2(x_i, y_i)
        }, error=function(e){})

        if(return_periods_all$C_2[i] == 0){return_periods_all$C_2[i] <- NA}
    }

    # compute new TR distribution
    for (i in 1:nrow(return_periods_all)){
        tryCatch({ # do to the numerical computation of the integral, some values are not solvable. They are replaced by NA!
            x_i <- return_periods_all$u_2[i]
            y_i <- return_periods_all$v_2[i]
            C_i <- return_periods_all$C_2[i]

            return_periods_all$Tr_2[i] <- 1/(1 - x_i - y_i + C_i)
        }, error=function(e){})

        if(return_periods_all$Tr_2[i] <= 0){return_periods_all$Tr_2[i] <- NA}
    }

    output <- return_periods_all |>
        setNames(c('u = F(x)', 'v = F(y)', 'x', 'y', 'Tr_ref', 'xt_2', 'yt_2', 'u_2', 'v_2', 'C_2', 'Tr_2'))

    return(output)
}


#' function to plot heatmap of changes in return period (Tr) over time for selected evetns
#'
#' @param tr_comparison data frame with columns containing the absolute value of the drought features (x and y),
#' the return period of reference (Tr_ref) and the return period of the study (Tr_new). Preferably the output
#' of function `get_comparison_tr()`.
#'
#'@export
#'
plot_tr_comparison2 <- function(tr_comparison){

    plot <- tr_comparison |> mutate(diff = Tr_ref - Tr_new,
                                    change = diff/Tr_new) |>
        ggplot(aes(x = as.factor(round(x, 1)), y = as.factor(round(y, 1)), fill = change)) +
        geom_tile() +
        scale_fill_gradient(low = "#ffcccc",high = "#ff2222") +
        ggtitle('Changes in the return period of reference events')+
        labs(x = 'Severity',
             y = 'Duration (months)')

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
#'
#'@export
#'
plot_tr_distribution_comparison2 <- function(tr_dist_comparison, data_reference, data_study,
                                            na.rm = F){

    if(na.rm){
        dist_comparison %<>% fill(Tr_ref, Tr_2)
    }

    theme_set(theme_bw())
    theme_update(aspect.ratio=1)

    plot <- ggplot2::ggplot(dist_comparison1, aes(x = x, y = y))+
        geom_contour_filled(aes(z = Tr_ref), breaks = c(1,2,5,10,20,50,100,200),polygon_outline = FALSE)+
        geom_contour(aes(z = Tr_2), breaks = c(2,5,10,20,50,100), colour = 'darkgrey')+
        geom_text_contour(aes(z = Tr_2), skip = 0,
                          breaks = c(2,5,10,20,50),
                          label.placer = label_placer_n(1),
                          colour = 'darkred')+
        ggtitle('MECC cumulative probability function')+
        scale_x_continuous(expand = c(0.0,0.1), limits = c(0,max(series1$sev)))+
        scale_y_continuous(expand = c(0.0,0.1), limits = c(0,max(series1$dur)))+
        labs(x = 'Severity (SPEI3)', y = 'Duration (months)')+
        theme_bw()+
        theme_update(aspect.ratio=1)+
        geom_point(inherit.aes = F, data = series1, aes(x = sev, y = dur), alpha = 0.5)+
        geom_point(inherit.aes = F, data = series2, aes(x = sev, y = dur), alpha = 0.5, col ='red')

    return(plot)
}
