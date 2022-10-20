###############
# Data generation and pre-processing functions


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
get_data_station_MECC <- function(station_id, var_name='kl', date_bounds = NA){
    # station_id <- 880
    # var_name <- 'kl'
    # date_bounds <- c("1921-1-1", "2020-12-31")
    # rain_threshold <- 1

    # adding `current = T` it takes longer but makes the function more resilient to updates
    link <- quiet(rdwd::selectDWD(id=station_id, res='daily', var = var_name, per='rh',
                            current = T, quiet = T)
                  )

    meta_data <- quiet(rdwd::metaInfo(station_id, FALSE) #metadata
                       )

    # check if variable is available
    if (var_name %in% meta_data$var){
        data <- data.frame()

        if (link[1] == link[2]){
            data <-  rdwd::readDWD(rdwd::dataDWD(link[1], read=FALSE, quiet = T),
                                   varnames=TRUE, fread = F, quiet = T)
        } else {
            # join data from both recent and historical data sets
            if (grepl('historical', link[2])){
                # without fread it is slower, but more reliable
                data_2 <- rdwd::readDWD(rdwd::dataDWD(link[2], read=FALSE, quiet = T),
                                        varnames=TRUE,fread = F, quiet = T)
                data <- rbind(data, data_2)
            }
            if (grepl('recent', link[1])){
                # without fread it is slower, but more reliable
                data_1 <- rdwd::readDWD(rdwd::dataDWD(link[1], read=FALSE, quiet = T),
                                        varnames=TRUE,fread = F, quiet = T)
                data <- rbind(data, data_1)
            }

        }
        # filter data by dates, if bounds are provided
        if (is.na(date_bounds)){

            date_bounds[1] <- as.Date(min(data$MESS_DATUM, na.rm = T))
            date_bounds[2] <- as.Date(max(data$MESS_DATUM, na.rm = T))

            date_bounds <- zoo::as.Date(date_bounds)

            data %<>% dplyr::filter(., MESS_DATUM >= date_bounds[1], MESS_DATUM <= date_bounds[2]) %>%
                suppressWarnings()%>%
                dplyr::mutate(Date= .$MESS_DATUM) %>% complete(Date= seq(from = as.Date(date_bounds[1]),
                                                                         to = as.Date(date_bounds[2]),
                                                                         by = "day"))

        } else {

            data %<>% dplyr::filter(., MESS_DATUM >= date_bounds[1], MESS_DATUM <= date_bounds[2]) %>%
                suppressWarnings()%>%
                dplyr::mutate(Date= .$MESS_DATUM) %>% tidyr::complete(Date= seq(from = as.Date(date_bounds[1]),
                                                                                to = as.Date(date_bounds[2]),
                                                                                by = "day"))
        }

        data <- data[!duplicated(data$Date),] # remove duplicates

        data %<>% setNames(sub("\\..*", "", colnames(.))) #%>%#rename columns to more simple name
            # filter(!is.na(STATIONS_ID))

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
#' @param input_type character string. Either "DWD" (to use the output of function `get_data_station_MECC()`) or "MANUAL" to give each variable manually
#' @param list_data if `input_type == "DWD"`, the output of `get_data_station_MECC()`, else it is `NA`
#' @param date series of dates (daily) to be of the time series.
#' @param t_max series of maximum daily temperature (Celcius)
#' @param t_min series of minimum daily temperature (Celcius)
#' @param rh series of average daily relative humidity (-)
#' @param rh_max series of maximum daily relative humidity (-)
#' @param rh_min series of minimum daily relative humidity (-)
#' @param n_hours series of daily total number of sunlight hours (hours)
#' @param cloud_cover series of daily degree of cloud coverage (Okta scale)
#' @param mean_wind series of daily average wind speed (m.s-1)
#' @param station_metadata data frame with metadata, preferably the output from `get_data_station()`.
#' Should contain 6 columns (name, id, latitude, longitude, height, county and var; in this exact order and names).
#' @param method carachter string with what equation should be used to estimate Evapotranspiration.
#' Values are *HargreavesSamani* or *PenmanMonteith*.
#'
#' @details
#' All inputs (expect `station_metadata`) are available in the first element of the output of function `get_data_station()`.
#' For clarity, they can be incerted separatly
#'
#' @export
#'
calculate_ETP_daily_MECC <- function(input_type = 'dwd', list_data = NA, date = NA, t_max = NA,
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
    data('constants')

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
        # load(file='data/constants.rda')

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

        # load(file='data/constants.rda')

        constants$lat <- station_metadata$latitude
        constants$lat_rad <- station_metadata$latitude * pi/180
        constants$Elev <- station_metadata$height
        constants$z <- 2 # ?? is it alright?

        var_names <- c('Tmax', 'Tmin')

        input <- Evapotranspiration::ReadInputs(varnames = var_names,
                                                climatedata = series, constants = constants,
                                                stopmissing=c(20,20,3), timestep="daily",
                                                message = 'no')

        results1 <- ET.HargreavesSamani(input, constants, ts="daily", crop = "short",
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
#' @param year_bounds vector with two integer values, the years of beginning and end of analysis
#' @param fill_gaps Boolean value. Indicates if the SPEI will be calculated based on the raw data (with gaps)
#' or with the gap-filled deficit series.
#'
#' @details Note that the function exports two SPEI columns. One called `SPEI`, containing the calculated
#' values of SPEI and with eventual NA values, due to missing data, and a second called `SPEI_filled`,
#' where missing deficit (P-ETP) values were filled with the average value of deficit for the respective
#' month over the time series.
#'
#'@export
#'
calculate_SPEI_MECC <-  function(Date, Precipitation, ETP, scale = 3,
                                 year_bounds = c(1900,2019), fill_gaps = F){
    # uncomment to test function
    # Date = station_etp$Date
    # Precipitation = data_station$data$RSK
    # ETP = station_etp$ETP
    # year_bounds = c(1900,2019)
    # scale = 3
    # fill_gaps = F

    # get monthly accumulation
    series <- tibble::tibble(date = as.POSIXct(Date),
                             prec = Precipitation,
                             etp = ETP) |>
        tibbletime::as_tbl_time(date) |>
        collapse_by('month') |>
        group_by(date) |>
        summarise(prec = sum(.data[["prec"]]),
                  etp = sum(.data[['etp']])) |>
        mutate(deficit = prec - etp)

    # get mean deficit month to fill gaps
    mean_deficit_month <- series[complete.cases(series),] |>
        mutate(month = lubridate::month(date))|>
        group_by(month) |>
        summarise(prec = mean(.data[["prec"]]),
                  etp = mean(.data[['etp']]),
                  deficit = mean(.data[['deficit']]))

    # get correct month series
    beg <- as.Date(min(series$date))
    end <- as.Date(max(series$date))
    beg_year <- lubridate::year(beg)
    beg_month <- lubridate::month(beg)
    end_year <- lubridate::year(end)
    end_month <- lubridate::month(end)

    n_months <- (end_year - beg_year)*12 + end_month - beg_month + 1
    all_months <- data.frame(date = seq(beg+1, length=n_months, by="1 month") - 1)

    series <- left_join(all_months, series, by = 'date') |>
        mutate(deficit_fill = ifelse(is.na(deficit),
                                     mean_deficit_month$deficit[lubridate::month(date)],
                                     deficit))

    if (fill_gaps){
        spei <- suppressWarnings(f_spei(vtime = series$date, vdeficit = series$deficit_fill,
                                        n = scale))
    } else {
        spei <- suppressWarnings(f_spei(vtime = series$date, vdeficit = series$deficit,
                                        n = scale))
    }

    series <- dplyr::left_join(series, spei, by = 'date') |>
        stats::setNames(c('Date', 'Precipitation', 'ETP', 'Deficit','Deficit_filled', 'SPEI'))|>
        as.tibble()

    n_NA <- series |>
        filter(lubridate::year(Date) %in% seq(year_bounds[1], year_bounds[2]))
    n_NA <- sum(is.na(n_NA$Deficit))

    if (n_NA > 0){
        cat('The series has', n_NA, 'NAs in the period of interest.
        They were filled with the average for the occuring month over the time series.
        Please check validity.
        ')
    }

    return(series)

}


#' Function to select drought events
#'
#' @param Date series of dates (monthly) to be of the time series.
#' @param SPEI series of month SPEI (-), preferably obtained with `calculate_SPEI_MECC()`.
#' @param threshold scalar, maximum value of SPEI for a period to be classified as droguht
#' @param year_bounds vector with two integer values, the years of beggining and end of analysis.
#'
#' @details Note that the function `calculate_SPEI_MECC()` has two SPEI columns. One (`.$SPEI`)
#' with filled
#'
#'@export
#'
get_drought_series <- function(Date, SPEI, threshold = 0, year_bounds = c(1900,2019)){

    ## uncomment to test function
    # Date = station_spei$Date
    # SPEI = station_spei$SPEI
    # threshold = 0
    # year_bounds = c(1900,2019)

    series <- data.frame(date = Date, spei = SPEI) |>
        filter(lubridate::year(Date) >= year_bounds[1] & lubridate::year(Date) <= year_bounds[2]) |>
        mutate(drought = (spei <= threshold)*1)

    # set NA series if missing value occurs in a zero streak
    for (i in 1:nrow(series)){

        if(is.na(series$drought[i])){
            beg_affected <- max(which(series$drought[1:i] != 1)) + 1 # first zero value in the streak
            end_affected <- suppressWarnings(min(min(which(series$drought[(i+1):nrow(series)] != 1))+ i- 1,
                                                 nrow(series))) # last zero value in the streak
            series$drought[beg_affected:end_affected] <- NA
        }
    }

    series %<>% mutate(year = lubridate::year(date), spei_aux = ifelse(is.na(drought), NA,spei*drought),
                       drought_aux_NA = ifelse(is.na(drought), 0, drought),
                       drought_aux = with(rle(drought_aux_NA), rep(cumsum(values) * values, lengths)))

    droughts <- series[,c(2:7)] |>
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



