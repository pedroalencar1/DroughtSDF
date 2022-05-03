###############
# Auxiliar functions

#' Calibrates parameters of multiple log-logist functions
#'
#' @description
#' Auxiliar function to calculate SPEI. Calibrates parameters of multiple log-logist functions
#' using Probability Weighted Moments (PWM) according to Singh (1998) "Entropu-based parameter
#' estimation in Hydrology" (Ch. 18).
#'
#' @param var A matrix or data frame with the variable (hydrologic deficit for SPEI). The
#' data has to be organized with months (or desired interval) in rows (e.g. 12 rows) and
#' years in columns.
#' @param n_param Number of parameters of the Log-logist distribution (either 2
#' or 3).
#'
#' @return
#' @export
#'
param_loglogist <- function(var, n_param = 3){
    number_years <- ncol(var)
    param <- 1:number_years
    param <- 1- (param-0.35)/number_years
    param <- matrix(rep(param,nrow(var)), ncol = number_years,
                    nrow = nrow(var), byrow = T)

    w0 <- rowSums(var, na.rm = T)/number_years
    w1 <- rowSums(var*param,na.rm = T)/number_years
    w2 <- rowSums(var*param^2, na.rm = T)/number_years

    b <- (2*w1 - w0)/(6*w1 - w0 - 6*w2)
    a <- (w0 - 2*w1)*b/(gamma(1 + 1/b) * gamma(1 - 1/b))
    c <- w0 - (w0 - 2*w1)*b

    if (n_param == 2){
        parameters <- data.frame(a = a,b = b)
    } else if (n_param == 3){
        parameters <- data.frame(a = a,b = b, c = c)
    } else {
        parameters <- NA
    }
    return(parameters)
}

#' Standard Precipitation Evaporation Index calculation
#'
#' @param vtime a data.frame column or vector with daily time stamps (Date type)
#' @param vdeficit a data.frame column or vector with daily hydrological deficit
#' obtained by the difference of precipitation and potential evapotranspitation (P - ET0)
#' @param n a natural number that indicates the accumulation time (pentad, week, month, etc)
#'
#' @return The function return a list with two elements. One data frame with time stamped pentad values and a matrix with years organized in columns.
#'
#' @description Internal funciton to calculate the SPEI
#'
#' @export
#'
f_spei <- function(vtime, vdeficit, n){

    # vtime <- series$date
    # vdeficit <- series$deficit
    # n = 3

    nyear <- max(lubridate::year(vtime)) - min(lubridate::year(vtime)) + 1

    #get accumulated deficit over n periods
    deficit_acc <- runner::runner(vdeficit, f = function(x) sum(x), k = n)
    deficit_acc[1:(n-1)] <- NA
    deficit_acc[is.na(deficit_acc)] <- -1e7 #necessary to sort

    #get data as matrix with nyear columns.
    #The number of rows is equal to the number of periods in a year (73 pentads, 48-52 weeks, etc)
    deficit_matrix <- as.data.frame(matrix(deficit_acc, ncol = nyear, byrow = F))

    #sort the values to assess parameters
    deficit_matrix_sort <- t(apply(deficit_matrix, 1, sort))
    deficit_matrix_sort[deficit_matrix_sort == -1e7] <- NA

    # View(deficit_matrix)

    #get paramenters of log-logistic distribution
    parameters <- param_loglogist(deficit_matrix_sort, n_param = 3)

    #get probabilities to assess SPEI
    deficit_matrix[deficit_matrix == -1e7] <- NA
    prob <- (1 + (parameters$a/(deficit_matrix - parameters$c))^parameters$b)^-1
    aux <- unlist(prob)
    aux[is.nan(aux)] <- 1e-4
    prob <-matrix(aux, dim(prob))

    # spei_matrix <- as.data.frame(qnorm(prob))
    spei_list <- c(qnorm(prob))
    spei_time <- data.frame(date = vtime,spei = spei_list[1:length(vtime)])

    return(spei_time)
}


#' Suppress function messages and Concatenate and Print (cat)
#'
#' @param ... the functional expression to be evaluated
#' @param messages logical; suppress all messages?
#' @param cat logical; suppress all concatenate and print calls from \code{\link{cat}}?
#'
#' @export
#'
#' @references
#' RDRR <https://rdrr.io/cran/SimDesign/src/R/util.R> (line)
#'
#' @examples
#' myfun <- function(x){
#'    message('This function is rather chatty')
#'    cat("It even prints in different output forms!\n")
#'    message('And even at different....')
#'    cat("...times!\n")
#'    x
#' }
#'
#' out <- myfun(1)
#' out
#'
#' # tell the function to shhhh
#' out <- quiet(myfun(1))
#' out
#'
quiet <- function(..., messages=FALSE, cat=FALSE){
    if(!cat){
        sink(tempfile())
        on.exit(sink())
    }
    out <- if(messages) eval(...) else suppressMessages(eval(...))
    out
}
