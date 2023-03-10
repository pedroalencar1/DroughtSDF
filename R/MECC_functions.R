
######################## ENTROPY FUNCTIONS


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
marginal_interval <- function(data, d = 0.01){

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
get_1d_constraints <- function(data_column){
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
get_marginal_multipliers <- function(data_series){

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
get_marginal_distribution <- function(data, mult){
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
#' @param guess a vector with 5 values.
#' @param test_solutions Boolean. If `TRUE`, a list of other possible guesses will be tested
#' if the user's provided guess do not converge to a solution.
#'
#' @details for the initial `guess` provided as input, in drought analysis applications the
#' 4 initial values are positive and the last is negative. Other observed (but not proved)
#' relations between final values and that may help with the initial guess are:
#' i1 = i3, i2 = i4, i1 < i2 < abs(i5).
#' When `test_solutions==TRUE`, a list of guesses is built with the user's proposed guess and
#' an additional (fixed) list of possible solutions. We suggest keeping this value equal to TRUE
#' int he first run. If, after all guesses are tested, a solution was not found, please switch
#' it to FALSE and test other possible values individually. The additional guesses tested are:
#'
#' `list_guesses <- list(guess,
#'                      c(1,1,1,1,-1),
#'                      c(5,5,5,5,-5),
#'                      c(10,10,10,10,-10),
#'                      c(20,20,20,20,-20),
#'                      c(1,65,1,65,-130),
#'                      c(2,55,2,55,-110),
#'                      c(3,45,3,45,-90),
#'                      c(4,35,4,35,-70),
#'                      c(5,25,5,25,-50),
#'                      c(1,35,1,35,-80),
#'                      c(2,30,2,30,-70),
#'                      c(3,25,3,25,-60),
#'                      c(4,20,4,20,-50),
#'                      c(5,15,5,15,-40))`
#'
#'
#' @return The function returns a numeric vector with names and six elements, the Lagrangian multipliers *Lambda_0*,
#' *Lambda_1*, and *Lambda_2*, *Gamma_1*, *Gamma_2*, and *Lambda_3*
#'
#' @export
#'
get_copula_multipliers <- function(data, guess = c(1,10,1,10,-100), test_solutions = F){

  list_guesses <- list(guess,
                       c(1,1,1,1,-1),
                       c(5,5,5,5,-5),
                       c(10,10,10,10,-10),
                       c(20,20,20,20,-20),
                       c(1,80,1,80,-160),
                       c(10,50,10,50,-115),
                       c(1,65,1,65,-130),
                       c(2,55,2,55,-110),
                       c(3,45,3,45,-90),
                       c(4,35,4,35,-70),
                       c(5,25,5,25,-50),
                       c(1,35,1,35,-80),
                       c(2,30,2,30,-70),
                       c(3,25,3,25,-60),
                       c(4,20,4,20,-50),
                       c(5,15,5,15,-40))

  #calculate copula constraint based on spearman correlation
  Euv <- cor(x = data[,6], y = data[,7], method = 'spearman')
  Euv <- as.numeric((Euv + 3)/12)

  # define objective function
  fun_obj <- function(a) pracma::integral2(function(x,y)
    exp(-a[1]*(x - 0.5) - a[2]*(x^2 - 0.333333) - a[3]*(y - 0.5) - a[4]*(y^2 - 0.333333) - a[5]*(x*y - Euv)),
    xmin = 0,xmax = 1, ymin = 0, ymax = 1)$Q


  # Check if algorithm should run only user's guess or also the entire list
  if (test_solutions){
    # test_solutions == TRUE!
    #solve integral
    # this routine allows checking multiple initial guesses
    count <- 1
    while(count <= length(list_guesses)){

      guess_i <- list_guesses[[count]]

      # catch possible error
      test_solution <- try(pracma::fminsearch(fun_obj, guess_i, method="Nelder-Mead",
                                              maxiter = 10000)$xmin,
                           silent = T)

      if (class(test_solution) == 'try-error'){
        count <- count+1
        # cat('error \n')
      } else {
        multipliers <- test_solution
        # cat(copula_mult_1)
        break
      }
    }

    if (count > length(list_guesses)){
      cat('No solution possible with suggested guesses. Pleae try again')
      return(NULL)
    }
  } else{
    # test_solutions == FALSE
    multipliers <- pracma::fminsearch(fun_obj, guess, method="Nelder-Mead",
                                      maxiter = 10000)$xmin
  }

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
#' @param type string indicating if the probabilities should be calculated to the complete range of the marginals
#' or to some specific values. Available options: `complete` and `limited`. Default value is `complete`
#' @param values vector required if `type == "limited"`. Default value is `NULL`
#'
#' @return a 4 column data frame with x and y values for mapping, and the values of Copula and Survival Copula distributions.
#'
#' @export
#'
get_copula_distribution <- function(copula_mult, type = 'complete', values = NULL){

  type <- tolower(type)

  copula <- function(x,y) exp(copula_mult[1] -copula_mult[2]*(x) - copula_mult[3]*(x^2) - copula_mult[4]*(y) -
                                copula_mult[5]*(y^2) - copula_mult[6]*x*y)

  Copula <- function(xm,ym) pracma::integral2(fun = copula, xmin = 0,xmax = xm, ymin = 0, ymax = ym)$Q

  # MATLAB handels well xi and yi values equal to 1 and 0, but R doesn't. This new interval fixes it.
  if (type =='complete'){
      xi <- (seq(1,99, length.out = 99)/100)^0.5
  } else if (type =='limited') {
      xi <- values
  } else {
      cat('Invalid TYPE option')
      return(NULL)
  }


  C_values <- crossing(xi,xi) |>
    setNames(c('yi', 'xi'))|>
    select(xi,yi) |>
    mutate(dist = 0, surv = 0)


  for (i in 1:nrow(C_values)){
    tryCatch({ # do to the numerical computation of the integral, some values are not solvable. They are replaced by NA!
      C_values$dist[i] <- Copula(C_values$xi[i], C_values$yi[i])
      C_values$surv[i] <- -1 + C_values$xi[i] + C_values$yi[i] + Copula(1-C_values$xi[i], 1-C_values$yi[i])

    }, error=function(e){})

    if (C_values$dist[i] == 0){C_values$dist[i] <- NA}
    if (C_values$surv[i] == 0){C_values$surv[i] <- NA}

  }

    return(C_values)
}


#' Function to assess and compare copula asymmetry changes
#'
#' @param C_values1,C_values2 matrix with 4 columns `xi, yi, dist, surv` which indicate the values of the marginals (`xi, yi`)
#' and the cumulative probability of the copula distribution and survival copula for the historical (`1`) and analysed (`2`)
#' periods. Preferably those matrix should be the output of function `get_copula_distribution()`
#' @param u a number in the interval (0,0.5], indicating the interest area of assymetry measurement. Default value is `0.2`.
#' note that u has to exist in the columns xi, yi of `C_value`.
#'
#' @return a 2 column data frame with metric names (`k_hist, k_curr, k_diff`) and values. `k_hist` is the metric from Kato et al., calculated
#' for the historical period and `k_curr` for the analysed/current period. `k_diff` if the relative difference ($\frac{k_diff - k_hist}{k_hist}$)
#'
#' @details Based on the paper of Kato et al.(2022 - 10.1007/s00362-022-01297-w).
#' The larger the value of kato's index, the higher the asymmetry. Negative values of
#' `k_diff` indicate that there was a reduction in asymmetry, which can indicate an
#' increase on the frequency of more extreme droughts.
#'
#' @export
#'
kato_comparison_2 <- function(C_values1, C_values2, u = 0.2){

    p = c(u, 1-u)

    c1d <- C_values1$dist[C_values1$xi == p[1] & C_values1$yi == p[1]]
    c1s <- C_values1$surv[C_values1$xi == p[2] & C_values1$yi == p[2]]

    c2d <- C_values2$dist[C_values2$xi == p[1] & C_values2$yi == p[1]]
    c2s <- C_values2$surv[C_values2$xi == p[2] & C_values2$yi == p[2]]

    k_hist <- log(c1s/c1d)
    k_curr <- log(c2s/c2d)
    k_diff <- (k_curr - k_hist)/k_hist

    output <- data.frame('metric' = c('k_hist', 'k_curr', 'k_diff'), 'value' = c(k_hist, k_curr, k_diff))

    return(output)
}

#' Function to assess copula asymmetry
#'
#' @param C_values matrix with 4 columns `xi, yi, dist, surv` which indicate the values of the marginals (`xi, yi`)
#' and the cumulative probability of the copula distribution and survival copula analysed. Preferably this matrix should be
#' the output of function `get_copula_distribution()`
#' @param u a number in the interval (0,0.5], indicating the interest area of asymmetry measurement. Default value is `0.2`.
#' note that u has to exist in the columns xi, yi of `C_value`.
#'
#' @return a single value `k_value`
#'
#' @details Based on the paper of Kato et al.(2022 - 10.1007/s00362-022-01297-w)
#'
#' @export
#'
kato_comparison_1 <- function(C_values1, u = 0.2){

    p = c(u, 1-u)

    c1d <- C_values1$dist[C_values1$xi == p[1] & C_values1$yi == p[1]]
    c1s <- C_values1$surv[C_values1$xi == p[2] & C_values1$yi == p[2]]


    k_value <- log(c1s/c1d)

    return(k_value)

}


#' function to obtain the return period of the copula to particular event features (based on the probability of the marginals)
#'
#' @param data data frame with seven columns containing years, severity and duration, its
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
get_return_periods <- function(data, d, marginal_mult, copula_mult, p_values){

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
get_comparison_tr <- function(data_study, return_periods_reference, d){

  # data_study <- series2
  # d = 0.01
  # return_periods_reference <- return_periods_1

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
  copula_multipliers <- get_copula_multipliers(data_study, test_solutions = T)

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
get_distribution_comparison <- function(data_study, data_reference, d){

  # get marginal multipliers
  marg_mult_1 <- get_marginal_multipliers(data_reference[,4:5])
  marg_mult_2 <- get_marginal_multipliers(data_study[,4:5])

  # get copula multipliers and distribution (reference)
  copula_mult_1 <- get_copula_multipliers(data_reference, test_solutions = T)
  copula_mult_2 <- get_copula_multipliers(data_study, test_solutions = T)

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


#' Pipe-line to get relevant MECC data
#'
#' @param station_id integer. The ID number of the DWD station
#' @param year_limits a vector with two integers, the years of beginning and end of the study period
#' @param year_break an integer, the year in which the series is divided for comparison
#'
#' @return The function returns a list with 6 elements, the copula distributions for each
#' interval, the return period for relevant events in each interval, the comaprison of
#' return periods and its distribution.
#'
#' @export
#'
pipe_line_MECC <- function(station_id, year_limits, year_break){

  tic<-proc.time()["elapsed"] #get elapted time

  cat('\nProcessing...\nThere are 13 steps and the processing can take up to a few minutes.' )

  # 1. read data
  data_station <- get_data_station_MECC(station_id, var_name='kl',
                                        date_bounds = NA)

  # removes many old years that might cause problems with PET calculation due to missing values
  if (min(lubridate::year(data_station$data$Date)) < year_limits[1]-1){
    data_station$data %<>% filter(lubridate::year(data_station$data$Date) >=year_limits[1]-1)
  }

  cat('\n1')

  # 2. calc. PET
  station_etp <- calculate_ETP_daily_MECC(input_type = 'dwd', list_data = data_station,
                                          method = 'hargreavessamani')
  cat('-2')

  # 3. calc SPEI
  station_spei <- quiet(calculate_SPEI_MECC(Date = station_etp$Date, ETP = station_etp$ETP,
                                      Precipitation = data_station$data$RSK, scale = 3,
                                      year_bounds = year_limits, fill_gaps = F))
  cat('-3')


  # 4. get droughts
  station_droghts <- get_drought_series(Date = station_spei$Date, SPEI = station_spei$SPEI,
                                        threshold = 0, year_bounds = year_limits)
  cat('-4')

  # 5. get intervals
  series1 <- station_droghts$drought_max |>
    dplyr::filter(year < year_break) |>
    marginal_interval()

  series2 <- station_droghts$drought_max |>
    dplyr::filter(year >= year_break) |>
    marginal_interval()

  cat('-5')

  # 6. get marginal multipliers
  mult1 <- get_marginal_multipliers(series1[,4:5])
  mult2 <- get_marginal_multipliers(series2[,4:5])
  cat('-6')

  # 7. get ent-based marginals
  series1 <- get_marginal_distribution(series1, mult1)
  series2 <- get_marginal_distribution(series2, mult2)
  cat('-7')

  # 8. get_copula_multipliers
  copula_mult_1 <- get_copula_multipliers(series1, test_solutions = T)
  copula_mult_2 <- get_copula_multipliers(series2, test_solutions = T)
  cat('-8')

  # 9. get_copula_distribution
  copula_dist_1 <- get_copula_distribution(copula_mult_1)
  copula_dist_2 <- get_copula_distribution(copula_mult_2)
  cat('-9')

  # 10. get_return_periods
  return_periods_1 <- get_return_periods(series1, d = 0.1, marginal_mult = mult1,
                                         copula_mult = copula_mult_1,
                                         p_values = c(0.8,0.9,0.95,0.98,.99))
  return_periods_2 <-get_return_periods(series2, d = 0.1, marginal_mult = mult2,
                                        copula_mult = copula_mult_2,
                                        p_values = c(0.8,0.9,0.95,0.98,.99))
  cat('-10')

  # if(exists("series2")) cat('-')

  # 11 .get_comparison_tr
  copula_comparison <- get_comparison_tr(data_study = series2,
                                         return_periods_reference = return_periods_1,
                                         d = 0.1)
  cat('-11')

  # 12. get_distribution_comparison
  dist_comparison <- get_distribution_comparison(data_study = series2,
                                                 data_reference = series1, d = 0.1)
  cat('-12')

  # 13. output
  output <- list(copula_dist_1 = copula_dist_1,
                 copula_dist_2 = copula_dist_2,
                 return_periods_1 = return_periods_1,
                 return_periods_2 = return_periods_2,
                 copula_comparison = copula_comparison,
                 dist_comparison = dist_comparison,
                 series1 = series1,
                 series2 = series2,
                 data_station = data_station)
  cat('-13\n')
  toc<-proc.time()["elapsed"]
  cat('Finished! Elapsed time =',toc-tic,'\n')

  return(output)
}






















