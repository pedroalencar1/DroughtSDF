
get_metric <- function(param, series){

  series <- sort(series)

  empiric <- seq(from = 1/(length(series) + 1), by = 1/(length(series) + 1),
                 length.out = length(series))

  prob <- data.frame(data = series, emp = empiric)


  prob$exponential <- stats::pexp(prob$data,
                                  rate = param$exponential['a'])

  prob$lognormal <- stats::plnorm(prob$data,
                                  meanlog  = param$lognormal['a'],
                                  sdlog  = param$lognormal['b'])

  prob$gamma <- stats::pgamma(prob$data,
                              shape = param$gamma['b'],
                              scale = param$gamma['a'])

  prob$weibul <- stats::pweibull(prob$data,
                                 shape = param$weibul['a'],
                                 scale = param$weibul['b'])

  prob$loglogist <- actuar::pllogis(prob$data,
                                    shape = param$loglogist['a']*param$loglogist['b'],
                                    scale = param$loglogist['a'])

  prob$gumbel <- EnvStats::pevd(prob$data,
                                location = param$gumbel['b'],
                                scale = 1/param$gumbel['a'])


  metric <- data.frame(distribution =colnames(prob)[3:8], ks_dist = NA,
                       rmse = NA, nse = NA, pbias = NA)

  for (i in 1:6){
    metric$ks_dist[i] <- max(abs(prob$emp - prob[,2+i]))
    metric$rmse[i] <- sqrt(mean((prob$emp - prob[,2+i])^2))
    metric$nse[i] <- 1 - sum((prob$emp - prob[,2+i])^2)/sum((prob$emp - mean(prob$emp))^2)
    metric$pbias[i] <- 100*sum(prob$emp - prob[,2+i])/sum(prob$emp)
  }

  return(metric)
}
