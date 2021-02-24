get_multipliers <- function(list_param){

  lambdas <- list(NA)
  #exponential
  lambdas[['exponential']]['L0'] <- -log(list_param[['exponential']]['a'])
  lambdas[['exponential']]['L1'] <- list_param[['exponential']]['a']

  #lognormal
  lambdas[['lognormal']]['L0'] <- log(pi*sqrt(2)) + log(list_param[['lognormal']]['b']) +
    (list_param[['lognormal']]['a']^2)/(2*list_param[['lognormal']]['b']^2)
  lambdas[['lognormal']]['L1'] <- 1 - (list_param[['lognormal']]['a']^2)/(list_param[['lognormal']]['b']^2)
  lambdas[['lognormal']]['L2'] <- 1/(2*list_param[['lognormal']]['b']^2)

  #gamma
  lambdas[['gamma']]['L0'] <- list_param[['gamma']]['b']*log( list_param[['gamma']]['a']) +
    log(gamma(list_param[['gamma']]['b']))
  lambdas[['gamma']]['L1'] <- 1/list_param[['gamma']]['a']
  lambdas[['gamma']]['L2'] <- 1 - list_param[['gamma']]['b']

  #weibul
  lambdas[['weibul']]['L0'] <- list_param[['weibul']]['a']*log( list_param[['weibul']]['b']) -
    log(list_param[['weibul']]['a'])
  lambdas[['weibul']]['L1'] <- 1 - list_param[['weibul']]['a']
  lambdas[['weibul']]['L2'] <- 1/(list_param[['weibul']]['b'])^(list_param[['weibul']]['a'])

  #loglogist
  lambdas[['loglogist']]['L0'] <- log(list_param[['loglogist']]['a']/list_param[['loglogist']]['b']) #eq 17.11
  lambdas[['loglogist']]['L1'] <- 1-list_param[['loglogist']]['b']
  lambdas[['loglogist']]['L2'] <- 2

  #gumbel
  lambdas[['gumbel']]['L0'] <- -log(list_param[['gumbel']]['a']) - list_param[['gumbel']]['a']*list_param[['gumbel']]['b']
  lambdas[['gumbel']]['L1'] <- list_param[['gumbel']]['a']
  lambdas[['gumbel']]['L2'] <- exp(list_param[['gumbel']]['a']*list_param[['gumbel']]['b'])

  lambdas <- lambdas[-1]

  return(lambdas)
}
