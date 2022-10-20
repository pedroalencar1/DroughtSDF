#analysis for the Bengue Catchment using calibrated distribution for Aiuaba

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(tictoc)

dat <- read.delim('rain_daily.dat', sep = ',') |>
  dplyr::select(1:17, 19:32, 18) |>
  setNames(c('date', 'day_seq', paste('X', 1:30, sep = '')))

dates <- data.frame(date_i = dat$date) %>%
  dplyr::mutate(date_c = as.character(date_i),
                lengths = nchar(date_c),
                day = ifelse(lengths == 7, as.integer(substr(date_c,1,1)), as.integer(substr(date_c,1,2))),
                month = ifelse(lengths == 7, as.integer(substr(date_c,2,3)), as.integer(substr(date_c,3,4))),
                year = ifelse(lengths == 7, as.integer(substr(date_c,4,7)), as.integer(substr(date_c,5,8))),
                date = as.Date(paste(year, month, day, sep = '-'))
                ) 
dat$date <- dates$date

list_durations <- vector('list', length = 30)
names(list_durations) <- paste('X', 1:30, sep = '')
par <- c(0.004,3.306,0.488)

### very slow and very exact method 
### total processing time: 29 hours
# for (i in 1:30){
#   # i = 1
#   df <- dplyr::select(dat, c('date', paste('X',i, sep = '')))
#   
#   for (j in 1:50){
#     # j = 1
#     probs <- runif(n = nrow(df))
#     durs <- vapply(probs, FUN = x_from_prob_g3, FUN.VALUE = 1.0, par = par)
#     durs <- durs*df[,2]
#     df <- cbind(df, durs) 
#     names(df)[names(df) == 'durs'] <- paste('run',j,sep = '')
#   }
#   
#   list_durations[[i]] <- df
# }

### faster slow and less exact method 
### total processing time: 3.4 min
par <- c(0.004,3.306,0.488)
fun <- fun_acc_g3()

ProbReference <- data.frame(x_values = sort(abs(sqrt(seq(0,25, length.out = 1001)) - 5)))
ProbReference$probability <- sapply(ProbReference$x_values, fun) 

list_durations <- vector('list', length = 30)
names(list_durations) <- paste('X', 1:30, sep = '')
# tic()
for (i in 1:30){
  # i = 1
  df <- dplyr::select(dat, c('date', paste('X',i, sep = '')))
  
  for (j in 1:50){
    # j = 1
    probs <- runif(n = nrow(df))
    durs <- vapply(probs, FUN = find_x_from_prob, FUN.VALUE = 1.0, data = ProbReference)
    durs <- durs*df[,2] #%>%
      # as.data.frame(.) %>%
      # mutate_if(is.numeric, ~24*(. > 24))
    durs[durs > 24] <- 24

    df <- cbind(df, durs)
    names(df)[names(df) == 'durs'] <- paste('run',j,sep = '')
  }

  list_durations[[i]] <- df
}
# toc()

#export
yaml::write_yaml(list_durations, "list_durations.yaml")

for (i in 1:30){
  write.table(list_durations[[i]], file = paste('durations_X',i,'.txt',sep = ''),
              sep = ";", row.names = F, quote = F)
}


