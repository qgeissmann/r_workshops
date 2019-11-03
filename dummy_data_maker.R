rm(list=ls())
set.seed(1)
library( data.table)

N_PROTO <- 30
date <- as.Date("2019-03-01") + 1:N_PROTO * 7
site <- LETTERS[1:6]
trap_id <- 1:4

dt <- data.table(expand.grid(date=date,site=site,trap_id=trap_id),key=c("date","site"))

trend_pest <- cumsum(rnorm(N_PROTO))
trend_pred <- cumsum(rnorm(N_PROTO))
trend_para <- trend_pred + trend_pred


range01 <- function(x){(x-min(x))/(max(x)-min(x))}

trends <- data.table(date=date, 
                     trend_pest=range01(trend_pest), 
                     trend_pred=range01(trend_pred), 
                     trend_para=range01(trend_para),
                     t=1:N_PROTO)



effects <- data.table(a1 = c(10.2, 7.4, 9.2, 15.3, 19.2, 21.9), 
                      b1 = c(0.5, 2.1, 3.9, 6.1,3.1,9.0),
                      site=LETTERS[1:6])


dt <- trends[effects[dt, on='site'], on='date']

dt[, `:=`(
          N_pest = rpois(.N, a1*trend_pest + b1),
          N_pred = rpois(.N, a1*trend_pred + b1),
          N_para =rpois(.N, a1*trend_para + b1)
)]


dt[date < '2019-04-01', `:=`(
  N_pest = 0,
  N_pred = 0,
  N_para = 0
)]

dt <- dt[sample(1:nrow(dt),size = round(nrow(dt) * .8),  replace=F)]

m <- rep(c('conv', 'orga'), each=3)
names(m) <- site
dt[, practice:=m[site]]

dt

dt_final <- dt[,.(date,site,practice, N_pest, N_para, N_pred)]
setkey(dt_final, date,site)
fwrite(dt_final, "2019-ag_practices_effect_on_pest.csv")

