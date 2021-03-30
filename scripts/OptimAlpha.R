
library(tidyquant)
library(readxl)
library(lubridate)

library(tidyverse)
library(reshape2)

install.packages('imputeTS')
library(imputeTS)

install.packages(scales)
library(scales)

# ---- equity data ----x
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)



# ---- initial testbed asset list ----x
# Satrix EFTSs
# USD/ZAR
# GBP/ZAR
# SA Repo Rate
# Property: REITS
# offshore investments (wtf)
# Bitcoin (maybe)


# ----- Capitec ------x
# Interest:
#   Current Account:         2.25% pa
#   Savings Account (fixed): 4.08% - 7.71% pa
#   fees:                    R5 * 12 = R60 pa   






from <- '2000-01-01'
to   <- '2021-01-25'
to   <- Sys.Date()






# --------------------------- Generated Data --------------------------- x

satrix_yield <- function(princ=100, prime=0.07, from='2000-01-01', to="2021-01-25") {
  # ----- Satrix (Unused funds) -----x
  # https://resources.easyequities.co.za/EasyEquities_CostProfile.pdf?_ga=2.170339296.1542280750.1612357726-1531892195.1608836285
  
  #   Bracket                   Earn               Fees
  #   R0 - R100'000             Prime -3.50%       (1.75%)
  #   R100'000 - R1'000'000     Prime -3.50%       (1.50%)
  #   R1'000'000 +              Prime -3.50%       (1.27%)
  
  # Prime = 7%
  
  if (princ   <= 100000)                      {fee  <- 0.0175; earn <- prime - 0.035}
  if ((100000  < princ) | (princ <= 1000000)) {fee  <- 0.0150; earn <- prime - 0.035}
  if ((1000000 < princ) | (princ <= 100000))  {fee  <- 0.0127; earn <- prime - 0.035}
  
  res <- c()
  res$A <- princ*(1+earn - fee)^n
  
  A <- princ
  dates <- seq(ymd(to)-years(n), ymd(to), by='year')
  
  dates <- seq(as.Date(from), as.Date(to), by='month')
  n <- length(dates)
  data <- c(as.character(dates[1]), A)
  for (i in 1:n){
    A <- A*(1 + earn)
    data <- rbind(data, c(as.character(dates[i+1]), A))
    
  }
  
  data        <- as.data.frame(data)
  names(data) <- c('date', 'price')
  data$date   <- as.Date(data$date)
  data$price  <- as.numeric(data$price)
  data$symbol <- 'STX free funds'
  res$data    <- data
  
  
  return(res)
}


compound_int <- function(P=100, net_an=0, i=0, n=1, cp=1, to="2021-01-25") {
  "
  Return: 
    FV (FUTURE VALUE) ROI
      - given a principal figure
      - after annual installments/costs
      - accounting for variable compounding periods
            
  Arguments:
    P:      principal amount
    net_an: net annuity (deposit - withdrawal/cost)
    i:      interest rate
    n:      periods
    cp:     compounding rate per period
  "
  
  res <- c()
  res$Principal <- P
  res$net_an   <- net_an
  res$i         <- i
  res$n         <- n
  res$cp        <- cp
  
  data <- c()
  dates <- seq(ymd(to)-years(n), ymd(to), by='year')
  
  
  A <- P
  As <- 0
  for (per in 1:n*cp){
    if (per == 2) As <- net_an
    A <- A*(1 + i/cp)
    As <- net_an + As*(1 + i/cp)
    data <- rbind(data, c(as.character(dates[per]), A, As, A+As))
    
    
  }
  
  res$A <- A
  res$As <- As
  print(data)
  data        <- as.data.frame(data)
  print(data)
  names(data)     <- c('date', 'principal', 'annuity', 'total')
  data$date       <- as.Date(data$date)
  data$principal  <- as.numeric(data$principal)
  data$annuity    <- as.numeric(data$annuity)
  data$total      <- as.numeric(data$total)
  res$data        <- data
  
  res$principal <- P*(1 + i/cp)^(n*cp)
  res$annuity   <- net_an*(((1+(i/cp))^(n*cp)-1)/i)
  res$total     <- res$principal + res$annuity
  return(res)
}





# plot(stx_free_data$date, stx_free_data$price, ylim=c(0,max(stx_free_data$price)), type='l')




stx_free_funds <- satrix_yield(princ=100000)
stx_free_data  <- stx_free_funds$data



# yield on capitec current account
capitec <- compound_int(P=10000, i=.025, net_an=-5, n=10, to=to)
capitec <- capitec$data
capitec$price <- capitec$total
capitec$symbol <- 'capitec'
plot(capitec$date, capitec$total, 'l', col='Blue', lwd=1.5, ylim=c(0,max(capitec$principal)))
lines(capitec$date, capitec$principal, col='green', lwd=1.5)
lines(capitec$date, capitec$annuity, col='red', lwd=1.5)

# yield on capitec savings account
# compound_int(P=13423, i=.0425, net_an=-5, n=10, to=to)
# compound_int(P=13423, i=0.07, net_an=-5, n=10, to=to)






# ---------- get dat ------------x
STX40.JO     <- tq_get('STX40.JO', get = "stock.prices", from=from, to=to)
usd_zar      <- tq_get('USDZAR=X', from=from, to=to)
gbp_zar      <- tq_get('GBPZAR=X', from=from, to=to)
btc_usd      <- tq_get('BTC-USD', from=from, to=to)
Tbills       <- tq_get('INTGSTZAM193N', from=from, to=to, get = "economic.data")
FFB.JO       <- tq_get('FFB.JO', from=from, to=to) 
FFA.JO       <- tq_get('FFA.JO', from=from, to=to)
inflation_SA <- tq_get('FPCPITOTLZGZAF', from=from, to=to, get = "economic.data")
inflation_US <- tq_get('FPCPITOTLZGUSA', from=from, to=to, get = "economic.data")


print(paste0('Average Inflation: ', round(mean(inflation_SA$price), 2), '% pa'))


add_price_col <- function(yahoo_fin_df) {
  yahoo_fin_df$price <- yahoo_fin_df$close
  yahoo_fin_df
  }
STX40.JO     <- add_price_col(STX40.JO)
usd_zar      <- add_price_col(usd_zar) 
gbp_zar      <- add_price_col(gbp_zar) 
btc_usd      <- add_price_col(btc_usd) 
FFB.JO       <- add_price_col(FFB.JO) 
FFA.JO       <- add_price_col(FFA.JO) 


# ---- normalize ----x
normalize <- function(xvar) (xvar - mean(xvar, na.rm=T))/sd(xvar, na.rm=T)
adj_normalize <- function(xvar) (xvar - xvar[1])/sd(xvar, na.rm=T)


# ---- datasets ----x
x <- list(STX40.JO, 
          usd_zar,
          gbp_zar, 
          # btc_usd,
          # FFB.JO, 
          FFA.JO, 
          # Tbills, 
          # inflation_SA,
          # # inflation_US, 
          stx_free_data
          # capitec
          )


adj_normalize(x[[1]]$price)

normalize <- adj_normalize
m <- 0
mn <- 0
for (xx in x){
  print(paste('min:', min(normalize(xx$price), na.rm=TRUE)))
  print(paste('max:', max(normalize(xx$price), na.rm=TRUE)))
  print('-------------------------------')
  mx <- max(normalize(xx$price), na.rm=TRUE)
  mt <- min(normalize(xx$price), na.rm=TRUE)
  if (mx > m) m <- mx
  if (mt < mn) mn <- mt
}

grandplot <- function(x, mt, mm) {
  # ---- plot ----x
  cols <- sample(colors(), length(x))
  plot(x[[1]]$date, normalize(x[[1]]$price), type='line', frame=F, main='Portfolio', 
       font.main=1, col=cols[1], ylab='Price', xlab='date', ylim=c(mn,m), yaxt = "n")
  
  
  for (l in 2:length(x)) {
    lines(x[[l]]$date, normalize(x[[l]]$price), col=cols[l])
    
  }
  

  legend('topleft', legend=unlist(lapply(x, FUN = function(i) i$symbol[1])),
         col=cols, lty=1, border.lwd=0)
}
grandplot(x, mt, mm)








# Annuities
# https://www.investopedia.com/retirement/calculating-present-and-future-value-of-annuities/





# ---------------------------------------------- Compute Return on Portfolio -------------------------------------------------x
 

# ------- For each asset:
# ---------------------- Compute weighted purchase price


# EXAMPLE: STX40
# Invest:
#   Pricipal:                10'000
#   Annuity (end of period): 1'000
#   N:                       10 year
#   

from; to






# ============================================ UPDATED APPROACH ==============================================x
# ============================================ UPDATED APPROACH ==============================================x
# ============================================ UPDATED APPROACH ==============================================x


compute_asset_return <- function(
  data=STX40.JO,
  annuity=c(1000, rep(100, length(seq(as.Date(from), as.Date(to), by='month'))-1)),
  annuity_schedule=seq(as.Date(from), as.Date(to), by='month'),
  print=TRUE
) {
  
  # ---- time sequence ----x 
  annuity_date <- annuity_schedule
  
  # ---- impute data ----x
  data <- na_interpolation(data)
  
  # ---- dates ----x
  date_index    <- unlist(lapply(annuity_date, FUN = function(x) {which.min(abs(data$date - x))}))
  purchase_date <- data$date[date_index]
  current_date  <- data$date[which.min(abs(data$date-as.Date(to)))]
  days          <- cur_date - min_date
  
  
  # ---- invest ----x
  value     <- annuity
  
  # ---- price data ----x
  price <- unlist(lapply(purchase_date, FUN=function(x) data[data$date == x, 'price']))
  
  # ---- compute quantity purchased ----x
  quantity <- value/price
  
  # ---- cummulative values ----x
  sum_quantity    <- cumsum(quantity)
  portfolio_value <- sum_quantity * price
  total_invested  <- cumsum(value)
  
  df <- data.frame(purchase_date,
                   current_date, 
                   days, 
                   price,
                   quantity,
                   sum_quantity,
                   value,
                   total_invested,
                   sum_quantity,
                   portfolio_value)
  
  

  

  # ----- summaries -----x
  summary_stats <- df[nrow(df),]
  summary_stats$ROI             <- summary_stats$portfolio_value/summary_stats$total_invested
  summary_stats$interest_earned <- summary_stats$portfolio_value - summary_stats$total_invested
  
  if (print) {
    print(paste0('Final Return: ', round(summary_stats$ROI,4)*100, '%'))
    print(paste0('Total Annuity: R', summary_stats$total_invested))
    print(paste0('Total Portfolio Value: R', round(summary_stats$portfolio_value,2)))
    print(paste0('Total Interest: R', round(summary_stats$interest_earned,2)))
    
  }
  
  return(list(summary_stats=summary_stats, df=df))
}




plot_portfolio <- function(asset_return, asset_name) {
  df <- asset_return$df
  plot(df$purchase_date, df$portfolio_value, 'line',
       col='black', lty=1, lwd=2,frame=F, main=paste0('Portfolio Return: ', asset_name), 
       font.main=1, ylab='$$$', xlab='date')
  polygon( 
    x=c(min(df$purchase_date), df$purchase_date, max(df$purchase_date)),
    y=c(0, df$portfolio_value, 0), 
    col=alpha('steelblue', alpha = 0.9),
    border=FALSE
  )
  lines(df$purchase_date, df$total_invested, col='darkred', lty=1, lwd=2)
  polygon( 
    x=c(min(df$purchase_date), df$purchase_date, max(df$purchase_date)),
    y=c(0, df$total_invested, 0), 
    col=alpha('red', alpha = 0.4),
    border=FALSE
  )
  legend('topleft', box.lwd = 0, lty=1,
         legend = c('Annuity', 'Total Value'),
         col = c('darkred', 'darkblue'))
}








asset_return <- compute_asset_return()
gbz          <- compute_asset_return(data=gbp_zar)


asset_return$summary_stats
gbz$summary_stats


plot_portfolio(asset_return, 'STX40')
plot_portfolio(gbz, asset_name='GBP-ZAR')






portfolio <- list(gbz, asset_return)

#####




aggregate_data <- function(portfolio) {
  
  # ----- portfolio value -----x
  dt <- portfolio[[1]]$df[,c('purchase_date', 'portfolio_value')]
  for (p in 2:length(portfolio)){
    d1 <- portfolio[[p]]$df[,c('purchase_date', 'portfolio_value')]
    dt <- merge(dt, d1, by='purchase_date', all=TRUE)
  }
  dt[,-1]  <-  na_interpolation(dt[,-1])
  dt$total <- rowSums(dt[,-1])
  dt       <- dt[,c('purchase_date', 'total')]
  
  
  # ----- total invested -----x
  dt2 <- portfolio[[1]]$df[,c('purchase_date', 'total_invested')]
  for (p in 2:length(portfolio)){
    d1  <- portfolio[[p]]$df[,c('purchase_date', 'total_invested')]
    dt2 <- merge(dt2, d1, by='purchase_date', all=TRUE)
  }
  
  # dt2[,-1][is.na(dt2[,-1])] <- 0
  dt2[,-1]  <-  na_interpolation(dt2[,-1])
  dt2$agg_invested          <- rowSums(dt2[,-1])
  dt2                       <- dt2[,c('purchase_date', 'agg_invested')]
  
  
  
  data <- merge(dt, dt2, by='purchase_date', all=TRUE)
  
}



stx40 <- compute_asset_return(data = STX40.JO)
gbp   <- compute_asset_return(data = gbp_zar)

sd_zr <- compute_asset_return(data = usd_zar)
btc   <- compute_asset_return(data = btc_usd)

ffb <- compute_asset_return(data = FFB.JO)
ffa   <- compute_asset_return(data = FFA.JO)
tbill <- compute_asset_return(data = Tbills)
inf_sa   <- compute_asset_return(data = inflation_SA)
stx_free <- compute_asset_return(data = stx_free_data)
inf_us   <- compute_asset_return(data = inflation_US)

cpt   <- compute_asset_return(data = capitec)






portfolio <- list(stx40, gbp, sd_zr, btc, ffb, ffa, tbill, inf_sa, stx_free, inf_us, cpt)
names <- c('stx40', 'gbp', 'sd_zr', 'btc', 'ffb', 'ffa', 'tbill', 'inf_sa', 'stx_free', 'inf_us', 'cpt')
for (p in 1:length(portfolio)) {
  plot_portfolio(portfolio[[p]], names[p])
}

data <- aggregate_data(portfolio)


plot_agg_data <- function(data, c1='#83aff4', c2='#008080') {
  plot(data$purchase_date, data$total, 'line',
       col='black', lty=1, lwd=2,frame=F, main=paste0('Total Portfolio'), 
       font.main=1, ylab='value', xlab='date')
  polygon( 
    x=c(min(data$purchase_date), data$purchase_date, max(data$purchase_date)),
    y=c(0, data$total, 0), 
    col=alpha(c1, alpha = 0.9),
    border=FALSE
  )
  lines(data$purchase_date, data$agg_invested, col=c2, lty=1, lwd=2)
  polygon( 
    x=c(min(data$purchase_date), data$purchase_date, max(data$purchase_date)),
    y=c(0, data$agg_invested, 0), 
    col=alpha(c2, alpha = 0.4),
    border=FALSE
  )
  legend('topleft',border.lwd = 0, lty=1, lwd=1,
         legend = c('Annuity', 'Total Value'),
         col = c(c2, c1))
}


plot_agg_data(data)



# ----- correlation analysis -----x



portfolio_correlation <- function(portfolio) {
  # ----- portfolio value -----x
  dt <- portfolio[[1]]$df[,c('purchase_date', 'price')]
  for (p in 2:length(portfolio)){
    d1 <- portfolio[[p]]$df[,c('purchase_date', 'price')]
    dt <- merge(dt, d1, by='purchase_date', all=TRUE)
  }
  dt[,-1]  <-  na_interpolation(dt[,-1])
  return(cor(dt[,-1]))
  
}

portfolio_correlation(portfolio)

corrplot(portfolio_correlation(portfolio))
?corrplot

# ============================================ UPDATED APPROACH ==============================================x
# ============================================ UPDATED APPROACH ==============================================x
# ============================================ UPDATED APPROACH ==============================================x






# --------- combining portfolios

 
merge(gbz$df[1:10,], asset_return$df[1:10,], on='purchase_date')
names(asset_return$df)




# --------- combining portfolios ----------x


gbz$df[1:10,'purchase_date'] + asset_return$df[1:10,]$portfolio_value


# outer merge



# TREAT AS SINGLE ASSET WITH MANY DIFFERENT PRICES ! (great price variation)


cols <- c('purchase_date', 'current_date', 'days', 'principal', 'price')
d1   <- gbz$df[, cols]
d2   <- asset_return$df[, cols]
df <- merge(x=d1, d2, by = cols, all = TRUE)
df <- df[order(df$purchase_date), ]






# ---------- RECURSIVELY UPDATE: weighted price  -------------x
x_bar        <- c(df$price[1])
pur_quantity <- cumsum(df$principal)  # Raw purchase quantity
for (i in 2:nrow(df)) {
  x_bar <- c(x_bar,
             x_bar[i-1] * (pur_quantity[i-1]/pur_quantity[i]) +
               df$price[i]   * (pur_quantity[i] - pur_quantity[i-1])/pur_quantity[i]
  )
}


# wieghted purchase price at each time!
df$ave_price <- x_bar

# ---- interest earned ----x
df$ROI_on_ave_price <- df$price/df$ave_price

#
df$cummulative_invested <- pur_quantity

# NBNBNB! Portofolio value
df$portfolio_value <- df$ROI_on_ave_price * df$cummulative_invested


# ---- compute interest ----x

df$interest_earned <- df$portfolio_value - df$cummulative_invested
summary_stats <- df[nrow(df),]


if (print) {
  print(paste0('Final Return: ', round(summary_stats$ROI,4)*100, '%'))
  print(paste0('Total Annuity: R', summary_stats$cummulative_invested))
  print(paste0('Total Portfolio: R', round(summary_stats$portfolio_value,2)))
  print(paste0('Total Interest: R', round(summary_stats$interest_earned,2)))

}

rt <- (list(summary_stats=summary_stats, df=df))


plot_portfolio(rt, asset_name='GBP-ZAR')























  

# ----- create 1 dataframe -----x
x[[1]][,c('date', 'symbol', 'price')]

data <- c()
for (xx in x){
  data <- merge(data, data.frame(xx), on='date')
}



merge(x[[1]][,c('date', 'symbol', 'price')], 
      x[[1]][,c('date', 'symbol', 'price')],
      c('date', 'symbol'))[1:10,]


library(tidyverse)

tidyverse::

# ---- ETF name ----x
# https://www.jse.co.za/trade/equities-market/equities/exchange-traded-products/exchange-traded-funds
eft_list <- read_excel('~/Desktop/OptimAlpha/data/jse EFT list.xlsx')
eft_list$Alpha























# ---- tickers: ALL AVAILABLE ON YAHOO FINANCE ----x
tickers        <- read_excel('~/Desktop/OptimAlpha/data/Yahoo Ticker Symbols - September 2017.xlsx')
names(tickers) <- tickers[3,]
tickers        <- tickers[4:nrow(tickers),]

# ---- data structure for Dash ----x
asset_list     <- apply(tickers, 1, FUN = function(x) {list(label=x[[1]], value=x[[2]])})


# ---- key assets ----x
index <- which(unlist(lapply(tickers$Ticker,  function(x){grepl('STX', x, fixed = F)})))
tickers[index, ]

ind <- which(tickers$Country == 'South')
index <- which(unlist(lapply(tickers$Country,  function(x){grepl('Africa', x, fixed = F)})))
tickers[index, ]


index <- which(unlist(lapply(tickers$Exchange,  function(x){grepl(NA, x, fixed = F)})))
tickers[index, ]



tickers[tickers$Ticker == 'STX40.JO',]
unique(tickers$Exchange)



# ---- find 'Apple' example ----x
'Apple' %in% tickers$Name
index <- which(unlist(lapply(tickers$Name, function(x){grepl('Apple', x, fixed = F)} )))
c('Apple 2', 'gs')  %in% 'Apple'
grepl('Apple', c('Apple 2', 'gs'), fixed = F)






# 
# 
# 
# summarize_data <- function(from, to, purchase_value, data) {
#   'Return: only the data required to compute the ROI on the period'
#   
#   min_date <- which.min(abs(data$date-as.Date(from)))
#   cur_date <- which.min(abs(data$date-as.Date(to)))
#   days     <- cur_date - min_date
#   
#   # --- impute missing values ----x
#   data[, c("open", "high", "low", "close", "volume", "adjusted", "price")] <- 
#     na_interpolation(data[, c("open", "high", "low", "close", "volume", "adjusted", "price")])
#   
#   
#   
#   df <- data.frame(purchase_date   = data[min_date, 'date'], 
#                    curr_date       = data[cur_date, 'date'], 
#                    days            = cur_date - min_date,
#                    purchase_price  = data[min_date, 'price'], 
#                    curr_price      = data[cur_date, 'price'], 
#                    interest_earned = data[cur_date, 'price'] - data[min_date, 'price'], 
#                    ROI             = 1 + (data[cur_date, 'price'] - data[min_date, 'price'])/data[min_date, 'price'])
#   
#   
#   names             <- c('purchase_date', 'curr_date', 'days', 'purchase_price', 'curr_price', 'interest_earned', 'ROI')
#   names(df)         <- names
#   df$purchase_value <- purchase_value
#   df$current_value  <- purchase_value * df$ROI
#   return(df)
# }
# 
# 
# 
# 
# 
# 
# 
# annuity_date <- seq(as.Date(from), as.Date(to), by='month')[-1]
# 
# 
# dt <- summarize_data(from=from, to=to, purchase_value=1000, data=STX40.JO)
# for (a in as.character(annuity_date)){
#   
#   # print(a)
#   dt <- rbind(dt, summarize_data(a, to=to, purchase_value=100, data=STX40.JO))
#   
# }
# 
# 
# 
# # ================== VECTOR FORM ====================x
# 
# compute_asset_return <- function(
#   data=STX40.JO,
#   annuity=c(1000, rep(100, length(seq(as.Date(from), as.Date(to), by='month'))-1)),
#   annuity_schedule=seq(as.Date(from), as.Date(to), by='month'),
#   print=TRUE
#   ) {
#   
#   # ---- time sequence ----x 
#   annuity_date <- annuity_schedule
#   
#   # ---- impute data ----x
#   data <- na_interpolation(data)
#   
#   # ---- dates ----x
#   date_index    <- unlist(lapply(annuity_date, FUN = function(x) {which.min(abs(data$date - x))}))
#   purchase_date <- data$date[date_index]
#   current_date  <- data$date[which.min(abs(data$date-as.Date(to)))]
#   days          <- cur_date - min_date
#   
#   
#   # ---- invest ----x
#   principal     <- annuity
#   
#   # ---- price data ----x
#   price <- unlist(lapply(purchase_date, FUN=function(x) data[data$date == x, 'price']))
#   
#   
#   df <- data.frame(purchase_date,
#                    current_date, 
#                    days, 
#                    principal,
#                    price)
#   
#   # ---------- RECURSIVELY UPDATE: weighted price  -------------x
#   x_bar        <- c(df$price[1])
#   pur_quantity <- cumsum(df$principal)  # Raw purchase quantity
#   for (i in 2:nrow(df)) {
#     x_bar <- c(x_bar, 
#                x_bar[i-1] * (pur_quantity[i-1]/pur_quantity[i]) + 
#                  df$price[i]   * (pur_quantity[i] - pur_quantity[i-1])/pur_quantity[i]
#     )
#   }
#   
#   
#   # wieghted purchase price at each time! 
#   df$ave_price <- x_bar
# 
#   # ---- interest earned ----x
#   df$ROI_on_ave_price <- df$price/df$ave_price
#   
#   # 
#   df$cummulative_invested <- pur_quantity
#   
#   # NBNBNB! Portofolio value
#   df$portfolio_value <- df$ROI_on_ave_price * df$cummulative_invested
#   
#   
#   # ---- compute interest ----x
#   
#   df$interest_earned <- df$portfolio_value - df$cummulative_invested
#   summary_stats <- df[nrow(df),]
#   
#   
#   if (print) {
#     print(paste0('Final Return: ', round(summary_stats$ROI,4)*100, '%'))
#     print(paste0('Total Annuity: R', summary_stats$cummulative_invested))
#     print(paste0('Total Portfolio: R', round(summary_stats$portfolio_value,2)))
#     print(paste0('Total Interest: R', round(summary_stats$interest_earned,2)))
#     
#   }
#   
#   return(list(summary_stats=summary_stats, df=df))
#   }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# asset_return <- compute_asset_return()
# 
# 
# 
# plot_portfolio <- function(asset_return, asset_name) {
#   df <- asset_return$df
#   plot(df$purchase_date, df$portfolio_value, 'line',
#        col='black', lty=1, lwd=2,frame=F, main=paste0('Portfolio Return: ', asset_name), 
#        font.main=1, ylab='$$$', xlab='date')
#   polygon( 
#     x=c(min(df$purchase_date), df$purchase_date, max(df$purchase_date)),
#     y=c(0, df$portfolio_value, 0), 
#     col=alpha('steelblue', alpha = 0.9),
#     border=FALSE
#   )
#   lines(df$purchase_date, df$cummulative_invested, col='darkred', lty=1, lwd=2)
#   polygon( 
#     x=c(min(df$purchase_date), df$purchase_date, max(df$purchase_date)),
#     y=c(0, df$cummulative_invested, 0), 
#     col=alpha('red', alpha = 0.4),
#     border=FALSE
#   )
#   legend('topleft', box.lwd = 0, lty=1,
#          legend = c('Annuity', 'Total Value'),
#          col = c('darkred', 'darkblue'))
#   
# }
# 
# 
# 
# 
# 
# # calls 
# asset_return <- compute_asset_return()
# asset_return$summary_stats
# gbz$summary_stats
# plot_portfolio(asset_return, 'STX40')
# gbz <- compute_asset_return(data=gbp_zar)
# plot_portfolio(gbz, asset_name='GBP-ZAR')









