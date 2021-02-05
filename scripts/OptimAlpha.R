
library(tidyquant)
library(readxl)
library(lubridate)

library(tidyverse)
library(reshape2)


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


# --------------------------- Generated Data --------------------------- x

satrix_yield <- function(princ=100, n=1, prime=0.07, to="2021-01-25") {
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
  res$A <- princ*(1+earn)^n
  
  A <- princ
  dates <- seq(ymd(to)-years(n), ymd(to), by='year')
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




stx_free_funds <- satrix_yield(princ=100000, n=100)
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


# ---- datasets ----x
x <- list(STX40.JO, 
          usd_zar,
          # gbp_zar, 
          # btc_usd,
          # FFB.JO, 
          # FFA.JO, 
          # Tbills, 
          # inflation_SA
          # # inflation_US, 
          stx_free_data
          # capitec
          )


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
       font.main=1, col=cols[1], ylab='Price', xlab='date', ylim=c(mn,m))
  
  
  for (l in 2:length(x)) {
    lines(x[[l]]$date, normalize(x[[l]]$price), col=cols[l])
    
  }
  

  legend('topleft', legend=unlist(lapply(x, FUN = function(i) i$symbol[1])),
         col=cols, lty=1, border.lwd=0)
}
grandplot(x, mt, mm)








# Annuities
# https://www.investopedia.com/retirement/calculating-present-and-future-value-of-annuities/















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












