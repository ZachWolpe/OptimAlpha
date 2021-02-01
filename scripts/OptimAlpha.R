
library(tidyquant)
library(readxl)

# ---- equity data ----x
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
STX40.JO  <- tq_get('STX40.JO', get = "stock.prices", from = '2000-01-01', to='2021-01-25')







from = '2000-01-01'
to   = '2021-01-25'


usd_zar <- tq_get('USDZAR=X', from=from, to=to)


# ---- plot ----x
normalize <- function(xvar) (xvar - mean(xvar, na.rm=T))/sd(xvar, na.rm=T)

plot(STX40.JO$date, normalize(STX40.JO$close), type='line', frame=F, main='Satrix Top 40', 
     font.main=1, col='steelblue', ylab='Price', xlab='date')
lines(usd_zar$date, normalize(usd_zar$close), col='grey')
legend('bottomright', legend=c(STX40.JO[1,1], usd_zar[1,1]),
       col=c('steelblue', 'grey'), lty=1, border.lwd=0)





eft_list <- read_excel('~/Desktop/OptimAlpha/data/jse EFT list.xlsx')

eft_list$Alpha


# ---------- 5 years Correlation Across EFTs ------------x
STX40.JO  <- tq_get('STX40.JO', get = "stock.prices", from = '2000-01-01', to='2021-01-25')



eft_list$Alpha

res <- c()
for (a in eft_list$Alpha) {
  tick <- paste0(a, '.JO')
  if (tick != 'NA.JO') {
    res <- rbind(res, tq_get(tick, get = "stock.prices", from = '2015-01-01', to='2021-01-25'))
  }
}


library(tidyverse)





library(reshape2)



wid_view <- dcast(res, date ~ symbol, value.var='close')

index(wid_view) <- wid_view[,1]


plot(wid_view[,1], wid_view[,5])

r1 <- res[sample(1:70000, 10),c('date','symbol','close')]
dcast(r1, date ~ symbol, value.var='close')

data.frame(1:10, )


tq_get('PREFTX.JO', get = "stock.prices", from = '2015-01-01', to='2021-01-25')
  

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corr <- cor(res)
tq_get('STX40.JO', get = "stock.prices", from = '2000-01-01', to='2021-01-25')

# efts
# https://www.jse.co.za/trade/equities-market/equities/exchange-traded-products/exchange-traded-funds












# ---- tickers ----x
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


  
install.packages('BatchGetSymbols')
library(BatchGetSymbols)

BatchGetSymbols::fix.ticker.name('Satrixsdjkn')










