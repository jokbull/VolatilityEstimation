# 
# 
# data(ttrc)
# ohlc <- ttrc[,c("Open","High","Low","Close")]
# 
# DT <- data.table(ttrc)
# setnames(DT,names(DT), c("Date","OpenPrice","HighPrice","LowPrice","ClosePrice","Volume"))
# 
# microbenchmark::microbenchmark(
#   volatility(DT,calc="garman"),
#   TTR::volatility(ohlc,calc="garman")
# )
# 
# 
# 
# 
# getSymbol(dateRange = as.Date(c("2005-01-01","2015-04-30"))  )
# # 
# AdjPrice(SH50)


#' @export
runExample <- function(n=20) {
  getSymbol("SH50Index")
  v <- function(x,data=DT,...) {
    HistoricalVolatility(data,calc=x,...)
  }
  
  methodList <- c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang")
  
  SH50[, paste0("vol.",methodList) := lapply(methodList, v, data=SH50,n=n)]
  plotVol(SH50Index[-(1:210)])
}