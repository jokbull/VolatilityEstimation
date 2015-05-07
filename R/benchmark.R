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
# v <- function(x,data=DT,...) {
#   HistoryVolatility(data,calc=x,...)
# }
# methodList <- c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang")
# 
# DT[, paste0("vol.",methodList) := lapply(methodList, v, data=DT)]
#                   