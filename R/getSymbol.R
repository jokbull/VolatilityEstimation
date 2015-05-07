
#' @export
getSymbol <- function(symbol = "SH50", mode = c("DB","TXT"), dateRange = NULL,...){
  SecuCode <- switch(symbol,
      SH50 = "510050.SH",
      SH50Index = "000016.SH")
  mode <- match.arg(mode)
  if (is.null(dateRange)) dateRange <- c(as.Date("2005-01-01"),Sys.Date())
  switch(mode, 
    DB = { 
      
      require(RJDBC)
      if (.Platform$OS.type == "unix"){
        drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
                    "/etc/sqljdbc_2.0/sqljdbc4.jar")
      } else {
        drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
                    "C:/jdbc/sqljdbc_4.1/chs/sqljdbc4.jar")
      }
      
      conn <- dbConnect(drv, "jdbc:sqlserver://192.168.1.108" ,"zlfund", "zlfund")
      
      SecuMarket <- switch(substr(SecuCode,8,9),SH=83,SZ=90)
      
      sqlText <- sprintf("
          With TD as 
          (
            select TradingDate 
            from QT_TradingDayNew 
            where SecuMarket = %d and IfTradingDay = 1 and TradingDate between '%s' and  '%s'
          ),
          QT as (
          select 
          	 sm.SecuCode,
          	 sm.SecuMarket,
          	 sm.SecuAbbr,
          	 sm.InnerCode, 
          	 TD.TradingDate, 
          	 max(Fac.ExDiviDate) as ExDiviDate
          from TD 
          	join SecuMain sm on  sm.SecuCode = '%s' and sm.SecuMarket = %d
          	left join QT_AdjustingFactor Fac on Fac.ExDiviDate <= TD.TradingDate and sm.InnerCode = Fac.InnerCode
          group by sm.SecuCode, sm.SecuMarket, sm.SecuAbbr,sm.InnerCode, TD.TradingDate
          )
          select QT.SecuCode,QT.SecuAbbr,QT.TradingDate, coalesce(Fac.RatioAdjustingFactor,1) as RatioAdjustingFactor, D.PrevClosePrice, D.OpenPrice, D.HighPrice, D.LowPrice, D.ClosePrice, D.TurnoverVolume, D.TurnoverValue, D.TurnoverDeals
          from QT 
          	left join QT_AdjustingFactor Fac on QT.InnerCode = Fac.InnerCode and QT.ExDiviDate = Fac.ExDiviDate
          	left join QT_DailyQuote D on QT.InnerCode=D.InnerCode AND D.TradingDay = QT.TradingDate
          order by QT.TradingDate
          ", SecuMarket,  dateRange[1], dateRange[2],substr(SecuCode,1,6), SecuMarket)
      
      queryResults <- dbGetQuery(conn, sqlText)
      setDT(queryResults)
      assign(symbol, queryResults, envir = parent.frame())
    },
    TXT = {
      print("TXT")
      assign(symbol,  1:10, envir = parent.frame())
    }
  )
}
#' @export
AdjPrice <- function(DT){
  if (!"RatioAdjustingFactor" %in% names(DT)) stop("Lack of RatioAdjustingFactor")
  indexPrice <- grep("Price", names(DT))
  RatioAdjustingFactor <- DT[,RatioAdjustingFactor]
  DT[, names(DT)[indexPrice] := lapply(.SD, function(x) x * RatioAdjustingFactor / tail(RatioAdjustingFactor,1)), .SDcols = indexPrice]
  invisible(DT)
}

