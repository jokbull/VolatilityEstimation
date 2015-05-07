.OnAttach <- function(libname, pkgname) {
  packageStartupMessage("Volatility Estimation Toolset")
}

.onLoad <- function(libname, pkgname) {
  if(!require(data.table)) stop("need data.table package")
}

#' @export 
HistoricalVolatility <- function (DT , n = 10, 
    calc = c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang"), 
    N = 252, ...) 
{
  require(data.table)
  calc <- match.arg(calc)
  switch(calc, 
    close = {
      if ("ClosePrice" %in% names(DT)) {
        tt = DT[, ClosePrice]
        r <- ROC(DT[, ClosePrice], 1, ...)
        s <- sqrt(N) *  RcppRoll::roll_sd(r,n)
      }
    },
    garman.klass = {
      if (all(c("OpenPrice","HighPrice","LowPrice","ClosePrice") %in% names(DT))) {
        s <- sqrt(N/n * RcppRoll::roll_sum(DT[,
                    0.5*(log(HighPrice/LowPrice))^2 - (2 * log(2) - 1) * log(ClosePrice / OpenPrice)^2
                ], n)) 
      }
    },
    parkinson = {
      if (all(c("HighPrice","LowPrice") %in% names(DT))) {
        s <- sqrt(N/(4 * n * log(2)) * RcppRoll::roll_sum(DT[, log(HighPrice/LowPrice)^2], n))
      }
    },
    rogers.satchell = {
      if (all(c("OpenPrice","HighPrice","LowPrice","ClosePrice") %in% names(DT))) {
        s <- sqrt(N/n * RcppRoll::roll_sum(DT[,
          log(HighPrice/ClosePrice) * log(HighPrice/OpenPrice) + log(LowPrice/ClosePrice) * log(LowPrice/OpenPrice)], n))
      }                                                                                                           
    },
    gk.yz = {
      if (all(c("OpenPrice","HighPrice","LowPrice","ClosePrice") %in% names(DT))) {
        s <- sqrt(N/n * RcppRoll::roll_sum(DT[,
        log(OpenPrice/c(NA,head(ClosePrice, -1)))^2 + 0.5 * log(HighPrice/LowPrice)^2 - (2*log(2)-1) * log(ClosePrice/OpenPrice)^2
        ],n))
      }
    },
    yang.zhang = {
    
      dots <- list(...)
      if (is.null(dots$alpha)) {
        alpha <- 1.34
      }
      if (is.null(dots$k)) {
        k <- (alpha - 1)/(alpha + (n + 1)/(n - 1))
      }
      s2o <- N * RcppRoll::roll_var(DT[, log(OpenPrice/c(NA,head(ClosePrice, -1))) ], n = n)
      s2c <- N * RcppRoll::roll_var(DT[, log(ClosePrice / OpenPrice) ], n = n)
      s2rs <- HistoricalVolatility(DT = DT, n = n , calc = "rogers.satchell", 
                       N = N, ...)
      s <- sqrt(c(rep(NA,n-1),s2o + k * s2c) + (1 - k) * (s2rs^2))
      return(invisible(s))
  })
  invisible(c(rep(NA,n-1),s))
}

#' @export
VolatilityCones <- function(DT, 
  methodList = c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang"),
  n = c(5,10,15,20,25,30,40,50,60,80,100,120,150,180,210))
{
  methodList <- match.arg(methodList, several.ok = TRUE)
  cones <- lapply(methodList, function(method){
    res <- sapply(n, function(h) {
      volCone <- HistoricalVolatility(DT, calc=method,n=h)
      adjustcoef <- 1 / ( 1 - h / NROW(DT) + (h^2 - 1) / 3 / NROW(DT)^2)
      quantile(volCone,probs=c(0.05,0.25,0.5,0.75,0.95), na.rm = TRUE) * adjustcoef
    })
    colnames(res) <- n
    rownames(res) <-  c("5%", "1st Qu.", "Median",  "3rd Qu.", "9%")
    comment(res) <- method
    plotVolCones(res)
    
  })
  names(cones) <- methodList
  cones
}

#' @export
plotVol <- function(DT){
  # eChart don't deal with NA...
  chartData <- reshape2::melt(DT, id = grep("TradingDate",names(DT)), measure = grep("vol.",names(DT))  )
  chartData$value <- round(chartData$value,4)
  chartData[,Date:=as.character(as.Date(TradingDate),format = "%Y/%m/%d")]
  u <- recharts::echart( chartData[!is.na(value)],  ~Date, ~value, series=~variable,type="line")
#   
#   
#   toolbox: {
#     show : true,
#     feature : {
#       mark : {show: true},
#       dataView : {show: true, readOnly: false},
#       magicType : {show: true, type: ['line', 'bar', 'stack', 'tiled']},
#       restore : {show: true},
#       saveAsImage : {show: true}
#     }
#   },
#   title : {
#     text: 'xxxx',
#     subtext: 'yyyy'
#   },

  u$x$title <- list(text = "Historical Volatility")
  u$x$xAxis$boundaryGap <- TRUE
  u$x$toolbox <- list( show = TRUE,
      feature = list(
        mark=list(show=TRUE),
        dataView = list(show=TRUE,readOnly = FALSE),
        magicType = list(show=TRUE,type=list("line")),
        restore  = list(show=TRUE),
        saveAsImage = list(show=TRUE)
        )
      )
# dataZoom : {
#   show : true,
#   realtime: true,
#   start : 50,
#   end : 100
# },
  u$x$dataZoom <- list(show=TRUE,realtime=TRUE,start=0,end=100)

# u$x$calculable <- TRUE
# tooltip : {
#   trigger: 'axis'
# },
  u$x$tooltip <- list(trigger = "axis")
  u
}
#' @export
plotVolCones <- function(volCone,...){
  if (is.list(volCone)) lapply(volCone,plotVolCones,...)
  chartData <- reshape2::melt(volCone)
  colnames(chartData) <- c("Level","Time","Volatility")
  u <- recharts::echart( chartData,  ~Time, ~Volatility, series=~Level,type="line")
  u$x$title <- list(text = "Volatility Cones", subtext = paste(comment(volCone), "Method"))
  u$x$xAxis$boundaryGap <- c(0.01,0.01)
  u$x$tooltip <- list(trigger = "item")
  u$x$toolbox <- list( show = TRUE,
                       feature = list(
                         mark=list(show=TRUE),
                         dataView = list(show=TRUE,readOnly = FALSE),
                         magicType = list(show=TRUE,type=list("line")),
                         restore  = list(show=TRUE),
                         saveAsImage = list(show=TRUE)
                       )
  )
  u
  
}

                           
                           
                  