.OnAttach <- function(libname, pkgname) {
  packageStartupMessage("Volatility Estimation Toolset")
}
.onLoad <- function(libname, pkgname) {
  if(!require(data.table)) stop("need data.table package")
}


#' @export
HistoryVolatility <- function (DT , n = 10, 
    calc = c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang"), 
    N = 252, ...) 
{
  calc <- match.arg(calc)
  switch(calc, 
    close = {
      if ("ClosePrice" %in% names(DT)) {
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
      s2rs <- HistoryVolatility(DT = DT, n = n , calc = "rogers.satchell", 
                       N = N, ...)
      s <- sqrt(c(rep(NA,n-1),s2o + k * s2c) + (1 - k) * (s2rs^2))
      return(invisible(s))
  })
  invisible(c(rep(NA,n-1),s))
}


VolatilityCone <- function(DT){
  volIndex <- grep("vol.",names(DT))
  if(length(volIndex))
    DT[,lapply(.SD, RcppRoll::roll_max, n=45),.SDcols=volIndex]
}


                           
                           
                  