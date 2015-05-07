
#' @export
getSymbol <- function(symbol = "SH50", mode = c("DB","TXT"), dateRange = NULL,...){
  SecuCode <- switch(symbol,
      SH50 = "510050.SH")
  mode <- match.arg(mode)
  if (is.null(dateRange)) dateRange <- as.Date(c("2014-01-01","2015-04-30"))
  switch(mode, 
    DB = { 
      print("RJDBC")
      assign(symbol,  1:5, envir = parent.frame())
    },
    TXT = {
      print("TXT")
      assign(symbol,  1:10, envir = parent.frame())
    }
  )
}
