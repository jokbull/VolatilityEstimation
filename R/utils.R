#' @export
ROC <- function (x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE) 
{
    type <- match.arg(type)
    NAs <- NULL
    if (na.pad) {
      NAs <- rep(NA, n)
    }
    c(NAs,switch(type,
       discrete = x[(n + 1):NROW(x)]/x[1:(NROW(x) - n)] - 1,
       continuous = diff(log(x), n)))
         
}
