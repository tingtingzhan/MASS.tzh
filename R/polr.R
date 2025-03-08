
#' @title S3 methods for \link[MASS]{polr}
#' 
#' @param x a \link[MASS]{polr} object
#' 
#' @examples
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#' m = polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)
#' 
#' @name S3_polr
#' @export
getLink.polr <- function(x) x$method
# no ?stats::family method for 'polr' object
# see ?MASS::polr
# 'logit' is written as 'logistic'
# but inside ?MASS::polr, `switch(method, logistic = plogis, ...)`, this is indeed 'logit'

#' @rdname S3_polr
#' @export
desc_.polr <- function(x) paste('ordered', x$method, 'regression')


#' @rdname S3_polr
#' @importFrom utils packageDate
#' @export
.pval.summary.polr <- function(x) {
  cf <- x$coefficients
  dt <- packageDate(pkg = 'MASS')
  if (dt == as.Date('2025-02-19')) stop('MASS:::summary.polr does not carry p-value(s)')
  stop('check if new version of MASS:::summary.polr carry p-value(s)')
}