
#' @title S3 methods for \link[MASS]{polr}
#' 
#' @param x a \link[MASS]{polr} object
#' 
#' @examples
#' library(MASS)
#' library(ecip)
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#' m = polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)
#' .pval(m)
#' @keywords internal
#' @name S3_polr
#' @importFrom ecip getLink
#' @export getLink.polr
#' @export
getLink.polr <- function(x) x$method
# no ?stats::family method for 'polr' object
# see ?MASS::polr
# 'logit' is written as 'logistic'
# but inside ?MASS::polr, `switch(method, logistic = plogis, ...)`, this is indeed 'logit'

#' @rdname S3_polr
#' @importFrom ecip desc_
#' @export desc_.polr
#' @export
desc_.polr <- function(x) paste('ordered', x$method, 'regression')


#' @rdname S3_polr
#' @importFrom ecip .pval
#' @method .pval summary.polr
#' @export .pval.summary.polr
#' @export
.pval.summary.polr <- function(x) {
  cf <- x$coefficients
  stop('MASS:::summary.polr does not carry p-value(s)')
}