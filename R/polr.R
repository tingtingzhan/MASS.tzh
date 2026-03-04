
#' @title Support for \link[MASS]{polr}
#' 
#' @examples
#' library(MASS)
#' library(ecip)
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#' list('`polr`' = polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)) |>
#'  fastmd::render2html()
#' @keywords internal
#' @name polr
NULL


#' @importFrom ecip getLink
#' @export
getLink.polr <- function(x) x$method
# no ?stats::family method for 'polr' object
# see ?MASS::polr
# 'logit' is written as 'logistic'
# but inside ?MASS::polr, `switch(method, logistic = plogis, ...)`, this is indeed 'logit'



#' @importFrom ecip desc_
#' @export
desc_.polr <- function(x) {
  paste('ordered', x$method, 'regression')
}



#' @importFrom ecip .pval
#' @method .pval summary.polr
#' @export
.pval.summary.polr <- function(x) {
  cf <- x$coefficients
  stop('MASS:::summary.polr does not carry p-value(s)')
}


#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.polr <- md_ecip