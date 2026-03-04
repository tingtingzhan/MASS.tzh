
#' @title Support for \link[MASS]{rlm}
#' 
#' @examples
#' library(ecip); list(
#'  '`rlm`' = rlm(stack.loss ~ ., data = stackloss) # c('rlm', 'lm')
#' ) |> fastmd::render2html()
#' @keywords internal
#' @name rlm
NULL


#' @importFrom ecip desc_
#' @export
desc_.rlm <- function(x) 'robust linear regression'


# must!!
# ?lmtest::coeftest will dispatch to ?lmtest::coeftest.default
#' @importFrom sfsmisc f.robftest
#' @importFrom ecip .pval
#' @export
.pval.rlm <- function(x) {
  x$coefficients |> 
    names() |> 
    vapply(FUN = \(iv) {
      f.robftest(object = x, var = iv)$p.value
    }, FUN.VALUE = 0, USE.NAMES = TRUE)
}

#' @importFrom ecip .pval
#' @method .pval summary.rlm
#' @export
.pval.summary.rlm <- function(x) {
  cf <- x$coefficients
  stop('MASS:::summary.rlm does not carry p-value(s)')
}





