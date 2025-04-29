
#' @title S3 methods for \link[MASS]{rlm}
#' 
#' @param x an \link[MASS]{rlm} object
#' 
#' @examples
#' library(MASS)
#' # ?MASS::rlm
#' m = rlm(stack.loss ~ ., data = stackloss)
#' 
#' @name S3_rlm
#' @export
desc_.rlm <- function(x) 'robust linear regression'


# must!!
# ?lmtest::coeftest will dispatch to ?lmtest::coeftest.default
#' @importFrom sfsmisc f.robftest
#' @export
.pval.rlm <- function(x) {
  vapply(names(x$coefficients), FUN = \(iv) {
    f.robftest(object = x, var = iv)$p.value
  }, FUN.VALUE = 0, USE.NAMES = TRUE)
}

#' @rdname S3_rlm
#' @importFrom utils packageDate
#' @export
.pval.summary.rlm <- function(x) {
  cf <- x$coefficients
  dt <- packageDate(pkg = 'MASS')
  if (dt == as.Date('2025-02-19')) stop('MASS:::summary.rlm does not carry p-value(s)')
  stop('check if new version of MASS:::summary.rlm carry p-value(s)')
}


