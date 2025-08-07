
#' @title S3 methods for \link[MASS]{rlm}
#' 
#' @param x an \link[MASS]{rlm} object
#' 
#' @examples
#' library(MASS)
#' # ?MASS::rlm
#' m = rlm(stack.loss ~ ., data = stackloss)
#' 
#' @keywords internal
#' @name S3_rlm
#' @export
desc_.rlm <- function(x) 'robust linear regression'


# must!!
# ?lmtest::coeftest will dispatch to ?lmtest::coeftest.default
#' @name S3_rlm
#' @importFrom sfsmisc f.robftest
#' @importFrom ecip .pval
#' @export .pval.rlm
#' @export
.pval.rlm <- function(x) {
  x$coefficients |> 
    names() |> 
    vapply(FUN = \(iv) {
      f.robftest(object = x, var = iv)$p.value
    }, FUN.VALUE = 0, USE.NAMES = TRUE)
}

#' @rdname S3_rlm
#' @importFrom utils packageDate
#' @importFrom ecip .pval
#' @method .pval summary.rlm
#' @export .pval.summary.rlm
#' @export
.pval.summary.rlm <- function(x) {
  cf <- x$coefficients
  dt <- packageDate(pkg = 'MASS')
  if (dt == as.Date('2025-02-19')) stop('MASS:::summary.rlm does not carry p-value(s)')
  stop('check if new version of MASS:::summary.rlm carry p-value(s)')
}



#' @title R Markdown Lines for \link[MASS]{rlm}
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' library(rmd.tzh); library(ecip); list(
#'  '`rlm`' = rlm(stack.loss ~ ., data = stackloss)
#' ) |> render_(file = 'rlm')
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_
#' @importFrom ecip md_ecip
#' @export md_.rlm
#' @export
md_.rlm <- md_ecip




