
#' @title S3 methods for \link[MASS]{rlm}
#' 
#' @param x an \link[MASS]{rlm} object
#' 
#' @examples
#' library(ecip); list(
#'  '`rlm`' = rlm(stack.loss ~ ., data = stackloss)
#' ) |> fastmd::render2html()
#' @keywords internal
#' @name S3_rlm
#' @importFrom ecip desc_
#' @export desc_.rlm
#' @export
desc_.rlm <- function(x) 'robust linear regression'


# must!!
# ?lmtest::coeftest will dispatch to ?lmtest::coeftest.default
#' @rdname S3_rlm
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
#' @importFrom ecip .pval
#' @method .pval summary.rlm
#' @export .pval.summary.rlm
#' @export
.pval.summary.rlm <- function(x) {
  cf <- x$coefficients
  stop('MASS:::summary.rlm does not carry p-value(s)')
}



#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.rlm <- md_ecip




