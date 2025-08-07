

#' @title getCanonicalLink
#' 
#' @description ..
#' 
#' @param x object returned from function \link[MASS]{glm.nb}
#' 
#' @examples
#' library(MASS)
#' glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine) |>
#'   getCanonicalLink.negbin()
#'
#' @keywords internal   
#' @importFrom MASS negative.binomial
#' @importFrom ecip getCanonicalLink
#' @export getCanonicalLink.negbin
#' @export
getCanonicalLink.negbin <- function(x) formals(fun = negative.binomial)$link


