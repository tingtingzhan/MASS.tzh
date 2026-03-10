

#' @title Support for \link[MASS]{glm.nb}
#' 
#' @examples
#' library(MASS)
#' nb = glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
#' class(nb) # c('negbin', 'glm', 'lm')
#' 
#' library(ecip)
#' nb |> getCanonicalLink()
#' list(negbin = nb) |>
#'   fastmd::render2html()
#'
#' @keywords internal   
#' @name negbin
NULL




#' @importFrom ecip getCanonicalLink
#' @export
getCanonicalLink.negbin <- function(x) {
  formals(fun = negative.binomial)$link
}



#' @importFrom ecip desc_
#' @importClassesFrom fastmd md_lines
#' @export
desc_.negbin <- function(x) {
  'negative binomial generalized linear regression' |>
    sprintf(fmt = '*%s*') |>
    new(Class = 'md_lines', package = 'MASS')
}


# inhertis