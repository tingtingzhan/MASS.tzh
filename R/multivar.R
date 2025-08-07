
#' @title as.multivar
#' 
#' @param x ..
#' 
#' @param ... parameters of [subset.univar()]
#' 
#' @keywords internal
#' @importFrom stats update
#' @importFrom ecip vterms
#' @aliases multivar
#' @export
as.multivar <- function(x, ...) {
  
  if (!inherits(x, what = 'univar')) stop()
  
  lhs <- vterms(x[[1L]])[[1L]] # do not bother to check `lhs` being same accross `x`
  
  x1 <- x |> 
    subset.univar(...)
  
  if (length(x1)) {
    #rhs <- x1 |>
    #  vapply(FUN = \(i) {
    #    (vterms(i)[[2L]]) |> deparse1()
    #  }, FUN.VALUE = '') |>
    #  paste(collapse = ' + ') |>
    #  str2lang() # only good for non-random effects
    trm <- x1 |> lapply(FUN = vterms)
    group <- trm |> lapply(FUN = attr, which = 'group', exact = TRUE) |> unique()
    if (length(group) != 1L) stop('should not happen')
    fix. <- trm |>
      lapply(FUN = '[[', 2L) |>
      Reduce(f = \(e1, e2) call(name = '+', e1, e2))
    rhs <- if (!length(group[[1L]])) fix. else {
      c(list(fix.), group[[1L]]) |>
        Reduce(f = \(e1, e2) call(name = '+', e1, e2))
    }
  } else {
    rhs <- 1 # only good for non-random effects!!!
  }
  
  ret <- x[[1L]] |> 
    update(formula. = call(name = '~', lhs, rhs) |> eval()) |>
    stepAIC_complete()
  attr(ret, which = 'univar') <- x
  attr(ret, which = 'p_thres') <- attr(x1, which = 'p_thres', exact = TRUE)
  class(ret) <- c('multivar', class(ret))
  return(ret)
  
}



#' @title as_flextable.multivar
#' 
#' @description ..
#'  
#' @param x [multivar] object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal 
#' @importFrom flextable as_flextable color
#' @importFrom flextable.tzh as_flextable.matrix
#' @importFrom rmd.tzh label_pvalue_sym
#' @importFrom ecip intercept_rm.matrix
#' @export as_flextable.multivar
#' @export
as_flextable.multivar <- function(x, ...) {
  
  # '\u274c' # unicode 'Cross Mark'
  # '\U1f6ab' # unicode 'No Entry Sign'
  
  u <- x |> 
    attr(which = 'univar', exact = TRUE) |>
    as.matrix.univar()
  
  names(x)[[1L]] <- paste(
    names(x)[[1L]], 
    x |> 
      attr(which = 'p_thres', exact = TRUE) |> 
      label_pvalue_sym(add_p = TRUE)()
  )
  
  m <- x |>
    as.matrix.stepAIC() |>
    intercept_rm.matrix()
  
  ret <- cbind(u, 
               array('\U1f6ab', dim = c(nrow(u), ncol(m)), dimnames = list(NULL, colnames(m))))
  id <- match(rownames(m), table = rownames(u))
  if (anyNA(id)) stop('should not happen')
  ret[id, seq_len(ncol(m))+1L] <- m
  colnames(ret)[1L] <- paste('(Univariable)', colnames(ret)[1L], sep = '\n')
  
  ret |>
    as_flextable.matrix(
      row.title = ecip(x[[length(x)]])@endpoint,
      hline_i = u |> attr(which = 'nrow', exact = TRUE) |> cumsum()
    ) |>
    color(
      j = 2:3, # univariable column, initial multivariable column
      color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      '\u274c: predictor(s) removed by stepwise algorithm.',
      '\U1f6ab: predictor(s) not considered in the model.'
    ))

}



#' @export
print.multivar <- function(x, ...) {
  x |> as_flextable.multivar() |> print()
}





#' @title Additional S3 methods for `multivar`
#' 
#' @param x `multivar`
#' 
#' @name S3_multivar
#' @keywords internal
#' @importFrom ecip endpoint
#' @export endpoint.multivar
#' @export
endpoint.multivar <- function(x) (x[[length(x)]]) |> endpoint()

#' @rdname S3_multivar
#' @importFrom ecip nobsText
#' @export nobsText.multivar
#' @export
nobsText.multivar <- function(x) (x[[length(x)]]) |> nobsText()

#' @rdname S3_multivar
#' @importFrom ecip desc_
#' @export desc_.multivar
#' @export
desc_.multivar <- function(x) (x[[length(x)]]) |> desc_()





#' @title Model Description of [multivar] Object
#' 
#' @description ..
#' 
#' @param x a [multivar] object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' library(lme4.tzh)
#' library(HSAUR3)
#' library(ordinal)
#' m1 = lm(mpg ~ cyl + am + hp + wt + qsec + drat + disp, data = ecip::mtc)
#' m2 = glmer(outcome ~ treatment + visit + (1|patientID), data = toenail,
#'   family = binomial, nAGQ = 20)
#' m3 = clmm(SURENESS ~ PROD + SOUPTYPE + (1|RESP) + (1|RESP:PROD), data = soup,
#'  link = 'probit', threshold = 'equidistant')
#'   
#' library(ecip); list(
#'  'multivar, `lm`' = m1 |> as.univar() |> as.multivar(subset = min_pvalue < .1),
#'  'multivar, `merMod`' = m2 |> as.univar() |> as.multivar(subset = min_pvalue < .1)# ,
#'  # 'multivar, `clmm`' = m3 |> as.univar() |> as.multivar(subset = min_pvalue < .1)# still bug
#' ) |> rmd.tzh::render_(file = 'multivar')
#' @keywords internal
#' @importFrom stats formula
#' @importFrom rmd.tzh md_ fromPackage pkg_text
#' @importClassesFrom rmd.tzh md_lines
#' @export md_.multivar
#' @export
md_.multivar <- function(x, xnm, ...) {
  
  u <- x |> 
    attr(which = 'univar', exact = TRUE)
  
  v <- u |>
    vapply(FUN = \(i) deparse1(vterms(i)[[2L]]), FUN.VALUE = '')
  
  pkg <- u[[1L]] |> fromPackage()
  
  z1 <- sprintf(
    fmt = 'The relationship between **`%s`** and %s is analyzed based on %s by first fitting univariable *%s* models due to the limited sample size, denegerated experimental design and/or substantial missingness across the predictors, using %s.',
    x |> endpoint() |> deparse1(),
    paste0('`', v, '`', collapse = ', '),
    nobsText(x),
    desc_(x),
    pkg |> pkg_text()
  ) |>
    new(Class = 'md_lines', package = pkg)
  
  z2 <- sprintf(
    fmt = 'Next, univariable predictor(s) with $p$-value<%.2f are considered for the multivariable model. Lastly, a backward stepwise variable selection by [Akaike information criterion (AIC)](https://en.wikipedia.org/wiki/Akaike_information_criterion) is performed using <u>**`R`**</u> package <u>**`MASS`**</u>.',
    x |> attr(which = 'p_thres', exact = TRUE)
  ) |>
    new(Class = 'md_lines', package = 'MASS')
  
  z3 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2, z3) # rmd.tzh::c.md_lines
  
}

