
# require S3 generic [.pval()]; do *not* move to children packages!!


#' @title Convert into Multiple Univariable Structures
#' 
#' @param object see **Usage**
#' 
#' @param ... additional parameters, currently of no
#' 
#' @keywords internal
#' @name univar
#' @export
as.univar <- function(object, ...) UseMethod(generic = 'as.univar')





#' @rdname univar
#' 
#' @param envir \link[base]{environment}, 
#' for \link[stats]{terms} and \link[stats]{formula} dispatches,
#' so that the returned univariable formula(s) are 
#' in the same \link[base]{environment} as the input  
#' 
#' @examples
#' as.univar(y ~ x1 + I(x2^2))
#' as.univar(y ~ x1 + x2 + (1|g) + (1|g1:g2)) # e.g., ?ordinal::clmm
#' @method as.univar terms
#' @export as.univar.terms
#' @export
as.univar.terms <- function(object, envir = parent.frame(), ...) {
  
  v <- vterms(object)
  
  group <- v |> attr(which = 'group', exact = TRUE)
  
  if (!length(group)) {
    fn <- \(e) {
      call(name = '~', v[[1L]], e) |> 
        eval(envir = envir)
    }
  } else {
    fn <- \(e) {
      eg <- Reduce(f = \(e1, e2) call(name = '+', e1, e2), x = c(list(e), group))
      # connect all groupings naively by `+`
      call(name = '~', v[[1L]], eg) |> 
        eval(envir = envir)
    }
  }
  
  v[-1] |> # remove left-hand-side
    lapply(FUN = fn)
  
}


#' @rdname univar
#' @param formula \link[stats]{formula}
#' 
#' @details
#' Function [as.univar.formula()] and [as.univar.terms()] return a \link[base]{list} of \link[stats]{formula}s.
#' 
#' 
#' @importFrom stats terms.formula
#' @method as.univar formula
#' @export as.univar.formula
#' @export
as.univar.formula <- function(formula, envir = parent.frame(), ...) {
  formula |> 
    terms.formula() |> 
    as.univar.terms(envir = envir, ...)
}




#' @rdname univar
#' 
#' @details
#' Function [as.univar.default()] returns a \link[stats]{listof} regression models.
#' 
#' @examples
#' # see ?md_
#' @importFrom stats terms update
#' @importFrom ecip isIntercept .pval
#' @method as.univar default
#' @export as.univar.default
#' @export
as.univar.default <- function(object, ...) {
  
  ret <- object |> 
    formula() |> # must for mixed models, e.g., ?ordinal::clmm, ?lme4::merMod
    terms() |> 
    as.univar.terms() |>
    lapply(FUN = \(fom) update(object, formula. = fom))
  # univariable regression models
  
  fn_min_pvalue <- \(x) {
    p0 <- .pval(x) # use S3 [.pval()]
    p <- p0[!isIntercept(names(p0))] # remove intercept important!
    # missing pvalue: missing observed event ('coxph'), etc
    if (all(is.na(p))) return(1) # do not want to pick
    min(p, na.rm = TRUE)
  }
  
  attr(ret, which = 'min_pvalue') <- ret |>
    vapply(FUN = fn_min_pvalue, FUN.VALUE = NA_real_)
  attr(ret, which = 'initial.model') <- object
  class(ret) <- c('univar', 'listof')
  return(ret)
  
}




#' @importFrom ecip ecip simple_matrix_ecip intercept_rm.ecip
#' @export
as.matrix.univar <- function(x, ...) {
  # !! needed when combining `univar` and `multivar`
  # !! only prints \link[rmd.tzh]{label_pvalue_sym} and `@nobs`
  y <- x |> 
    lapply(FUN = \(i) {
      i |>
        ecip() |>
        intercept_rm.ecip() |>
        simple_matrix_ecip()
    })
  
  ret <- y |> 
    do.call(what = rbind)
  attr(ret, which = 'nrow') <- y |> vapply(FUN = nrow, FUN.VALUE = NA_integer_)
  return(ret)
}



#' @title Convert [univar] Object to \link[flextable]{flextable}
#' 
#' @description ..
#'  
#' @param x [univar] object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns 
#' Function [as_flextable.univar()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal  
#' @importFrom flextable as_flextable
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.univar
#' @export
as_flextable.univar <- function(x, ...) {
  y <- x |> 
    as.matrix.univar()
  
  y |> 
    as_flextable.matrix(
      row.title = deparse1(endpoint(x[[1L]])),
      hline_i = y |> attr(which = 'nrow', exact = TRUE) |> cumsum()
    )
}

#' @title R Markdown Lines for [univar] Object
#' 
#' @param x [univar] object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom methods new
#' @export md_.univar
#' @export
md_.univar <- function(x, xnm, ...) {
  c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable.univar(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
}


#' @export
print.univar <- function(x, ...) {
  x |> as_flextable.univar() |> print()
}










# base::subset
#' @title subset.univar
#' 
#' @param x [univar] object
#' 
#' @param subset ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @export subset.univar
#' @export
subset.univar <- function(x, subset, ...) {
  subset <- substitute(subset)
  v_sub <- all.vars(subset)
  if (!all(v_sub %in% c('min_pvalue'))) stop('criterion must be set on `min_pvalue`, for now')
  min_pvalue <- attr(x, which = 'min_pvalue', exact = TRUE)
  ret <- x[eval(subset)]
  attr(ret, which = 'p_thres') <- subset[[3L]]
  return(ret)
}



#' @export
`[.univar` <- function(x, i) {
  ret <- unclass(x)[i]
  attr(ret, which = 'min_pvalue') <- attr(x, which = 'min_pvalue', exact = TRUE)[i]
  class(ret) <- class(x) # otherwise class info dropped
  return(ret)
}

