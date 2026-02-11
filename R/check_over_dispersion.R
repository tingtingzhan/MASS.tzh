

#' @title check_over_dispersion
#' 
#' @description select pois vs. negbin based on overdispersion
#' 
#' @param model a \link[stats]{glm} model for \link[stats]{poisson} family
#' 
#' @param ... ..
#' 
#' @examples
#' glm(Days ~ ., family = poisson, data = MASS::quine) |>
#'  check_over_dispersion() |> 
#'  stepAIC_complete()
#' 
#' @importFrom AER dispersiontest
#' @export
check_over_dispersion <- function(model, ...) {
  if (!inherits(model, what = 'glm') || family(model)$family != 'poisson') stop('input needs to be Poisson regression model')
  dsp <- dispersiontest(model, trafo = 1)
  if (dsp$p.value >= .05) return(model)
  
  message('Over dispersion detected; use negative binomial regression instead')
  cl <- model$call
  cl$family <- NULL
  cl[[1L]] <- quote(glm.nb)
  return(eval(cl))
}
