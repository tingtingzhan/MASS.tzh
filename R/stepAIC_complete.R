

#' @title Perform \link[MASS]{stepAIC} on Complete Data
#' 
#' @description ..
#' 
#' @param object a regression model
#' 
#' @note
#' \link[MASS]{stepAIC} (as of 2025-02-19) ??? does not have a parameter for the end user to specify the
#' threshold of AIC improvement, that should be regarded as an improvement.
#' Instead, this quantity is hard coded (in `if (bAIC >= AIC + 1e-07) break`).
#' 
#' @examples 
#' stk = stackloss
#' stk[2L, 'Air.Flow'] = stk[5L, 'Water.Temp'] = 
#'   stk[16L, 'Acid.Conc.'] = stk[21L, 'stack.loss'] = NA
#' 
#' lm(stack.loss ~ ., data = stk) |> stepAIC_complete() |> summary()
#' lm(stack.loss ~ Air.Flow, data = stk) |> stepAIC_complete() |> summary()
#' 
#' mtc = mtcars |> within.data.frame(expr = {
#'  cyl = factor(cyl)
#'  gear = factor(gear)
#'  vs = as.logical(vs)
#'  am = as.logical(am)
#' })
#' glm(am ~ hp + wt + vs, data = mtc, family = binomial) |> stepAIC_complete()
#' glm(am ~ ., family = binomial, data = mtc) |> stepAIC_complete() |> suppressWarnings()
#' 
#' library(MASS)
#' glm.nb(Days ~ ., data = quine) |> stepAIC_complete()
#' 
#' library(ordinal)
#' clm(rating ~ temp * contact, data = ordinal::wine) |>
#'  stepAIC_complete()
#' 
#' @importFrom MASS stepAIC
#' @importFrom stats complete.cases terms
#' @export
stepAIC_complete <- function(object) {
  
  trms0 <- terms(object)
  if (any(trms0[[3L]] == c('0', '1'))) return(object) # input model contains at most an intercept term
  
  datacall <- object$call$data
  data <- eval(datacall)
  
  yok <- complete.cases(data[intersect(names(data), all.vars(trms0[[2L]]))])
  all_ok <- complete.cases(data[intersect(names(data), all.vars(trms0))])
  if (n_rm <- sum(yok & !all_ok)) cat(sprintf(fmt = 'Backward selection (%d subjects with missingness removed)\n\n', n_rm))
  
  # tested on returned object from
  # ?stats::lm
  # ?stats::glm
  # ?survival::coxph
  object$call$data <- if (inherits(object, what = c('coxph', 'negbin', 'clm', 'logistf'))) {
    data[all_ok, , drop = FALSE] # a 'data.frame', these object must
  } else quote(data[all_ok, , drop = FALSE]) # output cleaner :)
  ret0 <- stepAIC(object, direction = 'backward', trace = 0L)
  
  if (any(terms(ret0)[[3L]] == c('0', '1'))) cat('All variables removed by backward selection algorithm!\n')
  
  # ret <- eval(call(name = 'update', quote(ret0), data = datacall)) # sometimes error!
  # ret <- eval(call(name = 'update', quote(ret0), data = data)) # maybe this works, but I don't care anyway :)
  cl <- ret0$call
  cl$data <- datacall
  # cl$data <- data # NO!! output carries the articulation of `data` ..
  #ret <- eval(cl) # sometimes error!!
  ret <- eval(parse(text = deparse1(cl))) # this way it works ...
  attr(ret, which = 'old_terms') <- trms0
  return(ret)
  
}
