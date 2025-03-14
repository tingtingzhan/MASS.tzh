

#' @title Perform \link[MASS]{stepAIC} on Complete Data
#' 
#' @description 
#' Perform \link[MASS]{stepAIC} on complete data, and \link[stats]{update} the return with full data.
#' 
#' @param object a regression model
#' 
#' @param lower \link[stats]{formula} of the lower `scope` (see function \link[MASS]{stepAIC}),
#' default `~1` indicating an intercept-only minimum model in a **downward** selection.
#' 
#' @param upper \link[stats]{formula} of **additional** predictors of the upper `scope` (see function \link[MASS]{stepAIC})
#' of a **forward** selection.
#' If parameter `upper` is provided, then parameter `lower` is ignored.
#' 
#' @param ... additional parameters not in use
#' 
#' @details
#' Read section **Note** of function \link[MASS]{stepAIC} documentation.
#' 
#' 
#' @note
#' \link[MASS]{stepAIC} (as of 2025-02-19) ??? does not have a parameter for the end user to specify the
#' threshold of AIC improvement, that should be regarded as an improvement.
#' Instead, this quantity is hard coded (in `if (bAIC >= AIC + 1e-07) break`).
#' 
#' @examples 
#' sapply(airquality, FUN = function(i) mean(is.na(i))) # missingness in `Ozone` and `Solar.R`
#' summary(m <- lm(Temp ~ Ozone + Solar.R + Wind, data = airquality))
#' tryCatch(m |> stepAIC(trace = FALSE), error = identity)
#' m |> stepAIC_complete() |> summary()
#' plot(Temp ~ Ozone, data = airquality)
#' 
#' @importFrom MASS stepAIC
#' @importFrom stats complete.cases terms update update.formula
#' @export
stepAIC_complete <- function(
    object,
    lower = ~ 1,
    upper,
    ...
) {
  
  #message('global stepAIC_complete') # dev-mode
  
  old_trm <- object |> terms()
  old_v <- old_trm |> attr(which = 'variables', exact = TRUE) |> as.list.default()
  if (length(old_v) == 2L) return(object) # term variables `list(edp)`; input model contains at most an intercept term
  
  datacall <- object$call$data
  data <- eval(datacall) # let err
  
  # backward-only or backward-forward
  # yok <- complete.cases(data[intersect(names(data), all.vars(old_trm[[2L]]))]) # no longer used
  
  ########## backward-selection
  
  back_ok <- complete.cases(data[intersect(names(data), all.vars(old_trm))])
  backward <- object |> 
    update(data = data[back_ok, , drop = FALSE]) |>
    stepAIC(direction = 'backward', trace = 0L) |>
    update(data = data)
  backward$call$data <- datacall # silly but works!
  new_v <- backward |> terms() |> attr(which = 'variables', exact = TRUE) |> as.list.default()
  if (length(new_v) == 2L) cat('All variables removed by backward selection algorithm!\n')
  
  if (!missing(upper)) {
    
    ########## forward-selection
    
    upper_scope <- backward |>
      terms() |> 
      update.formula(new = call(name = '~', quote(.), call(name = '+', upper[[2L]], quote(.))))
    
    forw_ok <- complete.cases(data[intersect(names(data), all.vars(upper_scope))])
    .forw_data <- data[forw_ok, , drop = FALSE]
    
    # stepAIC(direction = 'forward') -> ?stats::add1
    # requires `data` in `.GlobalEnv` !!  Even ?base::new.env does not work!!
    # `assignments to the global environment` is a NOTE of R check
    if (exists('.forw_data', envir = .GlobalEnv)) stop('remove existing `.forw_data` from .GlobalEnv')
    assign(x = '.forw_data', value = .forw_data, envir = .GlobalEnv)
    forward <- backward |> 
      update(data = .forw_data) |> # I dont care about `$call` in intermediate step(s)
      stepAIC(direction = 'forward', scope = list(upper = upper_scope), trace = 0L) |>
      update(data = data)
    rm(list = '.forw_data', envir = .GlobalEnv)
    forward$call$data <- datacall # silly but works!
    
    #env <- new.env()
    #assign(x = '.forw_data', value = .forw_data, envir = env)
    #cl <- quote(expr = {
    #  backward |> 
    #  update(data = .forw_data) |>
    #  stepAIC(direction = 'forward', scope = list(upper = upper_scope), trace = 0L)
    #})
    #eval(cl, envir = env) # does not work!!!
    #rm(env)
    
  } # else do nothing

  # attr(ret1, which = 'old_terms') <- old_trm # deprecated!!!!!
  ret <- list(
    Initial = object,
    'Backward Stepwise' = backward,
    'Forward Stepwise' = if (!missing(upper)) forward # else NULL
  )
  ret <- ret[lengths(ret, use.names = FALSE) > 0L]
  attr(ret, which = 'lower') <- lower
  attr(ret, which = 'upper') <- if (!missing(upper)) upper # else NULL
  class(ret) <- c('stepAIC', 'listof')
  return(ret)
  
}




#' @title .Sprintf.stepAIC
#' 
#' @param x returned object of [stepAIC_complete()]
#' 
#' @note
#' Do not make this S3!  This is to be appended to `Sprintf.default()`
#' 
#' @export
.Sprintf.stepAIC <- function(x) {
  sprintf(fmt = '%s stepwise variable selection by Akaike information criterion (AIC) is performed using <u>**`R`**</u> package <u>**`MASS`**</u>.',
          if (length(upper <- attr(x, which = 'upper', exact = TRUE))) {
            'Backward-forward'
          } else 'Backward')
}

