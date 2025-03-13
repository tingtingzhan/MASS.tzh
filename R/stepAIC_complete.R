

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
  
  # always perform backward-selection
  
  back_ok <- complete.cases(data[intersect(names(data), all.vars(old_trm))])
  back <- object |> 
    update(data = data[back_ok, , drop = FALSE]) |>
    stepAIC(direction = 'backward', trace = 0L)
  
  if (missing(upper)) {
    
    ret0 <- back 
    
  } else { # forward selection
    
    upper_scope <- back |>
      terms() |> 
      update.formula(new = call(name = '~', quote(.), call(name = '+', upper[[2L]], quote(.))))
    
    forw_ok <- complete.cases(data[intersect(names(data), all.vars(upper_scope))])
    .forw_data <- data[forw_ok, , drop = FALSE]
    
    # stepAIC(direction = 'forward') -> ?stats::add1
    # requires `data` in `.GlobalEnv` !!  Even ?base::new.env does not work!!
    # `assignments to the global environment` is a NOTE of R check
    if (exists('.forw_data', envir = .GlobalEnv)) stop('remove existing `.forw_data` from .GlobalEnv')
    assign(x = '.forw_data', value = .forw_data, envir = .GlobalEnv)
    ret0 <- back |> 
      update(data = .forw_data) |> # I dont care about `$call` in intermediate step(s)
      stepAIC(direction = 'forward', scope = list(upper = upper_scope), trace = 0L)
    rm(list = '.forw_data', envir = .GlobalEnv)
    
    #env <- new.env()
    #assign(x = '.forw_data', value = .forw_data, envir = env)
    #cl <- quote(expr = {
    #  back |> 
    #  update(data = .forw_data) |> # I dont care about `$call` in intermediate step(s)
    #  stepAIC(direction = 'forward', scope = list(upper = upper_scope), trace = 0L)
    #})
    #ret0 <- eval(cl, envir = env) # does not work!!!
    #rm(env)
    
  }
  
  ret <- ret0 |>
    update(data = data)
  ret$call$data <- datacall # silly but works!
  
  new_v <- ret |> terms() |> attr(which = 'variables', exact = TRUE) |> as.list.default()
  if (length(new_v) == 2L) cat('All variables removed by backward selection algorithm!\n')
  
  attr(ret, which = 'old_terms') <- old_trm
  attr(ret, which = 'lower') <- lower
  attr(ret, which = 'upper') <- if (!missing(upper)) upper # else NULL
  attr(ret, which = 'model.start') <- object
  attr(ret, which = 'model.backward') <- if (!missing(upper)) back # else NULL
  class(ret) <- c('stepAIC', class(ret))
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
  
  old_lab_ <- x |> attr(which = 'old_terms', exact = TRUE) |> attr(which = 'term.labels', exact = TRUE)
  old_lab <- paste0('`', old_lab_, '`', collapse = ', ')
  
  if (length(upper <- attr(x, which = 'upper', exact = TRUE))) {
    return(sprintf(
      fmt = 'Forward-backward stepwise variable selection by Akaike information criterion (AIC) is performed using <u>**`R`**</u> package <u>**`MASS`**</u>. Initial model starts with predictor(s) %s, backward selection first, then forward selection with additional predictor(s) %s.',
      old_lab,
      paste0('`', all.vars(upper[[2L]]), '`', collapse = ', ')
    ))
  } else {
    return(sprintf(
      fmt = 'Backward stepwise variable selection by Akaike information criterion (AIC) is performed using <u>**`R`**</u> package <u>**`MASS`**</u>, from candidate predictor(s) %s.',
      old_lab
    ))
  }
  
}

