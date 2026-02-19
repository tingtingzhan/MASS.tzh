

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
#' Function \link[MASS]{stepAIC} (as of 2025-02-19) ??? does not have a parameter for the end user to specify the
#' threshold of AIC improvement, that should be regarded as an improvement.
#' Instead, this quantity is hard coded (in `if (bAIC >= AIC + 1e-07) break`).
#' 
#' Function \link[MASS]{stepAIC} (as of 2025-02-19) is hard coded for S3 regression object (e.g., `object$formula <- Terms`).
#' 
#' @examples 
#' library(MASS)
#' airquality |> sapply(FUN = \(i) mean(is.na(i))) # missingness in `Ozone` and `Solar.R`
#' summary(m <- lm(Temp ~ Ozone + Solar.R + Wind, data = airquality))
#' tryCatch(m |> stepAIC(trace = FALSE), error = identity)
#' m |> stepAIC_complete()
#' lm(Temp ~ Solar.R + Wind, data = airquality) |>
#'  stepAIC_complete(upper = ~ Ozone) # not necessarily the same!
#'  
#' library(lme4.tzh); library(HSAUR3)
#' ?lme4::glmer
#' gm2 = glmer(outcome ~ treatment*visit + (1|patientID), data=toenail, family=binomial,nAGQ=20)
#' gm2 |> stepAIC_complete()
#' 
#' library(ecip); list(
#'  'lm' = m |> stepAIC_complete(),
#'  'glmer' = gm2 |> stepAIC_complete()
#' ) |> fastmd::render2html()
#' @keywords internal
#' @export
stepAIC_complete <- function(
    object,
    lower = ~ 1,
    upper,
    ...
) {
  
  trm <- object |> terms()
  old_v <- trm |> attr(which = 'variables', exact = TRUE) |> as.list.default()
  if (length(old_v) == 2L) return(object) # term variables `list(edp)`; input model contains at most an intercept term
  
  cl <- getElement(object, name = 'call') # `object` could be S4
  datacall <- cl$data
  data <- eval(datacall) # let err
  
  ########## backward-selection
  
  back_ok <- complete.cases(data[intersect(names(data), all.vars(trm))])
  # some S4 regression model requires `data` in `.GlobalEnv`
  .back_data <- data[back_ok, , drop = FALSE]
  if (exists('.back_data', envir = .GlobalEnv)) stop('remove existing `.back_data` from .GlobalEnv')
  assign(x = '.back_data', value = .back_data, envir = .GlobalEnv)
  backward <- object |> 
    update(data = .back_data) |>
    ifelse(isS4(object), yes = stepAIC4, no = stepAIC)(direction = 'backward', trace = 0L) |>
    update(data = data)
  rm(list = '.back_data', envir = .GlobalEnv)
  # end of dealing with `.GlobalEnv`
  if (isS4(backward)) {
    backward@call$data <- datacall
  } else backward$call$data <- datacall # silly but works!
  new_v <- backward |> terms() |> attr(which = 'variables', exact = TRUE) |> as.list.default()
  if (length(new_v) == 2L) cat('All variables removed by backward selection algorithm!\n')
  
  if (!missing(upper)) { # not tested on S4 `object`, but should be pretty ready
    
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
      ifelse(isS4(object), yes = stepAIC4, no = stepAIC)(direction = 'forward', scope = list(upper = upper_scope), trace = 0L) |>
      update(data = data)
    rm(list = '.forw_data', envir = .GlobalEnv)
    if (isS4(forward)) {
      forward@call$data <- datacall
    } else forward$call$data <- datacall # silly but works!
    
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





#' @title Convert \link[MASS]{stepAIC} Return to \link[base]{matrix}
#' 
#' @description
#' S3 method dispatch for S3 generic \link[base]{as.matrix}.
#' 
#' @param x returned object from function \link[MASS]{stepAIC}, or \link[MASS.tzh]{stepAIC_complete}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom flextable as_flextable color add_footer_lines
#' @importFrom ecip ecip as.matrix.ecip
#' @export as.matrix.stepAIC
#' @export
as.matrix.stepAIC <- function(x, ...) {
  
  # '\u274c' # unicode 'Cross Mark'
  # '\U1f6ab' # unicode 'No Entry Sign'
  
  nx <- length(x)
  
  z <- x |> lapply(FUN = ecip)
  
  # all intermediate models, only print p-values
  id0 <- seq_len(nx-1L)
  m0 <- z[id0] |> lapply(FUN = as.matrix.ecip, type = 'p_only')
  
  # final model
  m <- z[[nx]] |> as.matrix.ecip(type = 'ncol1')
  
  r0 <- m0 |>
    lapply(FUN = rownames) |>
    unlist(use.names = FALSE) |>
    unique.default() # do *not* ?base::sort !!!
  
  out0 <- array(data = '\u274c', dim = c(length(r0), length(m0)), dimnames = list(r0, NULL))
  for (i in id0) {
    out0[match(x = rownames(m0[[i]]), table = r0), i] <- m0[[i]]
  }
  
  r1 <- setdiff(rownames(m), r0)
  
  # `out0`, augmented
  out0a <- if (length(r1)) { # final model has more predictors (e.g., stepAIC_complete(., upper = ~ a1 + a2))
    rbind(out0, array('\U1f6ab', dim = c(length(r1), ncol(out0)), dimnames = list(r1, NULL)))
  } else out0 # final model does *not* have more predictors
  
  out1 <- cbind(out0a, if (length(attr(x, which = 'upper', exact = TRUE))) '\U1f6ab' else '\u274c')
  out1[match(rownames(m), table = rownames(out1)), nx] <- m
  
  colnames(out1) <- c(m0, list(m)) |> vapply(FUN = colnames, FUN.VALUE = '', USE.NAMES = FALSE)
  colnames(out1) <- paste(
    paste0('(', names(x), ')'),
    colnames(out1), 
    sep = '\n')
  
  attr(out1, which = 'row.title') <- x[[1L]] |> endpoint() |> deparse1()
  attr(out1, which = 'hline_i') <- nrow(out0)
  attr(out1, which = 'vline_j') <- id0
  return(out1)
  
}





#' @title Turn \link[MASS.tzh]{stepAIC_complete} Return to \link[flextable]{flextable}
#' 
#' @param x returned value of function \link[MASS.tzh]{stepAIC_complete}
#' 
#' @param row.title,hline_i,vline_j ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom flextable as_flextable color
#' @importFrom fastmd as_flextable.matrix
#' @export as_flextable.stepAIC
#' @export
as_flextable.stepAIC <- function(
    x, 
    row.title = z |> attr(which = 'row.title', exact = TRUE),
    hline_i = z |> attr(which = 'hline_i', exact = TRUE),
    vline_j = z |> attr(which = 'vline_j', exact = TRUE), # needed for `color_j`
    ...
) {
  z <- as.matrix.stepAIC(x, ...)
  z |>
    as_flextable.matrix(
      row.title = row.title,
      hline_i = hline_i
    ) |>
    color(j = vline_j + 1L, color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      '\u274c: predictor(s) removed by stepwise algorithm.',
      '\U1f6ab: predictor(s) not considered in the model.'
    ))
}







#' @title R Markdown Lines for `stepAIC`
#' 
#' @param x,xnm,... ..
#' 
#' @keywords internal
#' @importFrom fastmd md_ md_flextable_
#' @importClassesFrom fastmd md_lines
#' @importFrom ecip md_regression_
#' @export md_.stepAIC
#' @export
md_.stepAIC <- function(x, xnm, ...) {
  
  z1 <- x[[length(x)]] |> 
    md_regression_()
  
  z2 <- x |>
    md_stepAIC_int() 
  
  z3 <- md_flextable_(xnm = xnm, ...)
  
  c(z1, z2, z3) # ?fastmd::c.md_lines
  
}




md_stepAIC_int <- \(x) {
  
  upper <- x |> attr(which = 'upper', exact = TRUE)
  
  tmp <- x[[1L]] |> 
    terms() |> 
    attr(which = 'variables', exact = TRUE) |> 
    as.list.default() |>
    vapply(FUN = deparse1, FUN.VALUE = '')
    
  z1 <- sprintf(
    fmt = 'The initial model started with predictors %s, then followed by a %s stepwise variable selection by [@Akaike74 information criterion (AIC)](https://en.wikipedia.org/wiki/Akaike_information_criterion) performed using <u>**`R`**</u> package <u>**`MASS`**</u>. ',
    paste0('`', tmp[-(1:2)], '`', collapse = ', '),
    if (length(upper)) {
      'backward-forward'
    } else 'backward'
  ) |> 
    new(Class = 'md_lines', bibentry = .akaike74())
  
  z2 <- if (length(upper)) {
    sprintf(
      fmt = 'Additional predictor(s) considered in the subsequent forward stepwise selection were %s.',
      paste0('`', all.vars(upper[[2L]]), '`', collapse = ', ')
    ) |>
      new(Class = 'md_lines')
  } # else MULL
  
  c(z1, z2) # fastmd::c.md_lines
  
}

