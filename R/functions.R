### ------------------------

#' Calculate multiplicative effect from regression table output
#'
#' @description Calculate multiplicative effect from glmmTMB output including measured single pollutant effects and control treatment.
#' @param model regression model output (glmmTMB) or regression table/ regression coefficients in model$fit$par form
#' @param est.pos row number of estimates in regression table for single pollutant effects
#' @param link link used in model: one of "logit", "log", or "identity"; default = "logit"
#' @param control position of the control treatment parameter (row number in the regression table or position in regression coefficient vector)
#' 
#' @return a list containing
#' the calculated proportions, 
#' the expected multiplicative effect and

multiplicative <- function(model, est.pos, link = "logit", control = 1) {
  
  #prepare empty output list:
  out <- list(
    proportions = rep(NA, length(est.pos)),
    multiplicative.effect = NA
  )
  
  # rename and prepare input
  estimates <- model$fit$par
  k = 1
  
  #calculate proportion alive for all single pollutants and write to output 
  if(link == "logit")
    for(i in est.pos){
       out$proportions[k] <- 1 - plogis(estimates[control] + estimates[i])
       k = k + 1
    }
  if(link == "log")
    for(i in est.pos){
      out$proportions[k] <- exp(estimates[control] + estimates[i])/exp(estimates[control])
      k = k + 1
    }
  if(link == "identity") 
    for(i in est.pos){
      out$proportions[k] <- (estimates[control] + estimates[i])/(estimates[control])
      k = k + 1
    }
  
  # calculate multiplicative effects (expected proportional effect relative to control according to multiplicative model) and write to output:
  
  out$multiplicative.effect = prod(out$proportions) 
  
  if(mean(out$proportions <= 1) != 1)  
  stop("Attention: Not all effects are negative. Multiplicative null model prediction might be wrong!")

  print(out)
}


### ------------------------

#' Calculate simple additive effect from regression table output
#'
#' @description Calculate simple additive effect from glmmTMB output including measured single pollutant effects and control treatment. 
#' @param model regression model output (glmmTMB) or regression table/ regression coefficients in model$fit$par form
#' @param est.pos row number of estimates in regression table for single pollutant effects
#' @param link link used in model: one of "logit", "log", or "identity"; default = "identity"
#' @param control position of the control treatment parameter (row number in the regression table or position in regression coefficient vector)
#' 
#' @return list with the expected additive effect 


additive <- function(model, est.pos, link = "identity", control = 1) {
  
  #prepare empty output list:
  out <- list(
    additive.effect = NA
  )
  
  # rename and prepare input
  estimates <- model$fit$par

  #calculate additive effect 
  if(link == "identity"){
    predicted = c(estimates[control], estimates[control] + estimates[est.pos])
    pred.diff = predicted[-1] - predicted[1]
    out$additive.effect <- predicted[1] + sum(pred.diff)
  }
  if(link == "log"){
    predicted = exp(c(estimates[control], estimates[control] + estimates[est.pos]))
    pred.diff = predicted[-1] - predicted[1]
    out$additive.effect <- predicted[control] + sum(pred.diff)
  }
  if(link == "logit"){
    predicted = plogis(c(estimates[control], estimates[control] + estimates[est.pos]))
    pred.diff = predicted[-1] - predicted[1]
    out$additive.effect <- predicted[1] + sum(pred.diff)
  }
  
  print(out)
}






