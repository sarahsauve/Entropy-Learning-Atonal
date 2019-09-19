variance.explained <- function(model, data){
  SStot <- sum((data-mean(data))^2)
  SSres <- sum(residuals(model)^2)
  n <- length(data)
  fit <- ((SStot-SSres)/n)/(SStot/n)
  fit
}
