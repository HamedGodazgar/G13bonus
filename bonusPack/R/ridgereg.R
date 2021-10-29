
#' @author hamed and Omid
#' @description ridgereg
#' @example model_object <- ridgereg(Petal.Length~Species, data = iris, 1)
#' @export ridgereg
#' @name ridgereg
#' @param formula the formula 
#' @param data the data
#' @param lambda the lambda
#' @return obj
#' @title ridgereg

ridgereg <- function(formula,data,lambda){
  x <- model.matrix(formula,data)
  y <- as.matrix(data[all.vars(formula)[1]])
  n <- ncol(x)
  lambda_i <- diag(lambda, n)
  
  beta_hat <- solve(crossprod(x) + lambda_i) %*% t(x) %*% y
  
  y_hat <- x %*% beta_hat
  yy <- t(y_hat)
  rownames(yy) <- ''
  yy
  
  xx <- t(beta_hat)
  row.names(xx) <- ''
  colnames(xx) <- c('', 'Speciesversicolor' , 'Speciesvirginica')
  xx
  
  obj <- list('beta_hat' = xx,
              'y_hat' = yy,
              call = match.call())
  class(obj) <- 'ridgereg'
  return(obj)
}

model_object <- ridgereg(Petal.Length~Species, data = iris, 1)

print.ridgereg <- function(obj){
  cat("Call:\n")
  print(obj$call)
  cat("\nCoefficients:\n")
  print(obj$beta_hat)
}

print(model_object)


predict.ridgereg <- function(obj){
  print(obj$y_hat)
}

predict(model_object)

coef.ridgereg <- function(obj){
  print(obj$beta_hat)
}

coef(model_object)[1]
coef(model_object)[2]
coef(model_object)[3] 