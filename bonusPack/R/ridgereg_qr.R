
#' @author Hamed and Omid
#' @description ridgereg by qr decomposition
#' @example model_object_qr <- ridgereg_qr(Petal.Length~Species, data = iris, lambda = 1)
#' @export ridgereg_qr
#' @name ridgereg_qr
#' @param formula the formula 
#' @param data the data
#' @param lambda the lambda
#' @return obj
#' @title ridgereg_qr


ridgereg_qr <- function(formula,data,lambda){
  X <- model.matrix(formula,data)
  Y <- as.matrix(data[all.vars(formula)[1]])
  nr = nrow(X)
  ncx = ncol(X)
  lambda_i <- diag(lambda, nr)
  
  XXl = crossprod(X) + diag(nr*lambda,ncx,ncx)
  QR = qr(XXl)
  XY = crossprod(X,Y)
  
  bhat = qr.coef(QR,XY) 
  xx <- t(bhat)
  row.names(xx) <- ''
  colnames(xx) <- c('', 'Speciesversicolor' , 'Speciesvirginica')
  xx
  
  Yhat = X %*% bhat 
  yy <- t(Yhat)
  rownames(yy) <- ''
  yy
  
  obj <- list('beta_hat' = xx,
              'y_hat' = yy,
              call = match.call())
  
  class(obj) <- 'ridgereg_qr'
  
  return(obj)
}

model_object_qr <- ridgereg_qr(Petal.Length~Species, data = iris, lambda = 1)

print.ridgereg_qr <- function(obj){
  cat("Call:\n")
  print(obj$call)
  cat("\nCoefficients:\n")
  print(obj$beta_hat)
}

print(model_object_qr)


predict.ridgereg_qr <- function(obj){
  print(obj$y_hat)
}

predict(model_object_qr)

coef.ridgereg_qr <- function(obj){
  print(obj$beta_hat)
}

coef(model_object_qr)

