library(MASS)
mod_object <- ridgereg(Petal.Length~Species, data = iris,lambda = 1)
model_object <- lm.ridge(Petal.Length~Species, data = iris,lambda=1)
test_that("ridgereg works", {
  expect_true(abs(coef(model_object)[1]-coef(mod_object)[1])<0.2)
})
test_that("ridgereg works", {
  expect_true(abs(coef(model_object)[2]-coef(mod_object)[2])<0.2)
})
test_that("ridgereg works", {
  expect_true(abs(coef(model_object)[3]-coef(mod_object)[3])<0.2)
})