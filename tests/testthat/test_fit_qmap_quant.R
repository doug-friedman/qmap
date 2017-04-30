context("Fit QUANT Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:50,2]
modv = modprecip[1:50,2]


## Numeric Accuracy
quant.fit = fitQmapQUANT(obsv, modv)


test_that("coeffcients of power transfer functions", {
  expect_equal(quant.fit$par$modq[1,1], 10.55, tolerance=1e-7)
  expect_equal(quant.fit$par$fitq[1,1], 0.1,  tolerance=1e-7)
})

test_that("different objects", {
  expect_named(fitQmapQUANT(obsprecip[1:10,], modprecip[1:10,]))
})


## General Tests
test_that("class of fitQmapQUANT", {
  expect_is(quant.fit, "fitQmapQUANT")
})


## Argument Variations
test_that("unequal length obs/mod vectors", {
  expect_named(fitQmapQUANT(obsv, modv[1:5], wet.day=F))
})

test_that("wet day options", {
  expect_named(fitQmapQUANT.default(obsv, modv, wet.day=T))
  expect_named(fitQmapQUANT.default(obsv, modv, wet.day=10))
})

test_that("bootstrap options", {
  expect_named(fitQmapQUANT(obsv, modv, nboot=3))
})


## Warnings and Errors
test_that("quantile step invalid", {
  expect_error(fitQmapQUANT(obsv, modv, qstep=-1), 
               "'qstep' should be NULL or in the 'qstep < 1 & qstep > 0' interval")
})

test_that("wet day invalid", {
  expect_error(fitQmapQUANT(obsprecip[,2], modprecip[,2], wet.day="bad"), 
               "'wet.day' should be 'numeric' or 'logical'")
})

