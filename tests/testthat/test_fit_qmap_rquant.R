context("Fit RQUANT Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:50,2]
modv = modprecip[1:50,2]


## Numeric Accuracy
RQUANT.fit = fitQmapRQUANT(obsv, modv)


test_that("fitted values", {
  expect_equal(RQUANT.fit$par$modq[1,1], 10.55, tolerance=1e-7)
})

test_that("different objects", {
  expect_named(fitQmapRQUANT(obsv, modv))
})


## General Tests
test_that("class of fitQmapRQUANT", {
  expect_is(RQUANT.fit, "fitQmapRQUANT")
})


## Argument Variations
test_that("unequal length obs/mod vectors", {
  expect_named(fitQmapRQUANT(obsv, modv[1:5], wet.day=F))
})

test_that("wet day options", {
  expect_named(fitQmapRQUANT(obsv, modv, wet.day=T))
  expect_named(fitQmapRQUANT(obsv, modv, wet.day=10))
})

test_that("bootstrap options", {
  expect_named(fitQmapRQUANT(obsv, modv, nboot=3))
})


## Warnings and Errors
test_that("quantile step invalid", {
  expect_error(fitQmapRQUANT(obsv, modv, qstep=-1), 
               "'qstep' should be NULL or in the 'qstep < 1 & qstep > 0' interval")
})

test_that("wet day invalid", {
  expect_error(fitQmapRQUANT(obsprecip[,2], modprecip[,2], wet.day="bad"), 
               "'wet.day' should be 'numeric' or 'logical'")
})

