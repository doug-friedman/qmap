context("Fit DIST Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:50,2]
modv = modprecip[1:50,2]


## Numeric Accuracy
DIST.fit = fitQmapDIST(obsv, modv)


test_that("fitted values", {
  expect_equal(unname(DIST.fit$par[1,1]), 0.4599685, tolerance=1e-7)
})

test_that("different objects", {
  expect_named(fitQmapDIST(obsprecip[1:50,], modprecip[1:50,]))
})


## General Tests
test_that("class of fitQmapDIST", {
  expect_is(DIST.fit, "fitQmapDIST")
})


## Argument Variations
test_that("different distributions", {
  expect_named(fitQmapDIST(obsv, modv, distr="berngamma"))
})


test_that("unequal length obs/mod vectors", {
  expect_named(fitQmapDIST(obsv, modv[1:5], wet.day=F))
})

test_that("wet day options", {
  expect_named(fitQmapDIST(obsv, modv, wet.day=T))
  expect_named(fitQmapDIST(obsv, modv, wet.day=10))
})

test_that("bootstrap options", {
  expect_named(fitQmapDIST(obsv, modv, nboot=3))
})


## Warnings and Errors
test_that("quantile step invalid", {
  expect_error(fitQmapDIST(obsv, modv, qstep=-1), 
               "'qstep' should be NULL or in the 'qstep < 1 & qstep > 0' interval")
})

