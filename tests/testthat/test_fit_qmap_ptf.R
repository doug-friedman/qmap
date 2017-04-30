context("Fit PTF Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:10,2]
modv = modprecip[1:10,2]


## Different Fits
pwr.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="power")
lin.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="linear")
ex.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="expasympt")
sc.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="scale")
pwr0.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="power.x0")
#ex0.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="expasympt.x0")


test_that("coeffcients of power transfer functions", {
  expect_equal(pwr.fit$par[1], 0.0003932986, tolerance=1e-7)
  expect_equal(pwr.fit$par[2], 0.1264265, tolerance=1e-7)
})

test_that("coeffcients of linear transfer functions", {
  expect_equal(lin.fit$par[1], 0, tolerance=1e-7)
  expect_equal(lin.fit$par[2], 0, tolerance=1e-7)
})

test_that("coeffcients of exponential transfer functions", {
  expect_equal(ex.fit$par[1], 0, tolerance=1e-7)
  expect_equal(ex.fit$par[2], 0, tolerance=1e-7)
  expect_equal(ex.fit$par[3], 0, tolerance=1e-7)
})

test_that("coeffcients of scale transfer functions", {
  expect_equal(sc.fit$par[1], 0, tolerance=1e-7)
})

test_that("coeffcients of power0 transfer functions", {
  expect_equal(pwr0.fit$par[1], 1, tolerance=1e-7)
  expect_equal(pwr0.fit$par[2], 1, tolerance=1e-7)
  expect_equal(pwr0.fit$par[3], 0.1, tolerance=1e-7)
})

test_that("coeffcients of exponential0 transfer functions", {
  ## TODO
})

test_that("different objects", {
  expect_named(fitQmapPTF(obsprecip[1:10,], modprecip[1:10,]))
})



## General Tests
test_that("class of fitQmapPTF", {
  expect_is(lin.fit, "fitQmapPTF")
})


## Argument Variations
test_that("unequal length obs/mod vectors", {
  expect_named(fitQmapPTF(obsv, modv[1:5], wet.day=F))
})

test_that("wet day options", {
  expect_named(fitQmapPTF(obsv, modv, wet.day=T))
  expect_named(fitQmapPTF(obsv, modv, wet.day=10))
})

test_that("optim options", {
  expect_named(fitQmapPTF(obsv, modv, opar=list(method="L-BFGS-B")))
  expect_named(fitQmapPTF(obsv, modv, opar=list(method="BFGS")))
})

test_that("cost options", {
  expect_warning(fitQmapPTF(obsv, modv, cost="MAE"))
})


## Warnings and Errors
test_that("quantile step invalid", {
  expect_error(fitQmapPTF(obsv, modv, qstep=-1), 
               "'qstep' should be NULL or in the 'qstep < 1 & qstep > 0' interval")
})

test_that("wet day invalid", {
  expect_error(fitQmapPTF(obsv, modv, wet.day="bad"), 
               "'wet.day' should be 'numeric' or 'logical'")
})

test_that("optim warning", {
  expect_warning(fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="expasympt.x0"),
                 "optim 'method' Nelder-Mead failed. Optimizing with 'SANN'\npossibly unstable result")
          
})
