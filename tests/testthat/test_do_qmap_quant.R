context("Do QUANT Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:30,2]
modv = modprecip[1:30,2]

quant.fit = fitQmapQUANT(obsv, modv, wet.day=F, qstep = 1/10)
quant.mat.fit = fitQmapQUANT(obsprecip[1:30,], modprecip[1:30,], qstep = 1/10)

test_that("numerical accuracy", {
  expect_equal(doQmapQUANT(modv, quant.fit)[24], 6.138884, tolerance=1e-7)
})

test_that("matrix/data.frame do", {
  expect_named(doQmapQUANT(modprecip[1:30,], quant.mat.fit))
})

test_that("approx types", {
  expect_length(doQmapQUANT(modv, quant.fit, type="tricub"), 30)
})


test_that("wrong class", {
  expect_error(doQmapQUANT(modv, "badarg"), "class\\(fobj\\) should be fitQmapQUANT")
})