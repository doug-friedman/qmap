context("Do RQUANT Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:30,2]
modv = modprecip[1:30,2]

rquant.fit = fitQmapRQUANT(obsv, modv, wet.day=F, qstep = 1/10, nboot=1)
rquant.mat.fit = fitQmapRQUANT(obsprecip[1:30,], modprecip[1:30,], qstep = 1/10, nboot=1)

test_that("numerical accuracy", {
  expect_equal(doQmapRQUANT(modv, rquant.fit)[2], 0.1321339, tolerance=1e-7)
})

test_that("matrix/data.frame do", {
  expect_named(doQmapRQUANT(modprecip[1:30,], rquant.mat.fit))
})

test_that("approx types", {
  expect_length(doQmapRQUANT(modv, rquant.fit, type="linear2"), 30)
  expect_length(doQmapRQUANT(modv, rquant.fit, type="tricub"), 30)
})


test_that("wrong class", {
  expect_error(doQmapRQUANT(modv, "badarg"), "class\\(fobj\\) should be fitQmapRQUANT")
})