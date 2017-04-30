context("Do DIST Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:30,2]
modv = modprecip[1:30,2]

DIST.fit = fitQmapDIST(obsv, modv, wet.day=F, qstep = 1/10)
DIST.mat.fit = fitQmapDIST(obsprecip[1:30,], modprecip[1:30,], qstep = 1/10)

test_that("numerical accuracy", {
  expect_equal(doQmapDIST(modv, DIST.fit)[24], 7.252906, tolerance=1e-7)
})

test_that("matrix/data.frame do", {
  expect_named(doQmapDIST(modprecip[1:30,], DIST.mat.fit))
})

test_that("wrong class", {
  expect_error(doQmapDIST(modv, "badarg"), "class\\(fobj\\) should be fitQmapDIST")
})