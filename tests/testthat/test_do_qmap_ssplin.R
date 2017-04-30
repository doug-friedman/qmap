context("Do SSPLIN Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:30,2]
modv = modprecip[1:30,2]

ssplin.fit = fitQmapSSPLIN(obsv, modv, wet.day=F, qstep = 1/10)
ssplin.mat.fit = fitQmapSSPLIN(obsprecip[1:30,], modprecip[1:30,], qstep = 1/10)

test_that("numerical accuracy", {
  expect_equal(doQmapSSPLIN(modv, ssplin.fit)[1], -1.932272e-17, tolerance=1e-7)
})

test_that("matrix/data.frame do", {
  expect_named(doQmapSSPLIN(modprecip[1:30,], ssplin.mat.fit))
})

test_that("wrong class", {
  expect_error(doQmapSSPLIN(modv, "badarg"), "class\\(fobj\\) should be fitQmapSSPLIN")
})