context("Do PTF Method")

data(obsprecip)
data(modprecip)

obsv = obsprecip[1:10,2]
modv = modprecip[1:10,2]

pwr.fit = fitQmapPTF(obsv, modv, wet.day=F, qstep = 1/10, transfun="power")
pwr.mat.fit = fitQmapPTF(obsprecip[1:10,], modprecip[1:10,], wet.day=F, qstep = 1/10, transfun="power")

test_that("numerical accuracy", {
  expect_equal(doQmapPTF(modv, pwr.fit)[2], 0.0005317149, tolerance=1e-7)
})

test_that("matrix/data.frame do", {
  expect_named(doQmapPTF(modprecip[1:10,], pwr.mat.fit))
})

test_that("wrong class", {
  expect_error(doQmapPTF(modv, "badarg"), "class\\(fobj\\) should be fitQmapPTF")
})