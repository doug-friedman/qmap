context("PTF Method")

data(obsprecip)
data(modprecip)
qm.fit = fitQmapPTF.default(obsprecip[,2], modprecip[,2], wet.day=F, qstep = 1/10, transfun="linear")

test_that("coeffcients of transfer function", {
  expect_equal(qm.fit$par[1], -1.731044, tolerance=1e-7)
  expect_equal(qm.fit$par[2], 0.8447307, tolerance=1e-7)
})