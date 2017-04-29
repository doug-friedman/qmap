context("PTF Method")

data(obsprecip)
data(modprecip)
qm.fit = fitQmapPTF.default(obsprecip[,2], modprecip[,2], wet.day=F, qstep = 1/10, transfun="linear")

linear_tfun = function(x,a,b){x <- a+b*x}

test_that("chosen transfer function was applied", {
  expect_equal(qm.fit$tfun, linear_tfun)
})