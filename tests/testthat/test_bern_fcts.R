context("Bern Distribution Functions")

data(obsprecip)
obs.tiles = quantile(obsprecip[1:30,1])
ts.be = startbernexp(obsprecip[1:30,1])
ts.bg = startberngamma(obsprecip[1:30,1])
ts.bn = startbernlnorm(obsprecip[1:30,1])
ts.bw = startbernweibull(obsprecip[1:30,1])


test_that("bernexp", {
  expect_equal(ts.be$prob, 0.5666667, tolerance=1e-7)
  expect_equal(ts.be$rate, 0.3030303, tolerance=1e-7)
  expect_equal(dbernexp(obs.tiles, prob=ts.be$prob, rate=ts.be$rate)[1],  0.433333333, tolerance=1e-7)
  ## TODO test qbernexp
  expect_equal(pbernexp(obs.tiles, prob=ts.be$prob, rate=ts.be$rate)[1],  0.433333333, tolerance=1e-7)
  expect_gte(rbernexp(obs.tiles, prob=ts.be$prob, rate=ts.be$rate)[1],  0)
})

test_that("berngamma", {
  expect_equal(ts.bg$prob, 0.5666667, tolerance=1e-7)
  expect_equal(ts.bg$scale, 5.070455, tolerance=1e-7)
  expect_equal(ts.bg$shape, 0.6508292, tolerance=1e-7)
  expect_equal(dberngamma(obs.tiles, prob=ts.bg$prob, scale=ts.bg$scale, shape=ts.bg$shape)[1],  0.433333333, tolerance=1e-7)
  ## TODO test qberngamma
  expect_equal(pberngamma(obs.tiles, prob=ts.bg$prob, scale=ts.bg$scale, shape=ts.bg$shape)[1],  0.433333333, tolerance=1e-7)
  expect_gte(rberngamma(obs.tiles, prob=ts.bg$prob, scale=ts.bg$scale, shape=ts.bg$shape)[1],  0)
})

test_that("bernlnorm", {
  expect_equal(ts.bn$prob, 0.5666667, tolerance=1e-7)
  expect_equal(ts.bn$meanlog, -0.03231185, tolerance=1e-7)
  expect_equal(ts.bn$sdlog, 1.882328, tolerance=1e-6)
  expect_equal(dbernlnorm(obs.tiles, prob=ts.bn$prob, meanlog=ts.bn$meanlog, sdlog=ts.bn$sdlog)[1],  0.433333333, tolerance=1e-7)
  ## TODO test qberngamma
  expect_equal(pbernlnorm(obs.tiles, prob=ts.bn$prob, meanlog=ts.bn$meanlog, sdlog=ts.bn$sdlog)[1],  0.433333333, tolerance=1e-7)
  expect_gte(rbernlnorm(obs.tiles, prob=ts.bn$prob, meanlog=ts.bn$meanlog, sdlog=ts.bn$sdlog)[1],  0)
})

test_that("bernweibull", {
  expect_equal(ts.bw$prob, 0.5666667, tolerance=1e-7)
  expect_equal(ts.bw$shape, 0.6375086, tolerance=1e-7)
  expect_equal(ts.bw$scale, 2.374842, tolerance=1e-7)
  expect_equal(dbernweibull(obs.tiles, prob=ts.bg$prob, scale=ts.bg$scale, shape=ts.bg$shape)[1],  0.433333333, tolerance=1e-7)
  ## TODO test qberngamma
  expect_equal(pbernweibull(obs.tiles, prob=ts.bg$prob, scale=ts.bg$scale, shape=ts.bg$shape)[1],  0.433333333, tolerance=1e-7)
  expect_gte(rbernweibull(obs.tiles, prob=ts.bg$prob, scale=ts.bg$scale, shape=ts.bg$shape)[1],  0)
})