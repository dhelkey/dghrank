context('Continious Valued Null Simulations')




test_that('simulateObs works',{
  a = simulateObs(10, 2, 10, 10)
  expect_equal(dim(a), c(10,10))
  expect_true( round(mean(a)) == 10)
  expect_true( round(sd(a)) == 2)
})


##Make sure that simulating from the emperical distribution works as expected
data('neonatal')
e = neonatal$outcome_cont
e[is.na(e)] = 0
d = simulateObs(10, 4, 10, 10, e)
hist(d)

