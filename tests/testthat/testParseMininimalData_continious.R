context('parseMinimalData')

#parseMinimalData will work on continious outcomes just fine :)
data('neonatal')
minimal_data = neonatal[ ,c('outcome_cont', 'instid', 'male', 'outborn', 'csect', 'cont1', 'cont2')]
y = minimal_data[ ,1]



pm = function(...){ #Wrapper to easily run
  parseMinimalData(minimal_data, 3, 2, ...)
}

test_that('returns the values we expect for a continious variable', {
  #Set Na or NaN values to 0
  y_set0 = y
  y_set0[!complete.cases(y)] = 0
  expect_equivalent(pm()$y, y_set0)
  expect_equivalent( length(pm()$y), length(minimal_data[ ,1]))

  #Instead, remove Na or NaN values
  expect_true( length(pm()$y) > length(pm(outcome.na = 'remove')))
  expect_equivalent(pm(outcome.na = 'remove')$y, y[complete.cases(y)])
})

