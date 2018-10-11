context('ParseMinimalData')

#Extract data
data("neonatal")

#Run it through a data set with NA values
minimal_data = neonatal[ ,  c('outcome1', 'instid', 'male', 'outborn', 'csect','ap5','gaweeks')]
minimal_data$male[70] = NA
minimal_data$csect[c(42, 419)] = NA #Introduce NA valeus
num_cat = 3
num_cont = 2
N_full = dim(minimal_data)[1]

#Smaller versions to test
minimal_data_cat = minimal_data[ ,c('outcome1', 'instid', 'male')]
minimal_data_cont = minimal_data[ ,c('outcome1', 'instid', 'ap5')]
minimal_data_none = minimal_data[ ,c('outcome1', 'instid')]


#Count number of NAs
na_count_vec = sapply(colnames(minimal_data), function(x) sum(is.na(minimal_data[[x]])))
na_outcomes = na_count_vec['outcome1']
na_cat = sum(na_count_vec[c('male','outborn','csect')])
na_cont = sum(na_count_vec[c('ap5','gaweeks')])


minimal_data_clean = minimal_data[complete.cases(minimal_data), ]

pm = function(x = minimal_data, ...){
  #Wrapper for parseMinimalData to make testing easier
  #Set to remove no NA values
  parseMinimalData(x, num_cat, num_cont, ...)
}

test_that('The options to remove NA values work',{
  expect_equal( pm()$N, N_full)
  expect_equivalent( pm(outcome.na = 'remove')$N, N_full - na_outcomes)
  expect_equivalent( pm(cat.na = 'remove')$N, N_full - na_cat)
  expect_equivalent( pm( cont.na = 'remove')$N, N_full - na_cont)
  expect_equivalent( pm(cat.na = 'remove', cont.na = 'remove')$N, N_full - na_cat - na_cont)
  expect_equivalent( pm(outcome.na = 'remove',
                        cat.na = 'remove', cont.na = 'remove')$N,
                     N_full - sum(!complete.cases(minimal_data)))
  expect_equivalent(sum(pm(outcome.na = 'set0')$y == 0),
                    sum(pm(outcome.na = 'remove')$y == 0) + na_outcomes)
  expect_equivalent( sum(pm(cat.na = 'category')$cat_var_mat$male == '99'), 1)
})

test_that('parseMinimalData should never* NA values',{ #with the exception of 'NA' category for categorical variables, if desired
  expect_equal(sum(is.na(pm()$y)), 0)
  expect_equal(sum(is.na(pm()$cont_var_mat)), 0)
  expect_equal(sum(is.na(pm()$cat_var_mat)), 0)
  expect_equal(sum(is.na(pm(outcome.na = 'remove')$y)), 0)
  expect_equal(sum(is.na(pm(cat.na = 'remove')$cat_var_mat)), 0)
  expect_equal(sum(is.na(pm(cont.na = 'remove')$cont_var_mat)), 0)
})

test_that('parseMinimalData returns the right values',{
  expect_match( pm()$indicator_name, 'outcome1')
  expect_equivalent( dim(pm()$cont_var_mat)[1], dim(pm()$cat_var_mat)[1])
  expect_equivalent( dim(pm(cont.na = 'remove')$cont_var_mat)[1], dim(pm(cont.na = 'remove')$cat_var_mat)[1])
  expect_equivalent( dim(pm(cat.na = 'remove')$cont_var_mat)[1], dim(pm(cat.na = 'remove')$cat_var_mat)[1])
})

test_that('y, inst_vec, cat_var_mat, and cont_var_mat all have the same length if they all exist',{
  lengthFun = function(r){
    var(c( dim(r$cont_var_mat)[1], dim(r$cat_var_mat)[1], length(r$y)))
  }
  expect_equal(lengthFun( pm()), 0)
  expect_equal(lengthFun( pm(outcome.na = 'remove')), 0)
  expect_equal(lengthFun( pm(cat.na = 'remove')), 0)
  expect_equal(lengthFun( pm(cont.na = 'remove')), 0)
  expect_equal(lengthFun( pm(minimal_data_clean, cont.na = 'remove')), 0)
})

p = function(minimal_data, num_cat, num_cont,...){
  parseMinimalData(minimal_data, num_cat, num_cont, verbose = FALSE,...)
}

test_that('No covariates',{
  r = p(minimal_data_none, 0, 0)
  expect_equal( length(r$y), N_full)
  expect_equal( r$cat_var_mat, NULL)
  expect_equal( r$cont_var_mat, NULL)
  expect_equal( mean(p(minimal_data_none, 0, 0, outcome.na = 'remove')$y),
                mean(neonatal$outcome1, na.rm =TRUE))
  expect_true(var(r$y)> 0)
})

test_that('Only 1 categorical covariate',{
  r = p(minimal_data_cat, 1, 0)
  expect_equal( dim(r$cat_var_mat), c(N_full, 1))
  expect_equal( length(levels(r$cat_var_mat$male)), 3)
  expect_equal( length(r$y), dim(r$cat_var_mat)[1])

  r2 = p(minimal_data_cat,1,0, cat.na = 'remove')
  expect_equal( length(levels(r2$cat_var_mat$male)), 2)
  expect_equal(sum(r$cat_var_mat$male == 0),sum(r$cat_var_mat$male == 0) )
  expect_equal( length(r$y), length(r2$y) + 1)
  expect_equal( length(r2$y), dim(r2$cat_var_mat)[1])
})
