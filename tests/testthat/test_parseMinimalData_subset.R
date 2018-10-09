context('ParseMinimalData:Subset')

#Extract data
data("neonatal")

#Run it through a data set with NA values
minimal_data = neonatal[ ,  c('outcome1', 'instid', 'subset', 'male', 'outborn', 'csect','ap5','gaweeks')]
minimal_data$male[70] = NA
minimal_data$csect[c(42, 419)] = NA #Introduce NA valeus
num_cat = 3
num_cont = 2
N_full = dim(minimal_data)[1]


pm = function(minimal_data, ...){
  processMinimalData(minimal_data, num_cat, num_cont, subset = TRUE,...)
}


test_that('subset works',{
  r = pm(minimal_data, num_cat, num_cont)
  expect_equal( length(r$inst_vec), length(r$subset_vec))
  expect_equal( length(r$inst_vec), N_full)
  expect_equal( length( levels(r$inst_vec)), 118)
  expect_equal( length( levels(r$subset)), 7)
})



test_that('missing values in subset are handled correctly', {
  r1 = pm(minimal_data, subset.na = 'category')
  r2 = pm(minimal_data, subset.na = 'remove')

  expect_equivalent( length(levels(r1$subset)), 7)
  expect_equal( length(levels(r2$subset)), 6)
})
