context('partitionSummary')

#Set up data
data("neonatal")
minimal_data = neonatal[ ,c('outcome1', 'instid','subset', 'male', 'ap5')]
r = processMinimalData(minimal_data, 1,1, subset=TRUE)

test_that('Verify results w/ single partitioning vector',{
  s = partitionSummary(r$y, r$inst_vec)
  expect_true(all(c('levels_out', 'n_vec','o_mean','ind_mat') %in% names(s)))
  expect_error(partitionSummary(r$y, factor(c(1,2))))
  #Should match manual computation of mean and n
  expect_equivalent( s$o_mean, sapply( levels(r$inst_vec), function(i) mean(r$y[r$inst_vec == i])))
  expect_equivalent( s$n_vec, sapply( levels(r$inst_vec), function(i) sum(r$inst_vec == i)))

  #Now check subset
  s = partitionSummary(r$y, r$subset_vec)
  expect_equivalent( s$o_mean,  sapply( levels(r$subset_vec), function(i) mean(r$y[r$subset_vec == i])))
  expect_equivalent( s$n,  sapply( levels(r$subset_vec), function(i) sum(r$subset_vec == i)))
})

test_that('Verify results w/ multiple partitioning vectors',{
  s = partitionSummary(r$y, r$inst_vec, r$subset_vec)
  expect_equal(dim(s$part_mat)[2] , 2)

  #The indexing is a little hard for inst-subset, so just loop over all combinations
  # and discard the combinations that don't exist in the data
  bool_vec = NULL #Append values to this vector with c(), all elements should be TRUE
  for (i in levels(r$inst_vec)){
    for(sub in levels(r$subset_vec)){
      indices = r$inst_vec == i & r$subset_vec==sub
      if(sum(indices) > 0)
        bool_vec = c(bool_vec,as.numeric(s$o_mean[s$part_mat[ ,1] == i & s$part_mat[ ,2] == sub]) ==
                       as.numeric(mean(r$y[r$inst_vec == i & r$subset_vec == sub])),
                     as.numeric(s$n_vec[s$part_mat[ ,1] == i & s$part_mat[ ,2] == sub]) ==
                       as.numeric(sum(r$inst_vec == i & r$subset_vec == sub)))
    }
  }
  expect_true(all(bool_vec))
})

test_that('indicator matrix works as expected',{

  #Fit the three diferent kinds of partitions
  partition_list = list(
    partitionSummary(r$y, r$inst_vec),
    partitionSummary(r$y, r$subset_vec),
    partitionSummary(r$y, r$inst_vec, r$subset_vec)
  )

  #Simple linear algebra for obtaining n_vec and o_vec :)
  for (s in partition_list){
    n_vec = apply(s$ind_mat, 2, sum)
    expect_equivalent( n_vec, as.numeric(s$n_vec))
    expect_equivalent( as.numeric(t(as.matrix(s$ind_mat)) %*% r$y / n_vec), s$o_mean)
  }
})

test_that('null values return as expected', {
  expect_equal( partitionSummary(r$y, NULL), NULL)
})




