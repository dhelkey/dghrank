context('null simulations')
##TODO redo these tests for null simulation purposees


data("neonatal")
minimal_data = neonatal[ ,c('outcome_cont', 'instid', 'male', 'outborn', 'csect', 'ap5')]
dat = parseMinimalData(minimal_data, 4, 0)
pcf_cat_vec = apply(dat$cat_var_mat, 1, paste, collapse = '-')
#


#TODO this is testing that:
##Test that that we get the right results for different values of gamma
# a = designBasedContNull(c(0.0, 0.1, 0.5, 0.9, 1),
#                         dat$y, pcf_cat_vec, dat$inst_vec )
#
# a1 = designBasedCont(0.0,dat$y, pcf_cat_vec, dat$inst_vec  )
# a2 = designBasedCont(0.1 ,dat$y, pcf_cat_vec, dat$inst_vec  )
# a3 = designBasedCont(0.5,dat$y, pcf_cat_vec, dat$inst_vec  )
# a4 = designBasedCont(0.9,dat$y, pcf_cat_vec, dat$inst_vec  )
# a5 = designBasedCont(1,dat$y, pcf_cat_vec, dat$inst_vec  )
#
#
# expect_equal(a[1, ], a1$Z)
# expect_equal(a[2, ], a2$Z)
# expect_equal(a[3, ], a3$Z)
# expect_equal(a[4, ], a4$Z)
# expect_equal(a[5, ], a5$Z)
#
#
#
context('generateNullData')
data('neonatal')

test_that('generateNull data generates expected mean and sd', {
  minimal_data = neonatal[ ,c('outcome_cont', 'instid', 'male', 'outborn', 'csect', 'ap5')]
  r = fitBabyMonitor(minimal_data, 4, 0)
  pcf_vec = apply(r$dat$cat_var_mat, 1, paste, collapse = '-')
  null_data = generateNullData(r$dat$y, pcf_vec, iters = 5000)
  pcf_levels = levels(as.factor(pcf_vec))
  m_vec = sapply(pcf_levels, function(l) mean(r$dat$y[pcf_vec == l]))
  s_vec = sapply(pcf_levels, function(l) sd(r$dat$y[pcf_vec == l]))
  s_vec[is.na(s_vec)] = 0
  o_m_vec = sapply(pcf_levels, function(l) mean(null_data$data[pcf_vec == l, ]))
  o_s_vec = sapply(pcf_levels, function(l) sd(null_data$data[pcf_vec == l, ]))


  #Test data generation
  expect_equal( length(null_data$n), length(null_data$m))
  expect_equal( length(null_data$n), length(null_data$s))
  expect_equivalent(m_vec,null_data$m)
  expect_equivalent(s_vec,null_data$s)
  expect_true( mean( m_vec - o_m_vec) < 0.005)
  expect_true( mean( s_vec - o_s_vec, na.rm = TRUE) < 0.001)
})
#
#
#
# #At the end of null simulations, save them (then run some tests on the matrix to make sure verything works) #(also check mean and variance of Z-scores...)
library(abind)
library(parallel)
#
#
# ##Test the simulating things in a block
minimal_data = neonatal[ ,c('outcome_cont', 'instid', 'male', 'outborn', 'csect', 'ap5')]
r = fitBabyMonitor(minimal_data, 4, 0, outcome.na = 'remove')
r2 = fitBabyMonitor(minimal_data, 4, 0)
#
# #Compare distribuitinos w/ and w/o data removed
par(mfrow = c(1,2))
hist(r$dat$y)
hist(r2$dat$y)
#
pcf_vec = apply(r$dat$cat_var_mat, 1, paste, collapse = '-')
null_data = generateNullData(r$dat$y, pcf_vec, iters = 6)
gamma_vec = seq(0.01, 0.99, length = 20)
l_out = computeNullBlock(null_data$data, pcf_vec, r$dat$inst_vec, gamma_vec)
#
#
#
# ###Set up for parallel computation
# n_cores = detectCores()
# iters_core = 15
# null_data_list = lapply(1:n_cores,
#                         function(i)
#                           generateNullData(r$dat$y, pcf_vec,
#                                         iters = iters_core)$data)
# cl = makeCluster(n_cores)
# clusterExport(cl, c('abind', 'designBasedContNull'))
# z_null_list = parLapply(cl,
#                         null_data_list,
#                       computeNullBlock,
#                       pcf_vec = pcf_vec,
#                       inst_vec = r$dat$inst_vec,
#                       gamma_vec = gamma_vec)
# stopCluster(cl)
#
# ##Bind the results together
# a = abind(z_null_list, along = 3)
#
#
# ##Now count percentage above and below z*
# z_star = 1.96
#
# calibFun = function(x, z_star = 1.96, mode = 'upper'){
#   #Mode = upper/lower
#
#   num = (x > z_star)
#   if (mode == 'lower'){
#     num = x < - z_star
#   }
#    return(sum(num) / length(x))
# }
#
# par(mfrow = c(1,1))
# plot(gamma_vec, sapply(1:length(gamma_vec), function(i) calibFun( a[i,,] )), type = 'o',
#      xlab = 'gamma', ylab = 'Percent outside of z = 1.96')
# points(gamma_vec, sapply(1:length(gamma_vec), function(i) calibFun( a[i,,] , mode = 'lower')),
#        col = 'blue')
# lines(gamma_vec, sapply(1:length(gamma_vec), function(i) calibFun( a[i,,] , mode = 'lower')),
#       col = 'blue')
# abline(h = 0.025)
# legend('topright',c('Lower', 'Upper'), fill = c('black', 'blue'))
# #plot(gsapply(1:20, calibFun, x = a))
#
#
#
# ##COmpute in parallel (makCluster and then parLapply)
#
#
#
#
# #SImulate data
#
