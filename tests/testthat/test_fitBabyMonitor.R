context('fitBabyMonitor')

##Test that it matches Draper-Gittoes w/o covariates
data("neonatal")
minimal_data = neonatal[ , c('outcome1', 'instid',
                             'male','outborn', 'csect')]
num_cat = 3
num_cont = 0
subset = FALSE
iters = 1000
burn_in = 10
r=fitBabyMonitor(minimal_data, num_cat,num_cont)
dat = processMinimalData(minimal_data, num_cat, num_cont)
dg = designBased(0.5, dat$y, r$pcf_vec, dat$inst_vec)


test_that('WE have the human readable structure we expect', {
  expect_true( all( c('inst_mat', 'full_subset_mat_nobaseline', 'full_subset_mat_nobaseline') %in% names(r)))
})


test_that('DG & DGH perform functionally the same', {
  expect_true(cor(dg$Z, r$inst_mat$effect_z) > 0.995)
  expect_true(cor(dg$O - dg$E, r$inst_mat$effect_est) > 0.995)

})


#Compare w/ Draper-Gittoes Rankings
par(mfrow =c(2,2))
plot(dg$Z, r$inst_mat$effect_z)
p1 = plotRankingLines(r$inst_mat$effect_est, r$inst_mat$effect_lower, r$inst_mat$effect_upper, lwd = 2)
dg$lower = dg$D - r$dat$z_star * dg$SE
dg$upper = dg$D + r$dat$z_star * dg$SE
plotRankingLines(dg$D, dg$lower, dg$upper, lwd = 2, order = p1$order)



##Now make sure it matches D-G w/o covariates
test_that('Matches D-G w/o covariates',{
  minimal_data = neonatal[ ,c('outcome1', 'instid')]
  num_cat = num_cont = 0
  r = fitBabyMonitor(minimal_data, num_cat, num_cont)
  dg = designBased(0.5, r$dat$y, rep(1, r$dat$N), r$dat$inst_vec)
 # plot(r$inst$effect_z, dg$Z)
  expect_true(mean(r$inst$effect_z - dg$Z) < 0.15)
  expect_true(cor(r$inst$effect_z, dg$Z) > 0.995)
})


test_that('subset_inst works',{
  minimal_data = neonatal[ ,c('outcome1', 'instid',  'subset', 'male', 'csect', 'cont1')]
  r = fitBabyMonitor(minimal_data, 2, 1, subset = TRUE)
  par(mfrow = c(2,2))
  a = plotRankingLines(r$subset_nobaseline_mat$effect_est,r$subset_nobaseline_mat$effect_lower, r$subset_nobaseline_mat$effect_upper )
  a = plotRankingLines(r$subset_baseline_mat$effect_est,r$subset_baseline_mat$effect_lower, r$subset_baseline_mat$effect_upper, order = a$order )
  a = plotRankingLines(r$subset_nobaseline_mat$score_est,r$subset_nobaseline_mat$score_lower, r$subset_nobaseline_mat$score_upper , order = a$order)
  expect_true(TRUE) #THis one is a visual test
})

test_that('subset works',{
  minimal_data = neonatal[ ,c('outcome1', 'instid', 'subset', 'male', 'csect', 'cont1')]
  r = fitBabyMonitor(minimal_data, 2, 1, subset = TRUE)
  plotRankingLines(r$full_subset_mat_nobaseline$effect_est,r$full_subset_mat_nobaseline$effect_lower, r$full_subset_mat_nobaseline$effect_upper )
  plotRankingLines(r$full_subset_mat_baseline$effect_est,r$full_subset_mat_baseline$effect_lower, r$full_subset_mat_baseline$effect_upper )
  plotRankingLines(r$full_subset_mat_nobaseline$score_est,r$full_subset_mat_nobaseline$score_lower, r$full_subset_mat_nobaseline$score_upper )

  expect_true(TRUE)
})


