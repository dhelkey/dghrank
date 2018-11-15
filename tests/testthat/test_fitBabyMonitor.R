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
dat = parseMinimalData(minimal_data, num_cat, num_cont)
dg = designBased(0.5, dat$y, r$pcf_vec, dat$inst_vec)


test_that('WE have the human readable structure we expect', {
  expect_true( all( c('inst_mat', 'full_subset_mat_nobaseline', 'full_subset_mat_nobaseline') %in% names(r)))
})


test_that('DG & DGH perform functionally the same', {
  expect_true(cor(dg$Z, r$inst_mat$stat_z) > 0.995)
  expect_true(cor(dg$O - dg$E, r$inst_mat$effect_est) > 0.995)

})


#Compare w/ Draper-Gittoes Rankings
par(mfrow =c(2,2))
plot(dg$Z, r$inst_mat$stat_z)
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
 # plot(r$inst$stat_z, dg$Z)
  expect_true(mean(r$inst$stat_z - dg$Z) < 0.15)
  expect_true(cor(r$inst$stat_z, dg$Z) > 0.995)
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

test_that('effect and score z-scores are the same',{
  data("neonatal")
  minimal_data = neonatal[ , c('outcome2', 'instid',
                               'male','outborn', 'csect')]
  r=fitBabyMonitor(minimal_data, 3,0)
  m = r$inst_mat
  expect_equal(mean(m$stat_z - m$score_z),0)
})


test_that('names for output data.frames work as expected',{
    minimal_data = neonatal[ ,c('outcome1', 'instid', 'subset', 'male', 'csect', 'cont1')]
    r = fitBabyMonitor(minimal_data, 2, 1, subset = TRUE)
    expect_equal(names(r$inst_mat)[1], 'inst')
    expect_equal(names(r$subset_nobaseline_mat)[1:2], c('inst','subset'))
    expect_equal(names(r$subset_baseline_mat)[1:2], c('inst','subset'))
    expect_equal(names(r$full_subset_mat_nobaseline)[1], 'subset')
    expect_equal(names(r$full_subset_mat_nobaseline)[1], 'subset')
})


##Test that effect and score give the same inference for intervals around 0.........
test_that('inference from effect and score yield same results',{
  data('neonatal')
  minimal_data = neonatal[ ,c('outcome1', 'instid', 'male', 'csect')]
  r = fitBabyMonitor(minimal_data, 2,0)

  ##Flag by
  s_l = r$inst_mat$score_lower > 0
  s_u = r$inst_mat$score_upper < 0
  e_l = r$inst_mat$effect_lower > 0
  e_u = r$inst_mat$effect_upper < 0

  expect_equal(s_l, e_l)
  expect_equal(s_u, e_u)
})

##TODO - test that we get the same results (we should)
# w/ subset vs nonsubset data...
test_that('we get same results in inst_mat whether or not we include a explanatory variable',{
  data("neonatal")
  m1 = neonatal[ ,c('outcome1', 'instid', 'male', 'csect', 'cont1', 'cont2')]
  m2 = neonatal[ ,c('outcome1', 'instid', 'subset', 'male', 'csect', 'cont1', 'cont2')]


  r1 = fitBabyMonitor(m1, 2,2, subset = FALSE)
  r2 = fitBabyMonitor(m2, 2,2, subset = TRUE)

  #Point estimates and intervals for inst_mat should be the same
  expect_false(is.null(r$inst_mat$effect_est))
  expect_equal(r1$inst_mat$effect_est, r2$inst_mat$effect_est, tol = 0.01)
  expect_equal(r1$inst_mat$effect_lower, r2$inst_mat$effect_lower, tol = 0.01)
  expect_equal(r1$inst_mat$effect_upper, r2$inst_mat$effect_upper, tol = 0.01)
  expect_equal(r1$inst_mat$score_est, r2$inst_mat$score_est, tol = 0.01)
  expect_equal(r1$inst_mat$score_lower, r2$inst_mat$score_lower, tol = 0.01)
  expect_equal(r1$inst_mat$score_upper, r2$inst_mat$score_upper, tol = 0.01)
})

