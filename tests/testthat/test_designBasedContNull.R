context('designBasedContNull')
##TODO redo tests, should be a little faster, and maybe we don't need to test this one as hard.


# data("neonatal")
# minimal_data = neonatal[ ,c('outcome_cont', 'instid', 'male', 'outborn', 'csect', 'ap5')]
# dat = parseMinimalData(minimal_data, 4, 0)
# pcf_cat_vec = apply(dat$cat_var_mat, 1, paste, collapse = '-')
#
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
# dim(a)
#
# expect_equal(a[1, ], a1$Z)
# expect_equal(a[2, ], a2$Z)
# expect_equal(a[3, ], a3$Z)
# expect_equal(a[4, ], a4$Z)
# expect_equal(a[5, ], a5$Z)
#
#
# computeNullBlock = function(simulated_data, gamma_vec){
#
# }



