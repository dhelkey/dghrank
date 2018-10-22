context('scoreComposite')

data('neonatal')
m1 = neonatal[ ,c('outcome1', 'instid', 'subset','male','outborn',
                            'csect', 'cont1','cont2')]
m2 = neonatal[ ,c('outcome2', 'instid', 'subset',
                  'csect', 'cont1','cont2')]
m3  = neonatal[ ,c('outcome3', 'instid', 'subset')]
r1 = fitBabyMonitor(m1, 3, 2, subset = TRUE)
r2 = fitBabyMonitor(m2, 1, 2, subset = TRUE)
r3 = fitBabyMonitor(m3[1:10000, ], 0, 0, subset = TRUE)
r_list = list(r3, r2, r1)


##TODO, test why this  edge case breaks fitBabyMonitor, not a priority currently
# m = m2[10000:19000, ]
# dat = parseMinimalData(m, 1, 2)
# a = partitionSummary(dat$y, dat$inst, dat$subset_vec)

a = scoreComposite(r_list, type = 'inst')
scoreComposite(r_list, type = 'subset_nobaseline')
tail(scoreComposite(r_list, type = 'inst_subset_nobaseline'))



##Manually test a few for the each case
i = 44
out = scoreComposite(r_list, type = 'inst')[44,]

mean(c(r1$inst_mat[44,]$score_est, r2$inst_mat[44,]$score_est,
  r3$inst_mat[44,]$score_est))

sqrt(r1$inst_mat[44,]$score_s^2 +
         r2$inst_mat[44,]$score_s^2 +
       r3$inst_mat[44,]$score_s^2)
