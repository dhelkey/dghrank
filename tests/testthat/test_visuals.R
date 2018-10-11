data("neonatal")
r = fitBabyMonitor(neonatal[ ,c('outcome1', 'instid', 'male', 'outborn', 'csect', 'cont1', 'cont2')],
                   2,2)
i = r$inst_mat
i[i$effect_lower > 0, ]

n_keep = 118
keep_locat = sample(1:r$dat$p, n_keep )
plotRankingLines(i$score_est[keep_locat], i$score_lower[keep_locat], i$score_upper[keep_locat])
