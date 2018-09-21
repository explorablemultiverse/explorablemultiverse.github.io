#
# Helper functions used in analysis.R.
#
# Anonymous Capybara, June 2018/
#

library(boot)
library(simpleboot)

samplemean <- function(x, d) {return(mean(x[d]))}

#
# Difference between the means of two independent samples and its 95% BCa bootstrap CI,
# returned as a data frame.
#
diff_means_bootstrap <- function(group1, group2, conf.level = 0.95, R = 1000) {
  pointEstimate <- samplemean(group1) - samplemean(group2)
  set.seed(0) # make deterministic
  bootstrap_samples <- two.boot(sample1 = group1, sample2 = group2, FUN = samplemean, R = R)
  bootci <- boot.ci(bootstrap_samples, type = "bca", conf = conf.level)
  data.frame(
    diff_means = pointEstimate,
    ci_lower = bootci$bca[4],
    ci_upper = bootci$bca[5])
}

#
# Difference between the means of two independent samples, its 95% t-based CI,
# and the p-value for H0=0, returned as a data frame.
#
diff_means_t <- function(group1, group2, conf.level = 0.95) {
  pointEstimate <- samplemean(group1) - samplemean(group2)
  ttest <- t.test(group1, group2)
  data.frame(
    diff_means = pointEstimate,
    ci_lower = ttest$conf.int[1],
    ci_upper = ttest$conf.int[2],
    p_value = ttest$p.value)
}
