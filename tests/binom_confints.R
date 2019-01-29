# define binomial distribution
suc = 10
tot = 50

# generate observed data
d = c(
  rep(1, suc),
  rep(0, tot-suc)
)

# binomial distribution density plot
plot_data = data.frame(
  n = 0:50,
  p = dbinom(0:50, 50, 0.2)
)
plot = ggplot(plot_data) +
  geom_point(aes(x=n/50,y=p), col="dodgerblue") +
  geom_line(aes(x=n/50,y=p), col="dodgerblue") +
  theme_classic()

# all sorts of confints
library(binom)
confints = binom.confint(10, 50)

# generate percentile bootstrap confint by hand
R = 10000
b_means = numeric(R)
for (i in 1:R) b_means[i] = mean(sample(d, tot, replace = T))
quantile(b_means, c(0.025, 0.975))

library(boot)
boot_mean = function(d, i) {mean(d[i])}
b = boot(data = d, 
     statistic = boot_mean,
     R = R)
boot.ci(b, type = "bca")$bca[4:5]

# function
mean_ci_bootbca = function(d) {
  boot_mean = function(d, i) {mean(d[i])}
  b = boot::boot(data = d, 
           statistic = boot_mean,
           R = R)
  boot::boot.ci(b, type = "bca")$bca[4:5]
}
mean_ci_bootbca_res = mean_ci_bootbca(d)

# CIs for plot
confints = 
  rbind(
    confints,
    data.frame(method="boot.bca", x=10, n=50, mean=suc/tot, lower=mean_ci_bootbca_res[1], upper=mean_ci_bootbca_res[2])
  ) %>% rownames_to_column() %>% mutate(rowname = as.numeric(rowname))

plot + 
  #geom_bar(data = b_means_cut, mapping = aes(x=n_50, y=Freq), stat="identity") +
  geom_segment(data = confints, mapping = aes(y=rowname/200, x=lower, yend=rowname/200, xend=upper, col = method))
