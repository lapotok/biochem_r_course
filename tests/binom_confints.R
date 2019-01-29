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

ggplot(plot_data) +
  geom_line(aes(x=n,y=p), col="dodgerblue") +
  theme_classic()

# generate percentile bootstrap confint by hand
R = 10000
b_means = numeric(R)
for (i in 1:R) b_means[i] = mean(sample(d, tot, replace = T))
quantile(b_means, c(0.025, 0.975))
#plot + 
  ggplot() +
  geom_histogram(data = data.frame(x=b_means), aes(x=x, y=..density..))

library(binom)
binom.confint(10, 50)

library(boot)
boot_mean = function(d, i) {mean(d[i])}
b = boot(data = d, 
     statistic = boot_mean,
     R = R)
boot.ci(b)