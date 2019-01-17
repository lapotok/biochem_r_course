# как связана стандартная ошибка (среднего) с дисперсией выборочных средних
library(tidyverse)
library(ggpubr)
library(cowplot)

pop = rnorm(1e4, 50, 20)

g = data.frame(x=pop) %>%
  ggplot(aes(x=x)) +
  geom_histogram(aes(y=..density..), col="dodgerblue2", fill="dodgerblue", bins=50) +
  stat_density(geom="line", col="dodgerblue4") +
  theme_classic() +
  background_grid()

pop_mean = mean(pop)
pop_se = sd(pop)/sqrt(length(pop))
pop_se

# считаем стандартную ошибку методом бутстрепа
# стандартная ошибка - стандартное отклонение выборочных оценок среднего
R = 10000
output = numeric(R)
for (i in 1:R){
  output[i] = mean(sample(pop, size = length(pop), replace = T))
}
sd(output)

# то же с помощью библиотеки boot
library(boot)
boot_mean = function(d, i) mean(d[i])
boot(pop, boot_mean, R, parallel = "snow")