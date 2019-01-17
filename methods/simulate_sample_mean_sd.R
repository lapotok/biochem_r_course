library(tidyverse)
library(magrittr)
library(ggpubr)

theme_set(theme_classic() + theme(plot.title = element_text(size = 15, face="bold", colour = "dodgerblue"),
                                  axis.title.x = element_text(face="bold"),
                                  axis.title.y = element_text(face="bold")))

# simulate t-test I/II errors + power

# параметры генеральных совокупностей
mu1 = 50
mu2 = 55
sigma = 10

pop1 = rnorm(1e6, mu1, sigma)
pop2 = rnorm(1e6, mu2, sigma)

mean(pop1)
sd(pop1)
mean(pop2)
sd(pop2)

# выборки
n = 30
s1 = sample(pop1, n, replace = T)
s2 = sample(pop2, n, replace = T)

mean(s1)
sd(s1)
mean(s2)
sd(s2)

# анализ зависимости усредненного выборочного среднего от величины выборки
R = 100
n_range = 3:100
outcomes_mean = data.frame(n=n_range, mean_sums=rep(0, length(n_range))) %>% set_rownames(n_range)
# симулируем расчет выборочных средних для разного размера выборок
for (n_i in n_range){
  for (i in 1:R){
    outcomes_mean[as.character(n_i), "mean_sums"] = 
      outcomes_mean[as.character(n_i), "mean_sums"] + mean(sample(pop1, n_i, replace = T))
  }
}
# строим график
outcomes_mean_g =
  outcomes_mean %>%
  mutate(averaged_mean = mean_sums/R) %>% 
  ggplot(aes(x=n, y=averaged_mean)) +
  geom_line(color="dodgerblue") +
  geom_point(alpha=.6) +
  geom_hline(yintercept = mu1, color = "red", linetype = "dashed") +
  labs(title = "Зависимость среднего от величины выборки",
       x = "Размер выборки (n)",
       y = glue::glue("Выборочное среднее (R={R})"))


# анализ зависимости усредненного выборочного стандартного отклонения от величины выборки
R = 100
n_range = 3:100
sd1 = function(x) sqrt( sum((x-mean(x))^2)/length(x) ) # biased sd
outcomes_sd = data.frame(n=n_range, 
                      sd_sums=rep(0, length(n_range)), 
                      sd1_sums=rep(0, length(n_range))) %>% set_rownames(n_range)
# симулируем расчет выборочных SD для разного размера выборок
for (n_i in n_range){
  for (i in 1:R){
    s_i = sample(pop1, n_i, replace = T)
    outcomes_sd[as.character(n_i), "sd_sums"] = 
      outcomes_sd[as.character(n_i), "sd_sums"] + sd(s_i)
    outcomes_sd[as.character(n_i), "sd1_sums"] = 
      outcomes_sd[as.character(n_i), "sd1_sums"] + sd1(s_i)
  }
}

# строим график
outcomes_sd = 
  outcomes_sd %>%
  mutate(averaged_sd = sd_sums/R,
         averaged_sd1 = sd1_sums/R)

library(drc)
m0 = drm(averaged_sd ~ n, data = outcomes_sd, fct = drc::MM.2())
m1 = drm(averaged_sd1 ~ n, data = outcomes_sd, fct = drc::MM.2())
predicted = data.frame(
  n = n_range,
  m0 = m0 %>% predict(),
  m1 = m1 %>% predict()
)

outcomes_sd_g0 = 
  outcomes_sd %>% 
  ggplot(aes(x=n, y=averaged_sd)) +
  geom_line(color="dodgerblue", alpha=.4) +
  geom_point(alpha=.4, col="dodgerblue") +
  geom_line(data = predicted, aes(x=n, y=m0), color = "dodgerblue", size=1) +
  geom_hline(yintercept = sigma, color = "red", linetype = "dashed") +
  labs(title = "Зависимость SD от величины выборки",
       x = "Размер выборки (n)",
       y = glue::glue("Выборочное SD (R={R})")) +
  coord_cartesian(ylim=c(6.7, 10))

outcomes_sd_g01 = 
  outcomes_sd_g0 +
  geom_line(aes(y=averaged_sd1), color="darkgreen", linetype="dashed", alpha=.3) +
  geom_line(aes(y=averaged_sd1), color="darkgreen", linetype="dashed", alpha=.3) +
  geom_point(aes(y=averaged_sd1), alpha=.4, shape=17, color="darkgreen") +
  geom_line(data = predicted, aes(x=n, y=m1), color = "darkgreen", size=1, linetype="dashed") 

# save to pptx
library(patchwork)
mean_sd0 = (outcomes_mean_g + geom_vline(xintercept = 30, col="darkgreen", linetype="dotted")) / plot_spacer() / (outcomes_sd_g0 + geom_vline(xintercept = 30, col="darkgreen", linetype="dotted")) + plot_layout(heights = c(.2, .1, .7))
mean_sd01 = (outcomes_mean_g + geom_vline(xintercept = 30, col="darkgreen", linetype="dotted")) / plot_spacer() / (outcomes_sd_g01 + geom_vline(xintercept = 30, col="darkgreen", linetype="dotted")) + plot_layout(heights = c(.2, .1, .7))

tmp <- tempfile(pattern = "ggplot_pptx", fileext = ".pptx")
pptx = officer::read_pptx() %>% 
  officer::add_slide("Title and Content",
                     "Office Theme") %>% 
  rvg::ph_with_vg_at(print(mean_sd0), w=8, h=6.5, l=1, t=1) %>% 
  officer::add_slide("Title and Content",
                     "Office Theme") %>% 
  rvg::ph_with_vg_at(print(mean_sd01), w=8, h=6.5, l=1, t=1) %>% 
  print(tmp)
utils::browseURL(url = tmp)