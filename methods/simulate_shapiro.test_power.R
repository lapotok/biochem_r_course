my_theme = 
  theme_classic() +
  theme(panel.grid.major = element_line(linetype = "dashed", color = "gray"))

# зависимость мощности диагностики ненормальности распределения от размера
pop = rgamma(1e6, 3)


g_hist = data.frame(x=pop) %>% 
  ggplot(aes(x=pop)) +
  stat_density(geom="area", col = NA, fill = "dodgerblue", alpha=.3) +
  geom_histogram(aes(y=..density..), fill = "dodgerblue", col = "dodgerblue3", bins=30) +
  stat_density(geom = "line", col="dodgerblue4") +
  labs(x="Значение признака", y="Плотность вероятности", title="Гистограмма распределения") +
  coord_cartesian(expand = F) + my_theme
 
n_range = 3:100
R = 500

outcomes = numeric(length(n_range)) %>% setNames(n_range)
for (n in n_range){
  for (i in 1:R){
    outcomes[as.character(n)] = 
      outcomes[as.character(n)] +
      (shapiro.test(sample(pop, n, replace = T))$p.value < .05)
  }
}

d = 
  data.frame(
    n = n_range,
    power = outcomes/R
  )

g_power = ggplot(d, aes(x=n, y=power)) +
  geom_hline(yintercept = 1, linetype = "dotted", col = "dodgerblue") +
  geom_point() +
  geom_smooth(method = "loess", se = F, color="dodgerblue4") +
  labs(x="Величина выборки (n)", y="Мощность (определение ненормальности)", title="Выявление ненормальности в выборках разного размера") +
  coord_cartesian(expand = F) + my_theme

g_comb = cowplot::ggdraw() + cowplot::draw_plot(g_power) + cowplot::draw_plot(g_hist, x=.4, y=.1, w=.5, h=.5)
