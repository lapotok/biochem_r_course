# находим с помощью симуляции достаточное n для достижения необходимой мощности

set.seed(321)
s1 = rnorm(30, 50, 10)
s2 = rnorm(30, 55, 10)

t.test(s1, s2) # не видим отличий (p=.313)

# вопрос: сколько надо n, чтобы мощность теста была > 0.8 при ошибке I рода < 0.05

# генерим выборки от n=3 до n=100 (mean=50 vs 55, sd=10) и считаем t-test... 
# ... повторяем R(1000) раз и смотрим % найденных отличий для каждого n

R = 100
n1 = 3
n2 = 100
n_range = as.character(n1:n2)

outcomes = 
  data.frame(n = as.numeric(n_range), 
             power = rep(0, n2-n1+1)) %>% 
    set_rownames(n_range)

for (n in n_range){ # для каждого числа выборок n_i
  cat(n, "\n")
  for (i in 1:R){ #  повторение процедуры R раз
    s1_ = rnorm(n, mean(s1), 10)
    s2_ = rnorm(n, mean(s2), 10)
    outcomes[n,"power"] = 
      outcomes[n,"power"] + 
      (t.test(s1_, s2_)$p.value < 0.05)/R*100
  }
}

outcomes
outcomes_g = outcomes %>%  
  ggplot(aes(x=n, y=power)) + 
  geom_point(size=3, alpha=.6) +
  geom_smooth(data = outcomes, 
              mapping=aes(x=n, y=power), 
              method="lm", 
              formula = y ~ x + I(x^2), 
              se = F) +
  geom_hline(yintercept = 80, 
             col="red", 
             linetype="dashed") +
  theme_classic(base_size = 12) +
  labs(x="Размер выборки (n)", y="Мощность теста (% незамеченных эффектов)") +
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))

outcomes_g + 
  ggrepel::geom_label_repel(
    data = outcomes %>% filter(power > 80) %>% head(1), 
    mapping = aes(label=paste("n = ", n, ", power = ", power %>% round(2))),
    nudge_y = .05, 
    nudge_x = -10) 

outcomes_g %>% ggplotly()

# а точно ли соответствует генеральной совокупности выборочное среднее и дисперсия?


# проверяем себя с помощью теста мощности
power.t.test(sd=10, power=.8, d=mean(s1)-mean(s2))
