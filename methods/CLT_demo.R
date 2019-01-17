# CLT demo

library(tidyverse)

# модель распределения (ген.совокупность)
set.seed(123)
pop = rgamma(3000, 3)
#pop %>% ggpubr::gghistogram() # смотрим на гистограмму ген.совокупности

# пример выборки (n=5)
pop %>% sample(5)
pop %>% sample(5) %>% mean()

# делаем много (R) выборок размера n и считаем среднее в каждой
R = 1000
n = 30
sample_means = c()
for (i in 1:R)  sample_means[i] = pop %>% sample(n) %>% mean(.)

# смотрим на гистограмму выборочных средних
#sample_means %>% ggpubr::gghistogram()

# сводим вместе обе таблицы и смотрим сразу на две гистограммы
sample_means_df = data.frame(
  values = c(pop, sample_means),
  type = c(rep("pop", length(pop)), rep("sample_means", length(sample_means)))
)
g21 = 
  sample_means_df %>% 
  filter(type=="pop") %>% 
  ggplot(aes(x=values, y=..density..)) +
  geom_histogram(bins=50, fill="dodgerblue", alpha=.6) +
  geom_density(col="dodgerblue") +
  labs(title="Распределение генеральной совокупности", x="Значение переменной", y="Вероятность")

g22 = sample_means_df %>% ggpubr::gghistogram("values", "..density..", fill="type", col=NA, add="mean", add_density = T, bins=50, alpha=.3)

# другой размер выборки - более узкий интервал
n = 200
sample_means1 = c()
for (i in 1:R)  sample_means1[i] = pop %>% sample(n) %>% mean(.)
sample_means_df1 = data.frame(
  values = c(pop, sample_means1),
  type = c(rep("pop", length(pop)), rep("sample_means", length(sample_means1)))
)

# демонстрация того, что это все значит - без комментариев пока
R = 10
n = 30
sample_means_demo = data.frame()
for (i in 1:R) sample_means_demo = rbind(sample_means_demo, data.frame(values=sample(pop, n), value_types=i))

library(ggridges)
g3 = sample_means_demo %>% 
    ggplot(aes(x=values, y=as.factor(value_types))) +
    geom_density_ridges(col="grey10", 
                                  jittered_points=TRUE, scale = .95, rel_min_height = .01,
                                  point_shape = "↑", point_size = 2, size = 0.05,
                                  position = position_points_jitter(height = 0)) +
    geom_point(data = sample_means_demo %>% group_by(value_types) %>% summarise(means = mean(values)), aes(x=means, y=value_types), col="red") +
    geom_density(data=sample_means_df %>% filter(type == "sample_means"), aes(x=values, y=stat(density)), fill="red", col="darkred", alpha=.5) +
    geom_density(data=sample_means_df1 %>% filter(type == "sample_means"), aes(x=values, y=stat(density)), fill="red", col="darkred", alpha=.7) + 
    geom_vline(xintercept = mean(pop), col="red", linetype="dashed") +
    labs(title="Иллюстрация центральной предельной теоремы", x="Значение переменной", y="Номер выборки") +
    theme_minimal()

g4 = sample_means_df %>% filter(type == "sample_means") %>% ggqqplot("values", fill="red", col="red", ggtheme = theme_classic())

g5 = pop %>% cut(seq(min(pop), max(pop), l=3) %>% pretty(n=2)) %>% table() %>% as.data.frame() %>%  ggbarplot(".", "Freq", fill="dodgerblue", col=NA, alpha=.6, width = .9)
g6 = pop %>% cut(seq(min(pop), max(pop), l=10) %>% pretty()) %>% table() %>% as.data.frame() %>%  ggbarplot(".", "Freq", fill="dodgerblue", col=NA, alpha=.6, width = .9)

library(rvg)
library(officer)
doc <- read_pptx() %>% # создаем презентацию
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g21), l=1, t=1, w=7, h=7) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g22), l=1, t=1, w=7, h=7) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g3), l=1, t=1, w=7, h=7) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g4), l=1, t=1, w=3, h=3) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g5), l=1, t=1, w=3, h=3) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g6), l=1, t=1, w=3, h=3) %>% # вставляем график
  print(target = "demo.pptx") # задаем имя файла

