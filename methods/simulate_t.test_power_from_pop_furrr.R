# симулируем мощность t-теста (for vs map vs parallel)
library(tidyverse)
library(furrr) # https://davisvaughan.github.io/furrr/
library(tictoc)
library(ggpubr)

# генеральные совокупности
pop1 = rnorm(1e6, 50, 10) %>% round()
pop2 = rnorm(1e6, 55, 10) %>% round()

# что надо сделать
# =================

# какие выборки будем брать?
n_set = c(3:5, 10, 20)
n_set

# 1. создать выборки (под максимальный размер)
s1 = sample(pop1, max(n_set))
s1

s2 = sample(pop2, max(n_set))
s2
# 2. создаем вектор, в который будем записывать результаты
numeric(length(n_set)) # вектор из n нулей
numeric(length(n_set)) %>% setNames(n_set) # подписываем значения вектора размерами выборки, которые будем брать
output = numeric(length(n_set)) %>% setNames(n_set) # а теперь все вместе
output
# 3. проходимся по перечню размеров выборок (i - номер элемента в перечне n_set);
#    каждый элемент n_set[i] определяет сколько первых элементов выборки мы возьмем для анализа в t.test

# seq_along автоматически добывает порядковые номера элементов
n_set
seq_along(n_set)
n_set[3]
output[3]

# цикл для расчета
for (i in seq_along(n_set)) { 
  # из одних и тех же выборок s1 и s2 берем все больше элементов
  cat( # выводим сообщение, чтобы видеть, что происходит
      "Подвыборки для t-теста №", i, "( n = ", n_set[i],")", "\n",
      "s1:", s1[1:n_set[i]], "\n",
      "s2:", s2[1:n_set[i]], "\n"
    )
  output[i] = t.test(s1[1:n_set[i]], s2[1:n_set[i]])$p.value < .05
}

# такой цикл можно заменить функцией map (map_int, чтобы получать вектор вместо списка)
sample_ttest = function(s1, s2, n) t.test(s1[1:n], s2[1:n])$p.value < .05
map_int(n_set, ~sample_ttest(s1, s2, .x))


# а теперь это все пихаем в функцию
nset_ttest = function(pop1, pop2, n_set) {
  s1 = sample(pop1, max(n_set))
  s2 = sample(pop2, max(n_set))
  output = numeric(length(n_set)) %>% setNames(n_set)
  for (i in seq_along(n_set)) { # автоматически добываем порядковые номера элементов
    # из одной и той же выборки берем все больше элементов
    output[i] = t.test(s1[1:n_set[i]], s2[1:n_set[i]])$p.value < .05
  }
  return(output)
}

nset_ttest_map = function(pop1, pop2, n_set) {
  s1 = sample(pop1, max(n_set))
  s2 = sample(pop2, max(n_set))
  map_int(n_set, ~sample_ttest(s1, s2, .x))
}

nset_ttest_future_map = function(pop1, pop2, n_set) {
  s1 = sample(pop1, max(n_set))
  s2 = sample(pop2, max(n_set))
  future_map_int(n_set, ~sample_ttest(s1, s2, .x))
}

# пример работы функции
nset_ttest(pop1, pop2, 3:5)

R = 100

# время работы функции с циклами
tic()
res = map(1:R, ~ nset_ttest(pop1, pop2, 3:100))
toc()

res_averaged = res %>% as.data.frame() %>% rowSums()

g = 
  data.frame(
    n = res_averaged %>% names() %>% as.numeric(),
    out = res_averaged / R
  ) %>%
  ggplot(aes(x = n, y = out)) +
  geom_point(alpha = .5) +
  geom_line(alpha = .1) +
  geom_smooth(method = "loess", se = F, size = .5) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),
    se = F,
    size = .5,
    col = "red"
  ) +
  theme_classic()

g %>% ggplotly()

# время работы функции с map
tic()
res = map(1:R, ~ nset_ttest_map(pop1, pop2, 3:100))
toc()

# время работы функции с future_map (как внутри функции, так и снаружи)
plan(sequential)
tic()
res = future_map(1:R, ~ nset_ttest_future_map(pop1, pop2, 3:100))
toc()

plan(multiprocess) # вот теперь полетит!
tic()
res = future_map(1:R, ~ nset_ttest_future_map(pop1, pop2, 3:100))
toc()


# пытаемся попробовать компилирование (не помогло)
# =================================================

# компилируем отдельно функцию
nset_ttest_future_map_cmp = compiler::cmpfun(nset_ttest_future_map)

tic()
res = future_map(1:R, ~ nset_ttest_future_map_cmp(pop1, pop2, 3:100))
toc()

# делаем из всей процедуры функцию и ее компилируем
nset_ttest_future_map_multiple = function(pop1, pop2, n_set, R) {
  future_map(1:R, ~ nset_ttest_future_map(pop1, pop2, n_set))
}

nset_ttest_future_map_multiple_cmp = compiler::cmpfun(nset_ttest_future_map_multiple)
tic()
res = nset_ttest_future_map_multiple_cmp(pop1, pop2, 3:100, R)
toc()
