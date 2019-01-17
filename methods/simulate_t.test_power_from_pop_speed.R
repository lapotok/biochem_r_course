library(compiler)
library(tidyverse)
library(magrittr)

do_stuff = function(R=100){
  set.seed(1234)
  pop1 = rnorm(1e6, 50, 10)
  pop2 = rnorm(1e6, 55, 10)
  s1 = sample(pop1, 30, replace = T)
  s2 = sample(pop2, 30, replace = T)
  
  t.test(s1, s2) # не видим отличий (p=.088)
  
  # вопрос: сколько надо n, чтобы мощность теста была > 0.8 при ошибке I рода < 0.05
  
  # генерим выборки от n=3 до n=100 (mean=50 vs 55, sd=10) и считаем t-test... 
  # ... повторяем R(1000) раз и смотрим % найденных отличий для каждого n
  
  R = R
  n1 = 3
  n2 = 100
  n_range = as.character(n1:n2)
  
  outcomes = 
    data.frame(n = as.numeric(n_range), 
               sum = rep(0, n2-n1+1),
               power = rep(0, n2-n1+1)) %>% 
    set_rownames(n_range)
  
  for(i in 1:R){ #  повторение процедуры R раз
    s1max = sample(pop1, max(n_range), replace = T)
    s2max = sample(pop2, max(n_range), replace = T)
    
    for (n in n_range){ # для каждого числа выборок n_i
      outcomes[n,"sum"] = 
        outcomes[n,"sum"] + 
        (t.test(s1max[1:n], s2max[1:n])$p.value < .05)
    }
  }
  
  outcomes = 
    outcomes %>% 
    mutate(power = sum/R*100)
  return(outcomes)
}

cmp_do_stuff = cmpfun(do_stuff)
enableJIT(3)
system.time({cmp_do_stuff(50)})
system.time({do_stuff(50)})
cmp_do_stuff(5)
