pop = rnorm(1000000, mean=50, sd=10)
var(pop)

x = sample(pop, size = 100, replace = T)
x

# var not corrected
sum(((x-mean(x))^2))/length(x)
# var
sum(((x-mean(x))^2))/(length(x)-1)

# function to compare
var_cmp = function(x){
  data.frame(
    var_nc = sum(((x-mean(x))^2))/length(x),
    var = sum(((x-mean(x))^2))/(length(x)-1)
  )
}

var_cmp(x)

# multiple repeats
R = 10
for (i in 1:R) sample(pop, size = 100, replace = T) %>% var_cmp() %>% print()

# new function:
# * R times extract sample (size n) from population and calculate both var statistics
var_cmp_repeated = function(pop, n, R){
  
}


library(purrr)




?map
# модель 
1:20 %>% map_dbl(~ sample(pop, size = 10, replace = T) %>% var1()) 

set.seed(123)
# задаем условия
calc_grid = expand.grid(R=10, n=c(3,5,10,20,30,100), corr = c(0,1))
calc_grid

mean_var = function(data, R, df) sum((x-mean(x))^2)



calc_grid %>% 
  mutate(df = n-corr) %>% 
  mutate(var = paste(R, n, corr, df, sep=" "))

calc_grid %>% 
  pmap(~ sum(((-mean(x))^2)/(length(x)-ifelse(..3 == "corrected", 1, 0))))

  


calc_grid %>% pmap(
  ~ 1:(..1) %>% map_dbl(~ sample(pop, size = ..2, replace = T) %>% var_select(..3))
)


df = data.frame(
  `003` = 1:10000 %>% map_dbl(~ sample(pop, size = 3, replace = T) %>% var()),
  `005` = 1:10000 %>% map_dbl(~ sample(pop, size = 5, replace = T) %>% var()),
  `010` = 1:10000 %>% map_dbl(~ sample(pop, size = 10, replace = T) %>% var()),
  `050` = 1:10000 %>% map_dbl(~ sample(pop, size = 50, replace = T) %>% var()),
  `100` = 1:10000 %>% map_dbl(~ sample(pop, size = 100, replace = T) %>% var())
)
df1 = data.frame(
  `003` = 1:10000 %>% map_dbl(~ sample(pop, size = 3, replace = T) %>% var1()),
  `005` = 1:10000 %>% map_dbl(~ sample(pop, size = 5, replace = T) %>% var1()),
  `010` = 1:10000 %>% map_dbl(~ sample(pop, size = 10, replace = T) %>% var1()),
  `050` = 1:10000 %>% map_dbl(~ sample(pop, size = 50, replace = T) %>% var1()),
  `100` = 1:10000 %>% map_dbl(~ sample(pop, size = 100, replace = T) %>% var1())
)
dfl = df %>% gather("sample_size", "emp_var")
dfl %>% group_by(sample_size) %>% summarise_at(vars("emp_var"), median)
dfl1 = df1 %>% gather("sample_size", "emp_var")
dfl1 %>% group_by(sample_size) %>% summarise_at(vars("emp_var"), median)

library(patchwork)


dfl_p = dfl %>% 
  ggdensity("emp_var", fill="sample_size", alpha=.4, palette = terrain.colors(6)) +
  coord_cartesian(xlim=c(0,250)) +
  ggtitle("Sample variance (corrected)")
  
dfl1_p = dfl1 %>% 
  ggdensity("emp_var", fill="sample_size", alpha=.4, palette = terrain.colors(6)) +
  coord_cartesian(xlim=c(0,250)) +
  ggtitle("Sample variance (biased)")

dfl_p / dfl1_p
