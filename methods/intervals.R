# intervals
library(tidyverse)

df = data.frame()
for (seed in 1:10000){
  set.seed(seed)
  n = 100
  mean = 50
  sd = 5
  sam = rnorm(n, mean, sd) %>% round()
  df = rbind(df, data.frame(seed=seed, sw=shapiro.test(sam)$p.value))
}

df %>% arrange(desc(sw)) %>% head

set.seed(3286)
n = 100
mean = 50
sd = 5
sam = rnorm(n, mean, sd) %>% round()
sam

dnorm1 = function(x, mean=0, sd=1, multiply=1) dnorm(x, mean, sd)*multiply+1

# data.frame(x=sam)%>% ggplot(aes(x=x)) + geom_dotplot(method = "histodot", fill="white", col="white", binwidth=1) +
#   stat_function(fun=dnorm1, args = list(mean=50, sd=5, multiply=6), col="blue") +
#   #geom_dotplot(data=data.frame(x=sam[1:20]),method = "histodot", binwidth=1) +
#   #geom_dotplot(data=data.frame(x=sam[1:60]),method = "histodot", binwidth=1) 
#   geom_dotplot(method = "histodot", binwidth=1)


sam_times = c()
for (i in 1: length(sam)){
  sam_times[i] = which(sam[i]==sam[1:i]) %>% length()
}

df = data.frame(x=sam, y=sam_times, z = rep(1, length(sam)))
box = ggplot(df, aes(x=z, y=x)) + geom_boxplot(col="darkgreen") + stat_summary(geom="errorbar", fun.data = mean_cl_normal, color="dodgerblue", width=.3, position = position_nudge(x=-1)) + theme_void() + coord_flip(ylim=c(mean-3.2*sd, mean+3.2*sd), expand = F)

dots = ggplot(df, aes(x=x,y=y)) +
  geom_ribbon(data=data.frame(x=seq(mean-sd, mean+sd, l=100), ymin=rep(.5, 100), ymax=dnorm1(seq(mean-sd, mean+sd, l=100), mean, sd, length(sam))), aes(x=x, ymin=ymin, ymax=ymax, y=NULL), fill="dodgerblue", col=NA, alpha=.4) +
  stat_function(fun=dnorm1, args = list(mean=mean, sd=sd, multiply=length(sam)), col="dodgerblue") +
  geom_point(size=6, alpha=.6) +
  coord_cartesian(xlim=c(mean-3.2*sd, mean+3.2*sd), ylim=c(.5, max(sam_times)+1), expand = F) +
  labs(x="Значение переменной", y="Частота значения в выборке (разы)")
  
#library(patchwork)  
g = box/dots + plot_layout(heights = c(.1,.9))

library(rvg)
library(officer)

doc <- read_pptx() %>% # создаем презентацию
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g), l=1, t=2, w=8, h=3) %>% # вставляем график
  print(target = "demo.pptx") # задаем имя файла
