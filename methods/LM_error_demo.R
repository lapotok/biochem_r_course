# lm error
# plot intervals http://rstudio-pubs-static.s3.amazonaws.com/7024_e3a68a9b35454e74abfe15b621c50502.html
# https://stackoverflow.com/questions/12544090/predict-lm-in-r-how-to-get-nonconstant-prediction-bands-around-fitted-values
# http://r.789695.n4.nabble.com/How-does-predict-calculate-prediction-intervals-td4657108.html

d = data.frame(x = 1:40)
d$y = d$x*2+6 + rnorm(40, 0, 7)
d %>% ggscatter("x", "y")

m = lm(y~x, d)
m
confint(m)
coeffs = expand.grid(b0 = seq(confint(m)[1,1], confint(m)[1,2], l=10), b1 = seq(confint(m)[2,1], confint(m)[2,2], l=10))

# conf/pred int corridor
predx <- data.frame(x=seq(0,40,l=100))
pred.int <- cbind(predx, predict(m, newdata = predx, interval = "prediction", level = 0.95))
conf.int <- cbind(predx, predict(m, newdata = predx, interval = "confidence", level = 0.95))

g = d %>% ggscatter("x", "y") + coord_cartesian(xlim=c(0, 41), ylim=c(min(pred.int$lwr),max(pred.int$upr)), expand = F) + labs(x="Концентрация", y="Оптическая плотность")
g_optimal_orig = g +
  stat_function(fun = function(x) coefficients(m)[1] + coefficients(m)[2]*x, col="blue", size=1) +
  stat_function(fun = function(x) 6+2*x, col="red", size=1) 

linfun = function(x, b0, b1) b0 + b1 * x

g_all = g
for (i in 1:nrow(coeffs)){
  g_all = g_all + stat_function(fun = linfun, args = list(b0=coeffs[i,"b0"], b1=coeffs[i, "b1"]), col="blue", alpha=.1)
}

g_all_int = g_all
g_optimal_orig_int = g_int = g
g_all_int = g_all_int  +
  geom_smooth(data = conf.int, aes(x=x, y=fit, ymin = lwr, ymax = upr), stat = "identity", fill="red", alpha=.4) +
  geom_smooth(data = pred.int, aes(x=x, y=fit, ymin = lwr, ymax = upr), stat = "identity", fill="green", alpha=.1)

g_int = g_int  +
  geom_smooth(data = conf.int, aes(x=x, y=fit, ymin = lwr, ymax = upr), stat = "identity", fill="red", alpha=.4) +
  geom_smooth(data = pred.int, aes(x=x, y=fit, ymin = lwr, ymax = upr), stat = "identity", fill="green", alpha=.1)

g_optimal_orig_int = g_optimal_orig_int  +
  stat_function(fun = function(x) coefficients(m)[1] + coefficients(m)[2]*x, col="blue", size=1) +
  stat_function(fun = function(x) 6+2*x, col="red", size=1) +
  geom_smooth(data = conf.int, aes(x=x, y=fit, ymin = lwr, ymax = upr), stat = "identity", fill="red", alpha=.4) +
  geom_smooth(data = pred.int, aes(x=x, y=fit, ymin = lwr, ymax = upr), stat = "identity", fill="green", alpha=.1)


# prediction interval
pred = predict(m, newdata = data.frame(x=10), interval = "prediction")
g_int_pred = g_int +
  geom_segment(data=data.frame(x=10, y=min(pred.int$lwr), xend=10, yend=pred[3]), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) +
  geom_segment(data=data.frame(x=0, y=pred[1], xend=10, yend=pred[1]), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) +
  geom_segment(data=data.frame(x=0, y=pred[2], xend=10, yend=pred[2]), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) +
  geom_segment(data=data.frame(x=0, y=pred[3], xend=10, yend=pred[3]), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) 

# inversion interval
library(investr)
inv <- calibrate(m, y0 = 60, interval = "inversion", level = 0.95)
g_int_pred_inv = g_int_pred +
  geom_segment(data=data.frame(x=0, y=60, xend=inv$upper, yend=60), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) +
  geom_segment(data=data.frame(x=inv$upper, y=min(pred.int$lwr), xend=inv$upper, yend=60), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) +
  geom_segment(data=data.frame(x=inv$estimate, y=min(pred.int$lwr), xend=inv$estimate, yend=60), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) +
  geom_segment(data=data.frame(x=inv$lower, y=min(pred.int$lwr), xend=inv$lower, yend=60), mapping=aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed", alpha=.5) 

doc <- read_pptx() %>% # создаем презентацию
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g), l=1, t=1, w=8, h=6) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g_optimal_orig), l=1, t=1, w=8, h=6) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g_all), l=1, t=1, w=8, h=6) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g_all_int), l=1, t=1, w=8, h=6) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g_int), l=1, t=1, w=8, h=6) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g_int_pred), l=1, t=1, w=8, h=6) %>% # вставляем график
  add_slide(layout = "Title and Content", master = "Office Theme") %>% # добавляем слайд 1
  ph_with_vg_at(code = print(g_int_pred_inv), l=1, t=1, w=8, h=6) %>% # вставляем график
  print(target = "demo.pptx") # задаем имя файла
