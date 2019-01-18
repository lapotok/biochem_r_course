library(tidyverse)
library(magrittr)
library(ggpubr)

elisa = rio::import("data/elisa_w_outlier.xlsx")
str(elisa)

library(drc)

# wide to long
elisa_l = elisa %>% gather("Replica", "Value", -Sample, -Conc)

# default model
elisa_m1 = drm(Value ~ Conc, data = elisa_l, fct = LL.5())
plot(elisa_m1, log = "xy", type = "all")

summary(elisa_m1)
modelFit(elisa_m1) # sucks because of the outlier... but how to find it?

plot(elisa_l$Conc, cooks.distance(elisa_m1), log = "x")
plot(elisa_l$Conc, residuals(elisa_m1), log = "x")
plot(elisa_l$Conc, residuals(elisa_m1)/fitted(elisa_m1), log = "x")

# jacknife

coeffs_j = list()
for (i in 1:nrow(elisa_l)){
  coeffs_j[[i]] = elisa_l[-i, ] %>% drm(Value ~ Conc, data = ., fct = LL.5()) %>% coefficients()
}

lof = numeric(nrow(elisa_l)) %>% set_names(1:nrow(elisa_l))

for (i in 1:nrow(elisa_l)){
  lof[i] = elisa_l[-i, ] %>% drm(Value ~ Conc, data = ., fct = LL.5()) %>% modelFit() %>% .$`p value` %>% .[2]
}

elisa_l %>% 
  mutate(is_outlier = residuals(elisa_m1)/fitted(elisa_m1)>2,
         lof = lof)



# bootstrap
library(boot)
coeffs_drm = function(data, indices){
  drm(Value ~ Conc, data = data, fct = LL.5()) %>% coefficients()
}
boot_drm = boot(R = 1000,  data = elisa_l[, c("Value", "Conc")], statistic = coeffs_drm)

# compare the coeffs

# init model
drm(Value ~ Conc, data = elisa_l, fct = LL.5()) %>% 
  coefficients() %>% 
  as.data.frame() 

# no outlier!
elisa_l %>% 
  mutate(is_outlier = residuals(elisa_m1)/fitted(elisa_m1)>2) %>%
  filter(!is_outlier) %>%
  drm(Value ~ Conc, data = ., fct = LL.5()) %>% # plot(log = "xy", type = "all")
  coefficients() %>% 
  as.data.frame() 

boot_drm

elisa_l$Conc, residuals(elisa_m1)/fitted(elisa_m1)