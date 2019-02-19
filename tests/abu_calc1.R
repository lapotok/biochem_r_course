# load the data
abu = rio::import("~/Downloads/Vietnam_Abundance__.xlsx")

# prepare the data
abu1 =
  abu %>%
  naniar::replace_with_na_all(~.x == "na") %>% # make proper NAs
  mutate_if(is.character, as.factor) %>% # char -> factor
  mutate_at(vars(starts_with("Ch_")), as.numeric) %>% # fix numeric data
  mutate(Ch_sumNA = Ch_ear+Ch_urgen+Ch_belly+Ch_nose_eye+Ch_chin) %>% # total mite count
  mutate(Ch_sumNA1 = ifelse(Ch_sumNA > 0, 1,0)) # infection
abu1 %>% str()
abu1 %>% nrow()

# filter out noninfested
abu_infested = 
  abu1 %>% filter(Coll == "exp" & Ch_sumNA1 > 0) %>% 
  select(Year, Host_species, G, Age, W, Ch_sumNA) %>%
  mutate_if(is.factor, droplevels)
abu_infested %>% nrow()

# how many years?
years = abu_infested$Year %>% unique() # not so many years left...
years # not so many years left...

# plots
abu_infested %>% 
  filter(Year == 2011) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
    theme_bw()
abu_infested %>% 
  filter(Year == 2011) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
    scale_y_continuous(trans="sqrt") + # sqrt transform Y axis
    theme_bw()

# считаем бутстреп доверительный интервал
infest_bci = function(x) {
  # delta = NA, M = 10000 - фикс чтобы считалось побыстрее (надо delta = 0.01)
  bci = as.numeric(bootBCa::BCa(x, delta = NA, M = 1000, mean)[4:5]) 
  data.frame(mean = mean(x), 
             ci_L = ifelse(length(x)>1,bci[1], NA), 
             ci_U = ifelse(length(x)>1,bci[2], NA))
}
abu_infested %>% filter(Year == 2011 & Host_species == "Bandicota indica") %$% infest_bci(Ch_sumNA)

abu_infested_nested = abu_infested %>% select(Year, Host_species, Ch_sumNA) %>% group_by(Year, Host_species) %>% nest()
abu_infested_nested[1,]
abu_infested_nested[[1, "data"]]

# multiple BCa intervals for infestaton
abu_infested_nested_bci = abu_infested_nested %>% 
  mutate(bci = map(data, ~infest_bci(.x$Ch_sumNA)),
         n = map(data, ~nrow(.x)))

abu_infested_nested_bci %>% 
  unnest(bci) %>% 
  select(-data) %>% 
  mutate_if(is.numeric, ~round(.x, 1)) %>% 
  View
