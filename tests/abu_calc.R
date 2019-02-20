############### DATA PREP ############### 

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

############### INTENSITY PLOTS ############### 

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


species_rename = function(species_names){
  new_species_names = species_names
  for(i in 1:length(species_names)){
    sns = str_split_fixed(species_names[i], " ", 2)
    new_species_names[i] = paste0(substr(sns[1], 1, 1), ". ", substr(sns[2], 1, 10))
  }
  return(new_species_names)
}
abu_infested = 
  abu_infested %>% 
  mutate(Host_species = as.character(Host_species),
         Host_species = species_rename(Host_species),
         Host_species = as.factor(Host_species))

g_2011 = abu_infested %>% 
  filter(Year == 2011) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
  scale_y_continuous(trans="sqrt") + # sqrt transform Y axis
  theme_bw()
g_2014 = abu_infested %>% 
  filter(Year == 2014) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
  scale_y_continuous(trans="sqrt") + # sqrt transform Y axis
  theme_bw()
g_2015 = abu_infested %>% 
  filter(Year == 2015) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
  scale_y_continuous(trans="sqrt") + # sqrt transform Y axis
  theme_bw()
g_2017 = abu_infested %>% 
  filter(Year == 2017) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
  scale_y_continuous(trans="sqrt") + # sqrt transform Y axis
  theme_bw()
g_2018 = abu_infested %>% 
  filter(Year == 2018) %>% 
  ggpubr::gghistogram("Ch_sumNA", facet.by = "Host_species", bins = 30) + 
  scale_y_continuous(trans="sqrt") + # sqrt transform Y axis
  theme_bw()

cowplot::plot_grid(g_2011, g_2014, g_2015, g_2017, g_2018, labels = c("2011", "2014", "2015", "2017", "2018"))


############### INTENSITY BCI ############### 

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

############### PREVALENCE CI (C-P) ############### 

# total
abu1_year_totals = 
  abu1 %>% 
  filter(Coll == "exp") %>%
  group_by(Year, Host_species) %>%
  summarise(n_total = n())
#infested
abu1_year_infested = 
  abu1 %>% 
  filter(Coll == "exp" &
           Ch_sumNA > 0) %>%
  group_by(Year, Host_species) %>%
  summarise(n_infested = n())

# merge to calc prevalence
abu1_year_prevalence = full_join(abu1_year_totals, abu1_year_infested)
abu1_year_prevalence %>% naniar::vis_miss() # check for NA
abu1_year_prevalence = 
  abu1_year_prevalence %>% 
  mutate(n_infested = ifelse(is.na(n_infested), 0, n_infested),
         prevalence = n_infested/n_total) # NA -> 0

# calc prevalence CI
abu1_year_prevalence_nested = abu1_year_prevalence %>% group_by(Year, Host_species) %>% nest()
abu1_year_prevalence_ci = 
  abu1_year_prevalence_nested %>% 
  mutate(CI_CP = map(data, ~binom.test(.x$n_infested, .x$n_total)$conf.int),
         CI_CP_L = map(CI_CP, ~.x[1]),
         CI_CP_U = map(CI_CP, ~.x[2])) %>%
  select(-CI_CP) %>%
  unnest() %>%
  select(Year, Host_species, n_infested, n_total, prevalence, CI_CP_L, CI_CP_U)

# Plots
ggplot(abu1_year_prevalence_ci, aes(x=Host_species, y=prevalence, ymin=CI_CP_L, ymax=CI_CP_U)) +
  geom_pointrange(size = 1, col="dodgerblue", shape=21, fill="white") +
  geom_text(aes(label = n_total), size=2.5, color = "black") +
  facet_grid(Year ~ .) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Host species", y = latex2exp::TeX("Prevalence = n_{infested}/n_{total} & Clopper-Pearson CI"))
