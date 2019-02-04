# source script
# source("https://docs.google.com/document/export?format=txt&id=11wna4Fq9oziZ2gsOdLxywyS5cRaOCazqBqaGeo7-f6w")

# get table
# rio::import("https://drive.google.com/uc?authuser=0&id=1oCvunUF1ajetOnyvxgfZVPlIrVeCEi6s&export=download", format="xlsx")
#d = rio::import("https://drive.google.com/uc?authuser=0&id=1TkxlJs8t8jwve-ZX7Bw6f1j4kx_F0Wh8&export=csv")
abu = rio::import("~/Downloads/Vietnam_Abundance__.xlsx")


abu1 =
  abu %>%
  naniar::replace_with_na_all(~.x == "na") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("Ch_")), as.numeric) %>%
  mutate(Ch_sumNA = Ch_ear+Ch_urgen+Ch_belly+Ch_nose_eye+Ch_chin) %>%
  mutate(Ch_sumNA1 = ifelse(Ch_sumNA > 0, 1,0))

abu1 %>% str()
abu1 %>% View()

# total
abu1_year_totals = 
  abu1 %>% 
  filter(Coll == "exp") %>%
  group_by(Year) %>%
  summarise(n = n())
#infested
abu1_year_infested = 
  abu1 %>% 
  filter(Coll == "exp" &
                  Ch_sumNA > 0) %>%
  group_by(Year) %>%
  summarise(n = n())

# percent infested
data.frame(
  abu1_year_totals[,1], 
  abu1_year_infested[,2]/abu1_year_totals[,2]*100
) %>% set_colnames(c("year", "percent_infested"))
