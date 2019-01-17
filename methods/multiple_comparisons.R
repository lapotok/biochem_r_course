library(tidyverse)
library(purrr)

# сравнение разных типов множественного сравнения и его ошибок

# сравниваем ген. совокупности

pop_t_m = 50
pop_t_dm = 5
pop_t_sd = 10
N = 1e4
n_groups = 10

pop = 
  list(
    A01 = rnorm(N, pop_t_m + pop_t_dm, pop_t_sd)
  )
for (i in 1:(n_groups-1)) pop[[paste("B", sprintf("%02d",i), sep="")]] = rnorm(N, pop_t_m, pop_t_sd)
str(pop)

# list to df_long
pop_df_long = 
  data.frame(
    x=numeric(0), 
    g=character(0)
  )
for (i in names(pop)) pop_df_long = rbind(pop_df_long, data.frame(x=pop[[i]], g=rep(i, N)))
str(pop_df_long)

# что будем оценивать
# * ошибку первого рода: сколько левых отличий найдено (BX - BY)
# * ошибку второго рода: сколько истинных отличий пропущено (A1-BX, ...)

# что будем сравнивать
# 1. перекрывание доверительных интервалов
# 2. тест Тьюки
# 3. t-test с поправками Бонферрони и других (без учета АНОВА)
# 4. t-test с поправками Бонферрони и других (используя АНОВА для отсеивания случаев без отличий)

# протокол сравнения
# 1. создаем набор выборок (R раз)
# 2. считаем для каждой пары групп достоверность различий согласно каждому из критериев
# 3. считаем для каждого критерия сколько найдено и пропущено истинных различий, а также сколько найдено левых

# вспомогательная функция для вычисления перекрывания дов.интервалов
# функция аналогична rstatix::t_test, но разница м/у группами оценивается пересечением CI
ci_test = function(samples_df){
  
  # group pais
  groups = samples_df$g %>% unique()
  group_pairs = 
    expand.grid(group1=groups, group2=groups) %>%
    mutate_all(as.character) %>%
    filter(group1 < group2) %>%
    arrange(group1, group2)
  
  # confidence intervals calculation
  samples_df_ci = 
    samples_df %>% 
    group_by(g) %>% 
    nest() %>% 
    mutate(mci = map(data, ~ mean_cl_normal(.x$x)),
           mcib = map(data, ~ mean_cl_boot(.x$x))) %>% unnest(mci, mcib) %>%
    select(-data, -y, -y1) %>%
    rename(lci_normal = ymin,
           uci_normal = ymax,
           lci_boot = ymin1,
           uci_boot = ymax1)
  
  # confidence intervals overlapping
  for(ci_type in c("normal", "boot")){
    # generate interval objects
    samples_df_ci_int = 
      samples_df_ci %>%
      select(ends_with(ci_type)) %>%
      as.matrix(ncol=2) %>% 
      intervals::Intervals_full() %>%
      set_rownames(samples_df_ci$g)
    # include overlapping info
    overlaps = rep(NA, nrow(group_pairs))
    for (i in 1:nrow(group_pairs)) {
      overlaps[i] = 
        intervals::interval_overlap(
          samples_df_ci_int[group_pairs$group1[i]], 
          samples_df_ci_int[group_pairs$group2[i]])[[1]] %>% length()
    }
    group_pairs[[paste("p_ci_", ci_type, sep="")]] = overlaps
  }
  return(group_pairs)
} # Usage: ci_test(samples_df)

# пишем функцию, которая все это делает 1 раз для одного набора выборок
do_comparisons = function(pop_df_long, sample_size, output = "logical"){
  # делаем выборки - из каждой группы берем по n=sample_size элементов
  samples_df = pop_df_long %>% group_by(g) %>% sample_n(sample_size) %>% ungroup()
  # t-tests
  samples_df_ttest = 
    samples_df %>% 
    rstatix::t_test(x ~ g, p.adjust.method = "bonferroni") %>% 
    rename(p_t_bonferroni=p.adj, p_t_none=p) %>% 
    select(group1, group2, p_t_none, p_t_bonferroni) %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_holm", method = "holm") %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_hochberg", method = "hochberg") %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_hommel", method = "hommel") %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_hochberg", method = "hochberg") %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_BH", method = "BH") %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_BY", method = "BY") %>%
    rstatix::adjust_pvalue("p_t_none", "p_t_fdr", method = "fdr")
  # wilcoxon tests
  samples_df_wilcox = 
    samples_df %>% 
    rstatix::wilcox_test(x ~ g, p.adjust.method = "bonferroni") %>% 
    rename(p_w_bonferroni=p.adj, p_w_none=p) %>% 
    select(group1, group2, p_w_none, p_w_bonferroni) %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_holm", method = "holm") %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_hochberg", method = "hochberg") %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_hommel", method = "hommel") %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_hochberg", method = "hochberg") %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_BH", method = "BH") %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_BY", method = "BY") %>%
    rstatix::adjust_pvalue("p_w_none", "p_w_fdr", method = "fdr")
  # anova significance
  samples_df_anova =
    samples_df %>% 
    rstatix::anova_test(x ~ g) %>% .$p
  # kruskal test significance
  samples_df_kruskal =
    samples_df %>% 
    rstatix::kruskal_test(x ~ g) %>% .$p
  # tukey test
  samples_df_tukey = 
    samples_df %>% 
    rstatix::tukey_hsd2(x ~ g) %>% 
    select(group1, group2, p.adj) %>%
    rename(p_tukey = p.adj)
  # confidence intervals
  sample_df_ci = 
    samples_df %>%
    ci_test()
  
  # combine everything
  samples_df_combinedtests = samples_df_ttest %>%
    mutate(p_anova = samples_df_anova,
           p_kruskal = samples_df_kruskal) %>%
    full_join(samples_df_wilcox, by = c("group1", "group2")) %>%
    full_join(samples_df_tukey, by = c("group1", "group2")) %>%
    full_join(sample_df_ci, by = c("group1", "group2"))
  
  if (output == "logical") {
    samples_df_combinedtests = 
      samples_df_combinedtests %>%
        mutate_at(vars(starts_with("p_")), ~ ifelse(. < .05, T, F))
  }
  return(samples_df_combinedtests)
}

res = do_comparisons(pop_df_long, 30, output = "logical")

# TO BE CONTINUED
# то же с непараметрическими тестами и критериями 