# CALCULATE SCORE DISTRIBUTIONS

# create one big data frame with indices for each simulted datset

score_distributions <-
  boots_samples %>% 
  select(sample_nr, sim_data) %>% 
  unnest(sim_data) %>%
  # make all variables integer vectors for simplicitys sake
  mutate_all(as.integer) %>% 
  # move to long format tidy data, 3 columns: dataset, item, score
  # numer of rows = n_sample * items * participant
  gather(key = item, value = score, -sample_nr, -group) %>% 
  # Fix the fact that simulation creates scores [1, 2, 3] instead of [0, 1, 2]
  mutate(score = score - 1) %>% 
  # group_by dataset, item, and score 
  group_by(sample_nr, item, score, group) %>% 
  summarise(n = n()) %>% 
  ungroup 


score_distributions %>% group_by(item) %>% 
  
  summarise(chi_test = chisq.test(x = group, y = score))


test <-
  score_distributions %>% 
  group_by(item, score, group) %>% 
  summarise(mean = mean(n), p2.5 = quantile(n, 0.025), p97.5 = quantile(n, 0.975)) %>% 
  group_by(item, group) %>% 
  mutate(proportion = mean/sum(mean), propCIlo = p2.5/sum(mean), propCIup = p97.5/sum(mean)) %>% 
  mutate(propCIlo = ifelse(score == 0, propCIlo,
                           ifelse(score == 1, NA,
                                  1 - propCIlo)),
         propCIup = ifelse(score == 0, propCIup,
                           ifelse(score == 1, NA,
                                  1 - propCIup)))
# summarise( mean, quantile 2.5 and quantile 97.5)
# tibble with 5 columns: dataset, item, mean, q2.5, q97.5

test %>% 
  filter(item == "i_alcEffectHealth") %>% 
  ggplot(aes(as.factor(group))) +
  geom_bar(aes(weight = proportion, fill = ordered(score)), position = position_stack(reverse = TRUE)) +
  geom_errorbar(aes(ymin = propCIlo, ymax = propCIup), width = 0.2) +
  coord_flip() 

