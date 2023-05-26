library(tidyverse)


# all_results <- read_rds('modeling/results/all_models.rds')

  
meta <- read_rds("modeling/meta_results.rds")

data <- read_csv("data_ready/data_ready.csv")

inputs <- c("total_students", "ratio_f2m", "ratio_s2t", "ratio_ft2pt")

countries <- c("hun", "pol", "svk", "cze")

source('stats/analysis_helpers.R')

### betas 


summary_beta <- map(
  countries,
  suffering,
  data = results,
  r = "beta"
) %>% 
  map2(
    .,
    countries, 
    function(x,y){
      x %>% 
        mutate(country = y) %>% 
        relocate(country)
    }
  ) %>% 
  bind_rows()

####################################
## Save Betas

summary_beta %>% 
  write_rds('stats/summary_beta.rds')
  



################################################################################


summary_school_inefficiencies <- map(
    countries,
    suffering_ineff,
    data = results,
    r = 'technical_inefficiency'
  ) %>% 
  map(
    .,
    function(x){
      x %>% 
        mutate(
          value = case_when(
            model == "res1" ~ 2 - value,
            model == "res2" ~ 2 - value,
            model == "res3" ~ value,
            model == "res4" ~ value
          )
        )
    }
  ) %>% 
  map(
    .,
    ineff_summary
  ) %>% 
    map2(
      .,
      countries, 
      function(x,y){
        x %>% 
          mutate(country = y) %>% 
          relocate(country)
      }
    ) %>% 
    bind_rows()
  
  
summary_school_inefficiencies %>% 
  write_rds('stats/summary_inefficiencies.rds')
 
################################################################################ 

summary_lamda <- results %>% 
  filter(
    result == 'lamda' 
  ) %>% 
  select(- c(res3, res4)) %>% 
  pivot_longer(res1:res2) %>% 
  unnest(value) %>% 
  mutate(
    result = rep(c("z1", "z2"), 8)
  ) %>% 
  relocate(country) %>% 
  pivot_wider(names_from = result) %>% 
  rename(model = "name")


summary_lamda %>% 
  write_rds("stats/summary_lamda.rds")


################################################################################

#### meta summary


tmp <- results %>% 
  filter(result == "technical_inefficiency") %>% 
  select(-result) %>% 
  pivot_longer(res1:res4, names_to = "model") %>% 
  unnest(value) %>% 
  mutate(
    value = case_when(
      model == "res1" ~ 2 - value,
      model == "res2" ~ 2 - value,
      model == "res3" ~ value,
      model == "res4" ~ value
    )
  ) %>% 
  ggplot()+
  stat_ecdf(
    aes(
      x = value
    )
  )+ 
  facet_grid(country ~ model)

tmp %>% 
  write_rds("stats/cumulative_inefficiency_country.rds")



################################################################################
### meta beta

meta_beta_summary <- meta %>% 
  filter(result == "beta") %>% 
  select(- result) %>% 
  pivot_longer(res1_all:res4_all, names_to = "model") %>% 
  unnest(value) %>% 
  group_by(model) %>% 
  summarise(
    across(
      V1:V4,
      mean
    )
  )

colnames(meta_beta_summary)[2:5] <- inputs


meta_beta_summary %>% 
  write_rds("stats/meta_beta_summary.rds")


####################
# meta ineffciencies

meta_inefficiency <- meta %>% 
  filter(result == "technical_inefficiency") %>% 
  select(- result) %>% 
  pivot_longer(everything(), names_to = 'model') %>% 
  unnest(value) %>% 
  mutate(
    value = case_when(
      model == "res1_all" ~ 2 - value,
      model == "res2_all" ~ 2 - value,
      model == "res3_all" ~ value,
      model == "res4_all" ~ value
    )
  ) %>% 
  group_by(model) %>% 
  summarise(mean = mean(value), sd = sd(value))
  

meta_inefficiency %>% 
  write_rds('stats/meta_inefficiency_summary.rds')


#######################
# meta lamdas 

meta_lamda <- meta %>% 
  filter(result == "lamda") %>% 
  select(- result) %>% 
  pivot_longer(everything(), names_to = 'model') %>% 
  filter(model %in% c("res1_all", "res2_all")) %>% 
  unnest(value) %>% 
  mutate(
    variable = rep(c("z1", "z2"), 2)
  ) %>% 
  pivot_wider(names_from = variable)

meta_lamda %>% 
  write_rds("stats/meta_lamda.rds")

###############################
# meta cumulative inefficiency 

meta %>% 
  filter(result == "technical_inefficiency") %>% 
  select(- result) %>% 
  pivot_longer(everything(), names_to = 'model') %>% 
  unnest(value) %>% 
  mutate(
    value = case_when(
      model == "res1_all" ~ 2 - value,
      model == "res2_all" ~ 2 - value,
      model == "res3_all" ~ value,
      model == "res4_all" ~ value
    )
  ) %>% 
  group_by(model) %>% 
  ggplot() +
  stat_ecdf(
    aes(
      x = value
    )
  )+
  facet_grid(model~.)+
  labs(
    title = "Cumulative distribution of the inefficiency estimate per model",
    x = "inefficiency level",
    y = "CDF"
  ) %>% 
  write_rds("stats/cumulative_meta.rds")
