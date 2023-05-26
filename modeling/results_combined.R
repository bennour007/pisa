library(reticulate)

reticulate::use_condaenv("papyr2", required = T)

py_run_file(file = "modeling/call_py_objects.py")

all_res <- py$l

res_all <- py$res_all

################################################################################
# model_1
library(tidyverse)

model_names <- c("res1_hun", "res2_hun", "res3_hun", "res4_hun")
country_names <- c("svk", "hun", "cze", "pol")

all_res_list <- all_res %>% 
  map_depth(., 3, as_tibble, .name_repair = "universal") %>% 
  map_depth(., 1, as_tibble, .name_repair = "universal") %>% 
  map(
    ., 
    function(x){
      x %>% 
      mutate(
        result = c("alpha", "beta", "delta", "gamma", "lamda", "residuals", "frontier", "unconditional_expected_inefficiency", "technical_inefficiency")
      ) %>% 
      relocate(result)
    }
  ) %>% 
  map2(
    ., 
    country_names,
    function(l,n){
      l %>% 
        mutate(
          country = n
        ) %>% 
        relocate(country, .after = result)
    }
  )


renamer <- function(x){
  colnames(x)[3:6] <- str_sub(colnames(x)[3:6], start = 1, end = -5)
  return(x)
}



results <- all_res_list %>% 
  map(.,
      renamer
  ) %>% 
  bind_rows()




results %>% 
  write_rds("modeling/all_results.rds")



res_all_clean <- res_all %>% 
  map_depth(., 2, as_tibble) %>% 
  as_tibble() %>% 
  mutate(
    result = c("alpha", "beta", "delta", "gamma", "lamda", "residuals", "frontier", "unconditional_expected_inefficiency", "technical_inefficiency")
  ) %>% 
  relocate(result)

res_all_clean %>% 
  write_rds("modeling/meta_results.rds")
  
  