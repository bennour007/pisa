library(tidyverse)



results <- read_rds("modeling/results/all_results.rds")

data <- read_csv("data_ready/data_ready.csv")

inputs <- c("total_students", "ratio_f2m", "ratio_s2t", "ratio_ft2pt")

### betas 

beta_renamer <- function(x){
  colnames(x) <- inputs
  return(x)
}

synth <- function(x){
  x %>% 
    summarise(mean)
}


## hun



beta_hun <- results %>% 
  filter(country == 'hun' & result == 'beta') %>% 
  select(res1:res4) %>% 
  list() %>% 
  flatten() %>% 
  flatten() %>% 
  map(
    ., 
    beta_renamer
  ) %>% 
  map2(.,
       c(1:4),
    function(x,y){
      x %>% 
        mutate(
          model = glue::glue("res{y}")
        )
    }
  ) %>% 
  bind_rows() %>% 
  group_by(model) %>% 
  summarise(
    across(
      everything(),
      mean
    )
  )




suffering <- function(data, c, r) {
  data %>% 
    filter(
      country == c & result == r
    ) %>% 
    select(res1:res4) %>% 
    list() %>% 
    flatten() %>% 
    flatten() %>% 
    map(
      ., 
      beta_renamer
    ) %>% 
    map2(.,
         c(1:4),
         function(x,y){
           x %>% 
             mutate(
               model = glue::glue("res{y}")
             )
         }
    ) %>% 
    bind_rows() %>% 
    group_by(model) %>% 
    summarise(
      across(
        everything(),
        mean
      )
    ) %>% 
    ungroup()
}

countries <- c("hun", "pol", "svk", "cze")

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
  

summary_beta %>% 
  group_by(country) %>% 
  mutate(
    across(
      total_students:ratio_ft2pt,
      sd
    )
  )


suffering_ineff <- function(data, c, r) {
  data %>% 
    filter(
      country == c & result == r
    ) %>% 
    select(res1:res4) %>% 
    list() %>% 
    flatten() %>% 
  
    flatten() %>%
    map2(.,
         c(1:4),
         function(x,y){
           x %>%
             mutate(
               model = glue::glue("res{y}")
             )
         }
    ) %>%
    bind_rows() %>%
    group_by(model) %>%
    summarise(
      across(
        everything(),
        mean
      )
    ) %>%
    ungroup()
}


suffering_frontier <- function(data, c, r) {
  data %>% 
    filter(
      country == c & result == r
    ) %>% 
    select(res1:res2) %>% 
    list() %>% 
    flatten() %>% 
    
    flatten() %>%
    map2(.,
         c(1:2),
         function(x,y){
           x %>%
             mutate(
               model = glue::glue("res{y}")
             )
         }
    ) %>%
    bind_rows() 
    # group_by(model) %>%
    # summarise(
    #   across(
    #     everything(),
    #     mean
    #   )
    # ) %>%
    # ungroup()
}

  map(
    countries,
    suffering_ineff,
    data = results,
    r = 'technical_inefficiency'
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
  
  
  
  map(
    countries,
    suffering_ineff,
    data = results,
    r = 'residuals'
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
  
 
  
 frontier_data <- map(
    countries,
    suffering_frontier,
    data = results %>% select(-res4, -res3),
    r = 'frontier'
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

 input_data <- data %>% 
   select(
     CNT, 
     all_of(inputs)
   ) %>% 
   pivot_longer(total_students:ratio_ft2pt, names_to = 'input', values_to = 'in_val')
 
 input_data %>% 
   filter(input == 'total_students') %>% 
   na.omit()
   left_join(
     frontier_data %>% na.omit(),
     by =c("CNT" = "country")
   )
   ggplot(
      aes(
        y = in_val
      )
   )+
   geom_point(
     data = frontier_data %>% filter(country == 'pol'),
     aes(
       x = value
     )
   ) + 
   facet_grid(model ~ input)

 frontier_data 






  

