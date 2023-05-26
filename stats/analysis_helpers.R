beta_renamer <- function(x){
  colnames(x) <- inputs
  return(x)
}

synth <- function(x){
  x %>% 
    summarise(mean)
}


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
    bind_rows()
    
}

ineff_summary <- function(x){
  x %>% 
  group_by(model) %>%
  summarise(
    across(
      everything(),
      list(mean = mean, sd = sd)
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
  
}