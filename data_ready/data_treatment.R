library(Benchmarking)
library(tidyverse)

data <- read_csv("data_ready/data_4_treatment.csv")

data_raw <- data %>% 
  mutate(
    bol_location = if_else( ## 0 is rural, 1 is urban
      SC001Q01TA %in% c(1,2),
      1, 
      0
    ),
    funding = SC016Q01TA/100, ## show pct of gov funding 
    bol_career_guidance = if_else( ## if 1 guidance is voluntary, else is formal
      SC162Q01SA == 1, 
      1,
      0
    ),
    bol_competition = if_else( ## if there's other schools 1, else 0
      SC011Q01TA != 3,
      1,
      0
    ),
    # foreign_language <- if_else( ## is there a formal foreign language class in the school, but that's everywhere. ABORT MISSION.
    #   SC167Q05HA == 1, 
    #   1,
    #   0
    # )
    students_male = SC002Q01TA,
    students_female = SC002Q02TA,
    teachers_ft = SC018Q01TA01,
    teachers_pt = SC018Q01TA02,
    dropout_rate = SC164Q01HA/100
  ) %>% 
  select(
    CNT,
    CNTSCHID, 
    bol_location,
    funding,
    students_male,
    students_female,
    teachers_ft, 
    teachers_pt,
    bol_competition,
    bol_career_guidance,
    dropout_rate,
    starts_with("avg"),
    starts_with("med"),
    starts_with("SC053"),
    starts_with("SC064")
  )

parental <- data_raw %>% 
  select(
    starts_with("SC064")
  ) %>% 
  colnames()

extra_activities <- data_raw %>% 
  select(
    starts_with("SC053")
  ) %>% 
  colnames

data_clean <- data_raw %>% 
  mutate(
    ap_parental_eng = rowMeans(.[, parental], na.rm = T),
    ap_parental_eng = ap_parental_eng / 100,
    total_students = students_male + students_female,
    ratio_f2m = students_female / students_male,
    total_teachers = teachers_ft + teachers_pt,
    ratio_pt2ft = teachers_pt / teachers_ft, ## added this variable, take it into account in modeling
    ratio_ft2pt = teachers_ft / teachers_pt,
    ratio_t2s = total_teachers / total_students, ## change the name here and in the modeling
    ratio_s2t = total_students / total_teachers,
    bol_extra_acts = if_else(
      SC053Q01TA == 1 | SC053Q02TA == 1 | SC053Q03TA == 1 | SC053Q04TA == 1 | 
        SC053Q12IA == 1 | SC053Q13IA == 1 | SC053Q09TA == 1 | SC053Q10TA == 1 |
        SC053Q14IA == 1 | SC053Q15IA == 1 | SC053Q16IA == 1 | SC053D11TA == 1 ,
      1,
      0
    )
  ) %>% 
  select(
    CNT,
    CNTSCHID, 
    bol_location,
    bol_extra_acts,
    bol_competition,
    bol_career_guidance,
    funding,
    students_male,
    students_female,
    teachers_ft, 
    teachers_pt,
    total_students,
    total_teachers,
    ratio_f2m,
    ratio_pt2ft,
    ratio_ft2pt,
    ratio_t2s,
    ratio_s2t,
    ap_parental_eng,
    dropout_rate,
    starts_with("avg"),
    starts_with("med")
  )

data_clean %>% 
  write_csv("data_ready/data_ready.csv")

