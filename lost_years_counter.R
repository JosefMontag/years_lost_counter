library(readxl)
library(tidyverse)
rm(list=ls())

# Compute years lost by risk group using actuarial tables {{{

lost.years <- 
  lapply(c('M', 'Z'), function(gend) {
    life <- read_csv(paste0('Input_data/UT_Kannisto_2019', gend, '.csv')) %>%
        select(1, lx, dx, mx, Lx)
      names(life)[1] <- 'age'
    lapply(seq(12, 102, 5), function(died.at.age) {
    life %>%
        filter(age >= died.at.age) %>%
        mutate(gender = gend,
               died.at.age = died.at.age,
               risk.group = cumsum(dx) / sum(dx),
               prob.death = diff(c(0, risk.group)),
               ax = (Lx - lx + dx) / dx,
               ax = ifelse(age %in% 1:104, round(ax, 1), ax),
               potential.years = (age - died.at.age + ax),
               years.lost = cumsum(potential.years * dx) / cumsum(dx)
               )
      }) %>%
      bind_rows
  }) %>%
  bind_rows %>%
mutate(gender = ifelse(gender == 'M', 'Male', 'Female')) %>%
select(gender,
       died.at.age,
       prob.death,
       risk.group,
       alt.age.death = age, 
       lx,
       dx,
       ax,
       potential.years,
       years.lost.by.risk.group = years.lost
       ) 
lost.years

# Save the result
lost.years %>%
mutate_if(is.numeric, round, 6) %>%
mutate_all(as.character) %>%
  write_csv('lost_years_by_risk_group.csv')

# Consistency checks: {{{1

# Check years lost of highest risk groups are equal to 0.5 (a_x) {{{2

lost.years %>%
  group_by(gender, died.at.age) %>%
  filter(risk.group == min(risk.group)) %>%
  print(n = nrow(.))

# Compare lost years for risk group 1 with official e_x {{{2

life.raw <- 
  read_csv('Input_data/UT_Kannisto_2019M.csv') %>%
    select(1, , ex) %>% 
    mutate(gender = 'Male') %>%
    bind_rows(read_csv('Input_data/UT_Kannisto_2019Z.csv') %>%
              select(1, ex) %>%
              mutate(gender = 'Female')
            )
names(life.raw)[1] <- 'age'

lost.years %>%
  filter(risk.group == 1) %>%
  select(gender, age = died.at.age, starts_with('years.lost')) %>%
  left_join(life.raw) %>% 
  arrange(gender, age) %>%
  group_by(gender) %>%
  mutate(diff = years.lost.by.risk.group - ex) %>%
  print(n = nrow(.))
