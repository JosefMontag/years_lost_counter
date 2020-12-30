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

# Check consistency: Compare lost years for risk group 1 with official e_x {{{1

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

# Compute Covid-related lost years {{{1

# Get UZIS data
uzis_data <- read_csv('https://raw.githubusercontent.com/JosefMontag/years_lost_counter/main/Input_data/covid_cases_and_deaths.csv')

# Compute age structure of covid deaths
weights <- uzis_data %>%
   filter(!is.na(tyden_umrti)) %>%
   mutate(vek = as.integer(str_replace(vek_kat, '-.*', '')) + 2.5) %>%
   select(
       gender = pohlavi,
       died.at.age = vek
   ) %>%
   mutate(
       died.at.age = as.integer(floor(died.at.age)),
       gender = ifelse(gender == "M","Male","Female")
   ) %>%
   group_by(gender) %>%
   add_tally() %>%
   group_by(gender, died.at.age) %>%
   summarise(
       weight = n()/first(n),
       .groups = "drop"
   )


lost.years %>%
  filter(risk.group <= 0.01) %>%
  group_by(died.at.age, gender) %>%
  slice_tail %>%
  left_join(weights) %>%
  filter(!is.na(weight)) %>%
  group_by(gender) %>%
  summarize(sum(years.lost.by.risk.group * weight))

# Play

uzis_data %>%
  filter(!is.na(tyden_umrti)) %>%
   mutate(vek = as.integer(str_replace(vek_kat, '-.*', '')) + 2.5) %>%
  group_by(tyden_umrti, pohlavi) %>%
  summarize(vek = mean(vek)) %>%
  ggplot(aes(x = tyden_umrti, y = vek, color = pohlavi)) +
  geom_line()
