library(readxl)
library(tidyverse)

# Compute years lost by risk group using actuarial tables {{{

lost.years <- 
  lapply(c('M', 'Z'), function(gend) {
    # Load Czech actuarial table for Males/Females (documentation at
    # http://www.czso.cz/documents/10180/85591762/metodika_ut_akt2020.pdf)
    fl <- paste0('Input_data/UT_Kannisto_2019', gend, '.xlsx')
    life <- 
      read_xlsx(fl, skip = 2) %>%
        select(1, lx, dx, mx, Lx)
      names(life)[1] <- 'age'
    lapply(seq(12, 102, 5), function(died.at.age) {
    life %>%
        filter(age >= died.at.age) %>%
        mutate(gender = gend,
               died.at.age = died.at.age,
               risk.group = lead(1 - lx / max(lx), default = 1),
               prob.death = diff(c(0, risk.group)),
               # dx = c(dx[-n()], dx[n()] / mx[n()]),
               # years.lost.corr = 
               #   cumsum((age - died.at.age) * prob.death) / 
               #     risk.group + 0.5,
               years.lost.new = cumsum(rev(Lx)) /
                             lx + 0.5,
               years.lost = (cumsum(dx * (age - died.at.age + 1)) /
                             cumsum(dx)) - 0.5
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
       years.lost.new,
       years.lost.by.risk.group = years.lost
       ) 

# # Save the result
# lost.years %>%
# mutate_if(is.numeric, round, 6) %>%
# mutate_all(as.character) %>%
#   write_csv('lost_years_by_risk_group.csv')

# Compare lost years and official expectancy {{{1

life.raw <- 
  read_xlsx('UT_Kannisto_2019M.xlsx', skip = 2) %>%
    select(1, , ex) %>% 
    mutate(gender = 'Male') %>%
    bind_rows(read_xlsx('UT_Kannisto_2019Z.xlsx', skip = 2) %>%
              select(1, ex) %>%
              mutate(gender = 'Female')
            )
names(life.raw)[1] <- 'age'
life.raw <- life.raw %>%
  filter(age <= 105)

lost.years %>%
  group_by(died.at.age, gender) %>% 
  # filter(risk.group <=0.5) %>%
  slice_tail %>%
    ungroup %>%
  select(gender, age = died.at.age, starts_with('years.lost')) %>%
  left_join(life.raw) %>% 
  arrange(gender, age) %>%
  group_by(gender) %>% 
  # group_by(gender, age > 70) %>% 
  # summarize_all(mean)
  print(n=50)

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
  # filter(died.at == 0) %>%
  filter(risk.group <= 0.2) %>%
  group_by(died.at.age, gender) %>%
  slice_tail %>%
  print(n=50)
  left_join(weights) %>%
  filter(!is.na(weight)) %>%
  group_by(gender) %>%
  summarize(sum(years.lost * weight))
