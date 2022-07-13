library(sae, exclude = "select")
library(tidycensus)
library(tidyverse)

census_api_key("9fb9cc8584a00b7006e7550c685129b65240b9aa")

# --------------- LOAD POVERTY DATA ---------------
selected_states = c("MN",  "ND", "SD", "WI")  #"MN",  "ND", "SD",
## Population counts
pop <- get_acs(geography = "county", 
               variables = "B01003_001", 
               year = 2019,
               state = selected_states)

## Poverty counts
pov <- get_acs(geography = "county", 
               variables = "B17001_002",
               year = 2019,
               state = selected_states)


poverty <- data.frame(fips=pop$GEOID, county=pop$NAME, pop=pop$estimate, pov=pov$estimate, SE=pov$moe/1.645) %>%
  mutate(povPerc=pov/pop, povPercSE=SE/pop)
#View(poverty)
#poverty %>% head()

# --------------- LOAD RACE DATA ---------------
race = get_acs(geography = "county", 
               variables = c(total="B02001_001", white="B02001_002", 
                             black="B02001_003", native="B02001_004", 
                             asian="B02001_005", 
                             other1="B02001_006", other2="B02001_007", 
                             mult1="B02001_008", mult2="B02001_009", 
                             mult3="B02001_010"), 
               state = selected_states, 
               year = 2019)

race = race %>% 
  select(NAME, variable, estimate) %>% 
  spread(key=variable, value=estimate ) %>% 
  mutate(other=other1+other2+mult1+mult2+mult3) %>% 
  select(-other1, -other2, -mult1, -mult2, -mult3) %>% 
  relocate(NAME, total, white, native, black, asian, other) %>% 
  mutate(white=white/total, native=native/total, black=black/total, 
         asian=asian/total, other=other/total)
race = race %>% select (NAME, white, native, black, asian, other)
#race %>% head()


# --------------- LOAD LATINO DATA ---------------

latino = get_acs(geography = "county", 
                 variables = c(non_hispanic="B08134_001", hispanic="B08134_010"), 
                 state = selected_states, 
                 year = 2019)

latino = latino %>% 
  select(NAME, variable, estimate) %>% 
  spread(key=variable, value=estimate ) %>% 
  mutate(total = non_hispanic+hispanic) %>% 
  mutate(non_hispanic=non_hispanic/total, hispanic= hispanic/total) %>% 
  select(-total)
#latino %>% head()

# ---------------  Households with Children/Marital Status  ---------------

parents = get_acs(geography = "county", 
                  variables = c(married = "B09002_002", 
                                male_only = "B09002_009", 
                                female_only = "B09002_015"), 
                  state = selected_states, 
                  year = 2019)
parents = parents %>% 
  select(NAME, variable, estimate) %>% 
  spread(key=variable, value=estimate ) %>% 
  mutate(total=female_only + male_only + married) %>% 
  mutate(married=married/total, male_only = male_only/total, female_only=female_only/total) %>% 
  select(-total)

# ---------------  Vehicle data  ---------------

vehicle = get_acs(geography = "county",
                  variables = c(no_car = "B08014_002", total = "B08014_001"),
                  state = selected_states,
                  year = 2019)

vehicle = vehicle %>%
  select(NAME, variable, estimate) %>%
  spread(key=variable, value=estimate ) %>%
  mutate(no_car=no_car/total) %>%
  select(-total)

# ---------------  Bachelor's Degree  ---------------


college = get_acs(geography = "county",
                  variables = c(degree = "B06009_005", total = "B06009_001"),
                  state = selected_states,
                  year = 2019)

college = college %>%
  select(NAME, variable, estimate) %>%
  spread(key=variable, value=estimate ) %>%
  mutate(degree=degree/total) %>%
  select(-total)


# ---------------  combing the covariates  ---------------
X = race %>% 
  left_join(latino) %>% 
  left_join(parents) %>% 
  left_join(vehicle) %>%
  left_join(college) %>%
  relocate(county=NAME)  

# combining everything
all_data = poverty %>% left_join(X) 
all_data %>% summary()

save(all_data, file="all_data.RDA")