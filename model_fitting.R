library(rstan); library(dplyr)
load("all_data.RDA")
all_data = all_data %>% select(-non_hispanic, -married, -other)

#------------ DESIGN MATRIX ------------  
# Fit one model with 1-9 another with 1-6

# 1. white
# 2. native
# 3. black 
# 4. hispanic 
# 5. BA degree holders  
# 6. female_only single parent household
# 7. asian
# 8. male_only single parent household
# 9. No car
xi.large = all_data[, 8:ncol(all_data)] 
xi.small = xi.large %>% select(-asian, -male_only, -no_car)

X.l = model.matrix(~., xi.large )
X.s = model.matrix(~., xi.small )


#------------ LARGE MODEL ------------  

stan_inputs = list(
  K=nrow(all_data),
  P=ncol(xi.large),
  X=X.l, 
  y=all_data$povPerc,
  D=all_data$povPercSE
)

m1.fit <- stan(file = "fay-harriot.stan", 
                   data=stan_inputs,
                   iter=10000, chains=4, 
                   warmup=8000,   save_warmup = FALSE,
                   control=list(adapt_delta = 0.85, 
                                stepsize = 0.001, max_treedepth = 20))

#save(m1.fit, file="stan_results.RDA")
#------------ Smaller Model ------------  

stan_inputs = list(
  K=nrow(all_data),
  P=ncol(xi.small),
  X=X.s,
  y=all_data$povPerc,
  D=all_data$povPercSE
)


m2.fit <- stan(file = "fay-harriot.stan",
               data=stan_inputs,
               iter=10000, chains=3,
               warmup=8000,   save_warmup = FALSE,
               control=list(adapt_delta = 0.85,
                            stepsize = 0.001, max_treedepth = 20))


# #------------ SAVE RESULTS ------------
# 
save(m1.fit, m2.fit, file="stan_results.RDA")
