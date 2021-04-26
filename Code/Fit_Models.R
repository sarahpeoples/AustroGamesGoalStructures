Q <- rep(0,7)

# model_dat=list(                  ##          
#  N=N,                            ##        
#  P=P,                            ##        
#  G=G,                            ##          
#  Q=Q,                            ##          
#  Land=Land,                      ##          
#  Water=Water,                    ## 
#  Con_Between=Con_Between,        ##          
#  Con_Within=Con_Within,          ##          
#  Con_Other=Con_Other,            ##
#  Strat=Strat,                    ## 
#  D = sTree^2,                    ##            
#  Games=Games                     ##          
#  )                               ##

########################################################### First, univariate models
 sm <- stan_model(file="Code/Model_Code.stan")
# Base model
model_dat$Q <- c(1,0,0,0,0,0,0)
fit0 <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base0")
# Land model
model_dat$Q <- c(1,1,0,0,0,0,0)
fit1 <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base1")
# Water model
model_dat$Q <- c(1,0,1,0,0,0,0)
fit2<- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base2")
# Between model
model_dat$Q <- c(1,0,0,1,0,0,0)
fit3 <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base3")
# Within model
model_dat$Q <- c(1,0,0,0,1,0,0)
fit4 <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base4")
# Other model
model_dat$Q <- c(1,0,0,0,0,1,0)
fit5 <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base5")
# Strat model
model_dat$Q <- c(1,0,0,0,0,0,1)
fit6 <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base6")
########################################################### Then control for cooperative land foraging
# Within model plus land
model_dat$Q <- c(1,1,0,0,1,0,0)
fit4b <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base7")
# Other model plus land
model_dat$Q <- c(1,1,0,0,0,1,0)
fit5b <- sampling(object = sm, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("base8")

############################################################################################################ With Phylo Control
########################################################### First, univariate models
 smp <- stan_model(file="Code/Model_Code_PhyloControl.stan")
# Base model
model_dat$Q <- c(1,0,0,0,0,0,0)
fit0f <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo0")
# Land model
model_dat$Q <- c(1,1,0,0,0,0,0)
fit1f <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo1")
# Water model
model_dat$Q <- c(1,0,1,0,0,0,0)
fit2f<- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo2")
# Between model
model_dat$Q <- c(1,0,0,1,0,0,0)
fit3f <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo3")
# Within model
model_dat$Q <- c(1,0,0,0,1,0,0)
fit4f <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo4")
# Other model
model_dat$Q <- c(1,0,0,0,0,1,0)
fit5f <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo5")
# Strat model
model_dat$Q <- c(1,0,0,0,0,0,1)
fit6f <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo6")
########################################################### Then control for cooperative land foraging
# Within model
model_dat$Q <- c(1,1,0,0,1,0,0)
fit4bf <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo7")
# Other model
model_dat$Q <- c(1,1,0,0,0,1,0)
fit5bf <- sampling(object = smp, data=model_dat, iter = iter, warmup=warmup, chains = nchains, control=list(adapt_delta=adapt_delta, max_treedepth=max_treedepth))
print("phylo8")
