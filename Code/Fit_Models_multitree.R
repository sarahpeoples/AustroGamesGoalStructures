

#####################################
trees <- list()
for(j in 1:10){
d1 <- read.nexus(paste0("Data/phylogeny_random10_Greenhill/",j,".trees"))

d2<-cophenetic.phylo(d1)                                                  #
 d3 <- d2/max(d2)                                                          #
 treeNames <- as.character(colnames(d3))                                   #

 d3b <- d3[which(treeNames %in% d_final$Phylo_TreeTaxaName),which(treeNames %in% d_final$Phylo_TreeTaxaName)]  

 d3c <- d3b[d_final$Phylo_TreeTaxaName,d_final$Phylo_TreeTaxaName]

trees[[j]] <- d3c
}

for(j in 1:10)
print(rownames(trees[[j]])==rownames(sTree))

###########################################################
# Base model
data_list <- vector("list", 10)
model_dat$Q <- c(1,0,0,0,0,0,0)
for(i in 1:10){
data_list[[i]] <- model_dat
data_list[[i]]$D <- trees[[i]]^2
}

 smt <- stan_model(file="Code/Model_Code_PhyloControl.stan")

fit0f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit0f_mt <- sflist2stanfit(fit0f_mt)
print("mt0")

# Land model
data_list <- vector("list", 10)
model_dat$Q <- c(1,1,0,0,0,0,0)
for(i in 1:10){
data_list[[i]] <- model_dat
data_list[[i]]$D <- trees[[i]]^2
}

fit1f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit1f_mt <- sflist2stanfit(fit1f_mt)
print("mt1")

# Water model
data_list <- vector("list", 10)
model_dat$Q <- c(1,0,1,0,0,0,0)
for(i in 1:10){
data_list[[i]] <- model_dat
data_list[[i]]$D <- trees[[i]]^2
}

fit2f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit2f_mt <- sflist2stanfit(fit2f_mt)
print("mt2")

# Between model
data_list <- vector("list", 10)
model_dat$Q <- c(1,0,0,1,0,0,0)
for(i in 1:10){
	data_list[[i]] <- model_dat
	data_list[[i]]$D <- trees[[i]]^2
}

fit3f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit3f_mt <- sflist2stanfit(fit3f_mt)
print("mt3")

# Within model
data_list <- vector("list", 10)
model_dat$Q <- c(1,0,0,0,1,0,0)
for(i in 1:10){
	data_list[[i]] <- model_dat
	data_list[[i]]$D <- trees[[i]]^2
}

fit4f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit4f_mt <- sflist2stanfit(fit4f_mt)
print("mt4")

# Other model
data_list <- vector("list", 10)
model_dat$Q <- c(1,0,0,0,0,1,0)
for(i in 1:10){
	data_list[[i]] <- model_dat
	data_list[[i]]$D <- trees[[i]]^2
}

fit5f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit5f_mt <- sflist2stanfit(fit5f_mt)
print("mt5")

# Strat model
data_list <- vector("list", 10)
model_dat$Q <- c(1,0,0,0,0,0,1)
for(i in 1:10){
	data_list[[i]] <- model_dat
	data_list[[i]]$D <- trees[[i]]^2
}

fit6f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit6f_mt <- sflist2stanfit(fit6f_mt)
print("mt6")

# Within model and land foraging
data_list <- vector("list", 10)
model_dat$Q <- c(1,1,0,0,1,0,0)
for(i in 1:10){
	data_list[[i]] <- model_dat
	data_list[[i]]$D <- trees[[i]]^2
}

fit7f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit7f_mt <- sflist2stanfit(fit7f_mt)
print("mt7")

# Other model and land foraging
data_list <- vector("list", 10)
model_dat$Q <- c(1,1,0,0,0,1,0)
for(i in 1:10){
	data_list[[i]] <- model_dat
	data_list[[i]]$D <- trees[[i]]^2
}

fit8f_mt <- mclapply(1:10, function(z){
	sampling(object = smt, data = data_list[[z]], iter = iter, warmup = warmup, cores = cores, chains = nchains, control = list(max_treedepth = max_treedepth, adapt_delta = adapt_delta))
	}, 
	mc.cores = 1)
fit8f_mt <- sflist2stanfit(fit8f_mt)
print("mt8")
 