################################################################################
# Games analysis

########################### Set Directory
setwd("")

########################### Load packages and functions
source("Code/Project_Support.R")

nchains = 3 
warmup = 3000
iter = 6000
adapt_delta=0.99
max_treedepth=15
cores = parallel::detectCores()

########################### Simulate data
source("Code/Simulate_Data.R")

########################### Test model
source("Code/Test_Models.R")

########################### Process data for Stan
source("Code/Data_preprocessing.R")			    	
load("Data/model_data.RData")                  

########################### Fit models in Stan
source("Code/Fit_Models.R")

########################### Make Table
source("Code/Make_Table.R")

########################## Make Plots
source("Code/Predictive_Plots.R")
source ("Code/Plots_Paper.R")
source ("Code/Tables_Paper.R")

#### Extras
nchains = 1 
warmup = 2000
iter = 4000
adapt_delta=0.99
max_treedepth=14
source("Code/Fit_Models_multitree.R")

source("Code/Predictive_Plots_multitree.R")

########################### Save R workspace
save.image("AustroGames.RData")

