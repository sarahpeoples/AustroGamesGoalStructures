library(parallel)
library(devtools)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(rethinking)
library(RColorBrewer)
library(ape)
library(phytools)
library(ggridges)
library(ggtree)
library(ggplot2)
library(png)
library(tidyverse)



devtools::install_github("ccp-eva/AustroGames", ref = "main")
library(AustroGames)


Softmax <- function(x) { # Softmax link function
X <-exp(x)               #
return(X/sum(X))         #
}


source.utf8 <- function(f) {
    l <- readLines(f, encoding="UTF-8")
    eval(parse(text=l),envir=.GlobalEnv)
}


reduceR <- function(x, sep=";"){
	if( length(x)==1){
		return(x)
	} else{
		g <- x[1]
		for(i in 2:length(x))
			g <- paste(g, x[i], sep=sep)
		return(g)
	}
}

