
table_games <- function(Beta,Q,M,L=-1){
  Pred <- array(NA,c((iter-warmup)*nchains,G,2))
  Theta<-rep(NA,G)

  ############################################################### Make Predictions
  for( q in 1:((iter-warmup)*nchains)){# Iterate over MCMC samples
  ########################## 
  for(g in 1:G-1){
   if(L==-1){ Theta[g] <- Beta[q,g,1];  }
   if(L==0 ){ Theta[g] <- Beta[q,g,1];  }
   if(L==1){  Theta[g] <- Beta[q,g,1] + Beta[q,g,2];  }
   }
   Theta[G] <- 0
   Pred[q,,1] <- Softmax(Theta)
 
  ########################## 
   for(g in 1:G-1){
    if(L==-1){Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2]   + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7]; } 
    if(L==0){ Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2]*0 + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7]; } 
    if(L==1){ Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2]*1 + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7]; } 
    }
   Theta[G] <- 0

   Pred[q,,2] <- Softmax(Theta)
   }
          
 Contrast2 <- log(Pred[,3,2] / Pred[,2,2]) - log(Pred[,3,1] / Pred[,2,1])


  ZZ <- HPDI(Contrast2,prob=c(0.9))
  ZZ2 <- median(Contrast2,prob=c(0.9))

  ZZ3 <- paste0(format(ZZ2, digits=2, nsmall=2), " (", format(ZZ, digits=2, nsmall=2)[1],"; ", format(ZZ, digits=2, nsmall=2)[2],")")
   return(ZZ3)
}


table_games_O <- function(Beta,Q,M,O=-1){
  Pred <- array(NA,c((iter-warmup)*nchains,G,2))
  Theta<-rep(NA,G)

  ############################################################### Make Predictions
  for( q in 1:((iter-warmup)*nchains)){# Iterate over MCMC samples
  ########################## 
  for(g in 1:G-1){
   if(O==-1){ Theta[g] <- Beta[q,g,1];  }
   if(O==0 ){ Theta[g] <- Beta[q,g,1];  }
   if(O==1){  Theta[g] <- Beta[q,g,1] + Beta[q,g,6];  }
   }
   Theta[G] <- 0
   Pred[q,,1] <- Softmax(Theta)
 
  ########################## 
   for(g in 1:G-1){
    if(O==-1){Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6]   + Beta[q,g,7]*Q[7]; } 
    if(O==0){ Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6]*0 + Beta[q,g,7]*Q[7]; } 
    if(O==1){ Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6]*1 + Beta[q,g,7]*Q[7]; } 
    }
   Theta[G] <- 0

   Pred[q,,2] <- Softmax(Theta)
   }
          
 Contrast2 <- log(Pred[,3,2] / Pred[,2,2]) - log(Pred[,3,1] / Pred[,2,1])


  ZZ <- HPDI(Contrast2,prob=c(0.9))
  ZZ2 <- median(Contrast2,prob=c(0.9))

  ZZ3 <- paste0(format(ZZ2, digits=2, nsmall=2), " (", format(ZZ, digits=2, nsmall=2)[1],"; ", format(ZZ, digits=2, nsmall=2)[2],")")
   return(ZZ3)
}

table_games_W <- function(Beta,Q,M,W=-1){
  Pred <- array(NA,c((iter-warmup)*nchains,G,2))
  Theta<-rep(NA,G)

  ############################################################### Make Predictions
  for( q in 1:((iter-warmup)*nchains)){# Iterate over MCMC samples
  ########################## 
  for(g in 1:G-1){
   if(W==-1){ Theta[g] <- Beta[q,g,1];  }
   if(W==0 ){ Theta[g] <- Beta[q,g,1];  }
   if(W==1){  Theta[g] <- Beta[q,g,1] + Beta[q,g,5];  }
   }
   Theta[G] <- 0
   Pred[q,,1] <- Softmax(Theta)
 
  ########################## 
   for(g in 1:G-1){
    if(W==-1){Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5]   + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7]; } 
    if(W==0){ Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5]*0 + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7]; } 
    if(W==1){ Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5]*1 + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7]; } 
    }
   Theta[G] <- 0

   Pred[q,,2] <- Softmax(Theta)
   }
          
 Contrast2 <- log(Pred[,3,2] / Pred[,2,2]) - log(Pred[,3,1] / Pred[,2,1])


  ZZ <- HPDI(Contrast2,prob=c(0.9))
  ZZ2 <- median(Contrast2,prob=c(0.9))

  ZZ3 <- paste0(format(ZZ2, digits=2, nsmall=2), " (", format(ZZ, digits=2, nsmall=2)[1],"; ", format(ZZ, digits=2, nsmall=2)[2],")")
   return(ZZ3)
}


ResN <- matrix("",ncol=2,nrow=18)

colnames(ResN) <- c("Predictor 1", "Predictor 2")
rownames(ResN) <- c("Base model","Interdependence land", "Interdependence water","Intra-group conflict","Intra-cultural conflict","Inter-cultural conflict","Social stratification", "Intra-group conflict (Interdependence land)","Inter-cultural conflict (Interdependence land)", 
 "Base model","Interdependence land", "Interdependence water","Intra-group conflict","Intra-cultural conflict","Inter-cultural conflict","Social stratification",
 "Intra-group conflict (Interdependence land)","Inter-cultural conflict (Interdependence land)")

################################################################################### Standard Models
ResN[1,1] <- ""
ResN[2,1] <- table_games(Beta=rstan::extract(fit1,"Beta")$Beta, Q=c(1,1,0,0,0,0,0), M="Land", L=-1)
ResN[3,1] <- table_games(Beta=rstan::extract(fit2,"Beta")$Beta, Q=c(1,0,1,0,0,0,0), M="Water", L=-1)
ResN[4,1] <- table_games(Beta=rstan::extract(fit4,"Beta")$Beta, Q=c(1,0,0,0,1,0,0), M="Within", L=-1)
ResN[5,1] <- table_games(Beta=rstan::extract(fit3,"Beta")$Beta, Q=c(1,0,0,1,0,0,0), M="Between", L=-1)
ResN[6,1] <- table_games(Beta=rstan::extract(fit5,"Beta")$Beta, Q=c(1,0,0,0,0,1,0), M="Other", L=-1)
ResN[7,1] <- table_games(Beta=rstan::extract(fit6,"Beta")$Beta, Q=c(1,0,0,0,0,0,1), M="Strat", L=-1)

ResN[8,1] <- table_games(Beta=rstan::extract(fit4b,"Beta")$Beta, Q=c(1,1,0,0,1,0,0), M="LandWithin",L=0)
ResN[9,1] <- table_games(Beta=rstan::extract(fit5b,"Beta")$Beta, Q=c(1,1,0,0,0,1,0), M="LandOther",L=0)

ResN[8,2] <- table_games_W(Beta=rstan::extract(fit4b,"Beta")$Beta, Q=c(1,1,0,0,1,0,0), M="WithinLand",W=0)
ResN[9,2] <- table_games_O(Beta=rstan::extract(fit5b,"Beta")$Beta, Q=c(1,1,0,0,0,1,0), M="OtherLand",O=0)


################################################################################### Phylogenetic Models
ResN[10,1] <- ""
ResN[11,1] <- table_games(Beta=rstan::extract(fit1f,"Beta")$Beta, Q=c(1,1,0,0,0,0,0), M="Land", L=-1)
ResN[12,1] <- table_games(Beta=rstan::extract(fit2f,"Beta")$Beta, Q=c(1,0,1,0,0,0,0), M="Water", L=-1)
ResN[13,1] <- table_games(Beta=rstan::extract(fit4f,"Beta")$Beta, Q=c(1,0,0,0,1,0,0), M="Within", L=-1)
ResN[14,1] <- table_games(Beta=rstan::extract(fit3f,"Beta")$Beta, Q=c(1,0,0,1,0,0,0), M="Between", L=-1)
ResN[15,1] <- table_games(Beta=rstan::extract(fit5f,"Beta")$Beta, Q=c(1,0,0,0,0,1,0), M="Other", L=-1)
ResN[16,1] <- table_games(Beta=rstan::extract(fit6f,"Beta")$Beta, Q=c(1,0,0,0,0,0,1), M="Strat", L=-1)

ResN[17,1] <- table_games(Beta=rstan::extract(fit4bf,"Beta")$Beta, Q=c(1,1,0,0,1,0,0), M="LandWithin",L=0)
ResN[18,1] <- table_games(Beta=rstan::extract(fit5bf,"Beta")$Beta, Q=c(1,1,0,0,0,1,0), M="LandOther",L=0)

ResN[17,2] <- table_games_W(Beta=rstan::extract(fit4bf,"Beta")$Beta, Q=c(1,1,0,0,1,0,0), M="WithinLand",W=0)
ResN[18,2] <- table_games_O(Beta=rstan::extract(fit5bf,"Beta")$Beta, Q=c(1,1,0,0,0,1,0), M="OtherLand",O=0)

ResN <- cbind(rep("",18),ResN)
colnames(ResN)[1] <- "Phylo"

for(i in 10:18)
ResN[i,1] <- "$\\checkmark$"



WAIC <- function(X){
  y <- rstan::extract(X,pars="log_lik")$log_lik

  p_y <- exp(y)
  avg_y <- colMeans(p_y)
  lppd <- sum(log(avg_y))

  pd_v <- c()

  for(i in 1:dim(y)[2])
  pd_v[i] <- var(y[,i])

    pD <- sum(pd_v)


  # WAIC
WAIC<- -2*(lppd - pD) 

# Return Results
cbind(pD,lppd,WAIC)
}



waic_res <- matrix(NA,ncol=5,nrow=18)
colnames(waic_res) <- c("EP", "LPPD","WAIC", "D-WAIC", "W-WAIC")
rownames(ResN) <- c("Base model","Interdependence land", "Interdependence water","Intra-group conflict","Intra-cultural conflict","Inter-cultural conflict","Social stratification", "Intra-group conflict (Interdependence land)","Inter-cultural conflict (Interdependence land)", 
 "Base model","Interdependence land", "Interdependence water","Intra-group conflict","Intra-cultural conflict","Inter-cultural conflict","Social stratification",
 "Intra-group conflict (Interdependence land)","Inter-cultural conflict (Interdependence land)")

waic_res[1,1:3] <- WAIC(fit0)
waic_res[2,1:3] <- WAIC(fit1)
waic_res[3,1:3] <- WAIC(fit2)
waic_res[4,1:3] <- WAIC(fit4)
waic_res[5,1:3] <- WAIC(fit3)
waic_res[6,1:3] <- WAIC(fit5)
waic_res[7,1:3] <- WAIC(fit6)

waic_res[8,1:3] <- WAIC(fit4b)
waic_res[9,1:3] <- WAIC(fit5b)

waic_res[10,1:3] <- WAIC(fit0f)
waic_res[11,1:3] <- WAIC(fit1f)
waic_res[12,1:3] <- WAIC(fit2f)
waic_res[13,1:3] <- WAIC(fit4f)
waic_res[14,1:3] <- WAIC(fit3f)
waic_res[15,1:3] <- WAIC(fit5f)
waic_res[16,1:3] <- WAIC(fit6f)

waic_res[17,1:3] <- WAIC(fit4bf)
waic_res[18,1:3] <- WAIC(fit5bf)

waic_res[,4] <- waic_res[,3]-min(waic_res[,3],na.rm=TRUE)

waic_res[,5] <- exp(-0.5*waic_res[,4])/sum(exp(-0.5*waic_res[,4]),na.rm=TRUE)

waic_res <- waic_res[,-3] 

write.csv(cbind(ResN,round(waic_res,3)), "Output/ResultsTable.csv",quote=FALSE)
