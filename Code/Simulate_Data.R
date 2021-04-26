N <- 25

P <- 7 
G <- 6

I_L <- rbinom(N,1,0.5)
I_W <- rbinom(N,1,0.5)
Strat <- rbinom(N,1,0.5)

Games <- matrix(NA, nrow=N, ncol=G)

Beta <- matrix(NA, ncol=3, nrow=(G-1))
for( g in 1:(G-1))
Beta[g,] <- runif(3,-3,3)

Beta[1,1] <- 2

Beta[1,2] <- 5

Theta<-c()
for( i in 1:N){
for(g in 1:G-1)
Theta[g] <- Beta[g,1] + Beta[g,2]*I_L[i] + Beta[g,3]*I_W[i]
Theta[G] <- 0

Games[i,] <- rmultinom(1, 10, Softmax(Theta) )
}

Q <- rep(0,P)
Q[2:3] <- c(1,1)

 model_dat_fake=list(                          
  N=N,                                      
  P=P,                                      
  G=G,                                      
  Q=Q,                                      
  Land=I_L,                                
  Water=I_W,                              
  Con_Between=I_L,                  
  Con_Within=I_L,                    
  Con_Other=I_L,  
  Strat=Strat,                                                
  Games=Games                               
  )  

