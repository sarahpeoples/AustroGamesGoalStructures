
data{
  int N;                     //# Number of Cultures
  int P;                     //# Number of Parameters
  int G;                     //# Number of Games

  int Q[P];                  //# On off switch for predictors

  int Games[N,G];            //# Outcome of interest

  vector[N] Land;            //# Interdependence in land subsistence
  vector[N] Water;           //# Interdependence in water subsistence

  vector[N] Con_Between;     //# Conflict between groups in same culture
  vector[N] Con_Within;      //# Conflict within same group
  vector[N] Con_Other;       //# Conflict with other cultures

  vector[N] Strat;           //# Stratified

  matrix[N,N] D;             //# Relative branch lengths by language
}

parameters{
  matrix[G-1,P] Beta;               //# Effects - Log-odds scale
  real<lower=0,upper=1> Miss[1];     //# Missing Strat value   

  vector [N] Gamma_raw[G-1];        //# Control for correlations due to shared cultural history

  real<lower=0, upper=1>  Cor[G-1]; //# Higher-level priors
  real<lower=0> Decay[G-1];         //#
  real<lower=0>  Scale[G-1];        //#
}

model{
  vector[G] Theta;                  //# Local storage for link function
  matrix[N,N] X;                    //#
  vector[N] Strat2;                 //# Strat with missing

  vector [N] Gamma[G-1];

  Strat2 = Strat;                 //# Missing Strat value   
  Strat2[17] = Miss[1];

  to_vector(Beta) ~ normal(0,5);    //# Prior
  Miss ~ beta(1,1);

  Cor ~ beta(10,2);                   //# Priors on phylogentic process
  Decay ~ exponential(2.5);           //# 
  Scale ~ exponential(2.5);           //#

 for(g in 1:(G-1)){
  Gamma_raw[g] ~ normal(0,1);
  }

//########################### Construct Gaussian Process for phylogeny
for(g in 1:(G-1)){
 for (i in 1:(N-1)){
  for (j in (i+1):N){
                X[i,j] = Cor[g] * exp(- Decay[g] * D[i,j]);   //# Estimate Correlations
                X[j,i] = X[i,j];                         //# Fill Other Triangle
                       }}
 for (i in 1:N){
                X[i,i] = 1;                              //# Fill Diag
                       }


  Gamma[g] =  Scale[g]*(cholesky_decompose(X)*Gamma_raw[g]);
}

     
  for( i in 1:N){                          //# For each culture
   for(g in 1:(G-1))                       //# Model each game frequency relative to control
    Theta[g] = Beta[g,1] + Beta[g,2]*Land[i]*Q[2] + Beta[g,3]*Water[i]*Q[3] + Beta[g,4]*Con_Between[i]*Q[4] + Beta[g,5]*Con_Within[i]*Q[5] + Beta[g,6]*Con_Other[i]*Q[6] + Beta[g,7]*Strat2[i]*Q[7] + Gamma[g,i];                    
    Theta[G] = 0;                          //# Set last catergory as base-case              
     
  Games[i] ~ multinomial(softmax(Theta));  //# Model Outcomes
   }

}

generated quantities{
 vector[N] log_lik; 
 vector[G] Theta;                 //# Local storage for link function
 vector[N] Strat2;                //# Strat with missing

 vector [N] Gamma[G-1];
 matrix[N,N] X;                   //#

 Strat2 = Strat;                 //# Missing Strat value   
 Strat2[17] = Miss[1];


 for(g in 1:(G-1)){
 for (i in 1:(N-1)){
  for (j in (i+1):N){
                X[i,j] = Cor[g] * exp(- Decay[g] * D[i,j]);   //# Estimate Correlations
                X[j,i] = X[i,j];                              //# Fill Other Triangle
                       }}
 for (i in 1:N){
                X[i,i] = 1;                                   //# Fill Diag
                       }


  Gamma[g] =  Scale[g]*(cholesky_decompose(X)*Gamma_raw[g]);
 }

   for( i in 1:N){                        //# For each culture
   for(g in 1:(G-1))                       //# Model each game frequency relative to control
    Theta[g] = Beta[g,1] + Beta[g,2]*Land[i]*Q[2] + Beta[g,3]*Water[i]*Q[3] + Beta[g,4]*Con_Between[i]*Q[4] + Beta[g,5]*Con_Within[i]*Q[5] + Beta[g,6]*Con_Other[i]*Q[6] + Beta[g,7]*Strat2[i]*Q[7] + Gamma[g,i];                    
    Theta[G] = 0;                          //# Set last catergory as base-case                
       
   log_lik[i] = multinomial_lpmf( Games[i] | softmax(Theta));  //# Model Outcomes
 }
}

