
fitTest <- stan(file="Code/Model_Code.stan", data = model_dat_fake, iter = iter, warmup=warmup, chains = nchains)

B <- extract(fitTest,"Beta")$Beta

mB <- matrix(NA,ncol=3,nrow=(G-1))

for( i in 1:(G-1)){
for( j in 1 : 3){
 mB[i,j] <- mean(B[,i,j])
}}

plot(mB~Beta)
