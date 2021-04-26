
   

plot_games <- function(Beta, Q, M="M1",P="Basic")  {               
Pred <- array(NA,c((iter-warmup)*nchains,G,2))
Theta<-rep(NA,G)

############################################################### Make Predictions
for( q in 1:((iter-warmup)*nchains)){# Iterate over MCMC samples
########################## 
 for(g in 1:G-1){
  Theta[g] <- Beta[q,g,1];  
  }
  Theta[G] <- 0
  Pred[q,,1] <- Softmax(Theta)
 
########################## 
 for(g in 1:G-1){
  Theta[g] <- Beta[q,g,1] + Beta[q,g,2]*Q[2] + Beta[q,g,3]*Q[3] + Beta[q,g,4]*Q[4] + Beta[q,g,5]*Q[5] + Beta[q,g,6]*Q[6] + Beta[q,g,7]*Q[7];  
  }
  Theta[G] <- 0

 Pred[q,,2] <- Softmax(Theta)
 }
          
 Contrast1 <- Pred[,,2] - Pred[,,1]

 Contrast2 <- log(Pred[,3,2] / Pred[,2,2]) - log(Pred[,3,1] / Pred[,2,1])
  
######################################################################## Summary 

ID1 <- rep("Solitary",length(Contrast1[,1]))
ID2 <- rep("Competitive",length(Contrast1[,2]))
ID3 <- rep("Cooperative",length(Contrast1[,3]))
ID4 <- rep("Coop. vs. Compet.",length(Contrast2))

Value <- c(Contrast1[,1],Contrast1[,2],Contrast1[,3],Contrast2)
Estimate <- c(ID1,ID2,ID3,ID4)
dd <- data.frame(Value=Value,Estimate=Estimate,Predictors=rep(M,length(Estimate)),Model=rep(P,length(Estimate)))
return(dd)
}

################################################################################### Phylogentic Models Models
plotdat <- list()
plotdat[[1]] <- plot_games(Beta=rstan::extract(fit1f_mt,"Beta")$Beta, Q=c(1,1,0,0,0,0,0), M="Land", P="Phylogenetic controls")
plotdat[[2]] <- plot_games(Beta=rstan::extract(fit2f_mt,"Beta")$Beta, Q=c(1,0,1,0,0,0,0), M="Water", P="Phylogenetic controls")
plotdat[[3]] <- plot_games(Beta=rstan::extract(fit4f_mt,"Beta")$Beta, Q=c(1,0,0,0,1,0,0), M="Intra-group", P="Phylogenetic controls")
plotdat[[4]] <- plot_games(Beta=rstan::extract(fit3f_mt,"Beta")$Beta, Q=c(1,0,0,1,0,0,0), M="Intra-cultural", P="Phylogenetic controls")
plotdat[[5]] <- plot_games(Beta=rstan::extract(fit5f_mt,"Beta")$Beta, Q=c(1,0,0,0,0,1,0), M="Inter-cultural", P="Phylogenetic controls")
plotdat[[6]] <- plot_games(Beta=rstan::extract(fit6f_mt,"Beta")$Beta, Q=c(1,0,0,0,0,0,1), M="Stratification", P="Phylogenetic controls")

#######


dd <- do.call(rbind,plotdat)

dd$Estimate <- factor(dd$Estimate)
dd$Estimate <- factor(dd$Estimate,levels(dd$Estimate)[c(4,1,3,2)])

dd$Predictors <- factor(dd$Predictors)
dd$Predictors <- factor(dd$Predictors,levels(dd$Predictors)[c(6,1,2,3,4,5)])

my_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
# my_colors2 <- c("#D55E00", "#D55E00", "#D55E00", "#D55E00")

# full set
pp <- ggplot(dd, aes(x = Value, y = as.factor(Model), fill = Estimate)) + 
		stat_density_ridges(quantile_lines = TRUE, 
							quantiles = c(0.05, 0.95), 
							alpha = 0.95, 
							vline_size = 0.5) +
		scale_fill_manual(values = my_colors) +
		geom_vline(xintercept = 0, linetype="dashed", color = "black", size = 0.5) +
		facet_grid(Predictors ~ Estimate, scales = "free") + 
		labs(x = "Change in log relative frequency of goal structures of games", y = "") +
		theme(text = element_text(size = 16)) + 
		theme(axis.title = element_text(face = "bold"),  strip.text = element_text(size = 16)) + 
		theme(legend.position = "none") +
		theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())

ggsave("Output/FullSet_mt.pdf",pp,height=16,width=16)

# full set with switched axes (not used in manuscript)

# pp2 <- ggplot(dd, aes(x = Value, y = as.factor(Model), fill = Estimate)) + 
		# stat_density_ridges(quantile_lines = TRUE, 
							# quantiles = c(0.05, 0.95), 
							# alpha = 0.95, 
							# vline_size = 0.5) +
		# scale_fill_manual(values = my_colors) +
		# geom_vline(xintercept = 0, linetype="dashed", color = "black", size = 0.5) +
		# facet_grid(Estimate ~ Predictors, scales = "free") + 
		# labs(x = "Change in log relative frequency of goal structures of games", y = "") +
		# theme(text = element_text(size = 16)) + 
		# theme(axis.title = element_text(face = "bold"),  strip.text = element_text(size = 16)) + 
		# theme(legend.position = "none") +
		# theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())	
   	
# plot coop-comp difference
dd2 <- dd[which(dd$Estimate=="Coop. vs. Compet."),]

pp2 <- ggplot(dd2, aes(x=Value, y=1, fill=factor(..quantile..))) + 
stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
scale_fill_manual(
  name = "Probability", values = c("#A0A0A0A0", "#CC79A7", "#A0A0A0A0"),
  labels = c("(0, 0.05]", "(0.05, 0.95]", "(0.95, 1]")) + geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.5) +
  facet_grid(Model~Predictors, scales = "free") + labs(x="Change in log relative frequency of cooperative and competitive games", y=" ") +
   theme(text = element_text(size = 16)) + 
   theme(axis.title=element_text(face="bold"),  strip.text = element_text(size = 16)) + 
   theme(legend.position = "none") + 
   theme(axis.ticks.y = element_blank()) + 
   theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())

 ggsave("Output/Difference_coopvscomp_mt.pdf",pp2,height=6,width=16)
 

   # plot cooperative
 dd3 <- dd[which(dd$Estimate=="Cooperative"),]

pp3 <- ggplot(dd3, aes(x=Value, y=1, fill=factor(..quantile..))) + 
stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
scale_fill_manual(
  name = "Probability", values = c("#A0A0A0A0", "#009E73", "#A0A0A0A0"),
  labels = c("(0, 0.05]", "(0.05, 0.95]", "(0.95, 1]")) + geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.5) +
  facet_grid(Model~Predictors, scales = "free") + labs(x="Change in log relative frequency of cooperative games", y=" ") +
   theme(text = element_text(size = 16)) + 
   theme(axis.title=element_text(face="bold"),  strip.text = element_text(size = 16)) + 
   theme(legend.position = "none") + 
   theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())

 ggsave("Output/Difference_cooperative_mt.pdf",pp3,height=6,width=16)



   # plot competitive
 dd4 <- dd[which(dd$Estimate=="Competitive"),]

pp4 <- ggplot(dd4, aes(x=Value, y=1, fill=factor(..quantile..))) + 
stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
scale_fill_manual(
  name = "Probability", values = c("#A0A0A0A0", "#D55E00", "#A0A0A0A0"),
  labels = c("(0, 0.05]", "(0.05, 0.95]", "(0.95, 1]")) + geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.5) +
  facet_grid(Model~Predictors, scales = "free") + labs(x="Change in log relative frequency of competitive games", y=" ") +
   theme(text = element_text(size = 16)) + 
   theme(axis.title=element_text(face="bold"),  strip.text = element_text(size = 16)) + 
   theme(legend.position = "none") + 
   theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())

 ggsave("Output/Difference_competitive_mt.pdf",pp4,height=6,width=16)


   # plot solitary
 dd5 <- dd[which(dd$Estimate=="Solitary"),]

pp5 <- ggplot(dd5, aes(x=Value, y=1, fill=factor(..quantile..))) + 
stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
scale_fill_manual(
  name = "Probability", values = c("#A0A0A0A0", "#0072B2", "#A0A0A0A0"),
  labels = c("(0, 0.05]", "(0.05, 0.95]", "(0.95, 1]")) + geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.5) +
  facet_grid(Model~Predictors, scales = "free") + labs(x="Change in log relative frequency of solitary games", y=" ") +
   theme(text = element_text(size = 16)) + 
   theme(axis.title=element_text(face="bold"),  strip.text = element_text(size = 16)) + 
   theme(legend.position = "none") + 
   theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())

 ggsave("Output/Difference_solitary_mt.pdf",pp5,height=6,width=16)
 
