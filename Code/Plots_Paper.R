# # # # # # # # # # # # # #
# # # # # # # # # # # # # #
# # # # # # # # # # # # # #
# # # # # Plots of the phylogeny with games and predictor variables 


colrs= c("#0072B2", "#D55E00", "#009E73", "#F0E442", "#CC79A7", "#999999") 

# d1 <- read.nexus("Data/a400-m1pcv-time.mcct.nexus")
austro.new <- keep.tip(d1, tip = d_final$Phylo_TreeTaxaName)    # Prune the tree
austro.new$tip.label <- as.character(d_final$Culture)			# change tip labels from language to culture
# austro.new <- ladderize(austro.new)

xx <- c()
for(i in 1:length(d_final$Culture)){
 xx[i] <- which(d_final$Culture[i] == austro.new$tip.label)
}

d_tip_names <- d_final$Culture[order(xx)]
Games2 <- model_dat$Games[order(xx),]
Con_Within2 <- model_dat$Con_Within[order(xx)]
Con_Between2 <- model_dat$Con_Between[order(xx)]
Con_Other2 <- model_dat$Con_Other[order(xx)]
Water2 <- model_dat$Water[order(xx)]
Land2 <- model_dat$Land[order(xx)]
Strat2 <- model_dat$Strat[order(xx)]

phylo_plot_dat <- as.data.frame(cbind(as.character(d_final$Culture), as.character(d_final$Phylo_TreeTaxaName),model_dat$Games, model_dat$Con_Within, model_dat$Con_Between, model_dat$Con_Other, model_dat$Water, model_dat$Land, model_dat$Strat))
names(phylo_plot_dat) <- c("Culture", "Phylo_TreeTaxaName", "Solitary", "Competitive", "Cooperative", "Con_Within", "Con_Between", "Con_Other", "Water", "Land", "SocStrat")
phylo_plot_dat$SocStrat[phylo_plot_dat$SocStrat==-99] <- NA


# # # # # # # # # # # # # # # # # # # # #
# Only games (8/19/20)
pdf("Output/Tree_Games.pdf", width=7, height=8)
par(mai=c(0.2,0.5,0.7,0), mar=c(0,0,2.7,0))
plot.phylo(austro.new, label.offset=0, no.margin=F, x.lim=c(0,11), root.edge=T) # , main= "Frequency of games in each society"
# add.scale.bar(0,23)
# axisPhylo(side=1)
legend("topright", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(0.17,.05), cex=.8, pch=19, pt.cex=1.5)
tiplabels(pie= Games2,piecol=colrs, cex=0.7, offset=3)
mtext(text= "Games", side= 3, at=7.15, line=-0.5, cex=.9)
# Plot with N in it
# text(x= 8.1, y= c(1:24), labels=Games[,1], cex=0.5,col="#0072B2")
# text(x= 8.4, y= c(1:24), labels=Games[,2], cex=0.5,col="#D55E00")
# text(x= 8.7, y= c(1:24), labels=Games[,3], cex=0.5,col="#009E73")
dev.off()



# # # # # # # # # # # # # # # # # # # # #
# With conflict predictors on one plot
pdf("Output/Tree_Games_Conflict.pdf", width=9, height=8)
par(mai=c(0.2,0.5,0.7,0), mar=c(0,0,2.7,0))
plot.phylo(austro.new, label.offset=0, no.margin=F, x.lim=c(0,11), root.edge=T) # , main= "Frequency of games in each society"
# add.scale.bar(0,23)
# axisPhylo(side=1)
legend("topright", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(0.4,-0.07), cex=.75, pch=19)
tiplabels(pie= Games2,piecol=colrs, cex=0.7, offset=3)
mtext(text= "Games", side= 3, at=7.1, line=0, cex=0.7, font=2)
tiplabels(pie= ifelse(Con_Within2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=3.9)
mtext(text= "Intra-group", side= 3, at=8, line=0.6, cex=0.7, font=2)
mtext(text= "conflict", side= 3, at=8, line=0, cex=0.7, font=2)
# cono_image = readPNG("Data/Conflict_Other.png")				##### uncomment for pictures
# rasterImage(cono_image, 7.8, 23.9, 8.5, 25.1, xpd=NA)				##### uncomment for pictures
tiplabels(pie= ifelse(Con_Between2 ==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=5)
mtext(text= "Intra-cultural", side= 3, at=9.1, line=0.6, cex=0.7, font=2)
mtext(text= "conflict", side= 3, at=9.1, line=0, cex=0.7, font=2)
# conw_image = readPNG("Data/Conflict_Within.png")				##### uncomment for pictures
# rasterImage(conw_image, 8.8, 23.9, 9.5, 25.1, xpd=NA)				##### uncomment for pictures
tiplabels(pie= ifelse(Con_Other2 ==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=6.1)
mtext(text= "Inter-cultural", side= 3, at=10.2, line=0.6, cex=0.7, font=2)
mtext(text= "warfare", side= 3, at=10.2, line=0, cex=0.7, font=2)
# conb_image = readPNG("Data/Conflict_Between.png")				##### uncomment for pictures
# rasterImage(conb_image, 9.8, 23.9, 10.5, 25.1, xpd=NA)				##### uncomment for pictures
dev.off()

# to add pictures to the plot
# library(png)
# cono_image = readPNG("Data/Conflict_Other.png")
# rasterImage(cono_image, 8, 24, 8.3, 25, xpd=NA)



# # # # # # # # # # # # # # # # # # # # #
# With all predictors on one plot
pdf("Output/Tree_Games_AllPreds.pdf", width=9, height=8)
par(mai=c(0.2,0.5,1,0), mar=c(0,0,6,0))
plot.phylo(austro.new, label.offset=0, no.margin=F, x.lim=c(0,15))
tiplabels(pie= Games2,piecol=colrs, cex=0.7, offset=4)
mtext(text= "Games", side= 3, at=8.1, line=0, cex=0.7, font=2)

tiplabels(pie= ifelse(Land2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=5)
mtext(text= "Land-based", side= 3, at=9.1, line=0.5, cex=0.5, font=2)
mtext(text= "hunting", side= 3, at=9.1, line=0, cex=0.5, font=2)

tiplabels(pie= ifelse(Water2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=6.1)
mtext(text= "Water-based", side= 3, at=10.2, line=0.5, cex=0.5, font=2)
mtext(text= "hunting", side= 3, at=10.2, line=0, cex=0.5, font=2)

tiplabels(pie= ifelse(Con_Within2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=7.2)
mtext(text= "Intra-group", side= 3, at=11.3, line=0.5, cex=0.5, font=2)
mtext(text= "conflict", side= 3, at=11.3, line=0, cex=0.5, font=2)

tiplabels(pie= ifelse(Con_Between2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=8.3)
mtext(text= "Intra-cultural", side= 3, at=12.4, line=0.5, cex=0.5, font=2)
mtext(text= "conflict", side= 3, at=12.4, line=0, cex=0.5, font=2)

tiplabels(pie= ifelse(Con_Other2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=9.4)
mtext(text= "Inter-cultural", side= 3, at=13.5, line=0.5, cex=0.5, font=2)
mtext(text= "warfare", side= 3, at=13.5, line=0, cex=0.5, font=2)

tiplabels(pie= ifelse(Strat2==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=10.5)
mtext(text= "Social", side= 3, at=14.6, line=0.5, cex=0.5, font=2)
mtext(text= "stratification", side= 3, at=14.6, line=0, cex=0.5, font=2)
legend("topright", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(0.5,-0.1), cex=.75, pch=19)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.05,-0.05), cex=.75, pch=19)
dev.off()



# # # # # # # # # # # # # # # # # # # # #

phylo_plot_dat_wide <- phylo_plot_dat %>%
						pivot_longer( 
						values_to = "Freq",
						names_to = "Goal_structure",
						cols = c("Solitary", "Competitive", "Cooperative"))



# Reorder tree nodes
plot(austro.new)
austro.new <- ladderize(austro.new) 
plot(austro.new)

#' Reordering data to match tree
is_tip <- austro.new $edge[,2] <= length(austro.new $tip.label)
ordered_tips_index <- austro.new $edge[is_tip, 2]
ordered_tips <- austro.new $tip.label[ordered_tips_index]

phylo_plot_dat_wide <- phylo_plot_dat_wide[order(match(phylo_plot_dat_wide$Culture, ordered_tips)), ]
phylo_plot_dat_wide$Culture <- as.factor(phylo_plot_dat_wide$Culture)
phylo_plot_dat_wide$Culture <- ordered(phylo_plot_dat_wide$Culture, levels = ordered_tips)
phylo_plot_dat_wide$Freq <- as.numeric(phylo_plot_dat_wide$Freq)
phylo_plot_dat_wide$Goal_structure <- as.factor(phylo_plot_dat_wide$Goal_structure)
phylo_plot_dat_wide$Goal_structure <- factor(phylo_plot_dat_wide$Goal_structure, levels= c("Solitary", "Competitive", "Cooperative"))


# phylo_plot_dat_wide <- phylo_plot_dat_wide[ordered_tips, ]

#' Plot of the tree
phylo <- ggtree(austro.new, ladderize = F) +  
  geom_tiplab(offset = 0.1, hjust = 0, size = 2.5) + 
  xlim_tree(5.3) +
  theme(plot.margin = unit(c(.2, 0, .2, 0), "cm"))

bars <-	ggplot(phylo_plot_dat_wide, aes(y = Freq, x = Culture)) + 
  geom_col(aes(fill = Goal_structure)) +
  coord_flip() +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,10,15,20,30,40,50,60)) + 
  theme_tree2() + 
  theme(legend.position=c(.70, .45)) +
  scale_color_manual(values = "black") +
  scale_fill_manual(name = "Goal Structure", 
                    values = c("#0072B2", "#D55E00", "#009E73")) +
  theme(plot.margin = unit(c(.2, 0, .2, 0), "cm"))
  
cols <- c("0" = "grey", "1" = "black", "NA" = "white")
dots_conw <- ggplot(phylo_plot_dat_wide, aes(x= 1 ,y= Culture, shape= as.factor(Con_Within), colour = factor(Con_Within), fill = factor(Con_Within))) +
	geom_point(size = 7) +
	scale_shape_manual(values = c(1, 16)) +
	scale_colour_manual(values = cols,
		aesthetics = c("colour", "fill")) +
	theme_tree2(legend.position= "none") +
	theme(axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_text(size=9)) +
	scale_x_discrete(name ="Intra-group") +
	guides(x = "none", y = "none")

dots_conb <- ggplot(phylo_plot_dat_wide, aes(x= 1 ,y= Culture, shape= as.factor(Con_Between), colour = factor(Con_Between), fill = factor(Con_Between))) +
	geom_point(size = 7) +
	scale_shape_manual(values = c(1, 16)) +
	scale_colour_manual(values = cols,
		aesthetics = c("colour", "fill")) +
	theme_tree2(legend.position= "none") +
	theme(axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_text(size=9)) +
	scale_x_discrete(name ="Intra-cultural") +
	guides(x = "none", y = "none")
	
dots_cono <- ggplot(phylo_plot_dat_wide, aes(x= 1 ,y= Culture, shape= as.factor(Con_Other), colour = factor(Con_Other), fill = factor(Con_Other))) +
	geom_point(size = 7) +
	scale_shape_manual(values = c(1, 16)) +
	scale_colour_manual(values = cols,
		aesthetics = c("colour", "fill")) +
	theme_tree2(legend.position= "none") +
	theme(axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_text(size=9)) +
	scale_x_discrete(name ="Inter-cultural") +
	guides(x = "none", y = "none")
	
dots_strat <- ggplot(phylo_plot_dat_wide, aes(x= 1 ,y= Culture, shape= as.factor(SocStrat), colour = factor(SocStrat), fill = factor(SocStrat))) +
	geom_point(size = 7) +
	scale_shape_manual(values = c(1, 16)) +
	scale_colour_manual(values = cols,
		aesthetics = c("colour", "fill")) +
	theme_tree2(legend.position= "none") +
	theme(axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_text(size=9)) +
	scale_x_discrete(name ="SocStrat") +
	guides(x = "none", y = "none")
	
dots_wat <- ggplot(phylo_plot_dat_wide, aes(x= 1 ,y= Culture, shape= as.factor(Water), colour = factor(Water), fill = factor(Water))) +
	geom_point(size = 7) +
	scale_shape_manual(values = c(1, 16)) +
	scale_colour_manual(values = cols,
		aesthetics = c("colour", "fill")) +
	theme_tree2(legend.position= "none") +
	theme(axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_text(size=9)) +
	scale_x_discrete(name ="Water") +
	guides(x = "none", y = "none")
	
dots_land <- ggplot(phylo_plot_dat_wide, aes(x= 1 ,y= Culture, shape= as.factor(Land),colour = factor(Land), fill = factor(Land))) +
	geom_point(size = 7) +
	scale_shape_manual(values = c(1, 16)) +
	scale_colour_manual(values = cols,
		aesthetics = c("colour", "fill")) +
	theme_tree2(legend.position= "none") +
	theme(axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_text(size=9)) +
	scale_x_discrete(name ="Land") +
	guides(x = "none", y = "none")
	
#' Combining and outputting plots
pdf("Output/Tree_Games_bargraph.pdf", width=16, height=9)
cowplot::plot_grid(phylo, dots_strat, dots_land, dots_wat, dots_conw, dots_conb, dots_cono, bars,  ncol=8, align= "h", rel_widths= c(1.2,.2,.2,.2,.2,.2,.2,1.2))
dev.off()







# # # # # # # # # # # # # # # # # # # # #
# # # # Created on 5/19/20
# # With all predictors on one plot
# pdf("Output/Tree_Games_AllPreds_N.pdf", width=9, height=8)
# par(mai=c(0.2,0.5,1,0), mar=c(0,0,6,0))
# plot.phylo(austro.new, label.offset=0, no.margin=F, x.lim=c(0,15))
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=4)
# mtext(text= "Games", side= 3, at=8.1, line=0, cex=0.7, font=2)

# mtext(text= "Group", side= 3, at=9.1, line=0.5, cex=0.5, font=2)
# mtext(text= "hunting", side= 3, at=9.1, line=0, cex=0.5, font=2)
# tiplabels(pie= ifelse(Land==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=5)

# tiplabels(pie= ifelse(Water==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=6)
# mtext(text= "Group", side= 3, at=10.1, line=0.5, cex=0.5, font=2)
# mtext(text= "fishing", side= 3, at=10.1, line=0, cex=0.5, font=2)

# mtext(text= "Intra-gr.", side= 3, at=11.1, line=0.5, cex=0.5, font=2)
# mtext(text= "conflict", side= 3, at=11.1, line=0, cex=0.5, font=2)
# tiplabels(pie= ifelse(Con_Within==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=7)

# tiplabels(pie= ifelse(Con_Between==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=8)
# mtext(text= "Intra-cult.", side= 3, at=12.1, line=0.5, cex=0.5, font=2)
# mtext(text= "conflict", side= 3, at=12.1, line=0, cex=0.5, font=2)

# tiplabels(pie= ifelse(Con_Other==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=9)
# mtext(text= "Inter-cult.", side= 3, at=13.1, line=0.5, cex=0.5, font=2)
# mtext(text= "conflict", side= 3, at=13.1, line=0, cex=0.5, font=2)

# tiplabels(pie= ifelse(Strat==1,1,NA), piecol=c("#999999", "#FFFFFF"), cex=0.7, offset=10)
# mtext(text= "SocStrat", side= 3, at=14.1, line=0, cex=0.5, font=2)

# legend("topright", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(0.5,-0.1), cex=.75, pch=19)
# # Plot with N in it
# text(x= 7.1, y= c(1:24), labels=Games[,1], cex=0.5,col="#0072B2")
# text(x= 7.4, y= c(1:24), labels=Games[,2], cex=0.5,col="#D55E00")
# text(x= 7.7, y= c(1:24), labels=Games[,3], cex=0.5,col="#009E73")
# dev.off()











# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # par(mai=c(0.2,1.8,1,0))
# pdf("Tree_Games.pdf")
# plot.phylo(austro.new, label.offset=0.2, main= "Frequency of Games for each Culture")
# tiplabels(pie= Games , piecol=colrs,cex=0.7)
# legend("top", colnames(Games), fill=colrs, cex=0.5, xpd=T, inset=c(-0.15,-0.05))
# dev.off()

# # Land-based hunting in groups
# pdf("Tree_Games_Land.pdf")
# par(mai=c(0.2,0.5,1,0))
# plot.phylo(austro.new, label.offset=0.5, main= "Frequency of games for each culture with land-based hunting")
# tiplabels(pie= Land, piecol=c("#E69F00", "#56B4E9"), cex=0.7, offset=0.1)
# legend("topleft", c("Land = 2", "Land = 0|1"), col=c("#E69F00", "#56B4E9"), cex=.75, xpd=T, inset=c(0.2,-0.05), pch= 19)
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=0.35)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.01,-0.05), cex=.75, pch=19)
# dev.off()

# # Water-based hunting in groups
# pdf("Tree_Games_Water.pdf")
# par(mai=c(0.2,0.5,1,0))
# plot.phylo(austro.new, label.offset=0.5, main= "Frequency of games for each culture with water-based hunting")
# tiplabels(pie= Water, piecol=c("#0072B2", "#D55E00"), cex=0.7, offset=0.1)
# legend("topleft", c("Water = 3", "Water = 1|2"), col=c("#0072B2", "#D55E00"), cex=.75, xpd=T, inset=c(0.2,-0.05), pch= 19)
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=0.35)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.01,-0.05), cex=.75, pch=19)
# dev.off()

# # Conflict between communities
# pdf("Tree_Games_Between.pdf")
# par(mai=c(0.2,0.5,1,0))
# plot.phylo(austro.new, label.offset=0.5, main= "Frequency of games for each culture with conflict between communities")
# tiplabels(pie= Con_Between, piecol=c("#CC79A7", "#F0E442"), cex=0.7, offset=0.1)
# legend("topleft", c("Conflict Between = 1|2", "Conflict Between = 3|4"), col=c("#CC79A7", "#F0E442"), cex=.75, xpd=T, inset=c(0.2,-0.05), pch= 19)
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=0.35)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.01,-0.05), cex=.75, pch=19)
# dev.off()

# # Conflict within communities
# pdf("Tree_Games_Conflict.pdf")
# par(mai=c(0.2,0.5,1,0))
# plot.phylo(austro.new, label.offset=0.5, main= "Frequency of games for each culture with conflict within communities")
# tiplabels(pie= Con_Within, piecol=c("#D55E00", "#E69F00"), cex=0.7, offset=0.1)
# legend("topleft", c("Conflict Within = 1|2|3", "Conflict Within = 4"), col=c("#D55E00", "#E69F00"), cex=.75, xpd=T, inset=c(0.2,-0.05), pch= 19)
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=0.35)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.01,-0.05), cex=.75, pch=19)
# dev.off()

# # Conflict with other cultures
# pdf("Tree_Games_Other.pdf")
# par(mai=c(0.2,0.5,1,0))
# plot.phylo(austro.new, label.offset=0.5, main= "Frequency of games for each culture with conflict with other cultures")
# tiplabels(pie= Con_Other, piecol=c("#0072B2", "#56B4E9"), cex=0.7, offset=0.1)
# legend("topleft", c("Conflict Other = 1|2", "Conflict Other = 3|4"), col=c("#0072B2", "#56B4E9"), cex=.75, xpd=T, inset=c(0.2,-0.05), pch= 19)
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=0.35)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.01,-0.05), cex=.75, pch=19)
# dev.off()

# # Stratification
# Strat1 = Strat
# Strat1[Strat1<0] <- NA
# pdf("Tree_Games_Strat.pdf")
# par(mai=c(0.2,0.5,1,0))
# plot.phylo(austro.new, label.offset=0.5, main= "Frequency of games for each culture with social stratification")
# tiplabels(pie= Strat1, piecol=c("#F0E442", "#999999"), cex=0.7, offset=0.1)
# legend("topleft", c("Stratification = Highly Stratified", "Stratification = Egalitarian|Mod. Stratified"), col=c("#F0E442", "#999999"), cex=.75, xpd=T, inset=c(0.2,-0.05), pch= 19)
# tiplabels(pie= Games,piecol=colrs, cex=0.7, offset=0.35)
# legend("topleft", c("Solitary", "Competitive", "Cooperative"), col=colrs, xpd=T, inset=c(.01,-0.05), cex=.75, pch=19)
# dev.off()

