
data(Games)
data(Cultures)
data(Sources)

d <- read.csv("Data/Pulotu_Database_4_7_2015.txt", sep='\t', fileEncoding = "UTF-8", header=T)  # downloaded from https://figshare.com/articles/dataset/Pulotu_Dataset_Updated_4th_July_2015_www_pulotu_com/1472946
d_strat <- read.csv("Data/2017.08.17_pulotuDataset_Strat_Subsistence_Notes_fromJWatts.csv")    # from Joseph Watts (personal communication). Identical to published 2016 dataset, just not subsetted

colnames(d_strat)[1] <- "Culture"

# # Prepare game data
g <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = TRUE,
                 clean_origin = "remove_nonlocal",
                 clean_pulotu = TRUE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = TRUE,
                 clean_phylo = TRUE)

g$Phylo_TreeTaxaName <- c()

for ( i in 1:nrow(g)){
  foo.nr <- Cultures $Phylo_TreeTaxaName[which(Cultures$ABVD_code %in% unlist(strsplit(g$ABVD_code[i], ";")))]
  if(sum(!is.na(foo.nr))==1){
    g$Phylo_TreeTaxaName[i] <- paste0(foo.nr[1])
  } else if(sum(!is.na(foo.nr))>1){
    g$Phylo_TreeTaxaName[i] <- paste0(foo.nr[1])
  }
}

g$ABVD_code[which(g$ABVD_code == "1190;92")] <- "92"    #"Malagasy (Merina)", not Malagasy (Tanala)
res <- table(g$Phylo_TreeTaxaName, g$Goal_structure)

# Prepare Pulotu data
d_strat2 <- d_strat[,c("Culture", "ABVD_Code","v13.Social_stratification_.after_Sahlins._1958.")]				            	##
d_strat2$Culture <- as.character(d_strat2$Culture)				            	##
d_strat2$Culture[d_strat2$Culture=="Merina"] <- "Malagasy (Merina)"				            	##
d_strat2$Culture[d_strat2$Culture=="Tanala"] <- "Malagasy (Tanala)"				            	##
d_strat2$Culture <- as.factor(d_strat2$Culture)				            	##



p <- merge(d, d_strat2, by="Culture", all.x=T)				            	##

p2 <- p[,c("Culture","isocode", "ABVD_Code.x", "v1.Traditional_Time_Focus", "v13.Social_stratification_.after_Sahlins._1958.", 
           "v28.Land.based_hunting_performed_by_one_or_more_groups", "v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups", 
           "v14.Conflict_within_communities", "v15.Conflict_between_communities_of_the_culture", "v16.Conflict_with_other_cultures")]
names(p2) <- c("Culture","isocode", "ABVD_Code", "v1.Traditional_Time_Focus", "v13.Social_stratification_.after_Sahlins._1958.", 
               "v28.Land.based_hunting_performed_by_one_or_more_groups", "v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups", 
               "v14.Conflict_within_communities", "v15.Conflict_between_communities_of_the_culture", "v16.Conflict_with_other_cultures")

d4 <- read.csv("Data/tree_lookup.csv")
p2$Phylo_TreeTaxaName <- NA
p2$Phylo_TreeTaxaNameFull <- NA
p2$check <- NA
p2$check2 <- NA

for( i in 1:length(p2[,1])){
  p2$Phylo_TreeTaxaName[i] <- as.character(d4[d4$ABVD %in% unlist(strsplit(as.character(p2$ABVD_Code[i]), "; ")),][1,3])
  p2$Phylo_TreeTaxaNameFull[i] <- reduceR(as.character(d4[d4$ABVD %in% unlist(strsplit(as.character(p2$ABVD_Code[i]), "; ")),][,3]))
  p2$check[i] <- nrow(d4[d4$ABVD %in% unlist(strsplit(as.character(p2$ABVD_Code[i]), "; ")),])
  p2$check2[i] <- ifelse(p2$Phylo_TreeTaxaName[i] %in% rownames(res),1,0)
}

p2$Phylo_TreeTaxaName[which(p2$Phylo_TreeTaxaNameFull=="Taiof;Teop")] <- "Teop"
p2$Phylo_TreeTaxaName[which(p2$Phylo_TreeTaxaNameFull=="SubanonSiocon;SubanunSindangan")] <- "SubanunSindangan"

p3 <- p2[which(p2$Phylo_TreeTaxaName %in% rownames(res)),]

p3 <- p3[which(p3$Culture != "Mangaia"),]

res_final <- as.data.frame.matrix(res[which(rownames(res) %in% p3$Phylo_TreeTaxaName),])
p_final <- p3[which( p3$Phylo_TreeTaxaName %in% rownames(res_final)),]

res_final$Phylo_TreeTaxaName <- rownames(res_final)

d_final <- merge(p_final, res_final, by="Phylo_TreeTaxaName")

################################################################################################### Phylo data
d1<-read.nexus("Data/a400-m1pcv-time.mcct.nexus")
d2<-cophenetic.phylo(d1)
d3 <- d2/max(d2)
treeNames <- as.character(colnames(d3))

sTree<- d3[which(treeNames %in% d_final$Phylo_TreeTaxaName),which(treeNames %in% d_final$Phylo_TreeTaxaName)]     



z <- c()
for(i in 1:length(colnames(sTree))){
  z[i]<- which(as.character(d_final$Phylo_TreeTaxaName)[i]==colnames(sTree))
 }

d_final<-d_final[order(z),]


################### Prep for stan
d_final$v13.Social_stratification_.after_Sahlins._1958.[which(d_final$v13.Social_stratification_.after_Sahlins._1958.=="?")] <- NA  	##

# merge categories
Solitary <- d_final$"Solitary"
Competitive <- d_final$"Competitive vs. Solitary" + d_final$"Competitive"
Cooperative <- d_final$"Cooperative group" + d_final$"Cooperative group vs. Cooperative group" + d_final$"Competitive vs. Cooperative group"  
Games <- cbind(Solitary, Competitive, Cooperative)   

Land  <- ifelse(as.numeric(as.character(d_final$v28.Land.based_hunting_performed_by_one_or_more_groups))>1,1,0)
Water  <- ifelse(as.numeric(as.character(d_final$v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups))>2,1,0)
Con_Between  <- ifelse(as.numeric(as.character(d_final$v15.Conflict_between_communities_of_the_culture))<3,1,0)
# Con_Between[is.na(Con_Between)] <- 99  																		 ##
Con_Within  <- ifelse(as.numeric(as.character(d_final$v14.Conflict_within_communities))<4,1,0)
Con_Other  <- ifelse(as.numeric(as.character(d_final$v16.Conflict_with_other_cultures))<3,1,0)
# Con_Other[is.na(Con_Other)] <- 99  																			 ##
Strat  <- ifelse(as.numeric(as.character(d_final$v13.Social_stratification_.after_Sahlins._1958.))>2,1,0)
Strat[is.na(Strat)] <- -99  																					 ##

Q <- rep(0,7)
N<-nrow(Games)                             # Define Control Params
P<-7                                       # intercept, cultural variants
G<-3                                       # number of goal structures possible

model_dat=list(                          
  N=N,                                      
  P=P,                                      
  G=G,                                      
  Q=Q,                                      
  Land=Land,                                
  Water=Water,                     
  Con_Between=Con_Between,                  
  Con_Within=Con_Within,                    
  Con_Other=Con_Other, 
  Strat=Strat,                     
  D = sTree^2,                                
  Games=Games                               
)  


rm(list=setdiff(ls(), c("model_dat","d_final","d1","nchains","warmup","iter","adapt_delta","max_treedepth","cores","G", "Softmax","sTree")))       ##
save(model_dat,d_final,d1,nchains,warmup,iter,adapt_delta,max_treedepth,cores,G,Softmax,sTree, file = "Data/model_data.RData")             ##



