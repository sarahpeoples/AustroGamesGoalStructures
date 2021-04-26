# # create table with culture name and number of game GS (3 categories)
Culture <- as.character(d_final$Phylo_TreeTaxaName)
N_GS <- data.frame(cbind(Culture, model_dat$Games))
write.csv(N_GS, "Output/N_GS.csv", quote=F, row.names=F)

# # create table with culture name and number of game GS (6 categories)
Sol <- d_final$Solitary
Comp <- d_final$Competitive
CompSol <- d_final$"Competitive vs. Solitary"
CompCoop <- d_final$"Competitive vs. Cooperative group"
CoopCoop <- d_final$"Cooperative group vs. Cooperative group"
Coop <- d_final$"Cooperative group"
N_GS_spec <- data.frame(cbind(Culture, Sol, Comp, CompSol, CompCoop, CoopCoop, Coop))
names(N_GS_spec) <- c("Culture", "Solitary", "Competitive", "Competitive vs. Solitary", "Competitive vs. Cooperative group", "Cooperative group vs. Cooperative group", "Cooperative")
write.csv(N_GS_spec, "Output/N_GS_spec.csv", quote=F, row.names=F)



# # table of the sum of goal structures
N_GS_spec2 <- as.data.frame(N_GS_spec[,-1])
N_GS_spec2$Solitary <- as.numeric(as.character(N_GS_spec2$Solitary))
N_GS_spec2$Competitive <- as.numeric(as.character(N_GS_spec2$Competitive))
N_GS_spec2$"Competitive vs. Solitary" <- as.numeric(as.character(N_GS_spec2$"Competitive vs. Solitary"))
N_GS_spec2$"Competitive vs. Cooperative group" <-as.numeric(as.character(N_GS_spec2$"Competitive vs. Cooperative group"))
N_GS_spec2$"Cooperative group vs. Cooperative group" <- as.numeric(as.character(N_GS_spec2$"Cooperative group vs. Cooperative group"))
N_GS_spec2$Cooperative <- as.numeric(as.character(N_GS_spec2$Cooperative))
apply(N_GS_spec2, 2, FUN=sum)

N_GS_spec3 <- data.frame("Goal structure" = as.character(c(colnames(N_GS_spec2), "Total")), "Sample size (N)" = as.numeric(c(colSums(N_GS_spec2),NA)), row.names=NULL)
names(N_GS_spec3) <- c("Goal structure", "Sample size")
N_GS_spec3[7,1] <- "Total"
N_GS_spec3[7,2] <- sum(N_GS_spec3[1:6,2])
write.csv(N_GS_spec3, "Output/N_GS_sum.csv", quote=F, row.names=F)

 
# # Filter table
# filter <- data.frame(cbind(c("(None)", "Assigned to ABVD","Goal structure", "Non-foreign origin", "Matching ABVD codes with Pulotu", "Matching time frames with Pulotu", "Culture on the MCCT")), NA, NA)
# names(filter) <- c("Filter", "Games", "Cultures")

# filter$Games[1] <- nrow(g2)
# filter$Cultures[1] <- "-"

# filter$Games[2] <- sum(!is.na(g2$Culture_ID))
# filter$Cultures[2] <- nrow()

# levels(g$Goal.Structure)[levels(g$Goal.Structure)==""] <-NA
# filter$Games[3] <- nrow(g[!is.na(g$Goal.Structure),]) #before ABVD!
# filter$Cultures[3] <- sum(!is.na(unique(g$Culture_ID[!is.na(g$Goal.Structure)])))

# filter$Games[4] <- sum(g2$foreign==0)				#before ABVD!
# filter$Cultures[4] <- nrow()

# filter$Games[5] <- sum(g2$timing_ok==1)				#before ABVD!
# filter$Cultures[5] <- sum(!is.na(unique(g2$Culture_ID[g2$timing_ok==1])))

# filter$Games[6] <- nrow(legal_games)
# filter$Cultures[6] <- length(unique(legal_games$Culture_ID))

# filter$Games[7] <- sum(Games)
# filter$Cultures[7] <- length(d_final$tree_name)

# g2$[which(g2$timing_ok==1),]