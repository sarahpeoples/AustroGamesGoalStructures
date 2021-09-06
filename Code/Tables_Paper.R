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
filter <- data.frame(cbind(c("(None)",	
								"Coded as a game",
								"Linked to an ABVD",
								"Coded goal structure", 
								"Excluding non-local origin", 
								"ABVD on the MCCT", 
								"Matching time frames with Pulotu {+/-} 50 yrs",
								"Covariate data in Pulotu",
								"Total")), NA, NA, NA)
names(filter) <- c("Filter", "Games remaining", "Games dropped", "Cultures remaining")

# no filter
filter_1 <- game_filter(Games, clean_games = FALSE,
                 clean_ABVD = FALSE,
                 clean_GS = FALSE,
                 clean_origin = "keep_all",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = FALSE)
filter[1,2] <- nrow(filter_1)
filter[1,3] <- "-"
filter[1,4] <- "-"

# games
filter_2 <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = FALSE,
                 clean_GS = FALSE,
                 clean_origin = "keep_all",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = FALSE)
filter[2,2] <- nrow(filter_2)
filter[2,3] <- sum(nrow(filter_1) - nrow(filter_2))
filter[2,4] <- length(unique(filter_2$ABVD_code))

# abvd
filter_3 <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = FALSE,
                 clean_origin = "keep_all",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = FALSE)
filter[3,2] <- nrow(filter_3)
filter[3,3] <- sum(nrow(filter_2) - nrow(filter_3))
filter[3,4] <- length(unique(filter_3$ABVD_code))

# goal structure
filter_4 <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = TRUE,
                 clean_origin = "keep_all",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = FALSE)
filter[4,2] <- nrow(filter_4)
filter[4,3] <- sum(nrow(filter_3) - nrow(filter_4))
filter[4,4] <- length(unique(filter_4$ABVD_code))

# remove non-local origin
filter_5 <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = TRUE,
                 clean_origin = "remove_nonlocal",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = FALSE)
filter[5,2] <- nrow(filter_5)
filter[5,3] <- sum(nrow(filter_4) - nrow(filter_5))
filter[5,4] <- length(unique(filter_5$ABVD_code))

# on phylogeny
filter_6 <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = TRUE,
                 clean_origin = "remove_nonlocal",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = TRUE)
filter[6,2] <- nrow(filter_6)
filter[6,3] <- sum(nrow(filter_5) - nrow(filter_6))
filter[6,4] <- length(unique(filter_6$ABVD_code))

# pulotu time
filter_7 <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = TRUE,
                 clean_origin = "remove_nonlocal",
                 clean_pulotu = FALSE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = TRUE,
                 clean_phylo = TRUE)
filter[7,2] <- nrow(filter_7)
filter[7,3] <- sum(nrow(filter_6) - nrow(filter_7))
filter[7,4] <- length(unique(filter_7$ABVD_code))

# covariate data in pulotu
filter[8,2] <- sum(model_dat$Games)
filter[8,3] <- sum(nrow(filter_7) - sum(model_dat$Games))
filter[8,4] <- nrow(model_dat$D)

# total
filter[9,2] <- sum(model_dat$Games)
filter[9,3] <- "-" # sum(nrow(filter_1) - sum(model_dat$Games))
filter[9,4] <- nrow(model_dat$D)

write.csv(filter, "Output/N_filters.csv", quote=F, row.names=F)


