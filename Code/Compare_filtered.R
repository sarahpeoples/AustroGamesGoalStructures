# Compare games used for analyses and games that were excluded
# you might need to run parts of the data pre-processing script

# library(maps)
# library(mapdata)
library(dplyr)
library(ggmap)
library(tidyverse)
library(ggthemes)

# final dataset
g_final <- d_final

# to get longs & lats for map & get ABVD language on g_final to match g_orig & g_final
longlat <- read.csv("Data/cultures_longlat_updatesep2021.csv", sep=',', fileEncoding = "UTF-8", header=T)

g_final$longitude <- NA
g_final$latitude <- NA
g_final$ABVD_language <- NA
for ( i in 1:nrow(g_final)){        
  foo.nr <- longlat $ABVD_code[which(longlat$ABVD_code %in% unlist(strsplit(as.character(g_final$ABVD_Code[i]), "; ")))]
  if(sum(!is.na(foo.nr))==1){
    # g_final$ABVD_language[i] <- paste0(foo.nr[1])
    g_final$longitude[i] <- longlat$ABVD_longitude[which(longlat$ABVD_code %in% foo.nr)]
    g_final$latitude[i] <- longlat$ABVD_latitude[which(longlat $ABVD_code %in% foo.nr)]
    g_final$ABVD_language[i] <- longlat$ABVD_language[which(longlat $ABVD_code %in% foo.nr)]
  } else if(sum(!is.na(foo.nr))>1){
    # g_final$ABVD_language[i] <- paste0(foo.nr[1])
    g_final$longitude[i] <-longlat$ABVD_longitude[which(longlat $ABVD_code %in% foo.nr[1])]
    g_final$latitude[i] <- longlat$ABVD_latitude[which(longlat $ABVD_code %in% foo.nr[1])]
    g_final$ABVD_language[i] <- longlat$ABVD_language[which(longlat $ABVD_code %in% foo.nr[1])]
  }
}

g_final$ABVD_language[g_final$ABVD_language=="Marshallese (E. Dialect)"] <- "Marshallese"


# original dataset + ABVD + Pulotu
g_orig <- game_filter(Games, clean_games = TRUE,
                 clean_ABVD = TRUE,
                 clean_GS = FALSE,
                 clean_origin = "keep_all",
                 clean_pulotu = TRUE,
                 clean_pulotu_time_0 = FALSE,
                 clean_pulotu_time_50 = FALSE,
                 clean_phylo = FALSE)

g_orig$ABVD_language <- c()
for ( i in 1:nrow(g_orig)){
  foo.nr <- longlat $ABVD_language[which(longlat$ABVD_code %in% unlist(strsplit(g_orig$ABVD_code[i], ";")))]
  if(sum(!is.na(foo.nr))==1){
    g_orig$ABVD_language[i] <- paste0(foo.nr[1])
  } else if(sum(!is.na(foo.nr))>1){
    g_orig$ABVD_language[i] <- paste0(foo.nr[1])
  }
}

g_orig$ABVD_code[which(g_orig$ABVD_code == "1190;92")] <- "92"    #"Malagasy (Merina)", not Malagasy (Tanala)


# to get longs & lats for map
g_orig$longitude <- NA
g_orig$latitude <- NA
for ( i in 1:nrow(g_orig)){        
  foo.nr <- longlat$ABVD_code[which(longlat$ABVD_code %in% unlist(strsplit(as.character(g_orig$ABVD_code[i]), ";")))]
  if(sum(!is.na(foo.nr))==1){
    # g_orig$ABVD_language[i] <- paste0(foo.nr[1])
    g_orig$longitude[i] <- longlat$ABVD_longitude[which(longlat$ABVD_code %in% foo.nr)]
    g_orig$latitude[i] <- longlat$ABVD_latitude[which(longlat$ABVD_code %in% foo.nr)]
  } else if(sum(!is.na(foo.nr))>1){
    # g_orig$ABVD_language[i] <- paste0(foo.nr[1])
    g_orig$longitude[i] <-longlat$ABVD_longitude[which(longlat$ABVD_code %in% foo.nr[1])]
    g_orig$latitude[i] <- longlat$ABVD_latitude[which(longlat$ABVD_code %in% foo.nr[1])]
  }
}


# # # 
# data wrangling
#get g_orig2 in same format as g_final (every row is a language, not a game)
g_orig2 <- table(g_orig$ABVD_language, g_orig$Goal_structure)
g_orig3 <- cbind(rownames(g_orig2),g_orig2)
rownames(g_orig3) <- NULL
colnames(g_orig3)[1] <- "ABVD_language"
g_orig3 <- as.data.frame.matrix(g_orig3)

g_orig3[,2] <- as.integer(g_orig3[,2])
g_orig3[,3] <- as.integer(g_orig3[,3])
g_orig3[,4] <- as.integer(g_orig3[,4])
g_orig3[,5] <- as.integer(g_orig3[,5])
g_orig3[,6] <- as.integer(g_orig3[,6])
g_orig3[,7] <- as.integer(g_orig3[,7])

#get the long & lat back in df
g_orig3$longitude <- NA
g_orig3$latitude <- NA
for(i in 1:nrow(g_orig3)){
  foo.nr <- which(g_orig$ABVD_language %in% g_orig3[i,1])
  if(length(foo.nr)==1){
    g_orig3$longitude[i] <- g_orig$longitude[foo.nr]
    g_orig3$latitude[i] <- g_orig$latitude[foo.nr]
  }
  if(length(foo.nr)>1){
    g_orig3$longitude[i] <- g_orig$longitude[foo.nr[1]]
    g_orig3$latitude[i] <- g_orig$latitude[foo.nr[1]]
  }
}

# get the ABVD codes back in df
g_orig3$ABVD_Code <-NA
for (i in 1:nrow(g_orig3)){
	foo.nr <- which(g_orig$ABVD_language %in% g_orig3$ABVD_language[i])
	if(length(foo.nr) >= 1 ){
		g_orig3$ABVD_Code[i] <- g_orig$ABVD_code[foo.nr[1]]
	}
}

# remove rows where there are no games
g_orig4 <- NA
for(i in 1:nrow(g_orig3)){
  if(sum(g_orig3[i,2:7])!=0){
      g_orig4 <- rbind(g_orig4,g_orig3[i,])
  }
}
g_orig5 <- g_orig4[!is.na(g_orig4$ABVD_language),]


# merge the original and final
g_data <- full_join(g_final, g_orig5, by="ABVD_language", 
          suffix = c("_final", "_orig"), keep.y=TRUE)

# data wrangling
g_data2 <- pivot_longer(g_data, 
            cols = c(15:20, 24:29),
            names_to = c("Goal_structure","Version"),
            names_sep = "_",
            values_to = "Count"
            )

g_data2$ABVD_Code <- g_data2$ABVD_Code_orig		# to merge with pulotu data below
g_data2$Version2 <- as.factor(g_data2$Version)
g_data2$Version2 <- factor(g_data2$Version2, levels = c("orig", "final"))

# to compare included and excluded cases on their cultural attributes
# first use code from data_preprocessing.R
d <- read.csv("Data/Pulotu_Database_4_7_2015.txt", sep='\t', fileEncoding = "UTF-8", header=T)  # downloaded from https://figshare.com/articles/dataset/Pulotu_Dataset_Updated_4th_July_2015_www_pulotu_com/1472946
d_strat <- read.csv("Data/SocStrat_Watts_2016.csv")    # from Joseph Watts (personal communication). Identical to published 2016 dataset, just not subsetted

d_strat2 <- d_strat                     ##
d_strat2$Culture <- as.character(d_strat2$Culture)				            	##
d_strat2$Culture[d_strat2$Culture=="Merina"] <- "Malagasy (Merina)"				            	##
d_strat2$Culture[d_strat2$Culture=="Tanala"] <- "Malagasy (Tanala)"				            	##
d_strat2$Culture <- as.factor(d_strat2$Culture)				            	##

p <- merge(d, d_strat2, by="Culture", all.x=T)				            	##
p2 <- p[,c("Culture","isocode", "ABVD_Code.x", "v1.Traditional_Time_Focus", "Social_Stratification", 
           "v28.Land.based_hunting_performed_by_one_or_more_groups", "v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups", 
           "v14.Conflict_within_communities", "v15.Conflict_between_communities_of_the_culture", "v16.Conflict_with_other_cultures")]
names(p2) <- c("Culture","isocode", "ABVD_Code", "v1.Traditional_Time_Focus", "Social_Stratification", 
               "v28.Land.based_hunting_performed_by_one_or_more_groups", "v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups", 
               "v14.Conflict_within_communities", "v15.Conflict_between_communities_of_the_culture", "v16.Conflict_with_other_cultures")

# second get all the variables into gdata2
for(i in 1:nrow(g_data2)){
  foo.nr <- which(p2$ABVD_Code %in% g_data2$ABVD_Code[i])
  if(length(foo.nr)==1){
    g_data2 $Traditional_Time_Focus[i] <- p2$v1.Traditional_Time_Focus[foo.nr]
    g_data2 $Social_Stratification[i] <- p2$Social_Stratification[foo.nr]
	g_data2 $Land.based_hunting_performed_by_one_or_more_groups[i] <- p2$v28.Land.based_hunting_performed_by_one_or_more_groups[foo.nr]
    g_data2 $Fishing_and_water.based_hunting_performed_by_one_or_more_groups[i] <- p2$v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups[foo.nr]
    g_data2 $Conflict_within_communities[i] <- p2$v14.Conflict_within_communities[foo.nr]
    g_data2 $Conflict_between_communities_of_the_culture[i] <- p2$v15.Conflict_between_communities_of_the_culture[foo.nr]
    g_data2 $Conflict_with_other_cultures[i] <- p2$v16.Conflict_with_other_cultures[foo.nr]
  }
  if(length(foo.nr)>1){
    g_data2 $Traditional_Time_Focus[i] <- p2$v1.Traditional_Time_Focus[foo.nr[1]]
    g_data2 $Social_Stratification[i] <- p2$Social_Stratification[foo.nr[1]]
	g_data2 $Land.based_hunting_performed_by_one_or_more_groups[i] <- p2$v28.Land.based_hunting_performed_by_one_or_more_groups[foo.nr[1]]
    g_data2 $Fishing_and_water.based_hunting_performed_by_one_or_more_groups[i] <- p2$v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups[foo.nr[1]]
    g_data2 $Conflict_within_communities[i] <- p2$v14.Conflict_within_communities[foo.nr[1]]
    g_data2 $Conflict_between_communities_of_the_culture[i] <- p2$v15.Conflict_between_communities_of_the_culture[foo.nr[1]]
    g_data2 $Conflict_with_other_cultures[i] <- p2$v16.Conflict_with_other_cultures[foo.nr[1]]
  }
    if(length(foo.nr)==0){
    g_data2 $Traditional_Time_Focus[i] <- NA
    g_data2 $Social_Stratification[i] <-NA
	g_data2 $Land.based_hunting_performed_by_one_or_more_groups[i] <- NA
    g_data2 $Fishing_and_water.based_hunting_performed_by_one_or_more_groups[i] <- NA
    g_data2 $Conflict_within_communities[i] <- NA
    g_data2 $Conflict_between_communities_of_the_culture[i] <- NA
    g_data2 $Conflict_with_other_cultures[i] <- NA
  }
  rm("foo.nr")
}

# # g_data2 $Social_Stratification <- as.factor(g_data2 $Social_Stratification)
# g_data2 $Land.based_hunting_performed_by_one_or_more_groups <- as.factor(g_data2 $Land.based_hunting_performed_by_one_or_more_groups)
# g_data2 $Fishing_and_water.based_hunting_performed_by_one_or_more_groups <- as.factor(g_data2 $Fishing_and_water.based_hunting_performed_by_one_or_more_groups)
# g_data2 $Conflict_within_communities <- as.factor(g_data2 $Conflict_within_communities)
# g_data2 $Conflict_between_communities_of_the_culture <- as.factor(g_data2 $Conflict_between_communities_of_the_culture)
# g_data2 $Conflict_with_other_cultures <- as.factor(g_data2 $Conflict_with_other_cultures)

g_data2$Land  <- ifelse(as.numeric(as.character(g_data2 $v28.Land.based_hunting_performed_by_one_or_more_groups))>1,1,0)
g_data2$Water  <- ifelse(as.numeric(as.character(g_data2 $Fishing_and_water.based_hunting_performed_by_one_or_more_groups))>2,1,0)
g_data2$Con_Between  <- ifelse(as.numeric(as.character(g_data2$Conflict_between_communities_of_the_culture))<3,1,0)
g_data2$Con_Within  <- ifelse(as.numeric(as.character(g_data2 $Conflict_within_communities))<4,1,0)
g_data2$Con_Other  <- ifelse(as.numeric(as.character(g_data2 $Conflict_with_other_cultures))<3,1,0)
g_data2$Strat  <- ifelse(as.numeric(as.character(g_data2 $Social_Stratification))>2,1,0)
																					 ##



# # # # # # # # 
# plotting

# put them on a map in different colors
mapWorld <- borders("world", colour="gray75", fill="gray75") # create a layer of borders
ggplot() +   mapWorld +
geom_point(aes(x=g_orig5$longitude, y=g_orig5$latitude) ,color="red", size=3) +
geom_point(aes(x=g_final$longitude, y=g_final$latitude) ,color="black", size=1.5) +
labs(x="", y="") +
theme_few(base_size=14) +
theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())
ggsave("Output/Filters_map.pdf")


# make a barplot
# version 1
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = Goal_structure, label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind() +
  labs(fill="Goal structure") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_barplot1.pdf")

# version 2
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = Goal_structure, label = "")) + 
  geom_col(position = "dodge") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind() +
  labs(fill="Goal structure") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_barplot2.pdf")

# version 3
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = Version, label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind() +
  guides(fill="none") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_barplot3.pdf")
 
# version 4
ggplot(g_data2, aes(x = as.factor(ABVD_language), y = Count, 
                    fill = Goal_structure, label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("Version2")  +
  # geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  # scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind() +
  labs(fill="Goal structure", x="ABVD language") +
  theme_few(base_size=10) +
  geom_hline(yintercept=5, lty=2, color="gray50") +
  # scale_x_discrete(guide = guide_axis(n.dodge=2,check.overlap=TRUE))  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_barplot4.pdf")


# # # # # # # # # # #
# plot cultural variants with games by culture

# plot the cultures included and excluded by stratification
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = as.factor(Strat), label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind(na.value="gray80") +
  labs(fill="Social stratification") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_socstrat.pdf")


# plot the cultures included and excluded by Land.based_hunting_performed_by_one_or_more_groups
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = as.factor(Land), label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind(na.value="gray80") +
  labs(fill="Land-based hunting") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_land.pdf")


# plot the cultures included and excluded by Fishing_and_water.based_hunting_performed_by_one_or_more_groups
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = as.factor(Water), label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind(na.value="gray80") +
  labs(fill="Water-based hunting") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_water.pdf")

# plot the cultures included and excluded by Conflict_within_communities
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = as.factor(Con_Within), label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind(na.value="gray80") +
  labs(fill="Inter-personal conflict") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_within.pdf")

# plot the cultures included and excluded by Conflict_between_communities_of_the_culture
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = as.factor(Con_Between), label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind(na.value="gray80") +
  labs(fill="Intra-group conflict") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50") 
ggsave("Output/Filters_between.pdf")

# plot the cultures included and excluded by Conflict_with_other_cultures
ggplot(g_data2, aes(x = as.factor(Version), y = Count, 
                    fill = as.factor(Con_Other), label = "")) + 
  geom_col(position = "stack") +
  facet_wrap("ABVD_language",scales="free") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(name ="Version", limits=c("orig","final")) +
  scale_fill_colorblind(na.value="gray80") +
  labs(fill="Inter-group conflict") +
  theme_few(base_size=10) +
  scale_y_continuous(breaks = c(0,5,10,15,25,40)) +
  geom_hline(yintercept=5, lty=2, color="gray50")
ggsave("Output/Filters_other.pdf")
  
  
  
  
  