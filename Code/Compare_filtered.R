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



# # # # # # # # #
# wrangle data to get in right format for tables
t.dat <- g_data[,c(32,24:29,15:20)]
t.dat$Culture <- NA
t.dat$SocStrat <- NA
t.dat$Land <- NA
t.dat$Water <- NA
t.dat$Within <- NA
t.dat$Btwn <- NA
t.dat$Other <- NA

for(i in 1:nrow(t.dat)){
  foo.nr <- which(p2$ABVD_Code %in% t.dat $ABVD_Code_orig[i])
  	if(length(foo.nr)==0){
  		print(t.dat $ABVD_Code_orig[i])
	} else if(length(foo.nr)==1){
  		t.dat$Culture[i] <- p2$Culture[foo.nr]
	    t.dat$SocStrat[i] <- p2$Social_Stratification[foo.nr]
		t.dat$Land[i] <- p2$v28.Land.based_hunting_performed_by_one_or_more_groups[foo.nr]
	    t.dat$Water[i] <- p2$v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups[foo.nr]
    	t.dat$Within[i] <- p2$v14.Conflict_within_communities[foo.nr]
	    t.dat$Btwn[i] <- p2$v15.Conflict_between_communities_of_the_culture[foo.nr]
    	t.dat$Other[i] <- p2$v16.Conflict_with_other_cultures[foo.nr]
	} else if(length(foo.nr)>1){
  		t.dat$Culture[i] <- p2$Culture[foo.nr[1]]
	    t.dat$SocStrat[i] <- p2$Social_Stratification[foo.nr[1]]
		t.dat$Land[i] <- p2$v28.Land.based_hunting_performed_by_one_or_more_groups[foo.nr[1]]
	    t.dat$Water[i] <- p2$v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups[foo.nr[1]]
    	t.dat$Within[i] <- p2$v14.Conflict_within_communities[foo.nr[1]]
	    t.dat$Btwn[i] <- p2$v15.Conflict_between_communities_of_the_culture[foo.nr[1]]
    	t.dat$Other[i] <- p2$v16.Conflict_with_other_cultures[foo.nr[1]]
	}
	# if(length(foo.nr)==0){
		# foo.list <- unlist(strsplit(t.dat $ABVD_Code_orig[i], ";"))
 		# unlist(strsplit(p2$ABVD_Code,"; ")) %in% foo.list[1]
		
		# }
	# foo.match <- which(longlat$ABVD_code %in% t.dat $ABVD_Code_orig[i])
	# if(length(foo.match)>0)
		# t.dat$ABVD_language[i] <- longlat$ABVD_language[foo.match]
}

# manually add the 10 missing rows (weren't matched b/c of the ABVDs)
add_row <- function(data, i, foo.nr){
	t.dat <- data
	t.dat$Culture[i] <- p2$Culture[foo.nr]
	t.dat$SocStrat[i] <- p2$Social_Stratification[foo.nr]
	t.dat$Land[i] <- p2$v28.Land.based_hunting_performed_by_one_or_more_groups[foo.nr]
	t.dat$Water[i] <- p2$v31.Fishing_and_water.based_hunting_performed_by_one_or_more_groups[foo.nr]
	t.dat$Within[i] <- p2$v14.Conflict_within_communities[foo.nr]
	t.dat$Btwn[i] <- p2$v15.Conflict_between_communities_of_the_culture[foo.nr]
	t.dat$Other[i] <- p2$v16.Conflict_with_other_cultures[foo.nr]
	return(t.dat)
}

which(is.na(t.dat$Culture))
# i=32
# unlist(strsplit(t.dat$ABVD_Code_orig[i],";"))
# sum(unlist(strsplit(p2$ABVD_Code,"; ")) %in% unlist(strsplit(t.dat$ABVD_Code_orig[i],";")))
# which(unlist(strsplit(p2$ABVD_Code,"; ")) %in% unlist(strsplit(t.dat$ABVD_Code_orig[i],";")))
# unlist(strsplit(p2$ABVD_Code,"; ")) %in% unlist(strsplit(t.dat$ABVD_Code_orig[i],";"))
# p2$ABVD_Code
# p2[61,]
t.dat <- add_row(t.dat, 5,99)	# tahiti	#checked
t.dat <- add_row(t.dat, 6,86)	# rarotonga	#rows 56 and 86
t.dat <- add_row(t.dat, 8,88)	# rennell & bellona	#checked
t.dat <- add_row(t.dat, 11,64)	# marshall islands	#checked
t.dat <- add_row(t.dat, 12,21)	# chuuk		#checked
t.dat <- add_row(t.dat, 13,115)	# woleai	#checked
t.dat <- add_row(t.dat, 15,15)	# buka		#checked
t.dat <- add_row(t.dat, 24,97)		# subanun	#checked
t.dat <- add_row(t.dat, 31,60)	# manus(titan)	#checked
t.dat <- add_row(t.dat, 32,61)		# maori		#checked

# convert to coding in paper
t.dat $Land  <- ifelse(as.numeric(as.character(t.dat $Land))>1,1,0)
t.dat $Water  <- ifelse(as.numeric(as.character(t.dat $Water))>2,1,0)
t.dat $Btwn  <- ifelse(as.numeric(as.character(t.dat $Btwn))<3,1,0)
t.dat $Within  <- ifelse(as.numeric(as.character(t.dat $Within))<4,1,0)
t.dat $Other  <- ifelse(as.numeric(as.character(t.dat $Other))<3,1,0)
t.dat $SocStrat  <- ifelse(as.numeric(as.character(t.dat $SocStrat))>2,1,0)
t.dat $Included <- ifelse(is.na(t.dat$Competitive_final), 0,1)


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
ggplot(t.dat, aes(x = as.factor(Culture), y = as.factor(SocStrat), 
                    color = as.factor(Included), size=2)) + 
  geom_point() +
  scale_color_colorblind(na.value="gray80") +
  labs(color="Included", x = "Culture", y="Social Stratification") +
  guides(size="none") +
  theme_few(base_size=10)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_socstrat.pdf")


# plot the cultures included and excluded by Land.based_hunting_performed_by_one_or_more_groups
ggplot(t.dat, aes(x = as.factor(Culture), y = as.factor(Land), 
                    color = as.factor(Included), size=2)) + 
  geom_point() +
  scale_color_colorblind(na.value="gray80") +
  labs(color="Included", x = "Culture", y="Land-based hunting in groups") +
  guides(size="none") +
  theme_few(base_size=10)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_land.pdf")


# plot the cultures included and excluded by Fishing_and_water.based_hunting_performed_by_one_or_more_groups
ggplot(t.dat, aes(x = as.factor(Culture), y = as.factor(Water), 
                    color = as.factor(Included), size=2)) + 
  geom_point() +
  scale_color_colorblind(na.value="gray80") +
  labs(color="Included", x = "Culture", y="Water-based hunting in groups") +
  guides(size="none") +
  theme_few(base_size=10)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_water.pdf")

# plot the cultures included and excluded by Conflict_within_communities
ggplot(t.dat, aes(x = as.factor(Culture), y = as.factor(Within), 
                    color = as.factor(Included), size=2)) + 
  geom_point() +
  scale_color_colorblind(na.value="gray80") +
  labs(color="Included", x = "Culture", y="Inter-personal conflict") +
  guides(size="none") +
  theme_few(base_size=10)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_within.pdf")

# plot the cultures included and excluded by Conflict_between_communities_of_the_culture
ggplot(t.dat, aes(x = as.factor(Culture), y = as.factor(Btwn), 
                    color = as.factor(Included), size=2)) + 
  geom_point() +
  scale_color_colorblind(na.value="gray80") +
  labs(color="Included", x = "Culture", y="Intra-group conflict") +
  guides(size="none") +
  theme_few(base_size=10)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_between.pdf")

# plot the cultures included and excluded by Conflict_with_other_cultures
ggplot(t.dat, aes(x = as.factor(Culture), y = as.factor(Other), 
                    color = as.factor(Included), size=2)) + 
  geom_point() +
  scale_color_colorblind(na.value="gray80") +
  labs(color="Included", x = "Culture", y="Inter-group conflict") +
  guides(size="none") +
  theme_few(base_size=10)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
ggsave("Output/Filters_other.pdf")
  
  
  
  
  

# # # # # # # # # # #
# tables 
socstrat_tab <- t.dat[,c(14,15,2:13)]
socstrat_tab <-socstrat_tab[order(socstrat_tab $SocStrat,na.last=FALSE), ] 
write.csv(socstrat_tab, "Output/Filter_socstrat.csv", quote=F, row.names=F )

land_tab <- t.dat[,c(14,16,2:13)]
land_tab <- land_tab[order(land_tab $Land,na.last=FALSE), ] 
write.csv(land_tab, "Output/Filter_land.csv", quote=F, row.names=F )

water_tab <- t.dat[,c(14,17,2:13)]
water_tab <- water_tab[order(water_tab $Water,na.last=FALSE), ] 
write.csv(water_tab, "Output/Filter_water.csv", quote=F, row.names=F )

within_tab <- t.dat[,c(14,18,2:13)]
within_tab <- within_tab[order(within_tab $Within,na.last=FALSE), ] 
write.csv(within_tab, "Output/Filter_within.csv", quote=F, row.names=F )

btwn_tab <- t.dat[,c(14,19,2:13)]
btwn_tab <- btwn_tab[order(btwn_tab $Btwn,na.last=FALSE), ] 
write.csv(btwn_tab, "Output/Filter_btwn.csv", quote=F, row.names=F )

other_tab <- t.dat[,c(14,20,2:13)]
other_tab <- other_tab[order(other_tab $Other,na.last=FALSE), ] 
write.csv(other_tab, "Output/Filter_other.csv", quote=F, row.names=F )

# 2x2 table with percentage included/excluded and levels of cultural covariates
socstrat_tab2 <- as.matrix(table(t.dat$SocStrat, t.dat$Included, useNA="ifany"))
socstrat_tab2 <- cbind(rownames(socstrat_tab2),socstrat_tab2)
colnames(socstrat_tab2)[1] <- "Social stratification"
write.csv(socstrat_tab2, "Output/Filter_socstrat2.csv", quote=F, row.names=F)

land_tab2 <- as.matrix(table(t.dat$Land, t.dat$Included, useNA="ifany"))
land_tab2 <- cbind(rownames(land_tab2), land_tab2)
colnames(land_tab2)[1] <- "Land"
write.csv(land_tab2, "Output/Filter_land2.csv", quote=F, row.names=F)

water_tab2 <- as.matrix(table(t.dat$Water, t.dat$Included, useNA="ifany"))
water_tab2 <- cbind(rownames(water_tab2), water_tab2)
colnames(water_tab2)[1] <- "Water"
write.csv(water_tab2, "Output/Filter_water2.csv", quote=F, row.names=F)

within_tab2 <- as.matrix(table(t.dat$Within, t.dat$Included, useNA="ifany"))
within_tab2 <- cbind(rownames(within_tab2), within_tab2)
colnames(within_tab2)[1] <- "Inter-personal conflict"
write.csv(within_tab2, "Output/Filter_within2.csv", quote=F, row.names=F)

btwn_tab2 <- as.matrix(table(t.dat$Btwn, t.dat$Included, useNA="ifany"))
btwn_tab2 <- cbind(rownames(btwn_tab2), btwn_tab2)
colnames(btwn_tab2)[1] <- "Intra-group conflict"
write.csv(btwn_tab2, "Output/Filter_btwn2.csv", quote=F, row.names=F)

other_tab2 <- as.matrix(table(t.dat$Other, t.dat$Included, useNA="ifany"))
other_tab2 <- cbind(rownames(other_tab2), other_tab2)
colnames(other_tab2)[1] <- "Inter-group conflict"
write.csv(other_tab2, "Output/Filter_other2.csv", quote=F, row.names=F)



# table(within_tab$Competitive_final>=1, within_tab$Competitive_orig>=1, within_tab$Within, useNA= "ifany")
# table(within_tab$"Cooperative group_final">=1, within_tab $"Cooperative group_orig">=1, within_tab$Within,useNA= "ifany")
# table(within_tab $Solitary_orig>=1, within_tab $Solitary_final>=1, within_tab $Within, useNA= "ifany")

