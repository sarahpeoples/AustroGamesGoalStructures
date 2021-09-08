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

g_data2$Version2 <- as.factor(g_data2$Version)
g_data2$Version2 <- factor(g_data2$Version2, levels = c("orig", "final"))


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



