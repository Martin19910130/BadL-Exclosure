# prepare R for the work and load librarys
rm(list = ls())
gc()

library(ggplot2)
library(reshape2)
library(ggthemes)

# 1. read data into R and prepare it for the Work
setwd("~/Documents/Germany/Other/Martin/GCEF_Data/R_table/")
excl_gcef <- read.csv("Tab_excl_BL.csv", stringsAsFactors = F)

#   1.1 get overview about columns (what's Factor, what's num, what's character... had problems once)
str(excl_gcef)

#   1.2 subset into the two size measurments (longest leaf, diameter)
diam_specs <- subset(excl_gcef, species != "Tra_ori" & species != "Pla_lan")
leaf_specs <- subset(excl_gcef, species == "Tra_ori" | species == "Pla_lan")

#   1.3 subset into two different climate treatments
ambi_clim <- subset(excl_gcef, climate == "ambient")
futu_clim <- subset(excl_gcef, climate == "future")

#   1.4 subset for the 4 different treatments
treats <- unique(excl_gcef$treatment)
treat_subs <- c()

for(i in 1:length(treats))
{
  #store subsets in a list
  treat_subs[i] <- list(assign(paste(treats[i]), subset(excl_gcef, treatment == treats[i])))
  
  #rm environment cause it's already in the list
  rm(list = treats[i])
  
  #name list entries
  names(treat_subs)[i] <- treats[i]
}

#   1.5 create new column with climate and treatment and subset for it
excl_gcef[,"clim_trea"] <- paste(excl_gcef$climate, excl_gcef$treatment, sep = "_")
unique(excl_gcef$treatment)

# 2. first plots about count of individuals per species
#   2.1 without any treatment (Angeb plots)
spec_count_all <- as.data.frame(table(excl_gcef$species))

ggplot(spec_count_all, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ylab("Count") + 
  xlab("Species") + 
  ggtitle("Individual's per species") + theme_stata() + scale_fill_economist() + guides(fill = F) + 
  theme(axis.text.x = element_text(angle = 90))

rm(spec_count_all)

#   2.2 treatment comparison including climate
ambi_specs <- as.data.frame(table(ambi_clim$species))
colnames(ambi_specs) <- c("species", "count_ambient")

futu_specs <- as.data.frame(table(futu_clim$species))
colnames(futu_specs) <- c("species", "count_future")

clim_specs_count <- merge(futu_specs, ambi_specs, by = "species")
clim_specs_count <- melt(clim_specs_count, id.vars = "species")
rm(ambi_specs, futu_specs)

ggplot(clim_specs_count, aes(x = species, y = value, fill = variable, color = species)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_stata() + scale_color_economist() + 
  guides(color = F, fill = guide_legend(title = "Climate", 
                                        lable.theme = element_text(c("Future", "Ambient")))) + 
  scale_fill_brewer(labels = c("Future", "Ambient")) + theme(axis.text.x = element_text(angle = 90)) +
  ylab("Individuals") + xlab("Species")

#   2.3 treatment comparison including ins, path, ins + path, con
spec_counts <- c()
spec_counts_treat <- as.matrix(aggregate(excl_gcef$species, by = list(excl_gcef$treatment, 
                                                                      excl_gcef$climate), 
                                         FUN = table))

spec_count_treat <- data.frame(species = colnames(spec_counts_treat[, 3:ncol(spec_counts_treat)]), 
                               control_ambient = as.numeric(as.character(spec_counts_treat[1, 3:ncol(spec_counts_treat)])),
                               control_future = as.numeric(as.character(spec_counts_treat [5, 3:ncol(spec_counts_treat)])),
                               insect_ambient = as.numeric(as.character(spec_counts_treat[2, 3:ncol(spec_counts_treat)])),
                               insect_future = as.numeric(as.character(spec_counts_treat[6, 3:ncol(spec_counts_treat)])),
                               ins_pat_ambient = as.numeric(as.character(spec_counts_treat[3, 3:ncol(spec_counts_treat)])),
                               ins_pat_future = as.numeric(as.character(spec_counts_treat[7, 3:ncol(spec_counts_treat)])),
                               patho_ambient = as.numeric(as.character(spec_counts_treat[4, 3:ncol(spec_counts_treat)])),
                               patho_future = as.numeric(as.character(spec_counts_treat[8, 3:ncol(spec_counts_treat)])))

spec_count_treat$species <- unique(excl_gcef$species)   

test <- melt(spec_count_treat, id.vars = "species")

col_pal <- c("sienna1", "sienna3", "palegreen", "palegreen3", "cyan1", "cyan3", "azure2", "azure4")

ggplot(test, aes(x = species, y = value, fill = variable, color = species)) + 
  geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = col_pal, 
                                                                      labels = c("Control ambient", "Control future",
                                                                                 "Insect ambient" , "Insect future",
                                                                                 "Insect and pathogen ambient",
                                                                                 "Insect and pathogen future" ,
                                                                                 "Pathogen ambient", "Pathogen future")) +
  theme_stata() + theme(axis.text.y = element_text(angle = 0)) + ylab("Individuals") + 
  xlab("Species") + scale_color_economist() + guides(color = F, fill = guide_legend(title = "Treatment"))
  

  