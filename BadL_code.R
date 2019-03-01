# prepare R for the work and load librarys
rm(list = ls())
gc()

library(ggplot2)
library(reshape2)
library(ggthemes)

# 1. read data into R and prepare it for the Work
setwd("C:/Users/ma22buky/Documents/GCEF_Data_demografie/R_table")
excl_gcef <- read.csv("Tab_excl_BL.csv", stringsAsFactors = F)

#  create new column with climate and treatment and subset for it
excl_gcef[,"clim_trea"] <- paste(excl_gcef$climate, excl_gcef$treatment, sep = "_")

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

# 2. first plots about count of individuals per species
#   2.1 without any treatment (Angeb plots)
spec_count_all <- as.data.frame(table(excl_gcef$species))

ggplot(spec_count_all, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ylab("Individuals") + 
  xlab("Species") + 
  ggtitle("Individuals per species") + theme_stata() + scale_fill_economist() + guides(fill = F) 

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
  ylab("Individuals") + xlab("Species") + ggtitle("Individuals per climate treatment")

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

spec_count_treat$species <- sort(unique(excl_gcef$species))   

spec_count_treat <- melt(spec_count_treat, id.vars = "species")

col_pal <- c("sienna1", "sienna3", "palegreen", "palegreen3", "cyan1", "cyan3", "azure2", "azure4")

ggplot(spec_count_treat, aes(x = species, y = value, fill = variable, color = species)) + 
  geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = col_pal, 
                                                                      labels = c("Control ambient", "Control future",
                                                                                 "Insect ambient" , "Insect future",
                                                                                 "Insect and pathogen ambient",
                                                                                 "Insect and pathogen future" ,
                                                                                 "Pathogen ambient", "Pathogen future")) +
  theme_stata() + theme(axis.text.y = element_text(angle = 0)) + ylab("Individuals") + 
  xlab("Species") + scale_color_economist() + guides(color = F, fill = guide_legend(title = "Treatment")) + 
  geom_hline(yintercept = 50) + ggtitle("Individuals per treatment")
  
# 3. Boxplots about size differences concerning different aspects
#   3.1 Size comparison between climates
specs_dia <- unique(diam_specs$species)
spec_list_dia <- c()

for(i in 1:length(specs_dia))
{
  spec_list_dia[i] <- list(assign(paste(specs_dia[i]), subset(excl_gcef, species == specs_dia[i])))
  rm(list = specs_dia[i])
  names(spec_list_dia)[i] <- specs_dia[i]
}

ggplot(diam_specs, aes(x = species, y = area_cm2, fill = climate)) + geom_boxplot() + 
  scale_fill_brewer(labels = c("Ambient", "Future")) + theme_stata() + 
  guides(color = F, fill = guide_legend(title = "Climate")) + ylab("Size (in cm2)") + 
  xlab("Species")

t.test(spec_list_dia[[1]][, "area_cm2"] ~ spec_list_dia[[1]][, "climate"])
t.test(spec_list_dia[[2]][, "area_cm2"] ~ spec_list_dia[[2]][, "climate"])
t.test(spec_list_dia[[3]][, "area_cm2"] ~ spec_list_dia[[3]][, "climate"])
t.test(spec_list_dia[[4]][, "area_cm2"] ~ spec_list_dia[[4]][, "climate"])
t.test(spec_list_dia[[5]][, "area_cm2"] ~ spec_list_dia[[5]][, "climate"])

#   3.2 Size comparison between climate and treatment
ggplot(diam_specs, aes(x = species, y = area_cm2, fill = clim_trea)) + geom_boxplot() #change order to what the barplots are like!!!

# 4. Size ~ Flowers count
#   4.1 subset the different species into a list, first diameter specs 
specs_dia <- unique(diam_specs$species)
spec_list_dia <- c()

for(i in 1:length(specs_dia))
{
  spec_list_dia[i] <- list(assign(paste(specs_dia[i]), subset(excl_gcef, species == specs_dia[i])))
  rm(list = specs_dia[i])
  names(spec_list_dia)[i] <- specs_dia[i]
}

plot_outs <- c()
for(i in 1:length(spec_list_dia))
  plot_outs[i] <- list(ggplot(spec_list_dia[[i]], aes(x = flowers_1st_harvest, y = area_cm2)) +
                         geom_point() + ggtitle(paste(spec_list_dia[[i]][,"species"])) + 
  geom_smooth(method = "lm") + theme_stata() + ylab("Size (in cm2)"), xlab("Flower count"))

#calculate stats for lms
lm_sums <- c()
for(i in 1:length(spec_list_dia))
{
  lm_sums[i] <- list(lm(spec_list_dia[[i]] [,"flowers_1st_harvest"] ~ spec_list_dia[[i]][,"area_cm2"]))
  names(lm_sums)[i] <- names(spec_list_dia)[i]
}

names(lm_sums)

#   4.2 do the same but for the leafe count species
specs_lea <- unique(leaf_specs$species)
spec_list_lea <- c()

for(i in 1:length(specs_lea))
{
  spec_list_lea[i] <- list(assign(paste(specs_lea[i]), subset(leaf_specs, species == specs_lea[i])))
  rm(list = specs_lea[i])
  
  names(spec_list_lea)[i] <- specs_lea[i]
}

ggplot(spec_list_lea[[1]], aes(x = flowers_1st_harvest, y = leaf_number)) + geom_point()


 #test IPm already 
pla_lan <- subset(excl_gcef, species == "Pla_lan" & plot == "1_1")
pla_lan$leaf_number[is.na(pla_lan$leaf_number)] <- 0
pla_lan$seedl_start_sum <- sum(pla_lan$X.seedlings, na.rm = T)

pla_lan <- subset(pla_lan, leaf_number != "NA")
pla_lan$stage <- "continuous" 

pla_lan$leaf_number_august18[is.na(pla_lan$leaf_number_august18)] <- 0

for(i in 1:nrow(pla_lan))
{
  if(pla_lan[i, "leaf_number_august18"] == 0)
    pla_lan[i, "stagenext"] <- "dead"
  else
    pla_lan[i, "stagenext"] <- "continuous"
}
pla_lan$surv <- ifelse(pla_lan$stagenext == "dead", 0, 1)
pla_lan$leaf_number_august18 <- ifelse(pla_lan$stagenext == "dead", NA, pla_lan$leaf_number_august18)

library(IPMpack)

test <- data.frame(stage = pla_lan$stage, stageNext = pla_lan$stagenext, surv = pla_lan$surv,
                   size = pla_lan$leaf_number, sizeNext = pla_lan$leaf_number_august18, 
                   flowering = pla_lan$flowers_1st_harvest, fruits = pla_lan$seedl_start_sum, number = 1)

survobj <- makeSurvObj(test, surv ~ size + sizeNext)
sizeobj <- makeGrowthObj(test, sizeNext ~ size)
fecobj <- makeFecObj(test, Formula = fruits ~ size)


makeIPMFmatrix(survObj = survobj, growObj = sizeobj)

plot(sizeobj)
