#### curiosity over time analysis #### 
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(readxl)
library(cowplot)

setwd("~/Documents/3/curiosity2")

#load in data 
mdata_og <- read.csv("data/mdata_sofia.csv")
mdata_og <- mdata_og[1:351,]
mdata_og <- droplevels(mdata_og)

#clean up 
summary(mdata_og)
mdata_og$approach_phys <- as.factor(mdata_og$approach_phys)
mdata_og$lick <- as.factor(mdata_og$lick)
mdata_og$eat <- as.factor(mdata_og$eat)

#individuals 
inds <- as.data.frame(unique(mdata_og$name))
names(inds)[1] <- "name"

#pull individual info 
temp <- readRDS("data/c19082019.Rds")
temp <- temp[,c(1:6)]
inds <- merge(inds, temp, all.x=TRUE)
rm(temp)

#label condition type 
mdata_og$fam_condition <- NA
mdata_og$fam_condition <- ifelse(mdata_og$stimulus == "Familiar Food" | mdata_og$stimulus == "Familiar object", "familiar", "novel")

#just take the novel things 
mdata_novel <- mdata_og[which(mdata_og$fam_condition == "novel"),]

#summary of data
summary(mdata_novel)
table(mdata_og$fam_condition, mdata_og$stimulus)
table(mdata_og$fam_condition, mdata_og$condition, mdata_og$stimulus)
table(inds$sex)

###visuals####
#sex####
a <- ggplot() +
  geom_violin(data=mdata_novel, aes(x=sex, y=latency_touch, group = sex)) + 
  xlab("Sex") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_violin(data=mdata_novel, aes(x=sex, y=approach_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_violin(data=mdata_novel, aes(x=sex, y=sniffs_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_violin(data=mdata_novel, aes(x=sex, y=manipulate_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()

e <- ggplot() +
  geom_violin(data=mdata_novel, aes(x=sex, y=grid_time, group = sex)) + 
  xlab("Sex") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("sex_both_violin.jpg", x, base_height=7, base_width=14)

a <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=sex, y=latency_touch, group = sex)) + 
  xlab("Sex") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=sex, y=approach_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=sex, y=sniffs_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=sex, y=manipulate_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()

e <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=sex, y=grid_time, group = sex)) + 
  xlab("Sex") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("sex_both_box.jpg", x, base_height=7, base_width=14)

mdata_novel_food <- mdata_novel[which(mdata_novel$ntype=="Food"),]

a <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=sex, y=latency_touch, group = sex)) + 
  xlab("Sex") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=sex, y=approach_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=sex, y=sniffs_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=sex, y=manipulate_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()

e <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=sex, y=grid_time, group = sex)) + 
  xlab("Sex") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("sex_food_box.jpg", x, base_height=7, base_width=14)

mdata_novel_object <- mdata_novel[which(mdata_novel$ntype=="Object"),]

a <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=sex, y=latency_touch, group = sex)) + 
  xlab("Sex") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=sex, y=approach_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=sex, y=sniffs_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=sex, y=manipulate_n, group = sex)) + 
  xlab("Sex") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()

e <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=sex, y=grid_time, group = sex)) + 
  xlab("Sex") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("sex_object_box.jpg", x, base_height=7, base_width=14)



#groupsize####
a <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=groupsize, y=latency_touch, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=groupsize, y=approach_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=groupsize, y=sniffs_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=groupsize, y=manipulate_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()

e <- ggplot() +
  geom_boxplot(data=mdata_novel, aes(x=groupsize, y=grid_time, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("groupsize_both_box.jpg", x, base_height=7, base_width=14)

mdata_novel_food <- mdata_novel[which(mdata_novel$ntype=="Food"),]

a <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=groupsize, y=latency_touch, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=groupsize, y=approach_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=groupsize, y=sniffs_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=groupsize, y=manipulate_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()

e <- ggplot() +
  geom_boxplot(data=mdata_novel_food, aes(x=groupsize, y=grid_time, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel_food$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("groupsize_food_box.jpg", x, base_height=7, base_width=14)

mdata_novel_object <- mdata_novel[which(mdata_novel$ntype=="Object"),]

a <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=groupsize, y=latency_touch, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()  

b <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=groupsize, y=approach_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()  

c <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=groupsize, y=sniffs_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic() 

d <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=groupsize, y=manipulate_n, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()

e <- ggplot() +
  geom_boxplot(data=mdata_novel_object, aes(x=groupsize, y=grid_time, group = groupsize)) + 
  xlab("groupsize") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel_object$condition) + 
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("groupsize_object_box.jpg", x, base_height=7, base_width=14)


#age####
a <- ggplot() +
  geom_point(data=mdata_novel, aes(x=age_m, y=latency_touch, group = age_m)) + 
  xlab("age_m") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel$condition) + 
  ggpubr::stat_cor(data=mdata_novel, aes(x=age_m, y=latency_touch), label.x=7) + 
  geom_smooth(data=mdata_novel, aes(x=age_m, y=latency_touch), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_novel, aes(x=age_m, y=approach_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel$condition) + 
  ggpubr::stat_cor(data=mdata_novel, aes(x=age_m, y=approach_n), label.x=7) + 
  geom_smooth(data=mdata_novel, aes(x=age_m, y=approach_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_novel, aes(x=age_m, y=sniffs_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel$condition) + 
  ggpubr::stat_cor(data=mdata_novel, aes(x=age_m, y=sniffs_n), label.x=7) + 
  geom_smooth(data=mdata_novel, aes(x=age_m, y=sniffs_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic() 

d <- ggplot() +
  geom_point(data=mdata_novel, aes(x=age_m, y=manipulate_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel$condition) + 
  ggpubr::stat_cor(data=mdata_novel, aes(x=age_m, y=manipulate_n), label.x=7) + 
  geom_smooth(data=mdata_novel, aes(x=age_m, y=manipulate_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()

e <- ggplot() +
  geom_point(data=mdata_novel, aes(x=age_m, y=grid_time, group = age_m)) + 
  xlab("age_m") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel$condition) + 
  ggpubr::stat_cor(data=mdata_novel, aes(x=age_m, y=grid_time), label.x=7) + 
  geom_smooth(data=mdata_novel, aes(x=age_m, y=grid_time), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("age_both.jpg", x, base_height=7, base_width=14)

mdata_novel_food <- mdata_novel[which(mdata_novel$ntype=="Food"),]

a <- ggplot() +
  geom_point(data=mdata_novel_food, aes(x=age_m, y=latency_touch, group = age_m)) + 
  xlab("age_m") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel_food$condition) + 
  ggpubr::stat_cor(data=mdata_novel_food, aes(x=age_m, y=latency_touch), label.x=7) + 
  geom_smooth(data=mdata_novel_food, aes(x=age_m, y=latency_touch), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_novel_food, aes(x=age_m, y=approach_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel_food$condition) + 
  ggpubr::stat_cor(data=mdata_novel_food, aes(x=age_m, y=approach_n), label.x=7) + 
  geom_smooth(data=mdata_novel_food, aes(x=age_m, y=approach_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_novel_food, aes(x=age_m, y=sniffs_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel_food$condition) + 
  ggpubr::stat_cor(data=mdata_novel_food, aes(x=age_m, y=sniffs_n), label.x=7) + 
  geom_smooth(data=mdata_novel_food, aes(x=age_m, y=sniffs_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic() 

d <- ggplot() +
  geom_point(data=mdata_novel_food, aes(x=age_m, y=manipulate_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel_food$condition) + 
  ggpubr::stat_cor(data=mdata_novel_food, aes(x=age_m, y=sniffs_n), label.x=7) + 
  geom_smooth(data=mdata_novel_food, aes(x=age_m, y=sniffs_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()

e <- ggplot() +
  geom_point(data=mdata_novel_food, aes(x=age_m, y=grid_time, group = age_m)) + 
  xlab("age_m") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel_food$condition) + 
  ggpubr::stat_cor(data=mdata_novel_food, aes(x=age_m, y=grid_time), label.x=7) + 
  geom_smooth(data=mdata_novel_food, aes(x=age_m, y=grid_time), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("age_food.jpg", x, base_height=7, base_width=14)

mdata_novel_object <- mdata_novel[which(mdata_novel$ntype=="Object"),]

a <- ggplot() +
  geom_point(data=mdata_novel_object, aes(x=age_m, y=latency_touch, group = age_m)) + 
  xlab("age_m") + 
  ylab("Latency to touch") + 
  facet_wrap(mdata_novel_object$condition) + 
  ggpubr::stat_cor(data=mdata_novel_object, aes(x=age_m, y=latency_touch), label.x=7) + 
  geom_smooth(data=mdata_novel_object, aes(x=age_m, y=latency_touch), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_novel_object, aes(x=age_m, y=approach_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of approaches") + 
  facet_wrap(mdata_novel_object$condition) + 
  ggpubr::stat_cor(data=mdata_novel_object, aes(x=age_m, y=approach_n), label.x=7) + 
  geom_smooth(data=mdata_novel_object, aes(x=age_m, y=approach_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_novel_object, aes(x=age_m, y=sniffs_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of sniffs") + 
  facet_wrap(mdata_novel_object$condition) + 
  ggpubr::stat_cor(data=mdata_novel_object, aes(x=age_m, y=sniffs_n), label.x=7) + 
  geom_smooth(data=mdata_novel_object, aes(x=age_m, y=sniffs_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic() 

d <- ggplot() +
  geom_point(data=mdata_novel_object, aes(x=age_m, y=manipulate_n, group = age_m)) + 
  xlab("age_m") + 
  ylab("Number of manipulates") + 
  facet_wrap(mdata_novel_object$condition) + 
  ggpubr::stat_cor(data=mdata_novel_object, aes(x=age_m, y=manipulate_n), label.x=7) + 
  geom_smooth(data=mdata_novel_object, aes(x=age_m, y=manipulate_n), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()

e <- ggplot() +
  geom_point(data=mdata_novel_object, aes(x=age_m, y=grid_time, group = age_m)) + 
  xlab("age_m") + 
  ylab("Exploration duration") + 
  facet_wrap(mdata_novel_object$condition) + 
  ggpubr::stat_cor(data=mdata_novel_object, aes(x=age_m, y=grid_time), label.x=7) + 
  geom_smooth(data=mdata_novel_object, aes(x=age_m, y=grid_time), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()

x <- plot_grid(a,b,c,d,e, nrow=3)

save_plot("age_object.jpg", x, base_height=7, base_width=14)

rm(a, b, c, d, e, x)

#quick look at lms####
summary(lm(approach_n ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(approach_alone_n ~ sex + groupsize + age_m + ntype, data=mdata_novel))
summary(lm(approach_soc_n ~ sex + groupsize + age_m + ntype, data=mdata_novel))
summary(lm(closest_appr_dis ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(sniffs_n ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(manipulate_n ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(explore_n ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(man_dur ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(sniff_dur ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(exp_dur ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(lm(grid_time ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))


####THE THREE MODELS####
#1 latency to touch - neophobia
summary(glm(latency_touch ~ sex + groupsize + age_m + ntype + condition, data=mdata_novel))
summary(glm(latency_touch ~ sex + groupsize + age_m + ntype + condition + fam_condition, data=mdata_og))
summary(glm(latency_touch ~ sex + groupsize + age_m + ntype*condition*fam_condition, data=mdata_og))

hist(mdata_novel$latency_touch)

#2 number of exploration 
x <- (glm(explore_n ~ sex + groupsize + age_m + ntype + condition + ntype*condition, data=mdata_novel))
summary(glm(explore_n ~ sex + groupsize + age_m + ntype + condition + fam_condition, data=mdata_og))
x <- (glm(explore_n ~ sex + groupsize + age_m + ntype*condition*fam_condition, data=mdata_og))
plot(allEffects(x))
emmeans::emmeans(x, pairwise ~ fam_condition ~  condition|ntype|fam_condition)

 
#3 length of exploration 
summary(glm(exp_dur ~ sex + groupsize + age_m + ntype + condition + ntype*condition, data=mdata_novel))
summary(glm(exp_dur ~ sex + groupsize + age_m + ntype + condition + fam_condition, data=mdata_og))
x <- (glm(exp_dur ~ sex + groupsize + age_m + ntype*condition*fam_condition, data=mdata_og))
plot(allEffects(x))
emmeans::emmeans(x, pairwise ~ fam_condition ~  condition|ntype|fam_condition)



#one line for each individual####
xdata <- mdata_novel

#means
mdata_nov_sum <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_approach_n = mean(approach_n, na.rm = T))

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_latency_touch = mean(latency_touch, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_sniffs_n = mean(sniffs_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_manipulate_n = mean(manipulate_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_explore_n = mean(explore_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_grid_time = mean(grid_time, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(min_closest_appr_dis = min(closest_appr_dis, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

#sums
temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_approach_n = sum(approach_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_latency_touch = sum(latency_touch, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_sniffs_n = sum(sniffs_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_manipulate_n = sum(manipulate_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_explore_n = sum(explore_n, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_grid_time = sum(grid_time, na.rm = T))
mdata_nov_sum <- merge(mdata_nov_sum, temp)

###add quick PC score to each line #### 
as.data.frame(colnames(mdata_nov_sum))
temp <- mdata_nov_sum[,c(11,12,13,14,16)]
indslist <- mdata_nov_sum$name
temp <- as.data.frame(scale(temp)) #scale all the variables 

PCA(temp, ncp = 5, graph = TRUE)
res.pca <- PCA(temp, graph = FALSE)
get_eigenvalue(res.pca) 
fviz_eig(res.pca) #visualize

x <- res.pca$ind
x <- as.data.frame(x$coord)
colnames(x) <- c("PC1","PC2")
x <- x[,1:2]

mdata_nov_sum <- cbind(mdata_nov_sum, x)

#add control condition info ####

mdata_control <- mdata_og[which(mdata_og$fam_condition == "familiar"),]

xdata <- mdata_control 

#means
mdata_fam_sum <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_approach_n_fam = mean(approach_n, na.rm = T))

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_latency_touch_fam = mean(latency_touch, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_sniffs_n_fam = mean(sniffs_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_manipulate_n_fam = mean(manipulate_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_explore_n_fam = mean(explore_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(mean_grid_time_fam = mean(grid_time, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(min_closest_appr_dis_fam = min(closest_appr_dis, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

#sums
temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_approach_n_fam = sum(approach_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_latency_touch_fam = sum(latency_touch, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_sniffs_n_fam = sum(sniffs_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_manipulate_n_fam = sum(manipulate_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_explore_n_fam = sum(explore_n, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

temp <- xdata %>% group_by(ntype,condition,name) %>% summarize(sum_grid_time_fam = sum(grid_time, na.rm = T))
mdata_fam_sum <- merge(mdata_fam_sum, temp)

###add quick PC score to each line #### 
as.data.frame(colnames(mdata_fam_sum))
temp <- mdata_fam_sum[,c(11,12,13,14,16)]
indslist <- mdata_fam_sum$name
temp <- as.data.frame(scale(temp)) #scale all the variables 

PCA(temp, ncp = 5, graph = TRUE)
res.pca <- PCA(temp, graph = FALSE)
get_eigenvalue(res.pca) 
fviz_eig(res.pca) #visualize

x <- res.pca$ind
x <- as.data.frame(x$coord)
colnames(x) <- c("PC1_fam","PC2_fam")
x <- x[,1:2]

mdata_fam_sum <- cbind(mdata_fam_sum, x)

#put it together 

mdata_sum <- merge(mdata_nov_sum, mdata_fam_sum, by=c("ntype", "condition", "name"))

####actually make one single line for each individual#### 
as.data.frame(colnames(mdata_sum))
mdata_sum_ind <- mdata_sum

mdata_sum_ind$ntype <- tolower(mdata_sum_ind$ntype)
mdata_sum_ind$condition <- tolower(mdata_sum_ind$condition)

mdata_sum_ind$type_condition <- paste(mdata_sum_ind$condition, mdata_sum_ind$ntype,  sep = "_")

mdata_sum_ind$ntype <- NULL
mdata_sum_ind$condition <- NULL

mdata_sum_ind <- reshape(mdata_sum_ind, idvar = "name", timevar = "type_condition", direction = "wide", sep="_")

#looking at correlations between different kinds of conditions #### 

####with PC1####
a <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_individual_object, y=PC1_fam_individual_object)) + 
  xlab("Alone with novel object") + 
  ylab("Alone with familiar object") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_individual_object, y=PC1_fam_individual_object)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_individual_object, y=PC1_fam_individual_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_individual_food, y=PC1_fam_individual_food)) + 
  xlab("Alone with novel food") + 
  ylab("Alone with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_individual_food, y=PC1_fam_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_individual_food, y=PC1_fam_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_social_object, y=PC1_fam_social_object)) + 
  xlab("In group with novel object") + 
  ylab("In group with familiar object") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_social_object, y=PC1_fam_social_object)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_social_object, y=PC1_fam_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


d <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_social_food, y=PC1_fam_social_food)) + 
  xlab("In group with novel food") + 
  ylab("In group with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_social_food, y=PC1_fam_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_social_food, y=PC1_fam_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


x <- plot_grid(a,b,c,d, nrow=2)

save_plot("PC1_corr.jpg", x, base_height=7, base_width=14)

###sum approach#### 
####with sum approach number####
a <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_individual_object, y=sum_approach_n_fam_individual_object)) + 
  xlab("Alone with novel object") + 
  ylab("Alone with familiar object") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_individual_object, y=sum_approach_n_fam_individual_object)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_individual_object, y=sum_approach_n_fam_individual_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_individual_food, y=sum_approach_n_fam_individual_food)) + 
  xlab("Alone with novel food") + 
  ylab("Alone with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_individual_food, y=sum_approach_n_fam_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_individual_food, y=sum_approach_n_fam_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_social_object, y=sum_approach_n_fam_social_object)) + 
  xlab("In group with novel object") + 
  ylab("In group with familiar object") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_social_object, y=sum_approach_n_fam_social_object)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_social_object, y=sum_approach_n_fam_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


d <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_social_food, y=sum_approach_n_fam_social_food)) + 
  xlab("In group with novel food") + 
  ylab("In group with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_social_food, y=sum_approach_n_fam_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_social_food, y=sum_approach_n_fam_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


x <- plot_grid(a,b,c,d, nrow=2)

save_plot("sumapproach_corr.jpg", x, base_height=7, base_width=14)


####with grid time sum####
a <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_individual_object, y=sum_grid_time_fam_individual_object)) + 
  xlab("Alone with novel object") + 
  ylab("Alone with familiar object") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_individual_object, y=sum_grid_time_fam_individual_object)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_individual_object, y=sum_grid_time_fam_individual_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_individual_food, y=sum_grid_time_fam_individual_food)) + 
  xlab("Alone with novel food") + 
  ylab("Alone with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_individual_food, y=sum_grid_time_fam_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_individual_food, y=sum_grid_time_fam_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_social_object, y=sum_grid_time_fam_social_object)) + 
  xlab("In group with novel object") + 
  ylab("In group with familiar object") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_social_object, y=sum_grid_time_fam_social_object)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_social_object, y=sum_grid_time_fam_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


d <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_social_food, y=sum_grid_time_fam_social_food)) + 
  xlab("In group with novel food") + 
  ylab("In group with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_social_food, y=sum_grid_time_fam_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_social_food, y=sum_grid_time_fam_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


x <- plot_grid(a,b,c,d, nrow=2)

save_plot("sumgridtime_corr.jpg", x, base_height=7, base_width=14)

#looking at correlations between different kinds of conditions food vs. object#### 

####with PC1####
a <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_individual_object, y=PC1_individual_food)) + 
  xlab("Alone with novel object") + 
  ylab("Alone with novel food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_individual_object, y=PC1_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_individual_object, y=PC1_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_fam_individual_object, y=PC1_fam_individual_food)) + 
  xlab("Alone with familiar object") + 
  ylab("Alone with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_fam_individual_object, y=PC1_fam_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_fam_individual_object, y=PC1_fam_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_social_object, y=PC1_social_food)) + 
  xlab("In group with novel object") + 
  ylab("In group with novel food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_social_object, y=PC1_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_social_object, y=PC1_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


d <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=PC1_fam_social_object, y=PC1_fam_social_food)) + 
  xlab("In group with familiar object") + 
  ylab("In group with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=PC1_fam_social_object, y=PC1_fam_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=PC1_fam_social_object, y=PC1_fam_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


x <- plot_grid(a,b,c,d, nrow=2)

save_plot("PC1_corr_fvo.jpg", x, base_height=7, base_width=14)

###sum approach#### 
####with sum approach number####
a <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_individual_object, y=sum_approach_n_individual_food)) + 
  xlab("Alone with novel object") + 
  ylab("Alone with novel food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_individual_object, y=sum_approach_n_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_individual_object, y=sum_approach_n_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_fam_individual_object, y=sum_approach_n_fam_individual_food)) + 
  xlab("Alone with familiar object") + 
  ylab("Alone with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_fam_individual_object, y=sum_approach_n_fam_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_fam_individual_object, y=sum_approach_n_fam_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_social_object, y=sum_approach_n_social_food)) + 
  xlab("In group with familiar object") + 
  ylab("In group with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_social_object, y=sum_approach_n_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_social_object, y=sum_approach_n_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


d <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_approach_n_fam_social_object, y=sum_approach_n_fam_social_food)) + 
  xlab("In group with novel object") + 
  ylab("In group with novel food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_approach_n_fam_social_object, y=sum_approach_n_fam_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_approach_n_fam_social_object, y=sum_approach_n_fam_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


x <- plot_grid(a,b,c,d, nrow=2)

save_plot("sumapproach_corr_fvo.jpg", x, base_height=7, base_width=14)


####with grid time sum####
a <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_individual_object, y=sum_grid_time_individual_food)) + 
  xlab("Alone with novel object") + 
  ylab("Alone with novel food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_individual_object, y=sum_grid_time_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_individual_object, y=sum_grid_time_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_fam_individual_object, y=sum_grid_time_fam_individual_food)) + 
  xlab("Alone with familiar object") + 
  ylab("Alone with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_fam_individual_object, y=sum_grid_time_fam_individual_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_fam_individual_object, y=sum_grid_time_fam_individual_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_social_object, y=sum_grid_time_social_food)) + 
  xlab("In group with novel object") + 
  ylab("In group with novel food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_social_object, y=sum_grid_time_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_social_object, y=sum_grid_time_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


d <- ggplot() +
  geom_point(data=mdata_sum_ind, aes(x=sum_grid_time_fam_social_object, y=sum_grid_time_fam_social_food)) + 
  xlab("In group with familiar object") + 
  ylab("In group with familiar food") + 
  ggpubr::stat_cor(data=mdata_sum_ind, aes(x=sum_grid_time_fam_social_object, y=sum_grid_time_fam_social_food)) + 
  geom_smooth(data=mdata_sum_ind, aes(x=sum_grid_time_fam_social_object, y=sum_grid_time_fam_social_food), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  


x <- plot_grid(a,b,c,d, nrow=2)

save_plot("sumgridtime_corr_fvo.jpg", x, base_height=7, base_width=14)

rm(a, b, c, d, x)

###note what each of these mean (which objects/foods)####
#which objects are in which conditions#
table(mdata_novel$condition, mdata_novel$stimulus)
table(mdata_control$condition, mdata_control$stimulus)

#individual_object = plastic larvae, stuffed bunny
#social_object = butterflies, cat mice 
#individual_food = mozzarella balls, mussels
#social_food = tiger prawns, tofu 

####add other curiosity data to the individual data frame ####
cc <- read.csv("data/curiosity_clean.csv")

cdata <- cc[which(cc$session == "CP1"),]
as.data.frame(colnames(cdata))
cdata <- cdata[,c(1, 4:12, 70:72, 111:114, 122)]

MCdata_sum_ind <- merge(mdata_sum_ind, cdata, by="name")
as.data.frame(colnames(MCdata_sum_ind))


a <- ggplot() +
  geom_point(data=MCdata_sum_ind, aes(x=PC1_pos, y=PC1_social_object)) + 
  xlab("PC score curiosity panel (group)") + 
  ylab("PC score object (group)") + 
  ggpubr::stat_cor(data=MCdata_sum_ind, aes(x=PC1_pos, y=PC1_social_object)) + 
  geom_smooth(data=MCdata_sum_ind, aes(x=PC1_pos, y=PC1_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

b <- ggplot() +
  geom_point(data=MCdata_sum_ind, aes(x=as_allobjects_f, y=sum_approach_n_social_object)) + 
  xlab("Approaches curiosity panel (group)") + 
  ylab("Approaches object (group)") + 
  ggpubr::stat_cor(data=MCdata_sum_ind, aes(x=as_allobjects_f, y=sum_approach_n_social_object)) + 
  geom_smooth(data=MCdata_sum_ind, aes(x=as_allobjects_f, y=sum_approach_n_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

c <- ggplot() +
  geom_point(data=MCdata_sum_ind, aes(x=sum_asm_d, y=sum_grid_time_social_object)) + 
  xlab("Time spent with curiosity panel (group)") + 
  ylab("Time spent with object (group)") + 
  ggpubr::stat_cor(data=MCdata_sum_ind, aes(x=sum_asm_d, y=sum_grid_time_social_object)) + 
  geom_smooth(data=MCdata_sum_ind, aes(x=sum_asm_d, y=sum_grid_time_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic()  

d <- ggplot() +
  geom_point(data=MCdata_sum_ind, aes(x=fc_lat_perc, y=mean_latency_touch_social_object)) + 
  xlab("Latency to approach curiosity panel (group)") + 
  ylab("Latency to touch object (group)") + 
  ggpubr::stat_cor(data=MCdata_sum_ind, aes(x=fc_lat_perc, y=mean_latency_touch_social_object)) + 
  geom_smooth(data=MCdata_sum_ind, aes(x=fc_lat_perc, y=mean_latency_touch_social_object), color='#697D75', method=lm, size=1, se =FALSE) +
  theme_classic() 

x <- plot_grid(a,b,c,d, nrow=2)


save_plot("initialexp_CP.jpg", x, base_height=7, base_width=14)


#quick look at correlations with PC1
num_MCdata_sum_ind <- MCdata_sum_ind[,c(2:121,129:138)]
as.data.frame(cor(num_MCdata_sum_ind, num_MCdata_sum_ind$PC1_pos))


summary(lm(PC1_pos ~ mean_sniffs_n_individual_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ min_closest_appr_dis_individual_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ sum_sniffs_n_individual_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ mean_approach_n_fam_individual_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ sum_approach_n_fam_individual_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ mean_manipulate_n_fam_social_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ mean_explore_n_fam_social_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ sum_manipulate_n_fam_social_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ sum_explore_n_fam_social_food, data=MCdata_sum_ind))
summary(lm(PC1_pos ~ min_closest_appr_dis_fam_individual_object, data=MCdata_sum_ind))




####better way to do PCA?? ####
