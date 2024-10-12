#### pigeon cog test battery analysis and visualization code written by Sarah Kritzler and Mary Flaim
#### 30.09.2024

#### importing necessary libraries and data sheet
setwd('C:/Users/flaimm7d/Desktop/cogTestBattery')
library(readxl)
library(tidyverse)
library(psych)
library(ggpubr)
library(ggrepel)




minorRevisions_cogTest_v1 <- read_excel("mostCurrent_10052024.xlsx", sheet = "everythingEverything") 

#renaming variable column
minorRevisions_cogTest_v1$Fourand5RawRanks = minorRevisions_cogTest_v1$`4and5RawRanks`
  

####DVS of interest


####Correlations between DVS of all tasks 
#corr5DVsMid <- lowerCor(minorRevisions_cogTest_v1[ , c(16, 26, 31, 35, 39)])
corr5DVsMidRank <- corr.test(minorRevisions_cogTest_v1[ , c(16, 26, 31, 35, 39, 2, 3)], method = "spearman") #rank data
corr5DVsMidRaw <- corr.test(minorRevisions_cogTest_v1[ , c(14, 24, 29, 33, 37, 2, 3)], method = "spearman") #raw data, with mid points
corr5DVsLastRaw <- corr.test(minorRevisions_cogTest_v1[ , c(14, 27, 29, 41, 42, 2, 3)], method = "spearman") #raw data, with mid points

corr4DVsMidRaw <- corr.test(minorRevisions_cogTest_v1[ , c(14, 29, 33, 37, 2)], method = "spearman") #raw data, with mid points
corr4DVsLastRaw <- corr.test(minorRevisions_cogTest_v1[ , c(14, 29, 41, 42, 2)], method = "spearman") #raw data, with mid points

pretty5DVsMidRaw <- lowerCor(minorRevisions_cogTest_v1[ , c(14, 24, 29, 33, 37, 2, 3)], use = "pairwise", method= "spearman", minlength =2)


####PCA with rank data from 5 DVs (reaction time - middle, symbolic match to sample, delayed match to sample intercept and slope - middle, 
#### and reversal learning)
test = na.omit(minorRevisions_cogTest_v1)
pca_res5 <- prcomp(minorRevisions_cogTest_v1[ , c(16, 26, 31, 35, 39)], scale. = FALSE, na.action=na.omit)
pca_res5 <- prcomp(~Fourand5RawRanks+PT8Mid3Rank+SMTSrawRank+DMTSintMidRawRank+DMTSMidslopeRawRank, na.action=na.exclude, data = minorRevisions_cogTest_v1)
pca_res5 <- prcomp(test[ , c(16, 20, 31, 35, 39)], scale. = FALSE, na.action=na.omit)


pca_five_mid_rank = principal(minorRevisions_cogTest_v1[ , c(16, 26, 31, 35, 39)], nfactors = 2, residuals = FALSE,rotate="none",n.obs=NA, covar=FALSE,
                      scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",
                      use ="pairwise",cor="spearman",correct=.5,weight=NULL)

pca_five_mid_rank$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name)) %>% 
  ggplot(aes(x=PC1, y=PC2)) + geom_point() + geom_text(aes(x=PC1+0.2, label=ageNum))


dataFiveMidRank = pca_five_mid_rank$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name, tasksPreBattery))

####PCA with raw data from 5 DVs (reaction time - middle, symbolic match to sample, delayed match to sample intercept and slope - middle, 
#### and reversal learning)

pca_five_mid_raw = principal(minorRevisions_cogTest_v1[ , c(14, 24, 29, 33, 37)], nfactors = 2, residuals = FALSE,rotate="none",n.obs=NA, covar=FALSE,
                              scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",
                              use ="pairwise",cor="spearman",correct=.5,weight=NULL)

pca_five_mid_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name)) %>% 
  ggplot(aes(x=PC1, y=PC2)) + geom_point() + geom_text(aes(x=PC1+0.2, label=ageNum))


dataFiveMidRaw = pca_five_mid_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name, tasksPreBattery))



####Visualization of the PCs, from the PCA conducted with 5 variables, mid points (SMTS, RT, DMTS intercept and slop, reversal learning)

pc1pc2_FiveMidRaw = dataFiveMidRaw %>% 
  ggplot(aes(x=PC1, y=PC2, color=ageNum)) + 
  geom_hline(yintercept = 0, linetype="dashed", color="lightgrey") +
  geom_vline(xintercept = 0, linetype="dashed", color="lightgrey") +
  geom_point(size=5) + 
  geom_text(aes(x=PC1+0.07, y=PC2+0.05, label=ageNum)) +
  scale_color_gradient(low = "grey70", high="grey10") +
  #scale_x_continuous(limits = c(min(dataFiveMidRaw$PC1, na.rm=T),max(dataFiveMidRaw$PC1, na.rm=T))) +
  #scale_y_continuous(limits = c(min(dataFiveMidRaw$PC2, na.rm=T),max(dataFiveMidRaw$PC2, na.rm=T))) +

  #scale_x_continuous(limits = c(min(dataFiveMidRaw$PC1, na.rm=T),max(dataFiveMidRaw$PC1, na.rm=T))) +
  #scale_y_continuous(limits = c(min(dataFiveMidRaw$PC2, na.rm=T),max(dataFiveMidRaw$PC2, na.rm=T))) +
  ylim(c(-2.25, 2.25)) +
  xlim(c(-2.25, 2.25))+
  #geom_text_repel(aes(x=PC1+0.1, label=ageNum)) +
  labs(title="Association between PC1 and PC2 with reference to age", color="Age\nin years") + theme_pubr(14) +
  theme(legend.position = c(.94,.2))


pc1age_FiveMidRaw = dataFiveMidRaw %>% 
  ggplot(aes(x=ageNum, y=PC1)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  ylim(c(-2.25, 2.25)) +
  #scale_y_continuous(limits = c(-1*(abs(max(dataFiveMidRaw$PC1, na.rm=T))),(abs(max(dataFiveMidRaw$PC1, na.rm=T))))) +
  labs(title="Middle of Training \n A",
       x="Age in years") +
  theme_pubr(14)

pc2age_FiveMidRaw = dataFiveMidRaw %>% 
  ggplot(aes(x=ageNum, y=PC2)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  ylim(c(-2.25, 2.25)) +
  #scale_y_continuous(limits = c(-1*(abs(max(dataFiveMidRaw$PC2, na.rm=T))),(abs(max(dataFiveMidRaw$PC2, na.rm=T))))) +
  labs(title="\n B",
       x="Age in years") + theme_pubr(14)


#combine plots
cowplot::plot_grid(pc1age_FiveMidRaw, pc2age_FiveMidRaw, ncol=2)
cowplot::plot_grid(pc1pc2_FiveMidRaw, cowplot::plot_grid(pc1age_FiveMidRaw, pc2age_FiveMidRaw, ncol=2), ncol=1, rel_heights = c(1,0.4))


ggsave("PCAwith5DVsMidPoint.svg", device="svg", width = 10, height=13)


# Correlations ------------------------------------------------------------


psych::corr.test(dataFiveMidRaw$ageNum, dataFiveMidRaw$PC1)
psych::corr.test(dataFiveMidRaw$ageNum, dataFiveMidRaw$PC2)


####PCA with raw data from 5 DVs, end time point (reaction time - middle, symbolic match to sample, delayed match to sample intercept and slope - middle, 
#### and reversal learning)

pca_five_last_raw = principal(minorRevisions_cogTest_v1[ , c(14, 27, 29, 41, 42)], nfactors = 2, residuals = FALSE,rotate="none",n.obs=NA, covar=FALSE,
                             scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",
                             use ="pairwise",cor="spearman",correct=.5,weight=NULL)

pca_five_last_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name)) %>% 
  ggplot(aes(x=PC1, y=PC2)) + geom_point() + geom_text(aes(x=PC1+0.2, label=ageNum))


dataFiveLastRaw = pca_five_last_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name, tasksPreBattery))

####Visualization of the PCs, from the PCA conducted with 5 variables, end time points (SMTS, RT, DMTS intercept and slop, reversal learning)

pc1pc2_FiveLastRaw = dataFiveLastRaw %>% 
  ggplot(aes(x=PC1, y=PC2, color=ageNum)) + 
  geom_hline(yintercept = 0, linetype="dashed", color="lightgrey") +
  geom_vline(xintercept = 0, linetype="dashed", color="lightgrey") +
  geom_point(size=5) + 
  geom_text(aes(x=PC1+0.05, y=PC2+0.04, label=ageNum)) +
  scale_color_gradient(low = "grey70", high="grey10") +
  scale_x_continuous(limits = c(-1*max(dataFiveLastRaw$PC1, na.rm=T),max(dataFiveLastRaw$PC1, na.rm=T))) +
  scale_y_continuous(limits = c(-1*max(dataFiveLastRaw$PC2, na.rm=T),max(dataFiveLastRaw$PC2, na.rm=T))) +
  #geom_text_repel(aes(x=PC1+0.1, label=ageNum)) +
  labs(title="Association between PC1 and PC2 with reference to age", color="Age\nin years") + theme_pubr(14) +
  theme(legend.position = c(.94,.2))


pc1age_FiveLastRaw = dataFiveLastRaw %>% 
  ggplot(aes(x=ageNum, y=PC1)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  labs(title="End of Training \n C",
       x="Age in years") +
  ylim(c(-2.25, 2.25))+
  theme_pubr(14)

pc2age_FiveLastRaw = dataFiveLastRaw %>% 
  ggplot(aes(x=ageNum, y=PC2)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  labs(title="\n D",
       x="Age in years") +
  ylim(c(-2.25, 2.25))+ 
  theme_pubr(14)



#combine plots
cowplot::plot_grid(pc1pc2_FiveLastRaw, cowplot::plot_grid(pc1age_FiveLastRaw, pc2age_FiveLastRaw, ncol=2), ncol=1, rel_heights = c(1,0.4))
cowplot::plot_grid(pc1age_FiveMidRaw, pc2age_FiveMidRaw, pc1age_FiveLastRaw, pc2age_FiveLastRaw, ncol = 2)

ggsave("PCAwith_FiveLastRaw_onAge.svg", device="svg", width = 10, height=13)


# Correlations ------------------------------------------------------------

psych::corr.test(dataFiveLastRaw$ageNum, dataFiveLastRaw$PC1)
psych::corr.test(dataFiveLastRaw$ageNum, dataFiveLastRaw$PC2)



####PCA with raw data from 4 DVs (symbolic match to sample, delayed match to sample intercept and slope - middle, and reversal learning)
test = na.omit(minorRevisions_cogTest_v1)
pca_res <- prcomp(minorRevisions_cogTest_v1[ , c(14, 29, 33, 37)], scale. = FALSE, na.action=na.omit)
pca_res <- prcomp(~Fourand5RawRanks+SMTSrawRank+DMTSintMidRawRank+DMTSMidslopeRawRank, na.action=na.exclude, data = minorRevisions_cogTest_v1)
pca_res <- prcomp(test[ , c(14, 29, 33, 37)], scale. = FALSE, na.action=na.omit)


PCA_four_mid_raw = principal(minorRevisions_cogTest_v1[ , c(14, 29, 33, 37)], nfactors = 2, residuals = FALSE,rotate="none",n.obs=NA, covar=FALSE,
          scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",
          use ="pairwise",cor="spearman",correct=.5,weight=NULL)

PCA_four_mid_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name)) %>% 
  ggplot(aes(x=PC1, y=PC2)) + geom_point() + geom_text(aes(x=PC1+0.2, label=ageNum))


dataFourMidRaw = PCA_four_mid_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name, tasksPreBattery))

####Visualization of the PCs, from the PCA conducted with 4 variables mid point (SMTS, DMTS intercept and slop, reversal learning)

pc1pc2_FourMidRaw = dataFourMidRaw %>% 
  ggplot(aes(x=PC1, y=PC2, color=ageNum)) + 
  geom_hline(yintercept = 0, linetype="dashed", color="lightgrey") +
  geom_vline(xintercept = 0, linetype="dashed", color="lightgrey") +
  geom_point(size=5) + 
  geom_text(aes(x=PC1+0.07, y=PC2+0.05, label=ageNum)) +
  scale_color_gradient(low = "grey70", high="grey10") +
  xlim(c(-2.25, 2.25)) + ylim(c(-2.25, 2.25))+
  #scale_x_continuous(limits = c(-1*max(dat$PC1, na.rm=T),max(dat$PC1, na.rm=T))) +
  #scale_y_continuous(limits = c(-1*max(dat$PC2, na.rm=T),max(dat$PC2, na.rm=T))) +
  #geom_text_repel(aes(x=PC1+0.1, label=ageNum)) +
  labs(title="Association between PC1 and PC2\nwith reference to age", color="Age\nin years") + theme_pubr(14) +
  theme(legend.position = c(.94,.2))


pc1age_FourMidRaw = dataFourMidRaw %>% 
  ggplot(aes(x=ageNum, y=PC1)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  labs(title="Middle of Training\n A",
       x="Age in years") +
  ylim(c(-2.25, 2.25))+
  theme_pubr(14)

pc2age_FourMidRaw = dataFourMidRaw %>% 
  ggplot(aes(x=ageNum, y=PC2)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  labs(title="\n B",
       x="Age in years") + 
  ylim(c(-2.25, 2.25))+ theme_pubr(14)



#combine plots
cowplot::plot_grid(pc1pc2_FourMidRaw, cowplot::plot_grid(pc1age_FourMidRaw, pc2age_FourMidRaw, ncol=2), ncol=1, rel_heights = c(1,0.4))


ggsave("PCA_FourMidRaw.svg", device="svg", width = 10, height=13)


# Correlations ------------------------------------------------------------

psych::corr.test(dataFourMidRaw$ageNum, dataFourMidRaw$PC1)
psych::corr.test(dataFourMidRaw$ageNum, dataFourMidRaw$PC2)


#### histograms

pc1hist = dataFourMidRaw %>% 
  ggplot(aes(PC1)) + geom_histogram(binwidth = .25, color="black")


pc2hist = dataFourMidRaw %>% 
  ggplot(aes(PC2)) + geom_histogram(binwidth = .25, color="black")



####PCA with raw data from 4 DVs end point (symbolic match to sample, delayed match to sample intercept and slope - end, and reversal learning)
test = na.omit(minorRevisions_cogTest_v1)
pca_res <- prcomp(minorRevisions_cogTest_v1[ , c(14, 29, 41, 42)], scale. = FALSE, na.action=na.omit)
pca_res <- prcomp(~Fourand5RawRanks+SMTSrawRank+DMTSintMidRawRank+DMTSMidslopeRawRank, na.action=na.exclude, data = minorRevisions_cogTest_v1)
pca_res <- prcomp(test[ , c(14, 29, 41, 42)], scale. = FALSE, na.action=na.omit)


PCA_four_last_raw = principal(minorRevisions_cogTest_v1[ , c(14, 29, 41, 42)], nfactors = 2, residuals = FALSE,rotate="none",n.obs=NA, covar=FALSE,
                             scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",
                             use ="pairwise",cor="spearman",correct=.5,weight=NULL)

PCA_four_last_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name)) %>% 
  ggplot(aes(x=PC1, y=PC2)) + geom_point() + geom_text(aes(x=PC1+0.2, label=ageNum))


dataFourLastRaw = PCA_four_last_raw$scores |>
  bind_cols(minorRevisions_cogTest_v1 %>% select(ageNum, Name, tasksPreBattery))


####Visualization of the PCs, from the PCA conducted with 4 variables mid point (SMTS, DMTS intercept and slop, reversal learning)

pc1pc2_FourLastRaw = dataFourLastRaw %>% 
  ggplot(aes(x=PC1, y=PC2, color=ageNum)) + 
  geom_hline(yintercept = 0, linetype="dashed", color="lightgrey") +
  geom_vline(xintercept = 0, linetype="dashed", color="lightgrey") +
  geom_point(size=5) + 
  geom_text(aes(x=PC1+0.07, y=PC2+0.05, label=ageNum)) +
  scale_color_gradient(low = "grey70", high="grey10") +
  xlim(c(-2.25, 2.25)) + ylim(c(-2.25, 2.25))+
  #scale_x_continuous(limits = c(-1*max(dat$PC1, na.rm=T),max(dat$PC1, na.rm=T))) +
  #scale_y_continuous(limits = c(-1*max(dat$PC2, na.rm=T),max(dat$PC2, na.rm=T))) +
  #geom_text_repel(aes(x=PC1+0.1, label=ageNum)) +
  labs(title="Association between PC1 and PC2\nwith reference to age", color="Age\nin years") + theme_pubr(14) +
  theme(legend.position = c(.94,.2))


pc1age_FourLastRaw = dataFourLastRaw %>% 
  ggplot(aes(x=ageNum, y=PC1)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  labs(title="End of Training\n C",
       x="Age in years") +
  ylim(c(-2.25,2.25))+
  theme_pubr(14)

pc2age_FourLastRaw = dataFourLastRaw %>% 
  ggplot(aes(x=ageNum, y=PC2)) + geom_point(size=3) +
  geom_smooth(method="lm", color="black") +
  labs(title="\n D",
       x="Age in years") + 
  ylim(c(-2.25,2.25))+ 
  theme_pubr(14)



#combine plots
cowplot::plot_grid(pc1pc2_FourLastRaw, cowplot::plot_grid(pc1age_FourLastRaw, pc2age_FourLastRaw, ncol=2), ncol=1, rel_heights = c(1,0.4))
cowplot::plot_grid(pc1age_FourMidRaw, pc2age_FourMidRaw, pc1age_FourLastRaw, pc2age_FourLastRaw, ncol = 2)

ggsave("PCA_FourLastRaw_onAge.svg", device="svg", width = 10, height=13)


# Correlations ------------------------------------------------------------

psych::corr.test(dataFourLastRaw$ageNum, dataFourLastRaw$PC1)
psych::corr.test(dataFourLastRaw$ageNum, dataFourLastRaw$PC2)


#### histograms

pc1hist = dataFourLastRaw %>% 
  ggplot(aes(PC1)) + geom_histogram(binwidth = .25, color="black")


pc2hist = dataFourLastRaw %>% 
  ggplot(aes(PC2)) + geom_histogram(binwidth = .25, color="black")


# Supplements -------------------------------------------------------------

dat %>% 
  ggplot(aes(ageNum, tasksPreBattery)) + geom_point() + 
  labs(title="Association between age and task experience")

pc1exp = dat %>% 
  ggplot(aes(x=tasksPreBattery, y=PC1, color=tasksPreBattery)) + geom_point(size=5) +
  geom_smooth(method="lm") +
  labs(title="Association between task experience and PC1")

pc2aexp = dat %>% 
  ggplot(aes(x=tasksPreBattery, y=PC2, color=tasksPreBattery)) + geom_point(size=5) +
  geom_smooth(method="lm") +
  labs(title="Association between task experience and PC2")



#cowplot::plot_grid(pc1age, pc2age, pc1exp, pc2aexp, ncol=2)