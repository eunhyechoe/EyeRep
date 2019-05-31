# ---------*----------*---------*---------*---------*---------*---------*---------#
# ------------------------------------------------------------------------------- #
#                                 Coglab R Tool v1.0
#
#                  Edited by Eunhye Choe (Thanks to Prof. Do-joon Yi)
# ------------------------------------------------------------------------------- #
# Date: 2019-05-30
# Environment: R Studio Cloud, Windows 10 / macOS Mojave
# Note: Eye-specific Repetition Priming induced by Binocular Rivalry (Choe, 2019)
# ------------------------------------------------------------------------------- #
# ---------*----------*---------*---------*---------*---------*---------*---------#

# -- REQUIRED PACKAGES -- #
# tidyverse, emmeans, knitr, psych, dplyr, afex, BayesFactor, car, ggplot2, lme4, lmerTest, simr, sjPlot, tibble,
# -- download packages before start -- #



# get ready
rm(list=ls())
set.seed(4228) # for replication

# load packages 
pacman::p_load(tidyverse, emmeans, knitr, papaja, psych, dplyr, afex)
options(knitr.kable.NA = '') # hide NA with knitr function


# ---------------------------------------------------------- #
# 1. load data
# ---------------------------------------------------------- #

ER <- read.csv("/cloud/project/data_EyeRep1.csv")
glimpse(ER)

# SID: participant ID
# CB: dominant eye
# block: total 10 blocks
# trial: 48 trials per block
# eye: which eye the target appeared, 1 = dominant eye, 2 = non-dominant eye
# loc: which location the target appeared, 1-4 quadrants
# locrep: loctation repetition (n-1), 0 = unrepeated, 1 = repeated
# eyerep: eye repetition (n-1), 0 = unrepeated, 1 = repeated
# tarimg: number of target image, 1-10 (1-5: indoor, 6-10: outdoor)
# urban: correct answer for target categoriztion task, 1 = indoor, 2 = outdoor
# resp: participant's response for the task, 1 = indoor, 2 = outdoor
# corr: correctness for the response, 0 = incorrect, 1 = correct, 99 = no response
# RT: reaction time in ms.

# check number of trials for each condition/SID
table(ER$eyerep, ER$SID) # eye repetition probability (rep/unrep)
table(ER$locrep, ER$SID) # location repetition probability (rep/unrep)

table(ER$eye, ER$SID) # eye probability (left/right)
table(ER$loc, ER$SID) # location probability (1-4 quadrant)

table(ER$eye, ER$eyerep, ER$SID) # eye repetition probability for each eye (eye*eyerep)
table(ER$loc, ER$locrep, ER$SID) # location repetition probability for each location (loc*locrep)

# main factor; double to factor
ER$eyerep = factor(ER$eyerep, levels=c(1,0,99), labels=c("Eye_Rep","Eye_Unrep","NULL"))
ER$locrep = factor(ER$locrep, levels=c(1,0,99), labels=c("Loc_Rep","Loc_Unrep","NULL"))

# remove participants

# examples
# ER <- ER[which(ER$SID!=3 | 7 | 11),] # remove participants 3, 7, 11


# ---------------------------------------------------------- #
# 2. Accuracy Analysis
# ---------------------------------------------------------- #

# ---------------------------------------------------------- #
# 2.1. Data rendering
# ---------------------------------------------------------- #

ER[which(ER$corr==99),12] <-0 # corr(12th column); no answer(99) to incorrect(0)
ER <- ER[which(ER$eyerep!="NULL"),] # filter dummy trials (first trial of each block; null rep)
glimpse(ER)

# subject-level, long format (SID/eyerep/locrep)
ERaccL <- ER %>% group_by(SID, eyerep, locrep) %>%
  summarise(Accuracy = mean(corr)*100) %>%
  ungroup()
ERaccL

# summary table: grand mean (eyerep/locrep)
ERaccG <- ERaccL %>% group_by(eyerep, locrep) %>%
  summarise(M = mean(Accuracy), SD = sd(Accuracy)) %>%
  ungroup()
ERaccG %>% kable()

# marginal means of eye rep vs. eye unrep conditions
ERaccL %>% group_by(eyerep) %>%
  summarise(M = mean(Accuracy), SD = sd(Accuracy)) %>%
  ungroup() %>% kable()

# marginal means of location rep vs. location unrep conditions
ERaccL %>% group_by(locrep) %>%
  summarise(M = mean(Accuracy), SD = sd(Accuracy)) %>%
  ungroup() %>% kable()

# wide format (for ggplot geom_segments)
ERaccW <- ERaccL %>% spread(key = locrep, value = Accuracy)
head(ERaccW)

# group-level confidence interval (for ggplot geom_pointrange)
ERaccG$ci <- Rmisc::summarySEwithin(data = ERaccL,
                                    measurevar = "Accuracy", idvar = "SID", withinvars = c("eyerep", "locrep"))$ci
ERaccG$Accuracy <-ERaccG$M
head(ERaccG)

# ----------------------------------------------------------#
# 2.2. Accuracy Plot
# ----------------------------------------------------------#

# -------------------
# 2.2.1. Violin Plot

ggplot(data=ERaccL, aes(x=eyerep, y=Accuracy, fill=locrep)) +
  geom_violin(width = 0.5, trim=TRUE) +
  geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(ERaccW, eyerep == "Eye_Rep"), inherit.aes = FALSE,
               aes(x=1-.12, y=filter(ERaccW, eyerep == "Eye_Rep")$Loc_Rep,
                   xend=1+.12, yend=filter(ERaccW, eyerep == "Eye_Rep")$Loc_Unrep),
               color="gray80") +
  geom_segment(data=filter(ERaccW, eyerep == "Eye_Unrep"), inherit.aes = FALSE,
               aes(x=2-.12, y=filter(ERaccW, eyerep == "Eye_Unrep")$Loc_Rep,
                   xend=2+.12, yend=filter(ERaccW, eyerep == "Eye_Unrep")$Loc_Unrep),
               color="gray80") +
  geom_pointrange(data=ERaccG,
                  aes(x = eyerep, ymin = Accuracy-ci, ymax = Accuracy+ci, group = locrep),
                  position = position_dodge(0.5), color = "darkred", size = 1, show.legend = FALSE) +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#9CE84D", "#C880FF"),
                    labels=c("Repeated", "Unrepeated")) +
  labs(x = "Eye Repetition", 
       y = "Accuracy (%)", 
       fill='Location Repetition') +
  coord_cartesian(ylim = c(80, 100), clip = "on") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank()) 

# -------------------
# 2.2.2. Segment Plot (grand mean, ci, individual mean)

ggplot(data=ERaccL, aes(x=eyerep, y=Accuracy, fill=locrep)) +
  geom_bar(data = ERaccG, stat = "identity", position = position_dodge(1))+
  geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(ERaccW, eyerep == "Eye_Rep"), inherit.aes = FALSE,
               aes(x=1-.12, y=filter(ERaccW, eyerep == "Eye_Rep")$Loc_Rep,
                   xend=1+.12, yend=filter(ERaccW, eyerep == "Eye_Rep")$Loc_Unrep),
               color="gray80") +
  geom_segment(data=filter(ERaccW, eyerep == "Eye_Unrep"), inherit.aes = FALSE,
               aes(x=2-.12, y=filter(ERaccW, eyerep == "Eye_Unrep")$Loc_Rep,
                   xend=2+.12, yend=filter(ERaccW, eyerep == "Eye_Unrep")$Loc_Unrep),
               color="gray80") +
  geom_errorbar(data=ERaccG,
                aes(x = eyerep, ymin = Accuracy-ci, ymax = Accuracy+ci, group = locrep),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Accuracy (%)", 
       fill='Location Repetition') +
  coord_cartesian(ylim = c(80, 100), clip = "on") +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#9CE84D", "#C880FF"),
                    labels=c("Repeated", "Unrepeated")) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))


# ----------------------------------------------------------#
# 2.3. Accuracy Data Analysis
# ----------------------------------------------------------#

head(ERaccL)
head(ER)

# -------------------
# 2.3.1. ANOVA

ER.acc.aov <- aov_ez(id = "SID", dv = c("Accuracy"), data = ERaccL,
                     within = c("eyerep","locrep"))
anova(ER.acc.aov, es = "pes") %>% kable(digits = 4)
# pes = partial eta-squared


# -------------------
# 2.3.2. Linear Mixed Modeling 

ER.acc.lmer <- lmer(corr ~ eyerep*locrep + (1|SID) + (1|tarimg), ER)
anova(ER.acc.lmer)
summary(ER.acc.lmer)


# -------------------
# 2.3.3. Generalized Linear Mixed Modeling 

ER.acc.glmer <- glmer(corr ~ eyerep*locrep + (1|SID) + (1|tarimg), ER,
                      family = binomial(link="logit"))
anova(ER.acc.glmer)
summary(ER.acc.glmer)


# -------------------
# 2.3.4. Test Full Models (WARNING: sometimes messed up)
# !! SAVE CODE & WORKSPACE HERE !!

# see structure
str(ER)

# 'data.frame':	12690 obs. of  13 variables:
# $ SID   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ CB    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ block : int  1 1 1 1 1 1 1 1 1 1 ...
# $ trial : int  2 3 4 5 6 7 8 9 10 11 ...
# $ eye   : int  2 1 1 2 2 1 1 1 2 2 ...
# $ loc   : int  2 2 1 3 3 4 4 1 1 3 ...
# $ locrep: Factor w/ 3 levels "Loc_Rep","Loc_Unrep": 1 1 2 2 1 2 1 2 1 2 ...
# $ eyerep: Factor w/ 3 levels "Eye_Rep","Eye_Unrep": 1 2 1 2 1 2 1 1 2 1 ...
# $ tarimg: int  8 4 7 10 2 9 1 5 8 3 ...
# $ urban : int  2 1 2 2 1 2 1 1 2 1 ...
# $ resp  : int  2 1 2 2 1 1 1 1 2 1 ...
# $ corr  : num  1 1 1 1 1 0 1 1 1 1 ...
# $ RT    : num  1880 1482 1658 1629 1112 

# remove NULL factor level in dataframe
ER$eyerep = factor(ER$eyerep, levels=c("Eye_Rep","Eye_Unrep"), labels=c("Eye_Rep","Eye_Unrep"))
ER$locrep = factor(ER$locrep, levels=c("Loc_Rep", "Loc_Unrep"), labels=c("Loc_Rep","Loc_Unrep"))

# double to integer/factor
ER$RT <- as.integer(ER$RT)


library(parallel)
(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))
ER.acc.full <- afex::mixed(corr ~ eyerep*locrep + (1|SID) + (1|tarimg), ER,
                           method = "LRT", cl = cl, family = binomial(link="logit"),
                           control = glmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
stopCluster(cl)
anova(ER.acc.full)
summary(ER.acc.full)


# easy version 
ER.acc.full <- afex::mixed(corr ~ eyerep*locrep + (1|SID) + (1|tarimg), ER,
                           method = "LRT", family = binomial(link="logit"), expand_re = TRUE)
anova(ER.acc.full)
summary(ER.acc.full)


# ---------------------------------------------------------- #
# 3. Reaction Time Analysis
# ---------------------------------------------------------- #

# ---------------------------------------------------------- #
# 3.1. Data rendering
# ---------------------------------------------------------- #

# -------------------
# 3.1.1. Trimming

# filter null/incorrect data
cER <- ER %>% filter(corr ==1) # remove incorrect trial
cER <- cER %>% filter(locrep!="NULL") # remove null trial

# check distribution
hist(cER$RT)

# trimming 3sd outlier trials
tER <- cER %>% filter(RT > 200 & RT < 10000) %>%
  group_by(SID) %>% # grouping
  nest() %>% # ???
  mutate(lbound = map(data, ~mean(.$RT)-3*sd(.$RT)),
         ubound = map(data, ~mean(.$RT)+3*sd(.$RT))) %>% # make new data (3sd cut)
  unnest(lbound, ubound) %>% 
  unnest(data) %>% 
  mutate(Outlier = (RT < lbound)|(RT > ubound)) %>% # set outlier
  filter(Outlier == FALSE) %>% # filtering outlier
  select(SID, eyerep, locrep, RT, tarimg) # select

# outlier trial ratio
100-100*(nrow(tER)/nrow(cER))
## [1] 1.839836

# mean number of trials for each conditions
tER %>% group_by(SID, eyerep, locrep) %>% 
  summarise(NumTrial = length(RT)) %>%
  ungroup() %>%
  group_by(eyerep, locrep) %>%
  summarise(Mean = mean(NumTrial), 
            Median = median(NumTrial), 
            Min = min(NumTrial), 
            Max = max(NumTrial)) %>% 
  ungroup %>%
  kable()


# -------------------
# 3.1.2. Check Distribution

# before trimming
den1 <- ggplot(cER, aes(x=RT)) + 
  geom_density() + 
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den1

# after trimming
den2 <- ggplot(tER, aes(x=RT)) + 
  geom_density() + 
  theme_bw(base_size = 18) + 
  labs(x = "Trimmed RT") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den2


# -------------------
# 3.1.3. Data Format

# subject-level, long format (SID/eyerep/locrep)
ERrtL <- tER %>% group_by(SID, eyerep, locrep) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

# summary table: grand mean (eyerep/locrep)
ERrtG <- ERrtL %>% group_by(eyerep, locrep) %>%
  summarise(M = mean(RT), SD = sd(RT)) %>%
  ungroup()
ERrtG %>% kable()

# wide format for geom_segments
ERrtW <- ERrtL %>% spread(key = locrep, value = RT)

# group level ci for geom_pointrange
ERrtG$ci <- Rmisc::summarySEwithin(data = ERrtL,
                                   measurevar = "RT", idvar = "SID", withinvars = c("eyerep", "locrep"))$ci
ERrtG$RT <-ERrtG$M


# -------------------
# 3.1.4. Extra Format (for one-way; eye rep effect)

# subject-level, long format (SID/eyerep)
ERrtL.E <- ERrtL %>% group_by(SID, eyerep) %>%
  summarise(RT = mean(RT), SD = sd(RT)) %>%
  ungroup()
ERrtL.E %>% kable()

# summary table: grand mean (eyerep only)
ERrtG.E <- ERrtL %>% group_by(eyerep) %>%
  summarise(M = mean(RT), SD = sd(RT)) %>%
  ungroup()
ERrtG.E %>% kable()

# wide format (eyerep)
ERrtW.E <- ERrtL.E %>% spread(key = eyerep, value = RT)

# group level ci for geom_pointrange
ERrtG.E$ci <- Rmisc::summarySEwithin(data = ERrtL.E,
                                     measurevar = "RT", idvar = "SID", withinvars = c("eyerep"))$ci
ERrtG.E$RT <-ERrtG.E$M



# ----------------------------------------------------------#
# 3.2. Reaction Time Plot
# ----------------------------------------------------------#

# -------------------
# 3.2.1. Violin Plot

ggplot(data=ERrtL, aes(x=eyerep, y=RT, fill=locrep)) +
  geom_violin(width = 0.5, trim=TRUE) +
  geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(ERrtW, eyerep == "Eye_Rep"), inherit.aes = FALSE,
               aes(x=1-.12, y=filter(ERrtW, eyerep == "Eye_Rep")$Loc_Rep,
                   xend=1+.12, yend=filter(ERrtW, eyerep == "Eye_Rep")$Loc_Unrep),
               color="gray80") +
  geom_segment(data=filter(ERrtW, eyerep == "Eye_Unrep"), inherit.aes = FALSE,
               aes(x=2-.12, y=filter(ERrtW, eyerep == "Eye_Unrep")$Loc_Rep,
                   xend=2+.12, yend=filter(ERrtW, eyerep == "Eye_Unrep")$Loc_Unrep),
               color="gray80") +
  geom_pointrange(data=ERrtG,
                  aes(x = eyerep, ymin = RT-ci, ymax = RT+ci, group = locrep),
                  position = position_dodge(0.5), color = "darkred", size = 1, show.legend = FALSE) +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#9CE84D", "#C880FF"),
                    labels=c("Repeated", "Unrepeated")) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)", 
       fill='Location Repetition') +
  coord_cartesian(ylim = c(800, 2600), clip = "on") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank()) 


# -------------------
# 3.2.2. Segment Plot (grand mean, ci, individual mean)

ggplot(data=ERrtL, aes(x=eyerep, y=RT, fill=locrep)) +
  geom_bar(data = ERrtG, stat = "identity", position = position_dodge(1), colour = "black", size = 0.4)+
  geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(ERrtW, eyerep == "Eye_Rep"), inherit.aes = FALSE,
               aes(x=1-.12, y=filter(ERrtW, eyerep == "Eye_Rep")$Loc_Rep,
                   xend=1+.12, yend=filter(ERrtW, eyerep == "Eye_Rep")$Loc_Unrep),
               color="gray80") +
  geom_segment(data=filter(ERrtW, eyerep == "Eye_Unrep"), inherit.aes = FALSE,
               aes(x=2-.12, y=filter(ERrtW, eyerep == "Eye_Unrep")$Loc_Rep,
                   xend=2+.12, yend=filter(ERrtW, eyerep == "Eye_Unrep")$Loc_Unrep),
               color="gray80") +
  geom_errorbar(data=ERrtG,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci, group = locrep),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)", 
       fill='Location Repetition') +
  coord_cartesian(ylim = c(600, 2600), clip = "on") +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#E85E4D", "#C880FF"),
                    labels=c("Repeated", "Unrepeated")) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank())



# -------------------
# 3.2.3. MSK-preferred Plot (grand mean, ci, individual mean)

ggplot(data=ERrtL, aes(x=eyerep, y=RT, fill=locrep)) +
  geom_bar(data = ERrtG, stat = "identity", position = position_dodge(1), colour = "black", size = 0.3)+
  geom_errorbar(data=ERrtG,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci, group = locrep),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)", 
       fill='Location Repetition') +
  coord_cartesian(ylim = c(1000, 1600), clip = "on") +
  theme_bw(base_size = 15) +
  # add aterisk ------------------------------------------ change x, y, or label (*, **, n.s.) #
  # solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  geom_segment(aes(x=0.7, y=1421, xend=1.3, yend=1421), size = 0.3) +
  annotate("text", x=1, y=1426, label="***")+
  geom_segment(aes(x=1.7, y=1480, xend=2.3, yend=1480), size = 0.3) +
  annotate("text", x=2, y=1485, label="***")+
  geom_segment(aes(x=1, y=1546, xend=2, yend=1546), size = 0.3) +
  annotate("text", x=1.5, y=1560, label="italic(n.s.)", parse="TRUE")+ # TRUE parse for italic
  # ------------------------------------------------------------------------------------------ #
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#E85E4D", "#C880FF"),
                    labels=c("Repeated", "Unrepeated")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))


# -------------------
# 3.2.4. Bar Plot (Eye Repetition Only)

# if integer type is required for analysis or plot
# ERrtL.E$RT <- as.integer(ERrtL.E$RT) 

# long format for each condition
# ERrtL.E.ER<-filter(ERrtLE, eyerep == "Eye_Rep")
# ERrtL.E.EUR<-filter(ERrtLE, eyerep == "Eye_Unrep")

# plot 1 (individual point)
ggplot(data=ERrtL.E, aes(x=eyerep, y=RT, fill=eyerep)) +
  geom_bar(data = ERrtG.E, stat = "identity", colour = "black", size = 0.3, position = position_dodge(1))+
  geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=ERrtW.E, inherit.aes = FALSE,
               aes(x=1, y=ERrtW.E$Eye_Rep,
                   xend=2, yend=ERrtW.E$Eye_Unrep),
               color="gray80") +
  geom_errorbar(data=ERrtG.E,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci),
                position = position_dodge(1), width = 0, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)") +
  coord_cartesian(ylim = c(800, 1800), clip = "on") +
  theme_bw(base_size = 15) +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#E85E4D", "#C880FF"),) +
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))


# plot 2 (MSK-prefered)
ggplot(data=ERrtL.E, aes(x=eyerep, y=RT, fill=eyerep)) +
  geom_bar(data = ERrtG.E, stat = "identity", colour = "black", size = 0.3, position = position_dodge(1))+
  geom_errorbar(data=ERrtG.E,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)") +
  coord_cartesian(ylim = c(1000, 1500), clip = "on") +
  theme_bw(base_size = 15) +
  geom_segment(aes(x=1, y=1422, xend=2, yend=1422), size = 0.2) +
  annotate("text", x=1.5, y=1426, label="***")+
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#E85E4D", "#C880FF"),) +
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))


# ----------------------------------------------------------#
# 3.3. Reaction Time Data Analysis
# ----------------------------------------------------------#

# -------------------
# 3.3.1. ANOVA

# using individual's mean

# two-way ANOVA (eyerep*locrep)
ER.rt.aov <- aov_ez(id = "SID", dv = "RT", data = tER, 
                    within = c("eyerep","locrep")) # add [between = "whatever"] for btw factor
anova(ER.rt.aov, es = "pes") %>% kable(digits = 4)
summary(ER.rt.aov)

# post-hoc: one-way ANOVA (eyerep)
ER.rt.aov.ER <- aov_ez(id = "SID", dv = "RT", data = tER, 
                       within = c("eyerep"))
anova(ER.rt.aov.ER, es = "pes") %>% kable(digits = 4)
summary(ER.rt.aov.ER)

# post-hoc: one-way ANOVA (locrep)
ER.rt.aov.LR <- aov_ez(id = "SID", dv = "RT", data = tER, 
                       within = c("locrep"))
anova(ER.rt.aov.LR, es = "pes") %>% kable(digits = 4)
summary(ER.rt.aov.LR)

# -------------------
# 2.3.2. Linear Mixed Modeling 

rt.lmer <- lmer(RT ~ eyerep*locrep + (1|SID) + (1|tarimg), tER)
anova(rt.lmer)
summary(rt.lmer)

# LRT(likelihood ratio test) on random effects
library(lmerTest)
rand(rt.lmer)


# -------------------
# 2.3.3. Generalized Linear Mixed Modeling 

rt.glmer <- glmer(RT ~ eyerep*locrep + (1|SID) + (1|tarimg), data=tER, family = inverse.gaussian(link="identity"))
anova(rt.glmer)
summary(rt.glmer)

rt.glmer.ER <- glmer(RT ~ eyerep + (eyerep|SID) + (eyerep|tarimg), data=tER, family = inverse.gaussian(link="identity"))
anova(rt.glmer.ER)
summary(rt.glmer.ER)


# Chi square test
library(car)
Anova(rt.glmer, test.statistic="Chisq")

# visualization
library(sjPlot)
plot_model(rt.glmer, type = c("int"), mdrt.values="all")


# -------------------
# 2.3.4. Test Full Models (WARNING: sometimes messed up)
# !! SAVE CODE & WORKSPACE HERE !!

# see structure
str(tER)

# if required
# remove NULL factor level in dataframe
# tER$eyerep = factor(tER$eyerep, levels=c("Eye_Rep","Eye_Unrep"), labels=c("Eye_Rep","Eye_Unrep"))
# tER$locrep = factor(tER$locrep, levels=c("Loc_Rep", "Loc_Unrep"), labels=c("Loc_Rep","Loc_Unrep"))
# double to integer/factor
# ER$RT <- as.integer(ER$RT)

# easy version
rt.full <- afex::mixed(RT ~ eyerep*locrep + (1|SID) + (1|tarimg), all_fit=TRUE, tER, method = "LRT",
                       family=inverse.gaussian(link="identity"))
anova(rt.full)
summary(rt.full)


# full version
pacman::p_load(parallel)

(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))
rt.full <- afex::mixed(RT ~ eyerep*locrep + (1|SID) + (1|tarimg), all_fit=TRUE, tER, method = "LRT",
                       cl = cl,
                       family=inverse.gaussian(link="identity"),
                       control = glmerControl(optCtrl = list(maxfun = 1e6)))
stopCluster(cl)

# see result
anova(rt.full) 
print(rt.full)


# -------------------
# 2.3.5. Bayes Factor

library(BayesFactor)
bf.rt <- anovaBF(RT ~ eyerep*locrep, data=tER, whichRandom=c("SID","tarimg"))
summary(bf.rt)
plot(bf.rt)


# -----*------*-----*-----*-----*-----*-----*-----*-----*-----*-----#
# ----------------------------------------------------------------- #
#     !! analysis using brms package is going to be updated !!
#     !! power test for mixed model is going to be updated !!
# ----------------------------------------------------------------- #
# -----*------*-----*-----*-----*-----*-----*-----*-----*-----*-----#










# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# Appendix 1.
#
# ggplot Guide
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #

# ------------------------------------------------------------------
# 1.1. One-way Plot
# ------------------------------------------------------------------

# Getting started with ggplot!
# full-data base (for individual mean plotting)
# set aes with long format data: x(i.v), y(d.v), fill(i.v)
ggplot(data=ERrtL.E, aes(x=eyerep, y=RT, fill=eyerep)) +
  
  # ERrtL.E (long format)
  #   SID eyerep       RT    SD
  # <int> <fct>     <int> <dbl>
  # 1     1 Eye_Rep     995    NA
  # 2     1 Eye_Unrep   970    NA
  # 3     2 Eye_Rep     971    NA
  # 4     2 Eye_Unrep   997    NA
  

# bar graph (you can use geom_violin or geom_line)
# set data with grand mean data
# to remove boundary line, delete colour/size (or adjust size)
geom_bar(data = ERrtG.E, stat = "identity", colour = "black", size = 0.3, position = position_dodge(1))+
  
  # ERrtG.E (grand mean data with ci)
  #   eyerep        M    SD    ci    RT
  # <fct>     <dbl> <dbl> <dbl> <dbl>
  # 1 Eye_Rep   1256.  286.  33.1 1256.
  # 2 Eye_Unrep 1340.  325.  33.1 1340.
  
  
# = NOT NECESSARY =
# individual mean point (from long format data)
# set color and size
geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  
  
# = NOT NECESSARY =
# connect individual mean for each condition using segment
# set data with wide format data
# length of aes data should be same with length of segment data
# be careful with name of dataframe! see below 
  geom_segment(data=ERrtW.E, inherit.aes = FALSE,
               aes(x=1, y=ERrtW.E$Eye_Rep,
                   xend=2, yend=ERrtW.E$Eye_Unrep),
               color="gray80") +
  
  # ERrtW.E (wide format)
  #   SID    SD Eye_Rep Eye_Unrep
  # <int> <dbl>   <int>     <int>
  # 1     1    NA     995       970
  # 2     2    NA     971       997
  # 3     3    NA    1109      1174
  # 4     4    NA    1065      1040
  # 5     5    NA    1183      1221
  # 6     6    NA    1962      2071
  
# error bar using grand mean & ci/se (see above to calculate ci)
# you can plot se by replacing ci with se (if there is se in grandmean data)
# set data & aes with ERrtG data: x(factor 1), y with ci/se(d.v), group(factor 2)
# remove upper/lower line with width = 0
geom_errorbar(data=ERrtG.E,
              aes(x = eyerep, ymin = RT-ci, ymax = RT+ci),
              position = position_dodge(1), width = 0, color = "black", size = 0.3, show.legend = FALSE) +
  
  
# label x, y (i.v., d.v.) 
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)") +
  
# gragh range: ylim = c(lower limit, upper limit)
  coord_cartesian(ylim = c(800, 1800), clip = "on") +
  
# remove background and adjust size
  theme_bw(base_size = 15) +
  
# = NOT NECESSARY =
# add aterisk
# change x, y, or label (*, **, n.s.)
# modify thickness with size
# solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  geom_segment(aes(x=1, y=1422, xend=2, yend=1422), size = 0.2) +
  annotate("text", x=1.5, y=1426, label="***")+
#  annotate("text", x=1.5, y=1426, label="italic(n.s.)", parse="TRUE")+ # TRUE parse for italic
  
# legend & color setting
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#E85E4D", "#C880FF"),) +
  
# grid & axis setting
  theme(legend.position="none",
        panel.grid.major = element_blank(), # remove grid (which is default in gglot)
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), # remove panel border line
        axis.ticks.x = element_blank()) + # remove axis ticks for x
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),# add light-gray y grid (major: in this case, 200 ms)
        panel.grid.minor.y = element_line(color="#DBDBDB"),# (minor: 100 ms)
        axis.line = element_line(size = 0.5)) # left/bottom black axis line

# ------------------------------------------------------------------
# 1.2. Two-way Plot (2 X 2)
# ------------------------------------------------------------------

# Getting started with ggplot!
# full-data base (for individual mean plotting)
# set aes with long format data: x(factor 1), y(d.v), fill(factor 2)
ggplot(data=ERrtL, aes(x=eyerep, y=RT, fill=locrep)) +
  
  # ERrtL (long format)
  # SID eyerep    locrep       RT
  # <int> <fct>     <fct>     <dbl>
  # 1     1 Eye_Rep   Loc_Rep    967.
  # 2     1 Eye_Rep   Loc_Unrep 1024.
  # 3     1 Eye_Unrep Loc_Rep    921.
  # 4     1 Eye_Unrep Loc_Unrep 1020.
  # 5     2 Eye_Rep   Loc_Rep    930.
  # 6     2 Eye_Rep   Loc_Unrep 1013.
  # 7     2 Eye_Unrep Loc_Rep   1003.


# bar graph (you can use geom_violin or geom_line)
# set data with grand mean data
# to remove boundary line, delete colour/size (or adjust size)
geom_bar(data = ERrtG, stat = "identity", position = position_dodge(1), colour = "black", size = 0.3)+
  
  # ERrtG (grand mean data with ci)
  #   eyerep    locrep        M    SD    ci    RT
  # <fct>     <fct>     <dbl> <dbl> <dbl> <dbl>
  # 1 Eye_Rep   Loc_Rep   1174.  227.  42.9 1174.
  # 2 Eye_Rep   Loc_Unrep 1338.  319.  48.0 1338.
  # 3 Eye_Unrep Loc_Rep   1269.  268.  59.5 1269.
  # 4 Eye_Unrep Loc_Unrep 1412.  364.  53.9 1412.
  
  
# = NOT NECESSARY =
# individual mean point (from long format data)
# set color and size
geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  
  
# = NOT NECESSARY =
# connect individual mean for each condition using segment
# set data with wide format data
# length of aes data should be same with length of segment data
# be careful with name of dataframe! see below
  geom_segment(data=filter(ERrtW, eyerep == "Eye_Rep"), inherit.aes = FALSE, # when factor 1 = 1
               aes(x=1-.12, y=filter(ERrtW, eyerep == "Eye_Rep")$Loc_Rep, # factor 1 = 1, factor 2 = 1 on left (left panel)
                   xend=1+.12, yend=filter(ERrtW, eyerep == "Eye_Rep")$Loc_Unrep), # factor 1 = 1, factor 2 = 2 on right (left panel)
               color="gray80") +
  geom_segment(data=filter(ERrtW, eyerep == "Eye_Unrep"), inherit.aes = FALSE, # when factor 1 = 2
               aes(x=2-.12, y=filter(ERrtW, eyerep == "Eye_Unrep")$Loc_Rep, # factor 1 = 2, factor 2 = 1 on left (right panel)
                   xend=2+.12, yend=filter(ERrtW, eyerep == "Eye_Unrep")$Loc_Unrep), # factor 1 = 2, factor 2 = 2 on right (right panel)
               color="gray80") +
  
  
  # ERrtW (wide format)
  #   SID eyerep    Loc_Rep Loc_Unrep
  # <int> <fct>       <dbl>     <dbl>
  # 1     1 Eye_Rep      967.     1024.
  # 2     1 Eye_Unrep    921.     1020.
  # 3     2 Eye_Rep      930.     1013.
  # 4     2 Eye_Unrep   1003.      992.
  # 5     3 Eye_Rep     1020.     1199.
  

# error bar using grand mean & ci/se (see above to calculate ci)
# you can plot se by replacing ci with se (if there is se in grandmean data)
# set data & aes with ERrtG data: x(factor 1), y with ci/se(d.v), group(factor 2)
# remove upper/lower line with width = 0
geom_errorbar(data=ERrtG,
              aes(x = eyerep, ymin = RT-ci, ymax = RT+ci, group = locrep),
              position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  
  
# label x, y, fill (factor 1, d.v., factor 2)  
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)", 
       fill='Location Repetition') +
  
# gragh range: ylim = c(lower limit, upper limit)
  coord_cartesian(ylim = c(1000, 1600), clip = "on") +
  
# remove background and adjust size
  theme_bw(base_size = 15) + 
  

# = NOT NECESSARY =
# add aterisk
# change x, y, or label (*, **, n.s.)
# solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  
  geom_segment(aes(x=0.7, y=1421, xend=1.3, yend=1421), size = 0.3) +
  annotate("text", x=1, y=1426, label="***")+
  
  geom_segment(aes(x=1.7, y=1480, xend=2.3, yend=1480), size = 0.3) +
  annotate("text", x=2, y=1485, label="***")+
  
  geom_segment(aes(x=1, y=1546, xend=2, yend=1546), size = 0.3) +
  annotate("text", x=1.5, y=1560, label="italic(n.s.)", parse="TRUE")+ # TRUE parse for italic
  
  
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  
  
# legend for factor 2 (and color setting)
  scale_fill_manual(values=c("#E85E4D", "#C880FF"),
                    labels=c("Repeated", "Unrepeated")) +
  
# grid & axis
  theme(panel.grid.major = element_blank(), # remove grid (which is default in gglot)
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), # remove panel border line
        axis.ticks.x = element_blank()) + # remove axis ticks for x
  
  theme(panel.grid.major.y = element_line(color="#DBDBDB"), # add light-gray y grid (major: in this case, 200 ms)
        panel.grid.minor.y = element_line(color="#DBDBDB"), # (minor: 100 ms)
        axis.line = element_line(size = 0.5)) # left/bottom black axis line