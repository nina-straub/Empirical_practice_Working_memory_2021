############################## Data analysis ###################################

### Before start of analysis: Download needed packages from library ###

library(tidyverse)
library(tidyr)
library(dplyr)
library(afex)
library(forcats)
library(psych)
library(lsr)

################################################################################
### Read in demographic data and data of the experiment ###

Demo <- read.csv2("demogr_raw.csv", stringsAsFactors = TRUE) # Demographic data
Data <- read.csv2("simdat_raw.csv", stringsAsFactors = TRUE) # Experimental data

################################################################################
### Analysing demographic data ###

# Searching for subjects under 18 and above 35 years
which(Demo$age>35)
which(Demo$age<18)

# Subjects S26 and S33 are above 35 years, need to be excluded from data frames:
Data[Data$SubjectID=="S26",]
Data[Data$SubjectID=="S33",]
Data <- subset(Data, SubjectID!= "S26")
Data <- subset(Data, SubjectID!="S33")
Demo <- subset(Demo, SubjectID!= "S26")
Demo <- subset(Demo, SubjectID!="S33")

# Descriptive analysis of demographic data:
Demo_describe <- summary(Demo)
Demo_describe
sd(Demo$age)

################################################################################
### Checking K-values for outliers ###

boxplot(Data$K)

# One outlier was found_ Subject 16, looking closer at K-values of Subject 16
# Inverse K-values of subject 16 as the negative WM capacity in all conditions suggests a confusion of left/right mouse button click during the experiment
Data$K[Data$SubjectID=="S16"] <- Data$K[Data$SubjectID=="S16"]*(-1)

################################################################################
### For inferential statistics the baseline corrected K-values are needed ###
### In the following the baseline corrected K-values will be calculated and added to the data frame ###

# Combining factors Stimulation and Cue
Data$Stimulation_Cue <- fct_cross(Data$Stimulation, Data$Cue,sep="_")

# Correcting K-values for each condition
correct_4_l <-  data.frame(Data[Data$Stimulation_Cue == "tms4_left",],
                           k_correct = Data[Data$Stimulation_Cue == "tms4_left",4]-Data[Data$Stimulation_Cue == "tmsC_left",4])
correct_7_l <-  data.frame(Data[Data$Stimulation_Cue == "tms7_left",],
                           k_correct = Data[Data$Stimulation_Cue == "tms7_left",4]-Data[Data$Stimulation_Cue == "tmsC_left",4])
correct_4_r <-  data.frame(Data[Data$Stimulation_Cue == "tms4_right",],
                           k_correct = Data[Data$Stimulation_Cue == "tms4_right",4]-Data[Data$Stimulation_Cue == "tmsC_right",4])
correct_7_r <-  data.frame(Data[Data$Stimulation_Cue == "tms7_right",],
                           k_correct = Data[Data$Stimulation_Cue == "tms7_right",4]-Data[Data$Stimulation_Cue == "tmsC_right",4])


# Binding the data frames for each condition back together into one data frame including baseline corrected K-values
Data_bc <- rbind(correct_4_l,correct_4_r,correct_7_l,correct_7_r)

# Ordering the data frame by SubjectID
Data_bc <- Data_bc[order(Data_bc$SubjectID),]

# Drop levels that are not needed anymore (tmsC_right and tmsC_left)
Data_bc$Stimulation_Cue <-  droplevels(Data_bc$Stimulation_Cue)
Data_bc$Stimulation <- droplevels(Data_bc$Stimulation)
Data_bc$Cue <- droplevels(Data_bc$Cue)


################################################################################
### Inspect the assumptions of inferential statistics ###

# Checking the assumption of normal distribution
hist(Data_bc[Data_bc$Stimulation_Cue=="tms4_left",]$k_correct)
hist(Data_bc[Data_bc$Stimulation_Cue=="tms7_left",]$k_correct)
hist(Data_bc[Data_bc$Stimulation_Cue=="tms4_right",]$k_correct)
hist(Data_bc[Data_bc$Stimulation_Cue=="tms7_right",]$k_correct)

# Checking for outliers
boxplot(Data_bc[Data_bc$Stimulation_Cue=="tms4_left",]$k_correct)
boxplot(Data_bc[Data_bc$Stimulation_Cue=="tms4_left",]$k_correct)

# Descriptive statistics for the baseline corrected K-values
Data_describe <- describeBy(Data_bc$k_correct, group = Data_bc$Stimulation_Cue, digits = 3, mat = TRUE, range = FALSE)
Data_describe
mean(Data_bc$k_correct)

################################################################################
### Repeated measures ANOVA ###
### Hypothesis 1: There is an interaction between the factors Cue and Stimulation ###

# Repeated measures ANOVA
fit <- aov_ez(id = "SubjectID",
              dv = c("k_correct"),
              data = Data_bc,
              within = c("Stimulation", "Cue"))

# Summary of results
summary(fit)

# Interaction plot for the conditions Cue and Stimulation
interaction.plot(Data_bc$Stimulation, Data_bc$Cue, Data_bc$k_correct,
                 trace.label = "Cue",
                 xlab = "Stimulation",
                 ylab = "Baseline-corrected K-values",
                 ylim = c(-0.5, 0.5),
                 lty = c(1, 5))
abline(h = 0, col = "grey")

# Effect sizes for repeated measures ANOVA (generalized and partial eta-squared)
nice(fit, correction = "none", es = c("ges", "pes"))

################################################################################
### Interaction between Cue and Stimulation proofed significant. Next step: direction of interaction is analysed ###
### Hypothesis 2: T-test show that tms4_left K-values > 0 and tms7_left K-values < 0 ###
### T-tests for the conditions tms4_right and tms7_right should not be significant ###

# T-tests for left hemifield
t.test(Data_bc[Data_bc$Stimulation_Cue=="tms4_left",6],
       alternative="greater",
       mu = 0)
t.test(Data_bc[Data_bc$Stimulation_Cue=="tms7_left",6],
       alternative = "less",
       mu = 0)

# T-tests for right hemifield
t.test(Data_bc[Data_bc$Stimulation_Cue=="tms4_right",6],
       alternative="greater",
       mu = 0)
t.test(Data_bc[Data_bc$Stimulation_Cue=="tms7_right",6],
       alternative = "less",
       mu = 0)

# Effect sizes of t-tests (Cohen's d)
cohensD(Data_bc[Data_bc$Stimulation_Cue=="tms4_left",6],
        mu = 0)
cohensD(Data_bc[Data_bc$Stimulation_Cue=="tms7_left",6],
        mu = 0)

cohensD(Data_bc[Data_bc$Stimulation_Cue=="tms4_right",6],
        mu = 0)
cohensD(Data_bc[Data_bc$Stimulation_Cue=="tms7_right",6],
        mu = 0)

################################################################################
### Create plots for data ###
library(ggplot2)

# Bar plot to show mean results of baseline corrected K-values
ggplot(Data_bc, aes(x = Stimulation, y = k_correct)) +                                                                       # basic plot function
  geom_bar(position = "dodge", stat = "summary", fun = "mean", width = 0.5, aes(fill = Stimulation), show.legend = FALSE) +  # bar plot for means, no legend
  geom_abline(slope = 0) +                                                                                                   # abline at y = 0
  facet_wrap(~Cue) +                                                                                                         # different plots for each Cue
  theme_classic(base_size = 14) +                                                                                            # set theme
  ggtitle("Mean results") +                                                                                                  # titel
  scale_x_discrete(name="Stimulation", labels=c("4Hz", "7Hz")) +                                                             # labels for x-axis
  scale_fill_manual(name = "Cue", labels = c("left", "right"), values = c("grey30", "grey70")) +                             # labels and colours for Cue
  scale_y_continuous(name="Baseline-corrected K-values") +                                                                   # label for y-axis
ggsave(filename = "Mean_results.png", width = 7, height = 7)


# Line and point plot for baseline corrected K-values of each subject
ggplot(Data_bc, aes(x = Stimulation, y = k_correct, group = SubjectID, color= SubjectID)) +     # basic plot function, group by SubjectsID
  geom_point(show.legend = FALSE) +                                                             # point plot
  geom_line(show.legend =FALSE) +                                                               # line plot
  facet_wrap(~Cue) +                                                                            # different plots for left/right Cue
  theme_classic(base_size = 14) +                                                               # set theme
  scale_color_grey() +                                                                          # grey colours for black/white compatibility
  geom_abline(slope = 0) +                                                                      # abline at y = 0
  ggtitle("Individual results") +                                                               # titel
  scale_x_discrete(name="Stimulation",                                                          # labels for x-axis
                   labels=c("4Hz", "7Hz")) +
  scale_y_continuous(name="Baseline-corrected K-values")   # label for y-axis
ggsave(filename = "Individual_results.png", width = 7, height = 7)


