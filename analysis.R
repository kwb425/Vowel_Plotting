###################################################################################################
# Vowel Plotting, data from EMCS
# 
#                                                                           Written by Kim, Wiback,
#                                                                             2016.06.25. Ver. 1.1.
#                                                                             2016.06.30. Ver. 1.2.
###################################################################################################





## Pre-processing #################################################################################

### Clearing 
rm(list = ls())



######
# Path
######
setwd("/Users/KimWiback/Google_Drive/Git/Vowel_Plotting/R_&_Praat")



#####################
# Attaching libraries 
#####################
library(ggplot2)
library(vowels)
library(qcc)
library(lme4)
library(lsmeans)
library(vowels)
require(grid)
source("multiplot.R")



##############
# Loading data
##############
data = read.table("data.txt", sep = ",", header = T)



###################
# Data organization
###################

### Cutting
data[ , c("F3_ch1","F3_ch2","F3_ch3","F3_ch4","F3_ch5", # Removing F3
       "F4_ch1","F4_ch2","F4_ch3","F4_ch4","F4_ch5", # Removing F3
       "Pitch_ch1","Pitch_ch2","Pitch_ch3","Pitch_ch4","Pitch_ch5")] = # Removing pitch
  list(NULL)  

### Creating
# Creating Gender
data$Gender = NA 
# Extracting the gender from subject/number/gender_word/number format (e.g., s01f_hot001)
for (each_subject in 1:nrow(data)) {
  gender = substr(data$Filename[each_subject], 4, 4) # Extracting the gender
  data$Gender[each_subject] = gender
}
# Creating mean F1, F2 values
data$F1 = NA
data$F2 = NA
# Averaging over each subject
for (each_subject in 1:nrow(data))
{
  data$F1[each_subject] = (data$F1_ch1[each_subject] + data$F1_ch2[each_subject] + 
                             data$F1_ch3[each_subject] + data$F1_ch4[each_subject] + 
                             data$F1_ch5[each_subject]) / 5
  data$F2[each_subject] = (data$F2_ch1[each_subject] + data$F2_ch2[each_subject] + 
                             data$F2_ch3[each_subject] + data$F2_ch4[each_subject] + 
                             data$F2_ch5[each_subject]) / 5
}

### Replacing
# Phone2word
levels(data$Phoneme) = 
  c("hot", "had", "head", "hey", "hid", "heed", "hope", "hood", "food")
colnames(data)[3] = "Word"

### Factorizing
data$Gender = as.factor(data$Gender)
data$Word = as.factor(data$Word)





## Plot Analysis ##################################################################################



##############
# Pre-settings
##############

### Theme 
default.theme = theme(text = element_text(size = 20),
                      title = element_text(size = 20),
                      axis.text = element_text(colour = "black", size = 20),
                      axis.title.x = element_text(vjust = -0.45),
                      axis.title.y = element_text(vjust = .2),
                      axis.ticks = element_line(colour = "black"),
                      axis.line = element_line())



########################
# Plotting by the gender 
########################

### Data extraction for male plotting
male = data[data$Gender == "m", c("Filename", "Word", "Gender", "F1", "F2")]
# Computing mean
male_word_average = compute.means(male, separate = F, speaker = NA)
# Re-attaching the gender factor (lost in the mean calculation procedure)
male_word_average$gender = as.factor("m")

### Data extraction for female plotting
female = data[data$Gender == "f", c("Filename", "Word", "Gender", "F1", "F2")]
# Computing mean
female_word_average = compute.means(female, separate = F, speaker = NA)
# Re-attaching the gender factor (lost in the mean calculation procedure)
female_word_average$gender = as.factor("f")

### Data organization for both male and female
gender = rbind(male_word_average, female_word_average)
# Sorting by the words
gender = rbind(gender[gender$Vowel == "heed", ],
                gender[gender$Vowel == "hid", ],
                gender[gender$Vowel == "hey", ],
                gender[gender$Vowel == "head", ],
                gender[gender$Vowel == "had", ],
                gender[gender$Vowel == "hot", ],
                gender[gender$Vowel == "hope", ],
                gender[gender$Vowel == "food", ],
                gender[gender$Vowel == "hood", ],
                gender[gender$Vowel == "heed", ])

### Creating gg_objects (will be plot later.)
gg_object_1 = ggplot(gender, aes(x = F2, y = F1, label = Vowel)) + # Data and it's label
    geom_point(aes(shape = gender, color = gender), size = 5) + # Points by the gender
    geom_text(aes(color = gender), hjust = 1.5, vjust = -0.1, size = 7) + # Texts by the gender
    scale_y_reverse(lim = c(1000, 300)) + # F1 scaling
    scale_x_reverse(lim = c(3200, 500)) + # F2 scaling
    geom_path(aes(linetype = gender, color = gender), size = 1) + # Connecting the points
    ggtitle("Gender Comparision") + # Title
    theme_bw() + # Background coloring
    default.theme



##################################
# Plotting by the time: seperately
##################################

### Data extraction for male plotting
male_beg = data[data$Gender == "m", c("Filename", "Word", "Gender", "F1_ch1", "F2_ch1")]
male_mid = data[data$Gender == "m", c("Filename", "Word", "Gender", "F1_ch3", "F2_ch3")]
male_end = data[data$Gender == "m", c("Filename", "Word", "Gender", "F1_ch5", "F2_ch5")]
# Computing mean
male_beg_average = compute.means(male_beg, separate = F,speaker = NA)
# Attaching the time factor
male_beg_average$time = as.factor("beg")
# Matching colnames for later binding
colnames(male_beg_average)[4:5] = c("F1", "F2")
# Computing mean
male_mid_average = compute.means(male_mid, separate = F,speaker = NA)
# Attaching the time factor
male_mid_average$time = as.factor("mid")
# Matching colnames for later binding
colnames(male_mid_average)[4:5] = c("F1", "F2")
# Computing mean
male_end_average = compute.means(male_end, separate = F,speaker = NA)
# Attaching the time factor
male_end_average$time = as.factor("end")
# Matching colnames for later binding
colnames(male_end_average)[4:5] = c("F1", "F2")

### Data organization for the male
male_location = rbind(male_beg_average, male_mid_average, male_end_average)
# Sorting by the words
male_location = rbind(male_location[male_location$Vowel == "heed", ],
               male_location[male_location$Vowel == "hid", ],
               male_location[male_location$Vowel == "hey", ],
               male_location[male_location$Vowel == "head", ],
               male_location[male_location$Vowel == "had", ],
               male_location[male_location$Vowel == "hot", ],
               male_location[male_location$Vowel == "hope", ],
               male_location[male_location$Vowel == "food", ],
               male_location[male_location$Vowel == "hood", ],
               male_location[male_location$Vowel == "heed", ])

### Creating gg_objects (will be plot later.)
gg_object_2 = ggplot(male_location, aes(x = F2, y = F1, label = Vowel)) + # Data and it's label
  geom_point(aes(shape = time, color = time), size = 5) + # Points by the time
  geom_text(aes(color = time), hjust = 1.5, vjust = -0.1, size = 7) + # Texts by the time
  scale_y_reverse(lim = c(1000, 300)) + # F1 scaling
  scale_x_reverse(lim = c(3200, 300)) + # F2 scaling
  geom_path(aes(linetype = time, color = time), size = 1) + # Connecting the points
  ggtitle("Time-wise Change for Male") + # Title
  theme_bw() + # Background coloring
  default.theme

### Data extraction for female plotting
female_beg = data[data$Gender == "f", c("Filename", "Word", "Gender", "F1_ch1", "F2_ch1")]
female_mid = data[data$Gender == "f", c("Filename", "Word", "Gender", "F1_ch3", "F2_ch3")]
female_end = data[data$Gender == "f", c("Filename", "Word", "Gender", "F1_ch5", "F2_ch5")]
# Computing mean
female_beg_average = compute.means(female_beg, separate = F,speaker = NA)
# Attaching the time factor
female_beg_average$time = as.factor("beg")
# Matching colnames for later binding
colnames(female_beg_average)[4:5] = c("F1", "F2")
# Computing mean
female_mid_average = compute.means(female_mid, separate = F,speaker = NA)
# Attaching the time factor
female_mid_average$time = as.factor("mid")
# Matching colnames for later binding
colnames(female_mid_average)[4:5] = c("F1", "F2")
# Computing mean
female_end_average = compute.means(female_end, separate = F,speaker = NA)
# Attaching the time factor
female_end_average$time = as.factor("end")
# Matching colnames for later binding
colnames(female_end_average)[4:5] = c("F1", "F2")

### Data organization for the female
female_location = rbind(female_beg_average, female_mid_average, female_end_average)
# Sorting by the words
female_location = rbind(female_location[female_location$Vowel == "heed", ],
                      female_location[female_location$Vowel == "hid", ],
                      female_location[female_location$Vowel == "hey", ],
                      female_location[female_location$Vowel == "head", ],
                      female_location[female_location$Vowel == "had", ],
                      female_location[female_location$Vowel == "hot", ],
                      female_location[female_location$Vowel == "hope", ],
                      female_location[female_location$Vowel == "food", ],
                      female_location[female_location$Vowel == "hood", ],
                      female_location[female_location$Vowel == "heed", ])

### Creating gg_objects (will be plot later.)
gg_object_3 = ggplot(female_location, aes(x = F2, y = F1, label = Vowel)) + # Data and it's label
  geom_point(aes(shape = time, color = time), size = 5) + # Points by the time
  geom_text(aes(color = time), hjust = 1.5, vjust = -0.1, size = 7) + # Texts by the time
  scale_y_reverse(lim = c(1000, 300)) + # F1 scaling
  scale_x_reverse(lim = c(3200, 300)) + # F2 scaling
  geom_path(aes(linetype = time, color = time), size = 1) + # Connecting the points
  ggtitle("Time-wise Change for Female") + # Title
  theme_bw() + # Background coloring
  default.theme



##################################
# Plotting by the time: transition
##################################

### Data extraction for male plotting
male_transition = data[data$Gender == "m", 
                       c("Filename","Word","Gender", "F1_ch1", "F2_ch1","F1_ch5","F2_ch5")]
# Computing mean
male_transition = compute.means(male_transition, separate = F, speaker = NA)
# Matching colnames for later binding
colnames(male_transition)[4:5] = c("F1", "F2")

### Data extraction for female plotting
female_transition = data[data$Gender == "f", 
                         c("Filename","Word","Gender", "F1_ch1", "F2_ch1","F1_ch5","F2_ch5")]
# Computing mean
female_transition = compute.means(female_transition, separate = F, speaker = NA)
# Matching colnames for later binding
colnames(female_transition)[4:5] = c("F1", "F2")

### Plotting (this is not a gg_oject and will be plot right away.)
par(mfrow = c(1, 2))
# Vowelplot for the male
vowelplot(male_transition, color = "vowels", labels = "vowels", # Coloring & Labelling by vowels
          color.choice = rainbow(length(unique(male_transition[, 2]))), # More beautiful colors
          title = "Male Transition, ch1 -> ch5", leg = NA) # No legend
# Vowelplot for the female
vowelplot(female_transition, color = "vowels", labels = "vowels", # Coloring & Labelling by vowels
          color.choice = rainbow(length(unique(female_transition[, 2]))), # More beautiful colors
          title = "Female Transition, ch1 -> ch5", leg = NA) # No legend
par(mfrow = c(1, 1))



########################
# Plotting by each vowel
########################
# Dummies
data$zF1 = 0
data$zF2 = 0
male_hot = data$Word == "hot" & data$Gender == "m"
male_hope = data$Word == "hope" & data$Gender == "m"
male_hood = data$Word == "hood" & data$Gender == "m"
male_hid = data$Word == "hid" & data$Gender == "m"
male_hey = data$Word =="hey" & data$Gender == "m"
male_heed = data$Word =="heed" & data$Gender == "m"
male_head = data$Word =="head" & data$Gender == "m"
male_had = data$Word =="had" & data$Gender == "m"
male_food = data$Word =="food" & data$Gender == "m"
female_hot = data$Word == "hot" & data$Gender == "f"
female_hope = data$Word == "hope" & data$Gender == "f"
female_hood = data$Word == "hood" & data$Gender == "f"
female_hid = data$Word == "hid" & data$Gender == "f"
female_hey = data$Word =="hey" & data$Gender == "f"
female_heed = data$Word =="heed" & data$Gender == "f"
female_head = data$Word =="head" & data$Gender == "f"
female_had = data$Word =="had" & data$Gender == "f"
female_food = data$Word =="food" & data$Gender == "f"

### Data extraction & organization for male plotting
data$zF1[male_hot] = (data$F1[male_hot] - mean(data$F1[male_hot])) / sd(data$F1[male_hot])
data$zF1[male_hope] = (data$F1[male_hope] - mean(data$F1[male_hope])) / sd(data$F1[male_hope])
data$zF1[male_hood] = (data$F1[male_hood] - mean(data$F1[male_hood])) / sd(data$F1[male_hood])
data$zF1[male_hid] = (data$F1[male_hid] - mean(data$F1[male_hid])) / sd(data$F1[male_hid])
data$zF1[male_hey] = (data$F1[male_hey] - mean(data$F1[male_hey])) / sd(data$F1[male_hey])
data$zF1[male_heed] = (data$F1[male_heed] - mean(data$F1[male_heed])) / sd(data$F1[male_heed])
data$zF1[male_head] = (data$F1[male_head] - mean(data$F1[male_head])) / sd(data$F1[male_head])
data$zF1[male_had] = (data$F1[male_had] - mean(data$F1[male_had])) / sd(data$F1[male_had])
data$zF1[male_food] = (data$F1[male_food] - mean(data$F1[male_food])) / sd(data$F1[male_food])
data$zF2[male_hot] = (data$F2[male_hot] - mean(data$F2[male_hot])) / sd(data$F2[male_hot])
data$zF2[male_hope] = (data$F2[male_hope] - mean(data$F2[male_hope])) / sd(data$F2[male_hope])
data$zF2[male_hood] = (data$F2[male_hood] - mean(data$F2[male_hood])) / sd(data$F2[male_hood])
data$zF2[male_hid] = (data$F2[male_hid] - mean(data$F2[male_hid])) / sd(data$F2[male_hid])
data$zF2[male_hey] = (data$F2[male_hey] - mean(data$F2[male_hey])) / sd(data$F2[male_hey])
data$zF2[male_heed] = (data$F2[male_heed] - mean(data$F2[male_heed])) / sd(data$F2[male_heed])
data$zF2[male_head] = (data$F2[male_head] - mean(data$F2[male_head])) / sd(data$F2[male_head])
data$zF2[male_had] = (data$F2[male_had] - mean(data$F2[male_had])) / sd(data$F2[male_had])
data$zF2[male_food] = (data$F2[male_food] - mean(data$F2[male_food])) / sd(data$F2[male_food])

### Data extraction & organization for female plotting
data$zF1[female_hot] = (data$F1[female_hot] - mean(data$F1[female_hot])) / sd(data$F1[female_hot])
data$zF1[female_hope] = (data$F1[female_hope] - mean(data$F1[female_hope])) / sd(data$F1[female_hope])
data$zF1[female_hood] = (data$F1[female_hood] - mean(data$F1[female_hood])) / sd(data$F1[female_hood])
data$zF1[female_hid] = (data$F1[female_hid] - mean(data$F1[female_hid])) / sd(data$F1[female_hid])
data$zF1[female_hey] = (data$F1[female_hey] - mean(data$F1[female_hey])) / sd(data$F1[female_hey])
data$zF1[female_heed] = (data$F1[female_heed] - mean(data$F1[female_heed])) / sd(data$F1[female_heed])
data$zF1[female_head] = (data$F1[female_head] - mean(data$F1[female_head])) / sd(data$F1[female_head])
data$zF1[female_had] = (data$F1[female_had] - mean(data$F1[female_had])) / sd(data$F1[female_had])
data$zF1[female_food] = (data$F1[female_food] - mean(data$F1[female_food])) / sd(data$F1[female_food])
data$zF2[female_hot] = (data$F2[female_hot] - mean(data$F2[female_hot])) / sd(data$F2[female_hot])
data$zF2[female_hope] = (data$F2[female_hope] - mean(data$F2[female_hope])) / sd(data$F2[female_hope])
data$zF2[female_hood] = (data$F2[female_hood] - mean(data$F2[female_hood])) / sd(data$F2[female_hood])
data$zF2[female_hid] = (data$F2[female_hid] - mean(data$F2[female_hid])) / sd(data$F2[female_hid])
data$zF2[female_hey] = (data$F2[female_hey] - mean(data$F2[female_hey])) / sd(data$F2[female_hey])
data$zF2[female_heed] = (data$F2[female_heed] - mean(data$F2[female_heed])) / sd(data$F2[female_heed])
data$zF2[female_head] = (data$F2[female_head] - mean(data$F2[female_head])) / sd(data$F2[female_head])
data$zF2[female_had] = (data$F2[female_had] - mean(data$F2[female_had])) / sd(data$F2[female_had])
data$zF2[female_food] = (data$F2[female_food] - mean(data$F2[female_food])) / sd(data$F2[female_food])

### Creating gg_objects (will be plot later.)
data = droplevels(data)
gg_object_4 = 
  ggplot(data, aes(x = zF2, y = zF1, color = Gender, label = Word)) + # Data and it's label & color
  geom_density2d(aes(label = Word), bins = 5, size = 1) + # Bins of contours and their size
  scale_y_reverse() + # No need (standardized)
  scale_x_reverse() + # No need (standardized)
  ylim(-2, 2) + xlim(-2, 2) + 
  facet_wrap(~ Gender + Word, ncol = 9) + # Dividing the data into 9 columns with 2 independent vars
  ggtitle("Each Vowels") +
  theme_bw() + # Background coloring
  default.theme



#####################
# Plotting gg_objects
#####################

### When handled, plotting will be suppressed and has to be resume later like this.
Sys.sleep(3)
plot(gg_object_1)
Sys.sleep(3)
multiplot(gg_object_2, gg_object_2, cols = 2)
Sys.sleep(3)
plot(gg_object_4)

### When not handled, plotting will be instant, that is, right after you give ggplot() command.