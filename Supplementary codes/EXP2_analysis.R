
library(lme4)
library(lmerTest)
library(emmeans)


#### 1 description  ######
data1b <- read.csv(file.choose())

library(dplyr)

compute_summary_stats <- function(data, variable) {
  data %>%
    group_by(Induction,participant) %>%
    summarize(N = n(),
              Mean = mean({{ variable }}),
              SD = sd({{ variable }}),
              SE = SD / sqrt(N),
              CoV = (SD / Mean) * 100) %>%
    ungroup()
}


summary_stats <- data1b %>%
  compute_summary_stats(originality)
print(summary_stats)

summary_stats <- data1b %>%
  compute_summary_stats(appropriate)
print(summary_stats)

summary_stats <- data1b %>%
  compute_summary_stats(vivid)
print(summary_stats)

summary_stats <- data1b %>%
  compute_summary_stats(semantic_distance)
print(summary_stats)


summary_stats <- data1b %>%
  compute_summary_stats(DES)
print(summary_stats)



# 2 LMM ##########

## 2.1 subjective ratings #############

quality1.model <- lmer(originality ~ Induction  + (1 | noun1) + (1 | participant), data = data1b, REML = TRUE)
summary(quality1.model)

quality2.model <- lmer(appropriate ~ Induction  + (1 | noun1) + (1 | participant), data = data1b, REML = TRUE)
summary(quality2.model)
confint(object = quality2.model)

quality3.model <- lmer(vivid ~ Induction  + (1 | noun1) + (1 | participant), data = data1b, REML = TRUE)
summary(quality3.model)
confint(object = quality3.model)

## 2.2 semantic distance ########
quality4.model <- lmer(semantic_distance ~ Induction  + (1 | noun1) + (1 | participant), data = data1b, REML = TRUE)
summary(quality4.model)


## 2.3 DES ############
DES2.model <- lmer(DES ~ Induction  + (1 | noun1) + (1 | participant), data = data1b, REML = TRUE)
summary(DES2.model)


## 2.4 forward flow #############
taskff1.model <- lmer(forwardflow ~ Induction+ (1 | noun1) + (1 | participant), data = data1b, REML = TRUE)
summary(taskff1.model)
confint(object = taskff1.model)


#3 semantic distance of verbs  #########
library(tidyverse)
library(lme4)     

semdisB <- read.csv(file.choose())


result_list <- list()


for (ord in unique(semdisB$order)) {

  data_order <- semdisB %>% filter(order == ord)
  

  model <- lmer(value ~ Induction + (1 | sentences) + (1 | participant),
                data = data_order, REML = TRUE)
  

  result_list[[as.character(ord)]] <- summary(model)
}

for (ord in unique(semdisB$order)) {
  cat("Order =", ord, ":\n")
  print(result_list[[as.character(ord)]])
  cat("\n")
}



