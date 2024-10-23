
library(lme4)
library(lmerTest)
library(emmeans)

data1a <- read.csv(file.choose())

#1 description #####
library(dplyr)

compute_summary_stats <- function(data, variable) {
  data %>%
    group_by(Induction, Type) %>%
    summarize(N = n(),
              Mean = mean({{ variable }}),
              SD = sd({{ variable }}),
              SE = SD / sqrt(N),
              CoV = (SD / Mean) * 100) %>%
    ungroup()
}


summary_stats <- data1a %>%
  compute_summary_stats(originality)
print(summary_stats)

summary_stats <- data1a %>%
  compute_summary_stats(appropriate)
print(summary_stats)

summary_stats <- data1a %>%
  compute_summary_stats(vivid)
print(summary_stats)

summary_stats <- data1a %>%
  compute_summary_stats(semantic_distance)
print(summary_stats)


summary_stats <- data1a %>%
  compute_summary_stats(decision_error_score)
print(summary_stats)


#2 LMM #####

##2.1 subjective ratings ####
poemcomposition1.model <- lmer(originality ~ Induction * Type  + (1 | sentences) + (1 | participant), data = data1a, REML = TRUE)
summary(poemcomposition1.model)

poemcomposition2.model <- lmer(appropriate ~ Induction * Type  + (1 | sentences) + (1 | participant), data = data1a, REML = TRUE)
summary(poemcomposition2.model)

poemcomposition3.model <- lmer(vivid ~ Induction * Type  + (1 | sentences) + (1 | participant), data = data1a, REML = TRUE)
summary(poemcomposition3.model)
confint(object = poemcomposition3.model)

interaction_emmeans <- emmeans(poemcomposition3.model , ~ Induction * Type)


simple_effects <- pairs(interaction_emmeans, simple = "each")


print(simple_effects)



##2.2 DES #####

DES1.model <- lmer(decision_error_score~  Induction * Type + (1 | sentences) + (1 | participant),
                         data = data1a,REML=TRUE)
summary(DES1.model)

confint(object = DES1.model)


interaction_emmeans <- emmeans(DES1.model , ~ Induction * Type)


simple_effects <- pairs(interaction_emmeans, simple = "each")


print(simple_effects)


##2.3 semantic distance ######

Eyesemdis1.model <- lmer(semantic_distance ~ Type * Induction  + (1 | sentences) + (1 | participant),
                         data = data1a,REML=TRUE)
summary(Eyesemdis1.model)

confint(object = Eyesemdis1.model)


interaction_emmeans <- emmeans(Eyesemdis1.model, ~ Induction * Type)

simple_effects <- pairs(interaction_emmeans, simple = "each")


print(simple_effects)

##2.4 foward flow #####

forwardflowA1.model <- lmer(forwardflow ~ Induction * Type  + (1 | sentences) + (1 | participant),
                           data = data1a,REML=TRUE)
summary(forwardflowA1.model)

confint(object = forwardflowA1.model)


interaction_emmeans <- emmeans(forwardflowA1.model, ~ Induction * Type)


simple_effects <- pairs(interaction_emmeans, simple = "each")


print(simple_effects)

#3  semantic distance of verbs  #########

semdisA <- read.csv(file.choose())

library(lme4)
library(lmerTest)
library(emmeans)


#  order refers to the verb position; value means the semantic distance of each verb
semdisA2.model <- lmer(value ~ induction * type * order + (1 | sentences) + (1 | participant),
                       data = semdisA, REML = TRUE)
summary(semdisA2.model)
confint(object = semdisA2.model)

## LMM for each verb ########
library(lme4)

model_summaries <- list()


for (order_level in unique(semdisA$order)) {
  cat("Analyzing Order Level:", order_level, "\n")
  

  subset_data <- subset(semdisA, order == order_level)
  

  model <- lmer(value ~ induction * type + (1 | sentences) + (1 | participant),
                data = subset_data, REML = TRUE)
  

  model_summaries[[as.character(order_level)]] <- summary(model)
}


for (order_level in names(model_summaries)) {
  cat("Analysis for Order Level:", order_level, "\n")
  print(model_summaries[[order_level]])
  cat("\n")
}




