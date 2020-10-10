# install.packages("tidyverse")
library(tidyverse)
library(lubridate)
data = read.csv("abtest.csv")
data

data %>%
  summarize(conv_rate = mean(conversion))


# 1) LOGISTIC REGGRESION
# 1.1) COMPUTE POWER SAMPLE SIZE FOR LOGISTIC REGRESION
install.packages("powerMediation")
library(powerMediation)
total_sample_size <- SSizeLogisticBin(p1 = 0.2, # conversion inicial
                                      p2 = 0.3, # conversion final = p1 + delta (1M /0.1%)
                                      B = 0.5, # muestra en cada experiencia
                                      alpha = 0.05,
                                      power = 0.8)

total_sample_size
total_sample_size/2


# 1.2) PERFORMING TEST
# Load package for cleaning model results
library(broom)

# Run logistic regression
experiment_results <- glm(conversion ~ experiment,
                          family = "binomial",
                          data = data) %>%
  tidy()
experiment_results

# 2) T-TEST LINEAR REGRESSION (TIME SPEND ON EACH GROUP)
# 1.1) COMPUTE POWER SAMPLE SIZE FOR LINEAR REGRESSION
library(pwr)
pwr.t.test(power=0.8,
           sig.level=0.05,
           d=0.1
           )
t.test(time_spend ~ experiment, 
       data=data)

lm(time_spend ~ experiment, data=data) %>% 
  summary()


# Sequential analysis
# Load package to run sequential analysis
library(gsDesign)

# Run sequential analysis
seq_analysis_3looks <- gsDesign(k = 2,
                                test.type = 1,
                                alpha = 0.05,
                                beta = 0.2,
                                sfu = "Pocock")
seq_analysis_3looks
# find stopping points
max_n <- 3000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis_3looks$timing
stopping_points

#install.packages("bayesAB")

