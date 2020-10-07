# install.packages("tidyverse")
library(tidyverse)
library(lubridate)
data = read_csv("abtest.csv")
data

data %>%
  summarize(conv_rate = mean(conversion))

data %>%
  group_by(month(date)) %>%
  summarize(conversion_rate= mean(conversion))


# Compute conversion rate by week of the year
data_sum <- data %>%
  group_by(week(date)) %>%
  summarize(conversion_rate = mean(conversion))

# Build plot
ggplot(data_sum, aes(x = `week(date)`,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1))

# 1) LOGISTIC REGGRESION
# 1.1) COMPUTE POWER SAMPLE SIZE FOR LOGISTIC REGRESION
install.packages("powerMediation")
library(powerMediation)
total_sample_size <- SSizeLogisticBin(p1 = 0.2,
                                      p2 = 0.3,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)

total_sample_size
total_sample_size/2

# EXPLORE

# Group and summarize data
data_sum <- data %>%
  group_by(experiment, month(date)) %>%
  summarize(conversion_rate = mean(conversion))
data_sum
# Make plot of conversion rates over time
ggplot(data_sum,
       aes(x = `month(date)`,
           y = conversion_rate,
           color = experiment,
           group = experiment)) +
  geom_point() +
  geom_line()

# 1.2) PERFORMING TEST
# Load package for cleaning model results
library(broom)
# View summary of results
data %>%
  group_by(experiment) %>%
  summarize(conversion_rate = mean(conversion))

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


library(bayesAB)

A_binom <- rbinom(250, 1, .25)
B_binom <- rbinom(300, 1, .2)

plotBeta(65, 200)

AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 65, 'beta' = 200), n_samples = 1e5, distribution = 'bernoulli')
plot(AB1)
binomialBandit <-banditize(AB1)
