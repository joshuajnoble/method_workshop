# 
install.packages("MatchIt")
install.packages("stargazer")
install.packages("tidyverse")
install.packages("marginaleffects")
install.packages("cobalt")

library(data.table)
library(stargazer)
library(tidyverse)

library(MatchIt)
library(marginaleffects)
library(cobalt)
library(sandwich)
library(lmtest)

class_count <- 838

prek <- rbinom(class_count, 1, 0.4)
#new_books <- rbinom(class_count, 1, 0.25)
reading_scores <- data.frame(prek)

reading_scores <- reading_scores %>%
  mutate(
    reading_scores,
    class_size = round(rnorm(class_count, 25, 4)) - (prek * 2),
    teacher_exp = round(rgamma(class_count, shape=9, rate=1))
  )

reading_scores$new_books <- rbinom(class_count, 1, (reading_scores$class_size/(3.0 * max(reading_scores$class_size))))
reading_scores[reading_scores$class_size > unlist(quantile(reading_scores$class_size, 0.95)[[1]]),]$new_books = 1
reading_scores[reading_scores$class_size < unlist(quantile(reading_scores$class_size, 0.05)[[1]]),]$new_books = 0

for(i in 1:nrow(reading_scores)) {
  if(rbinom(1, 1, (reading_scores$teacher_exp[i]/(2.0 * max(reading_scores$teacher_exp)))) == 1) {
    reading_scores$new_books[i] = 1
  }
}
nrow(reading_scores[reading_scores$new_books == 1,])

nnew <- nrow(reading_scores[reading_scores$new_books == 0,])
ynew <- nrow(reading_scores[reading_scores$new_books == 1,])

reading_scores$scores <- rep(0, class_count)

n_over <- reading_scores[reading_scores$new_books == 0,]$class_size - 15
y_over <- reading_scores[reading_scores$new_books == 1,]$class_size - 15

class_mod_n <- (3.0 - dexp(n_over, rate=0.15) * 20) * (n_over/2)
class_mod_y <- (3.0 - dexp(y_over, rate=0.15) * 20) * (y_over/2)

reading_scores[reading_scores$new_books == 0,]$scores <- rep(50, nnew) +
  rnorm(nnew, 20, 10) + (1.8 * reading_scores[reading_scores$new_books == 0,]$prek) -
  (class_mod_n)

reading_scores[reading_scores$new_books == 1,]$scores <- rep(50, ynew) +
  rnorm(ynew, 23, 10) + (2.0 * reading_scores[reading_scores$new_books == 1,]$prek) -
  (class_mod_y)

saveRDS(reading_scores, "scores")