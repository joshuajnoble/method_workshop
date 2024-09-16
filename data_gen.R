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
# 
# class_count <- 838
# 
# prek <- rbinom(class_count, 1, 0.4)
# #new_books <- rbinom(class_count, 1, 0.25)
# reading_scores <- data.frame(prek)
# 
# reading_scores <- reading_scores %>%
#   mutate(
#     reading_scores,
#     class_size = round(rnorm(class_count, 25, 4)) - (prek * 2),
#     teacher_exp = round(rgamma(class_count, shape=9, rate=1))
#   )
# 
# reading_scores$new_books <- rbinom(class_count, 1, (reading_scores$class_size/(3.0 * max(reading_scores$class_size))))
# reading_scores[reading_scores$class_size > unlist(quantile(reading_scores$class_size, 0.95)[[1]]),]$new_books = 1
# reading_scores[reading_scores$class_size < unlist(quantile(reading_scores$class_size, 0.05)[[1]]),]$new_books = 0
# 
# for(i in 1:nrow(reading_scores)) {
#   if(rbinom(1, 1, (reading_scores$teacher_exp[i]/(2.0 * max(reading_scores$teacher_exp)))) == 1) {
#     reading_scores$new_books[i] = 1
#   }
# }
# nrow(reading_scores[reading_scores$new_books == 1,])
# 
# nnew <- nrow(reading_scores[reading_scores$new_books == 0,])
# ynew <- nrow(reading_scores[reading_scores$new_books == 1,])
#  
# reading_scores$scores <- rep(0, class_count)
# 
# n_over <- reading_scores[reading_scores$new_books == 0,]$class_size - 15
# y_over <- reading_scores[reading_scores$new_books == 1,]$class_size - 15
# 
# class_mod_n <- (3.0 - dexp(n_over, rate=0.15) * 20) * (n_over/2)
# plot(class_mod_n, reading_scores[reading_scores$new_books == 0,]$class_size)
# 
# class_mod_y <- (3.0 - dexp(y_over, rate=0.15) * 20) * (y_over/2)
# plot(class_mod_y, reading_scores[reading_scores$new_books == 1,]$class_size)
# 
# plot(class_mod_n, reading_scores[reading_scores$new_books == 0,]$class_size)
# plot(n_over, reading_scores[reading_scores$new_books == 0,]$class_size)
# 
# reading_scores[reading_scores$new_books == 0,]$scores <- rep(50, nnew) + 
#   rnorm(nnew, 20, 10) + (1.8 * reading_scores[reading_scores$new_books == 0,]$prek) - 
#   (class_mod_n)
# 
# reading_scores[reading_scores$new_books == 1,]$scores <- rep(50, ynew) + 
#   rnorm(ynew, 23, 10) + (2.0 * reading_scores[reading_scores$new_books == 1,]$prek) - 
#   (class_mod_y)
# 
# mean(reading_scores[reading_scores$new_books == 1,]$scores) - mean(reading_scores[reading_scores$new_books == 0,]$scores)
# 
# saveRDS(reading_scores, "scores")


reading_scores <- readRDS("scores")

mod <- lm("scores ~ new_books", reading_scores)
stargazer(mod, type = "text")

mod <- lm("scores ~ new_books + prek", reading_scores)
stargazer(mod, type = "text")

mod_prek <- glm("new_books ~ class_size + teacher_exp", family=binomial(link = "probit"), reading_scores)
stargazer(mod_prek, type = "text")

model_all <- lm(scores ~ new_books + prek + class_size + teacher_exp, reading_scores)
summary(model_all)
coeftest(model_all, vcov = vcovHC)

matched <- matchit(new_books ~ class_size + teacher_exp + prek, method = "cem", data = reading_scores)
#matched <- matchit(new_books ~ class_size + prek + teacher_exp, method = "exact", data = reading_scores)
summary(matched)
matched_results <- match.data(matched)

library(cobalt)

love.plot(matched, stats="mean.diffs", treat="new_books", drop.distance = TRUE)

summary(matched)

fit <- lm(scores ~ (new_books * (class_size + prek + teacher_exp)), data = matched_results, weights = weights)
avg_comparisons(fit,
                variables = c("prek"),
                vcov = ~subclass,
                wts = "weights",
                by = "new_books")

avg_comparisons(fit,
                     variables = c("new_books"),
                     vcov = ~subclass,
                     wts = "weights",
                     by = "prek")

avg_comparisons(fit,
                variables = "new_books",
                #newdata = subset(matched_results, prek == 1),
                vcov = ~subclass,
                wts = "weights")

tt <- t.test(scores ~ new_books, data = matched_results, alternative='t')
t.test(scores ~ new_books, data = matched_results, alternative='t')

lmtest::coeftest(lm(scores ~ (new_books + class_size + prek + teacher_exp), data = matched_results, weights = weights), vcov. = sandwich::vcovCL, cluster = ~subclass)

summary(comparisons(fit, vcov = ~subclass, variables = c("new_books", "prek"), wts="weights"))

# type = "response",
# by = "am",
# hypothesis = "revpairwise"

t <- t.test(scores ~ new_books, data=reading_scores)
t <- add_significance(data=reading_scores)

ggplot(reading_scores, aes(scores, colour=as.factor(new_books), group=as.factor(new_books))) +
  geom_density(adjust=2) + geom_text(
    label=paste("P-Value: ", signif(t$p.value, digits=4)), x=15, y=0.038,
    color = "blue"
  )

t.test(reading_scores[new_books == 1,]$class_size, reading_scores[new_books == 0,]$class_size)

splicemr <- matched_results[as.numeric(matched_results$subclass) < 50,]

ggplot(aes(y = scores, x = class_size, color=as.factor(prek)), data=splicemr) +
  geom_point(size=2) +
  geom_line(aes(group = as.factor(subclass)), color = "white", alpha=0.7) +
  guides(color = guide_legend(title = "Pre-K"))

ggplot(aes(y = teacher_exp, x = class_size, color=as.factor(new_books)), data=reading_scores) +
  geom_point(size=2)

ggplot(aes(y = scores, x = class_size, color=as.factor(new_books)), data=reading_scores) +
  geom_point(size=1)

cor_scores <- reading_scores
cor_scores$prek <- as.numeric(cor_scores$prek)
cor_scores$new_books <- as.numeric(cor_scores$new_books)
cor(cor_scores)

ggplot(reading_scores, aes(x = scores)) +
  geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                 position = "identity", bins = 20, alpha = 0.2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  geom_vline(xintercept = tapply(reading_scores$scores, reading_scores$new_books, mean),col=c("#00AFBB", "#E7B800"),size=1, linetype="dotted")

ggplot(reading_scores, aes(x = class_size)) +
  geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                 position = "identity", bins = 20, alpha = 0.2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

ggplot(matched_results, aes(x = class_size)) +
  geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                 position = "identity", bins = 20, alpha = 0.2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

ggplot(reading_scores, aes(x = class_size)) +
  geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                 position = "identity", bins = 20, alpha = 0.2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

summary(glm(new_books ~ teacher_exp + class_size, data = reading_scores))


ggplot(reading_scores, aes(scores, colour=as.factor(new_books), group=as.factor(new_books))) +
  geom_density(adjust=2)

splicemr <- matched_results[as.numeric(matched_results$subclass) < 10,]

class_means <- as.data.frame(group_by(matched_results, subclass) %>%
          reframe(xc=mean(class_size), 
                    yc=mean(teacher_exp),
                    density=(max(teacher_exp) - min(teacher_exp)) + (max(class_size) - min(class_size)) + 1,
                    prek=mean(prek),
                    n=n()))

ggplot() +
  ggnewscale::new_scale_color() +
  geom_point(data=class_means[class_means$prek == 1,], aes(x=xc, y=yc, color=n, size=density)) +
  scale_color_gradient(low = alpha("#00AFBB", 0.2), high = alpha("#00AFBB", 0.8), name = "Pre-K \nClassrooms", breaks=c(5, 15, 25)) +
  ggnewscale::new_scale_color() +
  geom_point(data=class_means[class_means$prek == 0,], aes(x=xc, y=yc, color=n, size=density)) +
  scale_color_gradient(low = alpha("#E7B800", 0.2), high = alpha("#E7B800", 0.8), name = "No Pre-K \nClassrooms", breaks=c(5, 15, 25)) +
  ggtitle("Matched groups") + xlab("Class Size") + ylab("Teacher Experience") +
  labs( size = "Spread", alpha = "Classrooms") +
  theme_light(base_size = 10)

# ggplot() + geom_rect(data=mids, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), colour="#e68cff33", fill ="#e68cff", alpha=0.2) +
#   geom_point(aes(teacher_exp, class_size, color=as.factor(prek)), data=splicemr) +         
#   guides(color = guide_legend(title = "Pre-K")) +
#   ggtitle("Matched groups for first 50 classrooms") + removeGrid()
# 
# ggplot() + geom_bin_2d(data=mids, aes(x=xc, y=yc, binwidth=n), colour="#e68cff33", fill ="#e68cff", alpha=0.2) +
#   geom_point(aes(class_size, teacher_exp, color=as.factor(prek)), data=splicemr) +         
#   guides(color = guide_legend(title = "Pre-K")) +
#   ggtitle("Matched groups for first 50 classrooms") + removeGrid()
# 


ggplot(aes(x = xc, y = yc, color=as.factor(subclass)), data=mids) +
  geom_count(aes(size=n), alpha=0.8) +
  removeGrid()

ematched <- matchit(new_books ~ class_size + prek + teacher_exp, method = "exact", data = reading_scores)
summary(ematched)
ematched_results <- match.data(ematched)

splice_exact <- ematched_results[as.numeric(ematched_results$subclass) < 10,]

ggplot(aes(x = teacher_exp, y = class_size, color=as.factor(subclass)), data=splice_exact) +
  geom_point(size=splice_exact$teacher_exp, alpha=0.1) +
  guides(color = guide_legend(title = "Factors")) + removeGrid()



ggplot(mtcars[1:16, ], aes(wt, disp)) +
  # geom_point(aes(color = mpg), size = 3) +
  # scale_color_gradient(low = alpha("navy", 0), high = "navy",
  #                      name = "Below average") +
  ggnewscale::new_scale_color() +
  geom_point(aes(color = mpg), data = mtcars[17:32,], size = 3) +
  scale_color_gradient(low = alpha("red3", 0), high = "red3", name = "Above average") +
  theme_light(base_size = 16)
