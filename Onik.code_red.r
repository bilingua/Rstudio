library(ggplot2)
library(lme4)
library(dplyr)

data <- read.csv('C:/Documents/Onik/R/duryagin_ReductionRussian.txt', sep = '\t')

# 1.1
ggplot(data, aes(f2, f1))+
  geom_point(aes(col = vowel))+
  scale_y_reverse()+
  scale_x_reverse()

# 1.2
ggplot(data, aes(vowel, f1))+
  geom_boxplot(aes(fill = vowel))+
  coord_flip()

ggplot(data, aes(vowel, f2))+
  geom_boxplot(aes(fill = vowel))+
  coord_flip()

# 1.3
boxplot(data$f1[data$vowel == 'a'])$out

#1.4
cor.test(data$f1, data$f2)

#1.5
cor.test(data$f1[data$vowel == 'a'], data$f2[data$vowel == 'a'])
cor.test(data$f1[data$vowel == 'A'], data$f2[data$vowel == 'A'])
cor.test(data$f1[data$vowel == 'y'], data$f2[data$vowel == 'y'])

#1.6
model <- lm(f2 ~ f1, data)
#1.6.1
summary(model)$call
#1.6.2
summary(model)$adj.r.squared
#1.6.3
ggplot(data, aes(f2, f1))+
  geom_point(aes(col = vowel))+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_smooth(method='lm', se = F, col = 'gray')

# 1.7
mix_model <- lmer(f1 ~ f2 + (1|vowel), data)
#1.7.1
summary(mix_model)$call
#1.7.2
summary(mix_model)$varcor$vowel[1]
#1.7.4
data$mix_pred <- predict(mix_model)
ggplot(data, aes(x = f2, y = f1))+
  geom_point(aes(col = vowel))+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_line(data = data, aes(x = f2, y = mix_pred, col = vowel), size = 0.7)
