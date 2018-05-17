# 2. English Lexicon Project data

data2 <- read.csv('C:/Documents/Onik/R/ELP.csv', sep = ',')

#2.1
```{r}
cor_matrix <- data2 %>% select_if(is.numeric) %>% cor
max_cor <- max(cor_matrix[lower.tri(cor_matrix)])
rownames(which(cor_matrix == max_cor, arr.ind=TRUE))
```
#2.2
```{r}
ggplot(data2, aes(SUBTLWF, Mean_RT))+
  geom_point(aes(col = Length))+
  scale_color_continuous(low = "lightblue", high = "red")+
  facet_wrap(~ POS)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_trans(x = "log10")
```
#2.3
```{r}
model2 <- lm(Mean_RT ~ log(SUBTLWF) + POS, data2)
```
#2.3.1
```{r}
summary(model2)$call
```
#2.3.2
```{r}
summary(model2)$adj.r.squared
```
#2.3.3
```{r}
ggplot(data2, aes(log(SUBTLWF), Mean_RT))+
  geom_point(aes(col = Length))+
  scale_color_continuous(low = "lightblue", high = "red")+
  geom_smooth(method='lm', se = F, col = 'gray')
```
#2.4
```{r}
mix_model2 <- lmer(Mean_RT ~ log(SUBTLWF) + (1|POS), data2)
```
#2.4.1
```{r}
summary(mix_model2)$call
```
#2.4.2
```{r}
summary(mix_model2)$varcor$POS[1]
```
#2.4.3
```{r}
data2$mix_pred <- predict(mix_model2)
ggplot(data2, aes(log(SUBTLWF), Mean_RT))+
  geom_point(aes(col = POS))+
  facet_wrap(~ POS)+
  geom_line(data = data2, aes(x = log(SUBTLWF), y = mix_pred), col = 'black', size = 0.7)
```
