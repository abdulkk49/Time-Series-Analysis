---
title: "R Notebook"
output: html_notebook
---

```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.
```{r}
#Histogram code

small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)

hist(small.size.dataset)

hist(small.size.dataset, xlab='My data points')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset))

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset), col='red', lwd=5)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)
lines(density(small.size.dataset), col='red', lwd=5)
```

```{r}
#Scatterplot Code
set.seed(2016)  # There is a typo in the video (set.seed=2016)
#Generate 50 scores from Normal distribution with mean 78 and sd 10
Test_1_scores=round(rnorm(50, 78, 10))

Test_2_scores=round(rnorm(50, 78, 14))

Test_1_scores # Data won't be the same with the data generated in the video lecture since there was a typo in set.seed. 

Test_2_scores # Data won't be the same with the data generated in the video lecture since there was a typo in set.seed. 

plot(Test_2_scores~Test_1_scores)

plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores')

plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores', col='blue')
```


```{r}
#Basic Inferential Stats
help(co2)
class(co2)
plot(co2, main='Atmospheric CO2 Concentration')
co2.values = as.numeric(co2) 
co2.times = as.numeric( time(co2) ) 


SSxx = sum((co2.times - mean(co2.times)) * (co2.times - mean(co2.times)) 
SSxy = sum((co2.values - mean(co2.values)) * (co2.times - mean(co2.times))
slope = SSxy / SSxx  
intercept = mean(co2.values) - slope*mean(co2.times)

co2.linear.model = lm(co2 ~ time(co2))
plot(co2, main='Atmospheric CO2 Concentration with Fitted Line') 
abline(co2.linear.model)
co2.fitted.values = slope*co2.times + intercept
co2.residuals
summary(co2.linear.model)
co2.residuals = resid(co2.linear.model)

par(mfrow=c(1,3)) 
c02.residuals = resid( co2.linear.model)
hist(co2.residuals, main= "Histogram of CO2 Residuals") 
qqnorm(c02.residuals, main= "Normal Probability Plot") 
qqline(c02.residuals) 
plot(c02.residuals ~ time(co2), main="Residuals on Time")
plot(c02.residuals ~ time(co2), xlim=c(1960, 1963), main="Zoomed in Residuals on Time")




```


```{r}
plot(extra~group, data = sleep)

sleep$group
attach(sleep)

extra.1 = extra[group == 1]
extra.1

extra.2=extra[group==2]

t.test(extra.1, extra.2, paired=TRUE, alternative="two.sided")

diffs = extra.1-extra.2 
qqnorm(diffs, main= "Normal Probability Plot") 
qqline(diffs)

sleep.linear.model = lm(extra.2 ~ extra.1 ) 
abline(sleep.linear.model)
sleep.residuals = resid(sleep.linear.model)
qqnorm(sleep.residuals)
qqline(sleep.residuals)
plot(extra.2~extra.1, xlab='extra sleep with drug 1', ylab='extra sleep with drug 2' , main='Extra Sleep Drug 2 against Extra Sleep Drug 1')
```

```{r}
pairs(trees, pch = 15, bg = c("blue"))
trees
```


