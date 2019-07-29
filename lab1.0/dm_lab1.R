
#Exercise 1.1

color = c('red', 'blue', 'orange', 'black')
colors = factor(color)
pattern <- sample(colors, 500, replace = TRUE)
pattern1 <- data.frame(pattern)
pattern2 <- matrix(pattern)


colors1 <- list('red', 'blue', 'orange', 'black')
patternl <- sample(colors1, 500, replace = TRUE)


#Exercise 1.2

data("cars")
cars2 <- cars
colnames(cars2) <- c("m/s", "m")
figure <- plot(cars2$`m/s`, cars2$m, main="scatterplot",xlab="speed", ylab="dist") + abline(lm(cars2$m~cars2$`m/s`))
cars$dist2 <- 3.6*cars$dist

#Exercise 1.3
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)
mean(age) #29.96296
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode <- getmode(age)
midrange <- (max(age) + min(age))/2

summary(age)
quantile(age)
boxplot(age)

#Exercise 1.4

age_adults <- c(23, 23, 27, 27, 39, 41, 47, 49, 50, 52, 54, 54, 56, 57, 58, 58, 60, 61)
percent_fat <- c(9.5, 26.5, 7.8, 17.8, 31.4, 25.9, 27.4, 27.2, 31.2, 34.6, 42.5, 28.8, 33.4, 30.2, 34.1, 32.9, 41.2, 35.7)

hopital_test <- data.frame(age_adults, percent_fat)

mean(age_adults)
median(age_adults)
sd(age_adults)

mean(percent_fat)
median(percent_fat)
sd(percent_fat)

figure2 <- plot(hopital_test$age_adults, hopital_test$percent_fat, main="scatterplot",xlab="age", ylab="%fat")
cor(age_adults,percent_fat)


#Exercise 1.5
OutVals1 = boxplot(age, plot=FALSE)$out
OutVals2 = boxplot(cars2$`m/s`, plot=FALSE)$out
OutVals3 = boxplot(cars2$m, plot=FALSE)$out
OutVals4 = boxplot(iris$Sepal.Length, plot=FALSE)$out
OutVals5 = boxplot(iris$Sepal.Width, plot=FALSE)$out
OutVals6 = boxplot(iris$Petal.Length, plot=FALSE)$out
OutVals7 = boxplot(iris$Petal.Width, plot=FALSE)$out

#Exercise 1.6
pres.age <- c(57, 61, 57, 57, 58, 57, 61, 54, 68, 51, 49, 64, 50, 48, 65, 52, 56, 46, 54, 49, 51, 47, 55, 55, 54, 42, 51, 56, 55, 51, 54, 51, 60, 61, 43, 55, 56, 61, 52, 69, 64, 46, 54, 47)
hist(pres.age)
OutVals8 = boxplot(pres.age, plot=FALSE)$out
boxplot(pres.age)

#Exercise 1.7
years <- c(1992,1994,1998,2002,2006,2010)
locations <- c('Albertville','Lillehammer','Nagano','Salt Lake City','Torino','Vancouver')
number_of_disciplines <- c(57,61,68,78,84,86)
heads_of_state <- c('F. Mitterand','King Harald V','Emperor Akihito','President G.Bush', 'President C. Ciampi','Governor General M.Jean')

winter_og <- data.frame(years, locations, number_of_disciplines, heads_of_state)

sub_winter_og <- winter_og
