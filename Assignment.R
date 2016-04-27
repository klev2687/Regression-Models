data(mtcars)
head(mtcars)

#Convert to Rmd later

mtcars$am <- factor(mtcars$am, labels = c("Auto", "Manual"))
#Exploratory Analysis
boxplot(mpg~am, data=mtcars, xlab="Transmission", ylab="MPG")
#Manual transmission seems to have higher MPG when compared to Automatic

fit1 <- lm(mpg~am, data = mtcars)
plot(fit1)
summary(fit1)$coef
#Automatic transmission has lower mean than Manual transmission which seems reasonable and agrees with the exploratory boxplot

#T test to confirm if mpg data samples are different
with(mtcars, t.test(mpg[am=="Auto"], mpg[am=="Manual"]))
#T test with p-value=0.001374 confirms that the means are significantly difference

#Need to check if other variables influence mpg 
sort(cor(mtcars)["mpg",])

#Consider wt, cyl, disp, hp as adjustment variables to remove bias. Checking for correlation between selected variable
cor(mtcars)[c("wt", "cyl", "disp", "hp"),]
#cyl and disp are highly correlated and hence excluded. We will model our regression using variable wt, am and hp

model <- lm(mpg~am+wt+hp, mtcars)
summary(model)
plot(model)

