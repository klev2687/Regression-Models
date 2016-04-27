data(mtcars)
head(mtcars)

#Convert to Rmd later
#Converting the appropriate variables to factors
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am, labels = c("Auto", "Manual"))
#Exploratory Analysis
boxplot(mpg~am, data=mtcars, xlab="Transmission", ylab="MPG", main="Transmission vs. MPG")
#Manual transmission seems to have higher MPG when compared to Automatic

fit1 <- lm(mpg~am, data = mtcars)
summary(fit1)
#Automatic transmission has lower mean than Manual transmission which seems reasonable and agrees with the exploratory boxplot

#T test to confirm if mpg data samples are different
with(mtcars, t.test(mpg[am=="Auto"], mpg[am=="Manual"]))
#T test with p-value=0.001374 confirms that the means are significantly difference

#Need to check if other variables influence mpg 
sort(cor(mtcars)["mpg",])

#Consider wt, cyl, disp, hp as adjustment variables to remove bias. Checking for correlation between selected variable
cor(mtcars)[c("wt", "cyl", "disp", "hp"),]
#cyl and disp are highly correlated and hence we exclude one of them to avoid bias. We will model our regression using variable wt, am, cyl and hp

model <- lm(mpg~am+wt+hp+cyl, mtcars)
summary(model)
par(mfrow=c(2,2))
plot(model)

anova(fit1, model)
#The p-value obtained is highly significant and we reject the null hypothesis and conclude that adjusted variables cyl, hp and wt contribute to the accuracy of the model.
