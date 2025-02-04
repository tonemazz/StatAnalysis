re = read.csv("C:/Users/tmazi/Desktop/Stats Project/UCI_Data_Sets/Real-Estate/realestate.csv")

# Previewing data
head(re)

# Looking at if there's any significant relationships
plot(re)

# Price and MRTdistance have an interesting curve, so we'll single that out
plot(re[, c(3, 7)])

# Now we create a scatterplot of our singled out data
plot(re$MRTdistance, re$price, xlab = "Distance from MRT Station", ylab = "Price")     

# Just eyeballing the data, a line of best fit could probably be used, but I thought the relationship between the data looked more logarithmic
fit.MRTdistance = lm(price ~ log(MRTdistance), data = re)

# Rendering our curve
curve(predict(fit.MRTdistance, newdata = data.frame(MRTdistance = x)), add = TRUE, col = "red", lwd = 2)

# Getting the summary of our model
summary(fit.MRTdistance)
AIC(fit.MRTdistance)

m1 = fit.MRTdistance
summary(m1)
coef(m1)

# We'll first try a model with all predictors, in a linear fit.
m2.all <- lm(price ~ ., data=re)
summary(m2.all)

# Longitude has an extremely high p-value, so we choose to disclude it from the model. We also include a logarithmic term for MRTdistance.
m2 = lm(price ~ log(MRTdistance) + age + latitude + convenience + date, data=re)
summary(m2)
AIC(m1, m2.all, m2)

################## R^2 (Adjusted) for:
#     m1:     0.5381 
# m2.all:     0.5762 
#     m2:     0.6481 

