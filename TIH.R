## Spagetti and boxplots of sodium

tiff("Plot2.tif", width = 4, height = 5, units = 'in', res = 300)

interaction.plot(TIH$Registration, TIH$ID, TIH$Sodium, xlab = "Registration", ylab = "Sodium (mmol/L)", legend=F, main="Sodium levels after discontinuation of thiazide threapy")

dev.off()

boxplot(Sodium~Registration,data=TIH, xlab = "Registration", ylab = "Sodium (mmol/L)", legend=F, main="Sodium levels")

## Spagetti and boxplots of urine osmolality
interaction.plot(TIH$Registration, TIH$ID, TIH$`Urine-osmolality`, xlab = "Registration", ylab = "Urine-osmolality (mOsmol/L)", legend=F, main="Urine-osmolality levels after discontinuation of thiazide threapy")
boxplot(`Urine-osmolality`~Registration,data=TIH, xlab = "Registration", ylab = "Urine-osmolality (mOsmol/L)", legend=F, main="Urine-osmolality levels after discontinuation of thiazide threapy")

boxplot(`Waterexcretion`~Registration,data=TIH, xlab = "Registration", ylab = "Free water excretion", legend=F, main="Free water excretion after discontinuation of thiazide threapy")
abline(h=0, lty=2)
boxplot(`UrinSodiumPotasium`~Registration,data=TIH, xlab = "Registration", ylab = "Urine sodium and potassium (mmol/L)", legend=F, main="Sodium and potassium excretion after discontinuation of thiazide threapy")


boxplot(Uosmbyeosm~Registration,data=TIH, xlab = "Registration", ylab = "Urine-osmolality (mOsmol/L)", legend=F, main="Urine-osmolality levels after discontinuation of thiazide threapy")
boxplot(Uosmbyeosm~Registration,data=TIH, xlab = "Registration", ylab = "Urine-osmolality (mOsmol/L)", legend=F, main="Urine-osmolality levels after discontinuation of thiazide threapy")



## linear regression
m1 <- lm(Waterexcretion ~ Registration, data = TIH)
summary(m1)
m2 <- glm(Waterexcretion ~ Registration, data = TIH)
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters 
fitted(model) # predicted values
residuals(m1) # residuals
anova(m1, m2) # anova table, compare models
vcov(model) # covariance matrix for model parameters 
influence(model) # regression diagnostics
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(m1)