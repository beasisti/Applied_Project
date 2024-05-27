# Libraries -------------------------------------------------------------------
library(dplyr)
library(zoo)
library(ggplot2)
library(broom)
library(corrplot)
library(insight)
library(lattice)
library(lme4)
library(lubridate)


data <- read.csv('./Datasets/top20_products.csv')

data$Time <- ymd(data$Time)
data$Year <- year(data$Time)
data$Month <- month(data$Time)
data <- data %>%
  mutate_at(vars(8:38), as.factor)

colnames(data)[colnames(data) == "Vendite.in.Valore.Solo.Special.Pack"] <- "Sconto.Solo.Special.Pack"
colnames(data)[colnames(data) == "Vendite.in.Valore.Solo.Volantino"] <- "Sconto.Solo.Volantino"
colnames(data)[colnames(data) == "Vendite.in.Valore.Solo.Display"] <- "Sconto.Solo.Display"
colnames(data)[colnames(data) == "Vendite.in.Valore.Solo.Riduzione.Prezzo"] <- "Sconto.Solo.Riduzione.Prezzo"
colnames(data)[colnames(data) == "Vendite.in.Valore.Solo.Sconto.Loyalty"] <- "Sconto.Solo.Loyalty"
# we do the regression using only these types of discounts 
data[, 13:38] <- NULL

data$Prezzo_NoSconto <- data$Vendite.in.Valore.Senza.promozioni / data$Vendite.in.Volume.Senza.promozioni
data$Prezzo_Sconto <- data$Vendite.in.Valore.Con.promozioni / data$Vendite.in.Volume.Con.promozioni
data <- data %>%
  mutate(Prezzo_Sconto = ifelse(is.na(Prezzo_Sconto), Prezzo_NoSconto, Prezzo_Sconto))


# Correlation plot -------------------------------------------------------------------

numeric_vars <- data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = 'color', type = 'full', 
         tl.col = "black", tl.cex = 0.8,
         title = "Matrice di Correlazione", mar = c(0, 0, 1, 0))


# Linear regression for Volume Sales -------------------------------------------------------------------

# log of the response (look at the histogram)
data <- data %>%
  mutate(Vendite.in.Volume.log = log(Vendite.in.Volume))

model.0 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto +
                  Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                  Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                  Sconto.Solo.Loyalty, data = data)
summary(model.0)


model.1 <- lm(Vendite.in.Volume.log ~ Prezzo_Sconto +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto.Solo.Loyalty, data = data)
summary(model.1)


model.3 <- lm(Vendite.in.Volume.log ~ Prezzo_Sconto +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo, 
              data = data)
summary(model.3)


model.4 <- lm(Vendite.in.Volume.log ~ Prezzo_Sconto + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo, 
              data = data)
summary(model.4)

plot(model.4)


# Prediction

predicted_vols <- predict(model.4, data)
data$Predicted.Volumes <- predicted_vols

data.moretti <- data[which(data$Brand == 'MORETTI'),]
ggplot(data.moretti, aes(x = Time)) + 
  geom_point(aes(y = Vendite.in.Volume.log, color = "Actual Volumes"), cex = .7) + 
  geom_point(aes(y = Predicted.Volumes, color = "Predicted Volumes"), cex = .7) + 
  labs(title = "Actual Volumes vs. Predicted", x = "Time", y = "Volumes") +
  scale_color_manual(values = c("Actual Volumes" = "blue", "Predicted Volumes" = "red")) +
  theme_minimal()


# Linear regression for € Sales -------------------------------------------------------------------

model.0 <- lm(Vendite.in.Valore ~ Vendite.in.Valore.Senza.promozioni + Vendite.in.Valore.Con.promozioni +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto.Solo.Loyalty + Sconto, data = data)
summary(model.0)

model.1 <- lm(Vendite.in.Valore ~ Vendite.in.Valore.Con.promozioni +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto.Solo.Loyalty + Sconto, data = data)
summary(model.1)

model.2 <- lm(Vendite.in.Valore ~ Vendite.in.Valore.Con.promozioni +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto, data = data)
summary(model.2)

model.3 <- lm(Vendite.in.Valore ~ Vendite.in.Valore.Con.promozioni +
                Sconto.Solo.Special.Pack + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto, data = data)
summary(model.3)

model.4 <- lm(Vendite.in.Valore ~ Vendite.in.Valore.Con.promozioni +
                Sconto.Solo.Special.Pack + Sconto.Solo.Riduzione.Prezzo + 
                Sconto, data = data)
summary(model.4)

model.5 <- lm(Vendite.in.Valore ~ Vendite.in.Valore.Con.promozioni + 
                Sconto.Solo.Riduzione.Prezzo + 
                Sconto, data = data)
summary(model.5)

plot(model.5)

# Prediction
predicted_sales <- predict(model.5, data)
data$Predicted.Sales <- predicted_sales

data.moretti <- data[which(data$Brand == 'MORETTI'),]
ggplot(data.moretti, aes(x = Time)) + 
  geom_point(aes(y = Vendite.in.Valore, color = "Actual Sales")) + 
  geom_point(aes(y = Predicted.Sales, color = "Predicted Sales")) + 
  labs(title = "Actual Sales vs. Predicted Sales", x = "Time", y = "Sales") +
  scale_color_manual(values = c("Actual Sales" = "blue", "Predicted Sales" = "red")) +
  theme_minimal()


# Linear Mixed Model for Volume Sales -------------------------------------------------------------------

ggplot(data = data, aes(x = as.factor(Product), y = Vendite.in.Volume, fill = as.factor(Product))) +
  geom_boxplot() +
  labs(x = 'Brand', y = 'Volumes') +
  ggtitle('Boxplot of Volume Sales among Brands') +
  theme_minimal() +
  theme(axis.text = element_text(size=rel(0.2)), axis.title = element_text(size=rel(1)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')

ggplot(data = data, aes(x = as.factor(Brand), y = Vendite.in.Volume, fill = as.factor(Brand))) +
  geom_boxplot() +
  labs(x = 'Brand', y = 'Volumes') +
  ggtitle('Boxplot of Volume Sales among Brands') +
  theme_minimal() +
  theme(axis.text = element_text(size=rel(0.2)), axis.title = element_text(size=rel(1)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')

ggplot(data = data, aes(x = as.factor(Vendor), y = Vendite.in.Volume, fill = as.factor(Vendor))) +
  geom_boxplot() +
  labs(x = 'Group', y = 'Volumes') +
  ggtitle('Boxplot of Volume Sales among Brands') +
  theme_minimal() +
  theme(axis.text = element_text(size=rel(0.2)), axis.title = element_text(size=rel(1)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')

boxplot(model.12$residuals ~ data$Brand, col = 'orange', xlab = 'Brand', ylab = 'Residuals')
# residuals differ a lot across brands

# standardize variables
data <- data %>%
  mutate(Vendite.in.Volume.Con.promozioni.std = scale(Vendite.in.Volume.Con.promozioni),
         Sconto.std = scale(Sconto))

lmm.0 = lmer(Vendite.in.Volume ~ Vendite.in.Volume.Con.promozioni.std + 
               Sconto.Solo.Riduzione.Prezzo + 
               Sconto.std + (1|Product), data = data)
summary(lmm.0)

# fixed effects and 95% CIs
confint(lmm.0, oldNames=TRUE)
fixef(lmm.0)

# random effects: b_0i
ranef(lmm.0)

# random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2, beta_3)
coef(lmm.0)
head(coef(lmm.0)$Product)

# let's plot all the regression lines
plot(data$Vendite.in.Valore.Con.promozioni.std, data$Vendite.in.Volume, col = 'grey',
     main = 'Data and regression lines')          
for(i in 1:12){
  abline(coef(lmm.0)$Product[i,1], coef(lmm.0)$Product[i,2])
}

plot(data$Sconto.std, data$Vendite.in.Volume, col = 'grey',
     main = 'Data and regression lines')          
for(i in 1:12){
  abline(coef(lmm.0)$Product[i,1], coef(lmm.0)$Product[i,4])
}


# Diagnostic plots 
# 1) Assessing Assumption on the within-group errors
plot(lmm.0)

qqnorm(resid(lmm.0))
qqline(resid(lmm.0), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm.0)$Product), main='Normal Q-Q Plot - Random Effects for Brand')
qqline(unlist(ranef(lmm.0)$Product), col='red', lwd=2)


# Prediction
predict_re <- predict(lmm.0)
head(predict_re)


# Let's add also the random slope
lmm.1 = lmer(Vendite.in.Volume ~ Vendite.in.Volume.Con.promozioni.std + 
               Sconto.Solo.Riduzione.Prezzo + 
               Sconto.std + (1 + Sconto.std|Brand), data = data)
summary(lmm.1)

confint(lmm.1, oldNames=TRUE)

# Fixed effects: (beta_0, beta_1, beta_2, beta_3)
fixef(lmm.1)

# Random effects: (b_0i, b_3i) 
ranef(lmm.1)
head(ranef(lmm.1)$Brand)

x11()
dotplot(ranef(lmm.1))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm.1)
head(coef(lmm.1)$Brand)

# Visualization of random effects 
x11()
par(mfrow=c(1,3))
plot(c(1:50), unlist(coef(lmm2)$school_id[1]),
     xlab='School i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts')
abline(h=fixef(lmm2)[1], lty=2, col='red', lwd=2)
legend(30, 13.5, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(c(1:50), unlist(coef(lmm2)$school_id[2]),
     xlab='School i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slope for gender')
abline(h=fixef(lmm2)[2], lty=2, col='red', lwd=2)
legend(30,-0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(c(1:50), unlist(coef(lmm2)$school_id[3]),
     xlab='Student i', ylab=expression(beta[2]+b['1i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random slopes for escs')
abline(h=fixef(lmm2)[3], lty=2, col='red', lwd=2)
legend(30, 5, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)


# Let's plot all the regression lines
## FEMALES
x11()
par(mfrow=c(1,2))
plot(school$escs[school$gender==0], school$achiev[school$gender==0],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for females')
for(i in 1:12){
  abline(coef(lmm2)$school_id[i,1], coef(lmm2)$school_id[i,3])
}

# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
plot(lmm.1)

qqnorm(resid(lmm.1))
qqline(resid(lmm.1), col='red', lwd=2)


# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm.1)$Brand[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm.1)$Brand[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm.1)$Brand[2]), main='Normal Q-Q Plot - Random Effects on escs')
qqline(unlist(ranef(lmm.1)$Brand[2]), col='red', lwd=2)

x11()
plot(unlist(ranef(lmm2)$school_id[2]),unlist(ranef(lmm2)$school_id[1]),
     ylab=expression(paste('Intercept  ', b['0i'])),
     xlab=expression(paste('escs  ', b['1i'])), col='dodgerblue2',
     main='Scatterplot of estimated random effects')
abline(v=0,h=0)
# Alternative plot(ranef(lmm2))


# Comparing models
#------------------
# The anova function, when given two or more arguments representing fitted models,
# produces likelihood ratio tests comparing the models.
anova(lmm1, lmm2)

# The p-value for the test is essentially zero -> we prefer lmm2
