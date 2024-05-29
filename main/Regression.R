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


# Creating Dataset for Regression -------------------------------------------------------------------

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

# add column for Holidays
easter_dates <- as.Date(c("2019-04-21", "2020-04-12", "2021-04-04", "2022-04-17", "2023-04-09", "2024-03-31"))
christmas_periods <- seq.Date(from = as.Date("2019-12-20"), to = as.Date("2024-01-06"), by = "year")

is_easter_period <- function(date) {
  any(sapply(easter_dates, function(easter) abs(as.numeric(difftime(date, easter, units = "days"))) <= 7))
}

is_christmas_period <- function(date) {
  any(sapply(christmas_periods, function(christmas) {
    christmas_start <- as.Date(format(christmas, "%Y-12-20"))
    christmas_end <- as.Date(format(christmas + 17, "%Y-%m-%d")) # Natale fino a 6 Gennaio
    date >= christmas_start & date <= christmas_end
  }))
}

data <- data %>%
  mutate(is_holiday = ifelse(sapply(Time, function(date) is_easter_period(date) | is_christmas_period(date)), 1, 0))


# add a column for Summer
is_summer_period <- function(date) {
  year <- year(date)
  summer_start <- as.Date(paste0(year, "-06-21"))
  summer_end <- as.Date(paste0(year, "-09-23"))
  date >= summer_start & date <= summer_end
}

data <- data %>%
  mutate(is_summer = ifelse(sapply(Time, is_summer_period), 1, 0))

data <- data %>%
  mutate_at(vars(21:22), as.factor)


# Correlation plot -------------------------------------------------------------------

numeric_vars <- data[, -c(17,18)] %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = 'color', type = 'full', 
         tl.col = "black", tl.cex = 0.8,
         title = "Matrice di Correlazione", mar = c(0, 0, 1, 0))

rm(numeric_vars)

# Linear regression for Volume Sales -------------------------------------------------------------------

# log of the response (look at the histogram)
data <- data %>%
  mutate(Vendite.in.Volume.log = log(Vendite.in.Volume),
         Prezzo_Sconto.log = log(Prezzo_Sconto))

model.0 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                  Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                  Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                  Sconto.Solo.Loyalty + is_holiday + is_summer + Month + Year,
              data = data)
summary(model.0) # 0.4482
# the model is significative, even if the R_adj is not really high

model.1 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_holiday + is_summer + Month + Year, data = data)
summary(model.1) # 0.4475 


model.2 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_holiday + is_summer + Month + Year, data = data)
summary(model.2) # 0.4465 

plot(model.2)


model.3 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_holiday + is_summer + Year, data = data)
summary(model.3)  # 0.4454

plot(model.3)


model.4 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + Year, data = data)
summary(model.4) # 0.443
plot(model.4)

# vanno sicuramente tolte delle osservazioni outlier / leverage


# Prediction

predicted_vols <- predict(model.4, data)
data$Predicted.Volumes.log <- predicted_vols

data <- data %>%
  mutate(Predicted.Volumes = exp(Predicted.Volumes.log))


data.brand <- data[which(data$Brand == 'BECK S'),]
ggplot(data.brand, aes(x = Time)) + 
  geom_point(aes(y = Vendite.in.Volume, color = "Actual Volumes"), cex = .7) + 
  geom_point(aes(y = Predicted.Volumes, color = "Predicted Volumes"), cex = .7) + 
  labs(title = "Actual Volumes vs. Predicted", x = "Time", y = "Volumes") +
  scale_color_manual(values = c("Actual Volumes" = "blue", "Predicted Volumes" = "red")) +
  theme_minimal()


# Linear Mixed Model for Volume Sales (Brand) -------------------------------------------------------------------

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

boxplot(model.4$residuals ~ data$Product, col = 'orange', xlab = 'Brand', ylab = 'Residuals')
boxplot(model.4$residuals ~ data$Brand, col = 'orange', xlab = 'Brand', ylab = 'Residuals')
# residuals differ a lot across brands and products

# random intercept
lmm.0 = lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
               Sconto.Solo.Volantino + 
               Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
               is_summer + (1 |Brand), data = data)
summary(lmm.0)

# fixed effects and 95% CIs
confint(lmm.0, oldNames=TRUE)
fixef(lmm.0)

# random effects: b_0i
ranef(lmm.0)

# random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2, beta_3)
coef(lmm.0)
head(coef(lmm.0)$Brand)

# let's plot all the regression lines
plot(data$Prezzo_NoSconto, data$Vendite.in.Volume.log, col = 'grey',
     main = 'Data and regression lines')          
for(i in 1:12){
  abline(coef(lmm.0)$Brand[i,1], coef(lmm.0)$Brand[i,2])
}



# Diagnostic plots 
# 1) Assessing Assumption on the within-group errors
plot(lmm.0)

qqnorm(resid(lmm.0))
qqline(resid(lmm.0), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm.0)$Brand), main='Normal Q-Q Plot - Random Effects for Brand')
qqline(unlist(ranef(lmm.0)$Brand), col='red', lwd=2)


# Prediction
predict_re <- predict(lmm.0)
head(predict_re)


# random slope
lmm.1 = lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
               Sconto.Solo.Volantino + 
               Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
               is_summer + (1 + Prezzo_NoSconto|Brand), data = data)
summary(lmm.1)

confint(lmm.1, oldNames=TRUE)

# Fixed effects: (beta_0, beta_1, beta_2, beta_3)
fixef(lmm.1)

# Random effects: (b_0i, b_3i) 
ranef(lmm.1)
head(ranef(lmm.1)$Brand)

dotplot(ranef(lmm.1))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm.1)
head(coef(lmm.1)$Brand)

# Let's plot all the regression lines

plot(data$Prezzo_NoSconto, data$Vendite.in.Volume.log, col='blue',
     xlab='escs', ylab='achievement',main='Data and regression lines for females')
for(i in 1:12){
  abline(coef(lmm.1)$Brand[i,1], coef(lmm.1)$Brand[i,2])
}

# Diagnostic plots 
# 1) Assessing Assumption on the within-group errors
plot(lmm.1)

qqnorm(resid(lmm.1))
qqline(resid(lmm.1), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm.1)$Brand[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm.1)$Brand[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm.1)$Brand[2]), main='Normal Q-Q Plot - Random Effects on escs')
qqline(unlist(ranef(lmm.1)$Brand[2]), col='red', lwd=2)

plot(ranef(lmm.1))


# Comparing models 
anova(lmm.0, lmm.1)

# the p-value for the test is essentially zero -> we prefer lmm.1



# random intercept
lmm.00 = lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
               Sconto.Solo.Volantino + 
               Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
               is_summer + (1 |Product), data = data)
summary(lmm.00)

# fixed effects and 95% CIs
confint(lmm.00, oldNames=TRUE)
fixef(lmm.00)

# random effects: b_0i
ranef(lmm.00)

# random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2, beta_3)
coef(lmm.00)
head(coef(lmm.00)$Product)

# let's plot all the regression lines
plot(data$Prezzo_NoSconto, data$Vendite.in.Volume.log, col = 'grey',
     main = 'Data and regression lines')          
for(i in 1:12){
  abline(coef(lmm.00)$Product[i,1], coef(lmm.00)$Product[i,2])
}

# Diagnostic plots 
# 1) Assessing Assumption on the within-group errors
plot(lmm.00)

qqnorm(resid(lmm.00))
qqline(resid(lmm.00), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm.00)$Product), main='Normal Q-Q Plot - Random Effects for Brand')
qqline(unlist(ranef(lmm.00)$Product), col='red', lwd=2)


# Prediction
predict_re <- predict(lmm.00)
head(predict_re)


# random slope
lmm.11 = lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
               Sconto.Solo.Volantino + 
               Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
               is_summer + (1 + Prezzo_NoSconto|Product), data = data)
summary(lmm.11)

confint(lmm.11, oldNames=TRUE)

# Fixed effects: (beta_0, beta_1, beta_2, beta_3)
fixef(lmm.11)

# Random effects: (b_0i, b_3i) 
ranef(lmm.11)
head(ranef(lmm.11)$Product)

dotplot(ranef(lmm.11))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm.11)
head(coef(lmm.11)$Product)

# Let's plot all the regression lines

plot(data$Prezzo_NoSconto, data$Vendite.in.Volume.log, col = 'blue', main = 'Data and regression lines for females')
for(i in 1:12){
  abline(coef(lmm.11)$Product[i,1], coef(lmm.11)$Product[i,2])
}

# Diagnostic plots 
# 1) Assessing Assumption on the within-group errors
plot(lmm.11)

qqnorm(resid(lmm.11))
qqline(resid(lmm.11), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm.11)$Product[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm.11)$Product[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm.11)$Product[2]), main='Normal Q-Q Plot - Random Effects on escs')
qqline(unlist(ranef(lmm.11)$Product[2]), col='red', lwd=2)

plot(ranef(lmm.11))


# Comparing models 
anova(lmm.00, lmm.11)
# the p-value for the test is essentially zero -> we prefer lmm.1
