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
library(tidyr)
library(plm)
library(merTools)
library(nortest)


# Creating Dataset for Regression -------------------------------------------------------------------

data <- read.csv('./Datasets/top20_products.csv')

data$Time <- ymd(data$Time)
data$Year <- year(data$Time)
data$Month <- month(data$Time)

data <- data %>%
  mutate_at(vars(8:38), as.factor)

data <- data %>%
  mutate(Product = recode(Product,
                          "MORETTI BIRRA LAGER REGOLARE 4.6 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800143550001" = "Moretti 66 Cl",
                          "PERONI BIRRA LAGER REGOLARE 4.7 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800000106790" = "Peroni 66 Cl",
                          "HEINEKEN BIRRA LAGER REGOLARE 5 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800689045420" = "Heineken 66 Cl",
                          "ICHNUSA NON FILTRATA BIRRA LAGER REGOLARE 5 % BOTTIGLIA DI VETRO 50 CL 1 CT - 800689068154" = "Ichnusa non filtrata 50 Cl",
                          "TUBORG BIRRA LAGER REGOLARE 5 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800000105931" = "Tuborg 66 Cl",
                          "ICHNUSA BIRRA LAGER REGOLARE 4.7 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800689075846" = "Ichnusa 66 Cl",
                          "PERONI BIRRA LAGER REGOLARE 4.7 % BOTTIGLIA DI VETRO 99 CL 3 CT - 800844000000" = "Peroni 33 Cl x 3",
                          "BECK S BIRRA LAGER REGOLARE 5 % BOTTIGLIA DI VETRO 66 CL 1 CT - 410013092378" = "Beck's 66 Cl",
                          "MORETTI BIRRA LAGER REGOLARE 4.6 % BOTTIGLIA DI VETRO 396 CL 6 CT - 800689002129" = "Moretti 66 Cl x 6",
                          "HEINEKEN BIRRA LAGER REGOLARE 5 % BOTTIGLIA DI VETRO 99 CL 3 CT - 800689043416" = "Heineken 33 Cl x 3",
                          "ICHNUSA BIRRA LAGER REGOLARE 4.7 % BOTTIGLIA DI VETRO 99 CL 3 CT - 800689032513" = "Ichnusa 33 Cl x 3",
                          "MORETTI BIRRA LAGER REGOLARE 4.6 % BOTTIGLIA DI VETRO 198 CL 6 CT - 800143522531" = "Moretti 33 Cl x 6",
                          "MORETTI BIRRA LAGER REGOLARE 4.6 % BOTTIGLIA DI VETRO 99 CL 3 CT - 800143522521" = "Moretti 33 Cl x 3",
                          "PERONI NASTRO AZZURRO BIRRA LAGER REGOLARE 5.2 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800000106793" = "Peroni Nastro Azzurro 66 Cl",
                          "BAVARIA BIRRA PILSNER REGOLARE 5 % BOTTIGLIA DI VETRO 66 CL 1 CT - 871480002489" = "Bavaria 66 Cl",
                          "BIRRIFICIO ANGELO PORETTI 3 LUPPOLI BIRRA LAGER REGOLARE 4.5 % BOTTIGLIA DI VETRO 66 CL 1 CT - 800795001002" = "Poretti 3 Luppoli 66 Cl",
                          "PERONI BIRRA LAGER REGOLARE 4.7 % BOTTIGLIA DI VETRO 198 CL 6 CT - 800844051001" = "Peroni 33 Cl x 6",
                          "CORONA BIRRA LAGER REGOLARE 4.6 % BOTTIGLIA DI VETRO 35.5 CL 1 CT - 750000103281" = "Corona 35.5 Cl",
                          "DREHER BIRRA LAGER REGOLARE 4.7 % BOTTIGLIA DI VETRO 99 CL 3 CT - 800689011133" = "Dreher 33 Cl",
                          "MORETTI BIRRA LAGER REGOLARE 4.6 % LATTINA 66 CL 2 CT - 800143544001" = "Moretti 33 Cl x 2 (lattina)"))


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


# add column for lag regression 

data <- data %>%
  arrange(Time) %>% 
  mutate(Vendite.in.Volume.Settimana.Precedente = lag(Vendite.in.Volume, default = 0))


# add cluster column

data.cluster <- read.csv('./Datasets/data_cluster.csv')
data.cluster$cluster <- data.cluster$cluster - 1
data <- merge(data, data.cluster[, c("Product", "cluster", "Time")], by = c("Product", "Time"), all.x = TRUE)
data$cluster <- as.factor(data$cluster)
# 1 is for leader, 0 for follower


# Correlation plot -------------------------------------------------------------------

numeric_vars <- data[, -c(17,18)] %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot::corrplot(cor_matrix, method = 'color', type = 'full', 
         tl.col = "black", tl.cex = 0.8,
         title = "Matrice di Correlazione", mar = c(0, 0, 1, 0))

rm(numeric_vars)


# Take a look at data -------------------------------------------------------------------

ggplot(data, aes(x = Vendite.in.Volume)) + 
  geom_histogram(binwidth = 5000, fill = "lightpink", color = "grey") +
  labs(title = "Distribuzione delle Vendite", x = "Vendite in Valore", y = "Frequenza") + 
  theme_classic()

ggplot(data, aes(x = Vendite.in.Volume.Settimana.Precedente)) + 
  geom_histogram(binwidth = 1000, fill = "grey", color = "skyblue") +
  labs(title = "Distribuzione delle Vendite", x = "Vendite in Valore", y = "Frequenza")

hist(data$Prezzo_Sconto, prob = T, xlab = 'Prezzo con Sconto')
hist(data$Prezzo_NoSconto, prob = T, xlab = 'Prezzo senza Sconto')

# apply the log 
data <- data %>%
  mutate(Vendite.in.Volume.log = log(Vendite.in.Volume),
         Prezzo_Sconto.log = log(Prezzo_Sconto))

data <- data %>%
  mutate(Vendite.in.Volume.Settimana.Precedente.log = ifelse(Vendite.in.Volume.Settimana.Precedente != 0,
                                                             log(Vendite.in.Volume.Settimana.Precedente),
                                                             0))


# Linear regression for Volume Sales -------------------------------------------------------------------

model.0 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Special.Pack + Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto.Solo.Loyalty + is_holiday + is_summer + Month + Year + cluster,
              data = data)
summary(model.0) # 0.6138 
# the model is significative, with a good R2

model.1 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                Sconto.Solo.Loyalty + is_holiday + is_summer + Month + Year + cluster,
              data = data)
summary(model.1) # 0.6128

model.2 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_holiday + is_summer + Month + Year + cluster,
              data = data)
summary(model.2) # 0.6116

model.3 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_holiday + is_summer + Year + cluster,
              data = data)
summary(model.3) # 0.6104

model.4 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + Year + cluster,
              data = data)
summary(model.4) # 0.6083

model.5 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + cluster,
              data = data) # 0.601
summary(model.5) 


model.6 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                cluster,
              data = data) # 0.585
summary(model.6) 

model.7 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                cluster,
              data = data) # 0.5664
summary(model.7) 

# select model.5 (it seems the most complete to me)


# Model Diagnostic -------------------------------------------------------------------

par(mfrow = c(2,2))
plot(model.5)

ad.test(residuals(model.5))

# proviamo ad aggiungere una dummy sui volumi bassi, potrebbero essere quelli che
# peggiorano il fit
data$Volumi.bassi <- as.factor(ifelse(data$Vendite.in.Volume < 100, 1, 0))

model.5.1 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + cluster + Volumi.bassi,
              data = data) # 0.6609
summary(model.5.1) 

par(mfrow = c(2,2))
plot(model.5.1)
# il fit non è comunque bello ma R2 si è alzato

# proviamo a togliere queste osservazioni


# Regression without Outliers -------------------------------------------------------------------

data.no.out <- data[data$Vendite.in.Volume >= 100, ]

model.no.out <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + cluster,
              data = data.no.out)
summary(model.no.out) # 0.5397

par(mfrow = c(2,2))
plot(model.no.out) # omoschedasticità direi ok 


# PLM Regression -------------------------------------------------------------------

model.5.2 <- plm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + cluster + Volumi.bassi, 
                data = data, 
                index = c("Product"))
summary(model.5.2) # 0.65443
# cluster non lo prende in considerazione perchè è relativo a ogni Product 

residuals <- residuals(model.5.2)
fitted_values <- as.numeric(fitted(model.5.2))

par(mfrow = c(1, 2))

plot(fitted_values, residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)


# without outliers 

model.5.2.no.out <- plm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                   Sconto.Solo.Volantino + 
                   Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                   is_summer + cluster,  
                   data = data.no.out, 
                   index = c("Product"))
summary(model.5.2.no.out) # 0.40383

residuals <- residuals(model.5.2.no.out)
fitted_values <- as.numeric(fitted(model.5.2.no.out))

par(mfrow = c(1, 2))

plot(fitted_values, residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)


# Lag regression -------------------------------------------------------------------

fit.0 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
              Sconto.Solo.Volantino + 
              Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
              is_summer + cluster + Volumi.bassi + 
              Vendite.in.Volume.Settimana.Precedente.log, data = data)
summary(fit.0)
# non ha molto senso, ci viene un'interpolazione lineare


# Prediction -------------------------------------------------------------------

data <- data[order(data$Time), ]

split_point <- floor(nrow(data) * 0.8)

train_data <- data[1:split_point, ]
test_data <- data[(split_point + 1):nrow(data), ]

predictions <- predict(model.5.1, newdata = test_data)
actual <- test_data$Vendite.in.Volume.log

MAE <- mean(abs(predictions - actual)) # 0.721864 
MSE <- mean((predictions - actual)^2) # 1.070857 

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")

predictions <- predict(model.5.1, newdata = test_data, interval = "prediction")

results <- data.frame(
  Data = test_data$Time,
  Product = test_data$Product,
  Actual = test_data$Vendite.in.Volume.log,
  Predicted = predictions[, 1],  
  Lower_CI = predictions[, "lwr"],  
  Upper_CI = predictions[, "upr"] 
)


# prediction for Moretti 

result.moretti <- results[which(results$Product == 'Moretti 66 Cl'),]

ggplot(result.moretti, aes(x = Data)) +
  geom_line(aes(y = Actual), color = "blue") +
  geom_point(aes(y = Actual), color = "blue") +
  geom_line(aes(y = Predicted), color = "orange") +
  geom_point(aes(y = Predicted), color = "orange") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "orange") +
  scale_color_manual(values = c("predicted" = "orange", "actual" = "blue"), labels = c("Predicted", "Actual")) +
  scale_fill_manual(values = c("predicted" = "orange", "actual" = "blue"), labels = c("Predicted", "Actual")) +
  theme_minimal() +
  labs(x = "Time", y = "Volume Sales", title = "Prediction Intervals for Moretti") 

# for the legend
result.moretti_long <- result.moretti %>%
  pivot_longer(cols = c("Actual", "Predicted"), names_to = "variable", values_to = "value")
ggplot(result.moretti_long, aes(x = Data, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Time", y = "Volume Sales", title = "Prediction Intervals for Moretti", color = "Legend") +
  theme(legend.position = "right") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange"))


# Coefficients -------------------------------------------------------------------

model_summary <- tidy(model.5.1, conf.int = TRUE)

ggplot(model_summary, aes(y = term, x = estimate)) +
  geom_point(size = 1, col = 'red') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, lwd = 0.7, col = 'red') +
  theme_minimal() +
  labs(title = "95% CI for Beta",
       y = "Coefficient",
       x = "")


# LMM Product -----------------------------------------------------------------------------

boxplot(residuals(model.5.1) ~ data$Product, col = 'lightgreen')

fm1mer.0 <- lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                 Sconto.Solo.Volantino + 
                 Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                 is_summer + cluster + Volumi.bassi + (1|Product),
               data = data)

sigma2_eps <- as.numeric(get_variance_residual(fm1mer.0))
sigma2_eps # var of error
sigma2_b <- as.numeric(get_variance_random(fm1mer.0))
sigma2_b # var of random intercept
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.6851717

# visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm1mer.0, condVar=T))


fm1mer.1 <- lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                 Sconto.Solo.Volantino + 
                 Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                 is_summer + cluster + Volumi.bassi + (1 + Prezzo_Sconto.log|Product),
               data = data)

sigma2_eps <- as.numeric(get_variance_residual(fm1mer.1))
sigma2_eps # var of error
sigma2_b <- as.numeric(get_variance_random(fm1mer.1))
sigma2_b # var of random intercept
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.8935421

# visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm1mer.1, condVar=T))

anova(fm1mer.0, fm1mer.1) 


fm1mer.2 <- lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                   Sconto.Solo.Volantino + 
                   Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                   is_summer + cluster + Volumi.bassi + 
                   (1 + Prezzo_Sconto.log + Prezzo_NoSconto|Product),
                 data = data)

sigma2_eps <- as.numeric(get_variance_residual(fm1mer.2))
sigma2_eps # var of error
sigma2_b <- as.numeric(get_variance_random(fm1mer.2))
sigma2_b # var of random intercept
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.9197901

# visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm1mer.2, condVar=T))

anova(fm1mer.1, fm1mer.2)

# Diagnostic
# 1) Assessing Assumption on the within-group errors
plot(fm1mer.2, col = 'black')  ## Pearson and raw residuals are the same now
qqnorm(resid(fm1mer.2))
qqline(resid(fm1mer.2), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(fm1mer.2)$Product), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm1mer.2)$Product), col='red', lwd=2)


# Prediction on test set
data <- data[order(data$Time), ]
split_point <- floor(nrow(data) * 0.8)
train_data <- data[1:split_point, ]
test_data <- data[(split_point + 1):nrow(data), ]
predictions <- predict(fm1mer.2, newdata = test_data)
actual <- test_data$Vendite.in.Volume.log

MAE <- mean(abs(predictions - actual)) # 0.4654202 
MSE <- mean((predictions - actual)^2) # 0.5529891 

pred_intervals <- predictInterval(fm1mer.2, newdata = test_data, n.sims = 1000, level = 0.95)

results <- data.frame(
  Data = test_data$Time,
  Product = test_data$Product,
  Actual = test_data$Vendite.in.Volume.log,
  Predicted = predictions,  
  Lower_CI = pred_intervals[, "lwr"],  
  Upper_CI = pred_intervals[, "upr"] 
)


# prediction for Moretti 

result.moretti <- results[which(results$Product == 'Moretti 66 Cl'),]

ggplot(result.moretti, aes(x = Data)) +
  geom_line(aes(y = Actual), color = "blue") +
  geom_point(aes(y = Actual), color = "blue") +
  geom_line(aes(y = Predicted), color = "orange") +
  geom_point(aes(y = Predicted), color = "orange") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "orange") +
  scale_color_manual(values = c("predicted" = "orange", "actual" = "blue"), labels = c("Predicted", "Actual")) +
  scale_fill_manual(values = c("predicted" = "orange", "actual" = "blue"), labels = c("Predicted", "Actual")) +
  theme_minimal() +
  labs(x = "Time", y = "Volume Sales", title = "Prediction Intervals for Moretti - LMM Product") +
  theme(legend.position = "bottom")


# LMM Brand -----------------------------------------------------------------------------

boxplot(residuals(model.5.1) ~ data$Brand, col = 'lightgreen')

fm2mer.0 <- lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                 Sconto.Solo.Volantino + 
                 Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                 is_summer + cluster + Volumi.bassi + (1|Brand),
               data = data)

sigma2_eps <- as.numeric(get_variance_residual(fm2mer.0))
sigma2_eps # var of error
sigma2_b <- as.numeric(get_variance_random(fm2mer.0))
sigma2_b # var of random intercept
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.6471791

# visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm2mer.0, condVar=T))


fm2mer.1 <- lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                 Sconto.Solo.Volantino + 
                 Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                 is_summer + cluster + Volumi.bassi + (1 + Prezzo_Sconto.log|Brand),
               data = data)

sigma2_eps <- as.numeric(get_variance_residual(fm2mer.1))
sigma2_eps # var of error
sigma2_b <- as.numeric(get_variance_random(fm2mer.1))
sigma2_b # var of random intercept
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.906462

# visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm2mer.1, condVar=T))

anova(fm2mer.0, fm2mer.1) # -> fm2mer.1


fm2mer.2 <- lmer(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                   Sconto.Solo.Volantino + 
                   Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                   is_summer + cluster + Volumi.bassi + 
                   (1 + Prezzo_Sconto.log + Prezzo_NoSconto|Brand),
                 data = data)

sigma2_eps <- as.numeric(get_variance_residual(fm2mer.2))
sigma2_eps # var of error
sigma2_b <- as.numeric(get_variance_random(fm2mer.2))
sigma2_b # var of random intercept
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.9278132

# visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm2mer.2, condVar=T))

anova(fm2mer.1, fm2mer.2) # -> fm2mer.2

# Diagnostic
# 1) Assessing Assumption on the within-group errors
plot(fm2mer.2, col = 'black')  ## Pearson and raw residuals are the same now
qqnorm(resid(fm2mer.2))
qqline(resid(fm2mer.2), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(fm2mer.2)$Brand), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm2mer.2)$Brand), col='red', lwd=2)


# Prediction on test set
predictions <- predict(fm2mer.2, newdata = test_data)
actual <- test_data$Vendite.in.Volume.log

MAE <- mean(abs(predictions - actual)) # 0.5439345 
MSE <- mean((predictions - actual)^2) # 0.6940848 

pred_intervals <- predictInterval(fm2mer.2, newdata = test_data, n.sims = 1000, level = 0.95)

results <- data.frame(
  Data = test_data$Time,
  Product = test_data$Product,
  Actual = test_data$Vendite.in.Volume.log,
  Predicted = predictions,  
  Lower_CI = pred_intervals[, "lwr"],  
  Upper_CI = pred_intervals[, "upr"] 
)


# prediction for Moretti 

result.moretti <- results[which(results$Product == 'Moretti 66 Cl'),]

ggplot(result.moretti, aes(x = Data)) +
  geom_line(aes(y = Actual), color = "blue") +
  geom_point(aes(y = Actual), color = "blue") +
  geom_line(aes(y = Predicted), color = "orange") +
  geom_point(aes(y = Predicted), color = "orange") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "orange") +
  scale_color_manual(values = c("predicted" = "orange", "actual" = "blue"), labels = c("Predicted", "Actual")) +
  scale_fill_manual(values = c("predicted" = "orange", "actual" = "blue"), labels = c("Predicted", "Actual")) +
  theme_minimal() +
  labs(x = "Time", y = "Volume Sales", title = "Prediction Intervals for Moretti - LMM Brand") +
  theme(legend.position = "bottom")


# Final comparison between LMM Product, LMM Brand & LM -------------------------------

anova(fm1mer.2, fm2mer.2)
# fm1mer.2 in better

AIC(model.5.1) 
