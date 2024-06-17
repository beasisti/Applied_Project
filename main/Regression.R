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
  arrange(Time) %>% # Assicurati che i dati siano ordinati per il tempo
  mutate(Vendite.in.Volume.Settimana.Precedente = lag(Vendite.in.Volume, default = 0))


# add cluster column
data.cluster <- read.csv('./Datasets/data_cluster.csv')
data <- merge(data, data.cluster[, c("Product", "cluster", "Time")], by = c("Product", "Time"), all.x = TRUE)
data$cluster <- as.factor(data$cluster)
# 2 is for leader, 1 for follower


# Correlation plot -------------------------------------------------------------------

numeric_vars <- data[, -c(17,18)] %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = 'color', type = 'full', 
         tl.col = "black", tl.cex = 0.8,
         title = "Matrice di Correlazione", mar = c(0, 0, 1, 0))

rm(numeric_vars)


# Take a look at data -------------------------------------------------------------------

ggplot(data, aes(x = Vendite.in.Volume)) + 
  geom_histogram(binwidth = 1000, fill = "grey", color = "skyblue") +
  labs(title = "Distribuzione delle Vendite", x = "Vendite in Valore", y = "Frequenza")

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

# non molto bene

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
ad.test(residuals(model.no.out)) # male ma vabbè guardiamo il qqplot e basta


# proviamo a togliere i leverage

leverage_values <- hatvalues(model.no.out)
high_leverage_points <- which(leverage_values > 0.02)

data.clean <- data.no.out[-high_leverage_points, ]

model.clean <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                     Sconto.Solo.Volantino + 
                     Sconto.Solo.Display + # Sconto.Solo.Riduzione.Prezzo + 
                     is_summer + cluster,
                   data = data.no.out)
summary(model.clean) # 0.5272

par(mfrow = c(2,2))
plot(model.clean) # ora ok
ad.test(residuals(model.clean)) # male ma vabbè guardiamo il qqplot e basta


# PLM Regression -------------------------------------------------------------------

model.5.2 <- plm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + cluster + Volumi.bassi, 
                data = data, 
                index = c("Product"))
summary(model.5.2) # 0.65443


residuals <- residuals(model.5.2)
fitted_values <- as.numeric(fitted(model.5.2))

par(mfrow = c(2, 2))

plot(fitted_values, residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

plot(fitted_values, sqrt(abs(residuals)), main = "Scale-Location", xlab = "Fitted values", ylab = "Sqrt(|residuals|)")
abline(h = 0, col = "red")


# without outliers 

model.5.2.no.out <- plm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                   Sconto.Solo.Volantino + 
                   Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                   is_summer + cluster,  
                   data = data.no.out, 
                   index = c("Product"))
summary(model.5.2.no.out) # 0.40383


# Lag regression -------------------------------------------------------------------

fit.0 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
              Sconto.Solo.Volantino + 
              Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
              is_summer + cluster + Volumi.bassi + 
              Vendite.in.Volume.Settimana.Precedente.log, data = data)
summary(fit.0)


# Prediction -------------------------------------------------------------------


y <- as.numeric(fitted(model.5))
res <- as.numeric(residuals(model.5))
plot(y,res)
 


# vanno sicuramente tolte delle osservazioni outlier / leverage
leverage <- hatvalues(model.4)
plot(leverage)
data.1 <- data[leverage <= 0.015, ]
# non riesco a fittare il modello model.4


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


