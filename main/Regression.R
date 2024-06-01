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


 
model.5 <- plm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + Year, data = data, index = c("Product"))
summary(model.5) 
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


# Lag regression for Volume Sales -------------------------------------------------------------------

model.4 <- lm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                Sconto.Solo.Volantino + 
                Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                is_summer + Year + Vendite.in.Volume.Settimana.Precedente.log, data = data)
summary(model.4)

model.5 <- plm(Vendite.in.Volume.log ~ Prezzo_NoSconto + Prezzo_Sconto.log +
                 Sconto.Solo.Volantino + 
                 Sconto.Solo.Display + Sconto.Solo.Riduzione.Prezzo + 
                 is_summer + Year, data = data, index = c("Product"))
summary(model.5) 
