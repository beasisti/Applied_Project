# Libraries -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(MASS)


data <- read.csv('top20_products.csv')

data$Time <- ymd(data$Time)
data$Year <- year(data$Time)
data$Month <- month(data$Time)

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
data <- data %>%
  mutate_at(vars(8:38), as.factor)

# Analisi esplorativa  -------------------------------------------------------------------

# summary delle variabili numeriche
summary(data[,c(5,6,7,39,40,41,42)])


max.vendite <- data %>% # corrisponde a max.promo
  filter(Vendite.in.Volume == max(Vendite.in.Volume))
max.vendite$Sconto # 24%

max.no.promo <- data %>%
  filter(Vendite.in.Volume.Senza.promozioni == max(Vendite.in.Volume.Senza.promozioni))

max.guadagno <- data %>% # corrisponde a max.vendite 
  filter(Vendite.in.Valore == max(Vendite.in.Valore))

max.guadagno.no.promo <- data %>%
  filter(Vendite.in.Valore.Senza.promozioni == max(Vendite.in.Valore.Senza.promozioni))

max.sconto <- data %>%
  filter(Sconto == max(Sconto)) # 50% 


min.vendite <- data %>% # corrisponde al min per tutte le altre covariate
  filter(Vendite.in.Volume == min(Vendite.in.Volume))
min.vendite$Sconto # 0 sconto
time.min <- min.vendite %>%
  arrange(Time)
time.min <- unique(time.min$Time)

sales <- data %>%
  filter(Time >= time.min[1] & Time <= time.min[8]) 

ggplot(sales, aes(x = Time, y = Vendite.in.Volume, color = Product)) + # possiamo colorare in base a Brand / Vendor
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(title = "Vendite con Promozioni nel Tempo",
       x = "Settimana",
       y = "Vendite Totali") +
  theme_minimal()


ggplot(sales, aes(x = Time, y = Vendite.in.Volume, color = Product)) + 
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Sales with Promotions Over Time",
    x = "Week",
    y = "Total Sales"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12),     
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)   
  )

ggplot(sales, aes(x = Time, y = Vendite.in.Volume, color = Product)) + 
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Sales with Promotions Over Time by Vendor",
    x = "Week",
    y = "Total Sales"
  ) +
  facet_grid(Vendor ~ .) +  # Raggruppa i grafici per Vendor in colonne
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


filtered_data <- sales %>%
  group_by(Product) %>%
  mutate(has_high_sales = any(Vendite.in.Volume > 100000)) %>%
  ungroup()

ggplot(filtered_data, aes(x = Time, y = Vendite.in.Volume, group = Product, color = has_high_sales)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +
  labs(title = "Vendite di Birra",
       x = "Data",
       y = "Vendite") +
  theme_minimal() +
  theme(legend.position = "none")

min <- data %>%
  filter(Sconto == min(Sconto)) # -23%


# Time series plots  -------------------------------------------------------------------

ggplot(data, aes(x = Time, y = Vendite.in.Volume.Con.promozioni, color = Product)) + 
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Sales with Promotions Over Time",
    x = "Week",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12),     
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)   
  )

ggplot(data, aes(x = Time, y = Vendite.in.Volume.Senza.promozioni, color = Product)) +
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Sales without Promotions Over Time",
    x = "Week",
    y = "Sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12),     
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)    
  )

ggplot(data, aes(x = Time, y = Vendite.in.Volume, color = Product)) +
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Beer Sales Over Time",
    x = "Week",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12),     
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)   
  )

ggplot(data, aes(x = Time, y = Sconto, color = Product)) +
  geom_point(size = 0.2) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Discounts Over Time",
    x = "Week",
    y = "% Discount"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12),     
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)   
  )


filtered_data1 <- data %>%
  group_by(Product) %>%
  mutate(has_high_sales = any(Vendite.in.Volume.Senza.promozioni > 50000)) %>%
  ungroup()

ggplot(filtered_data1, aes(x = Time, y = Vendite.in.Volume.Senza.promozioni, group = Product)) +
  geom_line(aes(color = ifelse(has_high_sales, Product, "Other")), size = 0.75) +
  scale_color_manual(values = c("Moretti 66 Cl" = "blue", "Peroni 66 Cl" = "red", "Ichnusa non filtrata 50 Cl" = "green", "Moretti 66 Cl x 6" = "orange", "Other" = "grey")) +
  labs(title = "Serie Temporali delle Vendite di Birra",
       x = "Data",
       y = "Vendite",
       color = "Prodotto") +
  theme_minimal()


# Boxplots  -------------------------------------------------------------------

ggplot(data, aes(x = factor(Month), y = Vendite.in.Volume)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Monthly Sales Distribution",
    x = "Month",
    y = "Sales Volume"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = month.abb) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = factor(Month), y = Vendite.in.Volume.Senza.promozioni)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Monthly Sales Distribution Without Promotions",
    x = "Month",
    y = "Sales Volume"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = month.abb) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = factor(Month), y = Vendite.in.Volume.Con.promozioni)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Monthly Sales Distribution With Promotions",
    x = "Month",
    y = "Sales Volume"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = month.abb) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = factor(Month), y = Sconto)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Monthly Discount Distribution",
    x = "Month",
    y = "Discount (%)"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = month.abb)

# Istogramma delle vendite
ggplot(data, aes(x = Vendite.in.Valore)) + 
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "darkgrey") +
  labs(
    title = "Sales Distribution",
    x = "Sales Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = Vendite.in.Valore.Senza.promozioni)) + 
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "darkgrey") +
  labs(
    title = "Sales Distribution Without Promotions",
    x = "Sales Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = Vendite.in.Valore.Con.promozioni)) + 
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "darkgrey") +
  labs(
    title = "Sales Distribution With Promotions",
    x = "Sales Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = Vendite.in.Volume)) + 
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "darkgrey") +
  labs(
    title = "Sales Distribution",
    x = "Sales Volume",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = Vendite.in.Volume.Senza.promozioni)) + 
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "darkgrey") +
  labs(
    title = "Sales Distribution Without Promotions",
    x = "Sales Volume",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12)
  )

ggplot(data, aes(x = Vendite.in.Volume.Con.promozioni)) + 
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "darkgrey") +
  labs(
    title = "Sales Distribution With Promotions",
    x = "Sales Volume",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12)
  )

