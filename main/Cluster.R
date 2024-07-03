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
library(dtwclust)
library(grid)


# Data -------------------------------------------------------------------

data <- read.csv('./Datasets/top20_products.csv')

dati <- data[, c(1,2,3,4,5,39)]
plot(dati[,5:6])

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
  mutate(Vendite.in.Volume.log = log(Vendite.in.Volume),
         Vendite.in.Valore.log = log(Vendite.in.Valore))


# K-means on aggregate data -------------------------------------------------------------------

data_aggregated <- data %>%
  group_by(Product) %>%
  summarise(Totale_Vendite_in_Valore = sum(Vendite.in.Valore, na.rm = TRUE),
            Totale_Vendite_in_Volume = sum(Vendite.in.Volume, na.rm = TRUE))
n <- dim(data_aggregated)[1]

k <- 2
result.k <- kmeans(data_aggregated[, -1], centers = k) 
result_clusters <- result.k$cluster        
result.k$centers 
centers <- as.data.frame(result.k$centers)

my_colors <- c("#FF69B4","#32CD32") 

point_colors <- c(my_colors[result.k$cluster], rep("black", times = k))

plot(rbind(data_aggregated[, -1], centers), 
     col = point_colors, 
     pch = c(rep(19, times=n), rep(4, times=k)), 
     cex = c(rep(1.25, times=n), rep(2, times=k)),
     lwd = c(rep(1, times=n), rep(2, times=k)),
     xlab = 'Total Value Sales', ylab = 'Total Volume Sales',
     title = 2)
high_sales <- data_aggregated[data_aggregated$Totale_Vendite_in_Valore > 3e+07, ]
text(high_sales[, -1], labels = high_sales$Product, pos = 2, cex = 0.8)

# we have identified 2 clusters, that we can interpret as high sales and low sales, 
# meaning that there exist two types of group in our products corresponding to those
# who are leaders in the market and those who are followers

data_aggregated <- cbind(data_aggregated, Cluster = factor(result_clusters))
data_aggregated$Cluster <- as.factor(ifelse(data_aggregated$Cluster == 1, 2, 1))
# Definire i colori per i punti dei dati e per i centroidi
my_colors <- c("#FF6347", "#00CED1")  # Colori per i cluster
centroid_color <- "black"              # Colore per i centroidi
cluster_labels <- c("Leader", "Follower")

plot <- ggplot(data = data_aggregated, aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume, color = Cluster)) +
  geom_point(shape = 19, size = 3) +  
  geom_point(data = centers, aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume), shape = 4, size = 4, stroke = 1.5, color = centroid_color) +  # Centroidi
  geom_point(data = data_aggregated[data_aggregated$Product == "Peroni 66 Cl", ], aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume), shape = 21, size = 3, stroke = 0.7, color = "black", fill = "#00CED1") +  # Punto con contorno per "Peroni 66 Cl"
  geom_text(data = high_sales, aes(label = Product), hjust = 1.15, vjust = 0, size = 3, color = "black") +  # Etichette per i prodotti con vendite elevate
  geom_text(data = data_aggregated[data_aggregated$Product == "Peroni 66 Cl", ], aes(label = Product), hjust = 1.15, vjust = 0, size = 3, color = "black", nudge_x = 10000, nudge_y = -10000) +  
  labs(x = "Value Sales", y = "Volume Sales", title = "Cluster on Total Sales of all Years") +
  scale_color_manual(values = my_colors, labels = cluster_labels) +  # Specificare i colori e le etichette per i cluster
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7), 
    panel.background = element_rect(fill = "transparent", color = NA),  # Sfondo trasparente
    plot.background = element_rect(fill = "transparent", color = NA),   # Sfondo trasparente per l'intera trama
    panel.grid.major = element_line(color = "#333333", linewidth = 0.2),  # Griglia maggiore grigia
    panel.grid.minor = element_line(color = "#333333", linewidth = 0.2)  # Griglia minore grigia tratteggiata
  )  
ggsave(
  filename = "./Plots/plot poster/clust.png", 
  plot = plot, 
  width = 8,    # Larghezza del grafico
  height = 5.5,    # Altezza del grafico
  units = "in",  # Unità di misura (può essere "in", "cm", o "mm")
  bg = "transparent"
)


# K-means on time series data -------------------------------------------------------------------

data_ts <- data %>%
  group_by(Product, Time) %>%
  summarise(Totale_Vendite_in_Volume = sum(Vendite.in.Volume, na.rm = TRUE)) %>%
  pivot_wider(names_from = Time, values_from = Totale_Vendite_in_Volume)

data_ts <- data_ts[complete.cases(data_ts), ]

set.seed(123)
dba_cluster <- tsclust(data_ts[,-1], type = "partitional", k = 2, distance = "Euclidean")
cluster_labels <- dba_cluster@cluster

data_ts$cluster <- as.factor(cluster_labels)

beer_data_transposed <- data_ts %>%
  gather(Date, Vendite.in.Volume, -Product, -cluster) %>%
  mutate(Date = as.Date(Date))

beer_data_transposed <- beer_data_transposed %>%
  arrange(Product, Date) %>%
  group_by(Product) %>%
  mutate(Vendite.in.Volume.log = log(Vendite.in.Volume)) %>%
  ungroup()

ggplot(beer_data_transposed, aes(x = Date, y = Vendite.in.Volume, color = cluster, group = Product)) +
  geom_line() +
  scale_color_manual(values = c('1' = '#FF6347', '2' = '#00CED1')) +
  labs(x = "Time", y = "Volume Sales", color = "Cluster") +
  theme_minimal()

# coloriamo la Peroni 66 Cl
beer_data_transposed <- beer_data_transposed %>%
  mutate(color = ifelse(Product == "Peroni 66 Cl", "#B22222", as.character(cluster)))
plot <- ggplot(beer_data_transposed, aes(x = Date, y = Vendite.in.Volume.log, color = cluster, group = Product)) +
  geom_line() +
  scale_color_manual(values = my_colors, labels = c('Leader', 'Follower')) +
  labs(x = "Time", y = "Volume Sales (log)", color = "Cluster", title = 'Cluster on Time-Series Sales') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7), 
    panel.background = element_rect(fill = "transparent", color = NA),  # Sfondo trasparente
    plot.background = element_rect(fill = "transparent", color = NA),   # Sfondo trasparente per l'intera trama
    panel.grid.major = element_line(color = "#333333", linewidth = 0.2),  # Griglia maggiore grigia
    panel.grid.minor = element_line(color = "#333333", linewidth = 0.2)  # Griglia minore grigia tratteggiata
  )  
ggsave(
  filename = "./Plots/plot poster/clust_ts.png", 
  plot = plot, 
  width = 10,    # Larghezza del grafico
  height = 5,    # Altezza del grafico
  units = "in",  # Unità di misura (può essere "in", "cm", o "mm")
  bg = "transparent"
)



# log scale
ggplot(beer_data_transposed, aes(x = Date, y = Vendite.in.Volume.log, color = cluster, group = Product)) +
  geom_line() +
  scale_color_manual(values = c('1' = '#FF00FF', '2' = '#FFFF00')) +
  labs(x = "Time", y = "Volume Sales", color = "Cluster") +
  theme_minimal()

# qua risulta anche la Peroni tra i leader, probabilmente questo è dovuto al fatto che 
# overall (quindi a livello di time series) risulta tra le leader di mercato, mentre
# quando andiamo ad aggregare i dati ci sono delle osservazioni outlier (alcune settimane
# le vendite sono di 0.66 L, un po' strano) che vanno ad influire sul cluster


# K-means on aggregate data per year -------------------------------------------------------------------

data$Time <- ymd(data$Time)
data$Year <- year(data$Time)
data$Month <- month(data$Time)

get_custom_year <- function(date) {
  year <- year(date)
  month <- month(date)
  
    if (month == 1 || (month == 2 && day(date) < 15)) {
    return(year - 1)
  } else {
    return(year)
  }
}

data$Custom_Year <- sapply(data$Time, get_custom_year)
custom_years <- unique(data$Custom_Year)

final_clusters <- data.frame(Product = unique(data$Product))

# par(mfrow = c(2, 3))
year_counter <- 1  # Initialize the counter

for (custom_year in custom_years) {
  custom_year = '2023'
  data_year <- data %>%
    filter(Custom_Year == custom_year) %>%
    group_by(Product) %>%
    summarise(Totale_Vendite_in_Valore = sum(Vendite.in.Valore, na.rm = TRUE),
              Totale_Vendite_in_Volume = sum(Vendite.in.Volume, na.rm = TRUE))
  
  n <- dim(data_year)[1]
  k <- 2
  result.k <- kmeans(data_year[, -1], centers = k)
  
  data_year$cluster <- result.k$cluster
  result_clusters <- result.k$cluster
  cluster_totals <- data_year %>%
    group_by(cluster) %>%
    summarise(Totale_Vendite_in_Valore = sum(Totale_Vendite_in_Valore))
  
  cluster_with_highest_sales <- cluster_totals$cluster[which.max(cluster_totals$Totale_Vendite_in_Valore)]
  data_year$cluster <- ifelse(data_year$cluster == cluster_with_highest_sales, 2, 1)
  
  col_name <- paste("cluster", custom_year, sep = ".")
  cluster_data <- data_year %>% dplyr::select(Product, cluster) %>% rename(!!col_name := cluster)
  final_clusters <- merge(final_clusters, cluster_data, by = "Product", all = TRUE)
  
  centers <- as.data.frame(result.k$centers)
  colors <- c("purple", "yellow")
  
  plot(rbind(data_year[, c(2:3)], centers), 
       col = c(colors[data_year$cluster], rep('black', times = k)), 
       pch = c(rep(19, times = n), rep(4, times = k)), 
       cex = c(rep(1, times = n), rep(2, times = k)),
       lwd = c(rep(1, times = n), rep(2, times = k)),
       xlab = 'Total Value Sales', ylab = 'Total Volume Sales',
       main = paste('Clustering of Year', year_counter), # or custom_year, depending on what we want 
       xlim = c(0, max(data_year$Totale_Vendite_in_Valore) + 2e06))
  
  cluster_2_products <- data_year[data_year$cluster == 1, ]
  text(cluster_2_products[, -1], labels = cluster_2_products$Product, pos = 1, cex = 0.8)
  
  year_counter <- year_counter + 1  
  
  data_year1 <- cbind(data_year, Cluster = factor(result_clusters))
  data_year1$Cluster <- as.factor(ifelse(data_year1$Cluster == 1, 2, 1))
  my_colors <- c("#FF6347", "#00CED1")  
  centroid_color <- "black"              
  cluster_labels <- c("Leader", "Follower")
  
  plot <- ggplot(data = data_year1, aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume, color = Cluster)) +
     geom_point(shape = 19, size = 3) +  
     geom_point(data = centers, aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume), shape = 4, size = 4, stroke = 1.5, color = centroid_color) +  # Centroidi
     geom_point(data = data_year1[data_year1$Product == "Peroni 66 Cl", ], aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume), shape = 21, size = 3, stroke = 0.7, color = "black", fill = "#00CED1") +  # Punto con contorno per "Peroni 66 Cl"
     geom_point(data = data_year1[data_year1$Product == "Heineken 66 Cl", ], aes(x = Totale_Vendite_in_Valore, y = Totale_Vendite_in_Volume), shape = 21, size = 3, stroke = 0.7, color = "black", fill = "#00CED1") +  # Punto con contorno per "Peroni 66 Cl"
     geom_text(data = data_year1[data_year1$Product == "Ichnusa non filtrata 50 Cl", ], aes(label = Product), hjust = 0.25, vjust = -1.5, size = 3, color = "black", nudge_x = 10000, nudge_y = -10000) +  # Etichetta per il prodotto "Ichnusa"
     geom_text(data = data_year1[data_year1$Product == "Peroni 66 Cl", ], aes(label = Product), hjust = 0.25, vjust = 1.75, size = 3, color = "black", nudge_x = 10000, nudge_y = -10000) +  
     geom_text(data = data_year1[data_year1$Product == "Heineken 66 Cl", ], aes(label = Product), hjust = 1.15, vjust = -0.15, size = 3, color = "black", nudge_x = 10000, nudge_y = -10000) +  
     geom_text(data = data_year1[data_year1$Cluster == 1 & data_year1$Product != "Ichnusa non filtrata 50 Cl", ], aes(label = Product), hjust = 1.15, vjust = -0.15, size = 3, color = "black") +  # Altre etichette per i prodotti con vendite elevate
     labs(x = "Value Sales", y = "Volume Sales", title = "Cluster on Total Sales of Year 5") +
     scale_color_manual(values = my_colors, labels = cluster_labels) +  # Specificare i colori e le etichette per i cluster
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
       axis.title.x = element_text(size = 14),
       axis.title.y = element_text(size = 14),
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 7), 
       panel.background = element_rect(fill = "transparent", color = NA),  # Sfondo trasparente
       plot.background = element_rect(fill = "transparent", color = NA),   # Sfondo trasparente per l'intera trama
       panel.grid.major = element_line(color = "#333333", linewidth = 0.2),  # Griglia maggiore grigia
       panel.grid.minor = element_line(color = "#333333", linewidth = 0.2)  # Griglia minore grigia tratteggiata
     )  
  ggsave(
    filename = "./Plots/plot poster/clu5.png", 
    plot = plot, 
    width = 8,    # Larghezza del grafico
    height = 5.5,    # Altezza del grafico
    units = "in",  # Unità di misura (può essere "in", "cm", o "mm")
    bg = "transparent"
  )
    
}

# interessante, nella regressione però direi di usare il risultato sui dati aggregati
# su tutti gli anni


# Adding cluster to original data -------------------------------------------------------------------

data_aggregated$cluster <- clusters
df_merged <- merge(data, data_aggregated[,c(1,4)], by = "Product", all.x = TRUE)

# save it for regression
# write.csv(df_merged, "./Datasets/data_cluster.csv", row.names = FALSE)

