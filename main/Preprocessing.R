# Libraries -------------------------------------------------------------------
library(dplyr)
library(zoo)
library(ggplot2)

data <- read.csv('./Datasets/birre.csv')
data <- data[which(data$Geography != "The maximum allowed number of stacked Report that can be exported is 400. The exported File does not include the complete CSV. "),]

str(data)
colnames(data)

# Data top 20 products --------------------------------------------------------

# calcolo le vendite totali per ogni prodotto
vendite_totali <- aggregate(Vendite.in.Volume ~ Product, data = data, sum)

# ordino i prodotti in base alle vendite totali
birre_ordinate <- vendite_totali[order(vendite_totali$Vendite.in.Volume, decreasing = TRUE), ]

# primi 20 prodotti
top_20_birre <- head(birre_ordinate, 20)

# prendo la time series 
time_series_top_20 <- lapply(top_20_birre$Product, function(birra) {
  subset(data, Product == birra)
})

time_series_top_20_df <- do.call(rbind, time_series_top_20)

super <- time_series_top_20[[1]][which(time_series_top_20[[1]]$Geography == 'Supermercati (7012)'),]
iper <- time_series_top_20[[1]][which(time_series_top_20[[1]]$Geography == 'Ipermercati (7011)'),]


# prendo solo i dati degli ipermercati perchè quelli dei super arrivano fino al 2021
time_series_ipermercati <- subset(time_series_top_20_df, Geography == "Ipermercati (7011)")
# mancano delle settimane

settimane_per_birra <- aggregate(Time ~ Product, data = time_series_ipermercati, function(x) length(unique(x)))
numero_totale_settimane <- length(unique(time_series_ipermercati$Time))
birre_con_settimane_mancanti <- settimane_per_birra[settimane_per_birra$Time < numero_totale_settimane, ]
print(birre_con_settimane_mancanti)

settimane_complete <- unique(time_series_ipermercati$Time)
birre_complete <- unique(time_series_ipermercati$Product)
# tutte le combinazioni di birra e settimana
tutte_combinazioni <- expand.grid(Product = birre_complete, Time = settimane_complete)
dataset_completo <- merge(tutte_combinazioni, time_series_ipermercati, by = c("Product", "Time"), all.x = TRUE)

dati_mancanti1 <- subset(dataset_completo, is.na(Vendite.in.Volume))
# mancano i dati per la peroni nastro azzurro
# proposta: teniamo questi dati NA, il problema è che sono in mezzo al dataset 
# quindi avremmo dei buchi nelle time series

dati_mancanti2 <- subset(dataset_completo, is.na(Vendite.in.Volume.Con.promozioni))
# nel caso in cui c'è NA nelle Vendite.in.Valore.Con.promozioni è perchè non ci sono 
# state promozioni -> qua mettiamo zero 
dataset_completo$Vendite.in.Valore.Con.promozioni <- ifelse(dataset_completo$Vendite.in.Valore == dataset_completo$Vendite.in.Valore.Senza.promozioni, 0, dataset_completo$Vendite.in.Valore.Con.promozioni)
dataset_completo$Vendite.in.Volume.Con.promozioni <- ifelse(dataset_completo$Vendite.in.Volume == dataset_completo$Vendite.in.Volume.Senza.promozioni, 0, dataset_completo$Vendite.in.Volume.Con.promozioni)

# le settimane in cui ci mancano i dati sono le ultime 3 e poi 3 in mezzo
# proposta: togliamo quelle agli estremi mentre per quelle centrali facciamo una media 
# tra valore precedente e successivo

# trovo le settimane in cui mancano dati di vendita
settimane_con_dati_mancanti <- unique(dataset_completo$Time[is.na(dataset_completo$Vendite.in.Volume)])
settimane_con_dati_mancanti <- settimane_con_dati_mancanti[c(4,5,6)]
# Filtra il dataset completo escludendo le settimane con dati mancanti
dataset_senza_settimane_mancanti <- subset(dataset_completo, !(Time %in% settimane_con_dati_mancanti))

dati_mancanti3 <- subset(dataset_senza_settimane_mancanti, is.na(Vendite.in.Volume))

# interpolazione per le 3 settimane in mezzo
dataset_senza_settimane_mancanti$Vendite.in.Valore <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Valore)
dataset_senza_settimane_mancanti$Vendite.in.Valore.Senza.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Valore.Senza.promozioni)
dataset_senza_settimane_mancanti$Vendite.in.Valore.Con.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Valore.Con.promozioni)

dataset_senza_settimane_mancanti$Vendite.in.Volume <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Volume)
dataset_senza_settimane_mancanti$Vendite.in.Volume.Senza.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Volume.Senza.promozioni)
dataset_senza_settimane_mancanti$Vendite.in.Volume.Con.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Volume.Con.promozioni)


# Tolgo le variabili che non ci interessano
colnames(dataset_senza_settimane_mancanti)
dataset_official <- dataset_senza_settimane_mancanti

# cambio nome colonna Time
dataset_official$Time <- sub("Settimana al ", "", dataset_official$Time)
dataset_official$Time <- as.Date(dataset_official$Time, format = "%d-%m-%Y")

# riempio gli NA della Peroni
dataset_official$Geography <- ifelse(is.na(dataset_official$Geography), "Ipermercati (7011)", dataset_official$Geography)
dataset_official$VENDOR_Name....VENDOR_TSVM1.. <- ifelse(is.na(dataset_official$VENDOR_Name....VENDOR_TSVM1..), "PERONI", dataset_official$VENDOR_Name....VENDOR_TSVM1..)
dataset_official$BRAND_Name....BRAND_TSVM1.. <- ifelse(is.na(dataset_official$BRAND_Name....BRAND_TSVM1..), "PERONI NASTRO AZZURRO", dataset_official$BRAND_Name....BRAND_TSVM1..)

# cambio nomi colonne
colnames(dataset_official)[colnames(dataset_official) == 'BRAND_Name....BRAND_TSVM1..'] <- 'Brand'
colnames(dataset_official)[colnames(dataset_official) == 'VENDOR_Name....VENDOR_TSVM1..'] <- 'Vendor'

# aggiungo colonna con % di sconto 
dataset_official$Sconto <- ifelse(dataset_official$Vendite.in.Volume.Con.promozioni == 0,
                                  0, 100 - (dataset_official$Vendite.in.Valore.Con.promozioni / dataset_official$Vendite.in.Volume.Con.promozioni) * 100 / (dataset_official$Vendite.in.Valore.Senza.promozioni / dataset_official$Vendite.in.Volume.Senza.promozioni))

# binary per determinare tipolgia di sconto

make_binary <- function(col) {
  ifelse(is.na(col), 0, 1)
}
sconto_columns <- names(dataset_official[, 10:40])
dataset_official[, sconto_columns] <- lapply(dataset_official[, sconto_columns], make_binary)

dataset_official <- dataset_official[, c(1:43, 75)]
dataset_official$Geography <- NULL
dataset_official$FR_CATEGORY_Name....CATEGORIA_TSVM1.. <- NULL
  
# Salvo file 
# write.csv(dataset_official, "./Datasets/top20_products.csv", row.names = FALSE)

