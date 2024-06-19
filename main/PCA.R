library(dplyr)
library(zoo)

data <- read.csv('birre.csv')
data <- birre
data <- data[which(data$Geography != "The maximum allowed number of stacked Report that can be exported is 400. The exported File does not include the complete CSV. "),]

str(data)
colnames(data)


# Calcola le vendite totali per ogni birra
vendite_totali <- aggregate(Vendite.in.Volume ~ Product, data = data, sum)

# Ordina le birre in base alle vendite totali
birre_ordinate <- vendite_totali[order(vendite_totali$Vendite.in.Volume, decreasing = TRUE), ]

# Seleziona le prime 20 birre
top_20_birre <- head(birre_ordinate, 20)
top_30_birre <- head(birre_ordinate, 30)

# prendo la time series 
time_series_top_20 <- lapply(top_20_birre$Product, function(birra) {
  subset(data, Product == birra)
})

time_series_top_20_df <- do.call(rbind, time_series_top_20)

super <- time_series_top_20[[1]][which(time_series_top_20[[1]]$Geography == 'Supermercati (7012)'),]
iper <- time_series_top_20[[1]][which(time_series_top_20[[1]]$Geography == 'Ipermercati (7011)'),]


# prendiamo solo i dati degli ipermercati perchè quelli dei super arrivano fino al 2021
time_series_ipermercati <- subset(time_series_top_20_df, Geography == "Ipermercati (7011)")
# mancano delle settimane

# Conta il numero di settimane uniche per ciascuna birra
settimane_per_birra <- aggregate(Time ~ Product, data = time_series_ipermercati, function(x) length(unique(x)))

# Calcola il numero totale di settimane possibili
numero_totale_settimane <- length(unique(time_series_ipermercati$Time))

# Stampa le birre che hanno meno settimane rispetto al totale
birre_con_settimane_mancanti <- settimane_per_birra[settimane_per_birra$Time < numero_totale_settimane, ]
print(birre_con_settimane_mancanti)


# Trova l'elenco completo delle settimane
settimane_complete <- unique(time_series_ipermercati$Time)

# Trova l'elenco completo delle birre
birre_complete <- unique(time_series_ipermercati$Product)

# Crea un dataframe contenente tutte le combinazioni di birra e settimana
tutte_combinazioni <- expand.grid(Product = birre_complete, Time = settimane_complete)

# Unisci il dataframe contenente tutte le combinazioni con il dataset originale
dataset_completo <- merge(tutte_combinazioni, time_series_ipermercati, by = c("Product", "Time"), all.x = TRUE)

dati_mancanti1 <- subset(dataset_completo, is.na(Vendite.in.Volume))
# mancano i dati per la peroni nastro azzurro
# -> proposta: teniamo questi dati NA, il problema è che sono in mezzo al dataset quindi avremmo dei buchi nelle time series

dati_mancanti2 <- subset(dataset_completo, is.na(Vendite.in.Volume.Con.promozioni))

# nel caso in cui c'è NA nelle Vendite.in.Valore.Con.promozioni è perchè non ci sono 
# state promozioni -> qua mettiamo zero 
dataset_completo$Vendite.in.Valore.Con.promozioni <- ifelse(dataset_completo$Vendite.in.Valore == dataset_completo$Vendite.in.Valore.Senza.promozioni, 0, dataset_completo$Vendite.in.Valore.Con.promozioni)
dataset_completo$Vendite.in.Volume.Con.promozioni <- ifelse(dataset_completo$Vendite.in.Volume == dataset_completo$Vendite.in.Volume.Senza.promozioni, 0, dataset_completo$Vendite.in.Volume.Con.promozioni)


# ora concentriamoci sugli altri NA
# le settimane in cui ci mancano i dati sono le ultime 3 e poi 3 in mezzo
# proposta: togliamo quelle agli estremi mentre per quelle centrali facciamo una media 
# tra valore precedente e successivo

# Trova le settimane in cui mancano dati di vendita
settimane_con_dati_mancanti <- unique(dataset_completo$Time[is.na(dataset_completo$Vendite.in.Volume)])
settimane_con_dati_mancanti <- settimane_con_dati_mancanti[c(4,5,6)]
# Filtra il dataset completo escludendo le settimane con dati mancanti
dataset_senza_settimane_mancanti <- subset(dataset_completo, !(Time %in% settimane_con_dati_mancanti))

dati_mancanti3 <- subset(dataset_senza_settimane_mancanti, is.na(Vendite.in.Volume))

# Media per le 3 in mezzo
dataset_senza_settimane_mancanti$Vendite.in.Valore <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Valore)
dataset_senza_settimane_mancanti$Vendite.in.Valore.Senza.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Valore.Senza.promozioni)
dataset_senza_settimane_mancanti$Vendite.in.Valore.Con.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Valore.Con.promozioni)

dataset_senza_settimane_mancanti$Vendite.in.Volume <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Volume)
dataset_senza_settimane_mancanti$Vendite.in.Volume.Senza.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Volume.Senza.promozioni)
dataset_senza_settimane_mancanti$Vendite.in.Volume.Con.promozioni <- na.approx(dataset_senza_settimane_mancanti$Vendite.in.Volume.Con.promozioni)


# Tolgo le variabili che non ci interessano
colnames(dataset_senza_settimane_mancanti)
dataset_official <- dataset_senza_settimane_mancanti[, c(1,2,3,5,6,7,8,9,41,42,43)]

# Cambio nome colonna Time
dataset_official$Time <- sub("Settimana al ", "", dataset_official$Time)
dataset_official$Time <- as.Date(dataset_official$Time, format = "%d-%m-%Y")

dataset_official$Geography <- ifelse(is.na(dataset_official$Geography), "Ipermercati (7011)", dataset_official$Geography)
dataset_official$VENDOR_Name....VENDOR_TSVM1.. <- ifelse(is.na(dataset_official$VENDOR_Name....VENDOR_TSVM1..), "PERONI", dataset_official$VENDOR_Name....VENDOR_TSVM1..)
dataset_official$BRAND_Name....BRAND_TSVM1.. <- ifelse(is.na(dataset_official$BRAND_Name....BRAND_TSVM1..), "PERONI NASTRO AZZURRO", dataset_official$BRAND_Name....BRAND_TSVM1..)

# Salvo file 
write.csv(dataset_official, "birre_da_usare.csv", row.names = FALSE)

########################## PCA SOLO MORETTI #################################

datamoretti <- dataset_senza_settimane_mancanti[1:5140,7:74]

datamoretti[is.na(datamoretti)] <- 0

# Standardizza tutte le variabili numeriche del dataset pulito
datamoretti_scaled <- scale(datamoretti)

# Converte il risultato in un data frame
datamoretti_scaled <- as.data.frame(datamoretti)

boxplot(scale(x = datamoretti_scaled, center = T, scale = F), las = 2, col = 'gold')

# Applica PCA
pca_result_scaled <- princomp(datamoretti_scaled, center = TRUE, scale. = TRUE)

# Visualizza un riassunto dei risultati
summary(pca_result_scaled)
loadings(pca_result_scaled)

# Ottieni le prime due componenti principali
principal_components <- pca_result_scaled$scores[, 1:2]

plot(cumsum(pca_result_scaled$sd^2) / sum(pca_result_scaled$sd^2), type = 'b', axes = FALSE,
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.9, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(tourists), labels = 1:ncol(tourists), las = 2)

# Converti in un data frame per ulteriori analisi
principal_components_df <- as.data.frame(principal_components)
colnames(principal_components_df) <- c("PC1", "PC2")

################### PCA SU TUTTO ####################

# Lettura dataset e selezione solo delle covariate numeriche
dataset2 <- read.csv('birre_da_usare.csv')
data_su_pca <- dataset2[,6:11]

# check NA e Varianze delle colonne
total_na <- sum(is.na(data_su_pca))
variances <- apply(data_su_pca, 2, var, na.rm = TRUE)
zero_variance_columns <- which(variances == 0)
print(zero_variance_columns) # --> ok 

# Standardizza tutte le variabili numeriche del dataset pulito
data_pca_scaled <- scale(data_su_pca)

data_pca_scaled <- as.data.frame(data_pca_scaled)

# Visualizzazione varianza covariate
boxplot(scale(x = data_pca_scaled, center = T, scale = F), las = 2, col = 'gold')

# Applicazione PCA
pca_result_scaled_tot <- princomp(data_pca_scaled, scores=T)

# Visualizzazione dei risultati
summary(pca_result_scaled_tot)
loadings(pca_result_scaled_tot)

# Abbreviazione nomi colonne per plot (boh brutto così, forse da riscrivere)
colnames(data_pca_scaled) <- abbreviate(colnames(data_pca_scaled))

# Grafico a gomito varianza cumulata 
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
barplot(pca_result_scaled_tot$sdev^2, las = 2, main = 'Principal Components', ylim = c(0, 4), ylab = 'Variances',cex.axis = 0.7)
abline(h = 1, col = 'blue')
barplot(sapply(data_pca_scaled, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 4),
        ylab = 'Variances',cex.axis = 0.2)
plot(cumsum(pca_result_scaled_tot$sd^2) / sum(pca_result_scaled_tot$sd^2), type = 'b', axes = FALSE,
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
# soglia scelta a 0.9 ma ok anche a 0.8
abline(h = 0.9, lty = 2, col = 'blue')
# abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(tourists), labels = 1:ncol(tourists), las = 2)

# Dataset proiettato sulle componenti principali 
scores <- pca_result_scaled_tot$scores

# Loading scelti su una soglia significativa
# Considerando i primi due --> media, aumento vendite con promozione porta relativa diminuzione di quelle non con promozione (direi)
loads <- pca_result_scaled_tot$loadings
par(mar = c(1, 6, 0, 2), mfrow = c(6, 1))
for (i in 1:6) {
  barplot(ifelse(abs(loads[, i]) < 0.3, 0, loads[, i]),
          ylim = c(-1, 1))
  abline(h = 0)
}

# BIPLOT
par(mfrow = c(1, 1))
plot(scores[, 1], scores[, 2], type = "n", xlab = "PC1", ylab = "PC2", asp = 1,
     xlim = c(-4, 3))
text(scores[, 1], scores[, 2], dimnames(data_su_pca)[[1]], cex = 0.7)
biplot(pca_result_scaled_tot)

# Estrazione ultime due componenti principali
principal_components <- scores[, 1:2]

# Converti in un data frame per ulteriori analisi
principal_components_df <- as.data.frame(principal_components)
colnames(principal_components_df) <- c("PC1", "PC2")
