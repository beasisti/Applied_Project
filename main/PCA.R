#per farlo girare prima devi runnare il preprocessing
#library



datamoretti <- dataset_senza_settimane_mancanti[1:5140,7:74]

datamoretti[is.na(datamoretti)] <- 0

# Standardizza tutte le variabili numeriche del dataset pulito
datamoretti_scaled <- scale(datamoretti)

# Converte il risultato in un data frame
datamoretti_scaled <- as.data.frame(datamoretti)

# Applica PCA
pca_result_scaled <- princomp(datamoretti_scaled, center = TRUE, scale. = TRUE)

# Visualizza un riassunto dei risultati
summary(pca_result_scaled)
loadings(pca_result_scaled)

# Ottieni le prime due componenti principali
principal_components <- pca_result_scaled$scores[, 1:2]

# Converti in un data frame per ulteriori analisi
principal_components_df <- as.data.frame(principal_components)
colnames(principal_components_df) <- c("PC1", "PC2")
