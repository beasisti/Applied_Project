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
library(MVN)
library(cowplot)


# mi interessa guardare: Vendite.in.Volume rispetto alla categorizzazione data da Product
#                        Sconto                                                   Brand
# facciamo due ANOVA separatamente per Brand e Product
# per giustificare quando facciamo LMM rispetto a Product e Brand (intercetta) e quando 
# aggiugiamo anche la random slope rispetto a Prezzo_Sconto e Prezzo_NoSconto 

# ANOVA per confrontare le verie tipologie di sconto su Vendite.in.Volume 


data <- read.csv('./Datasets/top20_products.csv')

data <- data %>%
  mutate(Vendite.in.Volume.log = log(Vendite.in.Volume))


# Two way MANOVA --------------------------------------------------------

factor1 <- factor(data$Brand) 
factor2 <- factor(data$Product) 
factor12 <- factor(paste(factor1, factor2, sep='-'))

x <- data[, c("Vendite.in.Volume.log", "Sconto")]
p <- dim(x)[2]
n <- dim(x)[1]

g <- length(levels(factor1))
b <- length(levels(factor2))
gb <- length(levels(factor12))

# COMPLETE model WITH interaction
fit <- manova(as.matrix(x) ~ factor1 + factor2 + factor1:factor2) 
summary.manova(fit, test="Wilks") # no interaction

# ADDITIVE model WITHOUT interaction
fit2 <- manova(as.matrix(x) ~ factor1 + factor2) 
summary.manova(fit2, test="Wilks") # reject H0

summary.aov(fit2)
# both are affected 

# One way MANOVA - Brand --------------------------------------------------------
# consideriamo le due risposte insieme e vediamo se cambia qualcosa

factor1 <- factor(data$Brand) 
x <- data[, c("Vendite.in.Volume.log", "Sconto")]
p <- dim(x)[2]

# set important variables
treat <- levels(factor1)
g <- length(treat)
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group

i1=which(factor1==treat[1])
i2=which(factor1==treat[2])
i3=which(factor1==treat[3])
i4=which(factor1==treat[4])
i5=which(factor1==treat[5])
i6=which(factor1==treat[6])
i7=which(factor1==treat[7])
i8=which(factor1==treat[8])
i9=which(factor1==treat[9])
i10=which(factor1==treat[10])
i11=which(factor1==treat[11])
i12=which(factor1==treat[12])

par(mfrow=c(1,dim(x)[2]))
for(i in 1:dim(x)[2]){
  boxplot(x[,i]~factor1, main=colnames(x)[i], ylim=c(min(x[,i]),max(x[,i])), col = rainbow(g))
}

fit <- manova(as.matrix(x) ~ factor1)
summary.manova(fit,test="Wilks") 
# reject H0

summary.aov(fit)
# both are affected 


# Assumptions test 1 ----------------------------------------------------------------------

# 1) normality (multivariate) in each group (g tests)
Ps <- NULL
for (i in 1:g){
  Ps <- c(Ps, mvn(x[factor1 == levels(factor1)[i],])$multivariateNormality)
}
Ps
# non è rispettata in tutti i gruppi

# 2) same covariance structure (= same covariance matrix Sigma)
S <- cov(x)
S1 <- cov(x[i1,])
S2 <- cov(x[i2,])
S3 <- cov(x[i3,])
S4 <- cov(x[i4,])
S5 <- cov(x[i5,])
S6 <- cov(x[i6,])
S7 <- cov(x[i7,])
S8 <- cov(x[i8,])
S9 <- cov(x[i9,])
S10 <- cov(x[i10,])
S11 <- cov(x[i11,])
S12 <- cov(x[i12,])

x11()
par(mfrow=c(2,6))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12), 
                        (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12), 
                        (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12), 
                        (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))
image(S5, col=heat.colors(100),main='Cov. S5', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))
image(S6, col=heat.colors(100),main='Cov. S6', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12), 
                        (0:100)/100, na.rm=TRUE))
image(S7, col=heat.colors(100),main='Cov. S7', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12), 
                        (0:100)/100, na.rm=TRUE))
image(S8, col=heat.colors(100),main='Cov. S8', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))
image(S9, col=heat.colors(100),main='Cov. S9', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))
image(S10, col=heat.colors(100),main='Cov. S10', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))
image(S11, col=heat.colors(100),main='Cov. S11', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))
image(S12, col=heat.colors(100),main='Cov. S12', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12),
                        (0:100)/100, na.rm=TRUE))


# One way MANOVA - Product --------------------------------------------------------
# consideriamo le due risposte insieme e vediamo se cambia qualcosa

factor1 <- factor(data$Product) 
x <- data[, c("Vendite.in.Volume.log", "Sconto")]
p <- dim(x)[2]

# set important variables
treat <- levels(factor1)
g <- length(treat)
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group

i1=which(factor1==treat[1])
i2=which(factor1==treat[2])
i3=which(factor1==treat[3])
i4=which(factor1==treat[4])
i5=which(factor1==treat[5])
i6=which(factor1==treat[6])
i7=which(factor1==treat[7])
i8=which(factor1==treat[8])
i9=which(factor1==treat[9])
i10=which(factor1==treat[10])
i11=which(factor1==treat[11])
i12=which(factor1==treat[12])
i13=which(factor1==treat[13])
i14=which(factor1==treat[14])
i15=which(factor1==treat[15])
i16=which(factor1==treat[16])
i17=which(factor1==treat[17])
i18=which(factor1==treat[18])
i19=which(factor1==treat[19])
i20=which(factor1==treat[20])
par(mfrow=c(1,dim(x)[2]))
for(i in 1:dim(x)[2]){
  boxplot(x[,i]~factor1, main=colnames(x)[i], ylim=c(min(x[,i]),max(x[,i])), col = rainbow(g))
}

fit <- manova(as.matrix(x) ~ factor1)
summary.manova(fit,test="Wilks") 
# reject H0

summary.aov(fit)
# both are affected 


# Assumptions test 2 ----------------------------------------------------------------------

# 1) normality (multivariate) in each group (g tests)
Ps <- NULL
for (i in 1:g){
  Ps <- c(Ps, mvn(x[factor1 == levels(factor1)[i],])$multivariateNormality[3])
}
Ps
# non è rispettata (solo in un paio di gruppi)

# 2) same covariance structure (= same covariance matrix Sigma)
S <- cov(x)
S1 <- cov(x[i1,])
S2 <- cov(x[i2,])
S3 <- cov(x[i3,])
S4 <- cov(x[i4,])
S5 <- cov(x[i5,])
S6 <- cov(x[i6,])
S7 <- cov(x[i7,])
S8 <- cov(x[i8,])
S9 <- cov(x[i9,])
S10 <- cov(x[i10,])
S11 <- cov(x[i11,])
S12 <- cov(x[i12,])
S13 <- cov(x[i13,])
S14 <- cov(x[i14,])
S15 <- cov(x[i15,])
S16 <- cov(x[i16,])
S17 <- cov(x[i17,])
S18 <- cov(x[i18,])
S19 <- cov(x[i19,])
S20 <- cov(x[i20,])

x11()
par(mfrow=c(4,5))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20), 
                        (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20), 
                        (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20), 
                        (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S5, col=heat.colors(100),main='Cov. S5', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S6, col=heat.colors(100),main='Cov. S6', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20), 
                        (0:100)/100, na.rm=TRUE))
image(S7, col=heat.colors(100),main='Cov. S7', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20), 
                        (0:100)/100, na.rm=TRUE))
image(S8, col=heat.colors(100),main='Cov. S8', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S9, col=heat.colors(100),main='Cov. S9', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S10, col=heat.colors(100),main='Cov. S10', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S11, col=heat.colors(100),main='Cov. S11', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S12, col=heat.colors(100),main='Cov. S12', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S13, col=heat.colors(100),main='Cov. S13', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S14, col=heat.colors(100),main='Cov. S14', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S15, col=heat.colors(100),main='Cov. S15', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S16, col=heat.colors(100),main='Cov. S16', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S17, col=heat.colors(100),main='Cov. S17', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S18, col=heat.colors(100),main='Cov. S18', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S19, col=heat.colors(100),main='Cov. S19', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))
image(S20, col=heat.colors(100),main='Cov. S20', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,
                              S13,S14,S15,S16,S17,S18,S19,S20),
                        (0:100)/100, na.rm=TRUE))


# One way ANOVA - Sconti --------------------------------------------------------

sconto_cols <- c("Vendite.in.Valore.Solo.Special.Pack", 
                 "Vendite.in.Valore.Solo.Volantino", 
                 "Vendite.in.Valore.Solo.Display", 
                 "Vendite.in.Valore.Solo.Riduzione.Prezzo", 
                 "Vendite.in.Valore.Solo.Sconto.Loyalty")

determine_tipo_sconto <- function(row) {
  sconti <- names(row)[row == 1]
  if (length(sconti) == 0) {
    return("No_sconto") 
  } else {
    return(paste(sconti, collapse = ", "))
  }
}

data <- data %>%
  rowwise() %>%
  mutate(Tipo_sconto = determine_tipo_sconto(across(all_of(sconto_cols))))

# togliamo le osservazioni senza sconto
no_sconto <- which(data$Tipo_sconto == 'No_sconto')
data <- data[-no_sconto,]

factor1 <- factor(data$Tipo_sconto) 
x <- data$Vendite.in.Volume.log
p <- dim(x)[2]

treat <- levels(factor1)
g <- length(treat)
n <- dim(data)[1]         
ng <- table(factor1)      

i1=which(factor1==treat[1])
i2=which(factor1==treat[2])
i3=which(factor1==treat[3])
i4=which(factor1==treat[4])
i5=which(factor1==treat[5])
i6=which(factor1==treat[6])
i7=which(factor1==treat[7])
i8=which(factor1==treat[8])
i9=which(factor1==treat[9])
i10=which(factor1==treat[10])
i11=which(factor1==treat[11])
i12=which(factor1==treat[12])
i13=which(factor1==treat[13])
i14=which(factor1==treat[14])

boxplot(x~factor1, main=colnames(x), ylim=c(min(x),max(x)), col = rainbow(g))

fit <- aov(x ~ factor1)
summary(fit)
# reject H0, there's difference


# Assumptions test 3 ----------------------------------------------------------------------

# 1) normality within each group
pvalue <- NULL
for (i in 1:g) {
  pval <- shapiro.test(x[factor1==treat[i]])$p
  pvalue <- c(pvalue, pval)
}
pvalue
# ok solo in alcuni gruppi 

# 2) homogeneity of variances
valid_groups <- which(ng > 1)
valid_indices <- which(factor1 %in% valid_groups)

x <- data[as.numeric(factor(data$Tipo_sconto)) %in% valid_groups,]
factor1 <- factor(x$Tipo_sconto)
x <- x$Vendite.in.Volume.log
bartlett.test(x, factor1)
# non rispettata, mettiamo che le assunzioni non vengono rispettate come 
# weaknesses dell'analisi


# Boxplot Vendite ~ Tipo_sconto -----------------------------------------------

data <- data %>%
  mutate(Tipo_sconto = recode(Tipo_sconto,
                              "Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Riduzione.Prezzo" = "F+D+RP",
                              "Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Riduzione.Prezzo" = "D+RP",          
                              "Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Riduzione.Prezzo, Vendite.in.Valore.Solo.Sconto.Loyalty" = "F+D+RP+L",
                              "Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Riduzione.Prezzo" = "F+RP",
                              "Vendite.in.Valore.Solo.Riduzione.Prezzo" = "RP",
                              "Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Riduzione.Prezzo, Vendite.in.Valore.Solo.Sconto.Loyalty" = "D+RP+L",
                              "Vendite.in.Valore.Solo.Volantino" = "F",
                              "Vendite.in.Valore.Solo.Special.Pack, Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Riduzione.Prezzo" = "SP+F+D+RP",
                              "Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Riduzione.Prezzo, Vendite.in.Valore.Solo.Sconto.Loyalty" = "F+RP+L",
                              "Vendite.in.Valore.Solo.Riduzione.Prezzo, Vendite.in.Valore.Solo.Sconto.Loyalty" = "RP+L",                   
                              "Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Sconto.Loyalty" = "F+D+L",
                              "Vendite.in.Valore.Solo.Volantino, Vendite.in.Valore.Solo.Display" = "F+D",
                              "Vendite.in.Valore.Solo.Special.Pack, Vendite.in.Valore.Solo.Display, Vendite.in.Valore.Solo.Riduzione.Prezzo" = "SP+D+RP",
                              "Vendite.in.Valore.Solo.Display" = "D"))

p <- ggplot(data, aes(x = factor(Tipo_sconto), y = Vendite.in.Volume.log)) +
  geom_boxplot(fill = "#D2691E", color = "black", outlier.size = 0.5) +
  labs(
    title = "Sales Distribution for Discount Type",
    x = "Discounts",
    y = "Sales Volume (log)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10), 
    panel.background = element_rect(fill = "transparent", color = NA),  # Sfondo trasparente
    plot.background = element_rect(fill = "transparent", color = NA),   # Sfondo trasparente per l'intera trama
    panel.grid.major = element_line(color = "#333333", linewidth = 0.2),  # Griglia maggiore grigia
    panel.grid.minor = element_line(color = "#333333", linewidth = 0.2)  # Griglia minore grigia tratteggiata
  )  

legend_text <- "F: Flyer discount\nD: Display discount\nL: Loyalty discount\nRP: Reduction Price discount\nSP: Special Pack discount"

legend_box <- ggdraw() + 
  draw_text(" ", x = 0.15, y = 0.9, hjust = 0, vjust = 1, size = 10) + # Spazio vuoto orizzontale per spostare a destra
  draw_text(legend_text, x = 0.15, y = 0.9, hjust = 0, vjust = 2, size = 7) +# Legenda spostata in basso e a destra
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # Sfondo trasparente
    plot.background = element_rect(fill = "transparent", color = NA)    # Sfondo trasparente per l'intera legenda
  )

# Combina il grafico e la legenda
combined_plot <- plot_grid(p, legend_box, ncol = 2, rel_widths = c(3, 1)) +
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(
  filename = "./Plots/plot poster/box_sales_disc.png", 
  plot = combined_plot, 
  width = 8,    # Larghezza del grafico
  height = 3,    # Altezza del grafico
  units = "in",  # Unità di misura (può essere "in", "cm", o "mm")
  bg = "transparent"
)
