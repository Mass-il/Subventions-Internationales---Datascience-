install.packages("lmtest")
install.packages("ggrepe")
library(haven)
library(plm)
library(dplyr)
library(tidyr)
library(broom)
library(naniar)
library(lmtest)
library(sandwich)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(clubSandwich)

#On télécharge tous les données disponibles
df1 <- read.csv("ODA_data.csv")
df2 <- read_dta("disast2.dta")
df3 <- read_dta("disast.dta")
df4 <- read.csv("disast.csv")
df5 <- read_dta("ODA_data.dta")

#fusionner les dataframes 
df_total <- bind_rows(df1,df2, df3, df4, df5)

# données de panel 
pdata <- pdata.frame(df_total, index = c("donorid", "recid", "year"))

# supprimer les valeurs doublées
duplicates <- pdata[duplicated(pdata[, c("donorid", "recid", "year")]) | 
                      duplicated(pdata[, c("donorid", "recid", "year")],
                                 fromLast = TRUE), ]

pdata <- pdata[!duplicated(pdata[, c("donorid", "recid", "year")]), ]

# afficher d'information sur les valeurs manquantes et les supprimer 
missing_percent <- colMeans(is.na(pdata)) * 100
print(missing_percent)

pdata <- na.omit(pdata)

#matrice de corrélation --------------------------------------------------------
df_cor <- bind_rows(df1,df4) #df1 = df5 et df2 = df3 = df4
df_cor <- na.omit(df_cor)
matrice <- cor(df_cor[, c("oda_disbursement","infantmort_r","population_r",
                          "fdiinflows_r","remittances_r","gdp_r","polrights_r",
                          "civlib_r","democracy_r","nbdisast", "population_d",
                          "gdp_d","colony","distcap","border","comlang","imports",
                          "exports", "donorid", "recid")])

# matrice de corrélation pour la présentation + rapport
matrice <- melt(matrice)
ggplot(matrice, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)+
  labs(title = "Matrice de corrélation (1990-2009)", x = "", y = "", 
       fill = "Corrélation") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# traiter les variables qualitatives -------------------------------------------
df_total$colony <- as.factor(df_total$colony)
df_total$border <- as.factor(df_total$border)
df_total$donorid <- as.factor(df_total$donorid)
df_total$recid <- as.factor(df_total$recid)
df_total$comlang <- as.factor(df_total$comlang)
df_total$polrights_r <- as.factor(df_total$polrights_r)
df_total$democracy_r <- as.factor(df_total$democracy_r)
df_total$civlib_r <- as.factor(df_total$civlib_r)
df_total$recname <- as.factor(df_total$recname)
df_total$donorname <- as.factor(df_total$donorname)
df_total$region <- as.factor(df_total$region)

#créer quelques nouvelles variables

#institution = polrights_r + democracy_r + civlib_r (en 5 groups):
pdata$institution <- rowSums(sapply(pdata[c("democracy_r", "civlib_r", "polrights_r")], 
                                    as.numeric))
pdata$institution <- cut(pdata$institution,breaks = seq(3, 28, length.out = 6),  
  labels = c("Very Low", "Low", "Medium", "High", "Very High"),
  include.lowest = TRUE)
pdata$institution <- as.factor(pdata$institution)

#effets fixes:
pdata$USA <- as.numeric(pdata$donorname == "USA")
pdata$GB <- as.numeric(pdata$donorname == "Great Britain")
pdata$J <- as.numeric(pdata$donorname == "Japan")
pdata$DE <- as.numeric(pdata$donorname == "Germany")
pdata$FR <- as.numeric(pdata$donorname == "France")
pdata$NE <- as.numeric(pdata$donorname == "The Netherlands")


###############################MODELE 1#########################################

# log(ODA Disbursement_d,r,t) = α_0 
#                             + β_1 Exports_d,r,t + β_2 Imports_d,r,t 
#                             + (γ_1,1 + γ_1,2 DistCap_d,r) * DistCap_d,r 
#                             + γ_2 Colony_d,r + γ_3 Border_d,r + γ_4 Comlang_d,r 
#                             + (δ_1,1 + δ_1,2 log(GDP_d,t)) * log(GDP_d,t) 
#                             + (δ_2,1 + δ_2,2 log(Population_d,t)) * log(Population_d,t) 
#                             + (μ_1,1 + μ_1,2 log(GDP_r,t)) * log(GDP_r,t) 
#                             + (μ_2,1 + μ_2,2 log(Population_r,t)) * log(Population_r,t) 
#                             + μ_3 InfantMort_r,t + μ_4 FDIinflows_r,t 
#                             + μ_5 log(Remittances_r,t) + μ_6 Institution_r,t 
#                             + (μ_7,1 + μ_7,2 NbDisaster_r,t) * NbDisaster_r,t

#supprimer toutes les données avec oda_disbursement = 0
mean(pdata$oda_disbursement == 0, na.rm = TRUE) * 100   #68.2149% sont = 0
pdata <- pdata %>%
  filter(oda_disbursement > 0 | is.na(oda_disbursement))#garder les valeurs positives

#panel data frame -> df ordinaire
if(inherits(pdata, "pdata.frame")) {
  pdata <- as.data.frame(pdata)
}

#comparer différent structure du variable gdp_d pour trouver la modèle avec la
#pouvoir explicative la plus élevée (R^2 ajusté)

model1 <- lm(log(oda_disbursement) ~ gdp_d, data = pdata)
summary(model1) #Multiple R-squared:  0.1368,	Adjusted R-squared:  0.1367

model1 <- lm(log(oda_disbursement) ~ gdp_d + I(gdp_d**2) , data = pdata)
summary(model1) #Multiple R-squared:  0.1505,	Adjusted R-squared:  0.1503

model1 <- lm(log(oda_disbursement) ~ log(gdp_d), data = pdata)
summary(model1) #Multiple R-squared:  0.1197,	Adjusted R-squared:  0.1196

model1 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) , data = pdata)
summary(model1) #Multiple R-squared:  0.1513,	Adjusted R-squared:  0.1511

#Resultat: (log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2)) a la valeur 
#R^2 ajustée la plus élevée et on ajoute la prochaine variable sur cette modèle

#Ici sont les modèles optimales par rapport à chaque variable 
#(obtenu par la même méthode comme chez gdp_d) 

model2 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2), data = pdata)
summary(model2) #Multiple R-squared:  0.1528,	Adjusted R-squared:  0.1525

model3 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2) + comlang, data = pdata)
summary(model3)  #Multiple R-squared:  0.1738,	Adjusted R-squared:  0.1734

model4 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2) + comlang 
             + log(remittances_r), data = pdata)
summary(model4) #Multiple R-squared:  0.1772,	Adjusted R-squared:  0.1767 

model5 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2) + comlang 
             + log(remittances_r)+ nbdisast + I(nbdisast**2), data = pdata)
summary(model5) #Multiple R-squared:  0.214,	Adjusted R-squared:  0.2134 

model6 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2) + comlang 
             + log(remittances_r)+ nbdisast + I(nbdisast**2) + distcap 
             + I(distcap**2), data = pdata)
summary(model6) #Multiple R-squared:  0.2246,	Adjusted R-squared:  0.2238

model7 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2) + comlang 
             + log(remittances_r)+ nbdisast + I(nbdisast**2) + distcap 
             + I(distcap**2) + colony, data = pdata)
summary(model7) #Multiple R-squared:  0.2389,	Adjusted R-squared:  0.238 

model8 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d)+ I(log(population_d)**2) + comlang 
             + log(remittances_r) + nbdisast + I(nbdisast**2) + distcap 
             + I(distcap**2) + colony + log(population_r) 
             + I(log(population_r)**2), data = pdata)
summary(model8) #Multiple R-squared:  0.2658,	Adjusted R-squared:  0.2648

model9 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
             + log(population_d) + I(log(population_d)**2) + comlang 
             + log(remittances_r) + nbdisast + I(nbdisast**2) + distcap 
             + I(distcap**2) + colony + log(population_r)
             + I(log(population_r)**2) + imports, data = pdata)
summary(model9) #Multiple R-squared:  0.2659,	Adjusted R-squared:  0.2649 


model10 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast + I(nbdisast**2)  + distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports, data = pdata)
summary(model10) #Multiple R-squared:  0.2662,	Adjusted R-squared:  0.2651 

model11 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast+ I(nbdisast**2) + distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports 
              + infantmort_r, data = pdata)   
summary(model11) #Multiple R-squared:  0.2728,	Adjusted R-squared:  0.2716 

model12 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast +I(nbdisast**2)+ distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports 
              + infantmort_r + log(gdp_r) + I(log(gdp_r)**2), data = pdata)
summary(model12) #Multiple R-squared:  0.2957,	Adjusted R-squared:  0.2944

model13 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast+I(nbdisast**2) + distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports 
              + infantmort_r + log(gdp_r) + I(log(gdp_r)**2) + fdiinflows_r
              , data = pdata)
summary(model13) #Multiple R-squared:  0.2979,	Adjusted R-squared:  0.2965

model14 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast+I(nbdisast**2) + distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports 
              + infantmort_r + log(gdp_r) + I(log(gdp_r)**2) + fdiinflows_r 
              + institution, data = pdata)
summary(model14) #Multiple R-squared:  0.3016,	Adjusted R-squared:    0.3

model15 <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast+I(nbdisast**2) + distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports 
              + infantmort_r + log(gdp_r) + I(log(gdp_r)**2) + fdiinflows_r 
              + institution + border, data = pdata)
summary(model15) #Multiple R-squared:  0.3022,	Adjusted R-squared:  0.3005

#graphique pour visualiser les residus et l'hétéroscédasticité
plot(model15, which = 3)

#white's test pour l'hétéroscédasticité
#Lopez, J. (2020), White's test and Bootstrapped White's test under the 
#methodology of Jeong, J., Lee, K. (1999) package version 0.0.1
install.packages("devtools")
devtools::install_github("jlopezper/whitestrap")
library(whitestrap)
white_test(model15)

#Calculer les erreurs standards robusts pour prendre en compte l'hétéroscédasticité
coeftest(model15, vcov. = vcovHC(model15, type = 'HC1'))

###############################MODELE 2#########################################

# log(ODA Disbursement_d,r,t) = α_0 
#                             + β_1 Exports_d,r,t + β_2 Imports_d,r,t 
#                             + (γ_1,1 + γ_1,2 DistCap_d,r) * DistCap_d,r 
#                             + γ_2 Colony_d,r + γ_3 Border_d,r + γ_4 Comlang_d,r 
#                             + (δ_1,1 + δ_1,2 log(GDP_d,t)) * log(GDP_d,t) 
#                             + (δ_2,1 + δ_2,2 log(Population_d,t)) * log(Population_d,t) 
#                             + (μ_1,1 + μ_1,2 log(GDP_r,t)) * log(GDP_r,t) 
#                             + (μ_2,1 + μ_2,2 log(Population_r,t)) * log(Population_r,t) 
#                             + μ_3 InfantMort_r,t + μ_4 FDIinflows_r,t 
#                             + μ_5 log(Remittances_r,t) + μ_6 Institution_r,t 
#                             + (μ_7,1 + μ_7,2 NbDisaster_r,t) * NbDisaster_r,t
#                             + ξ_1USA + γ_5 USA*Colony_d,r 
#                             + ξ_2 GB + γ_6 GB*Colony_d,r 
#                             + ξ_3 DE + γ_7 DE*Colony_d,r 
#                             + ξ_4 FR + γ_8 FR*Colony_d,r 
#                             + ξ_5 NE + γ_9 NE*Colony_d,r 
#                             + ξ_6 Japan
model_f <- lm(log(oda_disbursement) ~ log(gdp_d) + I(log(gdp_d)**2) 
              + log(population_d) + I(log(population_d)**2) + comlang 
              + log(remittances_r) + nbdisast+I(nbdisast**2) + distcap 
              + I(distcap**2) + colony + log(population_r)
              + I(log(population_r)**2) + imports + exports 
              + infantmort_r + log(gdp_r) + I(log(gdp_r)**2) + fdiinflows_r 
              + institution + border + USA + GB*colony + J + DE + FR + GB 
              + FR*colony + DE*colony + NE + NE*colony + USA*colony 
              , data = pdata)

summary(model_f) #Multiple R-squared:  0.323,	Adjusted R-squared:  0.3207 

#graphique pour visualiser les residus et l'hétéroscédasticité
plot(model_f, which = 3)

#white's test pour l'hétéroscédasticité
#Lopez, J. (2020), White's test and Bootstrapped White's test under the 
#methodology of Jeong, J., Lee, K. (1999) package version 0.0.1
white_test(model_f)

#Calculer les erreurs standards robusts pour prendre en compte l'hétéroscédasticité
coeftest(model_f, vcov. = vcovHC(model_f, type = 'HC1'))

#######################GRAPHIQUES SUPPLEMENTAIRES###############################

#Camembert USA pourcentage colonie vs le reste----------------------------------

#Filtre pour USA
usa_data <- pdata[pdata$donorname == "USA", ]
usa_data$group <- ifelse(usa_data$colony == 1, "Former Colony", "Non-Colony")

#Calculer les pourcentages
totals <- aggregate(oda_disbursement ~ group, data = usa_data, sum, na.rm = TRUE)
totals$percent <- prop.table(totals$oda_disbursement) * 100

#Creer le camembert
labels <- paste0(totals$group, "\n", 
                 round(totals$percent, 1), "%\n",
                 "($", round(totals$oda_disbursement/1e9, 1), "B)")

pie(totals$oda_disbursement,
    labels = labels,
    col = c("#E41A1C", "#377EB8"),  
    main = "USA ODA Disbursement\nColony vs. Non-Colony Recipients",
    border = "white")

#Camembert GB pourcentage colonie vs le reste-----------------------------------

#Filtre pour GB 
gb_data <- pdata[pdata$donorname == "Great Britain", ]
gb_data$group <- ifelse(gb_data$colony == 1, "Former Colony", "Non-Colony")

#Calculer les pourcentages
totals <- aggregate(oda_disbursement ~ group, data = gb_data, sum, na.rm = TRUE)
totals$percent <- prop.table(totals$oda_disbursement) * 100

#Creer le camembert
labels <- paste0(totals$group, "\n", 
                 round(totals$percent, 1), "%\n",
                 "($", round(totals$oda_disbursement/1e9, 1), "B)")
pie(totals$oda_disbursement,
    labels = labels,
    col = c("#E41A1C", "#377EB8"),
    main = "GB ODA Disbursement\nColony vs. Non-Colony Recipients",
    border = "white")

#Camembert FR pourcentage colonie vs le reste----------------------------------

#Filtre pour FR
fr_data <- pdata[pdata$donorname == "France", ]
fr_data$group <- ifelse(fr_data$colony == 1, "Former Colony", "Non-Colony")

#Calculer les pourcentages
totals <- aggregate(oda_disbursement ~ group, data = fr_data, sum, na.rm = TRUE)
totals$percent <- prop.table(totals$oda_disbursement) * 100

#Creer le camembert
labels <- paste0(totals$group, "\n", 
                 round(totals$percent, 1), "%\n",
                 "($", round(totals$oda_disbursement/1e9, 1), "B)")
pie(totals$oda_disbursement,
    labels = labels,
    col = c("#E41A1C", "#377EB8"),
    main = "France ODA Disbursement\nColony vs. Non-Colony Recipients",
    border = "white")

#Camembert NE pourcentage colonie vs le reste----------------------------------

#Filtre pour NE 
ne_data <- pdata[pdata$donorname == "Germany", ]
ne_data$group <- ifelse(ne_data$colony == 1, "Former Colony", "Non-Colony")

#Calculer les pourcentages
totals <- aggregate(oda_disbursement ~ group, data = ne_data, sum, na.rm = TRUE)
totals$percent <- prop.table(totals$oda_disbursement) * 100

#Creer le camembert
labels <- paste0(totals$group, "\n", 
                 round(totals$percent, 1), "%\n",
                 "($", round(totals$oda_disbursement/1e9, 1), "B)")
pie(totals$oda_disbursement,
    labels = labels,
    col = c("#E41A1C", "#377EB8"),  
    main = "Germany ODA Disbursement\nColony vs. Non-Colony Recipients",
    border = "white")

#Camembert des pays donateurs sans les petits donateurs ------------------------
#Calculer le oda_disbursement totale de chaque pays
total_par_pays <- aggregate(oda_disbursement ~ donorname, 
                            data = pdata, 
                            FUN = sum)

#L'ordre du camembert
total_par_pays <- total_par_pays[order(-total_par_pays$oda_disbursement), ]
pdata$oda_disbursement <- as.numeric(as.character(pdata$oda_disbursement))

#Parameters
n_pays <- 23
top_pays <- 8  

#regrouper les plus petits pays
autres_pays <- total_par_pays[(top_pays + 1):n_pays, ]
total2 <- rbind(
  total_par_pays[1:top_pays, ],
  data.frame(donorname = "Others", 
             oda_disbursement = sum(autres_pays$oda_disbursement))
)

#Camembert
par(mar = c(1, 1, 1, 1))
pie(total2$oda_disbursement, 
    labels = paste0(total2$donorname, "\n", 
                    round(total2$oda_disbursement/sum(total2$oda_disbursement)*100, 1), "%"),
    cex = 0.8, radius = 1, col = rainbow(length(total2$donorname)),
    main = "ODA Disbursement by Donor Country", clockwise = TRUE) 


#Camembert des pays recevant sans les petits recevants--------------------------
#Calculer le oda_disbursement totale de chaque pays
total_par_pays <- aggregate(oda_disbursement ~ recname, 
                            data = pdata, 
                            FUN = sum)

#L'ordre du camembert
total_par_pays <- total_par_pays[order(-total_par_pays$oda_disbursement), ]
pdata$oda_disbursement <- as.numeric(as.character(pdata$oda_disbursement))

#Parameters
n_pays <- 125
top_pays <- 8  

#regrouper les plus petits pays
autres_pays <- total_par_pays[(top_pays + 1):n_pays, ]
total2 <- rbind(
  total_par_pays[1:top_pays, ],
  data.frame(recname = "Others", 
             oda_disbursement = sum(autres_pays$oda_disbursement))
)

#Camembert
par(mar = c(1, 1, 1, 1))
pie(total2$oda_disbursement, 
    labels = paste0(total2$recname, "\n", 
                    round(total2$oda_disbursement/sum(total2$oda_disbursement)*100, 1), "%"),
    cex = 0.8, radius = 1, col = rainbow(length(total2$recname)),
    clockwise = TRUE) 

#top 10 recevant dons de UK-----------------------------------------------------
top_gb_recipients <- pdata %>%
  filter(donorname == "Great Britain") %>% 
  group_by(recname) %>%                   
  summarise(total_oda = sum(oda_disbursement, na.rm = TRUE)) %>% 
  arrange(desc(total_oda)) %>%             
  ungroup()
top_gb_recipients %>%
  slice_max(total_oda, n = 10) %>%         
  ggplot(aes(x = reorder(recname, total_oda), y = total_oda)) +
  geom_col(fill = "#012169") +             
  coord_flip() +                           
  labs(
    title = "Top 10 des pays receveurs de l'aide britannique (ODA)",
    x = "Pays receveur",
    y = "Total ODA (USD, somme sur toutes les années)",
    caption = "Source : Données ODA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.text.y = element_text(size = 10)                   
  )

# graphe inde-------------------------------------------------------------------
pays_selectionne <- "India"
pdata %>%
  filter(recname == pays_selectionne) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  ggplot(aes(x = year, y = democracy_r)) +
  geom_line(color = "steelblue", linewidth = 1) + 
  geom_point(color = "darkblue") +               
  labs(
    title = paste("Évolution du PIB pour", pays_selectionne),
    x = "Année",
    y = "PIB (gdp_r)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks()) 

######################AUTRES CALCULATIONS POUR LE RAPPORT#######################

#moyennes de "population_d", "population_r", "gdp_d", "gdp_r"-------------------
means <- colMeans(pdata[, c("population_d", "population_r", "gdp_d", "gdp_r")], 
                  na.rm = TRUE)
means[1]
means[2]
means[3]
means[4]