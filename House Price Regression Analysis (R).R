
data <- read.csv("C:/Users/aicha/Desktop/2 eme session/Statistique/UA3/Projet_UA3.csv", stringsAsFactors=TRUE)

# Question 1: Étudier la corrélation entre les variables du jeu de données --------------------------------------------------------------

library(GGally)
ggpairs(data)

# Il y a une très forte relation linéaire entre la surface habitable (lingA) et le nombre de pièces (rooms)
# Ce n’est pas surprenant : plus une maison est grande, plus elle peut contenir de pièces
# Ces corrélations signifient que certaines variables expliquent presque les mêmes choses.
# On va surveiller de près les groupes de variables suivantes : lingA, rooms, beds, baths : très corrélées entre elles

# Question 2:  Étudier la linéarité entre la variable dépendante et les variables explicatives du jeu de données--------------------------------------------------------------
library(car)
scatterplotMatrix(data,
                  smooth = FALSE,
                  pch = 19,
                  regLine = list(col = 'red'))

# En inspectant le nuage de points, on constate que les relations sont globalement linéaires entre la variable dépendante price et les variables lingA, baths, rooms et lValue
# lingA (surface habitable) : Plus la surface habitable est grande, plus le prix augmente → relation linéaire positive.
# baths (nombre de salles de bains) : Plus il y a de salles de bains, plus le prix est élevé.
# rooms (nombre de pièces) : Même chose, plus de pièces → prix plus élevé.
# lValue (valeur foncière) : Plus la valeur du lot est grande, plus le prix de la maison est élevé.


# Question 3: Construisez un modèle de régression linéaire multiple--------------------------------------------------------------

# Créer le modèle complet avec toutes les variables explicatives
modele_initial <- lm(price ~ ., data = data)

# Afficher le résumé du modèle
summary(modele_initial)

## ------- Statistiques globales du modèle
  # R² = 0.6272 : Le modèle explique environ 62.7% de la variation des prix (price).
  # R² ajusté = 0.6253 : Très proche de R², ce qui signifie que l'ajout de variables n'a pas trop complexifié le modèle.
  # F-statistic = 321.2, p-value < 2.2e-16 : Le modèle est hautement significatif, donc au moins une variable a un effet sur price

## ------- Interprétation des coefficients
  # Les variables significatives sont : lotS, age, lValue, lingA, beds, baths, rooms (p < 0.05) 
  # Les variables non significatives sont : pctCol, firep (p > 0.05, pourraient être retirées) 

## ------- Conclusion
  # Le modèle est bon et bien ajusté (R² > 0.62)
  # Certaines variables très liées au prix (lValue, lingA, baths)
  # D’autres sont redondantes ou inutiles (pctCol, firep) → on envisagera de les retirer plus tard


# Question 4: Vérifier les conditions d’application du modèle --------------------------------------------------------------
library(performance)
library(see)
check_model(modele_initial)


# Multicolinéarité → check_collinearity() du package performance
check_collinearity(modele_initial)

## ------- Interpretation
  # Aucune variable n’a un VIF > 5 → donc pas de multicolinéarité problématique
  # lingA, rooms, baths ont des VIF modérés, ce qui est cohérent car ils sont liés
  # On n’a pas besoin de retirer de variable pour cette raison

# Test d’homoscédasticité → bptest() (test de Breusch-Pagan)
library(lmtest)
bptest(modele_initial)

## ------- Interpretation
  # La p-value < 0.05 → on rejette H0
  # Il y a de l’hétéroscédasticité dans notre modèle.  Cela signifie que la variance des résidus n'est pas constante 


# Vérification de la linéarité 
harvtest(modele_initial)

## ------- Interpretation
  # La p-value est très petite (< 0.05) →  on rejette H0
  # Il y a donc un problème de linéarité globale dans le modèle


# Vérification de l’indépendance des résidus (Durbin- Watson Test) 
library(lmtest)
dwtest(modele_initial)

## ------- Interpretation
  # Statistique DW ≈ 1.69 : un peu inférieure à 2
  # p-value très faible (< 0.05) → on rejette H₀
  # Il y a une autocorrélation positive des résidus


# Vérification de la normalité des résidus

shapiro.test(residuals(modele_initial))

## ------- Interpretation
  # p-value très faible (< 0.05) → On rejette l'hypothèse de normalité.
  # Les résidus ne sont pas normalement distribués, ce qui peut poser problème pour certaines hypothèses du modèle de régression linéaire.

# -------------> Donc on dois transformer price pour stabiliser la variance, améliorer la linéarité et rendre les résidus plus normaux.


## Transformation de la variable price

# Charger le package
library(MASS)
# Identifier la meilleure transformation pour Y
boxcox(modele_initial)
# Transfomation logarithmique de price
data$price <- log(data$price)
#data$price

# Modèle avec Y transformée 
model_T <- lm(price ~ ., data = data)
summary(model_T)

## ------- Interpretation
   # Variables significatives : lotS, age, lValue, lingA, pctCol, baths
   # Variables non significatives : beds, firep, rooms (Ces variables seront probablement éliminées dans l’étape de sélection).


# Question 5: Utiliser la méthode de sélection du meilleur sous-ensemble--------------------------------------------------------------


library(leaps)
choix <- regsubsets(price ~., data = data)
summary <- summary(choix)
summary

# Les valeurs du critère R2 ajusté pour les modèles obtenus
summary_choix <- summary(choix)
summary_choix$adjr2
## ------- Interpretation
    # La différence de R² ajusté entre les modèles à 5, 6, 7 et 8 variables est très faible (respectivement ≈ 0.5588, 0.5602, 0.5607, 0.5607).
    # Cela indique que l’ajout de variables au-delà de 5 n’apporte qu’une amélioration marginale de l’ajustement. 
    # Par souci de simplicité et de performance, nous retenons donc le modèle à 5 variables, 
    # qui offre un bon compromis entre qualité prédictive et parcimonie.

# Les valeurs du critère BIC pour les modèles obtenus

summary_choix$bic

## ------- Interpretation
    # Le BIC est le plus bas pour le modèle à 5 variables (-1374.068), ce qui indique le meilleur compromis entre performance 
    # et simplicité. On retient donc ce modèle comme le plus optimal selon le BIC.

# Méthode graphique pour le choix des variables selon le BIC

plot(choix)

## ------- Interpretation
    # Le graphique montre que le modèle optimal selon le critère BIC inclut les variables lotS, age, lValue, lingA et baths.
    # C’est celui qui minimise le BIC, ce qui confirme le choix du modèle à 5 variables.


# Les valeurs du critère Cp de Mallows pour les modèles obtenus
summary_choix$cp

## ------- Interpretation

  # Le modèle à 6 variables explicatives a un Cp = 8.49, ce qui est proche de p = 7 (6 variables + 1 intercept).
  #Cela signifie que ce modèle est bien ajusté selon le critère de Mallows, car un bon modèle doit avoir Cp ≈ p.

# #Critère de Cp de Mallows avec une ligne de référence
plot(1:8, summary_choix$cp, 
     xlab ='P (# de prédicteurs + 1)', 
     ylab ='Cp')
abline(a=0, b=1)

## ------- Interpretation
    # Dans le graphique, on observe que le point correspondant au modèle à 6 variables est celui qui se situe
    # le plus proche de la ligne de référence (ligne Cp = p).
    # Cela signifie que ce modèle est bien ajusté selon le critère de Mallows.

#combiner les statistiques pertinentes du modèle 
cbind(summary_choix$which, 
      round(cbind(Adjr2= summary_choix$adjr2, 
                  Cp =summary_choix$cp, 
                  BIC = summary_choix$bic),3))
## ------- Interpretation

  # Modèle retenu selon BIC :Le modèle à 5 variables (lotS, age, lValue, lingA, baths) a le BIC le plus faible (-1374.068).
  # Il combine simplicité (5 variables) et bon ajustement (Adj R² = 0.559, Cp = 13.29 proche de p=6).
  # Modèle retenu selon Cp :Le modèle à 6 variables (lotS, age, lValue, lingA, pctCol, baths) a le Cp le plus proche de p (=7) avec Cp = 8.49.

  # -----> Finalement On choisit le modèle à 5 variables car il maximise la performance statistique tout en restant simple et interprétable.

#Modèle final avec ces 5 variables

model_sous_ensemble <- lm(price ~ lotS + age + lValue + lingA + baths, data = data)
summary(model_sous_ensemble)

## ------- Interpretation
  # Toutes les variables sont statistiquement significatives à 5% 
  # L’effet le plus fort est celui de lingA et baths, suivis par lValu
  # Le modèle final de régression linéaire multiple sélectionné selon le critère BIC comprend les variables lotS, age, lValue, lingA et baths.
  # Ces variables sont toutes significatives et permettent d’expliquer environ 56% de la variabilité du logarithme du prix des maisons.



# Question 6: Proposer une autre méthode de sélection de variables--------------------------------------------------------------

library(MASS)

# On part du modèle complet (toutes les variables)
model_T <- lm(price ~ ., data = data)
summary(model_T)
# data$price

# Méthode pas à pas avec AIC
modele_AIC <- stepAIC(model_T, direction = "both", trace = TRUE)

## ------- Interpretation
  # La méthode stepAIC a sélectionné un modèle avec 7 variables (lotS, age, lValue, lingA, pctCol, baths, rooms), 
  # en éliminant celles qui n'amélioraient pas suffisamment l’AIC (comme beds et firep). 
  # Le modèle retenu est celui ayant l’AIC le plus bas (-4151.9), indiquant un bon compromis entre complexité et performance.

modele_AIC <- lm(price ~ lotS + age + lValue + lingA + pctCol + baths + rooms, data = data)
summary(modele_AIC)

## ------- Interpretation
# Le modèle global est hautement significatif : p-value < 2.2e-16. 
# Le modèle explique environ 56.2 % de la variation de la variable log(price).
# Variables significatives (p < 0.05) : lotS, age, lValue, lingA, pctCol, baths.
##lingA (surface habitable) et baths (nombre de salles de bains) ont les plus forts effets positifs sur le prix.
##age a un effet négatif.
##pctCol (taux de diplômés) a un effet modéré mais significatif

# Question 7 --------------------------------------------------------------



# Identifier les relations potentiellement non linéaires
## Réorganiser les données au format "long"

# Charger la librairie
library(reshape2)

# Transformer le jeu de données en format long pour ggplot2
data_long <- melt(data, id.vars = "price")

# Charger ggplot2
library(ggplot2)

# Graphe loess + droite linéaire pour chaque variable explicative
ggplot(data_long)+ aes(x = value, y = price) +
  geom_point() +
  geom_smooth(method = "lm",se = F, color = "blue") +    
  geom_smooth(method = "loess",se = F, color = "red") +      
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Relations entre les variables explicatives et le prix",
       x = "Variable explicative", y = "Prix (log)") 

## ------- Interpretation
    #La variable lingA présente un effet non linéaire sur le prix (forme en ∩).
    #Elle est donc un bon candidat pour l’ajout d’un terme polynomial de degré 2 dans le modèle.



# Estimation du modèle avec terme polynomial

## Modèle linéaire de base

model_sous_ensemble <- lm(price ~ lotS + age + lValue + lingA + baths, data = data)
summary(model_sous_ensemble)

## Modèle avec terme polynomial

modele_poly <- lm(price ~lotS + age + lValue + poly(lingA, 2, raw = TRUE) + baths, data = data )
summary(modele_poly)



## Interprétation :

# Dans le modèle linéaire, l'effet de lingA est constant et positif :
# Le coefficient est 0.0003102, ce qui signifie qu'une augmentation de 1 unité de lingA entraîne une hausse estimée de 0.00031 unités de prix, toutes choses égales par ailleurs.
# Dans le modèle polynômial, on détecte une relation non linéaire :
# lingA a d'abord un effet positif, avec un coefficient de degré 1 égal à 0.0006161.
# Mais l’effet ralentit à mesure que lingA augmente, car le coefficient du terme quadratique (lingA²) est négatif : -7.423e-08.
# Cela reflète par exemple un effet de saturation :
# augmenter lingA continue d’augmenter le prix au début, mais de moins en moins fortement à mesure que lingA devient plus grand.


## Comparaion des modèle 

AIC(model_sous_ensemble, modele_poly)

## ------- Interpretation

    # Le modèle avec effet polynomial (modele_poly) est préféré au modèle linéaire (model_sous_ensemble) 
    # car il présente un meilleur ajustement (R² ajusté : 0.568 vs 0.5588), une erreur résiduelle plus faible (0.2976 vs 0.3008) 
    # et un AIC significativement plus bas (724.3 vs 759.8).
    # Un delta AIC supérieur à 10 indique une très forte amélioration du modèle. Ici, avec delta AIC = 35.5,
    #le modèle avec terme polynomial est clairement préférable en termes de performance statistique.
    # De plus, les coefficients montrent une relation non linéaire entre lingA et price, 
    # avec un effet initialement positif (0.0006161), puis décroissant (-7.423e-08), ce qui suggère un effet de saturation.


## Test de Fisher 
anova(model_sous_ensemble, modele_poly)

## ------- Interpretation

    # Le test de Fisher permet de comparer le modèle linéaire au modèle polynômial incluant un terme quadratique 

    # pour la variable lingA. Le résidu sum of squares (RSS) passe de 155.78 dans le modèle linéaire à 152.44 
    # dans le modèle polynômial, soit une réduction de 3.34 dans la somme des carrés des résidus (RSS), expliquée par le terme lingA2. 
    
    # La statistique F obtenue est de 37.693 avec une p-valeur très faible (1.026e-09), 
    # ce qui indique que l’ajout du terme quadratique améliore significativement l’ajustement du modèle. 
    
    #On rejette l’hypothèse nulle et on conclut que la relation entre lingA et price est mieux représentée 
    #par une courbe que par une droite.



# Question 8 --------------------------------------------------------------

# Visualisation de l'effet d'interaction avec ggplot

head(data)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(baths, breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


ggplot(data, aes(x = lingA, y = price, 
                 color = cut(age, breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(lotS , breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(lValue , breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(pctCol , breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(beds  , breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(firep  , breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = lingA, y = price, 
                 color = cut(rooms , breaks = 2))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


## ------- Interpretation

# Parmi toutes les variables testées en interaction avec lingA, baths est celle qui présente l’effet d’interaction 
# le plus clair, avec des pentes différentes et un décalage vertical marqué. Les variables rooms, beds, age, pctCol 
# et lValue montrent des effets modérés ou partiels. En revanche, firep et lotS ne présentent aucune interaction notable.
# Ainsi, baths est le meilleur choix pour modéliser une interaction avec lingA.


#Autre méthode de Visualisation de l'effet d'interaction avec coplot

# discretisation des variables

data$baths_T <- cut(data$baths, breaks = 2)
data$age_T <- cut(data$age, breaks = 2)
data$lotS_T <- cut(data$lotS, breaks = 2)
data$lValue_T <- cut(data$lValue, breaks = 2)
data$pctCol_T <- cut(data$pctCol, breaks = 2)
data$beds_T <- cut(data$beds, breaks = 2)
data$firep_T <- cut(data$firep, breaks = 2)
data$rooms_T <- cut(data$rooms, breaks = 2)

# Visualisation avec coplot
coplot(price ~ lingA | baths_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | age_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | lotS_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | lValue_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | pctCol_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | beds_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | firep_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data = data)

coplot(price ~ lingA | rooms_T, 
       panel = function(x, y, ...) {
         points(x, y, pch = 16)
         abline(lm(y ~ x), col = "red", lwd = 2)
       },
       data=data)

## ------- Interpretation

    # Les coplots confirment aussi que baths_T et rooms_T présentent les interactions les plus nettes, 
    # avec des pentes bien distinctes. beds_T et pctCol_T montrent un effet modéré, tandis que firep_T, age_T,
    # lotS_T et lValue_T n’indiquent aucune interaction visuelle marquée.

#Visuellement, baths_T est donc la meilleure variable modératrice pour lingA sans considération statistique avancée.



# Models d'interaction avec la variable retenue( baths_T)

modele_interaction <- lm(price ~  lotS + age + lValue + lingA* baths_T, data = data)
summary(modele_interaction)


## ------- Interpretation

    # L’interaction entre lingA et baths_T est statistiquement significative (p < 0.001), 
    # avec un effet négatif (-1.366e-04). Cela signifie que l’effet de lingA sur price dépend du niveau de baths : 
    # lorsque le nombre de salles de bains augmente, l’impact de la surface habitable (lingA) sur le prix diminue.
    
    # Ce modèle confirme une interaction significative, , mais son R2 ajusté (0.5571) est légèrement inférieur à celui du modèle sous-ensemble sans interaction (0.5588).
    # Il est donc moins performant, malgré l’effet interaction détecté.



# Comparaison avec Anova

anova(model_sous_ensemble ,modele_interaction)


## ------- Interpretation

      # Le test de Fisher compare le modèle avec interaction à celui sans. Le RSS augmente légèrement (de 155.78 à 156.28) 
      # après ajout de l’interaction, ce qui donne une somme des carrés négative (–0.51).
      # --> Cela signifie que l’interaction n’améliore pas l’ajustement global du modèle.
      # --> Même si l’interaction est significative dans le résumé du modèle, elle n’apporte pas de gain statistique global 
      # selon l’ANOVA.



## Interprétation des coefficients.

#Équation estimée du modèle :

    #log (price) = 11.32 + 0.03836·lotS – 0.001487·age + 0.000003722·lValue + 0.0004128·lingA + 0.354·baths_T – 0.0001366·(lingA × baths_T)

#Codage de baths_T :

    #baths_T = 0 ----> pour les Maison avec  nomnre de salle de bains ≤ 2.25  (groupe de référence)
    #baths_T = 1 ----> pour Maison avec un  nomnre de salle de bains > 2.25 (groupe modifié)

# Variables utilisées (valeurs réalistes et constantes dans les deux cas---> le choix des valeurs est a titre d'exemple) :

    # lotS = 0.5 (terrain moyen)
    # age = 20 (âge moyen)
    # lValue = 80 000 (valeur foncière typique)
    # lingA = 1500 (surface habitable moyenne)

# Resultat

    # Cas 1 : Maison avec  nomnre de salle de bains ≤ 2.25 ---->  (baths_T = 0)
    # log(price) = 11.32 + 0.019 – 0.03 + 0.2976 + 0.0004128 × 1500
    #            = 11.32 + 0.019 – 0.03 + 0.2976 + 0.6192
    #            = 12.2261

    # Cas 2 : Maison avec un  nomnre de salle de bains > 2.25 ---->  (baths_T = 1)
    # Seules les composantes baths_T et l’interaction changent :
    # log(price) = 11.32 + 0.019 – 0.03 + 0.2976 + 0.354 + 0.0002762 × 1500
    #            = 11.32 + 0.019 – 0.03 + 0.2976 + 0.354 + 0.4143
    #            = 12.3752

# Interpretation:

      # La maison ayant plus de salles de bains (>2,25) a un  log prix supérieur (12.3752 vs 12.2261), 
      # soit une hausse d’environ 5,5 %.

      # Toutefois, l’effet de la surface habitable (lingA) devient plus faible quand le nombre de salles de bais >2.25 :
      # le coefficient passe de 0.000413 à 0.0002764.

      # Cela signifie que chaque mètre carré rapporte moins lorsque la maison dispose déjà de plusieurs salles de bains.
      # ---> L’effet de lingA dépend donc du niveau de baths, ce qui confirme l’existence d’une interaction modératrice.



# Question 9 --------------------------------------------------------------

# Prediction avec BIC


xnew_sous_ensemble <- data.frame(
  lotS = 1,
  age = 25,
  lValue = 14000,
  lingA = 1000,
  baths = 1.5)
# Pour le modèle sous_ensemble
predict(model_sous_ensemble, newdata = xnew_sous_ensemble , interval = "prediction")

exp(11.83322) 
exp(11.2427)  
exp(12.42374) 
  /
## ------- Interpretation

# Le modèle sous-ensemble prédit un prix de vente d’environ 137 753 $ pour la maison donnée. 
# L’intervalle de prédiction, compris entre 76 321 $ et 248 635 $, reflète l’incertitude liée à cette estimation.
# Cet intervalle large indique une variabilité importante, mais la prédiction reste utile pour guider les décisions.


,
##### conclusion generale 

          # Au terme de cette analyse, plusieurs modèles de régression ont été évalués afin d’expliquer le prix 
          # des maisons. Le modèle sélectionné selon le critère BIC s’est révélé le plus adapté, en offrant un bon compromis entre 
          # performance et simplicité. Bien que l’ajout d’un terme quadratique ait permis une légère amélioration, nous avons 
          # privilégié la simplicité. L’effet d’interaction, quant à lui, n’a pas apporté de gain global. Le modèle BIC a donc 
          # été retenu comme modèle final, pour sa robustesse et son interprétabilité.



