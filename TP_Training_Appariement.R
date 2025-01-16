
# auto test, entraînement objectif partiel 

#=================================== Exercice 1 

avant <- c(167, 150, 175, 163, 149, 165, 168, 177, 140, 156, 162, 205)
apres <- c(127, 134, 131, 136, 123, 170, 135, 126, 144, 139, 129, 150)


#------1. Afficher les boxplots des deux Ã©chantillons de pression artÃ©rielle 

boxplot(avant,apres, names=c("avant","apres"),main="Pression arterielle avant et apres traitement")

#------2. On souhaite évaluer l'association entre le traitement et l'hypertension artérielle dans la population
#      d'hypertendus dont est issu l'échantillon. On fixera un risque de première espèce de 5%.

#--- (a) Donnez l'âge de l'estimation de la différence moyenne de pression artérielle de la
# population dont est issu notre échantillon, avec intervalle de confiance à 95%, en justifiant

# Je définis mon data frame 
df <- data.frame(
  avant = c(167, 150, 175, 163, 149, 165, 168, 177, 140, 156, 162, 205),
  apres = c(127, 134, 131, 136, 123, 170, 135, 126, 144, 139, 129, 150)
)

# je creer ma nouvelle colonne et calcul la moyenne et variance en conséquence
df$delta <- df$apres - df$avant 

Moy_diff <- mean(df$delta)
Moy_diff # -27.75

Var_diff <- var(df$delta)
Var_diff # 371.8409

n <- length(df$delta)
n # 12 

# Conditions du TCL 
# n > 30 or ici il n'y a que 12 patients donc condition non valide
# Vérifions si les données suivent loi normale avec un QQPLOT pour pouvoir faire IC avec Loi de Student 

qqnorm(df$delta)
qqline(df$delta)

# Semble suivrent loi normale nous pouvons ainsi utilisé student 
# soit 
moyenne + qt(c(0.025, 0.5, 0.975), n - 1) * sqrt(variance / n) # L'IC de la différence

# [-40.00195 -27.75000 -15.49805]

# soit 
##=== en precisant a R que les données sont appariées avec paired = TRUE , appariement important à prendre en compte.
t.test(df$apres, df$avant, paired = TRUE, conf.level = 0.95)
# -40.00195 -15.49805


#---(b) Précisez l'hypothèse nulle que vous souhaitez tester, ainsi que l'hypothèse alternative

# H0 : md = 0 ou mu avant = mu apres donc pas de d'association entre traitement et pression arterielle
# H1 : md != 0 donc association 

#---(c) Quel test pourriez-vous proposer pour répondre à cette question ?

# on peut faire : test z , student et wilcoxon si conditions valides. 


#---(d) Sous quelle(s) condition(s) le test choisi est-il valide ?

# si on choisi test z il faut que n > 30 ce qui n'est pas le cas
# donc test de student et ou wilcoxon 

# student condition de validité : di suivent loi normale, qqplot le confirme
# wilcoxon test non parametrique donc pas de condition

#---(e) Doit-il être apparié ?

# Oui car ce sont les mêmes patients, cas de paired data


#==== test student 

t.test(apres,avant,paired=TRUE)
# t = -4.9851, df = 11, p-value = 0.0004121

# valeur seuil
qt(0.975, df=11) # 2.20

# valeur absolue t > a valeur seuil donc on rejette H0 

# et p value inferieure à 0.05 donc on rejette bien H0 le test est significatif 

# au risque 5% il y a bien association entre traitement et la pression arterielle

#==== test de wilcoxon

wilcox.test(apres,avant,paired=T)
# V = 3, p-value = 0.005338



##================================== Exercice 2 ==================================

setwd("C:/Users/gerar/OneDrive/Bureau/MASTER SP/Stat_TP_S1_M1/TP_11_Stat_M1S1")
df_asthme <- read.csv2("dataset_ex2.csv")

#---1. Décrire les données (nombres de lignes et de colonnes, afficher les premières lignes, 
#     types des colonnes, distribution des colonnes, nombres de valeurs manquantes)


nrow(df_asthme) # 500 lignes donc 500 obs
ncol(df_asthme) # 5 colonnes donc 5 VA
head(df_asthme)
str(df_asthme)
summary(df_asthme)
View(df_asthme)

#------2. On souhaite déterminer s'il existe une diffÃ©rence d'amélioration entre les traitements A et B.

#---(a) Donnez l'estimation de la proportion d'mÃ©lioration en fonction du médicament 
# dans la population dont est issu notre Ã©chantillon, avec l'intervalle de confiance à 95% 

# faut que je fasse pour traitement A et traitement B 

# Je dois estimer prop_amelio_A et prop_B_amelio et m'assurer que le TCL est valide

# Traitement A 

n_A <- table(df_asthme$A)
n_A # Amélioration    Pas d'amélioration 
#      191                 309 

ntot <- 500

prop_A_amelio <-  191/ntot # nb amelio A / ntot
prop_A_amelio # 0.382


# TCL verifions condition npi >=5 et n(1-pi)>=5

ntot*0.382 # 191 ok
ntot*(1-0.382) # 309 ok 
alpha <- 0.05
zalpha <- qnorm(1-alpha/2)


# IC traitement A amelioration 95%

IC_lower <- prop_A_amelio - zalpha *sqrt(prop_A_amelio*(1-prop_A_amelio)/ntot)
IC_upper <- prop_A_amelio + zalpha *sqrt(prop_A_amelio*(1-prop_A_amelio)/ntot)
IC <- c(IC_lower,IC_upper)
IC # 0.3394118 0.4245882


# Traitement B 

table(df_asthme$B)

# Amélioration   Pas d'amélioration 
# 196                 304 


prop_B_amelio <- 196/ntot 
prop_B_amelio # 0.392

IC_lower <- prop_B_amelio - zalpha *sqrt(prop_B_amelio*(1-prop_B_amelio)/ntot)
IC_upper <- prop_B_amelio + zalpha *sqrt(prop_B_amelio*(1-prop_B_amelio)/ntot)
IC <- c(IC_lower,IC_upper)
IC # 0.3492085 0.4347915


#---(b) PrÃ©cisez l'hypothèse nulle que vous souhaitez tester, ainsi que l'hypothèse alternative.

# H0 : pi A = pi B Pas de diffÃ©rence d'amélioration entre les traitements A et B.
# H1 : piA != piB Difference 

#---(c) Quel test pourriez-vous proposer ?

# ce sont deux va qualitative et appariées donc test KHI2 Mc Nemar 

#---(d) Sous quelle(s) condition(s) le test choisi est-il valide ?
# condition : b+c/2 >=5 

table(df_asthme$A,df_asthme$B)
(113+118)/2 # 115.5 condition validitée

#---(e) Appariement ? 

# oui car paired data 

#--- (f) Application sur R : Indiquez, le cas Ã©chÃ©ant, la valeur de la statistique de test.
#--- (g) Application sur R : Quel est le degrÃ© de signification ? Que concluez-vous 

mcnemar.test(matrix(c(78,118,113,191),2,2),correct=F)
# McNemar's chi-squared = 0.10823, df = 1, p-value = 0.7422

# valeur seuil = 3.84 
# valeur stat inferieure à la valeur seuil donc non rejet de H0 
# pvalue superieur à 0.05 test non significatif on ne rejette pas H0 
# on conclut au risque 5% qu'il n'y a pas de différence entre les traitements A et B 


#-----3. On souhaite déterminer s'il existe une différence d'amélioration entre les traitements A et B 
#        chez les femmes

nb_tot_femme <- sum(df_asthme$sexe=="F") # 239

#---(a)Donnez l'estimation de la proportion d'mÃ©lioration chez les femmes en
# fonction du médicament dans la population dont est issu notre Ã©chantillon, avec l'ntervalle
# de confiance à 95%

table(df_asthme$sexe)
#  F   M 
# 239  261 

table(df_asthme$sexe=="F",df_asthme$A)
#       Amélioration      Pas d'amélioration
#FALSE            76                 185
#TRUE            115                 124

  
nb_femme_amelio_A <- 115 


table(df_asthme$sexe=="F",df_asthme$B)
#              Amélioration    Pas d'amélioration
#FALSE            72                 189
#TRUE            124                 115

nb_femme_amelio_B <- 124


prop_femme_amelio_A <- nb_femme_amelio_A/nb_tot_femme
prop_femme_amelio_A # 0.4811715

prop_femme_amelio_B <- nb_femme_amelio_B/nb_tot_femme
prop_femme_amelio_B # 0.5188285

# TCL ok 

# IC traitement A 

IC_lower <- prop_femme_amelio_A - zalpha *sqrt(prop_femme_amelio_A*(1-prop_femme_amelio_A)/nb_tot_femme)
IC_upper <- prop_femme_amelio_A + zalpha *sqrt(prop_femme_amelio_A*(1-prop_femme_amelio_A)/nb_tot_femme)
IC <- c(IC_lower,IC_upper)
IC # 0.4178267 0.5445164

# IC traitement B 

IC_lower <- prop_femme_amelio_B - zalpha *sqrt(prop_femme_amelio_B*(1-prop_femme_amelio_B)/nb_tot_femme)
IC_upper <- prop_femme_amelio_B + zalpha *sqrt(prop_femme_amelio_B*(1-prop_femme_amelio_B)/nb_tot_femme)
IC <- c(IC_lower,IC_upper)
IC # 0.4554836 0.5821733 


# Les données sont encore appariées car ce sont les memes femmes qui ont les 2 traitements 

# Faisons test khi2 Mc Nemar 


# H0 : pi avant = pi apres 
# H1 : pi avant != pi apres

# test khi2 mc nemar
# condition b+c/2 >=5 

df_femme <- df_asthme[df_asthme$sexe=="F",]
df_femme

table(df_femme$A,df_femme$B)
#                     Amélioration         Pas d'amélioration
#Amélioration                  56                  59
#Pas d'amélioration            68                  56

(59+68)/2 # 63.5 ok 

mcnemar.test(matrix(c(56,68,59,56),2,2),correct=F)
# McNemar's chi-squared = 0.6378, df = 1, p-value = 0.4245

# valeur stat inferieure à la valeur seuil 3.84 donc non rejet de H0 
# pvalue supérieure à 0.05 donc non rejet de H0 car test non significatif 
# on conclut qu'il n'y a pas de difference entre les deux traitements chez les femmes AU RISQUE 5%


#------4. On souhaite dÃ©terminer s'il existe une différence d'amélioration avec le traitement B 
#         selon le sexe du patient.

#---(a) Donnez l'estimation de la proportion d'amélioration sous médicament B
#en fonction du sexe dans la population dont est issu notre échantillon, avec l'intervalle de
#confiance à 95%

table(df_asthme$sexe)
# F   M 
# 239 261

nb_tot_femme <- 239
nb_tot_homme <- 261

df_femme
df_homme <- df_asthme[df_asthme$sexe=="M",]

table(df_femme$B)
# Amélioration    Pas d'amélioration 
#   124                 115

table(df_homme$B)
# Amélioration    Pas d'amélioration 
#  72                 189

prop_femme_amelio_B #  0.5188285

prop_homme_amelio_B <- 72/nb_tot_homme
prop_homme_amelio_B # 0.2758621


# IC traitement femme B 

IC_lower <- prop_femme_amelio_B - zalpha *sqrt(prop_femme_amelio_B*(1-prop_femme_amelio_B)/nb_tot_femme)
IC_upper <- prop_femme_amelio_B + zalpha *sqrt(prop_femme_amelio_B*(1-prop_femme_amelio_B)/nb_tot_femme)
IC <- c(IC_lower,IC_upper)
IC # 0.4554836 0.5821733 

# IC traitement homme B 

IC_lower <- prop_homme_amelio_B - zalpha *sqrt(prop_homme_amelio_B*(1-prop_homme_amelio_B)/nb_tot_homme)
IC_upper <- prop_homme_amelio_B + zalpha *sqrt(prop_homme_amelio_B*(1-prop_homme_amelio_B)/nb_tot_homme)
IC <- c(IC_lower,IC_upper)
IC # 0.2216390 0.3300852


# Plus d'appariement ici car selon le sexe et les femmes et hommes ne sont pas dependants  

##==== test de l'ecart reduit ou khi-2 ou Fisher
# application de ces divers tests : 

#=======test khi 2 d'indépendance pour échantillons non appariés

# si le choix du test est le khi2 d'independance : on regarde s'il y a association 
# entre l'amelioration du traitement B et le sexe du patient 

# Ainsi H0 : les variables sont indépendantes , aucune association entre elles
# H1 : les variables sont dépendantes , association entre elles 

# condition de validité : cij >= 5 
# condition validité 
test_khi2$expected # ok
 
test_khi2 <- chisq.test(df_asthme$B,df_asthme$sexe, correct=F)
test_khi2 # X-squared = 30.901, df = 1, p-value = 2.715e-08

# comparaison avec distrib X^2 

qchisq(0.95, df=1) # 3.84 

# valeur stat supérieure à la valeur seuil donc rejet de H0 
# p value inférieure a 0.05 donc rejet de H0 

# les variables sont dépendantes au risque 5% on conclut à une différence selon le sexe pour traitement B


#===== test fisher 

fisher.test(df_asthme$B, df_asthme$sexe)

# p-value = 3.207e-08
# pas de condition de validité et pas de valeur stat direct conclure avec p value 


# === test z de l'ecart reduit a present pour 2 échantillons, notion de prop_commune

# H0 : pi homme = pi femme 
# H1 : pi homme != pi femme

propA <- prop_femme_amelio_B
propB <- prop_homme_amelio_B

# simplifions l'écriture

# nA <- c'est: nb_tot_femme <- 239
# nB <- c'est: nb_tot_homme <- 261

nA <- 239
nB <- 261

prop_commune <- (nA * propA + nB * propB) / (nA + nB)
prop_commune # 0.392
nA * c(prop_commune, 1 - prop_commune)
nB * c(prop_commune, 1 - prop_commune)

stat_z <- (propA - propB) / sqrt(prop_commune * (1 - prop_commune) * (1 / nA + 1 / nB))
# 5.558858 
stat_z

pvalue <- 2*(1-pnorm(abs(stat_z)))
pvalue # 2.715458e-08


###==================   Exercice 3 ====================================

# Le nombre d'hypoglycémies (glycémie <70 mg/dl) a Ã©tÃ© mesurÃ© durant 2 mois chez 12 patients souffrant
# de diabète et soumis successivement à 3 médicaments.

mdct_A <- c(21, 22, 20, 20, 24, 28, 28, 20, 24, 25, 23, 21)
mdct_B <- c(15, 12, 19, 16, 18, 23, 11, 17, 14, 20, 16, 23)
mdct_C <- c(13, 11, 23, 22, 16, 7, 19, 14, 15, 12, 18, 20)

n <- 12

#-------1. Existe-t-il une association entre ces 3 traitements et le nombre d'hypoglycémies ?

#---(a)Donnez l'estimation du nombre moyen d'hypoglycémies en fonction des
# traitements dans la population dont est issu notre échantillon, avec IC 95%

(moy_A <- mean(mdct_A))
(moy_B <- mean(mdct_B))
(moy_C <- mean(mdct_C))

(var_A <- var(mdct_A))
var_B <- var(mdct_B)
var_C <- var(mdct_C)


# TCL : n> 30 or ici n = 12 donc on en peut pas faire l'IC  
# en revanche nous pouvons vÃ©rifier si les données suivent loi normale 
# ce qui nous permettra de faire IC selon student 

# verifions avec qqplot 

qqnorm(mdct_A);qqline(mdct_A) # OK
qqnorm(mdct_B);qqline(mdct_B) # OK
qqnorm(mdct_C);qqline(mdct_C) # OK

# IC pour medoc A 

moy_A + qt(c(0.025,0.5,0.975), n-1)*sqrt(var_A/n) # 21.16251 23.00000 24.83749

# IC pour medoc B
moy_B + qt(c(0.025,0.5,0.975), n-1)*sqrt(var_B/n) # 14.5617 17.0000 19.4383

# IC pour medoc C
moy_C + qt(c(0.025,0.5,0.975), n-1)*sqrt(var_C/n) # 12.80635 15.83333 18.86032

#---(b) Précisez l'hypothÃ¨se nulle que vous souhaitez tester, ainsi que l'hypothèse alternative.

# HO : moy A = moy B = moy C 
# H1 : au moins une différence

#---(c) Quel test pourriez-vous proposer pour rÃ©pondre Ã  cette question ?

# il y a plus de deux traitements j'opte pour le test non parametrique de Friedman 

(matrice_hypo <- matrix(c(mdct_A, mdct_B, mdct_C), ncol = 3))
(matrice_rangs <- t(apply(matrice_hypo, 1, rank)))
(somme_rangs <- colSums(matrice_rangs))
# vérifions s'il y a ex aequos
any(apply(matrice_rangs, 1, function(x) length(unique(x)) != 3)) # Pas d'ex aequos

friedman.test(matrice_hypo)
# Friedman chi-squared = 10.167, df = 2, p-value = 0.006199

# comparer avec valeur seuil 
qchisq(0.95,2) # 5.99
# valeur stat supérieure à la valeur seuil donc rejet de H0 
# p value inférieure à 0.05 donc test significatif donc rejet de H0 et on conclut 
# qu'il y a au moins une des ditributions differentes , au risque 5% 


#------2. Existe t-il une association entre les traitements B et C, et le nombre d'hypoglycémies ?

#---(a) Précisez l'hypothèse nulle que vous souhaitez tester, ainsi que l'hypothèse alternative
# H0 : mu B = mu C , pas de différence donc pas d'association ou distributions homogenes 
# H1 : association 


# Ici il y a deux traitements 
# Appariement car les patients ayant B et C sont les mêmes , paired data 
# ce sont des va quantitatives donc : 
# 2 ech , va quantitatives , appariées , donc : test z ecart reduit ? non car n<30 car n=12

# donc wilcoxon pour échantillons appariés ou student 


###==== wilcoxon apparié 

wilcox.test(mdct_B,mdct_C, paired=T) # V = 45, p-value = 0.6653

###==== t test appariÃ© (student)

delta <- mdct_B - mdct_C
# ou je fais simplement avec mct B et mct C  

t.test(mdct_B,mdct_C, paired=T)

#=========================== Exercice 4 ========================================

#***La base de données velo.txt a été constituée de façon à évaluer l'impact de l'effort sur la concentration
#sanguine en PSA (Prostatic Specific Antigen) qui est utilisé entre autre pour le diagnostic de cancer de la
#prostate. 

setwd("C:/Users/gerar/OneDrive/Bureau/MASTER SP/Stat_TP_S1_M1/TP_11_Stat_M1S1")
df_psa <- read.table("velo.txt", sep = "\t", header = TRUE, dec = ",")

View(df_psa)

#*****1. Le taux de PSA différe-t-il avant et après l'effort ? (psaavt et psaapt)

#---(a)Donnez l'estimation de la diffÃ©rence moyenne de PSA entre avant et après
# l'exercice dans la population dont est issu notre échantillon, avec l'intervalle de confiance à 95%


# avant et apres donc on création d'une nouvelle variable delta pour prendre en compte l'appariement. 

df_psa$delta <- df_psa$psaapt - df_psa$psaavt

moy_diff <- mean(df_psa$delta)
moy_diff # -0.007744361

n <- length(df_psa$delta) # 133
n
(n <- sum(!is.na(difference))) # ok pas de NA 

# TCL ok car n > 30 car n = 133 

var_diff <- var(df_psa$delta)
var_diff # 0.7510797

# IC 95%
alpha <- 0.05
zalpha <- qnorm(1-alpha/2)

IC_lower <- moy_diff - zalpha * sqrt(var_diff/n)
IC_upper <- moy_diff + zalpha * sqrt(var_diff/n)
IC <- c(IC_lower, IC_upper)
IC # -0.1550317  0.1395429

#---(b) 

# H0 : mu avant = mu apres ou mu_d = 0 
# H1 : mu avant != mu apres ou mu_d != 0 


#---(c) Quel test pourriez-vous proposer pour répondre à cette question ?

# ce sont des va quantitatives et appariées 
# donc je peux faire test z de l'ecart reduit et test de student 


#==== test z de l'ecart reduit pour echantillons appariés 
# conditions de validité 

# n > 30  ok 

zobs <- moy_diff / sqrt(var_diff/n)
zobs # -0.1030548

pvalue <- 2 * (1 - pnorm(abs(stat_z)))
pvalue # 0.9179194

# comparaison avec valeur seuil 1.96 
# valeur abs de zobs 0.10 inférieure à valeur stat donc non rejet de H0 
# pvalue supérieure à 0.05 donc test non significatif on ne rejette pas H0 
# on conclut donc au risque 5% qu'il n'y a pas de différence apres et avant le traitement.


