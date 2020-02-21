########################################################################################################
#L'objectif de ce script est d'ouvrir les trois bases de données
#Afin d'identifier les variables qui se trouvent dans un ou plusieurs de ces trois bases afin de faire un seule est unique base 
#Ensuite une description de la population est réalisée

#ON GARDE CEUX QUI ONT DE L'ASTHME AUX DERNIERES NOUVELLES

########################################################################################################

#_PACKAGES A INSTALLÉS POUR LANCER CE SCRIPT----

install.packages("tidyverse")
install.packages("haven") # permet d'importer une base de données depuis sas
install.packages("labelled") # permet de gérer les labels qui viennent de sas sur R 

library(tidyverse)
library(sas7bdat)
library(haven) # permet d'importer un document sas
library(labelled) # permet de garder le label lors d'un import d'une base de données sas
library(epiDisplay)# permet de décrire les variables avec tab1 par exemple
library(readxl) #pour ouvrir un document excel pour le recodage des variables
library(epiR) # permet d'utiliser notament la fonction epi.descriptives 
library(lubridate)

#######################################################################################################
# BASE DE DONNÉES TRANSMISE PAR ANNA 

        setwd("~/Documents/M2_NaiaRozès_2020/1_Data_sources/EGEA2_Anne Boudier")
        egea2_pheno_03fev20<- read_sas("egea2_pheno_03fev20.sas7bdat")
        #View(egea2_pheno_03fev20)
        #look_for(egea2_pheno_03fev20)
        length(table(egea2_pheno_03fev20$identnum)) # il y a 698 personnes asthmes aux dernieres nouvelles
        
        #les personnes ont bien toutes de l'asthme aux dernieres nouvelles
        
#Supprimer les sujets qui ont des DM sur le score AQLQ total ----
        
        egea2_pheno_03fev20 <- egea2_pheno_03fev20[which(!is.na(egea2_pheno_03fev20$WD14099)), ]
        length(table(egea2_pheno_03fev20$identnum))
                          #il y a 593 indivividus asthmatiques qui ont des données d'AQLQ total
                          # 105 individus ont été exclus car ils n'avaient pas de données pour l'AQLQ total
                        
# Recodage/calcul de certaines variables ----

# Calcul de la variable ville d'inclusion (appelee ensuite "centre")----

         egea2_pheno_03fev20$centre <- ifelse(egea2_pheno_03fev20$V1 %in% c(1, 5, 6, 8), 1,    # Paris
                                     ifelse(egea2_pheno_03fev20$V1 == 2, 2,          # Lyon    
                                            ifelse(egea2_pheno_03fev20$V1 == 3, 3,   # Marseille
                                                   ifelse(egea2_pheno_03fev20$V1 == 4, 4,  # Montpellier
                                                          ifelse(egea2_pheno_03fev20$V1 %in% c(7, 9), 5, NA)))))  # Grenoble


        
# RECODAGE DES VARIABLES ----
        
# un document excel a été créé afin d'indiquer les codes d'origine et le label des différentes variables ainsi que le nom simplifier
# ensuite ce tableau est chargé sur R ce qui permet d'avoir le recodage des variables directement et non d'avoir à les recoder une à une à la main


          cor_noms_variables <- read_excel("Correspondance_variable_05022020.xlsx") 
          cor_noms_variables <- as.data.frame(cor_noms_variables)
          
          colnames(egea2_pheno_03fev20) <- cor_noms_variables$APRES[match(colnames(egea2_pheno_03fev20),
                                                                          cor_noms_variables$AVANT)]
          
          rm(cor_noms_variables)

          #View(egea2_pheno_03fev20)
          #summary(egea2_pheno_03fev20)
          
          #dim(egea2_pheno_03fev20)
          #head(egea2_pheno_03fev20)




################################################################################
# BASES DE DONNÉES TRANSMISES PAR ALICIA----

        setwd("~/Documents/M2_NaiaRozès_2020/1_Data_sources/EGEA2_Alicia Guillien")

    # EGEA Q ----

        egea2_Q_20200302 <- readRDS("~/Documents/M2_NaiaRozès_2020/1_Data_sources/EGEA2_Alicia Guillien/egea2_Q_20200302.rds")
        #dim(egea2_Q_20200302)
        #head(egea2_Q_20200302)
        #summary(egea2_Q_20200302)

    #EGEA SIG----

        egea2_SIG_20200302 <- readRDS("~/Documents/M2_NaiaRozès_2020/1_Data_sources/EGEA2_Alicia Guillien/egea2_SIG_20200302.rds")
        #dim(egea2_SIG_20200302)
        #head(egea2_SIG_20200302)
        #summary(egea2_SIG_20200302)
          
    #EGEA2 DATES----
        egea2_dates <- readRDS("~/Documents/M2_NaiaRozès_2020/1_Data_sources/EGEA2_Alicia Guillien/egea2_dates copie.rds")

----------
#QUESTION    
  # comment identifier proprement les variables qui se trouvent sur plusieurs bases ?
      #identifier les variable crées par Alicia fichier : correspondances_variables07mars19.xlxs
  #est ce que je renomme les variables par leur label pour la base egea pheno ?
     #oui renommer les variables de la table egea2_pheno, garder la tracabilité sur un document excel
  
###############################################################################################@@
#FUSION DES BASES DE DONNÉES----

           egea2_complet <- NULL # je crée une nouvelle table egea2_complet qui sera ma table qui regroupe les trois bases de données pheno, Q et SIG

    # _Variables phenotypes----

          egea2_complet <- egea2_pheno_03fev20 # je mets les variables de pheno dans ma nouvelle table
          #View(egea2_complet)

    # _Variables d'expos issues des questionnaires----

            egea2_complet <- merge(x = egea2_complet,       # #je me sers des identifiants communs aux différentes tables comme clef pour les joindre
               y = egea2_Q_20200302,
               by.x = "identnum",
               by.y = "identnum")



      # _Variables d'expos SIG

            egea2_complet <- merge(x = egea2_complet, y = egea2_SIG_20200302, by.x = "identnum", by.y = "identnum")
            # il y a 535 individus asthmatiques, avec score de AQLQ total avec données SIG
            
            #summary(egea2_complet)
            #View(egea2_complet)
            
        #_Variables dates
            egea2_complet <- merge(x = egea2_complet, y = egea2_dates, by.x = "identnum", by.y = "identnum")
            
#____Creation variable saison
            # Fonction permettant de renvoyer le nom de la saison ("été"/"automne"/"hiver"/"printemps") 
            # à partir d'une date 
            # Attention : la fonction ne prend pas en compte l'année de la date
            # et donc les dates de début et fin de saison sont les mêmes quelle que soit l'année
            # et sont les suivantes : 
            # 1 = printemps : du 20/03 au 20/06 inclus
            # 2 = été : du 21/06 au 22/09 inclus
            # 3 = automne : du 23/09 au 21/12 inclus
            # 4 = hiver : du 22/12 au 19/03 inclus
            
            getSaison <- function(date_EFR){
              if (is.na(date_EFR)) {
                saison <- NA
              } else {
                d <- as.numeric(format(date, "%d"))
                m <- as.numeric(format(date, "%m"))
                if (m < 3 | (m == 3 & d < 20)) {
                  saison <- 4
                } else {
                  if (m < 6 | (m == 6 & d < 21)) {
                    saison <- 1
                  } else {
                    if (m < 9 | (m == 9 & d < 23)) {
                      saison <- 2
                    } else {
                      if (m < 12 | (m == 12 & d <  22)) {
                        saison <- 3
                      } else {
                        saison <- 4
                      }
                    }
                  }
                }
              }
              return(saison)
            }
           
#View(egea2_complet$saison)

#____ VERIFICATION ECART DATA
            
            egea2_complet$délaisEFR <- (egea2_complet$date_EFR) - (egea2_complet$date_Q)
            tab1(egea2_complet$délaisEFR)
            
#____ECART DATE quali----
            egea2_complet$délaisEFR_class <- rep(NA,length(egea2_complet$délaisEFR))
            egea2_complet$délaisEFR_class[which(egea2_complet$délaisEFR<0)]<- 0  # exmanen EFR avant questionaire Q
            egea2_complet$délaisEFR_class[which(egea2_complet$délaisEFR==0)]<- 1 # pas d'écart 
            egea2_complet$délaisEFR_class[which(egea2_complet$délaisEFR>0 & egea2_complet$délaisEFR<14)]<- 2 # 2 semaine d'écart
            egea2_complet$délaisEFR_class[which(egea2_complet$délaisEFR>=14 & egea2_complet$délaisEFR<28)]<- 3 #4semaines d'écart
            egea2_complet$délaisEFR_class[which(egea2_complet$délaisEFR>=28 & egea2_complet$délaisEFR<42)]<- 4 #6semaines d'écart
            egea2_complet$délaisEFR_class[which(egea2_complet$délaisEFR>=42 )]<- 5 #8semaines d'écart ou plus
            
            tab1(egea2_complet$délaisEFR_class)
            
# EXCLUSION DES VARIABLES "inutiles"----

            egea2_complet[, c("V1", 
                      "CC_hist_resid",
                      "avoir_chat",
                      "avoir_chien",
                      "animal_dom_enfance",
                      "chat_enfance",
                      "chien_enfance",
                      "comb_cuisine",
                     "degats_eaux_vie",
                      "degats_eaux_12dernmois",
                      "moisissures_maison",
                      "piece_moisie",
                     "WA64005",
                     "WA70165",
                     "WA82166",
                   "WD16023",
                     "WD16024",
                     "WD14089",
                     "WA94090",
                     "WA94092",
                  "WA84196",
                   "WA80485", 
                  "wk01007",
                   "wk01008",
                   "WA63040",
                   "WA80472 ",
                   "WA63050",
                   "wa83005",
                   "wa83006",
                   "wa83007",
                   "wa83008",
                   "wa83009 ",
                   "wa83010",
                   "wa83011",
                   "wa83012 ",
                   "wa83013",
                   "wa83014",
                   "wa83015",
                   "wa83016",
                   "wa83017",
                   "wa83018",
                   "wa83019",
                   "wa83020",
                   "wa83021",
                   "wa83022",
                   "wa83023",
                   "wa83024",
                   "wa83025",
                   "wa83026",
                   "wa83027",
                   "wa83028",
                   "wa83029",
                   "wa83030",
                   "wa83031",
                   "wa83032",
                   "wa83033",
                   "wa83034",
                   "wa83035",
                  "wa83036",
                  "wa83037",
                  "wa83038",
                  "wa83039",
                  "wa83040",
                  "WA80472",
                  "wa83009",
                  "wa83012",
                  "wa83041",
                  "wa83042",
                  "wa83043",
                  "wa83044",
                  "wa83045",
                  "wa83046",
                  "wa83047",
                  "wa83048",
                  "wa83049",
                  "wa83050",
                  "wa73013",
                  "wa73104")] <- NULL
                    
           
#View(egea2_complet)
            #_Nombre de famille----
            length(table(egea2_complet$identnum)) 

            
#CARACTÉRISTIUES PRINCIPALES DE LA POPULATION----

      #_Age----
            epi.descriptives(egea2_complet$age)$a
            boxplot(egea2_complet$age~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition de l'âge par centre")
      #_Taille----
            epi.descriptives(egea2_complet$taille)$a
            boxplot(egea2_complet$taille~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition de la taille par centre")
      #_Sexe----
            tab1(egea2_complet$sexe)  # 1: homme, 2: femme
            
      #_IMC - en quanti----
            epi.descriptives(egea2_complet$IMC)$a

# _Création de la variable IMC en classe ----
            egea2_complet$IMC_CLASS <- rep(NA,length(egea2_complet$IMC))
            
            egea2_complet$IMC_CLASS[which(egea2_complet$IMC<18.5)]<- 1  # dénutrition
            egea2_complet$IMC_CLASS[which(egea2_complet$IMC>=18.5 & egea2_complet$IMC<25)]<- 2 #normal
            egea2_complet$IMC_CLASS[which(egea2_complet$IMC>=25 & egea2_complet$IMC<30)]<- 3 #surpoids
            egea2_complet$IMC_CLASS[which(egea2_complet$IMC>30)]<- 4 #obésité
            
      #_IMC en quali ----
            tab1(egea2_complet$IMC_CLASS)
            
      #_Saison----
            tab1(egea2_complet$saison)
                                    # 1 = printemps : du 20/03 au 20/06 inclus
                                    # 2 = été : du 21/06 au 22/09 inclus
                                    # 3 = automne : du 23/09 au 21/12 inclus
                                     # 4 = hiver : du 22/12 au 19/03 inclus
            

      #_Catégorie socio profesionnelle----

            tab1(egea2_complet$CSP)  # 0:sans profession
                                     # 1 : cadre
                                     # 2 : technicien
                                     # 3 :  agriculteur ouvrier
           
      #_Niveau d'étude----
            tab1(egea2_complet$niv_etude)
                                      #0 : non diplomé
                                      #1 : CAP
                                      #2 :BEP/BAC
            
      #_Centre----
            tab1(egea2_complet$centre) #1 :Paris
                                       #2: Lyon
                                       #3: Marseille
                                       #4: Montpellier
                                       #5 : Grenoble
          
      #_Nombre de famille ----
            length(table(egea2_complet$NOFAMIL))  #356 familles
            
#____QUALITE DE VIE----
              
          #_AQLQ total----
            epi.descriptives(egea2_complet$AQLQ_total)$a
            
          #_AQLQ_AL----  
            #limitation d'activité
            
            epi.descriptives(egea2_complet$AQLQ_AL)$a
            
          #_AQLQ_EE ----
            #Exposition environnementale
            epi.descriptives(egea2_complet$AQLQ_EE)$a
          
          #_AQLQ_EF  ----
            #Fonction emotionnelle
            epi.descriptives(egea2_complet$AQLQ_EF)$a
          
          #_AQLQ_ S ----
            #Symptome
            epi.descriptives(egea2_complet$AQLQ_S)$a
           
            #write.table(egea2_complet, file = "egea2_complet.csv", sep = ",", col.names = NA, qmethod = "double")   
        
#____AIR INTERIEUR----
            
            #_Moisissures----
              
            tab1(egea2_complet$moisi_ch_sej_cuis)
            
              ## 1 si moisi dans la chambre, le sejour et/ou la cuisine
              # si jms eu de moisissure dans la maison ou si moisissure ailleurs que dans chambre/sejour/cuisine
            
            # _Dégats des eaux----
            
            tab1(egea2_complet$degats_des_eaux_12mois)
            
            # 1 si degat des eaux dans les 12 derniers mois
            # 0 si jms eu de degat des eaux ou si degat des eaux il y a plus de 12 mois
            
            #_Combustible cuisine gaz----
            
            tab1(egea2_complet$comb_gaz_cuisine)
            
            # _Nombre de spray javel par semaine----
            
            tab1(egea2_complet$javel_sem)
                  # 0 si <1 jour/sem,
                  # 1 si de 1 à 3 jour/sem,
                  # 2 si de 4 à 7 jour/sem.
            
            #_Nombre de spray par semaine----
            
            tab1(egea2_complet$sprays_sem)
              
                # 0 si non exposé,
                # 1 si utilisation d'un spray par semaine ,
                # 2 si utilisation d'au moins 2 sprays par semaine.
                
#____TABAC----          
            #_Statut tabagique----
            
            tab1(egea2_complet$statut_tabagique)
                  # 0 : Jamais fumeur,
                  # 1: Ancien fumeur,
                  # 2 : Fumeur actif.
      
            #_Nombre de paquet année----
            epi.descriptives(egea2_complet$PA)$a
            
            #_Nombre de paquet année en classe----
            tab1(egea2_complet$PA_4classes)
                  
                   # 0 si    0
                   # 1 si <10
                   # 2 si 10 - 20
                   #  3 si >20
            
            #_Tabagisme passif----
            tab1(egea2_complet$tabac_passif)
            
                    # 0 : aucun
                    # 1 :  intermédiaire
                    # 2 : haut
            
#____ANIMAUX DOMESTIQUES----
            
            #_Chien chat enfant----
            tab1(egea2_complet$chat_chien_enfance)
                 
                     # 0 si non,
                     #1 si oui.
            
            #_Chat chien maintenant----
            tab1(egea2_complet$chat_chien_mtn)
            
#____EXPOSITION PROFESIONNELLE A UN ASTHMAGÈNE----
            
            #_Exposition vie profesionnelle vie entière----
            tab1(egea2_complet$expo_prof_vie)
                          
                      #0 : aucune
                      #1 : basse
                      #2 : haute
            
            #_Exposition dernier emploi----
            tab1(egea2_complet$expo_prof_dern_emploi)
            
            
#____RURALITÉ----
            #_Né dans une commune rurale----
            tab1(egea2_complet$ne_com_rurale)
        
            #_Vivre dans une commune rurale----
            tab1(egea2_complet$ever_living_com_rurale)
            
#____ALIMENTATION ET ACTIVITE PHYSIQUE----   
           
            #_Alimentation----
            epi.descriptives(egea2_complet$AHEI_tot_v2)$a
            boxplot(egea2_complet$AHEI_tot_v2~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition du score AHEI par centre")
            #_Activité physique----
            epi.descriptives(egea2_complet$mets_tps_libre_travail)$a
            boxplot(egea2_complet$mets_tps_libre_travail~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition de l'intensite d'activité physique par centre")
#____POLLUTION ATMOSPHERIQUE----
           
             #_O3----
            epi.descriptives(egea2_complet$O3_Moy_A1_G)$a 
            
            #_NO2----
            epi.descriptives(egea2_complet$NO2_Moy_A1_G)$a
            
            #_PM25----
            epi.descriptives(egea2_complet$PM25_Moy_A1_G)$a
            #_PM10----
            epi.descriptives(egea2_complet$PM10_Moy_A1_G)$a

            par(mfrow=c(2,2))
            boxplot(egea2_complet$O3_Moy_A1_G~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Concentration en O3 par centre")
            boxplot(egea2_complet$NO2_Moy_A1_G~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Concentration en NO2  par centre")
            boxplot(egea2_complet$PM25_Moy_A1_G~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Concentration en PM2,5 par centre")
            boxplot(egea2_complet$PM10_Moy_A1_G~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Concentration en PM10 par centre")
            
#____ESPACE NATUREL----
            
            #_Espace vert----
            epi.descriptives(egea2_complet$dist_EV)$a
            epi.descriptives(egea2_complet$nb_EV_300m)$a
            
            
            #_Espace bleu----
            epi.descriptives(egea2_complet$dist_EB)$a
            epi.descriptives(egea2_complet$nb_EB_300m)$a
            
            par(mfrow=c(2,2))
            boxplot(egea2_complet$nb_EV_300m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Nombre d'espace vert par centre")
            boxplot(egea2_complet$nb_EB_300m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Nombre d'espace bleu par centre")
            
             #_Indice de régularité de Shannon----           
            epi.descriptives(egea2_complet$SEI_300m)$a
           
             hist(egea2_complet$SEI_300m)            
            boxplot(egea2_complet$SEI_300m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition de l'indice de Shannon par centre")
            
             #_Indice de végétation   NDVI----
            
            hist(egea2_complet$NDVI_100m)
            epi.descriptives(egea2_complet$NDVI_100m)$a
            epi.descriptives(egea2_complet$NDVI_300m)$a
            epi.descriptives(egea2_complet$NDVI_500m)$a
            
            par(mfrow=c(2,2))
            boxplot(egea2_complet$NDVI_100m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Indice de végétation (100m) par centre")
            boxplot(egea2_complet$NDVI_300m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Indice de végétation (300m) par centre")
            boxplot(egea2_complet$NDVI_500m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Indice de végétation (500m) par centre")
           
             #____ALTITUDE ----
           hist(egea2_complet$altitude)
           epi.descriptives(egea2_complet$altitude)$a         
           
           boxplot(egea2_complet$altitude~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition de l'altitude par centre")

#____POPULATION ET DENSITE DU BATI----
          
           #_Batit----
           hist(egea2_complet$BD_100m)
           epi.descriptives(egea2_complet$BD_100m)$a
           epi.descriptives(egea2_complet$BD_300m)$a
           
           par(mfrow=c(2,2))
           boxplot(egea2_complet$BD_100m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Densité du bâti (100m) par centre")
           boxplot(egea2_complet$BD_300m~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Densité du bâti (300m) par centre")
         
             #_Densité de population----
           hist(egea2_complet$dp_200x200m_nb_hab)
           
           epi.descriptives(egea2_complet$dp_200x200m_nb_hab)$a
           boxplot(egea2_complet$dp_200x200m_nb_hab~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition de densité de population par centre")

#____CLIMAT----
           #_Température----
           hist(egea2_complet$Tmin_A1)
           hist(egea2_complet$Tmoy_A1)
           hist(egea2_complet$Tmax_A1)
           
           epi.descriptives(egea2_complet$Tmin_A1)$a
           epi.descriptives(egea2_complet$Tmoy_A1)$a
           epi.descriptives(egea2_complet$Tmax_A1)$a
          
           par(mfrow=c(2,2))
           boxplot(egea2_complet$Tmin_A1~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= " T(°C) minimales par centre")
           boxplot(egea2_complet$Tmoy_A1~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= " T(°C)moyennes par centre") 
           boxplot(egea2_complet$Tmax_A1~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "T(°C)maximales par centre") 
           
           
           #_Vent---- 
           epi.descriptives(egea2_complet$FF_Q_A1)$a
           boxplot(egea2_complet$FF_Q_A1~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Vent par centre")
           
           
           #_Humidité annuelle relative----
           
           hist(egea2_complet$HU_Q_A1)
           epi.descriptives(egea2_complet$HU_Q_A1)$a
          
            #_Humidité annuelle absolue----
           hist(egea2_complet$HUM_ABS_A1)
           epi.descriptives(egea2_complet$HUM_ABS_A1)$a
           
           par(mfrow=c(2,2))
           boxplot(egea2_complet$HU_Q_A1~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Humidité annuelle relative par centre") 
           boxplot(egea2_complet$HUM_ABS_A1~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Humidité annuelle absolue par centre") 

#____ROUTES----
           #_Distance route----
           hist(egea2_complet$dist_route)
          epi.descriptives(egea2_complet$dist_route)$a
          
          #_Nombre de route dans une zone tampon de 100m----
          hist(egea2_complet$nb_routes)
          epi.descriptives(egea2_complet$nb_routes)$a

#____PISTES CYCLABLES----
          epi.descriptives(egea2_complet$nb_pc)$a
          tab1(egea2_complet$nb_pc)          
         
           par(mfrow=c(2,2))
          boxplot(egea2_complet$nb_pc~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Nombre de pistes cyclables par centre") 
          boxplot(egea2_complet$nb_routes~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Nombre de route par centre") 
          
#____ULTRA VIOLET  ----
          #_Annual DNA damage----
          epi.descriptives(egea2_complet$UV_A1_ddc)$a
          
          #_Annual erythemal UV dose----
          epi.descriptives(egea2_complet$UV_A1_dec)$a
          
          #_Annual vitamin D UV dose----
          epi.descriptives(egea2_complet$UV_A1_dvc)$a

          par(mfrow=c(2,2)) 
          boxplot(egea2_complet$UV_A1_ddc~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Dommage ADN annuel par centre") 
          boxplot(egea2_complet$UV_A1_dec~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Dose UV érythémale annuelle par centre") 
          boxplot(egea2_complet$UV_A1_dvc~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Dose UV vitamine D annuelle par centre") 
          
#____SERVICE DE SANTE DANS L IRIS ----
          epi.descriptives(egea2_complet$medecin_generaliste)$a
          epi.descriptives(egea2_complet$pneumologue)$a
          epi.descriptives(egea2_complet$autre_specialiste)$a
          epi.descriptives(egea2_complet$paramed)$a
          epi.descriptives(egea2_complet$pharmacie)$a
          epi.descriptives(egea2_complet$autres_services_sante)$a
          
          par(mfrow=c(3,3)) 
          boxplot(egea2_complet$medecin_generaliste~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Medecin généraliste par centre") 
          boxplot(egea2_complet$pneumologue$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Pneumologue par centre") 
          boxplot(egea2_complet$autre_specialiste~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Autres spécialistes par centre") 
          boxplot(egea2_complet$paramed~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Paramédicaux par centre") 
          boxplot(egea2_complet$pharmacie~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Pharmacie par centre") 
          boxplot(egea2_complet$autres_services_sante~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Autres services de santé par centre") 
          
          
#____ SCORE DE PEKKANEN----
          hist(egea2_complet$pekkanen_AQ)
          epi.descriptives(egea2_complet$pekkanen_AQ)$a
          epi.descriptives(egea2_complet$pekkanen_opt)$a
          
          par(mfrow=c(2,2)) 
          boxplot(egea2_complet$pekkanen_AQ~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Score de Pekkanen_AQ par centre") 
          boxplot(egea2_complet$pekkanen_opt~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Score de Pekkanen_AQ par centre") 
          
#____ASTHME----
          #_Ever asthma----
          
          as.factor(egea2_complet$ever_asthma)
          tab1(egea2_complet$ever_asthma)     
         
          #_Asthme derniere nouvelle-----
          
         # as.factor(egea2_complet$asthme_der_nouvelles)
          tab1(egea2_complet$asthme_der_nouvelles)  
          
          #_Asthme actuel----
          #as.factor(egea2_complet$asthme_actuel)
          tab1(egea2_complet$asthme_actuel)          
          
          #_Crise asthme dans les 12 derniers mois----
          #as.factor(egea2_complet$crise_asth_12dermois)
          tab1(egea2_complet$crise_asth_12dermois)

#____ALLERGIE----
          #_Papule----
          #hist(egea2_complet$allergie_papules)
          tab1(egea2_complet$allergie_papules)
          
          #_igE----
          #as.factor(egea2_complet$allergie_Ige)
          tab1(egea2_complet$allergie_Ige)   
          
#____TRAITEMENT----
          #_Corticoide inhalé dans les 12 derniers mois----
          #hist(egea2_complet$cortico_inhales_12m)
          #as.factor(egea2_complet$cortico_inhales_12m)          
          tab1(egea2_complet$cortico_inhales_12m)          
 
#____FONCTION VENTILLATOIRE----
          
          
          #_VEMS_preBD----
          hist(egea2_complet$VEMS_preBD)
          epi.descriptives(egea2_complet$VEMS_preBD)$a         
          
          #_CVF_preBD----
          hist(egea2_complet$CVF_preBD)
          epi.descriptives(egea2_complet$CVF_preBD)$a
          
          #_VEMSsurCVF_preBD----
          hist(egea2_complet$VEMSsurCVF_preBD)
          epi.descriptives(egea2_complet$VEMSsurCVF_preBD)$a
          
          #_VEMS_p100GLI----
          hist(egea2_complet$VEMS_p100GLI)
          epi.descriptives(egea2_complet$VEMS_p100GLI)$a
          
          #_CVF_p100GLI----
          hist(egea2_complet$CVF_p100GLI)
          epi.descriptives(egea2_complet$CVF_p100GLI)$a
          
          #_VEMSsurCVF_p100GLI----
          hist(egea2_complet$VEMSsurCVF_p100GLI)
          epi.descriptives(egea2_complet$VEMSsurCVF_p100GLI)$a          

          #_DEM2575_p100GLI----
          hist(egea2_complet$DEM2575_p100GLI)
          epi.descriptives(egea2_complet$DEM2575_p100GLI)$a          
          
          par(mfrow=c(3,3))
          boxplot(egea2_complet$VEMS_preBD~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "VEMS_preBD par centre") 
          boxplot(egea2_complet$CVF_preBD~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "CVF_preBD par centre") 
          boxplot(egea2_complet$VEMSsurCVF_preBD~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "VEMSsurCVF_preBD par centre") 
          boxplot(egea2_complet$VEMS_p100GLI~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "VEMS_p100GLI par centre") 
          boxplot(egea2_complet$CVF_p100GLI~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "CVF_p100GLI par centre") 
          boxplot(egea2_complet$VEMSsurCVF_p100GLI~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "VEMSsurCVF_p100GLI par centre") 
          boxplot(egea2_complet$DEM2575_p100GLI~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "DEM2575_p100GLI par centre") 
          
          
#_____ DESCRIPTION AQLQ PAR CENTRE ----
          
      #__AQLQ TOTAL----
          
          #___Paris----
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==1 & egea2_complet$sexe==1)]) $a #1 :Paris homme
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==1 & egea2_complet$sexe==2)]) $a#1 :Paris femme
          
          #___Lyon----
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==2 & egea2_complet$sexe==1)])$a #2: Lyon homme
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==2 & egea2_complet$sexe==2)])$a #Lyon femme
          
          #___Marseille----
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==3 & egea2_complet$sexe==1)])$a #3: Marseille Homme
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==3 & egea2_complet$sexe==2)])$a # Marseille Femme
          
          #___Montpellier----
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==4 & egea2_complet$sexe==1)])$a #4: Montpellier Homme
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==4 & egea2_complet$sexe==2)])$a # Montpellier Femme
                 
          #___Grenoble----          
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==5 & egea2_complet$sexe==1)])$a # Grenoble Homme
          epi.descriptives(egea2_complet$AQLQ_total[which(egea2_complet$centre==5 & egea2_complet$sexe==2)])$a #Grenoble Femme

      #__ AQLQ LIMITATION ACTIVITE  ----
          
          #___Paris----
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==1 & egea2_complet$sexe==1)]) $a #1 :Paris homme
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==1 & egea2_complet$sexe==2)]) $a#1 :Paris femme
          
          #__Lyon----
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==2 & egea2_complet$sexe==1)])$a #2: Lyon homme
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==2 & egea2_complet$sexe==2)])$a #Lyon femme
          
          #__Marseille----
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==3 & egea2_complet$sexe==1)])$a #3: Marseille Homme
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==3 & egea2_complet$sexe==2)])$a # Marseille Femme
          
          #_AQLQ AL Montpellier----
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==4 & egea2_complet$sexe==1)])$a #4: Montpellier Homme
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==4 & egea2_complet$sexe==2)])$a # Montpellier Femme
          
          #_AQLQ AL Grenoble----          
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==5 & egea2_complet$sexe==1)])$a # Grenoble Homme
          epi.descriptives(egea2_complet$AQLQ_AL[which(egea2_complet$centre==5 & egea2_complet$sexe==2)])$a #Grenoble Femme
          
#_ Description AQLQ exposition environnementale  par centre----
          #_AQLQ EE Paris----
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==1 & egea2_complet$sexe==1)]) $a #1 :Paris homme
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==1 & egea2_complet$sexe==2)]) $a#1 :Paris femme
          
          #_AQLQ EE Lyon----
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==2 & egea2_complet$sexe==1)])$a #2: Lyon homme
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==2 & egea2_complet$sexe==2)])$a #Lyon femme
          
          #_AQLQ EE Marseille----
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==3 & egea2_complet$sexe==1)])$a #3: Marseille Homme
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==3 & egea2_complet$sexe==2)])$a # Marseille Femme
          
          #_AQLQ EE Montpellier----
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==4 & egea2_complet$sexe==1)])$a #4: Montpellier Homme
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==4 & egea2_complet$sexe==2)])$a # Montpellier Femme
          
          #_AQLQ EE Grenoble----          
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==5 & egea2_complet$sexe==1)])$a # Grenoble Homme
          epi.descriptives(egea2_complet$AQLQ_EE[which(egea2_complet$centre==5 & egea2_complet$sexe==2)])$a #Grenoble Femme                         

#_ Description AQLQ fonction emotionnelle  par centre----
          #_AQLQ EF Paris----
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==1 & egea2_complet$sexe==1)]) $a #1 :Paris homme
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==1 & egea2_complet$sexe==2)]) $a#1 :Paris femme
          
          #_AQLQ EF Lyon----
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==2 & egea2_complet$sexe==1)])$a #2: Lyon homme
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==2 & egea2_complet$sexe==2)])$a #Lyon femme
          
          #_AQLQ EF Marseille----
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==3 & egea2_complet$sexe==1)])$a #3: Marseille Homme
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==3 & egea2_complet$sexe==2)])$a # Marseille Femme
          
          #_AQLQ EF Montpellier----
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==4 & egea2_complet$sexe==1)])$a #4: Montpellier Homme
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==4 & egea2_complet$sexe==2)])$a # Montpellier Femme
          
          #_AQLQ EF Grenoble----          
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==5 & egea2_complet$sexe==1)])$a # Grenoble Homme
          epi.descriptives(egea2_complet$AQLQ_EF[which(egea2_complet$centre==5 & egea2_complet$sexe==2)])$a #Grenoble Femme       
          
#_ Description AQLQ symptome  par centre----
          #_AQLQ S Paris----
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==1 & egea2_complet$sexe==1)]) $a #1 :Paris homme
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==1 & egea2_complet$sexe==2)]) $a#1 :Paris femme
          
          #_AQLQ S Lyon----
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==2 & egea2_complet$sexe==1)])$a #2: Lyon homme
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==2 & egea2_complet$sexe==2)])$a #Lyon femme
          
          #_AQLQ S Marseille----
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==3 & egea2_complet$sexe==1)])$a #3: Marseille Homme
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==3 & egea2_complet$sexe==2)])$a # Marseille Femme
          
          #_AQLQ S Montpellier----
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==4 & egea2_complet$sexe==1)])$a #4: Montpellier Homme
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==4 & egea2_complet$sexe==2)])$a # Montpellier Femme
          
          #_AQLQ S Grenoble----          
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==5 & egea2_complet$sexe==1)])$a # Grenoble Homme
          epi.descriptives(egea2_complet$AQLQ_S[which(egea2_complet$centre==5 & egea2_complet$sexe==2)])$a #Grenoble Femme         
##############
          #ETUDE LA DISTRIBUTION DU SCORE AQLQ----
          par(mfrow=c(1,2))
          hist(egea2_complet$AQLQ_total,xlab='Score AQLQ total',ylab='Effectif',main= "Répartition du score AQLQ total")
          par(mfrow=c(2,2))
          hist(egea2_complet$AQLQ_AL,xlab="Score AQLQ Limitation d'activité",ylab='Effectif',main=NULL)
          hist(egea2_complet$AQLQ_S,xlab="Score AQLQ Symptome",ylab='Effectif',main=NULL)
          hist(egea2_complet$AQLQ_EF,xlab="Score AQLQ Fonction émotionnelle",ylab='Effectif',main=NULL)
          hist(egea2_complet$AQLQ_EE,xlab="Score AQLQ Exposition Environementale",ylab='Effectif',main=NULL)
          
          #REPARTITON SCORE AQLQ PAR CENTRE----
          par(mfrow=c(2,3))
          boxplot(egea2_complet$AQLQ_total~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition du score AQLQ total par centre")
          boxplot(egea2_complet$AQLQ_AL~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition du score AQLQ AL par centre")
          boxplot(egea2_complet$AQLQ_S~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition du score AQLQ S par centre")
          boxplot(egea2_complet$AQLQ_EF~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition du score AQLQ EF par centre")
          boxplot(egea2_complet$AQLQ_EE~egea2_complet$centre, names=c("Paris","Lyon","Marseille","Montpellier","Grenoble"), main= "Répartition du score AQLQ EE par centre")
          
          
          
          
          