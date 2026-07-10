library(tidyverse)
library(readxl)
library(readODS)
library(roll)

#FONCTIONS LIMINAIRES DE STANDARDISATION ORTHOGRAPHIQUE ET D'EPUREMENT

standardiserlibelles<-function(x,dontnombre){
  
  #dontnombre=0 permet de conserver les arrondissements de Paris, Lyon et Marseille
  if(dontnombre==1){x=str_remove_all(x,"[0-9]+")}
  
  x=x %>% 
    toupper() %>% 
    str_remove_all("\\*+") %>% 
    str_replace_all("Ê|É|È|Ë","E") %>% 
    str_replace_all("Â|À|Ä|Ã","A") %>% 
    str_replace_all("Û|Ù|Ü","U") %>% 
    str_replace_all("Ô|Ö","O") %>% 
    str_replace_all("Î|Ï","I") %>% 
    str_replace_all("Ÿ","Y") %>% 
    str_replace_all("Ç","C") %>% 
    str_replace_all("Œ","OE") %>% 
    str_replace_all("Ñ","N") %>% 
    str_replace_all("_*\\([a-zA-Z]?\\)_*|\\'+|\\’+|\\‘+|–+|-+|\\s+","_") %>% 
    #Attention, potentiellement, à cette ligne de code, si jamais une police 
    #pluricommunale existait avec une ville commençant par "MER". Tout format 
    #du type "XXXX/MERXXXX" sera converti. Ce faux positif est cependant simple
    #à détecter dans la phase de correction "manuelle", et n'intervient jamais
    #sur la période 2013-2024
    str_replace_all("(_S)?/MER","_SUR_MER_") %>% 
    str_replace_all("_S/","_SUR_") %>% 
    str_replace_all("_+","_") %>% 
    str_remove_all("_$|^_") %>% 
    str_replace_all("SAINT_","ST_") %>% 
    str_replace_all("SAINTE_","STE_") %>% 
    str_replace_all("SAINTES_","STES_") %>% 
    str_replace("^REUNION$","LA_REUNION") %>% 
    str_remove("^TOTAL$") %>% 
    na_if('')
  
  return(x)
}

standardisationcomplementaire=function(x){
  x=x %>% 
    str_remove("^MAIRIE_DE_|^COMMUNE_DE_|^VILLE_DE_") %>% 
    str_remove("\\(_*COMMUNE_(NOUVELLE|ASSOCIEE|DELEGUEE).*\\)") %>% 
    str_remove("\\(_*(NOUVELLES?|ANCIENNES?)_COMMUNES?_*\\)")
  
  x=case_when(
    str_detect(x,"_LA$|\\(_*LA_*\\)")~paste("LA_",x,sep=""),
    str_detect(x,"_LE$|\\(_*LE_*\\)")~paste("LE_",x,sep=""),
    str_detect(x,"_LES$|\\(_*LES_*\\)")~paste("LES_",x,sep=""),
    str_detect(x,"_L$|\\(_*L_*\\)")~paste("L_",x,sep=""),
    T~x) %>% 
    str_replace_all("_LA?E?S?$|\\(_*LA_*\\)|\\(_*LE_*\\)|\\(_*LES_*\\)|_\\(_*L_*\\)","_") %>% 
    str_replace_all("((^|_)SANT($|_))|((^|_)SAIN($|_))|SAIINT","_SAINT_") %>% 
    str_replace_all("_SAINT_","_ST_") %>% 
    str_replace_all("_+","_") %>% 
    str_remove_all("_$|^_") %>% 
    na_if('')
  
  return(x)
}

#Fonction changeant les valeurs absentes en zéros. Les fichiers originaux 
#contiennent fréquemment des vides qui sont clairement censés être interprétés 
#de la sorte. ça n'est donc pas une simplification abusive à mon sens.
transfona0<-function(x){
  case_when(is.na(x)|is.nan(x)~0,
            T~x)
}

#Fonction d'épuration de la liste départementale (notamment pour enlever le
#chef-lieu lorsqu'il a été inscrit dans la même colonne)
epurdep=function(x){
  case_when(x%in%departements$ldep~x,T~NA)
}

#Fonction de remplissage des NA par un département approprié
rolldep<-function(x,col){
  for(i in 2:nrow(x)){
    x[[i,col]]=case_when(
      is.na(x[[i,col]])~x[[i-1,col]],
      T~x[[i,col]]
    )
    
  }
  return(x)
}

#Fonction de traitement des fusions ou redéfinitions qui conduisent à avoir deux 
#points de données à ajouter pour une même année. Commune à toutes les régions.
#Cette fonction n'est pas mobilisée pour les données semi-brutes.
redefinitions2=function(x,fusions){
  
  x[which(x$ccom%in%fusions),]=x[which(x$ccom%in%fusions),] %>% 
    group_by(ccom,annee) %>% 
    #Le choix de prendre le minimum pour l'indicateur de mutualisation est tout
    #à fait contestable. C'est un conflit rare cependant. En fait, cette option
    #mute automatiquement en 0 les cas, comme à Lille, où plusieurs commissa-
    #riats indépendants existent (la commune associée de Lomme a conservé le
    #sien) et où un non-consensus des indicateurs au sein de la commune peut
    #trahir une simple indication de coopération au sein des frontières
    #communales, type de coopération qui ne m'intéresse pas. Par contre, on fait
    #passer à la trappe d'éventuels cas où une convention existait entre des
    #communes tierces et certaines seulement des communes ayant fusionné ensuite
    #en communes nouvelles : si l'on préfère éviter cette difficulté, on
    #privilégiera le max au min
    mutate(miseadispointerco=min(miseadispointerco,na.rm=F),
           across((which(names(x)%in%c(
             "polmun","asvp","maitrechien","chien","brigcanine","gardechamp"
           ))-2),sum)
    ) %>% 
    ungroup()
  
  return(x)
  
}

#IMPORTATION DU CODE OFFICIEL GEOGRAPHIQUE 2026
#TELECHARGEMENT : https://www.insee.fr/fr/information/8740222 ; fichiers csv :
#Communes, Départements, Evenements sur les communes, Collectivités et
#territoires d'Outre-Mer, Communes des collectivités et territoires d'Outre-Mer
#A stocker dans un sous-fichier "COG" inclus dans un fichier "Données"

#Départements et collectivités territoriales
departements<-read_csv("Données/COG/v_departement_2026.csv") %>% 
  select(5,1,2) %>% 
  set_names(c("ldep","dep","reg")) %>% 
  bind_rows(read_csv("Données/COG/v_comer_2026.csv") %>% 
              select(3,1) %>% 
              set_names(c("ldep","dep")) %>% 
              mutate(dep=as.character(dep),
                     reg=as.character(NA))) %>% 
  mutate(ldep=standardiserlibelles(ldep,1))

communes<-read_csv("Données/COG/v_commune_2026.csv") %>% 
  #ccom est le code commune, ccomsub est le code commune d' "échelon inférieur",
  #à savoir celui de la commune, de la commune déléguée/associée, de
  #l'arrondissement. Pas d'élimination des nombres (cas des arrondissements)
  mutate(COMPARENT=if_else(is.na(COMPARENT),COM,COMPARENT),
         LIBELLE=standardiserlibelles(LIBELLE,0),
         TYPECOM=if_else(TYPECOM=="COM",1,0)) %>% 
  select(LIBELLE,DEP,COM,COMPARENT,TYPECOM) %>% 
  set_names(c("lcom","dep","ccomsub","ccom","typique")) %>% 
  bind_rows(read_csv("Données/COG/v_commune_comer_2026.csv") %>% 
              filter(NATURE_ZONAGE=="COM") %>% 
              mutate(COMER=as.character(COMER),
                     COM_COMER=as.character(COM_COMER),
                     LIBELLE=standardiserlibelles(LIBELLE,0),
                     ccom=COM_COMER,
                     NATURE_ZONAGE=1) %>% 
              select(LIBELLE,COMER,COM_COMER,ccom,NATURE_ZONAGE) %>% 
              set_names(c("lcom","dep","ccomsub","ccom","typique"))) %>% 
  mutate(date=as.Date(paste("2026","-01-01",sep="")))

#Report manuel de la documentation de l'INSEE au propos du fichier des
#évolutions communales
clélectureMOD=data.frame(
  MOD=c(10,20:21,30:35,41,50,70:72),
  LIBMOD=c("Chgt de nom","Création","Rétablissement","Suppression","Fusion simple",
           "Création de com nvlle","Fusion-association","Fusion-asso -> Fusion simple",
           "Suppression de com délég","Chgt de code car chgt dep",
           "Chgt de code car déplacement chef-lieu","Com asso -> com délég",
           "Rétablissement de com délég","Création de com délég")
)

evcom<-read_csv("Données/COG/v_mvt_commune_2026.csv") %>% 
  mutate(DEP_AV=if_else(str_detect(COM_AV,"^9(7|8)"),
                        substr(COM_AV,1,3),
                        substr(COM_AV,1,2)),
         DEP_AP=if_else(str_detect(COM_AP,"^9(7|8)"),
                        substr(COM_AP,1,3),
                        substr(COM_AP,1,2))) %>% 
  #Elimination des changements de département pré-1970 et de ceux rapidement
  #renversés de Chateaufort et Toussus-le-Noble (Yvelines)
  filter(!(MOD==41&DATE_EFF<as.Date("1970-01-01")&(
    COM_AV%in%c("91143","91620")|COM_AP%in%c("91143","91620"))))

#Les fichiers d'effectifs n'indiquant que les libellés des communes, ce jeu de
#noms de communes est crucial comme table de passage entre les libellés et les
#codes. Inclure certains des événements communaux permet de ne pas avoir à
#traiter manuellement les changements de noms, et surtout les fusions : bref, il
#s'agit d'avoir dans la table de passage toutes les graphies périmées associées
#à d'autres communes ayant existé depuis 1945, et de les assigner à un code
#contemporain.
#La colonne "date" indique la dernière date à laquelle un libellé a existé dans 
#le Code Officiel Géographique (à département donné)
communes=bind_rows(
  communes,
  evcom %>% 
    filter(MOD%in%c(31:34,72)&DEP_AV==DEP_AP) %>% 
    mutate(LIBELLE_AV=standardiserlibelles(LIBELLE_AV,0),
           LIBELLE_AP=standardiserlibelles(LIBELLE_AP,0),
           LIBELLE=case_when(MOD%in%c(31:34)~LIBELLE_AV,
                             MOD==72~LIBELLE_AP),
           ccomsub=case_when(MOD%in%c(31:34)~COM_AV,
                             MOD==72~COM_AP),
           ccom=case_when(MOD%in%c(31:34)~COM_AP,
                          MOD==72~COM_AV),
           typecom=case_when(MOD%in%c(31:34)~TYPECOM_AP,
                             MOD==72~TYPECOM_AV),
           typique=as.numeric(NA)) %>% 
    filter(typecom=="COM") %>% 
    select(LIBELLE,DEP_AP,ccomsub,ccom,typique,DATE_EFF) %>% 
    set_names(c("lcom","dep","ccomsub","ccom","typique","date"))
) %>% 
  bind_rows(
    evcom %>% 
      mutate(LIBELLE_AV=standardiserlibelles(LIBELLE_AV,0),
             LIBELLE_AP=standardiserlibelles(LIBELLE_AP,0),
             typique=as.numeric(NA)) %>% 
      filter(MOD==10&LIBELLE_AP%in%communes$lcom) %>% 
      select(LIBELLE_AV,DEP_AP,COM_AV,COM_AP,typique,DATE_EFF) %>% 
      set_names(c("lcom","dep","ccomsub","ccom","typique","date"))
  ) %>% 
  arrange(desc(date))

n=0

#Outre la condition n<10 pour éviter une boucle infinie, les deux conditions de
#la boucle sont que chaque code, vieux ou actuel, ne peut être associé qu'à un
#unique code actuel (c'est le traitement du cas des changements de numéro ou des
#fusions annulées), et deuxièmement que tous les codes attribués soient bien des
#codes de commune de plein droit au COG 2026
while(
  n<10&(
    nrow(unique(communes[,c("ccom","ccomsub")])!=length(unique(communes$ccomsub))
    )&length(
      setdiff(
        communes$ccom,
        pull(communes[
          which(communes$date==as.Date(paste("2026","-01-01",sep=""))),
          "ccom"
        ])))>0
  )){
  
  n=n+1
  
  #Ces deux paragraphes actualisent les changements de code commune liés à une
  #modification de département
  communes[which(communes$ccom%in%filter(evcom,MOD==41)$COM_AV),
           "ccom"]=filter(evcom,MOD==41)[match(
             pull(communes[which(communes$ccom%in%filter(evcom,MOD==41)$COM_AV),"ccom"]),
             filter(evcom,MOD==41)$COM_AV),
             "COM_AP"]

  communes[which(communes$ccom%in%filter(evcom,MOD==41)$COM_AV),
           "ccomsub"]=filter(evcom,MOD==41)[match(
             pull(communes[which(communes$ccom%in%filter(evcom,MOD==41)$COM_AV),"ccomsub"]),
             filter(evcom,MOD==41)$COM_AV),
             "COM_AP"]
  
  #D'abord on réattribue un code "actuel" en vérifiant que ccomsub n'a pas été
  #associé à un autre code plus récemment (cas des changements de chef-lieu de
  #commune nouvelle comme à Orée d'Anjou, ou très rares de fusions-associations
  #ensuite annulées et associées à d'autres développements ultérieurs, notamment
  #Nonsard-Lamarche et Heudicourt). Ensuite on regarde s'il y a eu "empilement"
  #(fusions successives, de sorte que le ccom après une première fusion n'est
  #pas le code définitif mais l'intrant pour un autre changement de code)
  communes[,"ccom"]=communes[match(communes$ccomsub,communes$ccomsub),"ccom"]
  communes[,"ccom"]=communes[match(communes$ccom,communes$ccomsub),"ccom"]
  
}

print(n)

communes=communes %>% 
  mutate(dep=if_else(str_detect(ccom,"^9(7|8)"),
                        substr(ccom,1,3),
                        substr(ccom,1,2))) %>% 
  left_join(departements[,c("dep","reg")],by=join_by(dep)) %>% 
  select(lcom,reg,dep,ccomsub,ccom,typique,date) %>% 
  arrange(desc(date)) %>% 
  group_by(lcom,dep) %>% 
  filter(row_number()==1) %>% 
  ungroup()

#Population communale, avec reconstitution des communes à leurs frontières de 2026
#TELECHARGEMENT : https://www.insee.fr/fr/statistiques/3698339
popcom<-read_excel("Données/popcommunale.xlsx",
                   skip = 5) %>% 
  rename(ccomsub=CODGEO) %>% 
  left_join(unique(select(communes,-c("lcom","date","typique"))),
            by=join_by(ccomsub)) %>% 
  select(-c("LIBGEO","REG","DEP","ccomsub",
            names(.)[which(str_detect(names(.),"PTOT|PSDC"))])) %>% 
  left_join(communes[which(communes$typique==1),
                     c("ccom","lcom")],
            by=join_by(ccom)) %>% 
  select(lcom,reg,dep,ccom,everything()) %>% 
  set_names(tolower(str_remove_all(names(.),"PMUN"))) %>% 
  group_by(ccom) %>% 
  mutate(across(4:(ncol(.)-1),~sum(as.numeric(.)))
         #En réactivant cette ligne, on renseigne une population 2024 et une
         #population 2025 par redite de celle de 2023. Les populations de réfé- 
         #rence communales seront actualisées par l'INSEE en déc 2026.
         #,`2024`=`2023`,`2025`=`2023`
         ) %>% 
  filter(row_number()==1) %>% 
  ungroup()



#FICHIER DES INTERCOMMUNALITES
#TELECHARGEMENT : https://www.insee.fr/fr/information/2510634
epci<-read_excel("Données/COG/epci.xlsx",
                 sheet = "Composition_communale",
                 skip = 5) %>% 
  select(1:4) %>% 
  mutate(across(c(2,4),~standardiserlibelles(.,1)))

#IMPORTATION DES FICHIERS D'EFFECTIFS UN A UN
#TELECHARGEMENT : https://www.data.gouv.fr/datasets/police-municipale-effectifs-par-commune
#On les suppose ici stockées dans un sous-dossier "Effectifs de police municipale"
#et renommés selon le format "effectifspolicemunicipaleXXXX" où XXXX est l'année

#/!\ LES DONNEES, EN FAIT DE DECEMBRE, SONT ICI CONSIDEREES COMME AU PREMIER
#JANVIER, POUR CONCORDER AVEC LES DONNEES DEMOGRAPHIQUES. TOUTES LES DATES SONT
#DONC DECALEES : PAR EXEMPLE, LES DONNEES 2015 CORRESPONDENT AUX RESULTATS DE
#L'ENQUETE DE DECEMBRE 2014

#Les données manquent pour Paris en 2013
polmun2014<-read_excel(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2013.xlsx", 
  skip = 7) %>% 
  select(1:6) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","brigcanine") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:6,as.numeric),
         ldep=epurdep(ldep)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)|ldep=="CREUSE") %>% 
  mutate(annee=2014,
         ldep=case_when(row_number()%in%c((nrow(.)-3):nrow(.))~"VIENNE",
                        T~ldep),
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

#Les données pour Paris manquent en 2014
polmun2015<-read_excel(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2014.xlsx", 
  skip = 10) %>% 
  select(1:7) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=epurdep(ldep)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)|ldep=="CREUSE") %>% 
  mutate(annee=2015,
         ldep=case_when(row_number()%in%c((nrow(.)-3):nrow(.))~"VIENNE",
                        T~ldep),
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

#Les données des Vosges et de Paris manquent 2015
polmun2016<-read_excel(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2015.xls",
  skip = 16) %>% 
  select(1:7) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=case_when(ldep=="TARNE_ET_GARONNE"~"TARN_ET_GARONNE",
                        lcom=="CHATEAUROUX"~"INDRE",
                        T~ldep),
         ldep=epurdep(ldep)
  ) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)|ldep=="CREUSE") %>% 
  mutate(annee=2016,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

polmun2017<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2016.ods",
  skip=15) %>% 
  select(1:7) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=epurdep(ldep)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2017,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

#Les données des départements corses et de Paris manquent en 2017
polmun2018<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2017.ods",
  skip=8) %>% 
  select(c(1:2,4:8)) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=epurdep(ldep)
  ) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2018,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

#Les données de la Creuse manquent en 2018
polmun2019<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2018.ods",
  skip=6) %>% 
  select(c(2,4,6:10)) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=case_when(ldep=="COTE_D_ARMOR"~"COTES_D_ARMOR",
                        ldep=="PYRENEES_ATLANTIQUE"~"PYRENEES_ATLANTIQUES",
                        T~ldep),
         ldep=epurdep(ldep)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2019,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

polmun2020<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2019.ods",
  skip=7) %>% 
  select(c(2,4,6:10)) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=case_when(ldep=="COTE_D_ARMOR"~"COTES_D_ARMOR",
                        ldep=="SPM"~"ST_PIERRE_ET_MIQUELON",
                        T~ldep),
         ldep=epurdep(ldep)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2020,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

polmun2021<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2020.ods",
  skip=7) %>% 
  select(c(2,4,6:10)) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:7,as.numeric),
         ldep=case_when(ldep=="COTE_D_ARMOR"~"COTES_D_ARMOR",
                        T~ldep),
         ldep=epurdep(ldep)
  ) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2021,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

polmun2022<-read_excel(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2021.xlsx",
  skip=7) %>% 
  select(c(2,4,6:11)) %>% 
  set_names("ldep","lcom","miseadispointerco","polmun",
            "asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:8,as.numeric),
         ldep=case_when(ldep=="COTE_D_ARMOR"~"COTES_D_ARMOR",
                        T~ldep),
         ldep=epurdep(ldep)
  ) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2022,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

polmun2023<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2022.ods",
  skip=9) %>% 
  select(c(2,4,6:11)) %>% 
  set_names("ldep","lcom","miseadispointerco","polmun",
            "asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         across(3:8,as.numeric),
         ldep=case_when(ldep=="TERRITOIRE_DU_BELFORT"~"TERRITOIRE_DE_BELFORT",
                        T~ldep),
         ldep=epurdep(ldep)
  ) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)|ldep%in%c("ST_MARTIN","ST_BARTHELEMY")) %>% 
  mutate(annee=2023,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

polmun2024<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2023.ods",
  skip=9) %>% 
  select(c(1,4,6:11)) %>% 
  set_names("ldep","lcom","miseadispointerco","polmun",
            "asvp","gardechamp","maitrechien","chien") %>% 
  mutate(ldep=standardiserlibelles(ldep,1),
         lcom=standardiserlibelles(lcom,0),
         ldep=str_remove_all(ldep,"COLLECTIVITE_DE_"),
         ldep=standardiserlibelles(ldep,1),
         across(3:8,as.numeric),
         ldep=epurdep(ldep)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2024,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()

#A noter qu'il semble y avoir 8 instances de NA réels dans ce jeu ; pour 
#c("CAUNES_MINERVOIS","STE_MENEHOULD","MARCK","GELOS","AIME_LA_PLAGNE",
#"MALAUNAY","BRETIGNY_SUR_ORGE","BRIIS_SOUS_FORGES")
#Je rappelle ne jamais faire la distinction pour ce code, et ne pas avoir 
#tenté d'en diagnostiquer la présence pour un autre millésime
polmun2025<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2024.ods",
  skip=9) %>% 
  select(c(1,4,6:11)) %>% 
  set_names("dep","lcom","miseadispointerco","polmun",
            "asvp","gardechamp","maitrechien","chien") %>% 
  mutate(lcom=standardiserlibelles(lcom,0),
         dep=str_pad(dep,width=2,pad="0"),
         across(3:8,as.numeric)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)&!is.na(lcom)) %>% 
  mutate(annee=2025,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()




#UNIFICATION DES FICHIERS
polmun=list()

for(i in 2014:2025){
  polmun[[as.character(i)]]=get(paste("polmun",i,sep=""))
}

polmun=lapply(polmun,function(x){
  
  if("ldep"%in%names(x)){
    x=x %>% left_join(departements,by=join_by(ldep)) %>% 
      mutate(across(which(names(.)%in%c("polmun","asvp","gardechamp","miseadispointerco",
                                        "brigcanine","maitrechien","chien")),
                    transfona0))
  }
  
  else{
    x=x %>% left_join(departements,by=join_by(dep)) %>% 
      mutate(across(which(names(.)%in%c("polmun","asvp","gardechamp","miseadispointerco",
                                        "brigcanine","maitrechien","chien")),
                    transfona0))
  }
  
  for(i in 1:4){
    if(!c("miseadispointerco","brigcanine","maitrechien",
          "chien")[i]%in%names(x)){
      x=mutate(x,nvllecol=NA) %>% 
        set_names(c(names(x),
                    c("miseadispointerco","brigcanine","maitrechien","chien")[i]
        ))
    }
  }
  
  return(x %>% select(reg,dep,ldep,annee,lcom,
                      miseadispointerco,
                      polmun,asvp,gardechamp,
                      brigcanine,maitrechien,chien))
}) %>% bind_rows() %>% 
  #Afin de rendre la présentation sur Excel plus jolie, je présume, le
  #département a été indiqué une case en dessous de là où il devrait être (selon
  #l'hypothèse qu'un libellé départemental s'applique depuis la case de sa
  #première mention jusqu'au moment où lui succède une autre mention,
  #l'hypothèse de ma fonction rolldep) spécifiquement sur les premiers
  #départements et en 2015 (2016 dans ma datation). Cela place quelques villes
  #qui sont premières de leur département alphabétiquement dans le département
  #suivant, et il était plus commode de résoudre cela avec les numéros
  #départementaux que directement dans polmun2016 sur les libellés.
  mutate(dep=case_when(annee==2016&lcom%in%c(
    "CHARLEVILLE_MEZIERES","FOIX","ARCIS_SUR_AUBE",
    "ALZONNE","BELMONT_SUR_RANCE","AIX_EN_PROVENCE","ARGENCES",
    "AURILLAC","ANGOULEME","ANTONY","AIGREFEUILLE_D_AUNIS",
    "AUBIGNY_SUR_NERE")~str_pad(as.numeric(dep)+1,2,pad="0"),
    T~dep)) %>% 
  select(-ldep) %>% 
  left_join(departements[,c("dep","ldep")],by=join_by(dep)) %>% 
  mutate(
    dep=factor(dep,levels=c(str_pad(as.character(1:19),2,pad="0"),
                            "2A","2B",as.character(21:96),
                            as.character(971:989))),
    #On ôte des libellés les renvois à notes de type "_(2)" et les codes postaux
    lcom=str_remove(lcom,"_\\(_?[0-9a-zA-Z]_?\\)$|_\\(?_?[0-9]{5}_?\\)?$")
    ) %>% 
  group_by(annee,dep) %>% 
  mutate(oc=max(row_number())) %>% 
  ungroup() %>% 
  #On élimine les libellés communaux égaux à des nombres ou vierges, car cela 
  #indique presque systématiquement un total de nombre de services départemen-
  #taux, avec pour données des autres colonnes des totaux d'agents. Une excep-
  #tion est introduite dans le cas où il s'agit de la seule ligne consacrée à 
  #un département, ce qui permet de recouvrer 6 points de données qui sont en-
  #suite traités manuellement. La seule exception est la valeur 2018 pour la 
  #Creuse, où les totaux départementaux à 0 semblent remplis automatiquement 
  #et la ligne de données vierge au-dessus semble devoir être lue comme une 
  #absence de données plutôt que comme des valeurs zéros. D'autant qu'une 
  #valeur zéro pour les ASVP serait unique dans les remontées aubussonnaises
  filter(!((str_detect(lcom,"^[0-9]+$")|is.na(lcom))&oc>1)) %>% 
  select(-oc) %>% 
  select(reg,dep,ldep,everything()) %>% 
  mutate(lcom=case_when(
    dep=="23"&annee<2026&is.na(lcom)~"AUBUSSON",
    dep=="977"&annee<2026~"ST_BARTHELEMY",
    dep=="978"&annee<2026~"ST_MARTIN",
    dep=="975"&annee<2026&lcom=="0"~"ST_PIERRE",
    T~lcom
  )) %>% 
  bind_rows(.,
            filter(.,dep=="975"&annee==2023) %>% 
              mutate(lcom="MIQUELON_LANGLADE")
            ) %>% 
  filter(!(lcom=="0"&dep=="23"&annee=="2019")) %>% 
  arrange(annee) %>% 
  arrange(lcom) %>% 
  arrange(dep)
