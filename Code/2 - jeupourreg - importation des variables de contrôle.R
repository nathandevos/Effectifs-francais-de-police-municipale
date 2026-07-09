library(tidyverse)
library(readxl)
library(readODS)
library(estimatr)

#IMPORTATION DES EFFECTIFS DE POLICIERS MUNICIPAUX

#Ajout des valeurs nulles de police municipale pour les villes non dotées, à
#savoir : c("OREE_D_ANJOU", "MAUGES_SUR_LOIRE", "SEVREMOINE", "TRELAZE",
#"AVION", "WITTENHEIM", "GARCHES", "ISSY_LES_MOULINEAUX")

#En téléchargeant directement le jeu des communes de plus de 15 000 habitants
#depuis datagouv
polmunpourreg=read_csv(
  "Données/policesmunicipalesplusde15000.csv"
  ) %>% select(c(4,6,8,9,12,21)) %>% 
  set_names(
    c("ccom","annee","polmun","asvp","maitrechien","popcouvnplusun")
    ) %>% 
  mutate(annee=annee+1) %>% 
    filter(annee%in%2016:2022) %>% 
  #Ajout des valeurs nulles des communes n'ayant jamais répondu
  bind_rows(
    data.frame(
      ccom=rep(
        c("49126","49244","49301","49353","62065","68376","92033","92040"),
        each=7),
      annee=rep(2016:2022,8),
      polmun=0,
      asvp=0,
      maitrechien=0,
      popcouvnplusun=popcom %>% 
        filter(ccom%in%c(
          "49126","49244","49301","49353","62065","68376","92033","92040"
        )) %>% 
        select(c("ccom",as.character(2016:2022))) %>% 
        pivot_longer(2:8,names_to="annee",values_to="popcouvnplusun") %>% 
        mutate(annee=as.numeric(annee)) %>% 
        arrange(annee) %>% 
        arrange(ccom) %>% 
        pull()
    )
  ) %>% 
    #dac : dont agents cynophiles
    mutate(popcouvnplusun=popcouvnplusun/1000,
           polmundac=(polmun+maitrechien)/popcouvnplusun,
           fdomun=(polmun+asvp)/popcouvnplusun,
           fdomundac=(polmun+asvp+maitrechien)/popcouvnplusun,
           polmun=polmun/popcouvnplusun) %>% 
    select(ccom,annee,polmun,polmundac,fdomun,fdomundac) %>% 
    mutate(across(3:6,logna,.names="log{col}"),
           across(3:6,~.^2,.names="lincar{col}"),
           across(3:6,~if_else(.==0,NA,.*log(.)-.),.names="lin2{col}"),
           across(3:6,~if_else(.==0,NA,log(.)^2),.names="log2{col}")) %>% 
  arrange(annee) %>% 
  arrange(ccom)

#S'il a été recréé en utilisant les autres scripts. Evidemment, on a pas besoin
#de décaler l'année, puisque dans ce cas-là c'est déjà fait
polmunpourreg=polmun15000 %>% 
  select(ccom,annee,polmun,asvp,maitrechien,popcouvnplusun) %>% 
  filter(annee%in%2016:2022) %>% 
  #Ajout des valeurs nulles des communes n'ayant jamais répondu
  bind_rows(
    data.frame(
      ccom=rep(
        c("49126","49244","49301","49353","62065","68376","92033","92040"),
        each=7),
      annee=rep(2016:2022,8),
      polmun=0,
      asvp=0,
      maitrechien=0,
      popcouvnplusun=popcom %>% 
        filter(ccom%in%c(
          "49126","49244","49301","49353","62065","68376","92033","92040"
          )) %>% 
        select(c("ccom",as.character(2016:2022))) %>% 
        pivot_longer(2:8,names_to="annee",values_to="popcouvnplusun") %>% 
        mutate(annee=as.numeric(annee)) %>% 
        arrange(annee) %>% 
        arrange(ccom) %>% 
        pull()
    )
  ) %>% 
  #dac : dont agents cynophiles
  mutate(popcouvnplusun=popcouvnplusun/1000,
         polmundac=(polmun+maitrechien)/popcouvnplusun,
         fdomun=(polmun+asvp)/popcouvnplusun,
         fdomundac=(polmun+asvp+maitrechien)/popcouvnplusun,
         polmun=polmun/popcouvnplusun) %>% 
  select(ccom,annee,polmun,polmundac,fdomun,fdomundac) %>% 
  mutate(across(3:6,logna,.names="log{col}"),
         across(3:6,~.^2,.names="lincar{col}"),
         across(3:6,~if_else(.==0,NA,.*log(.)-.),.names="lin2{col}"),
         across(3:6,~if_else(.==0,NA,log(.)^2),.names="log2{col}"))

#IMPORTATION DES DONNEES DE DELINQUANCE/CRIMINALITE
#TELECHARGEMENT : https://www.data.gouv.fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales

#Cette opération devrait prendre quelques minutes
#ATTENTION. DANS CE JEU NATIONAL, LES POINTS DE DONNEES RELATIFS A PARIS,
#MARSEILLE, ET LYON, CONCERNANT LES INDICATEURS "TRAFIC DE STUP" ET "VOLS AVEC
#ARMES", NE SONT PAS UNIQUEMENT IDENTIFIES, DU FAIT QUE COHABITAIENT DES DONNEES
#CENSUREES ET DIFFUSEES AU NIVEAU DES ARRONDISSEMENTS. CELA IMPLIQUE QUE LES
#MESURES NON VIERGES NE VALENT QUE POUR UN SOUS-ENSEMBLE D'ARRONDISSEMENTS, PAS
#LA TOTALITE. Comme ce sont deux indicateurs écartés car trop censurés, ça n'a 
#pas d'importance pour la suite
donneescrimcom<-read_csv2("Données/donneescrimcom.csv",
                          trim_ws = TRUE) %>% 
  rename(ccomsub=CODGEO_2025) %>% 
  #Toute la suite sert à redéfinir aux frontières communales du COG2026
  left_join(unique(communes[,c("ccomsub","ccom")]),
            by=join_by(ccomsub)) %>% 
  select(annee,ccom,indicateur,est_diffuse,everything()) %>% 
  select(-ccomsub) %>% 
  group_by(annee,ccom,indicateur,est_diffuse) %>% 
  mutate(taux_pour_mille=sum(taux_pour_mille*insee_pop)/sum(insee_pop),
         across(which(names(.)%in%c("nombre","insee_pop","insee_log"))-4,sum)) %>% 
  filter(row_number()==1) %>% 
  ungroup()

donneescrimcom15000=donneescrimcom %>% 
  filter(ccom%in%communesplus15000$ccom) %>% 
  group_by(indicateur) %>% 
  mutate(censure=mean(if_else(est_diffuse=="ndiff",1,0))) %>% 
  ungroup() %>% 
  filter(censure<=0.01) %>% 
  select(ccom,annee,indicateur,taux_pour_mille) %>% 
  mutate(indicateur=case_when(
    indicateur=="Violences physiques intrafamiliales"~"violencesintrafam",
    indicateur=="Violences physiques hors cadre familial"~"violencesextrafam",
    indicateur=="Vols sans violence contre des personnes"~"volscontrepers",
    indicateur=="Cambriolages de logement"~"cambriolages",
    indicateur=="Vols de véhicule"~"volsdevehicule",
    indicateur=="Vols dans les véhicules"~"volsdansvehicule",
    indicateur=="Vols d'accessoires sur véhicules"~"volssurvehicule",
    indicateur=="Destructions et dégradations volontaires"~"degradations",
    indicateur=="Usage de stupéfiants"~"usagestup",
    indicateur=="Escroqueries et fraudes aux moyens de paiement"~"escroqueries"
  )) %>% 
  pivot_wider(values_from="taux_pour_mille",names_from="indicateur") %>% 
  mutate(across(3:ncol(.),logna,.names="log{col}")) %>% 
  arrange(annee) %>% 
  arrange(ccom)

#IMPORTATION DES ESTIMATIONS DE PRESENCE DES FORCES DE L'ORDRE DE L'ETAT
#TELECHARGEMENT : https://www.insee.fr/fr/statistiques?debut=0&theme=24&categorie=1&collection=4

#Parmi les commissariats, seuls ceux d'Avignon, Tarascon et Montbéliard 
#semblent traverser une frontière départementale
zonescompetence<-read_delim("Données/competence-territoriale-pn-gn.csv", 
                            delim = ";",
                            escape_double = FALSE,
                            trim_ws = TRUE) %>% 
  set_names(c("ccom","lcom",names(.)[3:7])) %>% 
  filter(!str_detect(lcom,"arrondissement")) %>% 
  rbind(data.frame(
    ccom=c("13055","69123","75056"),
    lcom=c("Marseille","Lyon","Paris"),
    institution="PN",
    id_service=NA,
    service=NA,
    code_postal=NA,
    codes_postaux=NA
  )) %>% 
  mutate(dep=if_else(str_detect(ccom,"^9[7-8]"),
                     substr(ccom,1,3),
                     substr(ccom,1,2))) %>% 
  left_join(popcom[,c("ccom",2016:2022)],by=join_by(ccom))

#Liste des communes couvertes par la PN et la GN à la fois
doublesprerogatives=zonescompetence %>% 
  group_by(ccom) %>% 
  mutate(oc=1,oc=sum(oc)) %>% 
  ungroup() %>% 
  filter(oc>1) %>% select(ccom) %>% unique() %>% pull()

poparetrancher=popcom %>% 
  filter(ccom%in%doublesprerogatives) %>% 
  select(ccom,as.character(2016:2022)) %>% 
  pivot_longer(2:ncol(.),values_to = "retraitpop",names_to = "annee") %>% 
  mutate(annee=as.numeric(annee),
         ccom=substr(ccom,1,2),
         retraitpop=retraitpop/2) %>% 
  rename(dep=ccom)

BTS=list()

for(i in 2016:2022){
  BTS[[paste("BTS",i,sep="")]]=read_delim(
    paste("Données/Base Tous Salariés/FD_SALAAN_",i,".csv",sep=""),
    escape_double = FALSE,
    trim_ws = TRUE) %>% 
    filter(PCS%in%c("452B","531A","531B","532A","533A")) %>% 
    select(DEPR,DEPT,PCS) %>% 
    group_by(DEPR,DEPT,PCS) %>% 
    #Comme on a une ligne par salarié suivi, on compte juste le nombre de lignes
    #par catégorie pour avoir les effectifs départementaux
    mutate(valeur=max(row_number())) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    arrange(PCS) %>% 
    pivot_wider(values_from = "valeur",names_from = "PCS")
  
  for(k in 1:5){
    if(!c("452B","531A","531B","532A","533A")[k]%in%names(BTS[[paste("BTS",i,sep="")]])){
      BTS[[paste("BTS",i,sep="")]]=BTS[[paste("BTS",i,sep="")]] %>% 
        mutate(!!c("452B","531A","531B","532A","533A")[k]:=0)
    }
  }
  
  #Il y a clairement une erreur de poids pour les gendarmes en 2016-17
  if(i%in%2016:2017){
    BTS[[paste("BTS",i,sep="")]]=BTS[[paste("BTS",i,sep="")]] %>% 
      mutate(`452B`=`452B`/4,
             `532A`=`532A`/4)
  }
  
  BTS[[paste("BTS",i,sep="")]]=BTS[[paste("BTS",i,sep="")]] %>% 
    select(c("DEPR","DEPT","452B","531A","531B","532A","533A")) %>% 
    mutate(annee=i,
           across(3:7,~transfona0(.)*12))
}

BTS=BTS %>% lapply(function(x){
  rename(x,
         adjudants="452B",
         polnatbts="531A",
         polmunbts="531B",
         gendbts="532A",
         pompiersbts="533A") %>% 
    mutate(across(1:2,~case_when(.%in%c("Non renseigné","99")~NA,
                                 T~.)))
}) %>% 
  bind_rows()

#Calcul des totaux de population selon le département de travail
BTS=zonescompetence %>% 
  select("dep","institution",as.character(2016:2022)) %>% 
  pivot_longer(3:ncol(.),values_to = "popt",names_to = "annee") %>% 
  group_by(dep,institution,annee) %>% 
  mutate(annee=as.numeric(annee),
         popt=sum(popt),
         institution=paste("popt",tolower(institution),sep="")) %>% 
  ungroup() %>% 
  unique() %>% 
  left_join(poparetrancher,by=join_by(annee,dep)) %>% 
  mutate(popt=popt-transfona0(retraitpop)) %>% 
  select(-retraitpop) %>% 
  pivot_wider(values_from=popt,
              names_from=institution) %>% 
  rename("DEPT"="dep") %>% 
  left_join(BTS,.,by=join_by(DEPT,annee))

#Calcul des totaux de population selon le département de résidence
BTS=zonescompetence %>% 
  select("dep","institution",as.character(2016:2022)) %>% 
  pivot_longer(3:ncol(.),values_to = "popr",names_to = "annee") %>% 
  group_by(dep,institution,annee) %>% 
  mutate(annee=as.numeric(annee),
         popr=sum(popr),
         institution=paste("popr",tolower(institution),sep="")) %>% 
  ungroup() %>% 
  unique() %>% 
  left_join(poparetrancher,by=join_by(annee,dep)) %>% 
  mutate(popr=popr-transfona0(retraitpop)) %>% 
  select(-retraitpop) %>% 
  pivot_wider(values_from=popr,
              names_from=institution) %>% 
  rename("DEPR"="dep") %>% 
  left_join(BTS,.,by=join_by(DEPR,annee))

BTS=BTS %>% 
  mutate(across(which(str_detect(names(.),"pop")),~transfona0(.)/1000),
         popr=poprgn+poprpn,
         popt=poptgn+poptpn)

BTSR=BTS %>% select(annee,DEPR,everything()) %>% 
  filter(DEPR%in%departements$dep&nchar(DEPR)==2) %>% 
  select(-c("DEPT","adjudants")) %>% 
  group_by(annee) %>% 
  mutate(across(2:5,~sum(.,na.rm=T),.names="total{col}")) %>% 
  ungroup() %>% 
  group_by(annee,DEPR) %>% 
  mutate(polnatbts=sum(polnatbts,na.rm=T)/poprpn,
         gendbts=sum(gendbts,na.rm=T)/poprgn,
         polmunbts=sum(polmunbts,na.rm=T)/popr,
         pompiersbts=sum(pompiersbts,na.rm=T)/popr) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(DEPR) %>% 
  mutate(gendbts=case_when(
    annee==2018~mean(gendbts[which(annee%in%c(2017,2019))]),
    annee==2022~gendbts[which(annee==2021)],
    T~gendbts),
    polnatbts=case_when(
      annee==2022~polnatbts[which(annee==2021)],
      T~polnatbts)) %>% 
  ungroup() %>% 
  select(c("annee","DEPR","polnatbts","gendbts","polmunbts","pompiersbts",
           paste("total",
                 c("polnatbts","gendbts","polmunbts","pompiersbts"),
                 sep="")))

BTST=BTS %>% select(annee,DEPT,everything()) %>% 
  filter(DEPT%in%departements$dep&nchar(DEPT)==2) %>% 
  select(-c("DEPR","adjudants")) %>% 
  group_by(annee) %>% 
  mutate(across(2:5,~sum(.,na.rm=T),.names="total{col}")) %>% 
  ungroup() %>% 
  group_by(annee,DEPT) %>% 
  mutate(polnatbts=sum(polnatbts,na.rm=T)/poptpn,
         gendbts=sum(gendbts,na.rm=T)/poptgn,
         polmunbts=sum(polmunbts,na.rm=T)/popt,
         pompiersbts=sum(pompiersbts,na.rm=T)/popt) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(DEPT) %>% 
  mutate(gendbts=case_when(
    annee==2018~mean(gendbts[which(annee%in%c(2017,2019))]),
    annee==2022~gendbts[which(annee==2021)],
    T~gendbts),
    polnatbts=case_when(
      annee==2022~polnatbts[which(annee==2021)],
      T~polnatbts)) %>% 
  ungroup() %>% 
  select(c("annee","DEPT","polnatbts","gendbts","polmunbts","pompiersbts",
           paste("total",
                 c("polnatbts","gendbts","polmunbts","pompiersbts"),
                 sep=""))) %>% 
  arrange(DEPT) %>% 
  arrange(annee)

#IMPORTATION ZONAGES ET TAUX DE CHOMAGE
#TELECHARGEMENT : https://www.insee.fr/fr/statistiques/1893230

zonages1=read_excel("Données/Zonages/unitesurbaines.xlsx", 
                    sheet = "Composition_communale", skip = 5) %>% 
  select(1,3) %>% 
  set_names(c("ccom","aireurbaine")) %>% 
  filter(ccom%in%communesplus15000$ccom) %>% 
  mutate(aireurbaine=as.character(aireurbaine))

zonages2=read_excel("Données/Zonages/airesattraction.xlsx", 
                    sheet = "Composition_communale", skip = 5) %>% 
  select(1,3) %>% 
  set_names(c("ccom","aireattraction")) %>% 
  filter(ccom%in%communesplus15000$ccom) %>% 
  mutate(aireattraction=as.character(aireattraction))

zonages3=read_excel("Données/Zonages/zonesdemploi.xlsx", 
                    sheet = "Composition_communale", skip = 5) %>% 
  select(1,3,5) %>% 
  set_names(c("ccom","zoneemploi","zereg")) %>% 
  filter(ccom%in%communesplus15000$ccom) %>% 
  mutate(
    zoneemploi=if_else(is.na(zereg),
                       zoneemploi,
                       paste(substr(zereg,6,7),
                             substr(zereg,3,4),
                             sep=""))
  ) %>% 
  select(-zereg)

txchom<-read_excel("Données/chomage-zone-2003-2025.xlsx",
                   sheet = "txcho_ze",
                   skip = 4) %>% 
  select(c(1,5:ncol(.))) %>% 
  set_names(c("zoneemploi",names(.)[2:ncol(.)])) %>% 
  bind_rows(
    read_excel("Données/chomage-zone-2003-2025.xlsx",
               sheet = "txcho_parties reg",
               skip = 6) %>% 
      select(c(1,3:ncol(.))) %>% 
      set_names(c("zoneemploi",names(.)[2:ncol(.)]))
  ) %>% 
  pivot_longer(2:ncol(.),values_to = "txchom",names_to = "annee") %>% 
  mutate(annee=as.numeric(annee))

#IMPORTATION DONNEES FISCALES
#TELECHARGEMENT : https://www.data.gouv.fr/datasets/limpot-sur-le-revenu-par-collectivite-territoriale-ircom

#Au niveau du COG, il faut procéder à la mise à jour, et unifier les données des
#3 grandes communes en additionnant les arrondissements. Il n'y a pas de
#remontée propre à Paris, Marseille ou Lyon qui soit un doublon des données
#d'arrondissement et fausse l'addition

revenusfisc=list()

for(i in 2016:2022){
  revenusfisc[[paste("fisc",i,sep="")]]=read_excel(
    paste("Données/IRCOM/ircom_communes_complet_revenus_",i,".xlsx",sep=""),
    na = c("n.c.","."),
    skip = case_when(i<2020~3,
                     i>2021~5,
                     T~6),
    trim_ws=T) %>% 
    filter(row_number()!=1) %>% 
    set_names(c("dep","ccomsub","exlcom",names(.)[4:length(names(.))])) %>% 
    mutate(dep=if_else(!str_detect(dep,"^9(7|8)"),substr(dep,1,2),dep),
           ccomsub=paste(dep,ccomsub,sep="")) %>% 
    filter(ccomsub%in%(filter(communes,ccom%in%communesplus15000$ccom)$ccomsub)) %>% 
    left_join(
      unique(communes[,c("ccomsub","ccom")]),
      by=join_by(ccomsub)) %>% 
    left_join(
      filter(communes,typique==1)[,c("ccom","lcom")],
      by=join_by(ccom)) %>% 
    mutate(annee=as.character(i))
}

revenusfisc=bind_rows(revenusfisc) %>% 
  set_names(c("dep","ccomsub","exlcom","trancherev","nbrefoyers","revtot",
              "impotnettot","nbrefoyersimposes","revtotfoyersimposes",
              "nbrefoyerssal","revsaltot","nbrefoyersretraites",
              "retraitestot","ccom","lcom","annee")) %>% 
  select(-c("ccomsub","exlcom")) %>% 
  select(dep,ccom,lcom,annee,everything()) %>% 
  mutate(annee=as.numeric(annee),
         across(6:14,as.numeric),
         #La multiplication par 1000 permet de passer en euros
         across(c(7,8,10,12,14),~round(.*1000)),
         trancherev=case_when(
           str_detect(trancherev,"Total|TOTAL")~"total",
           str_detect(trancherev,"de 100 000")~"plus de 100 000",
           T~trancherev)) %>% 
  group_by(dep,ccom,lcom,annee,trancherev) %>% 
  mutate(across(1:9,sum)) %>% 
  filter(row_number()==1) %>% 
  ungroup()


#IMPORTATION DONNEES VALEURS FONCIERES PAR MERICSKAY
#TELECHARGEMENT : https://www.data.gouv.fr/datasets/indicateurs-immobiliers-par-commune-et-par-annee-prix-et-volumes-sur-la-periode-2014-2024

#Niveau COG, il faut bien sûr remettre beaucoup de choses à jour, mais on
#constate que les données sont déjà au niveau communal plutôt que
#d'arrondissement dans les 3 grandes communes

#Il manque l'Alsace-Moselle dans ce jeu, comme on sait

DVF=list()

for(i in 2016:2022){
  DVF[[paste("dvf",i,sep="")]]=read_csv(
    paste("Données/DVF Mericskay/dvf",i,".csv",sep="")
  ) %>% 
    select(-1) %>% 
    set_names(c("ccomsub","annee","nbremutations","nbremaisons","nbreapparts",
                "partmaisons","partapparts","prixmoy","prixmoym2","surfacemoy")) %>% 
    filter(ccomsub%in%(filter(communes,ccom%in%communesplus15000$ccom)$ccomsub))
}

DVF=bind_rows(DVF) %>% 
  left_join(
    filter(communes,ccom%in%communesplus15000$ccom)[,c("ccomsub","ccom")] %>% 
      group_by(ccomsub) %>% 
      filter(row_number()==1) %>% 
      ungroup(),
    by=join_by(ccomsub)) %>% 
  left_join(
    filter(communes,typique==1)[,c("ccom","lcom")],
    by=join_by(ccom)) %>% 
  select(-ccomsub) %>% 
  select(ccom,lcom,annee,everything()) %>% 
  group_by(ccom,lcom,annee) %>% 
  mutate(
    across(
      which(names(.)%in%c("prixmoy","prixmoym2","surfacemoy"))-3
      ,~sum(.*nbremutations)/sum(nbremutations)),
    across(
      which(names(.)%in%c("nbremutations","nbremaisons","nbreapparts"))-3
      ,sum),
    partmaisons=nbremaisons/nbremutations,
    partapparts=nbreapparts/nbremutations,
    ) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
  arrange(annee) %>% 
  arrange(ccom)


#IMPORTATION DOSSIER COMPLET COMMUNAL DE L'INSEE
#TELECHARGEMENT : https://www.insee.fr/fr/statistiques/5359146

    #Notes de brouillon préservées :

#1° 77 % des entrées débutent par la structure "[A-Z][0-9]{2}", où les deux chiffres 
#indiquent l'année.
#2° Toutes les données intéressantes sont de cette forme pour :
#source%in%c("Insee, RP2011, RP2016 et RP2022, géographie au 01/01/2025", 
#            "Insee, RP1967 à 1999 dénombrements, RP2011 au RP2022, géographie au 01/01/2025")
#3° Sauf éventuellement les naissances ou décès totaux par période, de forme :
#"[A-Z]{4}[0-9]{4}", avec les deux premiers chiffres pour l'année de début, 
#et les deux derniers pour celle de fin. Si >67, 19xx, sinon, 20xx.
#4° Ainsi que SUPERF.

#Pour les sources :
#source %in% c("Insee, Système d'information sur la démographie d'entreprises (SIDE) en géographie au 01/01/2025"),
#conserver ENCTOTxx,ENCITOTxx, et ^ETC.
#Format : deux derniers chiffres = année, 20xx

#Tout garder de : "Insee, statistiques de l'état civil en géographie au 01/01/2025"
#Format : deux derniers chiffres = année, 20xx

#Négliger pour manque d'amplitude temporelle :
#source %in% c("Insee, Répertoire électoral unique en géographie au 01/01/2024", 
#"Insee, base permanente des équipements au 01/01/2024",
#"Insee, Base Tous salariés, fichier Postes au lieu de travail en géographie au 01/01/2025",
#"Insee, partenaires territoriaux en géographie au 01/01/2025", 
#"Insee-DGFiP-Cnaf-Cnav-Ccmsa, Fichier localisé social et fiscal (FiLoSoFi) en géographie au 01/01/2024",
#"Insee, Flores (Fichier localisé des rémunérations et de l’emploi salarié) en géographie au 01/01/2025", 
#"Insee, Système d'information sur la démographie d'entreprises (SIDE) en géographie au 01/01/2024",
#"Insee, RP1967 à 1999 dénombrements, RP2011 au RP2022, géographie au 01/01/2025")

#Dans le dernier cas, amplitude temporelle inappropriée sauf pour var de naissances
#et de décès redondante

    #Fin des notes préservées

#En juin 2026, le dossier complet est partiellement au COG2025. En ce qui nous
#concerne, cela pose un problème pour Orée d'Anjou et pour Saint-Denis, avec
#deux points de données excédentaires (Champtoceaux et Pierrefitte-sur-Seine),
#mais on découvre après plus amples inspections que les valeurs desdites communes
#ont toutes été mutées en NA et que la fusion avec les valeurs pour la commune
#nouvelle sont en ordre (au moins pour la population, je présume aussi pour le
#reste)
dossier_complet <- read_delim_chunked(
  "Données/dossier_complet.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  chunk_size=4000,
  callback=DataFrameCallback$new(
    function(x,pos){
      subset(x,
             CODGEO%in%filter(
               communes,
               ccom%in%communesplus15000$ccom&!str_detect(lcom,"ARRONDISSEMENT")
               )$ccomsub
      )
    }
  )) %>% 
  filter(!CODGEO%in%c("49069","93059"))

metadossier<-read_delim(
  "Données/meta_dossier_complet.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(c(names(.)[1:3],"TYPE_VAR","SOURCE")) %>% 
  set_names(c("codvar","libvar1","libvar2","classevar","source")) %>% 
  filter(codvar!="CODGEO")

dossierpourreg=dossier_complet %>% 
  select(
    which(names(.)%in%c("CODGEO","SUPERF")|(
      names(.)%in%pull(metadossier[which(
        metadossier$source%in%c("Insee, RP2011, RP2016 et RP2022, géographie au 01/01/2025",
                                "Insee, statistiques de l'état civil en géographie au 01/01/2025")
      ),"codvar"]))|(
        names(.)%in%pull(metadossier[which(
          metadossier$source=="Insee, Système d'information sur la démographie d'entreprises (SIDE) en géographie au 01/01/2025"
        ),"codvar"])&str_detect(names(.),"^ETC|^ENCITOT|^ENCTOT")
      ))) %>% 
  pivot_longer(2:ncol(.),values_to="valeur",names_to="codvar") %>% 
  left_join(
    metadossier[,1:2] %>% 
      set_names(c("codvar","libvar")) %>% 
      mutate(libvar=str_remove(libvar,"\\s*\\(\\w*\\)")) %>% 
      group_by(codvar) %>% 
      filter(row_number()==1) %>% 
      ungroup(),
    by=join_by(codvar)
  ) %>% 
  mutate(annee=case_when(str_detect(codvar,"^[A-Z][0-9]{2}_")~as.numeric(paste("20",substr(codvar,2,3),sep="")),
                         str_detect(codvar,"[0-9]{2}$")~as.numeric(paste("20",str_extract(codvar,"[0-9]{2}$"),sep="")),
                         T~NA),
         codvar=case_when(str_detect(codvar,"^[A-Z][0-9]{2}_")~str_remove(codvar,"^[A-Z][0-9]{2}_"),
                          str_detect(codvar,"[0-9]{2}$")~str_remove(codvar,"[0-9]{2}$"),
                          T~codvar)
  ) %>% 
  rename(ccom=CODGEO) %>% 
  unique()


#IMPORTATION BUDGETS MUNICIPAUX
#TELECHARGEMENT : https://data.ofgl.fr/explore/dataset/ofgl-base-communes-consolidee/information/?disjunctive.agregat&disjunctive.reg_name&disjunctive.dep_name&disjunctive.epci_name&disjunctive.com_name&disjunctive.tranche_population&disjunctive.tranche_revenu_imposable_par_habitant&sort=exer
#Le téléchargement des fichiers 2012-2016 est sur le volet "infos"
#Le téléchargement des fichiers 2017-2024 est sur le volet "export"

#Les seules exceptions au COG2026 sont à nouveau Champtoceaux et Pierrefitte
#Plus exactement, du fait du changement de code d'Orée d'Anjou, ses données 
#sont celles couvertes par le code de Champtoceaux
#Ici, il faut additionner, à constater les statistiques de population dans le 
#cas de Saint-Denis/Pierrefitte-sur-Seine

budgetsmun=read_csv2_chunked(
  "Données/Budgets municipaux/ofgl-base-communes-consolidee.csv",
  callback=DataFrameCallback$new(
    function(x,pos){
      subset(x,
              `Code Insee 2024 Commune`%in%filter(
                communes,
                ccom%in%communesplus15000$ccom
              )$ccomsub
      )
    }
  ),
  chunk_size = 50000,
  trim_ws=TRUE)

budgetsmun=budgetsmun %>% select(c("Exercice","Code Insee 2024 Commune","Agrégat",
                                   "Montant BP","Montant BA",
                                   "Montant flux BP-BA","Montant",
                                   "Montant en € par habitant",
                                   "Population totale")) %>% 
  set_names(c("annee","ccomsub","agregat","montantbp","montantba","fluxbpba",
              "montant","montantparhab","popbud"))

for(i in 2012:2016){
  assign(paste("budgetmun",i,sep=""),
         read_csv2(
           paste("Données/Budgets municipaux/ofgl_base_communes_",i,"_consolidee_pj.csv",sep=""),
           trim_ws = TRUE) %>% 
           filter(`Code Insee 2024 Commune`%in%filter(
             communes,
             ccom%in%communesplus15000$ccom
             )$ccomsub) %>% 
           select(c("Exercice","Code Insee 2024 Commune","Agrégat",
                    "Montant BP","Montant BA","Montant flux BP-BA",
                    "Montant","Montant en € par habitant","Population totale")) %>% 
           set_names(c("annee","ccomsub","agregat","montantbp",
                       "montantba","fluxbpba",
                       "montant","montantparhab","popbud"))
  )
}

for(i in 2012:2016){
  budgetsmun=bind_rows(
    budgetsmun,
    get(paste("budgetmun",i,sep=""))
  )
}

budgetsmun=budgetsmun %>% 
  left_join(unique(communes[,c("ccomsub","ccom")]),by=join_by(ccomsub)) %>% 
  select(-ccomsub) %>% 
  select(ccom,everything()) %>% 
  group_by(ccom,annee,agregat) %>% 
  mutate(across(1:4,sum),
         montantparhab=sum(montantparhab*popbud)/sum(popbud)) %>% 
  select(-popbud) %>% 
  filter(row_number()==1) %>% 
  ungroup()

#TELECHARGEMENT : https://www.insee.fr/fr/statistiques/serie/011794845
#ET : https://www.insee.fr/fr/statistiques/3698339
budgetsmun=left_join(
  read_delim("Données/Masse salariale.csv", 
             delim = ";", escape_double = FALSE, trim_ws = TRUE, 
             skip = 3) %>% 
    select(-3) %>% 
    set_names(c("annee","indice")) %>% 
    mutate(indice=roll_sum(indice,width=4)) %>% 
    filter(str_detect(annee,"T1")) %>% 
    mutate(annee=as.numeric(substr(annee,1,4))) %>% 
    filter(annee%in%2012:2024) %>% 
    arrange(annee),
  read_delim("Données/popfrancedontmayottepost2014inclus.csv", 
             delim = ";", escape_double = FALSE, trim_ws = TRUE, 
             skip = 3) %>% 
    select(-3) %>% 
    set_names(c("annee","popfr")),
  by=join_by(annee)) %>% 
  mutate(indice=indice/popfr,
         indice=indice/indice[which(annee==2019)]) %>% 
  select(annee,indice) %>% 
  left_join(budgetsmun,.,by=join_by(annee)) %>% 
  arrange(annee)
