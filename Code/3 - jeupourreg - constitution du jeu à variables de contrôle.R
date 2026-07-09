library(tidyverse)
library(readxl)
library(readODS)
library(estimatr)

logna=function(x){if_else(x==0,NA,log(x))}

jeupourreg=popcom %>% 
  filter(ccom%in%communesplus15000$ccom) %>% 
  mutate(lcom=standardisationcomplementaire(standardiserlibelles(lcom,0))) %>% 
  select(c("dep","ccom","lcom",as.character(2016:2022))) %>% 
  pivot_longer(4:ncol(.),names_to="annee",values_to="pop") %>% 
  mutate(annee=as.numeric(annee),
         pop=pop/1000,
         d2020=if_else(annee==2020,1,0)) %>% 
  left_join(zonages1,by=join_by(ccom)) %>% 
  left_join(zonages2,by=join_by(ccom)) %>% 
  left_join(zonages3,by=join_by(ccom)) %>% 
  mutate(pseudoaire=case_when(
    str_detect(ccom,"^(75|77|78|91|92|93|94|95)")~paste("paris",substr(ccom,1,2),sep=""),
    T~aireurbaine)) %>% 
  left_join(txchom,by=join_by(zoneemploi,annee)) %>% 
  mutate(logtxchom=log(txchom),
         log2txchom=log(txchom)^2,
         lin2txchom=txchom*log(txchom)-txchom,
         cartxchom=txchom^2)

jeupourreg=left_join(jeupourreg,polmunpourreg,by=join_by(ccom,annee)) %>% 
  left_join(donneescrimcom15000,by=join_by(ccom,annee))

variablesfisc=c("revparhab","irppparhab",
                "partsalaries","partretraites",
                "partpauvres","partaises")

regresseursfisc=c("revparhab","residuirppparhab",
                  "partsalaries","partretraites",
                  "partpauvres","partaises")

regresseursfisc2=c("revparhab","residuirppparhab",
                  "partsalaries","partretraites",
                  "partpauvres","partaises")

jeupourreg=revenusfisc %>% 
  select(ccom,annee,trancherev,nbrefoyers,revtot,impotnettot,
         nbrefoyerssal,nbrefoyersretraites) %>% 
  group_by(ccom,annee) %>% 
  mutate(partpauvres=nbrefoyers[which(trancherev=="0 à 10 000")],
         partaises=sum(nbrefoyers[which(str_detect(trancherev,"100 000"))]),
         #ratio foyers salaries, retraites, pauvres et aisés sur total foyers
         across(5:8,~./nbrefoyers)) %>% 
  ungroup() %>% 
  filter(trancherev=="total") %>% 
  select(-c("trancherev","nbrefoyers")) %>% 
  group_by(annee) %>% 
  mutate(
    #Colonnes revtot et impotnettot donc
    #Rectifie le x1000 introduit par la division par la population
    across(2:3,~./1000),
    #Colonnes partsalaries et partretraites donc
    #Passe en points de pourcentage
    across(4:5,~.*100),
    #Colonnes partpauvres et partaises donc
    #Elimine la trajectoire nationale pour redresser les effets inflationnistes
    across(6:7,~./mean(.,na.rm=T))
  ) %>% 
  ungroup() %>% 
  rename(revparhab=revtot,
         irppparhab=impotnettot,
         partsalaries=nbrefoyerssal,
         partretraites=nbrefoyersretraites) %>% 
  left_join(budgetsmun %>% select(annee,indice) %>% unique(),
            by=join_by(annee)) %>% 
  left_join(jeupourreg,.,by=join_by(ccom,annee)) %>% 
  mutate(
    across(which(names(.)%in%c("revparhab","irppparhab")),~./pop/indice),
    across(which(names(.)%in%variablesfisc),logna,.names="log{col}"),
    across(which(names(.)%in%variablesfisc),~.^2,.names="lincar{col}"),
    across(which(names(.)%in%variablesfisc),~if_else(.==0,NA,.*log(.)-.),.names="lin2{col}"),
    across(which(names(.)%in%variablesfisc),~if_else(.==0,NA,log(.)^2),.names="log2{col}")
  )

jeupourreg[
  which(!is.na(jeupourreg$irppparhab)&!is.na(jeupourreg$revparhab)),
  (ncol(jeupourreg)+1):(ncol(jeupourreg)+2)
]=jeupourreg[
  which(!is.na(jeupourreg$irppparhab)&!is.na(jeupourreg$revparhab)),
] %>% 
  mutate(
    residuirppparhab=lm(data=.,irppparhab~revparhab)$residuals,
    lincarresiduirppparhab=lm(data=.,lincarirppparhab~lincarrevparhab)$residuals
  ) %>% 
  select((ncol(.)-1):ncol(.))

jeupourreg[
  which(!is.na(jeupourreg$irppparhab)&!is.na(jeupourreg$revparhab)&jeupourreg$irppparhab*jeupourreg$revparhab!=0),
  (ncol(jeupourreg)+1):(ncol(jeupourreg)+3)
]=jeupourreg[
  which(!is.na(jeupourreg$irppparhab)&!is.na(jeupourreg$revparhab)&jeupourreg$irppparhab*jeupourreg$revparhab!=0),
] %>% 
  mutate(
    logresiduirppparhab=lm(data=.,logirppparhab~logrevparhab)$residuals,
    log2residuirppparhab=lm(data=.,log2irppparhab~log2revparhab)$residuals,
    lin2residuirppparhab=lm(data=.,lin2irppparhab~lin2revparhab)$residuals,
  ) %>% 
  select((ncol(.)-2):ncol(.))

regresseursdc=tolower(c("NAISD","DECESD","ENCITOT","ENCTOT",
                        "ETCBE","ETCFZ","ETCGI","ETCMN", 
                        "ETCOQ","ETCRU","DENS"))

#4 NA introduits par la variable créations d'établissements indus
jeupourreg=dossierpourreg %>% 
  mutate(codvar=tolower(codvar)) %>% 
  filter(codvar%in%regresseursdc&codvar!="dens") %>% 
  select(-libvar) %>% 
  pivot_wider(names_from="codvar",values_from="valeur") %>% 
  left_join(jeupourreg,.,by=join_by(ccom,annee)) %>% 
  left_join(dossierpourreg %>% 
              filter(codvar=="SUPERF") %>% 
              select(ccom,valeur) %>% 
              set_names(c("ccom","dens")) %>% 
              unique(),
            by=join_by(ccom)) %>% 
  left_join(popcom %>% 
              filter(ccom%in%communesplus15000$ccom) %>% 
              select(c("ccom","2023")) %>% 
              set_names(c("ccom","pop2023")),
            by=join_by(ccom)) %>% 
  arrange(annee) %>% 
  arrange(ccom) %>% 
  mutate(across(which(names(.)%in%regresseursdc),~./pop),
         #Les créations d'entreprise seront par 10 000 habitants
         across(which(str_detect(names(.),"^etc|^enc")),~.*10),
         across(which(names(.)%in%regresseursdc),logna,.names="log{col}"),
         #densité exprimée comme habitants par km2
         dens=1/dens*1000)

variablesbudmun=c("epargnebrute","annuitedette",
                  "dgf","depfonc","depinv")

#Inclure dgf dans les régressions logarithmiques ampute 12 communes de 3 obser-
#vations ou plus. Or il est tout à fait évident, comme dgf est corrélé au niveau 
#de pauvreté, que la censure de ces 12 communes serait une censure endogène 
#biaisant potentiellement les coefficients estimés.
regresseursbudmun=c("epargnebrute","annuitedette",
                    "depfonc","depinv")

regresseursbudmun2=c("epargnebrute","annuitedette",
                     "depfonc","depinv")

jeupourreg=budgetsmun %>% 
  mutate(montantparhab=montantparhab/indice) %>% 
  select(ccom,annee,agregat,montantparhab) %>% 
  filter(agregat%in%c(
    "Epargne brute",
    "Annuité de la dette",
    "Dotation globale de fonctionnement",
    "Dépenses de fonctionnement",
    "Dépenses d'investissement hors remb"
  )) %>% 
  mutate(agregat=case_when(
    agregat=="Epargne brute"~"epargnebrute",
    agregat=="Annuité de la dette"~"annuitedette",
    agregat=="Dotation globale de fonctionnement"~"dgf",
    agregat=="Dépenses de fonctionnement"~"depfonc",
    agregat=="Dépenses d'investissement hors remb"~"depinv"
  )) %>% 
  pivot_wider(names_from=agregat,values_from=montantparhab) %>% 
  mutate(dgf=transfona0(dgf),
         across(which(names(.)%in%variablesbudmun),
                ~if_else(.<=0,NA,log(.)),.names="log{col}"),
         across(which(names(.)%in%variablesbudmun),
                ~if_else(.<=0,NA,.*log(.)-.),.names="lin2{col}"),
         across(which(names(.)%in%variablesbudmun),
                ~if_else(.<=0,NA,log(.)^2),.names="log2{col}"),
         across(which(names(.)%in%variablesbudmun),
                ~.^2,.names="lincar{col}"),
  ) %>% 
  mutate(across(1:ncol(.),~if_else(is.nan(.),NA,.))) %>% 
  left_join(jeupourreg,.,by=join_by(ccom,annee))

regresseursdvf=c("nbremaisons","nbremutations","surfacemoy","prixmoym2")

jeupourreg=left_join(
  jeupourreg,
  DVF %>% 
    select(c("ccom","annee",all_of(regresseursdvf))) %>% 
    left_join(revenusfisc %>% 
                filter(trancherev=="total") %>% 
                select(ccom,annee,nbrefoyers) %>% 
                mutate(nbrefoyers=nbrefoyers/1000),
              by=join_by(ccom,annee)) %>% 
    left_join(budgetsmun %>% 
                select(ccom,annee,indice) %>% 
                unique(),
              by=join_by(ccom,annee)) %>% 
    mutate(prixmoym2=prixmoym2/indice,
           nbremaisons=nbremaisons/nbrefoyers,
           nbremutations=nbremutations/nbrefoyers,
           across(which(names(.)%in%regresseursdvf),
                  logna,.names="log{col}")) %>% 
    select(-c("nbrefoyers","indice")),
  by=join_by(ccom,annee)
)

jeupourreg=jeupourreg %>% 
  left_join(
    zonescompetence %>% 
      filter(ccom%in%communesplus15000$ccom) %>% 
      mutate(GN=if_else(institution=="GN",1,0),
             PN=if_else(institution=="PN",1,0)) %>% 
      group_by(ccom) %>% 
      mutate(GN=sum(GN),PN=sum(PN)) %>% 
      filter(row_number()==1) %>% 
      ungroup() %>% 
      select(ccom,PN,GN),
    by=join_by(ccom)
  ) %>% 
  left_join(
    BTSR %>% 
      select(annee,DEPR,gendbts) %>% 
      set_names(c("annee","dep","gendarmes")),
    by=join_by(annee,dep)
  ) %>% 
  left_join(
    BTST %>% 
      select(annee,DEPT,polnatbts) %>% 
      set_names(c("annee","dep","polnat")),
    by=join_by(annee,dep)
  ) %>% 
  mutate(gendarmes=if_else(GN==0,0,gendarmes),
         polnat=if_else(PN==0,0,polnat),
         across(which(names(.)%in%c("polnat","gendarmes")),
                ~if_else(.==0,0,log(.)),.names="log{col}"),
         across(which(names(.)%in%c("polnat","gendarmes")),
                ~if_else(.==0,0,.*log(.)-.),.names="lin2{col}"),
         across(which(names(.)%in%c("polnat","gendarmes")),
                ~.^2,.names="lincar{col}"),
         across(which(names(.)%in%c("polnat","gendarmes")),
                ~if_else(.==0,0,log(.)^2),.names="log2{col}")
  )

#EXPORTATION
#write.csv(x=jeupourreg,file="jeupolmunpourreg.csv",row.names=FALSE,fileEncoding="UTF-8")

