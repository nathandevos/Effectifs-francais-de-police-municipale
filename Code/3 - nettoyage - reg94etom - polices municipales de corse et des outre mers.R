library(tidyverse)
library(readxl)
library(readODS)

redefinitionsreg94=function(x){
  
  x[which(x$dep=="2A"),]=mutate(
    x[which(x$dep=="2A"),],
    ccom=case_when(
      str_detect(lcom,"CAPA|PAYS_AJACCIEN")&annee<2026~"242010056",
      T~ccom)
  )
  
  x[which(x$dep=="2B"),]=mutate(
    x[which(x$dep=="2B"),],
    ccom=case_when(
      str_detect(lcom,"CALVI_BALAGNE")&annee<2026~"242020105",
      T~ccom)
  )
  
  return(x)
  
}

polmunreg94=polmun %>% 
  mutate(lcom=standardiserlibelles(lcom,1),
         lcom=standardisationcomplementaire(lcom)) %>% 
  filter(reg==94) %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("lcom","ccom","dep")],
            by=join_by(lcom,dep)) %>% 
  redefinitionsreg94() %>% 
  select(-lcom) %>% 
  left_join(unique(bind_rows(
    communes %>% 
      filter(typique==1) %>% 
      select(lcom,ccom),
    epci %>% 
      select(LIBEPCI,EPCI) %>% 
      set_names(c("lcom","ccom")))),
    by=join_by(ccom)) %>% 
  select(dep,ccom,lcom,annee,everything()) %>% 
  select(dep,ldep,ccom,lcom,annee,miseadispointerco,
         polmun,asvp,gardechamp,brigcanine,maitrechien,chien) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2013:2023))) %>% 
              pivot_longer(cols=2:12,values_to="popn",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)+1),
            by=join_by(ccom,annee)) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2014:2023))) %>% 
              pivot_longer(cols=2:11,values_to="popnplusun",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)),
            by=join_by(ccom,annee)) %>% 
  arrange(annee) %>% 
  arrange(lcom) %>% 
  arrange(dep)

redefinitionsOM=function(x){
  
  x[which(x$dep=="971"),]=mutate(
    x[which(x$dep=="971"),],
    ccom=case_when(
      lcom=="COMMUNAUTE_AGGLOMERATION_DU_SUD_BASSE_TERRE"~"249710070",
      T~ccom)
  )
  
  x[which(x$dep=="974"),]=mutate(
    x[which(x$dep=="974"),],
    ccom=case_when(
      lcom=="TERRITOIRE_DE_LA_COTE_OUEST"~"249740101",
      T~ccom)
  )
  
  x[which(x$dep=="976"),]=mutate(
    x[which(x$dep=="976"),],
    ccom=case_when(
      lcom%in%c("COMMUNAUTE_DE_COMMUNES_DU_SUD_(EPCI)",
                "INTERCO_CCSUD")~"200060473",
      lcom%in%c("COMMUNAUTE_DE_COMUNES_DE_PETITE_TERRE_(EPCI)",
                "INTERCO_PT")~"200050532",
      T~ccom)
  )
  
  return(x)
  
}

polmunOM=polmun %>% 
  mutate(lcom=standardiserlibelles(lcom,1),
         lcom=standardisationcomplementaire(lcom)) %>% 
  #Elimination d'une ligne vierge et filtrage des Outre-Mers
  filter(!(lcom==":_CHIFFRES_ISSUS_DE_L_ENQUETE"&annee==2020)&str_detect(dep,"^9(7|8)")) %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("lcom","ccom","dep")],
            by=join_by(lcom,dep)) %>% 
  redefinitionsOM() %>% 
  select(-lcom) %>% 
  left_join(unique(bind_rows(
    communes %>% 
      filter(typique==1) %>% 
      select(lcom,ccom),
    epci %>% 
      select(LIBEPCI,EPCI) %>% 
      set_names(c("lcom","ccom")))),
    by=join_by(ccom)) %>% 
  #Eliminationde de 3 lignes vierges non instructives
  filter(!((dep=="975"&annee<2026)|(ccom=="98718"&annee==2019))) %>% 
  select(dep,ccom,lcom,annee,everything()) %>% 
  select(dep,ldep,ccom,lcom,annee,miseadispointerco,
         polmun,asvp,gardechamp,brigcanine,maitrechien,chien) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2013:2023))) %>% 
              pivot_longer(cols=2:12,values_to="popn",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)+1),
            by=join_by(ccom,annee)) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2014:2023))) %>% 
              pivot_longer(cols=2:11,values_to="popnplusun",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)),
            by=join_by(ccom,annee)) %>% 
  arrange(annee) %>% 
  arrange(lcom) %>% 
  arrange(dep)

write.csv(x=bind_rows(polmunreg94,polmunOM) %>% 
            mutate(annee=annee-1,
                   dep=factor(dep,levels=c(str_pad(as.character(1:19),2,pad="0"),
                                           "2A","2B",as.character(21:96),
                                           as.character(971:989)))) %>% 
            set_names(
              c("DEP",
                "LIBDEP",
                "CODGEO",
                "LIBGEO",
                "ANNEE",
                "Mise en commun d'agents de police municipale entre plusieurs communes (binaire)",
                "Nombre d'agents de police municipale",
                "Nombre d'ASVP",
                "Nombre de gardes champêtres",
                "Nombre de brigades canines (2014)",
                "Nombre de maîtres chiens de police municipale",
                "Nombre de chiens de patrouille de police municipale",
                "Population au 1er janvier de l'année d'enquête",
                "Population au 1er janvier de l'année suivante")
            ),
          file="effectifspolicemunicipaleoutremers.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
