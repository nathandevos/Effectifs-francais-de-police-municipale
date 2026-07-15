library(tidyverse)
library(readxl)
library(readODS)

#FICHIERS REGIONAUX
polmunreg=bind_rows(
  polmunreg28,
  polmunreg53
)

polmunreg=left_join(
  polmunreg,
  data.frame(
    autrescommunes=polmunreg %>% 
      select(autrescommunes) %>% 
      filter(!is.na(autrescommunes)) %>% 
      unique() %>% pull(),
    LIBautrescommunes=unlist(lapply(
      as.list(polmunreg %>% 
                select(autrescommunes) %>% 
                filter(!is.na(autrescommunes)) %>% 
                unique() %>% pull()),
      function(x){
        if_else(
          is.na(x),NA,
          paste(pull(
            filter(communes,typique==1)[
              match(
                eval(parse(text=x)),
                filter(communes,typique==1)$ccom
              ),"lcom"
            ]
          ),collapse="_/_"))
      }
    ))),
  by=join_by(autrescommunes)) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2013:2023))) %>% 
              pivot_longer(cols=2:12,values_to="popn",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)+1),
            by=join_by(ccom,annee)) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2014:2023))) %>% 
              pivot_longer(cols=2:11,values_to="popnplusun",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)),
            by=join_by(ccom,annee))

write.csv(x=polmunreg %>% mutate(annee=annee-1) %>% 
            set_names(
              c("REG",
                "DEP",
                "LIBDEP",
                "CODGEO",
                "LIBGEO",
                "ANNEE",
                "Existence d'une convention de mutualisation",
                "Nombre d'agents de police municipale",
                "Nombre d'ASVP",
                "Nombre de gardes champêtres",
                "Nombre de brigades canines (2013)",
                "Nombre de maîtres-chiens de police municipale",
                "Nombre de chiens de patrouille de police municipale",
                "Communes partenaires (commande R)",
                "Communes partenaires (code INSEE)",
                "Communes partenaires (libellé)",
                "Population au 1er janvier de l'année d'enquête",
                "Population au 1er janvier de l'année suivante")
            ),
          file="jeuxregionauxpolicemunicipale.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

#FICHIERS 15000

polmun15000$autrescommunes[which(!is.na(polmun15000$autrescommunes))]=paste(
  "str_pad(width=5,pad=\"0\",",
  str_remove(
    polmun15000$autrescommunes[which(!is.na(polmun15000$autrescommunes))],
    "as.character\\("
    ),
  sep=""
  )

popcouvertes=data.frame(
  ccom=rep(
    unique(polmun15000[which(polmun15000$formea==1),c("ccom","autrescommunes")])$ccom,
    each=12),
  annee=rep(2013:2024,
            nrow(unique(polmun15000[which(polmun15000$formea==1),c("ccom","autrescommunes")]))
  ),
  autrescommunes=rep(
    unique(polmun15000[which(polmun15000$formea==1),c("ccom","autrescommunes")])$autrescommunes,
    each=12)
  ) %>% 
  split(.$autrescommunes) %>% 
  lapply(function(x){
    x=x %>% 
    left_join(popcom %>% 
                select(c("ccom",as.character(2013:2023))) %>% 
                filter(ccom%in%eval(parse(text=x$autrescommunes))) %>% 
                mutate(across(2:12,sum)) %>% 
                filter(row_number()==1) %>% 
                pivot_longer(cols=2:12,values_to="popcouvn",names_to="annee") %>% 
                mutate(annee=as.numeric(annee)) %>% 
                select(-ccom),
              by=join_by(annee)) %>% 
      left_join(popcom %>% 
                  select(c("ccom",as.character(2014:2023))) %>% 
                  filter(ccom%in%eval(parse(text=x$autrescommunes))) %>% 
                  mutate(across(2:11,sum)) %>% 
                  filter(row_number()==1) %>% 
                  pivot_longer(cols=2:11,values_to="popcouvnplusun",names_to="annee") %>% 
                  mutate(annee=as.numeric(annee)-1) %>% 
                  select(-ccom),
                by=join_by(annee))
  }) %>% 
  bind_rows() %>% 
  mutate(annee=annee+1)

polmun15000=left_join(
  polmun15000,
  data.frame(
    autrescommunes=polmun15000 %>% 
      select(autrescommunes) %>% 
      filter(!is.na(autrescommunes)) %>% 
      unique() %>% pull(),
    LIBautrescommunes=unlist(lapply(
      as.list(polmun15000 %>% 
                select(autrescommunes) %>% 
                filter(!is.na(autrescommunes)) %>% 
                unique() %>% pull()),
      function(x){
          paste(pull(
            filter(communes,typique==1)[
              match(
                eval(parse(text=x)),
                filter(communes,typique==1)$ccom
              ),"lcom"
            ]
            ),collapse="_/_")
      }
      ))
    ),
  by=join_by(autrescommunes)) %>% 
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
  left_join(popcouvertes,by=join_by(ccom,annee,autrescommunes)) %>% 
  group_by(ccom) %>% 
  mutate(popcouvnplusun=if_else(
           lead(formea)!=formea,
           popnplusun+transfona0(lead(popcouvn)),
           popnplusun+transfona0(popcouvnplusun)
           ),
         popcouvn=popn+transfona0(popcouvn)
         ) %>% 
  ungroup()

write.csv(x=polmun15000 %>% mutate(annee=annee-1) %>% 
            set_names(
              c("REG",
                "DEP",
                "LIBDEP",
                "CODGEO",
                "LIBGEO",
                "ANNEE",
                "Existence d'une convention de mutualisation",
                "Nombre d'agents de police municipale",
                "Nombre d'ASVP",
                "Nombre de gardes champêtres",
                "Nombre de brigades canines (2013)",
                "Nombre de maîtres-chiens de police municipale",
                "Nombre de chiens de patrouille de police municipale",
                "Communes partenaires (commande R)",
                "Forme A (voir Notes d'attribution)",
                "Communes partenaires (code INSEE)",
                "Communes partenaires (libellé)",
                "Population municipale au 1er janvier de l'année d'enquête",
                "Population municipale au 1er janvier de l'année suivante",
                "Population couverte au 1er janvier de l'année d'enquête",
                "Population couverte au 1er janvier de l'année suivante")
            ),
          file="policesmunicipalesplusde15000.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

#FICHIERS SEMI-BRUT

polmundsb=polmun %>% 
  correctionsortho2024() %>% 
  reportnotesdsb() %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("ccomsub","ccom","lcom","dep")],by=join_by(lcom,dep)) %>% 
  redefinitionsdsb() %>% 
  filter(
    !(str_detect(lcom,"CHIFFRES")&annee==2020)&!(
      lcom=="MOUGINS"&polmun==0&annee==2022
    )
  ) %>% 
  select(reg,dep,ldep,ccom,ccomsub,lcom,notes,everything()) %>% 
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
  mutate(
    dep=factor(
      dep,levels=c(str_pad(as.character(1:19),2,pad="0"),"2A","2B",
                   as.character(21:95),as.character(971:989))
    ),
    ccomsub=if_else(
      is.na(ccomsub)&nchar(ccom)==5&!ccom%in%c("50129","59183","59350"),
      ccom,ccomsub
    )
  ) %>% 
  unique() %>% 
  arrange(lcom) %>% 
  arrange(annee) %>% 
  arrange(ccom) %>% 
  arrange(dep)

#Pourcentage de codes standardisés complétés
(polmundsb %>% filter(!is.na(ccom)) %>% nrow())/nrow(polmundsb)

#On vérifie que ccomsub est bien vierge dans les cas d'une communauté de 
#communes ou d'une colonne ccom également vierge
polmundsb %>% filter(!is.na(ccomsub)&nchar(ccom)!=5) %>% View()

#Vérification de la bonne concordance des libellés d'EPCI, afin d'identifier 
#d'éventuelles erreurs de recopiage des codes dans la fonction de redéfinition
polmundsb %>% 
  rename(EPCI=ccom) %>% 
  left_join(unique(epci[,c("EPCI","LIBEPCI")]),by=join_by(EPCI)) %>% 
  filter(!is.na(LIBEPCI)) %>% 
  select(lcom,LIBEPCI) %>% 
  unique() %>% 
  View()

#Inspection des doublons au titre de ccomsub (il est parfaitement normal d'en
#avoir au titre de ccom). Il est normal qu'un certain nombre de ces doublons
#perdure. On devine qu'ils sont souvent engendrés par une remontée
#rectificatrice signalant la perte d'un agent, ou par deux remontées (une par
#saison) d'un nombre d'ASVP très saisonnier (voir La Roque-sur-Cèze)
polmundsb %>% 
  filter(!is.na(ccomsub)) %>% 
  group_by(annee,ccomsub) %>% 
  mutate(oc=1,oc=sum(oc)) %>% 
  filter(oc>1) %>% 
  ungroup() %>% 
  View()

#Vérification que les notes sont toutes des choses qu'il est normal de retrouver
#en notes
polmundsb %>% 
  filter(!is.na(notes)) %>% 
  select(notes) %>% 
  unique() %>% View()

#Vérification que les codes non complétés sont bien incomplétables
polmundsb %>% 
  filter(is.na(ccom)) %>% 
  select(lcom) %>% 
  unique() %>% View()

#Vérification que les codes sub non complétés hors EPCI sont bien incomplétables
polmundsb %>% filter(is.na(ccomsub)&nchar(ccom)==5) %>% View()
polmundsb %>% filter(is.na(ccomsub)&!is.na(ccom)) %>% View()

write.csv(x=polmundsb %>% mutate(annee=annee-1) %>% 
            set_names(
              c("REG",
                "DEP",
                "LIBDEP",
                "CODGEO (COG2026)",
                "CODGEO (subcommunal ou passé)",
                "Libellé original",
                "Notes originales",
                "ANNEE",
                "Existence d'une convention de mutualisation",
                "Nombre d'agents de police municipale",
                "Nombre d'ASVP",
                "Nombre de gardes champêtres",
                "Nombre de brigades canines (2013)",
                "Nombre de maîtres-chiens de police municipale",
                "Nombre de chiens de patrouille de police municipale",
                "Population au 1er janvier de l'année d'enquête",
                "Population au 1er janvier de l'année suivante")
            ),
          file="jeusemibrut.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

#EXPORTATION DES CORRESPONDANCES COMMUNES (si jamais c'est utile)

write.csv(x=communes,
          file="communes.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


