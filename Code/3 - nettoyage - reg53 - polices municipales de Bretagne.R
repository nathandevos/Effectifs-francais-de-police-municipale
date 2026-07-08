library(tidyverse)
library(readxl)
library(readODS)

#RAPPEL SUR LES DATES :
#/!\ LES DONNEES, EN FAIT DE DECEMBRE, SONT ICI CONSIDEREES COMME AU PREMIER
#JANVIER, POUR CONCORDER AVEC LES DONNEES DEMOGRAPHIQUES. TOUTES LES DATES SONT
#DONC DECALEES : PAR EXEMPLE, LES DONNEES 2015 CORRESPONDENT AUX RESULTATS DE
#L'ENQUETE DE DECEMBRE 2014

dput(departements %>% filter(reg==53) %>% select(dep) %>% pull())
c("22", "29", "35", "56")

#Dans les jeux régionaux, la fonction de purge élimine les remontées non infor- 
#matives (valeurs 0 qui ne contrastent pas avec des déclarations pré-existantes 
#passées ou ultérieures, simple indication de couverture par un dispositif 
#mutualisé, que ce dernier cas se matérialise par des 0 ou un double comptage)
purgesreg53=function(x){
  x %>% 
    filter(
      !(lcom=="GUINGAMP_PAIMPOL_AGGLOMERATION_(GPA)"&annee==2022
      )&!(
        lcom%in%c(
          #Double comptage de la police de Blavet-Bellevue-Océan Communauté
          "MERLEVENEZ",
          #Indication de convention et double comptage avec Theix-Noyalo
          "LE_HEZO","LA_TRINITE_SURZUR",
          #Indication de convention avec Sarzeau
          "LE_TOUR_DU_PARC",
          #Indication de convention avec Cléguerec
          "NEULLIAC",
          #Indication de convention avec Le Conquet
          "PLOUMOGUER",
          #Double comptage de Bourgbarré
          "NOUVOITOU",
          #Indication de convention avec Liffré
          "GOSNE",
          #Indication d'inexistence de service
          "MARTIGNE_FERCHAUD",
          #Double comptage par Férel du service de la Turballe, ou de sa
          #part mutualisée
          "FEREL",
          "FEREL_MUT._LA_TURBALLE_ST_MOLF_ASSERAC",
          "FEREL_MUT._LA_TURBALLE_ST_MOLF_ASSERAC_(44)",
          #Remontée précoce d'une embauche
          "QUESSOY"
        )&annee<2026
      )
    )
}

#Après la correction orthographique, qui applique les corrections indubitables 
#aux libellés, la fonction de redéfinition incorpore les réajustements plus 
#sujets à débat (les Notes d'Attribution sont là pour la justifier)
redefinitionsreg53=function(x){
  
  x=mutate(x,autrescommunes=as.character(NA))
  
  x[which(x$dep=="22"),]=mutate(
    x[which(x$dep=="22"),],
    ccom=case_when(
      str_detect(lcom,"GUINGAMP")&annee<2026~"22070",
      str_detect(lcom,"PLOUMAGOAR")&annee%in%2024:2025~"22225",
      lcom=="POMMERET_QUESSOY"~"22258",
      str_detect(lcom,"^PLESTIN_LES_GREVES")&annee%in%2024:2025~"22194",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="22070"&annee<2022~1,
      ccom=="22094"&annee%in%2019:2025~1,
      ccom=="22194"&annee==2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="22070"&miseadispointerco==1&annee<2026~"as.character(c(22067,22161,22223,22225,22272))",
      ccom=="22225"&miseadispointerco==1&annee<2026~"as.character(c(22067,22223,22272))",
      ccom=="22094"&miseadispointerco==1&annee<2026~"as.character(c(35256))",
      ccom=="22158"&miseadispointerco==1&annee<2026~"as.character(c(56041,56146))",
      ccom=="22194"&miseadispointerco==1&annee<2026~"as.character(c(22226,22319,22349,22350))",
      #Extension à venir de la police pluricommunale, avec l'adjonction de 
      #Lanvellec et Trémel, cf Ouest France 18-19 juin 2025
      #ccom=="22194"&annee==2026~"as.character(c(22119,22226,22319,22349,22350,22366))",
      ccom=="22258"&miseadispointerco==1&annee<2026~"as.character(c(22246))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="29"),]=mutate(
    x[which(x$dep=="29"),],
    ccom=case_when(
      lcom==c("PLOUVIEN_/_BOURG_BLANC")&annee<2026~"29209",
      str_detect(lcom,"LE_CONQUET")&annee<2026~"29040",
      T~ccom),
    miseadispointerco=case_when(
      ccom%in%c("29117","29195")&annee%in%2020:2025~1,
      #Si la convention entre Lannilis et Plouguerneau est maintenue
      #ccom%in%c("29117","29195")&annee%in%2026:2028~1,
      ccom=="29209"&annee%in%2023:2025~1,
      ccom=="29279"&annee%in%2023:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="29023"&miseadispointerco==1&annee<2026~"as.character(c(29279))",
      ccom=="29279"&miseadispointerco==1&annee<2026~"as.character(c(29023))",
      ccom=="29040"&miseadispointerco==1&annee<2026~"as.character(c(29201))",
      ccom=="29117"&miseadispointerco==1&annee<2026~"as.character(c(29195))",
      ccom=="29195"&miseadispointerco==1&annee<2026~"as.character(c(29117))",
      ccom=="29209"&miseadispointerco==1&annee<2026~"as.character(c(29015))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="35"),]=mutate(
    x[which(x$dep=="35"),],
    ccom=case_when(
      str_detect(lcom,"^MAEN_ROCH")&annee<2026~"35257",
      str_detect(lcom,"^VAL_D_ANAST")&annee<2026~"35168",
      T~ccom),
    miseadispointerco=case_when(
      ccom%in%c("35093","35241")&annee%in%2022:2025~1,
      ccom%in%c("35126","35139")&annee==2021~1,
      ccom=="35256"&annee%in%2019:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="35032"&miseadispointerco==1&annee<2026~"as.character(c(35204))",
      ccom=="35093"&miseadispointerco==1&annee<2026~"as.character(c(35241))",
      ccom=="35241"&miseadispointerco==1&annee<2026~"as.character(c(35093))",
      ccom=="35126"&miseadispointerco==1&annee<2026~"as.character(c(35139))",
      ccom=="35139"&miseadispointerco==1&annee<2026~"as.character(c(35126))",
      ccom=="35256"&miseadispointerco==1&annee<2026~"as.character(c(22094))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="56"),]=mutate(
    x[which(x$dep=="56"),],
    ccom=case_when(
      str_detect(lcom,"BLAVET_BELLEVUE|BELLEVUE_BLAVET|CCBBO")&annee<2026~"245600440",
      str_detect(lcom,"^THEIX_NOYALO")&annee<2026~"56251",
      str_detect(lcom,"^SARZEAU")&annee<2026~"56240",
      str_detect(lcom,"^INZINZAC_LOCHRIST")&annee<2026~"56090",
      str_detect(lcom,"^PLOUAY")&annee<2026~"56166",
      str_detect(lcom,"^QUEVEN")&annee<2026~"56185",
      lcom=="PLOEMEL_ERDEVEN"&annee<2026~"56054",
      T~ccom),
    miseadispointerco=case_when(
      ccom%in%c("56008","56106")&annee%in%2014:2019~1,
      ccom=="56054"&annee==2017~1,
      ccom%in%c("56067","56158")&annee==2025~1,
      ccom=="56090"&annee==2023~0,
      #Si la convention entre Inzinzac-Lochrist et Bubry courait bien en 2024
      ccom=="56090"&annee==2025~1,
      ccom=="56107"&annee==2022~0,
      ccom=="56185"&annee==2022~0,
      ccom=="56185"&annee==2023~1,
      ccom=="56240"&annee%in%2019:2025~1,
      ccom=="56251"&annee%in%2015:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="56008"&miseadispointerco==1&annee<2026~"as.character(c(56106))",
      ccom=="56106"&miseadispointerco==1&annee<2026~"as.character(c(56008))",
      ccom=="56053"&miseadispointerco==1&annee<2026~"as.character(c(56137,56231,56247,56254,56255))",
      ccom=="56054"&miseadispointerco==1&annee<2026~"as.character(c(56161))",
      ccom=="56067"&miseadispointerco==1&annee<2026~"as.character(c(56158))",
      ccom=="56158"&miseadispointerco==1&annee<2026~"as.character(c(56067))",
      ccom=="56090"&miseadispointerco==1&annee<2026~"as.character(c(56026))",
      ccom=="56107"&miseadispointerco==1&annee<2026~"as.character(c(56121))",
      ccom=="56121"&miseadispointerco==1&annee<2026~"as.character(c(56107))",
      ccom=="56166"&miseadispointerco==1&annee<2026~"as.character(c(56029,56040,56089))",
      ccom=="56185"&miseadispointerco==1&annee<2026~"as.character(c(56063))",
      ccom=="56186"&miseadispointerco==1&annee==2025~"as.character(c(56234))",
      ccom=="56234"&miseadispointerco==1&annee==2025~"as.character(c(56186))",
      ccom=="56251"&miseadispointerco==1&annee<2026~"as.character(c(56084,56259))",
      ccom=="56240"&miseadispointerco==1&annee<2024~"as.character(c(56252))",
      ccom=="56240"&miseadispointerco==1&annee%in%2024:2025~"as.character(c(56205,56252))",
      T~autrescommunes)
    )
  
  #Cas de Plouénour-Brignogan-Plages et Kerlouan (Côte des Légendes)
  x=x %>% 
    bind_rows(
      x %>% 
        filter(str_detect(lcom,"COTE_DES_LEGENDES")&annee==2024) %>% 
        mutate(ccom="29021",
               lcom="PLOUNEOUR_BRIGNOGAN_PLAGES",
               polmun=1,
               autrescommunes="as.character(c(29091))")
    ) %>% 
    mutate(
      polmun=if_else(str_detect(lcom,"COTE_DES_LEGENDES")&annee==2024,1,polmun),
      ccom=if_else(str_detect(lcom,"COTE_DES_LEGENDES")&annee==2024,"29091",ccom),
      autrescommunes=if_else(str_detect(lcom,"COTE_DES_LEGENDES")&annee==2024,
                             "as.character(c(29021))",autrescommunes)
    )
  
  x[which(x$ccom=="29021"&x$annee%in%2024:2025),"autrescommunes"]="as.character(c(29091))"
  x[which(x$ccom=="29091"&x$annee%in%2024:2025),"autrescommunes"]="as.character(c(29021))"
  
  #Cas de Liffré, La Bouëxière, Gosné, et Saint-Aubin-du-Cormier
  for(i in 1:4){
    x[which(x$miseadispointerco==1&x$ccom==c("35031","35121","35152","35253")[i]),
      "autrescommunes"]=paste(
        "as.character(c(",
        paste(c("35031","35121","35152","35253")[-i],collapse=","),
        "))",
        sep="")
  }
  
  #Case de La Fresnais, La Gouesnière, Hirel et Saint-Benoît-des-Ondes
  x=bind_rows(
    filter(x,!ccom%in%c("35116","35122","35132","35255")),
    data.frame(
      dep=rep("35",16),
      ccom=c(rep("35116",4),rep("35122",4),rep("35132",4),rep("35255",4)),
      lcom=c(
        rep("LA_FRESNAIS",4),
        rep("LA_GOUESNIERE",4),
        rep("HIREL",4),
        rep("ST_BENOIT_DES_ONDES",4)
      ),
      annee=rep(2022:2025,4),
      miseadispointerco=rep(1,16),
      polmun=rep(0,16),
      asvp=rep(0,16),
      gardechamp=rep(0.25,16),
      brigcanine=as.numeric(rep(NA,16)),
      maitrechien=rep(0,16),
      chien=rep(0,16),
      autrescommunes=c(
        rep("as.character(c(35122,35132,35255))",4),
        rep("as.character(c(35116,35132,35255))",4),
        rep("as.character(c(35116,35122,35255))",4),
        rep("as.character(c(35116,35122,35132))",4)
      )
    )
  )
  x[which(x$ccom%in%c("35116","35122","35132","35255")&x$annee%in%2022:2025),"gardechamp"]=0.25
  for(i in 1:4){
    x[which(x$annee>=2023&x$ccom==c("35116","35122","35132","35255")[i]),
      "autrescommunes"]=paste(
        "as.character(c(",
        paste(c("35116","35122","35132","35255")[-i],collapse=","),
        "))",
        sep="")
  }
  
  #Cas d'Arradon, Ploeren, ainsi que de la remontée 2022 de Baden
  
  #Si l'on accepte que la remontée 2023 pour Arradon et Ploeren est erronée
  x[which(x$annee==2024&x$ccom%in%c("56003","56164")),"miseadispointerco"]=1
  
  for(i in 1:3){
    
    x[which(x$annee==2023&x$ccom==c("56003","56164","56008")[i]),
      "autrescommunes"]=paste(
        "as.character(c(",
        paste(c("56003","56164","56008")[-i],collapse=","),
        "))",
        sep="")
    
    if(i<=2){
      x[which(x$miseadispointerco==1&x$annee!=2023&x$ccom==c("56003","56164")[i]),
        "autrescommunes"]=paste(
          "as.character(c(",
          paste(c("56003","56164")[-i],collapse=","),
          "))",
          sep="")
    }
    
  }
  
  #Cas de Cléguérec, Guerlédan et Neulliac
  
  x[which(str_detect(x$lcom,"^CLEGUEREC")&x$annee<2026),"ccom"]="56041"
  x[which(x$ccom=="22158"&x$annee%in%2019:2025),"polmun"]=0
  x[which(x$ccom%in%c("22158","56041")&x$annee%in%2019:2025),
    "miseadispointerco"]=1
  
  for(i in 1:2){
    x[which(x$ccom==c("22158","56041")[i]&x$miseadispointerco==1&x$annee<2026),
      "autrescommunes"]=paste(
        "as.character(c(",c("22158","56041")[-i],",56146))",sep=""
        )
  }
  
  #Cas de Crac'h, Locmariaquer, et Saint-Philibert
  for(i in 1:3){

    x[which(x$ccom==c("56046","56116","56233")[i]&x$annee<if_else(
      i==3,2023,2026)),"miseadispointerco"]=1
    
    x[which(x$ccom==c("56046","56116","56233")[i]&x$annee<2023&x$miseadispointerco==1),
      "autrescommunes"]=paste(
        "as.character(c(",
        paste(c("56046","56116","56233")[-i],collapse=","),
        "))",
        sep="")
    
    if(i<=2){
      x[which(x$ccom==c("56046","56116")[i]&x$annee%in%2023:2025&x$miseadispointerco==1),
        "autrescommunes"]=paste(
          "as.character(c(",
          paste(c("56046","56116")[-i],collapse=","),
          "))",
          sep="")
    }
    
  }
  
  #Cas de Gavres, Locmiquélic, Port-Louis, Riantec
  
  #Si l'indication 2022 de mutualisation pour Riantec est erronée.
  #x[which(x$ccom=="56193"&x$annee==2023),"miseadispointerco"]=0
  
  for(i in 1:4){
    x[which(x$annee==2025&x$miseadispointerco==1&x$ccom==c(
      "56062","56118","56181","56193"
      )[i]),"autrescommunes"]=paste(
        "as.character(c(",
        paste(c("56062","56118","56181","56193")[-i],collapse=","),
        "))",
        sep="")
  }
  
  #Cas de Josselin, Guillac, La Grée-Saint-Laurent, et Ploërmel
  #(à partir de 2025, de Saint-Malo-les-Trois-Fontaines)
  x[which(x$ccom=="56165"&x$miseadispointerco==1&x$annee==2025),
    "autrescommunes"]="as.character(c(56091))"
  x[which(x$ccom=="56091"&x$miseadispointerco==1&x$annee%in%2023:2024),
    "autrescommunes"]="as.character(c(56068,56079))"
  x[which(x$ccom=="56091"&x$miseadispointerco==1&x$annee==2025),
    "autrescommunes"]="as.character(c(56068,56079,56165))"
  #x[which(x$ccom=="56091"&x$miseadispointerco==1&x$annee==2026),
  #  "autrescommunes"]="as.character(c(56068,56079,56165,56227))"
  #x[which(x$ccom=="56227"&x$miseadispointerco==1&x$annee==2026),
  #  "autrescommunes"]="as.character(c(56068,56079,56091))"
  
  #Cas de Nivillac, La Roche-Bernard, et Saint-Dolay
  x=bind_rows(
    filter(x,!str_detect(lcom,"NIVILLAC|LA_ROCHE_BERNARD|ST_DOLAY")),
    data.frame(
      dep=rep("56",24),
      ccom=c(rep("56147",12),rep("56195",12)),
      lcom=c(rep("NIVILLAC",12),rep("LA_ROCHE_BERNARD",12)),
      annee=rep(2014:2025,2),
      miseadispointerco=rep(c(0,rep(1,11)),2),
      polmun=c(rep(1,18),0,rep(1,5)),
      asvp=rep(0,24),
      gardechamp=rep(0,24),
      brigcanine=rep(c(0,rep(NA,11)),2),
      maitrechien=rep(c(NA,rep(0,11)),2),
      chien=rep(c(NA,rep(0,11)),2),
      autrescommunes=c(
        NA,
        rep("as.character(c(56195))",6),
        rep("as.character(c(56195,56212))",5),
        NA,
        rep("as.character(c(56147))",6),
        rep("as.character(c(56147,56212))",5)
      )
    )
  )
  
  #Réassignation des effectifs de Merlevenez à la CCBBO
  x[which(x$ccom=="245600440"&x$annee==2024),"asvp"]=2
  
  #Un article du Télégramme témoigne du fait qu'il y avait bien deux policiers
  #municipaux à Pordic en décembre 2022
  x[which(x$ccom=="22251"&x$annee==2023),"polmun"]=2
  x[which(x$ccom=="22251"&x$annee==2023),"miseadispointerco"]=0
  
  return(x)
  
}

polmunreg53=polmun %>% 
  filter(reg==53) %>% 
  purgesreg53() %>% 
  mutate(lcom=standardiserlibelles(lcom,1),
         lcom=standardisationcomplementaire(lcom)) %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("lcom","dep","ccom")],
            by=join_by(lcom,dep)) %>% 
  select(dep,ccom,lcom,annee,everything()) %>% 
  redefinitionsreg53() %>% 
  select(-c("lcom","reg","ldep")) %>% 
  left_join(unique(bind_rows(
    communes %>% 
      filter(typique==1) %>% 
      select(lcom,ccom),
    epci %>% 
      select(LIBEPCI,EPCI) %>% 
      set_names(c("lcom","ccom")))),
    by=join_by(ccom)) %>% 
  redefinitions2(c("22055")) %>% 
  unique() %>% 
  arrange(annee) %>% 
  arrange(lcom) %>% 
  arrange(dep) %>% 
  left_join(departements,by=join_by(dep)) %>% 
  select(reg,dep,ldep,ccom,lcom,annee,everything()) %>% 
  mutate(
    CODautrescommunes=str_replace_all(
      str_remove_all(autrescommunes,"(^as.character\\(c\\()|\\)"),
      ",","_/_")
  )
