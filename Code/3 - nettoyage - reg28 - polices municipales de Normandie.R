library(tidyverse)
library(readxl)
library(readODS)

#RAPPEL SUR LES DATES :
#/!\ LES DONNEES, EN FAIT DE DECEMBRE, SONT ICI CONSIDEREES COMME AU PREMIER
#JANVIER, POUR CONCORDER AVEC LES DONNEES DEMOGRAPHIQUES. TOUTES LES DATES SONT
#DONC DECALEES : PAR EXEMPLE, LES DONNEES 2015 CORRESPONDENT AUX RESULTATS DE
#L'ENQUETE DE DECEMBRE 2014

dput(departements %>% filter(reg==28) %>% select(dep) %>% pull())
c("14", "27", "50", "61", "76")

#Dans les jeux régionaux, la fonction de purge élimine les remontées non infor- 
#matives (valeurs 0 qui ne contrastent pas avec des déclarations pré-existantes 
#passées ou ultérieures, simple indication de couverture par un dispositif 
#mutualisé, que ce dernier cas se matérialise par des 0 ou un double comptage)
purgesreg28=function(x){
  x %>% 
    filter(!(
      (
        #Simples indications de couverture par la police de Ouistreham
        lcom=="COLLEVILLE_MONTGOMERY"&annee%in%2022:2025
      )|(
        #Indicateur de couverture par Les Grandes-Ventes avec double comptage
        lcom=="TORCY_LE_GRAND"&annee%in%2022:2025
      )|(
        lcom%in%c(
          "TOURGEVILLE","MOULT_CHICHEBOVILLE","ST_AUBIN_D_ARQUENAY",
          "AVREMESNIL","CLEON","GRUCHET_ST_SIMEON","NOTRE_DAME_DE_BLIQUETUIT",
          "VARNEVILLE_BRETTEVILLE","VATTEVILLE_LA_RUE"
        )&annee<2026
      )
    )
    )
}

#Après la correction orthographique, qui applique les corrections indubitables 
#aux libellés, la fonction de redéfinition incorpore les réajustements plus 
#sujets à débat (les Notes d'Attribution sont là pour la justifier)
redefinitionsreg28=function(x){
  
  x=mutate(x,autrescommunes=as.character(NA)) %>% 
    filter(!(str_detect(lcom,"ARR_(ROUEN|LE_HAVRE)")&annee==2015))
  
  x[which(x$dep=="14"),]=mutate(
    x[which(x$dep=="14"),],
    ccom=case_when(
      lcom=="SALINE"&annee<2026~"14712",
      str_detect(lcom,"BENERVILLE")&annee<2026~"14059",
      str_detect(lcom,"OUISTREHAM")&annee<2026~"14488",
      str_detect(lcom,"TROUVILLE_SUR_MER")&annee<2026~"14715",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="14059"&annee<2026~1,
      ccom=="14488"&annee%in%2018:2025~1,
      ccom=="14712"&annee%in%2017:2019~1,
      ccom=="14715"&annee%in%2016:2021~1,
      ccom%in%c("14228","14384")&annee%in%2016:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="14059"&miseadispointerco==1&annee<2026~"as.character(c(14701))",
      ccom=="14488"&miseadispointerco==1&annee%in%2018:2021~"as.character(c(14166))",
      ccom=="14488"&miseadispointerco==1&annee%in%2022:2025~"as.character(c(14166,14558))",
      ccom=="14712"&miseadispointerco==1&annee<2026~"as.character(c(14666))",
      ccom=="14715"&miseadispointerco==1&annee!=2020&annee<2026~"as.character(c(14699))",
      ccom=="14715"&miseadispointerco==1&annee==2020~"as.character(c(14699,14755))",
      ccom=="14020"&miseadispointerco==1&annee<2026~"as.character(c(14456))",
      ccom=="14228"&miseadispointerco==1&annee<2026~"as.character(c(14384))",
      ccom=="14384"&miseadispointerco==1&annee<2026~"as.character(c(14228))",
      T~autrescommunes)
    )
  
  x[which(x$dep=="27"),]=mutate(
    x[which(x$dep=="27"),],
    ccom=case_when(
      str_detect(lcom,"PONT_DE_L_ARCHE")&annee<2026~"27469",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="27469"&annee%in%2020:2025~1,
      ccom=="27562"&annee%in%2023:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="27469"&miseadispointerco==1&annee<2026~"as.character(c(27188))",
      ccom=="27056"&miseadispointerco==1&annee<2026~"as.character(c(27398))",
      ccom=="27375"&miseadispointerco==1&annee<2026~"as.character(c(27351))",
      ccom=="27562"&miseadispointerco==1&annee<2026~"as.character(c(27681))",
      ccom=="27681"&miseadispointerco==1&annee<2026~"as.character(c(27562))",
      T~autrescommunes)
    )
  
  x[which(x$dep=="50"),]=mutate(
    x[which(x$dep=="50"),],
    ccom=case_when(
      lcom=="COMMUNAUTE_DE_COMMUNES_CARENTAN_EN_COTENTIN"&annee<2026~"200042729",
      lcom=="CHERBOURG_EN_COTENTIN_/_TOURLAVILLE"&annee<2026~"50129",
      lcom=="AGON_COUTAINVILLE_/_BLAINVILLE_SUR_MER"&annee<2026~"50003",
      lcom=="GRANVILLE_/_DONVILLE_LES_BAINS"&annee<2026~"50218",
      (str_detect(lcom,"LE_MONT_ST_MICHEL")&annee<2026
       )|(lcom=="PONTORSON"&annee==2017)~"50353",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="50218"&annee%in%2018:2025~1,
      ccom=="50353"&annee%in%2018:2025~1,
      ccom=="50129"&annee==2025~0,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="50003"&miseadispointerco==1&annee<2026~"as.character(c(50058))",
      ccom=="50218"&miseadispointerco==1&annee<2026~"as.character(c(50165))",
      ccom=="50353"&miseadispointerco==1&annee<2026~"as.character(c(50042,50410))",
      T~autrescommunes
    )
    )
  
  x[which(x$dep=="61"),]=mutate(
    x[which(x$dep=="61"),],
    autrescommunes=case_when(
      ccom=="61483"&miseadispointerco==1&annee<2026~"as.character(c(61096))",
      T~autrescommunes
    )
    )
  
  x[which(x$dep=="76"),]=mutate(
    x[which(x$dep=="76"),],
    ccom=case_when(
      (lcom=="MAIRIE"|str_detect(lcom,"LE_?TRAIT"))&annee<2026~"76709",
      lcom=="FORGES_LES_EAUX_/_LE_FOSSE"&annee<2026~"76276",
      lcom=="LES_GRANDES_VENTES_ET_TORCY_LE_GRAND"&annee<2026~"76321",
      str_detect(lcom,"BERNEVAL_LE_GRAND")&annee<2026~"76618",
      str_detect(lcom,"CAUX_((SEINE_AGGLO)|(VALLEE?_DE_SEINE))")&annee<2026~"200010700",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="76400"&annee==2022~1,
      ccom=="76709"&annee<2026~1,
      ccom=="76321"&annee%in%2022:2025~1,
      ccom=="76497"&annee==2025~1,
      ccom%in%c("76414","76545")&annee%in%2020:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="76400"&miseadispointerco==1&annee<2026~"as.character(c(76050,76330))",
      ccom=="76709"&miseadispointerco==1&annee<2026~"as.character(c(76750))",
      ccom=="76321"&miseadispointerco==1&annee<2026~"as.character(c(76697))",
      ccom=="76319"&miseadispointerco==1&annee<2026~"as.character(c(76497))",
      ccom=="76497"&miseadispointerco==1&annee<2026~"as.character(c(76319))",
      ccom=="76495"&miseadispointerco==1&annee<2026~"as.character(c(76234,76311,76385))",
      ccom=="76414"&miseadispointerco==1&annee<2026~"as.character(c(76545))",
      ccom=="76545"&miseadispointerco==1&annee<2026~"as.character(c(76414))",
      T~autrescommunes)
  )
  
  #Cas de La Couture-Boussey, Epieds, Garennes-sur-Eure, Ivry-la-Bataille,
  #Bueil, Bretagnolles, Serez
  x[which(str_detect(x$lcom,"COUTURE_BOUSSEY")&x$annee<2025),"ccom"]="27183"
  x[which(x$ccom=="27183"&x$annee<2025),"miseadispointerco"]=1
  x[which(x$ccom=="27119"&x$annee%in%2024:2025),"miseadispointerco"]=1
  x[which(x$ccom=="27355"&x$annee==2014),"miseadispointerco"]=1
  x[which(x$ccom=="27355"&x$annee==2014),"autrescommunes"]="as.character(c(27183,27220,27278))"
  x[which(x$ccom=="27183"&x$annee%in%2014:2022),"autrescommunes"]="as.character(c(27220,27278,27355))"
  x[which(x$ccom=="27183"&x$annee==2023),"autrescommunes"]="as.character(c(27220,27278))"
  x[which(x$ccom=="27183"&x$annee%in%2024:2025),"autrescommunes"]="as.character(c(27111,27119,27220,27278,27621))"
  x[which(x$ccom=="27119"&x$annee%in%2024:2025),"autrescommunes"]="as.character(c(27111,27183,27220,27278,27621))"
  
  #Correction de valeurs nulles pour Saint-Hilaire-du-Harcouët
  x[which(x$ccom=="50484"&x$annee==2025),"polmun"]=2
  x[which(x$ccom=="50484"&x$annee==2025),"asvp"]=1
  
  #Cas de de Saint-Pair-sur-Mer, Jullouville et Sartilly-Baie-Bocage
  x[which(x$ccom=="50565"&x$annee==2025),"miseadispointerco"]=1
  for(i in 1:3){
    x[which(x$ccom==c("50066","50532","50565")[i]&x$annee==2025),
      "autrescommunes"]=paste(
        "as.character(c(",
        paste(c("50066","50532","50565")[-i],collapse=","),
        "))",sep=""
      )
  }
  
  #Correction d'une valeur nulle pour Arques-la-Bataille
  x[which(x$ccom=="76026"&x$annee==2015),"gardechamp"]=1
  
  #Cas de Bois-Guillaume-Bihorel. On constate une répartition 2014 post-rétablissement
  #des deux communes fondatrices selon une clé 7-5. On attribue le 13e agent de 2013
  #non présent post-fusion de manière égalitaire
  x=x %>% 
    bind_rows(
      x %>% 
        filter(lcom=="BOIS_GUILLAUME_BIHOREL"&annee==2014) %>% 
        mutate(ccom="76108",
               lcom="BOIS_GUILLAUME",
               miseadispointerco=1,
               autrescommunes="as.character(c(76095))",
               polmun=7.5)
    ) %>% 
    mutate(
      polmun=if_else(
        lcom=="BOIS_GUILLAUME_BIHOREL"&annee==2014,
        5.5,polmun
        ),
      miseadispointerco=if_else(
        lcom=="BOIS_GUILLAUME_BIHOREL"&annee==2014,
        1,miseadispointerco
        ),
      autrescommunes=if_else(
        lcom=="BOIS_GUILLAUME_BIHOREL"&annee==2014,
        "as.character(c(76108))",autrescommunes
        ),
      ccom=if_else(
        lcom=="BOIS_GUILLAUME_BIHOREL"&annee==2014,
        "76095",ccom
        )
    )
  
  #Cas de Notre-Dame-de-Bondeville et du Houlme
  x=x %>% 
    bind_rows(
      x %>% 
        filter(lcom=="NOTRE_DAME_DE_BONDEVILLE_/_LE_HOULME"&annee==2024) %>% 
        mutate(ccom="76474",
               lcom="NOTRE_DAME_DE_BONDEVILLE",
               polmun=4)
    ) %>% 
    mutate(
      polmun=if_else(lcom=="NOTRE_DAME_DE_BONDEVILLE_/_LE_HOULME"&annee==2024,
                     1,polmun),
      ccom=if_else(lcom=="NOTRE_DAME_DE_BONDEVILLE_/_LE_HOULME"&annee==2024,
                   "76366",ccom)
    )
  
  x[which(x$ccom%in%c("76366","76474")&x$annee%in%2023:2025),"miseadispointerco"]=1
  x[which(x$ccom=="76366"&x$annee%in%2023:2025),"autrescommunes"]="as.character(c(76474))"
  x[which(x$ccom=="76474"&x$annee%in%2023:2025),"autrescommunes"]="as.character(c(76366))"
  
  #Cas du Syndicat Intercommunal des Vallées du Havre-Est (Gainneville, Saint-
  #Laurent-du-Brèvedent, Saint-Martin-du-Manoir, Saint-Vigor-d'Ymonville, et 
  #anciennement Rogerville)
  x[which(str_detect(x$lcom,"VALLEES_DU_HAVRE_EST|ST_VIGOR_D_YMONVILLE|ROGERVILLE")&x$annee<2026),"ccom"]="76296"
  x[which(x$ccom=="76296"&x$annee<2026),"miseadispointerco"]=1
  x[which(x$ccom=="76296"&x$annee<=2021),"autrescommunes"]="as.character(c(76533,76596,76616))"
  x[which(x$ccom=="76296"&x$annee==2022),"autrescommunes"]="as.character(c(76596,76616))"
  x[which(x$ccom=="76296"&x$annee%in%2023:2025),"autrescommunes"]="as.character(c(76596,76616,76657))"
  
  return(x)
  
}

polmunreg28=polmun %>% 
  filter(reg==28) %>% 
  select(-c("ldep","reg")) %>% 
  purgesreg28() %>% 
  mutate(lcom=standardiserlibelles(lcom,1),
         lcom=standardisationcomplementaire(lcom)) %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("lcom","dep","ccom")],
            by=join_by(lcom,dep)) %>% 
  select(dep,ccom,lcom,annee,everything()) %>% 
  redefinitionsreg28() %>% 
  select(-lcom) %>% 
  left_join(unique(bind_rows(
    communes %>% 
      filter(typique==1) %>% 
      select(lcom,ccom),
    epci %>% 
      select(LIBEPCI,EPCI) %>% 
      set_names(c("lcom","ccom")))),
    by=join_by(ccom)) %>% 
  redefinitions2(c("14431","14408","50082","50099","50129","50353","50487")) %>% 
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
