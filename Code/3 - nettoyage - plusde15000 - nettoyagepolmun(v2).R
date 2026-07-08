library(tidyverse)
library(readxl)
library(readODS)

#RAPPEL SUR LES DATES :
#/!\ LES DONNEES, EN FAIT DE DECEMBRE, SONT ICI CONSIDEREES COMME AU PREMIER
#JANVIER, POUR CONCORDER AVEC LES DONNEES DEMOGRAPHIQUES. TOUTES LES DATES SONT
#DONC DECALEES : PAR EXEMPLE, LES DONNEES 2015 CORRESPONDENT AUX RESULTATS DE
#L'ENQUETE DE DECEMBRE 2014

#Après la correction orthographique, qui applique les corrections indubitables 
#aux libellés, la fonction de redéfinition incorpore les réajustements plus 
#sujets à débat (les Notes d'Attribution sont là pour la justifier)
redefinitions15000=function(x){
  
  #Voir la page introductive des Notes d'Attribution en ce qui concerne l'idée 
  #de Forme A de mutualisation
  x=mutate(x,autrescommunes=as.character(NA),formea=0)
  
  x[which(x$dep=="01"),]=mutate(
    x[which(x$dep=="01"),],
    ccom=case_when(
      str_detect(lcom,"BELLEGARD")&annee<2026~"01033",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="01004"&annee%in%2016:2025~1,
      ccom=="01033"&annee<2026~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="01004"&miseadispointerco==1&annee==2016~"as.character(c(01089))",
      ccom=="01004"&miseadispointerco==1&annee%in%2017:2025~"as.character(c(01089,01149))",
      ccom=="01033"&miseadispointerco==1&annee%in%2014:2020~"as.character(c(01044,01189,01448))",
      ccom=="01033"&miseadispointerco==1&annee%in%2021:2025~paste(
        "as.character(c(",
        paste(epci %>% 
                filter(EPCI==240100891&CODGEO!="01033") %>% 
                select(CODGEO) %>% 
                pull(),
              collapse=","),
        "))",
        sep=""),
      T~autrescommunes),
    formea=case_when(
      ccom=="01033"&miseadispointerco==1&annee%in%2021:2025~1,
      T~formea)
    )
  
  x[which(x$dep=="16"),]=mutate(
    x[which(x$dep=="16"),],
    autrescommunes=case_when(
      ccom=="16089"&miseadispointerco==1&annee<2026~"as.character(c(16102))",
      ccom=="16102"&miseadispointerco==1&annee<2026~"as.character(c(16089))",
      T~autrescommunes)
    )
  
  x[which(x$dep=="27"),]=mutate(
    x[which(x$dep=="27"),],
    miseadispointerco=case_when(
      ccom=="27562"&annee%in%2023:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="27375"&miseadispointerco==1&annee<2026~"as.character(c(27351))",
      ccom=="27562"&miseadispointerco==1&annee<2026~"as.character(c(27681))",
      ccom=="27681"&miseadispointerco==1&annee<2026~"as.character(c(27562))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="34"),]=mutate(
    x[which(x$dep=="34"),],
    miseadispointerco=case_when(
      ccom=="34032"&annee%in%2019:2025~1,
      ccom=="34073"&annee%in%2019:2025~1,
      ccom%in%c("34037","34084")&annee%in%2022:2025~1,
      ccom=="34166"&annee%in%2023:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="34032"&miseadispointerco==1&annee%in%2019:2021~"as.character(c(34073))",
      ccom=="34032"&miseadispointerco==1&annee==2022~"as.character(c(34037,34073,34084))",
      ccom=="34032"&miseadispointerco==1&annee%in%2023:2025~"as.character(c(34037,34073,34084,34166))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="36"),]=mutate(
    x[which(x$dep=="36"),],
    lcom=str_remove(lcom,"_/_CHATEAUROUX$"),
    autrescommunes=case_when(
      ccom=="36044"&miseadispointerco==1&annee<2026~"as.character(c(36202))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="38"),]=mutate(
    x[which(x$dep=="38"),],
    miseadispointerco=case_when(
      ccom=="38193"&annee%in%2019:2025~1,
      ccom=="38382"&annee%in%2019:2025~1,
      ccom=="38553"&annee%in%2019:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="38193"&miseadispointerco==1&annee%in%2019:2022~"as.character(c(38530))",
      ccom=="38193"&miseadispointerco==1&annee%in%2023:2025~"as.character(c(38352,38530))",
      ccom=="38382"&miseadispointerco==1&annee<2026~"as.character(c(38170))",
      ccom=="38553"&miseadispointerco==1&annee<2026~"as.character(c(38339))",
      T~autrescommunes),
    formea=case_when(
      ccom=="38382"&miseadispointerco==1&annee%in%2019:2025~1,
      T~formea)
  )
  
  x[which(x$dep=="42"),]=mutate(
    x[which(x$dep=="42"),],
    miseadispointerco=case_when(
      ccom=="42147"&annee%in%2020:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="42147"&miseadispointerco==1&annee%in%2020:2022~"as.character(c(42046))",
      ccom=="42147"&miseadispointerco==1&annee%in%2023:2025~"as.character(c(42046,42087,42122,42180,42228,42290,42299))",
      T~autrescommunes),
    formea=case_when(
      ccom=="42147"&miseadispointerco==1&annee%in%2020:2025~1,
      T~formea)
  )
  
  x[which(x$dep=="49"),]=mutate(x[which(x$dep=="49"),],ccom=case_when(
    str_detect(lcom,"SEGRE")&annee<2026~"49331",
    T~ccom))
  
  x[which(x$dep=="50"),]=mutate(
    x[which(x$dep=="50"),],
    ccom=case_when(
      lcom=="CHERBOURG_EN_COTENTIN_/_TOURLAVILLE"&annee<2026~"50129",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="50129"&annee==2025~0,
      T~miseadispointerco)
    )
  
  x[which(x$dep=="51"),]=mutate(
    x[which(x$dep=="51"),],
    miseadispointerco=case_when(
      ccom=="51108"&annee%in%2024:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="51108"&miseadispointerco==1&annee<2026~"as.character(c(51160,51168,51242,51453,51483,51504,51506,51525))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="56"),]=mutate(
    x[which(x$dep=="56"),],
    miseadispointerco=case_when(
      ccom=="56107"&annee==2022~0,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="56107"&miseadispointerco==1&annee<2026~"as.character(c(56121))",
      ccom=="56121"&miseadispointerco==1&annee<2026~"as.character(c(56107))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="58"),]=mutate(
    x[which(x$dep=="58"),],
    ccom=case_when(
      lcom=="COMMUNAUTE_D_AGGLOMERATION_DE_NEVERS_EPCI"&annee<2026~"58194",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="58194"&annee%in%2024:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="58194"&miseadispointerco==1&annee<2026~"as.character(c(58051,58088,58117,58124,58126,58278))",
      T~autrescommunes),
    formea=case_when(
      ccom=="58194"&miseadispointerco==1&annee%in%2024:2025~1,
      T~formea)
  )
  
  x[which(x$dep=="59"),]=mutate(
    x[which(x$dep=="59"),],
    ccom=case_when(
      lcom=="DUNKERQUE_ST_POL"&annee<2026~"59183",
      lcom=="ARMENTIERES_LA_CHAPELLE_D_ARMENTIERES"&annee<2026~"59017",
      lcom%in%c("LILLE_HELLEMMES","LILLE_LOMME","LOMME")~"59350",
      str_detect(lcom,"^HEM")&annee<2026~"59299",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="59017"&annee%in%2023:2025~1,
      ccom=="59299"&annee<2026~1,
      ccom=="59507"&annee==2025~0,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="59017"&miseadispointerco==1&annee<2026~"as.character(c(59143))",
      ccom=="59299"&miseadispointerco==1&annee==2014~"as.character(c(59332,59598))",
      ccom=="59299"&miseadispointerco==1&annee%in%2015:2017~"as.character(c(59247,59332,59598))",
      ccom=="59299"&miseadispointerco==1&annee%in%2018:2025~"as.character(c(59247,59332,59339,59598))",
      #Mutualisation à venir entre Faches-Thumesnil et Ronchin
      #ccom=="59220"&miseadispointerco==1~"as.character(c(59507))",
      #ccom=="59507"&miseadispointerco==1~"as.character(c(59220))",
      T~autrescommunes),
    formea=case_when(
      ccom=="59017"&miseadispointerco==1&annee<2026~1,
      ccom=="59299"&miseadispointerco==1&annee<2026~1,
      T~formea)
    )
  
  x[which(x$dep=="60"),]=mutate(
    x[which(x$dep=="60"),],
    miseadispointerco=case_when(
      #L'existence d'une mutualisation en 2019 semble certaine entre Compiègne 
      #et Margny-lès-Compiègne, mais on ignore quand et si elle cesse avant de 
      #reprendre ou continuer sous une nouvelle forme en 2025
      #ccom%in%c("60159","60382")&annee==2020~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      #Cas de Compiègne et Margny-lès-Compiègne, si l'on parvient d'abord à 
      #compléter déclarations de mutualisation
      #ccom=="60159"&miseadispointerco==1&annee<2026~"as.character(c(60382))",
      #ccom=="60382"&miseadispointerco==1&annee<2026~"as.character(c(60159))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="69"),]=mutate(
    x[which(x$dep=="69"),],
    miseadispointerco=case_when(
      ccom=="69202"&annee%in%2022:2025~1,
      #On ignore quand s'achève, si elle s'achève, la mutualisation entamée en 
      #2018 entre Tassin-la-Demi-Lune et Charbonnières-les-Bains
      #ccom%in%c("69044","69244")&annee==2019~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="69142"&miseadispointerco==1&annee<2026~"as.character(c(69202))",
      ccom=="69202"&miseadispointerco==1&annee<2026~"as.character(c(69142))",
      ccom=="69275"&miseadispointerco==1&annee<2026~"as.character(c(69282))",
      ccom=="69282"&miseadispointerco==1&annee<2026~"as.character(c(69275))",
      #Cas de Tassin-la-Demi-Lune et Charbonnières-les-Bains, si l'on parvient 
      #d'abord à compléter déclarations de mutualisation
      #ccom=="69044"&miseadispointerco==1&annee<2026~"as.character(c(69244))",
      #ccom=="69244"&miseadispointerco==1&annee<2026~"as.character(c(69044))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="70"),]=mutate(
    x[which(x$dep=="70"),],
    miseadispointerco=case_when(
      ccom=="70550"&annee==2025~0,
      T~miseadispointerco),
    autrescommunes=case_when(
      #Fondation d'une police pluricommunale en 2025
      #ccom=="70550"&miseadispointerco==1~"as.character(c(70134,70179,70363,70388))",
      T~autrescommunes),
    formea=case_when(
      #Fondation d'une police pluricommunale en 2025
      #ccom=="70550"&miseadispointerco==1~1,
      T~formea)
  )
  
  x[which(x$dep=="74"),]=mutate(
    x[which(x$dep=="74"),],
    ccom=case_when(
      str_detect(lcom,"ST_JULIEN|_DU_SALEVE")&annee<2026~"74243",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="74243"&annee%in%2018:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="74243"&miseadispointerco==1&annee<2026~"as.character(c(74016,74031,74124,74201,74216))",
      T~autrescommunes),
    formea=case_when(
      ccom=="74243"&miseadispointerco==1&annee%in%2018:2025~1,
      T~formea)
    )
  
  x[which(x$dep=="77"),]=mutate(
    x[which(x$dep=="77"),],
    ccom=case_when(
      (lcom=="COMMUNAUTE_DE_COMMUNES"&annee==2014)|(
        str_detect(lcom,"SEINE_ECOLE")&annee%in%2015:2016)~"77407",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="77058"&annee==2022~1,
      ccom=="77186"&annee==2025~1,
      ccom=="77407"&annee<2017~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="77058"&miseadispointerco==1&annee<2026~"as.character(c(77059))",
      #Expansions à venir de la mutualisation de Bussy-Saint-Georges
      #ccom=="77058"&miseadispointerco==1&annee==2026~"as.character(c(77059,77121,77181,77221,77237,77438))",
      ccom=="77108"&miseadispointerco==1&annee<2026~"as.character(c(77363))",
      ccom=="77186"&miseadispointerco==1&annee<2026~"as.character(c(77014))",
      ccom=="77407"&miseadispointerco==1&annee<2026~"as.character(c(77378))",
      ccom=="77374"&miseadispointerco==1&annee<2026~"as.character(c(77390))",
      ccom=="77390"&miseadispointerco==1&annee<2026~"as.character(c(77374))",
      #Cas de Montévrain et de Chanteloup-en-Brie, si l'on parvient à compléter 
      #ou corriger au besoin les indications de mutualisation
      #ccom=="77085"&miseadispointerco==1&annee<2026~"as.character(c(77307))",
      #ccom=="77307"&miseadispointerco==1&annee<2026~"as.character(c(77085))",
      T~autrescommunes),
    formea=case_when(
      ccom=="77407"&miseadispointerco==1&annee<2017~1,
      T~formea)
    )
  
  x[which(x$dep=="78"),]=mutate(
    x[which(x$dep=="78"),],
    ccom=case_when(
      lcom=="ST_GERMAIN_EN_LAYE/MAREIL_MARLY"~"78551",
      str_detect(lcom,"SYNDICAT")&annee<2026~"78490",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="78551"&annee==2019~1,
      ccom=="78490"&annee%in%2024:2025~1,
      ccom%in%c("78642","78643")&annee%in%2022:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="78551"&miseadispointerco==1&annee<2026~"as.character(c(78367))",
      ccom=="78490"&miseadispointerco==1&annee<2026~"as.character(c(78165))",
      ccom=="78642"&miseadispointerco==1&annee<2026~"as.character(c(78643))",
      ccom=="78643"&miseadispointerco==1&annee<2026~"as.character(c(78642))",
      T~autrescommunes),
    formea=case_when(
      ccom=="78490"&miseadispointerco==1&annee<2026~1,
      T~formea)
    )
  
  x[which(x$dep=="81"),]=mutate(
    x[which(x$dep=="81"),],
    miseadispointerco=case_when(
      ccom=="81099"&annee<2026~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="81099"&miseadispointerco==1&annee<2026~"as.character(c(81038))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="84"),]=mutate(
    x[which(x$dep=="84"),],
    miseadispointerco=case_when(
      ccom=="84081"&annee%in%2022:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="84081"&miseadispointerco==1&annee<2026~"as.character(c(84092))",
      ccom=="84092"&miseadispointerco==1&annee<2026~"as.character(c(84081))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="85"),]=mutate(
    x[which(x$dep=="85"),],
    ccom=case_when(
      lcom=="TERRES_DE_MONTAIGU"~"85146",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="85146"&annee%in%2018:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="85146"&miseadispointerco==1&annee<2026~paste(
        "as.character(c(",
        paste(epci %>% 
                filter(EPCI==200070233&CODGEO!="85146") %>% 
                select(CODGEO) %>% 
                pull(),
              collapse=","),
        "))",
        sep=""),
      T~autrescommunes),
    formea=case_when(
      ccom=="85146"&miseadispointerco==1&annee%in%2018:2025~1,
      T~formea)
    )
  
  x[which(x$dep=="90"),]=mutate(
    x[which(x$dep=="90"),],
    miseadispointerco=case_when(
      ccom%in%c("90008","90010")&annee%in%2022:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="90010"&miseadispointerco==1&annee<2026~"as.character(c(90008,90039,90075))",
      ccom=="90008"&miseadispointerco==1&annee<2026~"as.character(c(90010))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="91"),]=mutate(
    x[which(x$dep=="91"),],
    autrescommunes=case_when(
      ccom=="91179"&miseadispointerco==1&annee<2026~"as.character(c(91386))",
      ccom=="91386"&miseadispointerco==1&annee%in%2023:2024~"as.character(c(91179))",
      ccom=="91386"&miseadispointerco==1&annee==2025~"as.character(c(91159,91179))",
      T~autrescommunes)
  )
  
  x[which(x$dep=="94"),]=mutate(
    x[which(x$dep=="94"),],
    ccom=case_when(
      str_detect(lcom,"ABLON_SUR_SEINE|VILLENEUVE_LE_ROI")&annee<2026~"94077",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="94077"&annee%in%2016:2025~1,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="94077"&miseadispointerco==1&annee<2026~"as.character(c(94001))",
      T~autrescommunes),
    formea=case_when(
      ccom=="94077"&miseadispointerco==1&annee<2026~1,
      T~formea)
    )
  
  x[which(x$dep=="95"),]=mutate(
    x[which(x$dep=="95"),],
    ccom=case_when(
      lcom=="DOMONT_/_PISCOP"~"95199",
      T~ccom),
    miseadispointerco=case_when(
      ccom=="95199"&annee%in%2022:2025~1,
      ccom%in%c("95500","95572")&annee==2024~0,
      T~miseadispointerco),
    autrescommunes=case_when(
      ccom=="95199"&miseadispointerco==1&annee<2026~"as.character(c(95489))",
      ccom=="95500"&miseadispointerco==1&annee<2026~"as.character(c(95572))",
      ccom=="95572"&miseadispointerco==1&annee<2026~"as.character(c(95500))",
      T~autrescommunes)
    )
  
  #Cas de Bourg-en-Bresse, Péronnas, Saint-Denis-lès-Bourg, et Viriat
  for(i in 1:4){
  
    x[which(x$annee==2022&x$ccom==c("01053","01289","01344","01451")[i]),
      "miseadispointerco"]=0
    x[which(x$annee%in%2023:2025&x$ccom==c("01053","01289","01344","01451")[i]),
      "miseadispointerco"]=1
    
    x[which(x$annee<2026&x$miseadispointerco==1&x$ccom==c("01053","01289","01344","01451")[i]),
      "autrescommunes"]=paste(
        "as.character(c(",
        paste(c("01053","01289","01344","01451")[-i],collapse=","),
        "))",sep="")
    
  }
  
  #Cas de Chartres, Champhol, Le Coudray, Lèves, Lucé, Luisant et Mainvilliers
  for(i in 1:7){
    
    x[which(x$annee%in%2024:2025&x$ccom==c(
      "28070","28085","28110","28209","28218","28220","28229"
      )[i]),"miseadispointerco"]=1
    
    x[which(x$annee%in%2024:2025&x$miseadispointerco==1&x$ccom==c(
      "28070","28085","28110","28209","28218","28220","28229"
      )[i]),"autrescommunes"]=paste(
        "as.character(c(",paste(c(
          "28070","28085","28110","28209","28218","28220","28229"
          )[-i],collapse=","),
        "))",sep="")
    
  }
  
  #Cas de Thionville, Terville, Manom, Angevillers, Fontoy et Tressange
  
  x=bind_rows(
    filter(x,!((
      annee%in%2023:2025&ccom%in%c("57022","57226"))|(
        annee<2026&lcom=="THIONVILLE_TERVILLE"
      ))) %>% 
      mutate(
        miseadispointerco=if_else(
          ccom=="57441"&annee%in%2021:2025,1,miseadispointerco
          ),
        autrescommunes=if_else(
          ccom=="57441"&annee%in%2021:2025,
          "as.character(c(57666,57672))",
          autrescommunes
        )),
    data.frame(
      dep=rep("57",4),
      ccom=c(rep("57666",2),rep("57672",2)),
      lcom=c(rep("TERVILLE",2),rep("THIONVILLE",2)),
      annee=rep(2020:2021,2),
      miseadispointerco=rep(1,4),
      polmun=rep(c(6,26),2),
      asvp=rep(c(1,2),2),
      gardechamp=rep(0,4),
      brigcanine=as.numeric(rep(NA,4)),
      maitrechien=rep(0,4),
      chien=rep(0,4),
      autrescommunes=c(
        "as.character(c(57672))",
        "as.character(c(57441,57672))",
        "as.character(c(57666))",
        "as.character(c(57441,57666))"
      ),
      formea=rep(0,4)
    )
  )
  
  x[which(x$annee%in%2019:2020&x$ccom%in%c("57666","57672")),
    "miseadispointerco"]=1
  x[which(x$annee%in%2021:2025&x$ccom%in%c("57441","57666","57672")),
    "miseadispointerco"]=1
  x[which(x$annee==2019&x$miseadispointerco==1&x$ccom=="57666"),
    "autrescommunes"]="as.character(c(57672))"
  x[which(x$annee==2019&x$miseadispointerco==1&x$ccom=="57672"),
    "autrescommunes"]="as.character(c(57666))"
  
  for(i in 1:6){
    
    if(i<=3){
      x[which(x$annee==2022&x$miseadispointerco==1&x$ccom==c(
        "57441","57666","57672"
      )[i]),"autrescommunes"]=paste(
        "as.character(c(",paste(c(
          "57441","57666","57672"
        )[-i],collapse=","),
        "))",sep="")
    }
    
    x[which(x$annee%in%2023:2025&x$miseadispointerco==1&x$ccom==c(
      "57022","57226","57441","57666","57672","57678"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "57022","57226","57441","57666","57672","57678"
      )[-i],collapse=","),
      "))",sep="")
    
  }
  
  #Cas de La Madeleine, Marquette-lez-Lille, Saint-André-lez-Lille, et Wambrechies
  x[which(x$annee==2022&x$ccom=="59368"),"miseadispointerco"]=1
  
  for(i in 1:4){
    
    if(i<=3){
      x[which(x$annee==2022&x$miseadispointerco==1&x$ccom==c(
        "59368","59386","59527"
      )[i]),"autrescommunes"]=paste(
        "as.character(c(",paste(c(
          "59368","59386","59527"
        )[-i],collapse=","),
        "))",sep="")
    }
    
    x[which(x$annee%in%2023:2025&x$miseadispointerco==1&x$ccom==c(
      "59368","59386","59527","59636"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "59368","59386","59527","59636"
      )[-i],collapse=","),
      "))",sep="")
    
  }

  #Cas d'Avon, Héricy, Samois-sur-Seine, Samoreau et Vulaines-sur-Seine
  for(i in 1:5){
    
    x[which(x$annee%in%2020:2025&x$ccom==c(
      "77014","77226","77441","77442","77533"
    )[i]),"miseadispointerco"]=1
    
    if(i==1){
      x[which(x$annee%in%2020:2022&x$miseadispointerco==1&x$ccom=="77014"),
        "autrescommunes"]="as.character(c(77226,77441,77442,77533))"
      x[which(x$annee%in%2023:2026&x$miseadispointerco==1&x$ccom=="77014"),
        "autrescommunes"]="as.character(c(77186,77226,77441,77442,77533))"
    }
    
    if(i!=1){
    x[which(x$annee<2026&x$miseadispointerco==1&x$ccom==c(
      "77014","77226","77441","77442","77533"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "77014","77226","77441","77442","77533"
      )[-i],collapse=","),
      "))",sep="")
    }
    
  }
  
  #Cas de Brie-Comte-Robert, Chevry-Cossigny, Servon, et Varennes-Jarcy
  #Traitement partiel par incidence de Boussy-Saint-Antoine et Quincy-sous-Sénart
  x[which(x$annee%in%2016:2017&x$ccom=="91631"),"miseadispointerco"]=1
  x[which(x$annee%in%2016:2017&x$ccom=="91631"),
    "autrescommunes"]="as.character(c(91097,91514))"
  
  for(i in 1:4){
    
    x[which(x$annee%in%2017:2025&x$ccom==c(
      "77053","77114","77450","91631"
    )[i]),"miseadispointerco"]=1
    
    if(i<=3){
    x[which(x$annee==2017&x$miseadispointerco==1&x$ccom==c(
      "77053","77114","77450"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "77053","77114","77450"
      )[-i],collapse=","),
      "))",sep="")
    }
    
    x[which(x$annee%in%2018:2025&x$miseadispointerco==1&x$ccom==c(
      "77053","77114","77450","91631"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "77053","77114","77450","91631"
      )[-i],collapse=","),
      "))",sep="")
    
  }
  
  #Cas de Lagny-sur-Marne, Pomponne, Dampmart, Thorigny-sur-Marne, Carnetin et 
  #Conches-sur-Gondoire
  for(i in 1:6){
    
    
    
    if(i<=2){
    x[which(x$annee==2024&x$ccom==c(
      "77243","77372"
    )[i]),"miseadispointerco"]=1
    }
    
    if(i<=4){
    x[which(x$annee==2025&x$ccom==c(
      "77155","77243","77372","77464"
    )[i]),"miseadispointerco"]=1
    }
    
    #x[which(x$annee==2026&x$ccom==c(
    #  "77062","77124","77155","77243","77372","77464"
    #)[i]),"miseadispointerco"]=1
    
    if(i<=2){
    x[which(x$annee==2024&x$miseadispointerco==1&x$ccom==c(
      "77243","77372"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "77243","77372"
      )[-i],collapse=","),
      "))",sep="")
    }
    
    if(i<=4){
    x[which(x$annee==2025&x$miseadispointerco==1&x$ccom==c(
      "77155","77243","77372","77464"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "77155","77243","77372","77464"
      )[-i],collapse=","),
      "))",sep="")
    }
    
    #x[which(x$annee==2026&x$miseadispointerco==1&x$ccom==c(
    #  "77062","77124","77155","77243","77372","77464"
    #)[i]),"autrescommunes"]=paste(
    #  "as.character(c(",paste(c(
    #    "77062","77124","77155","77243","77372","77464"
    #  )[-i],collapse=","),
    #  "))",sep="")
    
    if(i==6){
      x[which(x$ccom=="77243"&x$annee==2023),"miseadispointerco"]=0
      x[which(x$ccom=="77243"&x$miseadispointerco==1),"formea"]=1
    }
    
  }
  
  #Cas de la mutualisation de Maurepas et Coignières en 2018
  x=bind_rows(
    filter(x,lcom!="MAUREPAS/COIGNIERES"&annee<2026),
    data.frame(
      dep=rep("78",2),
      ccom=c("78383","78168"),
      lcom=c("MAUREPAS","COIGNIERES"),
      annee=rep(2019,2),
      miseadispointerco=rep(1,2),
      polmun=c(5,2),
      asvp=c(3,1),
      gardechamp=rep(0,2),
      brigcanine=as.numeric(rep(NA,2)),
      maitrechien=rep(0,2),
      chien=rep(0,2),
      autrescommunes=c("as.character(c(78168))",
                       "as.character(c(78383))"),
      formea=rep(0,2)
      )
    )
  
  #Cas de la mutualisation de Bougival et La Celle-Saint-Cloud en 2019-2020
  x=bind_rows(
    filter(x,!str_detect(lcom,"BOUGIVAL_?/")&annee<2026),
    data.frame(
      dep=rep("78",4),
      ccom=c(rep("78092",2),rep("78126",2)),
      lcom=c(rep("BOUGIVAL",2),rep("LA_CELLE_ST_CLOUD",2)),
      annee=rep(2020:2021,2),
      miseadispointerco=rep(1,4),
      polmun=c(7,6,1,1),
      asvp=rep(1,4),
      gardechamp=rep(0,4),
      brigcanine=as.numeric(rep(NA,4)),
      maitrechien=rep(0,4),
      chien=rep(0,4),
      autrescommunes=c(rep("as.character(c(78126))",2),
                       rep("as.character(c(78092))",2)),
      formea=rep(0,4)
    )
  )
  
  #Cas de la mutualisation de Vélizy-Villacoublay et Viroflay en 2019-2020
  x=bind_rows(
    filter(x,!str_detect(lcom,"VELIZY_VILLACOUBLAY_?/_?VIROFLAY")&annee<2026),
    data.frame(
      dep=rep("78",4),
      ccom=c(rep("78640",2),rep("78686",2)),
      lcom=c(rep("VELIZY_VILLACOUBLAY",2),rep("VIROFLAY",2)),
      annee=rep(2020:2021,2),
      miseadispointerco=rep(1,4),
      polmun=c(19,20,9,10),
      asvp=c(5.5,7,1.5,3),
      gardechamp=c(1,0,1,0),
      brigcanine=as.numeric(rep(NA,4)),
      maitrechien=rep(0,4),
      chien=rep(0,4),
      autrescommunes=c(rep("as.character(c(78686))",2),
                       rep("as.character(c(78640))",2)),
      formea=rep(0,4)
    )
  )
  
  #Cas de Juvisy-sur-Orge, Savigny-sur-Orge et Viry-Châtillon
  for(i in 1:3){
    
    if(i<=2){
    x[which(x$annee==2022&x$miseadispointerco==1&x$ccom==c(
      "91326","91687"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "91326","91687"
      )[-i],collapse=","),
      "))",sep="")
    }
    
    x[which(x$annee%in%2023:2025&x$miseadispointerco==1&x$ccom==c(
      "91326","91589","91687"
    )[i]),"autrescommunes"]=paste(
      "as.character(c(",paste(c(
        "91326","91589","91687"
      )[-i],collapse=","),
      "))",sep="")
    
  }
  
  #Corrections de valeur pour Saint-Amand-les-Eaux et Mitry-Mory
  x[which(x$ccom=="59526"&x$annee==2017),"polmun"]=0
  x[which(x$ccom=="77294"&x$annee==2025),"polmun"]=0
  #Limay est probablement aussi une erreur de ce type
  #x[which(x$ccom=="78335"&x$annee==2023),"polmun"]=0
  
  return(x)
  
}

polmun15000=polmun %>% 
  mutate(lcom=standardiserlibelles(str_remove_all(lcom,"_?\\(.*\\)"),1),
         lcom=standardisationcomplementaire(lcom)) %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("lcom","ccom","dep")],
            by=join_by(lcom,dep)) %>% 
  redefinitions15000() %>% 
  select(-c("lcom","reg","ldep")) %>% 
  left_join(communes[which(communes$typique==1),c("lcom","ccom","dep")],
            by=join_by(ccom,dep)) %>% 
  #Elimine un doublon avec erreur de données, et les communes de moins de 15000
  filter(!(lcom=="MOUGINS"&annee=="2022"&polmun==0)&ccom%in%communesplus15000$ccom) %>% 
  #L'ordre des colonnes importe pour le bon fonctionnement de redefinitions2
  select(dep,ccom,lcom,annee,miseadispointerco,
         polmun,asvp,gardechamp,brigcanine,maitrechien,chien,
         autrescommunes,formea) %>% 
  redefinitions2(
    c("01033","49023","49092","50129","53062","59183","59350","69149",
      "74010","74243","78158","78551","85194","91228","93066","94077")
    ) %>% 
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



#DETECTION DE VALEURS MANQUANTES
#INUTILE PRE-2024 CAR LE TRAITEMENT DES DONNEES SEMI-BRUTES GARANTIT L'EXHAUSTI-
#VITE DU JEU +15000. PAR CONTRE, SAUF A AVOIR ACTUALISE LES DONNEES SEMI-BRUTES 
#AVANT CELA, SERA NECESSAIRE A COMPTER DE 2025
#Pertinent seulement pour les communes de plus de 15 000 habitants (beaucoup de
#communes plus petites ne sont pas dotées d'une police municipale, et l'absence
#de donnée est donc une piste de détection beaucoup moins fiable d'une erreur de
#frappe dans les jeux de données du Ministère)
anneechoisie=2026

polmunpdn %>% 
  filter(annee==anneechoisie) %>% 
  select(lcom) %>% 
  unique() %>% 
  pull() %>% 
  setdiff(communesplus15000$lcom,.)

polmunpdn %>% 
  filter(annee==anneechoisie) %>% 
  select(ccom) %>% 
  unique() %>% 
  pull() %>% 
  setdiff(communesplus15000$ccom,.) %>% 
  sort() %>% 
  dput()



#DETECTION DE DOUBLONS
#Les doublons peuvent être causés ou bien par des doublons dans les fichiers
#originaux, ou bien par l'oubli de réunifier certaines données via la fonction
#redefinitions2 après avoir pratiqué une fusion via redefinitions1, ou par un
#véritable doublon dans les originaux (cas de MOUGINS). Il faut alors veiller à
#ajouter le code commune nécessaire en argument de redefinitions2 lors de la
#création de polmun15000
polmunpdn %>% 
  group_by(ccom,annee) %>% 
  mutate(occurences=1,
         occurences=sum(occurences)) %>% 
  filter(row_number()==1&occurences>1) %>% 
  ungroup() %>% 
  select(ccom,lcom,annee,occurences) %>% 
  #nrow()
  View()

