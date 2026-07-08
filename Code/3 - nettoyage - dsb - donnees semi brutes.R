library(tidyverse)
library(readxl)
library(readODS)

#RAPPEL SUR LES DATES :
#/!\ LES DONNEES, EN FAIT DE DECEMBRE, SONT ICI CONSIDEREES COMME AU PREMIER
#JANVIER, POUR CONCORDER AVEC LES DONNEES DEMOGRAPHIQUES. TOUTES LES DATES SONT
#DONC DECALEES : PAR EXEMPLE, LES DONNEES 2015 CORRESPONDENT AUX RESULTATS DE
#L'ENQUETE DE DECEMBRE 2014


#DIAGNOSTICS
depchoisi="01"

#Vérification de ce que les noms de communes sont bien des noms cohérents avec 
#le COG post-2010
polmun %>% 
  filter(dep==depchoisi) %>% 
  mutate(lcom=standardiserlibelles(lcom,0),
         lcom=standardisationcomplementaire(lcom)) %>% 
  #correctionsortho2024() %>% 
  #reportnotesdsb() %>% 
  select(lcom) %>% pull() %>% 
  setdiff(filter(communes,dep==depchoisi&date>=as.Date("2010-01-01"))$lcom)

#Vérification de ce que les observations non dotées d'un code au COG2026 sont 
#bel et bien des exceptions non fongibles dans le COG en l'état
polmun %>% 
  filter(dep==depchoisi) %>% 
  mutate(lcom=standardiserlibelles(lcom,0),
         lcom=standardisationcomplementaire(lcom)) %>% 
  correctionsortho2024() %>% 
  reportnotesdsb() %>% 
  correctionsortho2024() %>% 
  left_join(communes[,c("ccomsub","ccom","lcom","dep")],by=join_by(lcom,dep)) %>% 
  redefinitionsdsb() %>% 
  filter(is.na(ccom)) %>% 
  View()

#FONCTION DE PURIFICATION ET REPORT DES NOTES

#Suppression de précisions inutiles géographiques ou de nature de l'organisme
suppressionnotes=paste(
  "(_?\\(_?(ARR_(ROUEN|LE_HAVRE))_?\\))",
  "(_?\\(_?(SOUS_)?PREFECTURE.*_?\\))",
  "(_?\\(_?ANCIENNES?_COMMUNES?.*_?\\))",
  "(_?\\(_?COMMUNAUTE_DE_COMMUNES_?\\))",
  "(_?\\(_?EPCI_?\\))",
  "(_?\\(_?INTERCOMMUNALITE_?\\))",
  sep="|"
)

#Création d'un objet séparé pour alléger la rédaction de la fonction reportnotes
mutationnotes=paste(
  "(_\\(.*$)",
  "(_MUTUALISATION.*$)",
  "(_MUT\\._.*$)",
  "(_GARDE_CHAMPETRE.*$)",
  "(_?\\+_?[0-9]{1,2}_COMMUNES.*$)",
  "(_?(INFOS?_PAR_)?TEL_[0-9].*$)",
  sep="|"
)

reportnotesdsb=function(x){
  
  x=x %>% mutate(
    lcom=str_remove(lcom,suppressionnotes),
    notes=as.character(NA),
    notes=na_if(str_replace_all(str_remove_all(str_remove_all(
      case_when(
        #Dans le premier cas, on procède à une retranscription générale mais
        #qu'on n'applique pas 1° aux cas débutant par une parenthèse
        #("(MUTUALISATION)..." en Charente-Maritime) qui ne laisseraient qu'un
        #blanc à l'étape de rabotage du libellé, 2° aux cas de type
        #"POLICE_PLURICOMMUNALE_DE_..." car la contrainte d'un "_" initiale dans
        #le regex fait qu'on se retouverait juste avec "POLICE" comme libellé,
        #et ôter cette contrainte laisse un blanc
        is.na(notes)&!str_detect(
          lcom,"^(POLICE|PM_|\\()"
          )~str_extract(lcom,paste(
              mutationnotes,
              "(_?\\(.*\\)$)|(_\\(?(POLICE_|PM_)?(((PLURI|INTER)_?COMMUNALE)|LOCALE|MUTUALISEE_).*$)",
              sep="|")),
        #On procède ensuite à une version plus modeste de la retranscription des 
        #notes spécifiquement pour le cas jusque là négligé
      is.na(notes)&str_detect(lcom,mutationnotes)~str_extract(
        lcom,mutationnotes),
      T~notes),
      #Suite des str_remove_all et str_replace_all initiaux, qui reformatent 
      #simplement la variable en enlevant les parenthèses et en s'assurant que 
      #ce retrait ne crée par les "_" consécutifs, ou initiaux, ou finaux.
      "\\(|\\)"),"^_+|_+$"),"_+","_"),''),
    lcom=case_when(
      !str_detect(
        lcom,"^(POLICE|PM_|\\()"
        )~str_remove(lcom,paste(
          mutationnotes,
          "(_?\\(.*\\)$)|(_\\(?(POLICE_|PM_)?(((PLURI|INTER)_?COMMUNALE)|LOCALE|MUTUALISEE_).*$)",
          sep="|")),
      str_detect(lcom,mutationnotes)~str_remove(
        lcom,paste(mutationnotes,"(_:$)",sep="|")),
      T~lcom)
  )
  
  return(x)
  
}

#Dans le cadre du fichier des données semi-brutes, la fonction de redéfinitions
#est moins ambitieuse qu'ailleurs. Elle n'est pour l'essentiel qu'une façon
#commode de traiter l'ajout du COG aux intercommunalités et de rectifier
#manuellement quelques erreurs de report des notes, mais il est vrai qu'elle
#comporte aussi une poignée des corrections orthographiques plus douteuses
redefinitionsdsb=function(x){
  
  x[which(x$dep=="01"),]=mutate(
    x[which(x$dep=="01"),],
    ccom=case_when(
      str_detect(lcom,"BRESSE(_ET)?_SAONE")~"200071371",
      str_detect(lcom,"BELLEGARDIEN|TERRE_VALSERHONE")~"240100891",
      T~ccom
    )
  )
  
  x[which(x$dep=="02"),]=mutate(
    x[which(x$dep=="02"),],
    ccom=case_when(
      str_detect(lcom,"PAYS_D(E|U)_VERMANDOIS")~"240200493",
      str_detect(lcom,"DU_VAL_D(E_L)?_OISE")~"200040426",
      str_detect(lcom,"ST_QUENTINOIS")~"200071892",
      T~ccom
    )
  )
  
  x[which(x$dep=="06"),]=mutate(
    x[which(x$dep=="06"),],
    ccom=case_when(
      lcom=="ST_VINCENT_DE_PAUL"~"06128",
      T~ccom
    ),
    lcom=case_when(
      #On peut ne pas croire au bon sens de cette correction, mais elle me 
      #semble être la plus plausible
      lcom=="ST_VINCENT_DE_PAUL"~"ST_PAUL_DE_VENCE",
      T~lcom
    )
  )
  
  x[which(x$dep=="07"),]=mutate(
    x[which(x$dep=="07"),],
    ccom=case_when(
      str_detect(lcom,"GORGES_DE_L_ARDECHE")~"200039808",
      T~ccom
    )
  )
  
  x[which(x$dep=="08"),]=mutate(
    x[which(x$dep=="08"),],
    ccom=case_when(
      str_detect(lcom,"ARDENNE_METROPOLE")~"200041630",
      str_detect(lcom,"ARDENNES_THIERACHE")~"200041622",
      lcom=="SEDAN_51"~"08409",
      T~ccom
    ),
    notes=case_when(
      lcom=="SEDAN_51"~paste("51_(",notes,")",sep=""),
      T~notes
    ),
    lcom=case_when(
      lcom=="SEDAN_51"~"SEDAN",
      T~lcom
    )
  )
  
  x[which(x$dep=="11"),]=mutate(
    x[which(x$dep=="11"),],
    ccom=case_when(
      str_detect(lcom,"CARCASSONNE_AGGLO")~"200035715",
      T~ccom
    )
  )
  
  x[which(x$dep=="13"),]=mutate(
    x[which(x$dep=="13"),],
    ccom=case_when(
      str_detect(lcom,"BAUX_ALPILLES")~"241300375",
      T~ccom
    )
  )
  
  x[which(x$dep=="17"),]=mutate(
    x[which(x$dep=="17"),],
    ccom=case_when(
      str_detect(lcom,"C.D.C._GEMOZAC|GEMOZAC_ET_(DE_LA_)?SAINTONGE")~"241700632",
      str_detect(lcom,"_I?L+E_DE_RE")~"241700459",
      T~ccom
    )
  )
  
  x[which(x$dep=="2A"),]=mutate(
    x[which(x$dep=="2A"),],
    ccom=case_when(
      str_detect(lcom,"PAYS_AJACCIEN|CAPA")~"242010056",
      T~ccom
    )
  )
  
  x[which(x$dep=="2B"),]=mutate(
    x[which(x$dep=="2B"),],
    ccom=case_when(
      str_detect(lcom,"CALVI_BALAGNE")~"242020105",
      T~ccom
    )
  )
  
  x[which(x$dep=="21"),]=mutate(
    x[which(x$dep=="21"),],
    ccom=case_when(
      str_detect(lcom,"VAL_DE_SAONE")~"200070902",
      T~ccom
    )
  )
  
  #Le cas guingampais foisonne de libellés différents dans l'original. Voir les 
  #Notes d'Attribution pour comprendre le traitement si besoin
  x[which(x$dep=="22"),]=mutate(
    x[which(x$dep=="22"),],
    ccom=case_when(
      str_detect(lcom,"GUINGAMP_PAIMPOL_AGGLO")~"200067981",
      notes=="COMMUNAUTE"~as.character(NA),
      T~ccom
    ),
    lcom=case_when(
      notes=="COMMUNAUTE"~"GUINGAMP_COMMUNAUTE",
      T~lcom
    ),
    notes=case_when(
      notes=="COMMUNAUTE"~as.character(NA),
      T~notes
    ),
  )
  
  x[which(x$dep=="25"),]=mutate(
    x[which(x$dep=="25"),],
    ccom=case_when(
      str_detect(lcom,"PAYS_DE_MAICHE")~"200023075",
      str_detect(lcom,"PAYS_(DE_)?MONTBELIARD_AGGLO")~"200065647",
      T~ccom
    )
  )
  
  x[which(x$dep=="28"),]=mutate(
    x[which(x$dep=="28"),],
    ccom=case_when(
      lcom=="COURVILLE_SUR_EURE_GC._NON_ARME"~"28116",
      lcom=="DENONVILLE_5GARDE_CHAMPETRE_NON_ARME)"~"28129",
      T~ccom
    ),
    notes=case_when(
      lcom=="COURVILLE_SUR_EURE_GC._NON_ARME"~"GC._NON_ARME",
      lcom=="DENONVILLE_5GARDE_CHAMPETRE_NON_ARME)"~"GARDE_CHAMPETRE_NON_ARME",
      T~notes
    ),
    lcom=case_when(
      lcom=="COURVILLE_SUR_EURE_GC._NON_ARME"~"COURVILLE_SUR_EURE",
      lcom=="DENONVILLE_5GARDE_CHAMPETRE_NON_ARME)"~"DENONVILLE",
      T~lcom
    ),
  )
  
  x[which(x$dep=="30"),]=mutate(
    x[which(x$dep=="30"),],
    ccom=case_when(
      str_detect(lcom,"PAYS_D_UZES")~"200034379",
      str_detect(lcom,"PONT_DU_GARD")~"243000684",
      str_detect(lcom,"RHONY_VISTRE_VIDOURLE")~"243000569",
      str_detect(lcom,"PETITE_CAMARGUE")~"243000593",
      T~ccom
    )
  )
  
  x[which(x$dep=="31"),]=mutate(
    x[which(x$dep=="31"),],
    ccom=case_when(
      str_detect(lcom,"COTEAUX_(DE_)?BELLEVUE")~"243100815",
      str_detect(lcom,"FRONTONNAIS")~"200034957",
      T~ccom
    )
  )
  
  x[which(x$dep=="33"),]=mutate(
    x[which(x$dep=="33"),],
    ccom=case_when(
      str_detect(lcom,"MEDOC_ESTUAIRE")~"243301447",
      T~ccom
    ),
    notes=case_when(
      str_detect(ccom,"243301447")~str_remove(str_extract(lcom,":.*$"),"^:_"),
      T~notes
    ),
    lcom=case_when(
      str_detect(ccom,"243301447")~str_remove(lcom,"_:.*$"),
      T~lcom
    )
  )
  
  x[which(x$dep=="34"),]=mutate(
    x[which(x$dep=="34"),],
    ccom=case_when(
      str_detect(lcom,"SETE_AG+L?R?OPOLE")~"200066355",
      str_detect(lcom,"PAYS_DE_LUNEL|LUNEL_AGGLO")~"243400520",
      lcom=="EPCI_CCC"~"243400355",
      lcom=="EPCI_CCGPSL"~"200022986",
      T~ccom
    )
  )
  
  #Comme indiqué dans les Notes d'attribution relatives aux communes de plus de 
  #15 000 habitants, les "_/_CHATEAUROUX" ne trouvent pas vraiment de fondement.
  #On peut constater la bonne continuité des séries si on les ignore purement et 
  #simplement. VATAN_/_VALENCAY semble en revanche signaler une plus grande 
  #complexité, probablement une mutualisation.
  x[which(x$dep=="36"),]=mutate(
    x[which(x$dep=="36"),],
    ccom=case_when(
      lcom=="CHATILLON_SUR_INDRE_/_CHATEAUROUX"~"36045",
      lcom=="DEOLS_/_CHATEAUROUX"~"36063",
      lcom=="LE_POINCONNET_/_CHATEAUROUX"~"36159",
      T~ccom
    ),
    notes=case_when(
      str_detect(lcom,"_/_CHATEAUROUX")~"/_CHATEAUROUX",
      T~notes
    ),
    lcom=str_remove(lcom,"_/_CHATEAUROUX")
  )
  
  x[which(x$dep=="37"),]=mutate(
    x[which(x$dep=="37"),],
    ccom=case_when(
      str_detect(lcom,"SETE_AG+L?R?OPOLE")~"200066355",
      str_detect(lcom,"PAYS_DE_LUNEL|LUNEL_AGGLO")~"243400520",
      #Dans l'original post-standardisation orthographique, il est noté 
      #"CHINON_(EPCI)", conformément à un article de Saumur Kiosque datant le 
      #transfert du service à l'intercommunalité à octobre 2022
      #https://www.le-kiosque.org/chinon-vienne-et-loire-police-municipale-intercommunale-un-nouveau-service-de-proximite/
      lcom=="CHINON"&annee%in%2023:2025~"200043081",
      #CC du Grand Pic Saint Loup selon mon interprétation
      lcom=="EPCI_CCGPSL"~"200022986",
      T~ccom
    )
  )
  
  x[which(x$dep=="39"),]=mutate(
    x[which(x$dep=="39"),],
    ccom=case_when(
      str_detect(lcom,"D_EM+ERAUDE")~"200090579",
      T~ccom
    )
  )
  
  #L'indication "EPCI" après "LE_CONTROIS_EN_SOLOGNE" est visiblement erronée
  x[which(x$dep=="41"),]=mutate(
    x[which(x$dep=="41"),],
    ccom=case_when(
      lcom=="LE_CONTROIS_EN_SOLOGNE_EPCI"~"41059",
      str_detect(lcom,"COEUR_DE_SOLOGNE")~"200000800",
      T~ccom
    ),
    lcom=case_when(
      lcom=="LE_CONTROIS_EN_SOLOGNE_EPCI"~"LE_CONTROIS_EN_SOLOGNE",
      T~lcom
    )
  )
  
  x[which(x$dep=="44"),]=mutate(
    x[which(x$dep=="44"),],
    ccom=case_when(
      str_detect(lcom,"NANTES_METROPOLE")~"244400404",
      str_detect(lcom,"SUD_ESTUAIRE")~"244400586",
      T~ccom
    )
  )
  
  x[which(x$dep=="45"),]=mutate(
    x[which(x$dep=="45"),],
    ccom=case_when(
      str_detect(lcom,"ORLEANS_METROPOLE|AGGLOMERATION_ORLEANS")~"244500468",
      str_detect(lcom,"MONTARGOISE_ET_RIVE|_A_M_E")~"244500203",
      str_detect(lcom,"VAL_DE_SULLY")~"200070100",
      T~ccom
    )
  )
  
  #Le mutualisation par Segré au bénéfice d'une intercommunalité entière est en 
  #fait le partage de sa police avec ce qui seront dans peu de temps toutes les 
  #communes fondatrices de SEGRE_EN_ANJOU_BLEU
  x[which(x$dep=="49"),]=mutate(
    x[which(x$dep=="49"),],
    ccom=case_when(
      str_detect(lcom,"DU_CANTON_DE_SEGRE")&annee<2026~"49331",
      T~ccom
    )
  )
  
  #Tourlaville est une commune déléguée
  x[which(x$dep=="50"),]=mutate(
    x[which(x$dep=="50"),],
    ccom=case_when(
      lcom=="CHERBOURG_EN_COTENTIN_/_TOURLAVILLE"~"50129",
      T~ccom
    )
  )
  
  x[which(x$dep=="52"),]=mutate(
    x[which(x$dep=="52"),],
    ccom=case_when(
      str_detect(lcom,"GRAND_LANGRES")~"200072999",
      T~ccom
    )
  )
  
  x[which(x$dep=="54"),]=mutate(
    x[which(x$dep=="54"),],
    ccom=case_when(
      str_detect(lcom,"BASSIN_DE_POMPEY")~"245400601",
      str_detect(lcom,"GRAND_NANCY")~"245400676",
      lcom=="VILLERS"&annee==2014~"54578",
      T~ccom
    )
  )
  
  x[which(x$dep=="55"),]=mutate(
    x[which(x$dep=="55"),],
    ccom=case_when(
      str_detect(lcom,"VAL_DE_MEUSE_VOIE_SACREE")~"200066165",
      T~ccom
    )
  )
  
  x[which(x$dep=="56"),]=mutate(
    x[which(x$dep=="56"),],
    ccom=case_when(
      lcom=="CCBBO"|str_detect(lcom,"BELLEVUE_(BLAVET_)?OCEAN")~"245600440",
      T~ccom
    )
  )
  
  x[which(x$dep=="57"),]=mutate(
    x[which(x$dep=="57"),],
    ccom=case_when(
      str_detect(lcom,"ST_AVOLD_(ET_)?SYNERGIE")~"200067502",
      str_detect(lcom,"METZ_METROPOLE")~"200039865",
      T~ccom
    )
  )
  
  x[which(x$dep=="58"),]=mutate(
    x[which(x$dep=="58"),],
    ccom=case_when(
      lcom=="COMMUNAUTE_D_AGGLOMERATION_DE_NEVERS_EPCI"~"245804406",
      T~ccom
    )
  )
  
  #Saint-Pol-sur-Mer, Hellemmes et Lomme sont des communes associées/déléguées
  x[which(x$dep=="59"),]=mutate(
    x[which(x$dep=="59"),],
    ccom=case_when(
      lcom=="DUNKERQUE_ST_POL"~"59183",
      lcom%in%c("LILLE_HELLEMMES","LILLE_LOMME")~"59350",
      T~ccom
    )
  )
  
  x[which(x$dep=="62"),]=mutate(
    x[which(x$dep=="62"),],
    ccom=case_when(
      lcom=="DEUX_BAIES_EN_MONTREUILLOIS"~"200069029",
      T~ccom
    )
  )
  
  x=filter(x,!(annee==2016&dep=="62"&lcom%in%c("TINCHEBRAY_BOCAGE","VIMOUTIERS")))
  
  x[which(x$dep=="64"),]=mutate(
    x[which(x$dep=="64"),],
    ccom=case_when(
      str_detect(lcom,"PAU_BEARN_PYRENEES")~"200067254",
      T~ccom
    )
  )
  
  x[which(x$dep=="67"),]=mutate(
    x[which(x$dep=="67"),],
    ccom=case_when(
      str_detect(lcom,"BASSE_ZORN")~"246700843",
      T~ccom
    )
  )
  
  x[which(x$dep=="72"),]=mutate(
    x[which(x$dep=="72"),],
    ccom=case_when(
      str_detect(lcom,"(COMMUNES|MAINE)_SAOSNOIS")~"200072676",
      T~ccom
    )
  )
  
  x[which(x$dep=="73"),]=mutate(
    x[which(x$dep=="73"),],
    ccom=case_when(
      str_detect(lcom,"DE_YENNE")~"247300262",
      lcom=="LA_PERRIERE_1_ASVP_EN_HIVER"~"73227",
      T~ccom
    ),
    ccomsub=case_when(
      lcom=="LA_PERRIERE_1_ASVP_EN_HIVER"~"73198",
      T~ccomsub
    ),
    notes=case_when(
      lcom=="LA_PERRIERE_1_ASVP_EN_HIVER"~"1_ASVP_EN_HIVER",
      T~notes
    ),
    lcom=case_when(
      lcom=="LA_PERRIERE_1_ASVP_EN_HIVER"~"LA_PERRIERE",
      T~lcom
    )
  )
  
  x[which(x$dep=="74"),]=mutate(
    x[which(x$dep=="74"),],
    ccom=case_when(
      #Le cas "Bonneville / Faucigny-Glières" indique simplement la localisation
      #de l'unité, qui est intercommunale
      str_detect(lcom,"FAUCIGNY_GLIERES|COMMUNES_(DE_)?FAUCIGNY")~"200000172",
      T~ccom
    ),
    lcom=case_when(
      lcom=="POLICE_MUTUALISEE_EPAGY_METZ_TESSY"&!is.na(notes)~paste(
        lcom,"_[",notes,sep=""),
      T~lcom
    ),
    notes=case_when(
      notes=="STOCKAGE_DES_ARMES]_/_ARGONAY"~as.character(NA),
      T~notes
    )
  )
  
  x[which(x$dep=="76"),]=mutate(
    x[which(x$dep=="76"),],
    ccom=case_when(
      str_detect(lcom,"CAUX_SEINE_AGGLO|CAUX_VALLE")~"200010700",
      T~ccom
    )
  )
  
  x[which(x$dep=="77"),]=mutate(
    x[which(x$dep=="77"),],
    ccom=case_when(
      str_detect(lcom,"PAYS_DE_MEAUX")~"200072130",
      str_detect(lcom,"MELUN_VAL_DE_SEINE")~"247700057",
      str_detect(lcom,"OREE_DE_LA_BRIE")~"247700644",
      T~ccom
    ),
    lcom=case_when(
      #Voir Notes d'Attribution relatives à Saint-Fargeau-Ponthierry
      lcom=="COMMUNAUTE_DE_COMMUNES"&annee==2014~"COMMUNAUTE_DE_COMMUNES_SEINE_ECOLE",
      T~lcom
    )
  )
  
  x[which(x$dep=="78"),]=mutate(
    x[which(x$dep=="78"),],
    lcom=case_when(
      lcom=="SYNDICAT"&annee%in%2024:2025~paste(lcom,"_(",notes,")",sep=""),
      T~lcom
    ),
    notes=case_when(
      notes=="S.I.3.P.C."~as.character(NA),
      T~notes
    )
  )
  
  x[which(x$dep=="81"),]=mutate(
    x[which(x$dep=="81"),],
    ccom=case_when(
      str_detect(lcom,"SOR_ET_AGOUT")~"248100158",
      str_detect(lcom,"HAUT_LANGUEDOC")~"200066553",
      str_detect(lcom,"CASTRES_MAZAMET")~"248100430",
      T~ccom
    )
  )
  
  x[which(x$dep=="82"),]=mutate(
    x[which(x$dep=="82"),],
    ccom=case_when(
      str_detect(lcom,"(DEUX|2)_RIVES")~"248200016",
      str_detect(lcom,"GRAND_SUD")~"200066652",
      T~ccom
    )
  )
  
  x[which(x$dep=="83"),]=mutate(
    x[which(x$dep=="83"),],
    notes=case_when(
      lcom=="MONTFERRAT(VOIR_AMPUS)"~"VOIR_AMPUS",
      T~notes
    ),
    ccom=case_when(
      lcom=="MONTFERRAT(VOIR_AMPUS)"~"83082",
      T~ccom
    ),
    lcom=case_when(
      lcom=="MONTFERRAT(VOIR_AMPUS)"~"MONTFERRAT",
      T~lcom
    )
  )
  
  x[which(x$dep=="85"),]=mutate(
    x[which(x$dep=="85"),],
    ccom=case_when(
      str_detect(lcom,"VENDEE_SEVRE_AUTISE")~"248500563",
      str_detect(lcom,"TERRES_DE_MONTAIGU")~"200070233",
      T~ccom
    )
  )
  
  x[which(x$dep=="88"),]=mutate(
    x[which(x$dep=="88"),],
    ccom=case_when(
      str_detect(lcom,"BALLONS_DES_HAUTES_VOSGES")~"200033868",
      str_detect(lcom,"AGGLOMERATION_D_EPINAL")~"200068757",
      T~ccom
    )
  )
  
  x[which(x$dep=="90"),]=mutate(
    x[which(x$dep=="90"),],
    ccom=case_when(
      str_detect(lcom,"SUD_TERRITOIRE|CCST")~"249000241",
      str_detect(lcom,"GRAND_BELFORT")~"200069052",
      T~ccom
    )
  )
  
  x[which(x$dep=="91"),]=mutate(
    x[which(x$dep=="91"),],
    ccom=case_when(
      str_detect(lcom,"ENTRE_JUINE_ET_RENARDE")~"249100553",
      str_detect(lcom,"GRAND_PARIS_SUD")~"200059228",
      T~ccom
    ),
    lcom=case_when(
      lcom=="SYNDICAT_INTERCO_DE"&annee==2016~paste(lcom,notes,sep="_"),
      T~lcom
    ),
    notes=case_when(
      str_detect(lcom,"^SYNDICAT_INTERCO_DE")&annee==2016~as.character(NA),
      T~notes
    )
  )
  
  x[which(x$dep=="95"),]=mutate(
    x[which(x$dep=="95"),],
    ccom=case_when(
      str_detect(lcom,"VAL_?PARISIS")~"200058485",
      str_detect(lcom,"PLAINE_VALLEE")~"200056380",
      str_detect(lcom,"ROISSY_PAYS_DE_FRANCE")~"200055655",
      T~ccom
    )
  )
  
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
      str_detect(lcom,"COMMUNAUTE_DE_COMMUNES_DU_SUD|CCSUD")~"200060473",
      str_detect(lcom,"COM+UNES_DE_PETITE_TERRE|INTERCO_PT")~"200050532",
      T~ccom)
  )
  
  return(x)
  
}

#VOIR L'EXPORTATION DE FICHIER POUR LA CONSTITUTION DU JEU DSB FINAL

