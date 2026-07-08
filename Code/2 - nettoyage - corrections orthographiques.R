# Corrections orthographiques complètes pour 2013-2024.
# JE RAPPELLE QU'IL Y A UN DECALAGE D'UN AN SUR LES DATES
# Ce fichier ne contient que cette fonction et la procédure de test pour l'enrichir

#Cette fonction prend pour objet un jeu de données, et retraite la colonne
#"lcom". C'est le premier des deux temps dans l'adjonction du COG ; ici on a une
#fonction formulée de manière prudente, de sorte qu'en l'appliquant à des
#millésimes futurs on a très peu de chances de réaliser une correction erronée.

#La seconde partie est réalisée par la fonction de redéfinition (volet 3 du code)


#PROCEDURE D'ENRICHISSEMENT

depchoisi="01"

#Retourne la liste des libellés communaux dans le(s) département(s) choisi(s) 
#qui ne trouvent pas de contrepartie dans le COG. J'ai coupé à 2010 pour éviter
#que des noms désuets dénotant probablement d'une erreur ou d'une confusion ne
#passent le test
polmun %>% 
  filter(dep%in%depchoisi) %>% 
  #Peut aider à y voir plus clair en purgeant les notes entre parenthèses
  #mutate(lcom=str_remove(lcom,"_?\\(.*$")) %>% 
  select(lcom) %>% pull() %>% 
  setdiff(filter(communes,dep==depchoisi&date>=as.Date("2010-01-01"))$lcom)

#Permet de contrôlet la bonne application des correctifs
polmun %>% 
  filter(dep%in%depchoisi) %>% 
  #mutate(lcom=str_remove(lcom,"_?\\(.*$")) %>% 
  correctionsortho2024() %>% 
  select(lcom) %>% pull() %>% 
  setdiff(filter(communes,dep==depchoisi&date>=as.Date("2010-01-01"))$lcom)


#LA FONCTION DE CORRECTION
#ELLE EST EXHAUSTIVE POUR LES MILLESIMES 2013-2024

correctionsortho2024=function(x){
  
  x[which(x$dep=="01"),]=mutate(
    x[which(x$dep=="01"),],
    lcom=case_when(
      lcom=="AMBERIEU_EN_DOMBES"~"AMBERIEUX_EN_DOMBES",
      lcom=="BELIGNIEUX"~"BELIGNEUX",
      lcom=="PEYZIEUX"~"PEYZIEUX_SUR_SAONE",
      lcom=="PREVESSINS_MOENS"~"PREVESSIN_MOENS",
      lcom=="ST_MARCEL_EN_DOMBES"~"ST_MARCEL",
      lcom=="ST_MARTIN_DU_FRESNE"~"ST_MARTIN_DU_FRENE",
      T~lcom)
  )
  
  x[which(x$dep=="02"),]=mutate(
    x[which(x$dep=="02"),],
    lcom=case_when(
      lcom=="ATHIES_SOUS_SLAON"~"ATHIES_SOUS_LAON",
      lcom=="CHERY_CHARTREUSE"~"CHERY_CHARTREUVE",
      lcom=="VILLERS_COTTERETES"~"VILLERS_COTTERETS",
      T~lcom)
  )
  
  x[which(x$dep=="04"),]=mutate(
    x[which(x$dep=="04"),],
    lcom=case_when(
      str_detect(lcom,"^C_?HATEAU_ARNOUXS?T?/?(_ST)?(_AUBAN)?$")~"CHATEAU_ARNOUX_ST_AUBAN",
      lcom=="STE_CROIX_DE_VERDON"~"STE_CROIX_DU_VERDON",
      lcom=="ST_TULLE"~"STE_TULLE",
      T~lcom)
  )
  
  x[which(x$dep=="05"),]=mutate(
    x[which(x$dep=="05"),],
    lcom=case_when(
      lcom=="AIGUILLES_EN_QUEYRAS"~"AIGUILLES",
      lcom=="LA_GRAVE_LA_MEIJE"~"LA_GRAVE",
      lcom=="LA_SALLE_LESALPES"~"LA_SALLE_LES_ALPES",
      lcom=="MONETIER_LES_BAINS"~"LE_MONETIER_LES_BAINS",
      lcom=="ST_APPOLINAIRE"~"ST_APOLLINAIRE",
      lcom=="ST_BONNET"~"ST_BONNET_EN_CHAMPSAUR",
      lcom=="VALBUECH_MEOUGE"~"VAL_BUECH_MEOUGE",
      lcom=="VILLARD_D_ARENE"~"VILLAR_D_ARENE",
      lcom=="VILLARD_ST_PANCRACE"~"VILLAR_ST_PANCRACE",
      T~str_replace(lcom,"LE_DEVOLUY","DEVOLUY"))
  )
  
  x[which(x$dep=="06"),]=mutate(
    x[which(x$dep=="06"),],
    lcom=case_when(
      str_detect(lcom,"^ANTIBES_?/?_?JUAN_LES_PINS$")~"ANTIBES",
      lcom=="BAR_SUR_LOUP"~"LE_BAR_SUR_LOUP",
      str_detect(lcom,"^BLAUS+AS?C$")~"BLAUSASC",
      lcom=="CANNET"~"LE_CANNET",
      str_detect(lcom,"^CHATEAUNEUF(_DE_GRASSE)?$")~"CHATEAUNEUF_GRASSE",
      lcom=="MANDELIEU"~"MANDELIEU_LA_NAPOULE",
      lcom=="ROQUETTE_SUR_SIAGNE"~"LA_ROQUETTE_SUR_SIAGNE",
      lcom=="ST_LAURENT_DUR_VAR"~"ST_LAURENT_DU_VAR",
      lcom=="TOURETTE_LEVENS"~"TOURRETTE_LEVENS",
      str_detect(lcom,"^TOURR?ETTES?_SUR_LOUP$")~"TOURRETTES_SUR_LOUP",
      str_detect(lcom,"^VALLAURIS(_GOLFE?_JUAN)?$")~"VALLAURIS",
      T~lcom)
  )
  
  x[which(x$dep=="07"),]=mutate(
    x[which(x$dep=="07"),],
    lcom=case_when(
      lcom=="ST_ETIIENNE_DE_FONTBELLON"~"ST_ETIENNE_DE_FONTBELLON",
      T~lcom)
  )
  
  x[which(x$dep=="08"),]=mutate(
    x[which(x$dep=="08"),],
    lcom=case_when(
      lcom=="NOUVION/MEUSE"~"NOUVION_SUR_MEUSE",
      T~lcom)
  )
  
  x[which(x$dep=="09"),]=mutate(
    x[which(x$dep=="09"),],
    lcom=case_when(
      lcom=="BASTIDE_DE_SEROU"~"LA_BASTIDE_DE_SEROU",
      lcom=="MERCUS"~"MERCUS_GARRABET",
      lcom=="TARASCON"~"TARASCON_SUR_ARIEGE",
      lcom=="TOUR_DU_CRIEU"~"LA_TOUR_DU_CRIEU",
      lcom=="VERNJOUL"~"VERNAJOUL",
      T~lcom)
  )
  
  x[which(x$dep=="11"),]=mutate(
    x[which(x$dep=="11"),],
    lcom=case_when(
      lcom=="CANET_D_AUDE"~"CANET",
      lcom=="CARCASONNE"~"CARCASSONNE",
      lcom=="CASTELNAU"~"CASTELNAU_D_AUDE",
      lcom=="FLEURY_D_AUDE"~"FLEURY",
      lcom=="LAPALME"~"LA_PALME",
      lcom=="ROQUECOURBE"~"ROQUECOURBE_MINERVOIS",
      lcom=="TRAUSSE_MINERVOIS"~"TRAUSSE",
      T~lcom)
  )
  
  x[which(x$dep=="12"),]=mutate(
    x[which(x$dep=="12"),],
    lcom=case_when(
      lcom=="CAPDENAC"~"CAPDENAC_GARE",
      lcom=="SEVRAC_D_AVEYRON"~"SEVERAC_D_AVEYRON",
      lcom=="ST_GENIEZ"~"ST_GENIEZ_D_OLT_ET_D_AUBRAC",
      lcom=="ST_GEORGE_DE_LUZENCON"~"ST_GEORGES_DE_LUZENCON",
      T~lcom)
  )
  
  x[which(x$dep=="13"),]=mutate(
    x[which(x$dep=="13"),],
    lcom=case_when(
      lcom=="BERRE"~"BERRE_L_ETANG",
      lcom=="LANCON_DE_PROVENCE"~"LANCON_PROVENCE",
      lcom=="LE_PARADOU"~"PARADOU",
      lcom=="LES_STES_MARIES_DE_LA_MER"~"STES_MARIES_DE_LA_MER",
      lcom=="MAS_BLANC_LES_ALPILLES"~"MAS_BLANC_DES_ALPILLES",
      lcom=="ST_PAUL_LEZ_DURANCE"~"ST_PAUL_LES_DURANCE",
      lcom=="VITROLLES_EN_PROVENCE"~"VITROLLES",
      T~lcom)
  )
  
  x[which(x$dep=="14"),]=mutate(
    x[which(x$dep=="14"),],
    lcom=case_when(
      lcom=="ARROMANCHES"~"ARROMANCHES_LES_BAINS",
      lcom=="GRAND_CAMP_MAISY"~"GRANDCAMP_MAISY",
      lcom%in%c("MERVILLE_FRANCEVILLE",
                "MERVILLE_FRANCE_VILLE_PLAGE")~"MERVILLE_FRANCEVILLE_PLAGE",
      lcom=="OUISTEHAM"~"OUISTREHAM",
      lcom=="TOURGUEVILLE"~"TOURGEVILLE",
      T~str_replace(lcom,"/ORNE","_SUR_ORNE"))
  )
  
  x[which(x$dep=="16"),]=mutate(
    x[which(x$dep=="16"),],
    lcom=case_when(
      lcom=="BARBEZIEUX"~"BARBEZIEUX_ST_HILAIRE",
      lcom=="CHATEAUNEUF"~"CHATEAUNEUF_SUR_CHARENTE",
      lcom=="MAGNAS_SUR_TOUVRE"~"MAGNAC_SUR_TOUVRE",
      lcom=="ST_MICHEL_D_ENTRAYGUES"~"ST_MICHEL",
      lcom=="ST_YRIEIX"~"ST_YRIEIX_SUR_CHARENTE",
      T~lcom)
  )
  
  x[which(x$dep=="17"),]=mutate(
    x[which(x$dep=="17"),],
    lcom=case_when(
      lcom=="AIGREFEUILLE"~"AIGREFEUILLE_D_AUNIS",
      lcom=="ANDILLY_LES_MARAIS"~"ANDILLY",
      lcom=="BOURCEFRANC"~"BOURCEFRANC_LE_CHAPUS",
      lcom=="LA_BREE_LES_BAINS_17840"~"LA_BREE_LES_BAINS",
      lcom%in%c("CHATEAU_D_OLERON","LA_CHATEAU_D_OLERON")~"LE_CHATEAU_D_OLERON",
      lcom%in%c("CHATELAILLON","CHATELLAILLON_PLAGE")~"CHATELAILLON_PLAGE",
      lcom=="CLION_SUR_SEUGNE"~"CLION",
      lcom=="COURCON_D_AUNIS"~"COURCON",
      lcom=="DOMPIERRE_S_SUR_MER"~"DOMPIERRE_SUR_MER",
      lcom%in%c("GRAND_VILLAGE_PLAGE","LE_GRAND_VILLAGE")~"LE_GRAND_VILLAGE_PLAGE",
      lcom=="LA_FLOTTE_EN_RE"~"LA_FLOTTE",
      lcom=="LA_TRAMBLADE"~"LA_TREMBLADE",
      lcom=="LES_MATHES_LA_PALMYRE"~"LES_MATHES",
      lcom=="LES_PORTES"~"LES_PORTES_EN_RE",
      lcom=="LOIX_EN_RE"~"LOIX",
      lcom=="L_EGUILLE_SUR_SEUDRE"~"L_EGUILLE",
      lcom=="NIEUL_S_SUR_MER"~"NIEUL_SUR_MER",
      lcom=="RIVEDOUX"~"RIVEDOUX_PLAGE",
      lcom=="ST_CIRE_D_AUNIS"~"CIRE_D_AUNIS",
      lcom=="ST_SAVINIEN_SUR_CHARENTE"~"ST_SAVINIEN",
      lcom=="ST_SYR_DU_DORET"~"ST_CYR_DU_DORET",
      lcom=="THAIRE_D_AUNIS"~"THAIRE",
      lcom=="VAUX_S_SUR_MER"~"VAUX_SUR_MER",
      T~lcom)
  )
  
  x[which(x$dep=="18"),]=mutate(
    x[which(x$dep=="18"),],
    lcom=case_when(
      lcom=="BELLEVILLE"~"BELLEVILLE_SUR_LOIRE",
      lcom=="BENGY"~"BENGY_SUR_CRAON",
      T~lcom)
  )
  
  x[which(x$dep=="19"),]=mutate(
    x[which(x$dep=="19"),],
    lcom=case_when(
      lcom=="BEAULIEU"~"BEAULIEU_SUR_DORDOGNE",
      lcom=="BRIVE"~"BRIVE_LA_GAILLARDE",
      T~lcom)
  )
  
  x[which(x$dep=="2A"),]=mutate(
    x[which(x$dep=="2A"),],
    lcom=case_when(
      lcom=="BONIFACCIO"~"BONIFACIO",
      lcom=="OTA_PORTO"~"OTA",
      T~lcom)
  )
  
  x[which(x$dep=="2B"),]=mutate(
    x[which(x$dep=="2B"),],
    lcom=case_when(
      lcom=="ILE_ROUSSE"~"L_ILE_ROUSSE",
      T~lcom)
  )
  
  x[which(x$dep=="21"),]=mutate(
    x[which(x$dep=="21"),],
    lcom=case_when(
      lcom=="FOINTAINE_LES_DIJON"~"FONTAINE_LES_DIJON",
      T~lcom)
  )
  
  x[which(x$dep=="22"),]=mutate(
    x[which(x$dep=="22"),],
    lcom=case_when(
      lcom=="BINIC_ETAPLE_SUR_MER"~"BINIC_ETABLES_SUR_MER",
      lcom=="BREHAT"~"ILE_DE_BREHAT",
      lcom=="ETABLES_S_SUR_MER"~"ETABLES_SUR_MER",
      lcom=="FEHEL"~"FREHEL",
      lcom=="LANNIION"~"LANNION",
      lcom=="PLOEUC/LIE"~"PLOEUC_SUR_LIE",
      T~lcom)
  )
  
  x[which(x$dep=="23"),]=mutate(
    x[which(x$dep=="23"),],
    lcom=case_when(
      str_detect(lcom,"AUBUSSON")~"AUBUSSON",
      T~lcom)
  )
  
  x[which(x$dep=="24"),]=mutate(
    x[which(x$dep=="24"),],
    lcom=case_when(
      lcom=="BEYNAC"~"BEYNAC_ET_CAZENAC",
      lcom=="BOULAZC_ISLE_MANOIRE"~"BOULAZAC_ISLE_MANOIRE",
      lcom=="LES_EYZIES_DE_TAYAC"~"LES_EYZIES",
      lcom=="MONTIGNAC_EN_PERIGORD"~"MONTIGNAC_LASCAUX",
      lcom=="PORT_STE_FOY_ET_PONCHAT"~"PORT_STE_FOY_ET_PONCHAPT",
      lcom%in%c("TERRASSON","TERRASSON_LA_VILLEDIEU")~"TERRASSON_LAVILLEDIEU",
      T~lcom)
  )
  
  x[which(x$dep=="25"),]=mutate(
    x[which(x$dep=="25"),],
    lcom=case_when(
      lcom=="LES_HOPITAUX_NEUF"~"LES_HOPITAUX_NEUFS",
      lcom=="SELONCOUR"~"SELONCOURT",
      lcom=="SOMBACOURT"~"SOMBACOUR",
      T~lcom)
  )
  
  x[which(x$dep=="26"),]=mutate(
    x[which(x$dep=="26"),],
    lcom=case_when(
      lcom%in%c("BEGUDE_DE_MAZENC","ROCHE_DE_GLUN")~paste("LA_",lcom,sep=""),
      lcom%in%c("LIVRON","LORIOL")~paste(lcom,"_SUR_DROME",sep=""),
      lcom%in%c("ROMANS")~paste(lcom,"_SUR_ISERE",sep=""),
      lcom=="ALEX"~"ALLEX",
      lcom=="AOUSTE_SUR_SYE_(DIE)"~"AOUSTE_SUR_SYE",
      lcom=="BUIS_LES_BARRONIES"~"BUIS_LES_BARONNIES",
      lcom=="DONZERE_26290"~"DONZERE",
      lcom=="LA_PENNE_SUR_OUVEZE"~"LA_PENNE_SUR_L_OUVEZE",
      lcom=="MONTVENDE"~"MONTVENDRE",
      lcom=="PONT_D_ISERE"~"PONT_DE_L_ISERE",
      str_detect(lcom,"^ST_DONN?AT(_SUR_L_HERBASSE)?$")~"ST_DONAT_SUR_L_HERBASSE",
      lcom=="ST_NAZAIRE_EN_RYS"~"ST_NAZAIRE_EN_ROYANS",
      lcom=="ST_PAULTROIS_CHATEAUX"~"ST_PAUL_TROIS_CHATEAUX",
      lcom=="ST_VALLIER_SUR_RHONE"~"ST_VALLIER",
      T~lcom)
  )
  
  x[which(x$dep=="27"),]=mutate(x[which(x$dep=="27"),],lcom=case_when(
    lcom%in%c("AUBEVOYE_(VAL_D_HAZEY)",
              "VAL_D_HAZEY")&annee<2026~"LE_VAL_D_HAZEY",
    lcom%in%c("BONNEVILLE_SUR_ITON",
              "LA_BONNVEILLE_SUR_ITON")~"LA_BONNEVILLE_SUR_ITON",
    lcom=="BOSC_ROGER_EN_ROUMOIS"~"LE_BOSC_ROGER_EN_ROUMOIS",
    lcom=="BRETEUIL_SUR_ITON"~"BRETEUIL",
    lcom=="LE_MANOIR_SUR_SEINE"~"LE_MANOIR",
    lcom=="LE_VAL_DE_REUIL"~"VAL_DE_REUIL",
    T~lcom)
  )
  
  x[which(x$dep=="28"),]=mutate(
    x[which(x$dep=="28"),],
    lcom=case_when(
      lcom=="CHAMPOL"~"CHAMPHOL",
      lcom=="CLOYES_LES_3_RIVIERES"~"CLOYES_LES_TROIS_RIVIERES",
      lcom=="COUVILLE_SUR_EURE"~"COURVILLE_SUR_EURE",
      lcom=="MAZIERES_EN_DROUAIS"~"MEZIERES_EN_DROUAIS",
      lcom=="ST_AVIT_LES_GUEPIERES"~"ST_AVIT_LES_GUESPIERES",
      str_detect(lcom,"^ST_DENIS(_DE)?_L_?ANNERAY$")~"ST_DENIS_LANNERAY",
      lcom=="VILLAGES_VOVEENS"~"LES_VILLAGES_VOVEENS",
      lcom=="VILLEMEUX"~"VILLEMEUX_SUR_EURE",
      T~lcom)
  )
  
  x[which(x$dep=="29"),]=mutate(x[which(x$dep=="29"),],lcom=case_when(
    lcom=="BRIEC_DE_L_ODET"~"BRIEC",
    lcom=="LE_GUILVINEC"~"GUILVINEC",
    lcom=="PENMARC_H"~"PENMARCH",
    lcom=="PLOBANNALEC"~"PLOBANNALEC_LESCONIL",
    lcom=="PLOUNEOUR_BRIGNOAGAN_PLAGES"~"PLOUNEOUR_BRIGNOGAN_PLAGES",
    T~lcom)
  )
  
  x[which(x$dep=="30"),]=mutate(x[which(x$dep=="30"),],lcom=case_when(
    lcom=="CRUVIER_LASCOURS"~"CRUVIERS_LASCOURS",
    lcom=="FONS_OUTRE_GARDON"~"FONS",
    lcom=="LAUDUN_LARDOISE"~"LAUDUN_L_ARDOISE",
    lcom%in%c("MARSILLARGUES_ATTUECH",
              "MASSILLARGUES_ATUECH")~"MASSILLARGUES_ATTUECH",
    lcom=="ST_CHRISTOL_LES_ALES"~"ST_CHRISTOL_LEZ_ALES",
    T~lcom)
  )
  
  x[which(x$dep=="31"),]=mutate(x[which(x$dep=="31"),],lcom=case_when(
    str_detect(lcom,"^(CAZERES|GRENADE|ROQUES)_SUR_GARONNE$"
    )~str_remove(lcom,"_SUR_GARONNE"),
    lcom=="BUZET"~"BUZET_SUR_TARN",
    lcom=="LAGARDELLE"~"LAGARDELLE_SUR_LEZE",
    lcom=="LE_VERNET"~"VERNET",
    lcom=="LUSSAN"~"LUSSAN_ADEILHAC",
    lcom=="RAMONVILLE_ST_AGNES"~"RAMONVILLE_ST_AGNE",
    lcom=="ROUFFIAC"~"ROUFFIAC_TOLOSAN",
    lcom=="SAINTORENS_DE_GAMEVILLE"~"ST_ORENS_DE_GAMEVILLE",
    lcom=="VILLEMUR"~"VILLEMUR_SUR_TARN",
    T~lcom)
  )
  
  x[which(x$dep=="33"),]=mutate(
    x[which(x$dep=="33"),],
    lcom=case_when(
      lcom=="AMBARES"~"AMBARES_ET_LAGRAVE",
      lcom=="ANDERNOS"~"ANDERNOS_LES_BAINS",
      lcom=="ARTIGUES"&annee==2018~"ARTIGUES_PRES_BORDEAUX",
      lcom=="ARTIGUES_DE_LUSSAC"~"LES_ARTIGUES_DE_LUSSAC",
      lcom=="BEYCHAC_ET_CAILLEAU"~"BEYCHAC_ET_CAILLAU",
      lcom=="BOURG_SUR_GIRONDE"~"BOURG",
      lcom=="BORDEAUX(33063)"~"BORDEAUX",
      lcom=="BRAUD"~"BRAUD_ET_ST_LOUIS",
      lcom=="CADILLAC"~"CADILLAC_SUR_GARONNE",
      lcom=="CASTELNAU_MEDOC"~"CASTELNAU_DE_MEDOC",
      lcom%in%c("CASTRES","CASTRES_SUR_GIRONDE")~"CASTRES_GIRONDE",
      lcom=="CIVRAC_DE_DORDOGNE"~"CIVRAC_SUR_DORDOGNE",
      lcom=="GAILLAN_MEDOC"~"GAILLAN_EN_MEDOC",
      lcom=="GIRONDE/DROPT"~"GIRONDE_SUR_DROPT",
      lcom=="GRAYAN_L_HOPITAL"~"GRAYAN_ET_L_HOPITAL",
      lcom%in%c("LALANDE_FRONSAC","LALANDE_DE_FRONSAC")~"LA_LANDE_DE_FRONSAC",
      lcom%in%c("LE_TAILLAN","TAILLAN_MEDOC")~"LE_TAILLAN_MEDOC",
      lcom=="LESPARRE"~"LESPARRE_MEDOC",
      lcom=="LE_VERDON"~"LE_VERDON_SUR_MER",
      lcom=="LISTRAC"~"LISTRAC_MEDOC",
      str_detect(lcom,"^MARTIGNAS(_SUR_JALLES?)?$")~"MARTIGNAS_SUR_JALLE",
      lcom=="MOULIS_MEDOC"~"MOULIS_EN_MEDOC",
      lcom%in%c("PIAN_SUR_GARONNE","PIAN/GARONNE")~"LE_PIAN_SUR_GARONNE",
      lcom=="SOULAC"~"SOULAC_SUR_MER",
      lcom=="ST_ANDRE"&annee==2014~"ST_ANDRE_DE_CUBZAC",
      lcom%in%c("ST_AUBIN_MEDOC","ST_AUBIN_DU_MEDOC")~"ST_AUBIN_DE_MEDOC",
      lcom=="ST_CAPRAIS"&annee==2018~"ST_CAPRAIS_DE_BORDEAUX",
      lcom=="ST_CHRISTOLY"&annee%in%c(2014,2017)~"ST_CHRISTOLY_DE_BLAYE",
      lcom=="ST_CHRISTOLY_EN_MEDOC"~"ST_CHRISTOLY_MEDOC",
      lcom=="ST_HELENE"~"STE_HELENE",
      lcom=="ST_LAURENT"~"ST_LAURENT_MEDOC",
      lcom=="ST_LOUIS_DE_MONTFERRAN"~"ST_LOUIS_DE_MONTFERRAND",
      lcom=="ST_SULPICE"~"ST_SULPICE_DE_FALEYRENS",
      lcom=="ST_VIVIEN_MEDOC"~"ST_VIVIEN_DE_MEDOC",
      lcom=="VENDAYS"~"VENDAYS_MONTALIVET",
      lcom=="VAYRE"~"VAYRES",
      T~lcom)
  )
  
  x[which(x$dep=="34"),]=mutate(x[which(x$dep=="34"),],lcom=case_when(
    lcom=="BOUJAN"~"BOUJAN_SUR_LIBRON",
    lcom=="BOUSQUET_D_ORB"~"LE_BOUSQUET_D_ORB",
    lcom=="CASTENAU_DE_GUERS"~"CASTELNAU_DE_GUERS",
    lcom%in%c("CESSENOM_SUR_ORB","CESSENON")~"CESSENON_SUR_ORB",
    lcom=="CRESSAN"~"CREISSAN",
    lcom=="LAMALOU"~"LAMALOU_LES_BAINS",
    lcom=="LA_SALVETAT"~"LA_SALVETAT_SUR_AGOUT",
    lcom=="LEZIGAN_LA_CEBE"~"LEZIGNAN_LA_CEBE",
    lcom=="LIGNAN"~"LIGNAN_SUR_ORB",
    lcom=="MAUGUIO_CARNON"~"MAUGUIO",
    lcom=="NISSAN_LES_ENSERUNE"~"NISSAN_LEZ_ENSERUNE",
    lcom=="PRADES_SUR_LEZ"~"PRADES_LE_LEZ",
    lcom=="SOUSBES"~"SOUBES",
    lcom=="ST_CLEMENT_DE_RIVERE"~"ST_CLEMENT_DE_RIVIERE",
    lcom=="ST_GENIEZ_DE_FONTEDIT"~"ST_GENIES_DE_FONTEDIT",
    lcom=="ST_MATHIEU_DE_TREVIES"~"ST_MATHIEU_DE_TREVIERS",
    lcom=="ST_PARGOIR"~"ST_PARGOIRE",
    lcom=="ST_PONS_DE_THOMIERE"~"ST_PONS_DE_THOMIERES",
    T~lcom)
  )
  
  x[which(x$dep=="35"),]=mutate(
    x[which(x$dep=="35"),],
    lcom=case_when(
      lcom=="ARGENTRE_DU_PESSIS"~"ARGENTRE_DU_PLESSIS",
      lcom=="DOL_DE_BRETANGE"~"DOL_DE_BRETAGNE",
      lcom=="MONTFROT_SUR_MEU"~"MONTFORT_SUR_MEU",
      lcom=="ST_JOUAN_DU_GUERET"~"ST_JOUAN_DES_GUERETS",
      lcom=="VALD_IZE"~"VAL_D_IZE",
      #Les parenthèses n'indiquent pas mutualisation, elles donnent la liste 
      #des communes fondatrices des communes nouvelles en question
      lcom=="MAEN_ROCH_(ST_BRICE_ET_ST_ETIENNE)"~"MAEN_ROCH",
      lcom=="VAL_D_ANAST_(MAURE_DE_BRETAGNE+CAMPEL)"~"VAL_D_ANAST",
      T~lcom)
  )
  
  x[which(x$dep=="36"),]=mutate(
    x[which(x$dep=="36"),],
    lcom=case_when(
      lcom=="CHATILLON"~"CHATILLON_SUR_INDRE",
      T~lcom)
  )
  
  x[which(x$dep=="37"),]=mutate(
    x[which(x$dep=="37"),],
    lcom=case_when(
      lcom=="ESVRES_SUR_INDRE"~"ESVRES",
      lcom=="LA_VILE_AUX_DAMES"~"LA_VILLE_AUX_DAMES",
      T~lcom)
  )
  
  x[which(x$dep=="38"),]=mutate(
    x[which(x$dep=="38"),],
    lcom=case_when(
      lcom=="ALLEMONT"~"ALLEMOND",
      lcom=="APRIEU"~"APPRIEU",
      lcom=="BOURG_D_OISANS"~"LE_BOURG_D_OISANS",
      #Ce choix de correction ne semble a priori pas évident, mais l'attribuer à
      #Charvieu-Chavagnieux crée un doublon visiblement erroné. Nous avons
      #cependant la preuve :
      #https://www.ledauphine.com/isere-nord/2013/11/21/le-dernier-garde-champetre-part-a-la-retraite
      lcom=="CHAVAGNIEU"~"CHAMAGNIEU",
      str_detect(lcom,"^CHARVIEU_CHAVAGNI?EUX?$")~"CHARVIEU_CHAVAGNEUX",
      lcom=="ESTRANBLIN"~"ESTRABLIN",
      lcom=="GRESSE_EN_VECORS"~"GRESSE_EN_VERCORS",
      lcom=="HEYRIEU"~"HEYRIEUX",
      lcom=="HUEZ_EN_OISANS"~"HUEZ",
      lcom=="LA_BASTIE_DIVISIN"~"LA_BATIE_DIVISIN",
      lcom=="LA_GRAND_LEMPS"~"LE_GRAND_LEMPS",
      lcom=="LA_VERPILLERE"~"LA_VERPILLIERE",
      lcom=="LE_FONTANIL"~"FONTANIL_CORNILLON",
      lcom=="OZ_EN_OISANS"~"OZ",
      lcom=="PEAGE_DE_ROUSSILLON"~"LE_PEAGE_DE_ROUSSILLON",
      str_detect(lcom,"PONT_(DE|EN)_BEAUVOISIO?N")~"LE_PONT_DE_BEAUVOISIN",
      lcom=="PONT_DE_CLAIX"~"LE_PONT_DE_CLAIX",
      lcom=="SEYSSINET"~"SEYSSINET_PARISET",
      lcom=="ST_ROMAN_DE_JALIONAS"~"ST_ROMAIN_DE_JALIONAS",
      lcom%in%c("VARCES","VARCES_ALL_ET_R")~"VARCES_ALLIERES_ET_RISSET",
      lcom=="VAULNAVEYS_LE_HT"~"VAULNAVEYS_LE_HAUT",
      lcom=="VILLAGE_DU_LAC_DE_PALADRU"~"VILLAGES_DU_LAC_DE_PALADRU",
      T~lcom)
  )
  
  x[which(x$dep=="39"),]=mutate(
    x[which(x$dep=="39"),],
    lcom=case_when(
      str_detect(lcom,
                 "^(MOREZ_)?HAUTS?_(DE_)?BIEN+E(_MOREZ)?$"
      )~"HAUTS_DE_BIENNE",
      T~lcom)
  )
  
  x[which(x$dep=="40"),]=mutate(
    x[which(x$dep=="40"),],
    lcom=case_when(
      lcom=="AIRE_SUR_ADOUR"~"AIRE_SUR_L_ADOUR",
      lcom=="GRENADE_SUR_ADOUR"~"GRENADE_SUR_L_ADOUR",
      lcom=="PONTONX_SUR_ADOUR"~"PONTONX_SUR_L_ADOUR",
      lcom=="SAINGUINET"~"SANGUINET",
      lcom=="ST_VT_DE_TYROSSE"~"ST_VINCENT_DE_TYROSSE",
      lcom=="VIEUX_BOUCAU"~"VIEUX_BOUCAU_LES_BAINS",
      T~lcom)
  )
  
  x[which(x$dep=="41"),]=mutate(
    x[which(x$dep=="41"),],
    lcom=case_when(
      lcom=="CELETTES"~"CELLETTES",
      lcom=="COUR_CHEVRNY"~"COUR_CHEVERNY",
      lcom=="MONTRICHAARD"~"MONTRICHARD",
      lcom=="VEUZAIN"~"VEUZAIN_SUR_LOIRE",
      T~lcom)
  )
  
  #L'entrée Régigneux est difficile à expliquer. Périgneux ? Il y a d'autres
  #options. Dans tous les cas, la remontée étant vierge, elle est à ignorer 
  #pour un jeu régional, et à conserver pour un jeu de données brutes
  x[which(x$dep=="42"),]=mutate(
    x[which(x$dep=="42"),],
    lcom=case_when(
      lcom=="CHAMBON_FEUGEROLLES"~"LE_CHAMBON_FEUGEROLLES",
      lcom=="ST_JEAN_BONNEFOND"~"ST_JEAN_BONNEFONDS",
      lcom=="ST_ROMAINE_LE_PUY"~"ST_ROMAIN_LE_PUY",
      T~lcom)
  )
  
  x[which(x$dep=="43"),]=mutate(
    x[which(x$dep=="43"),],
    lcom=case_when(
      str_detect(lcom,"AUREC(/|_)LOIRE")~"AUREC_SUR_LOIRE",
      lcom=="CRAPONNE_/_ARZON"~"CRAPONNE_SUR_ARZON",
      lcom=="ESPALY"~"ESPALY_ST_MARCEL",
      lcom=="ST_SIGOLENE"~"STE_SIGOLENE",
      T~lcom)
  )
  
  x[which(x$dep=="44"),]=mutate(
    x[which(x$dep=="44"),],
    lcom=case_when(
      lcom=="CHAPELLE_BASSE_MER"~"LA_CHAPELLE_BASSE_MER",
      str_detect(lcom,"^GRAND?CHAMPS?_DES_FONTAINES$")~"GRANDCHAMP_DES_FONTAINES",
      lcom=="LA_BAULE"~"LA_BAULE_ESCOUBLAC",
      lcom=="LA_BERNERIE"~"LA_BERNERIE_EN_RETZ",
      lcom=="LA_HAYE_FOUASSIERE"~"LA_HAIE_FOUASSIERE",
      lcom=="LES_MOUTIERS"~"LES_MOUTIERS_EN_RETZ",
      str_detect(lcom,"^PONT?_?CHATEAU$")~"PONTCHATEAU",
      str_detect(lcom,"^ST_PHILBERT(_DE_GRAND_?LIEU)?$")~"ST_PHILBERT_DE_GRAND_LIEU",
      lcom=="THOUARE"~"THOUARE_SUR_LOIRE",
      lcom=="TREILLERES"~"TREILLIERES",
      T~lcom)
  )
  
  x[which(x$dep=="45"),]=mutate(
    x[which(x$dep=="45"),],
    lcom=case_when(
      str_detect(lcom,"^BEAUCHAMPS?_SUR_HUILL?ARD$")~"BEAUCHAMPS_SUR_HUILLARD",
      lcom=="BEAUNE_LA_ROLLANDE"~"BEAUNE_LA_ROLANDE",
      lcom=="BONNY"~"BONNY_SUR_LOIRE",
      lcom=="BRIARE_LE_CANAL"~"BRIARE",
      lcom=="CHALETTE/LOING"~"CHALETTE_SUR_LOING",
      lcom=="CORBEILLES_EN_GATINAIS"~"CORBEILLES",
      lcom=="LION_EN_SUILLAS"~"LION_EN_SULLIAS",
      lcom=="NEUVLLE_AUX_BOIS"~"NEUVILLE_AUX_BOIS",
      lcom=="NEUVY_EN_SUILLIAS"~"NEUVY_EN_SULLIAS",
      lcom=="NOYER"~"NOYERS",
      lcom=="PATAY_45310"~"PATAY",
      str_detect(lcom,"^PITHIVIERS(_|6)LE_VI?EIL$")~"PITHIVIERS_LE_VIEIL",
      lcom=="PREFONTAINNES"~"PREFONTAINES",
      lcom=="SERMAISE"~"SERMAISES",
      str_detect(lcom,"^VARR?ENNES_CHAI?NGY$")~"VARENNES_CHANGY",
      T~lcom)
  )
  
  x[which(x$dep=="47"),]=mutate(
    x[which(x$dep=="47"),],
    lcom=case_when(
      lcom=="CATELJALOUX"~"CASTELJALOUX",
      str_detect(lcom,"^(LE_)?PASSAGE_D_AGEN$")~"LE_PASSAGE",
      lcom=="PUJOL"~"PUJOLS",
      T~lcom)
  )
  
  #Les ",_?MENDE", au vu des séries, ne sont que des indications géographiques
  #Peut-être d'appartenance à une zone de gendarmerie, à un canton/arrondissement
  x[which(x$dep=="48"),]=mutate(
    x[which(x$dep=="48"),],
    lcom=case_when(
      lcom=="FLORAC_3_RIVIERES"~"FLORAC_TROIS_RIVIERES",
      T~str_remove(lcom,",_?MENDE$"))
  )
  
  x[which(x$dep=="49"),]=mutate(
    x[which(x$dep=="49"),],
    lcom=case_when(
      lcom=="BRISSAC_LOIR_AUBANCE"~"BRISSAC_LOIRE_AUBANCE",
      lcom=="CHALONNES"~"CHALONNES_SUR_LOIRE",
      lcom=="LE_MAY_SUE_EVRE"~"LE_MAY_SUR_EVRE",
      lcom=="MONTREVAULT_SUE_EVRE"~"MONTREVAULT_SUR_EVRE",
      lcom=="SEGREE_EN_ANJOU_BLEU"~"SEGRE_EN_ANJOU_BLEU",
      lcom=="STE_GEORGES_DES_GARDES"~"ST_GEORGES_DES_GARDES",
      lcom=="ST_BARHELEMY_D_ANJOU"~"ST_BARTHELEMY_D_ANJOU",
      lcom=="ST_CHRISTINE"~"STE_CHRISTINE",
      T~lcom)
  )
  
  x[which(x$dep=="50"),]=mutate(x[which(x$dep=="50"),],lcom=case_when(
    lcom=="CARENTANLES_MARAIS"~"CARENTAN_LES_MARAIS",
    lcom=="PORT_BAIL"~"PORT_BAIL_SUR_MER",
    lcom=="QUETREVILLE_SUR_SIENNE"~"QUETTREVILLE_SUR_SIENNE",
    T~lcom)
  )
  
  x[which(x$dep=="51"),]=mutate(
    x[which(x$dep=="51"),],
    lcom=case_when(
      lcom=="CHALONS_ENCHAMPAGNE"~"CHALONS_EN_CHAMPAGNE",
      lcom=="SEZANNE_51"~"SEZANNE",
      T~lcom)
  )
  
  x[which(x$dep=="52"),]=mutate(
    x[which(x$dep=="52"),],
    lcom=case_when(
      lcom=="ST_DIZER"~"ST_DIZIER",
      T~lcom)
  )
  
  x[which(x$dep=="54"),]=mutate(
    x[which(x$dep=="54"),],
    lcom=case_when(
      lcom=="BLENOD_LES_PAM"~"BLENOD_LES_PONT_A_MOUSSON",
      lcom=="LANEVEUVILLE_DEVANT_NANCY"~"LANEUVEVILLE_DEVANT_NANCY",
      lcom=="VANDOEUVRE"~"VANDOEUVRE_LES_NANCY",
      T~lcom)
  )
  
  x[which(x$dep=="56"),]=mutate(x[which(x$dep=="56"),],lcom=case_when(
    lcom=="CRAC_H"~"CRACH",
    lcom=="GAVRES_("~"GAVRES",
    lcom=="ILES_AUX_MOINES"~"ILE_AUX_MOINES",
    lcom=="JOSSELLIN"~"JOSSELIN",
    T~lcom)
  )
  
  x[which(x$dep=="57"),]=mutate(
    x[which(x$dep=="57"),],
    lcom=case_when(
      lcom=="ALTRIPE"~"ALTRIPPE",
      lcom=="BASS_HAM"~"BASSE_HAM",
      lcom=="BOULAY"~"BOULAY_MOSELLE",
      lcom=="FLORENGE"~"FLORANGE",
      str_detect(lcom,"FREY?MING(_MERLEBACH)?")~"FREYMING_MERLEBACH",
      lcom=="HETTANGE"~"HETTANGE_GRANDE",
      lcom=="KEDANGE_SUR_KANNER"~"KEDANGE_SUR_CANNER",
      lcom=="KNUTTANGE"~"KNUTANGE",
      lcom=="METEZERESCHE"~"METZERESCHE",
      lcom=="METZERWISSE"~"METZERVISSE",
      lcom=="NOVEANT"~"NOVEANT_SUR_MOSELLE",
      str_detect(lcom,"PETIT?E_ROS+ELLE")~"PETITE_ROSSELLE",
      lcom=="SEREMANGE_ERSERANGE"~"SEREMANGE_ERZANGE",
      lcom=="SHOENECK"~"SCHOENECK",
      T~lcom)
  )
  
  x[which(x$dep=="58"),]=mutate(
    x[which(x$dep=="58"),],
    lcom=case_when(
      lcom=="COSNE_SUR_LOIRE"~"COSNE_COURS_SUR_LOIRE",
      lcom=="GUERGINY"~"GUERIGNY",
      lcom=="POULLY_SUR_LOIRE"~"POUILLY_SUR_LOIRE",
      T~lcom)
  )
  
  x[which(x$dep=="59"),]=mutate(x[which(x$dep=="59"),],lcom=case_when(
    lcom=="ARMENTIERES_LA_CHAPPELLE_D_ARMENTIERES"~"ARMENTIERES_LA_CHAPELLE_D_ARMENTIERES",
    str_detect(lcom,"^AULNOYE?_LE(S|Z)_VALENCIENNES$")~"AULNOY_LEZ_VALENCIENNES",
    str_detect(lcom,"^AVESNES_LE(S|Z)_AUBERTS?$")~"AVESNES_LES_AUBERT",
    lcom=="BEAUVOUS_EN_CAMBRESIS"~"BEAUVOIS_EN_CAMBRESIS",
    str_detect(lcom,"^BRUAY(_SUR_(L_)?ESC?AUT)?$")~"BRUAY_SUR_L_ESCAUT",
    str_detect(lcom,"^CATEAU(_EN)?_CAMBRESIS$")~"LE_CATEAU_CAMBRESIS",
    lcom=="CONDE"~"CONDE_SUR_L_ESCAUT",
    lcom=="COUDERQUE"~"COUDEKERQUE_BRANCHE",
    lcom=="COURCHELETETS"~"COURCHELETTES",
    lcom=="ERQUINGHEM"~"ERQUINGHEM_LYS",
    lcom=="FLINES_LES_RACHES"~"FLINES_LEZ_RACHES",
    lcom=="FRESNES"~"FRESNES_SUR_ESCAUT",
    lcom=="GRANDE_SY_NTHE"~"GRANDE_SYNTHE",
    lcom=="GRAVELINE"~"GRAVELINES",
    lcom=="LYZ_LEZ_LANNOY"~"LYS_LEZ_LANNOY",
    lcom=="ONNAING_59264"~"ONNAING",
    lcom=="OSTRICURT"~"OSTRICOURT",
    lcom=="RAILLENCOURT_ST_OLLE"~"RAILLENCOURT_STE_OLLE",
    lcom=="ST_AMAND"~"ST_AMAND_LES_EAUX",
    lcom=="ST_ANDRE"&annee%in%2016:2021~"ST_ANDRE_LEZ_LILLE",
    lcom=="ST_HILAIRE"&annee==2025~"ST_HILAIRE_LEZ_CAMBRAI",
    lcom=="ST_POL"~"ST_POL_SUR_MER",
    lcom=="TEMPLEUVE"~"TEMPLEUVE_EN_PEVELE",
    lcom=="WALINCOURT_SALVIGNY"~"WALINCOURT_SELVIGNY",
    T~lcom)
  )
  
  x[which(x$dep=="60"),]=mutate(
    x[which(x$dep=="60"),],
    lcom=case_when(
      lcom=="BETHY_ST_PIERRE"~"BETHISY_ST_PIERRE",
      lcom=="BRETEUIL_SUR_NOYE"~"BRETEUIL",
      lcom=="LA_CHAPPELLE_EN_SERVAL"~"LA_CHAPELLE_EN_SERVAL",
      lcom=="LA_CROIX_ST_OUEN"~"LACROIX_ST_OUEN",
      lcom=="LE_PLESSY_BRION"~"LE_PLESSIS_BRION",
      lcom=="LONGEUIL_ANNEL"~"LONGUEIL_ANNEL",
      lcom=="MESNIL_EN_THELLE"~"LE_MESNIL_EN_THELLE",
      lcom=="MONTMARTINE"~"MONTMARTIN",
      str_detect(lcom,"^NANTEUIL_LE_H(A|O)UDOU?IN$")~"NANTEUIL_LE_HAUDOUIN",
      str_detect(lcom,"^RIBECOURT?_DR?ESLINCOURT$")~"RIBECOURT_DRESLINCOURT",
      lcom=="ST_LEU_D_ESSERNET"~"ST_LEU_D_ESSERENT",
      lcom=="VILLERS_SS_ST_LEU"~"VILLERS_SOUS_ST_LEU",
      T~lcom)
  )
  
  x[which(x$dep=="61"),]=mutate(
    x[which(x$dep=="61"),],
    lcom=case_when(
      lcom=="ATHIS"&annee==2015~"ATHIS_DE_L_ORNE",
      T~lcom)
  )
  
  x[which(x$dep=="62"),]=mutate(
    x[which(x$dep=="62"),],
    lcom=case_when(
      lcom=="BAILLEUL_SIRE_BERTHOULT"~"BAILLEUL_SIR_BERTHOULT",
      lcom=="BERCK_SUR_MER"~"BERCK",
      lcom=="BOULONE_SUR_MER"~"BOULOGNE_SUR_MER",
      str_detect(lcom,"^CALONNE_RIC?QUART$")~"CALONNE_RICOUART",
      lcom=="CHOQUES"~"CHOCQUES",
      lcom=="ENQUIHEN_PLAGE"~"EQUIHEN_PLAGE",
      lcom=="ETAPLES_SUR_MER"~"ETAPLES",
      lcom=="FOUQUIERES_LE_LENS"~"FOUQUIERES_LES_LENS",
      lcom=="ISBERGUE"~"ISBERGUES",
      lcom=="LE_PARC"~"LE_PARCQ",
      lcom=="LE_TOUQUET"~"LE_TOUQUET_PARIS_PLAGE",
      lcom=="MARCK_EN_CALAISIS"~"MARCK",
      lcom=="MEURCIN"~"MEURCHIN",
      lcom=="MONTREUI_SUR_MER"~"MONTREUIL_SUR_MER",
      lcom=="STE_CATHERINE_LES_ARRAS"~"STE_CATHERINE",
      lcom=="VENDIN_LEZ_BETHUNE"~"VENDIN_LES_BETHUNE",
      lcom=="VENDIN_LE_VEIL"~"VENDIN_LE_VIEIL",
      lcom=="VERQUUIN"~"VERQUIN",
      lcom=="VITRY_EN_AROIS"~"VITRY_EN_ARTOIS",
      lcom=="WIZERNNES"~"WIZERNES",
      T~lcom)
  )
  
  x[which(x$dep=="63"),]=mutate(
    x[which(x$dep=="63"),],
    lcom=case_when(
      lcom=="BESSE_ST_ANASTAISE"~"BESSE_ET_ST_ANASTAISE",
      lcom=="CLERMONT_FD"~"CLERMONT_FERRAND",
      lcom=="COURNON"~"COURNON_D_AUVERGNE",
      lcom=="LE_MONT_DORE"~"MONT_DORE",
      lcom=="MARTRES_DE_VEYRE"~"LES_MARTRES_DE_VEYRE",
      lcom=="VIVEROLES"~"VIVEROLS",
      T~lcom)
  )
  
  x[which(x$dep=="64"),]=mutate(
    x[which(x$dep=="64"),],
    lcom=case_when(
      lcom=="ARROS_NAY"~"ARROS_DE_NAY",
      lcom=="CAMBO"~"CAMBO_LES_BAINS",
      lcom=="SALIES_DE_EARN"~"SALIES_DE_BEARN",
      T~lcom)
  )
  
  x[which(x$dep=="65"),]=mutate(x[which(x$dep=="65"),],lcom=case_when(
    lcom=="BORDERE_SUR_L_ECHEZ"~"BORDERES_SUR_L_ECHEZ",
    lcom=="CAUTERET"~"CAUTERETS",
    lcom=="LOUDENVIEILLE"~"LOUDENVIELLE",
    lcom=="ST_LARY"~"ST_LARY_SOULAN",
    T~lcom)
  )
  
  x[which(x$dep=="66"),]=mutate(x[which(x$dep=="66"),],lcom=case_when(
    lcom=="AMELIE_LES_BAINS"~"AMELIE_LES_BAINS_PALALDA",
    lcom=="BAIXA"~"BAIXAS",
    lcom=="CASE_DE_PENE"~"CASES_DE_PENE",
    lcom=="ESPIRA_DE_AGLY"~"ESPIRA_DE_L_AGLY",
    lcom=="FONT_ROMEU"~"FONT_ROMEU_ODEILLO_VIA",
    lcom=="ILLE_SU_TET"~"ILLE_SUR_TET",
    lcom=="LE_PERTUS"~"LE_PERTHUS",
    lcom=="MASOS_(LOS)"~"LOS_MASOS",
    lcom=="MAURILLAS_LAS_ILLAS"~"MAUREILLAS_LAS_ILLAS",
    lcom=="ORTAFFA_NOUVELLE_COMMUNE_AVEC_PM"~"ORTAFFA",
    lcom=="PEYRESTORES"~"PEYRESTORTES",
    lcom=="PONTEILLA_NYLS"~"PONTEILLA",
    lcom=="PORT_VENDRE"~"PORT_VENDRES",
    lcom=="PRATS_DE_MOLLO"~"PRATS_DE_MOLLO_LA_PRESTE",
    lcom=="SAINTT_LAURENT_DE_CERDANS"~"ST_LAURENT_DE_CERDANS",
    lcom=="ST_GENIS_FONTAINES"~"ST_GENIS_DES_FONTAINES",
    lcom=="ST_JEAN_LASEILLE"~"ST_JEAN_LASSEILLE",
    lcom=="ST_JEAN_PLA_CORTS"~"ST_JEAN_PLA_DE_CORTS",
    lcom=="ST_LAURENT_SALQUE"~"ST_LAURENT_DE_LA_SALANQUE",
    lcom=="ST_PAUL_FENOUILLET"~"ST_PAUL_DE_FENOUILLET",
    lcom=="THUES_EN_VALLS"~"THUES_ENTRE_VALLS",
    lcom=="VERNET_LES_BAIN"~"VERNET_LES_BAINS",
    lcom%in%c("VILLELONGUESALANQUE",
              "VILLONGUE_DE_LA_SALANQUE")~"VILLELONGUE_DE_LA_SALANQUE",
    lcom=="VILLENEUVE_DE_LA_RIVIERE"~"VILLENEUVE_LA_RIVIERE",
    T~lcom)
  )
  
  x[which(x$dep=="67"),]=mutate(
    x[which(x$dep=="67"),],
    lcom=case_when(
      lcom=="BISCHEIM"~"BISCHHEIM",
      lcom=="BISCHHOFFSHEIM"~"BISCHOFFSHEIM",
      lcom=="HOEHNHEIM"~"HOENHEIM",
      lcom=="MARCKOSLEHIM"~"MARCKOLSHEIM",
      T~lcom)
  )
  
  x[which(x$dep=="68"),]=mutate(
    x[which(x$dep=="68"),],
    lcom=case_when(
      lcom=="DANEMARIE"~"DANNEMARIE",
      lcom=="HORBOURG_WHIR"~"HORBOURG_WIHR",
      lcom=="SOULTZ"&annee<2026~"SOULTZ_HAUT_RHIN",
      T~lcom)
  )
  
  x[which(x$dep=="69"),]=mutate(x[which(x$dep=="69"),],lcom=case_when(
    lcom=="ALBIGNY"~"ALBIGNY_SUR_SAONE",
    lcom=="ARBRESLE"~"L_ARBRESLE",
    lcom=="BOIS_D_OINGT"~"LE_BOIS_D_OINGT",
    lcom=="CALUIRE"~"CALUIRE_ET_CUIRE",
    lcom=="CHABAGNIERE"~"CHABANIERE",
    lcom=="CHAPONNOST"~"CHAPONOST",
    lcom%in%c("CHRBONNIERES","CHARBONNIERES")~"CHARBONNIERES_LES_BAINS",
    lcom%in%c("CHAZAY","CHAZAY_D_AZERGUE")~"CHAZAY_D_AZERGUES",
    lcom=="DECINES"~"DECINES_CHARPIEU",
    lcom=="FLEURIEU_SUR_L_ARBRESLE"~"FLEURIEUX_SUR_L_ARBRESLE",
    lcom=="FONTAINE_ST_MARTIN"~"FONTAINES_ST_MARTIN",
    lcom=="FONTAINE_SUR_SAONE"~"FONTAINES_SUR_SAONE",
    lcom=="GLEYZE"~"GLEIZE",
    lcom=="POMMIER"~"POMMIERS",
    lcom=="RILLEUX_LA_PAPE"~"RILLIEUX_LA_PAPE",
    lcom=="SAINTYE_FOY_LES_LYON"~"STE_FOY_LES_LYON",
    lcom=="ST_COLOMBE"~"STE_COLOMBE",
    lcom=="ST_CYR_AU_MT_D_OR"~"ST_CYR_AU_MONT_D_OR",
    lcom=="ST_DIDIER_AU_MT_D_OR"~"ST_DIDIER_AU_MONT_D_OR",
    lcom=="ST_DIDIER_SOUS_RIVIERE"~"ST_DIDIER_SOUS_RIVERIE",
    str_detect(lcom,"^ST_ETIENNE_(D|L)ES_O(U|L)LIERES$")~"ST_ETIENNE_DES_OULLIERES",
    lcom=="ST_FOY_L_ARGENTIERE"~"STE_FOY_L_ARGENTIERE",
    lcom=="ST_GERMAIN_EN_NUELLES"~"ST_GERMAIN_NUELLES",
    lcom=="ST_JEAN_D_ARDIERE"~"ST_JEAN_D_ARDIERES",
    lcom=="ST_SYMPHORIEN_COISE"~"ST_SYMPHORIEN_SUR_COISE",
    lcom=="TOUR_DE_SALVAGNY"~"LA_TOUR_DE_SALVAGNY",
    T~str_replace_all(lcom,"CHATILLON_D_AZERGUES?","CHATILLON"))
  )
  
  x[which(x$dep=="71"),]=mutate(
    x[which(x$dep=="71"),],
    lcom=case_when(
      lcom=="CR^CHES_SUR_SAONE"~"CRECHES_SUR_SAONE",
      lcom=="SENS"~"SENS_SUR_SEILLE",
      lcom=="VENEDENESSE_SUR_ARROUX"~"VENDENESSE_SUR_ARROUX",
      T~lcom)
  )
  
  x[which(x$dep=="72"),]=mutate(x[which(x$dep=="72"),],lcom=case_when(
    lcom=="MONTVAL_SUR_LOIR_(EX_CHATEAU_DU_LOIR)"~"MONTVAL_SUR_LOIR",
    lcom=="SOLESME"~"SOLESMES",
    T~lcom)
  )
  
  x[which(x$dep=="73"),]=mutate(
    x[which(x$dep=="73"),],
    lcom=case_when(
      lcom=="BONNEVAL_TARENTAISE"~"BONNEVAL",
      lcom=="BOURG_ST_MAURICE_LES_ARCS"~"BOURG_ST_MAURICE",
      lcom=="BRIDES_DES_BAINS"~"BRIDES_LES_BAINS",
      lcom=="CHINDIREUX"~"CHINDRIEUX",
      lcom=="FONCOUVERTE_LA_TOUSSUIRE"~"FONTCOUVERTE_LA_TOUSSUIRE",
      lcom=="LES_AVALANCHERS_VALMOREL"~"LES_AVANCHERS_VALMOREL",
      lcom%in%c("LE_FRENEY","LE_PLANAY")~str_remove(lcom,"^LE_"),
      str_detect(lcom,"^(LE_)?PONT_(D|L)E_B(EAU|ON)VOISIN$")~"LE_PONT_DE_BEAUVOISIN",
      lcom=="MACOT_LE_PLAGNE"~"MACOT_LA_PLAGNE",
      lcom=="MONTVALEZAN_LA_ROSIERE"~"MONTVALEZAN",
      lcom=="PEYSEY_NANCROIX"~"PEISEY_NANCROIX",
      lcom=="PRALOGNAN"~"PRALOGNAN_LA_VANOISE",
      lcom=="ST_FOY_TARENTAISE"~"STE_FOY_TARENTAISE",
      str_detect(lcom,"^ST_JULIEN_MONT?_?DENIS$")~"ST_JULIEN_MONT_DENIS",
      lcom=="ST_MARTIN_LA_PORTE"~"ST_MARTIN_DE_LA_PORTE",
      lcom=="ST_PIERRE_DALBIGNY"~"ST_PIERRE_D_ALBIGNY",
      lcom=="ST_SORLIN_DARVES"~"ST_SORLIN_D_ARVES",
      lcom=="VAL_DISERE"~"VAL_D_ISERE",
      lcom=="VAL_GELON_LA_ROCHETTE"~"VALGELON_LA_ROCHETTE",
      lcom%in%c("VILLARAMBERT","VILLAREMBERT_LE_CORBIER")~"VILLAREMBERT",
      lcom=="VILLARONDIN_BOURGET"~"VILLARODIN_BOURGET",
      T~lcom)
  )
  
  x[which(x$dep=="74"),]=mutate(x[which(x$dep=="74"),],lcom=case_when(
    lcom=="ARACHES"~"ARACHES_LA_FRASSE",
    lcom=="CHAMONIX_MT_BLANC"~"CHAMONIX_MONT_BLANC",
    lcom=="CONTAMINES_MONTJOIE"~"LES_CONTAMINES_MONTJOIE",
    lcom=="CONTAMINSE_SUR_ARVE"~"CONTAMINE_SUR_ARVE",
    lcom=="FAVERGES/SEYTHENEX"~"FAVERGES_SEYTHENEX",
    lcom=="MORZINE_AVORIAZ"~"MORZINE",
    lcom%in%c("REIGNIER","REIGNIER_ESSERY")~"REIGNIER_ESERY",
    lcom=="RUMILLY_74"~"RUMILLY",
    lcom=="SCIEZ_SUR_LEMAN"~"SCIEZ",
    lcom=="ST_GINGOLF"~"ST_GINGOLPH",
    lcom=="VILLE_LE_GRAND"~"VILLE_LA_GRAND",
    lcom=="VUIZ_EN_SALLAZ"~"VIUZ_EN_SALLAZ",
    T~lcom)
  )
  
  x=filter(x,!(dep=="75"&annee<2026&!str_detect(lcom,"PARIS")))
  x[which(x$dep=="75"&x$annee<2026),"lcom"]="PARIS"
  
  x[which(x$dep=="76"),]=mutate(
    x[which(x$dep=="76"),],
    lcom=case_when(
      lcom=="AMFREVILLE_LA_MIVOIE"~"AMFREVILLE_LA_MI_VOIE",
      lcom=="BIHOREL/BOIS_GUILLAUME"~"BOIS_GUILLAUME_BIHOREL",
      lcom=="CAMPNEUVILLE"~"CAMPNEUSEVILLE",
      lcom=="CRIQUETOT_SUR_L_ESNEVAL"~"CRIQUETOT_L_ESNEVAL",
      lcom=="ELBEUF_SUR_SEINE"~"ELBEUF",
      lcom=="GRANDCAMP"~"GRAND_CAMP",
      #Cf Notes d'Attribution relatives au Trait et à Yainville
      lcom=="MAIRIE"&annee==2022~"LE_TRAIT",
      lcom=="PETITVILLE"~"PETIVILLE",
      lcom=="PETIT_QUEVILLY"~"LE_PETIT_QUEVILLY",
      lcom=="ROUSMESNIL_BOUTEILLE"~"ROUXMESNIL_BOUTEILLES",
      T~lcom)
  )
  
  x[which(x$dep=="77"),]=mutate(x[which(x$dep=="77"),],lcom=case_when(
    lcom=="ACHERES_LE_FORET"~"ACHERES_LA_FORET",
    lcom=="CHATELET_EN_BRIE"~"LE_CHATELET_EN_BRIE",
    lcom=="EVRY_GREGY_SUR_YERRES"~"EVRY_GREGY_SUR_YERRE",
    lcom=="LAROCHETTE"~"LA_ROCHETTE",
    lcom=="MAISON_ROUGE_EN_BRIE"~"MAISON_ROUGE",
    lcom=="VILLENNOY"~"VILLENOY",
    T~lcom)
  )
  
  x[which(x$dep=="78"),]=mutate(
    x[which(x$dep=="78"),],
    lcom=str_replace_all(lcom,"MONVOISIN","MAUVOISIN"),
    lcom=case_when(
      lcom=="ALLAINVILLE_AUX_BOIS"~"ALLAINVILLE",
      lcom=="ALLUETS_LE_ROI"~"LES_ALLUETS_LE_ROI",
      lcom=="AUFFREVILLE_BRASSEUL"~"AUFFREVILLE_BRASSEUIL",
      lcom=="AUTEUIL_LE_ROI"~"AUTEUIL",
      lcom=="CHAUFFOUR_LES_BONNIERES"~"CHAUFOUR_LES_BONNIERES",
      lcom=="CIRY_LA_FORET"~"CIVRY_LA_FORET",
      lcom=="CLAIREFONTAINE"~"CLAIREFONTAINE_EN_YVELINES",
      lcom=="CLAYES_SOUS_BOIS"~"LES_CLAYES_SOUS_BOIS",
      lcom=="CONDE_SUR_VESGRES"~"CONDE_SUR_VESGRE",
      lcom=="GAMBAISEUL"~"GAMBAISEUIL",
      lcom=="GOUPILLERES"~"GOUPILLIERES",
      lcom=="GRESSAY"~"GRESSEY",
      lcom=="JEUFFOSSE"~"JEUFOSSE",
      lcom=="LAINVILLE"~"LAINVILLE_EN_VEXIN",
      lcom=="LA_HAUTE_VILLE"~"LA_HAUTEVILLE",
      lcom=="LA_QUEUE_LEZ_YVELINES"~"LA_QUEUE_LES_YVELINES",
      str_detect(lcom,"^LE_PRUNAY_EN_YE?VELINES$")~"PRUNAY_EN_YVELINES",
      lcom=="LOMMOYES"~"LOMMOYE",
      lcom=="MAREUIL_SUR_MAULDRE"~"MAREIL_SUR_MAULDRE",
      lcom=="MILLION_LA_CHAPELLE"~"MILON_LA_CHAPELLE",
      lcom=="MORAINVILLIERS_BURES"~"MORAINVILLIERS",
      lcom=="NOINVILLE_SUR_MONTCIENT"~"OINVILLE_SUR_MONTCIENT",
      lcom=="ORGEMONT"~"ORCEMONT",
      lcom=="PERRAY_EN_YVELINES"~"LE_PERRAY_EN_YVELINES",
      lcom=="PORT_VILLIEZ"~"PORT_VILLEZ",
      lcom=="LAINVILLE"~"LAINVILLE_EN_VEXIN",
      lcom=="LOMMOYES"~"LOMMOYE",
      lcom=="RICHEFOURG"~"RICHEBOURG",
      lcom=="SONGCHAMP"~"SONCHAMP",
      lcom=="ST_CYL_L_ECOLE"~"ST_CYR_L_ECOLE",
      lcom=="ST_GERMAIN_EN_LAYE_(FOURQUEUX)"~"ST_GERMAIN_EN_LAYE",
      lcom=="ST_HILLARION"~"ST_HILARION",
      lcom=="ST_ILLERS_LA_VILLE"~"ST_ILLIERS_LA_VILLE",
      lcom=="ST_ILLERS_LES_BOIS"~"ST_ILLIERS_LE_BOIS",
      lcom=="ST_LAMBERT_DES_BOIS"~"ST_LAMBERT",
      lcom=="ST_MARTIN_BRETHENCOURT"~"ST_MARTIN_DE_BRETHENCOURT",
      lcom=="ST_MARTIN_ES_CHAMPS"~"ST_MARTIN_DES_CHAMPS",
      lcom=="TESSANCOUR_SUR_AUBETTE"~"TESSANCOURT_SUR_AUBETTE",
      lcom=="TREMBLAY_SUR_MAULDRE"~"LE_TREMBLAY_SUR_MAULDRE",
      T~str_replace(lcom,"VIROLAY","VIROFLAY"))
  )
  
  x[which(x$dep=="79"),]=mutate(
    x[which(x$dep=="79"),],
    lcom=case_when(
      lcom=="ARGENTION_LES_VALLEES"~"ARGENTON_LES_VALLEES",
      lcom=="LA_MOTTE_ST_HERAY"~"LA_MOTHE_ST_HERAY",
      lcom=="MAUZAY_LE_MIGNON"~"MAUZE_SUR_LE_MIGNON",
      lcom=="PRAECQ"~"PRAHECQ",
      T~lcom)
  )
  
  x[which(x$dep=="80"),]=mutate(
    x[which(x$dep=="80"),],
    lcom=case_when(
      lcom=="CROTOY"~"LE_CROTOY",
      lcom=="DOINGT_FLAMICOURT"~"DOINGT",
      lcom=="FORT_MAHON"~"FORT_MAHON_PLAGE",
      lcom=="FRESSENVILLE"~"FRESSENNEVILLE",
      lcom=="MOLLIENS_AUX_BOIS"~"MOLLIENS_AU_BOIS",
      lcom=="ROSIERE_EN_SANTERRE"~"ROSIERES_EN_SANTERRE",
      lcom=="ST_VALERY"~"ST_VALERY_SUR_SOMME",
      T~lcom)
  )
  
  x[which(x$dep=="81"),]=mutate(
    x[which(x$dep=="81"),],
    lcom=case_when(
      lcom=="CORDES"~"CORDES_SUR_CIEL",
      lcom=="LACAUNE_LES_BAINS"~"LACAUNE",
      lcom%in%c("VILLEFRANCE_D_ALBIGEOIS",
                "VILLEFRANCHE_D_ALBI")~"VILLEFRANCHE_D_ALBIGEOIS",
      T~lcom)
  )
  
  x[which(x$dep=="82"),]=mutate(
    x[which(x$dep=="82"),],
    lcom=case_when(
      lcom=="BEAUMONT_D_LOMAGNE"~"BEAUMONT_DE_LOMAGNE",
      lcom=="CAUSSAGE"~"CAUSSADE",
      lcom=="LABASTIDEST_PIERRE"~"LABASTIDE_ST_PIERRE",
      lcom=="MONTACH"~"MONTECH",
      lcom=="NEFREPELISSE"~"NEGREPELISSE",
      lcom%in%c("ST_ANTONIN_DE_NOBLE_VAL",
                "ST_ANTONUIN_NOBLE_VAL")~"ST_ANTONIN_NOBLE_VAL",
      lcom=="ST_ETIENNE"~"ST_ETIENNE_DE_TULMONT",
      lcom=="VALENCE_D_AGEN"~"VALENCE",
      T~lcom)
  )
  
  x[which(x$dep=="83"),]=mutate(
    x[which(x$dep=="83"),],
    lcom=case_when(
      lcom%in%c("LAVANDOU","MUY","PLAN_DE_LA_TOUR","PRADET","THORONET","VAL")~paste("LE_",lcom,sep=""),
      lcom%in%c("FARLEDE","GARDE","GARDE_FREINET","MARTRE","MOLE","MOTTE",
                "ROQUEBRUSSANNE","SEYNE_SUR_MER","VALETTE_DU_VAR","VERDIERE")~paste("LA_",lcom,sep=""),
      lcom%in%c("SALLES_SUR_VERDON")~paste("LES_",lcom,sep=""),
      lcom%in%c("CAVALAIRE","SANARY","ST_CYR","ST_MANDRIER")~paste(lcom,"_SUR_MER",sep=""),
      lcom%in%c("FLASSANS")~paste(lcom,"_SUR_ISSOLE",sep=""),
      lcom%in%c("LA_VALETTE","PIERREFEU","ST_ANTONIN")~paste(lcom,"_DU_VAR",sep=""),
      str_detect(lcom,"^(LA_)?BAUDINARD$")~"BAUDINARD_SUR_VERDON",
      lcom=="ARTIGNOSC/_VERDON"~"ARTIGNOSC_SUR_VERDON",
      lcom=="BESSE_/_ISSOLE"~"BESSE_SUR_ISSOLE",
      lcom=="CAMPS_LA_SCE"~"CAMPS_LA_SOURCE",
      lcom=="COLOBRIERES"~"COLLOBRIERES",
      lcom=="COMPS_/ARTUBY"~"COMPS_SUR_ARTUBY",
      lcom%in%c("LA_LONDE","LONDE_LES_MAURES")~"LA_LONDE_LES_MAURES",
      lcom=="LA_SEYNE_S_SUR_MER"~"LA_SEYNE_SUR_MER",
      str_detect(lcom,"^LES_ARCS_?(/|SUR)?_?ARGENS$")~"LES_ARCS",
      lcom=="LES_CANNET_DES_MAURES"~"LE_CANNET_DES_MAURES",
      lcom=="LES_SALLES_/VERDON"~"LES_SALLES_SUR_VERDON",
      lcom%in%c("LE_LUC_EN_PROVENCE","LUC")~"LE_LUC",
      str_detect(lcom,"^(LE_)?RAYOL_CANADEL(_SUR_MER)?$")~"RAYOL_CANADEL_SUR_MER",
      lcom=="MEOUNES"~"MEOUNES_LES_MONTRIEUX",
      lcom=="MOISSAC"~"MOISSAC_BELLEVUE",
      str_detect(lcom,"^MONTFORT_?/?_?ARGENS$")~"MONTFORT_SUR_ARGENS",
      lcom%in%c("PLAN_D_AUPS","PLAN_D_AUPS_LA_STE_BAUME")~"PLAN_D_AUPS_STE_BAUME",
      lcom=="ROQUE_ESCAPOY"~"LA_ROQUE_ESCLAPON",
      lcom%in%c("SEILLONS","SEILLONS_SCE_D_ARGENS")~"SEILLONS_SOURCE_D_ARGENS",
      lcom=="SIX_FOURS"~"SIX_FOURS_LES_PLAGES",
      str_detect(lcom,"^STE_ANASTASIE_?(/?_?ISSOLE)?$")~"STE_ANASTASIE_SUR_ISSOLE",
      lcom=="ST_JULIEN_MONTAGNIER"~"ST_JULIEN",
      lcom=="ST_MAXIMIN"~"ST_MAXIMIN_LA_STE_BAUME",
      lcom=="TRANS_EN_PCE"~"TRANS_EN_PROVENCE",
      lcom=="VINON_/VERDON"~"VINON_SUR_VERDON",
      lcom%in%c("VINS_/CARAMY","VINS_SUR_CARAMI")~"VINS_SUR_CARAMY",
      T~lcom)
  )
  
  x[which(x$dep=="84"),]=mutate(
    x[which(x$dep=="84"),],
    lcom=case_when(
      lcom=="ALTHEN_LES_PALUDS"~"ALTHEN_DES_PALUDS",
      lcom=="CAMARET"~"CAMARET_SUR_AIGUES",
      lcom=="CHATEAUNEUF_DE_GAGAGNE"~"CHATEAUNEUF_DE_GADAGNE",
      lcom=="ISLE_SUR_LA_SORGUE"~"L_ISLE_SUR_LA_SORGUE",
      lcom=="LES_TAILLADES"~"TAILLADES",
      lcom=="ROUSSILON"~"ROUSSILLON",
      lcom=="VILLE_SUR_AUZON"~"VILLES_SUR_AUZON",
      T~lcom)
  )
  
  x[which(x$dep=="85"),]=mutate(
    x[which(x$dep=="85"),],
    lcom=case_when(
      lcom=="LE_CHATEAU_D_OLONNE"~"CHATEAU_D_OLONNE",
      lcom=="L_AIGUILLON_SUR_MER_LA_FAUTE_SUR_MER"~"L_AIGUILLON_SUR_MER",
      lcom=="MPNTAIGU"~"MONTAIGU",
      T~lcom)
  )
  
  x[which(x$dep=="88"),]=mutate(
    x[which(x$dep=="88"),],
    lcom=case_when(
      lcom=="CAP_AVENIR_VOSGES"~"CAPAVENIR_VOSGES",
      lcom=="CONTREVILLE"~"CONTREXEVILLE",
      lcom=="ETIVAL_CLAREFONTAINE"~"ETIVAL_CLAIREFONTAINE",
      lcom=="RAON_LETAPE"~"RAON_L_ETAPE",
      lcom=="THAON_LES_VOSGES_(ANCIEN_CAPAVENIR_VOSGES)"~"THAON_LES_VOSGES",
      lcom=="VAL_D_AJOL"~"LE_VAL_D_AJOL",
      lcom=="XONRUPT_LONGERMER"~"XONRUPT_LONGEMER",
      T~lcom)
  )
  
  x[which(x$dep=="89"),]=mutate(
    x[which(x$dep=="89"),],
    lcom=case_when(
      lcom=="BEINES"~"BEINE",
      lcom=="ST_GEORGES_SUR_BAULCHES"~"ST_GEORGES_SUR_BAULCHE",
      T~lcom)
  )
  
  x[which(x$dep=="90"),]=mutate(
    x[which(x$dep=="90"),],
    lcom=case_when(
      lcom=="ST_DIZIER"~"ST_DIZIER_L_EVEQUE",
      T~lcom)
  )
  
  x[which(x$dep=="91"),]=mutate(
    x[which(x$dep=="91"),],
    lcom=case_when(
      lcom%in%c("BALLANCOURT","GIRONVILLE")~paste(lcom,"_SUR_ESSONNE",sep=""),
      lcom=="COUDRAY_MONTCEAUX"~"LE_COUDRAY_MONTCEAUX",
      lcom=="FORGE_LES_BAINS"~"FORGES_LES_BAINS",
      lcom=="MONTHLERY"~"MONTLHERY",
      lcom=="SAINTRY"~"SAINTRY_SUR_SEINE",
      T~lcom)
  )
  
  x[which(x$dep=="92"),]=mutate(
    x[which(x$dep=="92"),],
    lcom=case_when(
      lcom=="CLICHY_LA_GARENNE"~"CLICHY",
      lcom=="LA_GARENNES_COLOMBES"~"LA_GARENNE_COLOMBES",
      T~str_replace_all(lcom,"_?/_?SEINE","_SUR_SEINE"))
  )
  
  x[which(x$dep=="93"),]=mutate(
    x[which(x$dep=="93"),],
    lcom=case_when(
      lcom%in%c("BLANC_MESNIL","BOURGET","PRE_ST_GERVAIS","RAINCY")~paste("LE_",lcom,sep=""),
      lcom%in%c("COURNEUVE")~paste("LA_",lcom,sep=""),
      lcom%in%c("LILAS","PAVILLONS_SOUS_BOIS")~paste("LES_",lcom,sep=""),
      lcom%in%c("PIERREFITTE")~paste(lcom,"_SUR_SEINE",sep=""),
      lcom=="GOURNAY"~"GOURNAY_SUR_MARNE",
      lcom=="ILE_ST_DENIS"~"L_ILE_ST_DENIS",
      lcom=="TREMBLAY"~"TREMBLAY_EN_FRANCE",
      T~lcom)
  )
  
  x[which(x$dep=="94"),]=mutate(
    x[which(x$dep=="94"),],
    lcom=case_when(
      lcom%in%c("KREMLIN_BICETRE")~paste("LE_",lcom,sep=""),
      lcom%in%c("IVRY")~paste(lcom,"_SUR_SEINE",sep=""),
      lcom%in%c("CHENNEVIERES","ORMESSON")~paste(lcom,"_SUR_MARNE",sep=""),
      lcom=="CHARENTON"~"CHARENTON_LE_PONT",
      lcom=="LA_QUEUE_EN_BRYE"~"LA_QUEUE_EN_BRIE",
      lcom=="LE_PERRREUX_SUR_MARNE"~"LE_PERREUX_SUR_MARNE",
      lcom=="LHAY_LES_ROSES"~"L_HAY_LES_ROSES",
      lcom=="PERIGNY_SUR_YERRES"~"PERIGNY",
      T~str_replace_all(lcom,"VILLEUNEUVE","VILLENEUVE"))
  )
  
  x[which(x$dep=="95"),]=mutate(
    x[which(x$dep=="95"),],
    lcom=case_when(
      lcom%in%c("FRETTE_SUR_SEINE")~paste("LA_",lcom,sep=""),
      lcom=="ASNIERE_SUR_OISE"~"ASNIERES_SUR_OISE",
      lcom=="BEAUCHAMPS"~"BEAUCHAMP",
      lcom=="BERNES"~"BERNES_SUR_OISE",
      str_detect(lcom,"^BR?UYERES_?(/|SUR)_?OISE$")~"BRUYERES_SUR_OISE",
      lcom=="MARINE"~"MARINES",
      lcom=="MONSOULT"~"MONTSOULT",
      str_detect(lcom,"^SOISS?Y_S(OUS|UR)_MONTMORENCY$")~"SOISY_SOUS_MONTMORENCY",
      lcom=="ST_MARTIN_DU_TRERTRE"~"ST_MARTIN_DU_TERTRE",
      lcom=="VAUD_HERLAND"~"VAUDHERLAND",
      T~str_remove(lcom,"_\\(?PM_LOCALE\\)?$"))
  )
  
  x[which(x$dep=="971"),]=mutate(
    x[which(x$dep=="971"),],
    lcom=case_when(
      lcom=="ABYMES"~"LES_ABYMES",
      lcom=="CAPESTERRE_MARIE_GALANTE"~"CAPESTERRE_DE_MARIE_GALANTE",
      T~lcom)
  )
  
  x[which(x$dep=="972"),]=mutate(
    x[which(x$dep=="972"),],
    lcom=case_when(
      lcom=="ANSE_D_ARLET"~"LES_ANSES_D_ARLET",
      lcom%in%c("GROS_MORNE_LE)","LE_GROS_MORNE")~"GROS_MORNE",
      lcom=="LE_LE_DIAMANT"~"LE_DIAMANT",
      lcom=="STE_ESPRIT"~"ST_ESPRIT",
      lcom=="ST_ANNE"~"STE_ANNE",
      T~lcom)
  )
  
  x[which(x$dep=="973"),]=mutate(
    x[which(x$dep=="973"),],
    lcom=case_when(
      lcom=="MONSINERY_TONNEGRANDE"~"MONTSINERY_TONNEGRANDE",
      T~lcom)
  )
  
  x[which(x$dep=="974"),]=mutate(
    x[which(x$dep=="974"),],
    lcom=case_when(
      lcom%in%c("AVIRONS","TROIS_BASSINS")~paste("LES_",lcom,sep=""),
      lcom%in%c("PORT","TAMPON")~paste("LE_",lcom,sep=""),
      lcom%in%c("POSSESSION","PLAINE_DES_PALMISTES")~paste("LA_",lcom,sep=""),
      lcom=="PETIT_ILES"~"PETITE_ILE",
      lcom=="LA_PLAINE_DES_PALMISTE"~"LA_PLAINE_DES_PALMISTES",
      lcom=="ETANG_SALE"~"L_ETANG_SALE",
      T~lcom)
  )
  
  x[which(x$dep=="976"),]=mutate(
    x[which(x$dep=="976"),],
    lcom=case_when(
      lcom%in%c("MTZAMBORO","M_TZAMBORO")~"MTSAMBORO",
      lcom=="MTSANGAMOUJI"~"M_TSANGAMOUJI",
      lcom=="DZAOUDZI_LABATTOIR"~"DZAOUDZI",
      T~lcom)
  )
  
  x[which(x$dep=="977"),]=mutate(
    x[which(x$dep=="977"),],
    lcom=case_when(
      is.na(lcom)~"ST_BARTHELEMY",
      T~lcom)
  )
  
  x[which(x$dep=="978"),]=mutate(
    x[which(x$dep=="978"),],
    lcom=case_when(
      is.na(lcom)~"ST_MARTIN",
      T~lcom)
  )
  
  x[which(x$dep=="987"),]=mutate(
    x[which(x$dep=="987"),],
    lcom=case_when(
      #Paatio est le chef-lieu de l'île Tahaa
      lcom=="PATIO"~"TAHAA",
      lcom=="PUKA_PUKA"~"PUKAPUKA",
      T~lcom)
  )
  
  x[which(x$dep=="988"),]=mutate(
    x[which(x$dep=="988"),],
    lcom=case_when(
      lcom=="MONT_DORE"~"LE_MONT_DORE",
      T~lcom)
  )
  
  return(x)
  
}
