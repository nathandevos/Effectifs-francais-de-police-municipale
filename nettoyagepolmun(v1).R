library(tidyverse)
library(readxl)
library(readODS)

#NOTE IMPORTANTE SUR LES ANNEES : Dans tout le code, je considère les données 
#(qui sont des données de décembre) comme des données au 1er janvier de l'année 
#suivante, de sorte à les rendre immédiatement comparables à des données 
#démographiques. Quand je me référerai à un problème figurant dans les fichiers 
#originaux, je donnerai la date d'enquête, non la date ici affichée, de sorte 
#que des différences d'un an entre le code et le propos ne sont pas des erreurs
#Ceci a pour conséquence un peu étrange qu'une ancienne commune peut avoir 
#répondu en décembre avant de fusionner le 1er janvier, et être notée comme 
#disposant d'effectifs pendant encore un an alors même qu'elle n'est plus.

#On supposera que les données sont enregistrées non pas dans le dossier de travail,
#mais dans un sous-dossier "Données"

#Première étape automatisée de la correction orthographique. On donne l'option,
#pour dontnombre==0, de ne pas enlever les chiffres. Cela permet de préserver 
#les numéros d'arrondissement dans les libellés parisiens, marseillais et lyonnais
standardiserlibelles<-function(x,dontnombre){
  
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
    #Attention, potentiellement, à cette ligne de code, si jamais une police 
    #pluricommunale existait avec une ville commençant par "MER". Tout format 
    #du type "XXXX/MERXXXX" sera converti. Ce faux positif est cependant simple
    #à détecter dans la phase de correction "manuelle"
    str_replace_all("(_S)?/MER","_SUR_MER_") %>% 
    str_replace_all("_S/","_SUR_") %>% 
    str_replace_all("_*\\([a-zA-Z]?\\)_*|\\'+|\\’+|\\‘+|–+|-+|\\s+","_") %>% 
    str_replace_all("_+","_") %>% 
    str_remove_all("_$|^_") %>% 
    str_replace_all("_SAINT","_ST") %>% 
    str_replace_all("SAINT_","ST_") %>% 
    str_replace_all("_SAINTE","_STE") %>% 
    str_replace_all("SAINTE_","STE_") %>% 
    str_replace("^REUNION$","LA_REUNION") %>% 
    str_remove("^TOTAL$") %>% 
    na_if('')
  
  return(x)
}

standardisationcomplementaire=function(x){
  x=case_when(
    str_detect(x,"_LA$|\\(_*LA_*\\)")~paste("LA_",x,sep=""),
    str_detect(x,"_LE$|\\(_*LE_*\\)")~paste("LE_",x,sep=""),
    str_detect(x,"_LES$|\\(_*LES_*\\)")~paste("LES_",x,sep=""),
    str_detect(x,"_L$|\\(_*L_*\\)")~paste("L_",x,sep=""),
    T~x
  ) %>% 
    str_replace_all("_LA?E?S?$|\\(_*LA_*\\)|\\(_*LE_*\\)|\\(_*LES_*\\)|_\\(_*L_*\\)","_") %>% 
    str_replace_all("((^|_)SANT($|_))|((^|_)SAIN($|_))|SAIINT","_SAINT_") %>% 
    str_replace_all("_SAINT_","_ST_") %>% 
    str_replace_all("_+","_") %>% 
    str_remove_all("_$|^_") %>% 
    na_if('')
  
  return(x)
}




#PROCEDURE DE DIAGNOSTIC DES FAUTES D'ORTHOGRAPHE

#LA CORRECTION N'EST PAS ACHEVEE POUR TOUS LES DEPARTEMENTS (voir fonction)

#A n'utiliser qu'après avoir constitué le fichier "polmun" plus bas
depchoisi="01"

#Indique les libellés qui ne correspondent ni à une commune existante de ce 
#département, ni à une commune ancienne ayant fusionné depuis. On recommandera 
#de procéder au rattachement des effectifs des communes anciennes aux communes 
#nouvelles, ainsi qu'à toute transformation sur les communautés de communes 
#ou notations de convention, PLUS TARD dans le traitement. Ne serait-ce que 
#parce qu'en cas de changement d'avis sur le traitement d'une fusion ou interco,
#on sera bien en peine de retrouver ce choix parmi les corrections orthographiques
#Réactiver la ligne "correctionortho" permet de vérifier que les corrections
#ont été réalisées correctement.
polmun %>% 
  #  correctionsortho() %>% 
  filter(dep==depchoisi) %>% select(lcom) %>% pull() %>% 
  setdiff(c(
    (classificationcommunes %>% filter(dep==depchoisi))$lcom,
    (communesnouvelles %>% filter(dep==depchoisi))$excommune
  ))

polmun %>% filter(dep==depchoisi) %>% arrange(lcom) %>% View()

#Fonction listant les corrections orthographiques détectées manuellement.
#La procédure de diagnostic ci-haut permet de l'enrichir.
correctionsortho=function(x){
  
  #Il arrive en 2015 que le libellé de département soit placé un peu trop bas dans 
  #les fichiers originaux, de sorte que dans certains cas les premières 
  #communes par ordre alphabétique doivent être reclassées comme appartenant 
  #au département suivant
  x[which(x$annee=="2016"),]=mutate(x[which(x$annee=="2016"),],dep=case_when(
    lcom%in%c("CHARLEVILLE_MEZIERES","FOIX","ARCIS_SUR_AUBE",
              "ALZONNE","BELMONT_SUR_RANCE","AIX_EN_PROVENCE","ARGENCES",
              "AURILLAC","ANGOULEME","ANTONY","AIGREFEUILLE_D_AUNIS",
              "AUBIGNY_SUR_NERE")~str_pad(as.numeric(dep)+1,2,pad="0"),
    T~dep))
  
  #Département 06 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="06"),]=mutate(x[which(x$dep=="06"),],lcom=case_when(
    str_detect(lcom,"^ANTIBES.*")~"ANTIBES",
    str_detect(lcom,"BLAUSS?AS?C*")~"BLAUSASC",
    lcom=="BAR_SUR_LOUP"~"LE_BAR_SUR_LOUP",
    lcom=="CANNET"~"LE_CANNET",
    str_detect(lcom,"^CHATEAUNEUF.*")~"CHATEAUNEUF_GRASSE",
    lcom=="ROQUETTE_SUR_SIAGNE"~"LA_ROQUETTE_SUR_SIAGNE",
    lcom=="MANDELIEU"~"MANDELIEU_LA_NAPOULE",
    str_detect(lcom,"^ST_LAURENT.*")~"ST_LAURENT_DU_VAR",
    str_detect(lcom,"TOURETTE_LEVENS")~"TOURRETTE_LEVENS",
    str_detect(lcom,"TOURR?ETTES?_SUR_LOUP")~"TOURRETTES_SUR_LOUP",
    str_detect(lcom,"^VALLAURIS.*")~"VALLAURIS",
    T~lcom))
  
  x[which(x$dep=="11"),]=mutate(x[which(x$dep=="11"),],lcom=case_when(
    lcom=="CARCASONNE"~"CARCASSONNE",
    lcom=="MAIRIE_DE_NARBONNE"~"NARBONNE",
    T~lcom))
  
  #Département 13 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="13"),]=mutate(x[which(x$dep=="13"),],lcom=case_when(
    lcom=="LANCON_DE_PROVENCE"~"LANCON_PROVENCE",
    str_detect(lcom,"^MAS_BLANC.*")~"MAS_BLANC_DES_ALPILLES",
    lcom=="LE_PARADOU"~"PARADOU",
    lcom=="LES_STES_MARIES_DE_LA_MER"~"SAINTES_MARIES_DE_LA_MER",
    lcom=="ST_PAUL_LEZ_DURANCE"~"ST_PAUL_LES_DURANCE",
    str_detect(lcom,"^VITROLLES.*")~"VITROLLES",
    T~lcom))
  
  x[which(x$dep=="19"),]=mutate(x[which(x$dep=="19"),],lcom=case_when(
    lcom=="BRIVE"~"BRIVE_LA_GAILLARDE",
    T~lcom))
  
  #Département 22 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="22"),]=mutate(x[which(x$dep=="22"),],lcom=case_when(
    lcom=="BINIC_ETAPLE_SUR_MER"~"BINIC_ETABLES_SUR_MER",
    lcom=="BREHAT"~"ILE_DE_BREHAT",
    lcom=="ETABLES_S_SUR_MER"~"ETABLES_SUR_MER",
    lcom=="FEHEL"~"FREHEL",
    lcom=="LANNIION"~"LANNION",
    T~lcom))
  
  x[which(x$dep=="26"),]=mutate(x[which(x$dep=="26"),],lcom=case_when(
    lcom=="ROMANS"~"ROMANS_SUR_ISERE",
    T~lcom))
  
  #Département 29 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="29"),]=mutate(x[which(x$dep=="29"),],lcom=case_when(
    lcom=="BRIEC_DE_L_ODET"~"BRIEC",
    lcom=="LE_GUILVINEC"~"GUILVINEC",
    lcom=="PLOBANNALEC"~"PLOBANNALEC_LESCONIL",
    lcom=="PENMARC_H"~"PENMARCH",
    T~lcom))
  
  #Département 31 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="31"),]=mutate(x[which(x$dep=="31"),],lcom=case_when(
    lcom=="RAMONVILLE_ST_AGNES"~"RAMONVILLE_ST_AGNE",
    lcom=="SAINTORENS_DE_GAMEVILLE"~"ST_ORENS_DE_GAMEVILLE",
    str_detect(lcom,"^CAZERES|^GRENADE|^ROQUES")~str_remove_all(lcom,"_SUR_GARONNE"),
    T~lcom))
  
  #Département 33 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="33"),]=mutate(x[which(x$dep=="33"),],lcom=case_when(
    lcom=="AMBARES"~"AMBARES_ET_LAGRAVE",
    lcom=="ARTIGUES"~"ARTIGUES_PRES_BORDEAUX",
    lcom=="BEYCHAC_ET_CAILLEAU"~"BEYCHAC_ET_CAILLAU",
    lcom=="BOURG_SUR_GIRONDE"~"BOURG",
    lcom=="BRAUD"~"BRAUD_ET_ST_LOUIS",
    lcom=="CADILLAC"~"CADILLAC_SUR_GARONNE",
    lcom=="CASTELNAU_MEDOC"~"CASTELNAU_DE_MEDOC",
    lcom=="CASTRES_SUR_GIRONDE"~"CASTRES_GIRONDE",
    lcom=="GAILLAN_MEDOC"~"GAILLAN_EN_MEDOC",
    lcom=="GIRONDE/DROPT"~"GIRONDE_SUR_DROPT",
    lcom=="GRAYAN_L_HOPITAL"~"GRAYAN_ET_L_HOPITAL",
    str_detect(lcom,"LALANDE_(DE_)?FRONSAC")~"LA_LANDE_DE_FRONSAC",
    lcom%in%c("LE_TAILLAN","TAILLAN_MEDOC")~"LE_TAILLAN_MEDOC",
    lcom=="LESPARRE"~"LESPARRE_MEDOC",
    lcom=="ARTIGUES_DE_LUSSAC"~"LES_ARTIGUES_DE_LUSSAC",
    lcom=="LE_VERDON"~"LE_VERDON_SUR_MER",
    lcom=="LISTRAC"~"LISTRAC_MEDOC",
    str_detect(lcom,"^MARTIGNAS")~"MARTIGNAS_SUR_JALLE",
    lcom%in%c("PIAN_SUR_GARONNE","PIAN/GARONNE")~"LE_PIAN_SUR_GARONNE",
    lcom=="SOULAC"~"SOULAC_SUR_MER",
    lcom%in%c("ST_AUBIN_MEDOC","ST_AUBIN_DU_MEDOC")~"ST_AUBIN_DE_MEDOC",
    lcom=="ST_CAPRAIS"~"ST_CAPRAIS_DE_BORDEAUX",
    lcom=="ST_CHRISTOLY"~"ST_CHRISTOLY_DE_BLAYE",
    lcom=="ST_CHRISTOLY_EN_MEDOC"~"ST_CHRISTOLY_MEDOC",
    lcom=="ST_HELENE"~"STE_HELENE",
    lcom=="ST_LAURENT"~"ST_LAURENT_MEDOC",
    lcom=="ST_LOUIS_DE_MONTFERRAN"~"ST_LOUIS_DE_MONTFERRAND",
    lcom=="ST_SULPICE"~"ST_SULPICE_DE_FALEYRENS",
    lcom=="VENDAYS"~"VENDAYS_MONTALIVET",
    lcom=="VAYRE"~"VAYRES",
    T~lcom))
  
  #Département 34 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="34"),]=mutate(x[which(x$dep=="34"),],lcom=case_when(
    lcom=="BOUJAN"~"BOUJAN_SUR_LIBRON",
    lcom=="BOUSQUET_D_ORB"~"LE_BOUSQUET_D_ORB",
    lcom=="CASTENAU_DE_GUERS"~"CASTELNAU_DE_GUERS",
    lcom%in%c("CESSENON","CESSENOM_SUR_ORB")~"CESSENON_SUR_ORB",
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
    T~lcom))
  
  #Département 35 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="35"),]=mutate(x[which(x$dep=="35"),],lcom=case_when(
    lcom=="DOL_DE_BRETANGE"~"DOL_DE_BRETAGNE",
    lcom=="ST_JOUAN_DU_GUERET"~"ST_JOUAN_DES_GUERETS",
    lcom=="ARGENTRE_DU_PESSIS"~"ARGENTRE_DU_PLESSIS",
    lcom=="MONTFROT_SUR_MEU"~"MONTFORT_SUR_MEU",
    lcom=="VALD_IZE"~"VAL_D_IZE",
    T~lcom))
  
  #Département 44 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="44"),]=mutate(x[which(x$dep=="44"),],lcom=case_when(
    str_detect(lcom,"^GRAND?CHAMPS?_DES_FONTAINES$")~"GRANDCHAMP_DES_FONTAINES",
    lcom=="LA_BAULE"~"LA_BAULE_ESCOUBLAC",
    lcom=="LA_BERNERIE"~"LA_BERNERIE_EN_RETZ",
    lcom=="CHAPELLE_BASSE_MER"~"LA_CHAPELLE_BASSE_MER",
    lcom=="LA_HAYE_FOUASSIERE"~"LA_HAIE_FOUASSIERE",
    lcom=="LES_MOUTIERS"~"LES_MOUTIERS_EN_RETZ",
    lcom=="MACHECOUL"~"MACHECOUL_ST_MEME",
    str_detect(lcom,"PONT?_?CHATEAU")~"PONTCHATEAU",
    str_detect(lcom,"^ST_PHILBERT.*")~"ST_PHILBERT_DE_GRAND_LIEU",
    lcom=="THOUARE"~"THOUARE_SUR_LOIRE",
    lcom=="TREILLERES"~"TREILLIERES",
    T~lcom))
  
  #Département 49 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="49"),]=mutate(x[which(x$dep=="49"),],lcom=case_when(
    lcom=="BEAUPREAU"~"BEAUPREAU_EN_MAUGES",
    lcom=="BRISSAC_LOIR_AUBANCE"~"BRISSAC_LOIRE_AUBANCE",
    lcom=="CHALONNES"~"CHALONNES_SUR_LOIRE",
    lcom=="CHEMILLE"~"CHEMILLE_MELAY",
    lcom=="LE_MAY_SUE_EVRE"~"LE_MAY_SUR_EVRE",
    lcom=="MONTREVAULT_SUE_EVRE"~"MONTREVAULT_SUR_EVRE",
    lcom=="SEGREE_EN_ANJOU_BLEU"~"SEGRE_EN_ANJOU_BLEU",
    lcom=="STE_GEORGES_DES_GARDES"~"ST_GEORGES_DES_GARDES",
    lcom=="ST_BARHELEMY_D_ANJOU"~"ST_BARTHELEMY_D_ANJOU",
    lcom=="ST_CHRISTINE"~"STE_CHRISTINE",
    T~lcom))
  
  x[which(x$dep=="51"),]=mutate(x[which(x$dep=="51"),],lcom=case_when(
    lcom=="CHALONS_ENCHAMPAGNE"~"CHALONS_EN_CHAMPAGNE",
    T~lcom))
  
  x[which(x$dep=="52"),]=mutate(x[which(x$dep=="52"),],lcom=case_when(
    lcom=="ST_DIZER"~"ST_DIZIER",
    T~lcom))
  
  x[which(x$dep=="53"),]=mutate(x[which(x$dep=="53"),],lcom=case_when(
    lcom=="VANDOEUVRE"~"VANDOEUVRE_LES_NANCY",
    T~lcom))
  
  #Département 59 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="59"),]=mutate(x[which(x$dep=="59"),],lcom=case_when(
    str_detect(lcom,"AULNOYE?_LE(S|Z)_VALENCIENNES")~"AULNOY_LEZ_VALENCIENNES",
    lcom=="AVESNES_LEZ_AUBERT"~"AVESNES_LES_AUBERT",
    lcom=="BEAUVOUS_EN_CAMBRESIS"~"BEAUVOIS_EN_CAMBRESIS",
    str_detect(lcom,"BRUAY(_SUR_ESCAUT)?")~"BRUAY_SUR_L_ESCAUT",
    str_detect(lcom,"CATEAU(_EN)?_CAMBRESIS")~"LE_CATEAU_CAMBRESIS",
    lcom=="CONDE"~"CONDE_SUR_L_ESCAUT",
    lcom=="COUDERQUE"~"COUDEKERQUE_BRANCHE",
    lcom=="COURCHELETETS"~"COURCHELETTES",
    str_detect(lcom,"^DUNKERQUE_ST_POL$|^FORT_MARDYCK$|^ST_POL(_SUR_MER)?$")~"DUNKERQUE",
    lcom=="ERQUINGHEM"~"ERQUINGHEM_LYS",
    lcom=="FLINES_LES_RACHES"~"FLINES_LEZ_RACHES",
    lcom=="GRANDE_SY_NTHE"~"GRANDE_SYNTHE",
    lcom=="GRAVELINE"~"GRAVELINES",
    lcom%in%c("LILLE_HELLEMMES","LILLE_LOMME","LOMME")~"LILLE",
    lcom=="LYZ_LEZ_LANNOY"~"LYS_LEZ_LANNOY",
    lcom=="OSTRICURT"~"OSTRICOURT",
    lcom=="RAILLENCOURT_ST_OLLE"~"RAILLENCOURT_STE_OLLE",
    lcom=="ST_AMAND"~"ST_AMAND_LES_EAUX",
    str_detect(lcom,"ST_POL(_SUR_MER)?")~"DUNKERQUE",
    lcom=="TEMPLEUVE"~"TEMPLEUVE_EN_PEVELE",
    as.numeric(annee)>2016&lcom=="TETEGHEM"~"TETEGHEM_COUDEKERQUE_VILLAGE",
    lcom=="WALINCOURT_SALVIGNY"~"WALINCOURT_SELVIGNY",
    T~lcom))
  
  #Département 62 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="62"),]=mutate(x[which(x$dep=="62"),],lcom=case_when(
    lcom=="BAILLEUL_SIRE_BERTHOULT"~"BAILLEUL_SIR_BERTHOULT",
    lcom=="BERCK_SUR_MER"~"BERCK",
    lcom=="BOULONE_SUR_MER"~"BOULOGNE_SUR_MER",
    str_detect(lcom,"CALONNE_RIC?Q?UART")~"CALONNE_RICOUART",
    lcom=="CHOQUES"~"CHOCQUES",
    lcom=="ENQUIHEN_PLAGE"~"EQUIHEN_PLAGE",
    lcom=="ETAPLES_SUR_MER"~"ETAPLES",
    lcom=="FOUQUIERES_LE_LENS"~"FOUQUIERES_LES_LENS",
    lcom=="HESDIN"~"HESDIN_LA_FORET",
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
    T~lcom))
  
  x[which(x$dep=="63"),]=mutate(x[which(x$dep=="63"),],lcom=case_when(
    lcom=="COURNON"~"COURNON_D_AUVERGNE",
    lcom=="CLERMONT_FD"~"CLERMONT_FERRAND",
    T~lcom))
  
  x[which(x$dep=="67"),]=mutate(x[which(x$dep=="67"),],lcom=case_when(
    lcom=="BISCHEIM"~"BISCHHEIM",
    T~lcom))
  
  #Département 69 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="69"),]=mutate(x[which(x$dep=="69"),],lcom=case_when(
    lcom=="ALBIGNY"~"ALBIGNY_SUR_SAONE",
    lcom=="ARBRESLE"~"L_ARBRESLE",
    lcom=="BOIS_D_OINGT"~"LE_BOIS_D_OINGT",
    lcom=="CALUIRE"~"CALUIRE_ET_CUIRE",
    lcom=="CHABAGNIERE"~"CHABANIERE",
    lcom=="CHAPONNOST"~"CHAPONOST",
    lcom%in%c("CHRBONNIERES","CHARBONNIERES")~"CHARBONNIERES_LES_BAINS",
    lcom%in%c("CHAZAY","CHAZAY_D_AZERGUE")~"CHAZAY_D_AZERGUES",
    #La fusion de COURS date du 1er janvier 2016, donc affecte l'enquête de fin
    #2016, celle qu'on note ici comme datant de 2017
    as.numeric(annee)>2016&lcom=="COURS_LA_VILLE"~"COURS",
    lcom=="DECINES"~"DECINES_CHARPIEU",
    lcom=="FLEURIEU_SUR_L_ARBRESLE"~"FLEURIEUX_SUR_L_ARBRESLE",
    lcom=="FONTAINE_ST_MARTIN"~"FONTAINES_ST_MARTIN",
    lcom=="FONTAINE_SUR_SAONE"~"FONTAINES_SUR_SAONE",
    lcom=="GLEYZE"~"GLEIZE",
    lcom=="MULATIERE_LA"~"LA_MULATIERE",
    lcom=="RILLEUX_LA_PAPE"~"RILLIEUX_LA_PAPE",
    lcom=="SAINTYE_FOY_LES_LYON"~"STE_FOY_LES_LYON",
    lcom=="ST_COLOMBE"~"STE_COLOMBE",
    lcom=="ST_CYR_AU_MT_D_OR"~"ST_CYR_AU_MONT_D_OR",
    lcom=="ST_DIDIER_AU_MT_D_OR"~"ST_DIDIER_AU_MONT_D_OR",
    lcom=="ST_DIDIER_SOUS_RIVIERE"~"ST_DIDIER_SOUS_RIVERIE",
    str_detect(lcom,"ST_ETIENNE_D?L?ES_OU?LL?IERES")~"ST_ETIENNE_DES_OULLIERES",
    lcom=="ST_FOY_L_ARGENTIERE"~"STE_FOY_L_ARGENTIERE",
    lcom=="ST_GERMAIN_EN_NUELLES"~"ST_GERMAIN_NUELLES",
    lcom=="ST_JEAN_D_ARDIERE"~"ST_JEAN_D_ARDIERES",
    lcom=="ST_SYMPHORIEN_COISE"~"ST_SYMPHORIEN_SUR_COISE",
    str_detect(lcom,"TOUR_DE_SALVAGNY")~"LA_TOUR_DE_SALVAGNY",
    T~str_replace_all(lcom,"CHATILLON_D_AZERGUES?","CHATILLON")))
  
  #Département 74 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="74"),]=mutate(x[which(x$dep=="74"),],lcom=case_when(
    lcom=="ARACHES"~"ARACHES_LA_FRASSE",
    lcom=="CHAMONIX_MT_BLANC"~"CHAMONIX_MONT_BLANC",
    str_detect(lcom,"^CONTAMINES_MONTJOIE")~"LES_CONTAMINES_MONTJOIE",
    lcom=="CONTAMINSE_SUR_ARVE"~"CONTAMINE_SUR_ARVE",
    lcom=="FAVERGES/SEYTHENEX"~"FAVERGES_SEYTHENEX",
    str_detect(lcom,"REIGNIER(_ESSERY)?")~"REIGNIER_ESERY",
    lcom=="SCIEZ_SUR_LEMAN"~"SCIEZ",
    lcom=="ST_GINGOLF"~"ST_GINGOLPH",
    lcom=="VILLE_LE_GRAND"~"VILLE_LA_GRAND",
    lcom=="VUIZ_EN_SALLAZ"~"VIUZ_EN_SALLAZ",
    T~lcom))
  
  #Département 76 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="76"),]=mutate(x[which(x$dep=="76"),],lcom=case_when(
    lcom=="AMFREVILLE_LA_MIVOIE"~"AMFREVILLE_LA_MI_VOIE",
    lcom=="BIHOREL/BOIS_GUILLAUME"~"BOIS_GUILLAUME_BIHOREL",
    lcom=="CAMPNEUVILLE"~"CAMPNEUSEVILLE",
    lcom=="CRIQUETOT_SUR_L_ESNEVAL"~"CRIQUETOT_L_ESNEVAL",
    lcom=="ELBEUF_SUR_SEINE"~"ELBEUF",
    lcom=="GRANDCAMP"~"GRANDCAMP_MAISY",
    lcom=="PETIT_QUEVILLY"~"LE_PETIT_QUEVILLY",
    lcom=="ROUSMESNIL_BOUTEILLE"~"ROUXMESNIL_BOUTEILLES",
    T~str_remove_all(lcom,"^MAIRIE_DE_|^COMMUNE_DE_")))
  
  #Département 77 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="77"),]=mutate(x[which(x$dep=="77"),],lcom=case_when(
    lcom=="ACHERES_LE_FORET"~"ACHERES_LA_FORET",
    lcom=="CHATELET_EN_BRIE"~"LE_CHATELET_EN_BRIE",
    lcom=="LAROCHETTE"~"LA_ROCHETTE",
    lcom=="MAISON_ROUGE_EN_BRIE"~"MAISON_ROUGE",
    lcom=="MONTCOURT_FROMONVILLE"~"MONCOURT_FROMONVILLE",
    lcom=="VILLENNOY"~"VILLENOY",
    T~lcom))
  
  #Département 78 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="78"),]=mutate(x[which(x$dep=="78"),],lcom=case_when(
    lcom=="AUFFREVILLE_BRASSEUL"~"AUFFREVILLE_BRASSEUIL",
    lcom=="CHAUFFOUR_LES_BONNIERES"~"CHAUFOUR_LES_BONNIERES",
    lcom=="CIRY_LA_FORET"~"CIVRY_LA_FORET",
    lcom=="CLAIREFONTAINE"~"CLAIREFONTAINE_EN_YVELINES",
    lcom=="CLAYES_SOUS_BOIS"~"LES_CLAYES_SOUS_BOIS",
    lcom=="CONDE_SUR_VESGRES"~"CONDE_SUR_VESGRE",
    lcom=="GOUPILLERES"~"GOUPILLIERES",
    lcom=="GRESSAY"~"GRESSEY",
    lcom=="JEUFFOSSE"~"JEUFOSSE",
    lcom=="JOUY_MONVOISIN"~"JOUY_MAUVOISIN",
    lcom=="LA_HAUTE_VILLE"~"LA_HAUTEVILLE",
    lcom=="LA_QUEUE_LEZ_YVELINES"~"LA_QUEUE_LES_YVELINES",
    str_detect(lcom,"PRUNAY_EN_Y")~"PRUNAY_EN_YVELINES",
    lcom=="LOMMOYES"~"LOMMOYE",
    lcom=="MAREUIL_SUR_MAULDRE"~"MAREIL_SUR_MAULDRE",
    lcom=="MILLION_LA_CHAPELLE"~"MILON_LA_CHAPELLE",
    lcom=="NOINVILLE_SUR_MONTCIENT"~"OINVILLE_SUR_MONTCIENT",
    lcom=="ORGEMONT"~"ORCEMONT",
    lcom=="PERRAY_EN_YVELINES"~"LE_PERRAY_EN_YVELINES",
    lcom=="PORT_VILLIEZ"~"PORT_VILLEZ",
    lcom=="LAINVILLE"~"LAINVILLE_EN_VEXIN",
    lcom=="LOMMOYES"~"LOMMOYE",
    lcom=="RICHEFOURG"~"RICHEBOURG",
    lcom=="SONGCHAMP"~"SONCHAMP",
    lcom=="ST_CYL_L_ECOLE"~"ST_CYR_L_ECOLE",
    lcom=="ST_HILLARION"~"ST_HILARION",
    lcom=="ST_ILLERS_LA_VILLE"~"ST_ILLIERS_LA_VILLE",
    lcom=="ST_ILLERS_LES_BOIS"~"ST_ILLIERS_LE_BOIS",
    lcom=="ST_LAMBERT_DES_BOIS"~"ST_LAMBERT",
    lcom=="ST_MARTIN_BRETHENCOURT"~"ST_MARTIN_DE_BRETHENCOURT",
    lcom=="ST_MARTIN_ES_CHAMPS"~"ST_MARTIN_DES_CHAMPS",
    lcom=="TESSANCOUR_SUR_AUBETTE"~"TESSANCOURT_SUR_AUBETTE",
    lcom=="TREMBLAY_SUR_MAULDRE"~"LE_TREMBLAY_SUR_MAULDRE",
    lcom=="ALLAINVILLE_AUX_BOIS"~"ALLAINVILLE",
    lcom=="ALLUETS_LE_ROI"~"LES_ALLUETS_LE_ROI",
    lcom=="AUTEUIL_LE_ROI"~"AUTEUIL",
    lcom=="BOISSY_MONVOISIN"~"BOISSY_MAUVOISIN",
    lcom=="FONTENAY_MONVOISIN"~"FONTENAY_MAUVOISIN",
    lcom=="GAMBAISEUL"~"GAMBAISEUIL",
    T~str_replace(lcom,"VIROLAY","VIROFLAY")))
  
  #Département 83 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="83"),]=mutate(x[which(x$dep=="83"),],lcom=case_when(
    lcom=="ARTIGNOSC/_VERDON"~"ARTIGNOSC_SUR_VERDON",
    lcom=="BESSE_/_ISSOLE"~"BESSE_SUR_ISSOLE",
    lcom=="FLASSANS"~"FLASSANS_SUR_ISSOLE",
    str_detect(lcom,"^STE_ANASTASIE/_?ISSOLE")~"STE_ANASTASIE_SUR_ISSOLE",
    lcom=="CAMPS_LA_SCE"~"CAMPS_LA_SOURCE",
    lcom=="COLOBRIERES"~"COLLOBRIERES",
    lcom=="COMPS_/ARTUBY"~"COMPS_SUR_ARTUBY",
    lcom=="GRESSAY"~"GRESSEY",
    lcom=="GARDE_FREINET"~"LA_GARDE_FREINET",
    lcom%in%c("LA_LONDE","LONDE_LES_MAURES")~"LA_LONDE_LES_MAURES",
    lcom=="LA_SEYNE_S_SUR_MER"~"LA_SEYNE_SUR_MER",
    lcom%in%c("LA_VALETTE","PIERREFEU","ST_ANTONIN")~paste(lcom,"_DU_VAR",sep=""),
    lcom%in%c("CAVALAIRE","SANARY")~paste(lcom,"_SUR_MER",sep=""),
    str_detect(lcom,"^LES_ARCS")~"LES_ARCS",
    lcom=="LES_CANNET_DES_MAURES"~"LE_CANNET_DES_MAURES",
    lcom=="LES_SALLES_/VERDON"~"LES_SALLES_SUR_VERDON",
    lcom=="BAUDINARD"~"BAUDINARD_SUR_VERDON",
    lcom%in%c("LE_LUC_EN_PROVENCE","LUC")~"LE_LUC",
    str_detect(lcom,"^(LE_)?RAYOL_CANADEL(_SUR_MER)?")~"RAYOL_CANADEL_SUR_MER",
    lcom%in%c("LAVANDOU","MUY","PLAN_DE_LA_TOUR","PRADET","THORONET",
              "VAL")~paste("LE_",lcom,sep=""),
    lcom%in%c("MARTRE","MOLE","MOTTE","GARDE","ROQUEBRUSSANNE","SEYNE_SUR_MER",
              "VALETTE_DU_VAR","VERDIERE","FARLEDE")~paste("LA_",lcom,sep=""),
    lcom%in%c("SALLES_SUR_VERDON")~paste("LES_",lcom,sep=""),
    lcom=="MEOUNES"~"MEOUNES_LES_MONTRIEUX",
    str_detect(lcom,"MONTFORT/_?ARGENS")~"MONTFORT_SUR_ARGENS",
    str_detect(lcom,"^PLAN_D_AUPS(_LA_STE_BAUME)?")~"PLAN_D_AUPS_STE_BAUME",
    lcom=="ROQUE_ESCAPOY"~"LA_ROQUE_ESCLAPON",
    lcom=="SIX_FOURS"~"SIX_FOURS_LES_PLAGES",
    str_detect(lcom,"^SEILLONS(_SCE_D_ARGENS)?")~"SEILLONS_SOURCE_D_ARGENS",
    lcom=="ST_JULIEN_MONTAGNIER"~"ST_JULIEN",
    lcom=="ST_MANDRIER"~"ST_MANDRIER_SUR_MER",
    lcom=="TRANS_EN_PCE"~"TRANS_EN_PROVENCE",
    lcom=="VINON_/VERDON"~"VINON_SUR_VERDON",
    lcom=="LA_BAUDINARD"~"BAUDINARD_SUR_VERDON",
    lcom%in%c("VINS_/CARAMY","VINS_SUR_CARAMI")~"VINS_SUR_CARAMY",
    T~lcom))
  
  #Département 84 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="84"),]=mutate(x[which(x$dep=="84"),],lcom=case_when(
    lcom=="ALTHEN_LES_PALUDS"~"ALTHEN_DES_PALUDS",
    lcom=="CAMARET"~"CAMARET_SUR_AIGUES",
    lcom=="ISLE_SUR_LA_SORGUE"~"L_ISLE_SUR_LA_SORGUE",
    lcom=="VILLE_SUR_AUZON"~"VILLES_SUR_AUZON",
    lcom=="CHATEAUNEUF_DE_GAGAGNE"~"CHATEAUNEUF_DE_GADAGNE",
    lcom=="ROUSSILON"~"ROUSSILLON",
    lcom=="LES_TAILLADES"~"TAILLADES",
    T~lcom))
  
  x[which(x$dep=="85"),]=mutate(x[which(x$dep=="85"),],lcom=case_when(
    lcom=="MPNTAIGU"~"MONTAIGU",
    T~lcom))
  
  x[which(x$dep=="90"),]=mutate(x[which(x$dep=="90"),],lcom=case_when(
    T~str_remove(lcom,"VILLE_DE_|COMMUNE_DE_")))
  
  #Département 91 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="91"),]=mutate(x[which(x$dep=="91"),],lcom=case_when(
    lcom%in%c("BALLANCOURT","GIRONVILLE")~paste(lcom,"_SUR_ESSONNE",sep=""),
    lcom=="FORGE_LES_BAINS"~"FORGES_LES_BAINS",
    lcom=="SAINTRY"~"SAINTRY_SUR_SEINE",
    lcom=="MONTHLERY"~"MONTLHERY",
    lcom=="COUDRAY_MONTCEAUX"~"LE_COUDRAY_MONTCEAUX",
    T~lcom))
  
  #Département 92 déjà entièrement traité
  x[which(x$dep=="92"),]=mutate(x[which(x$dep=="92"),],lcom=case_when(
    lcom=="CLICHY_LA_GARENNE"~"CLICHY",
    lcom=="LA_GARENNES_COLOMBES"~"LA_GARENNE_COLOMBES",
    T~str_replace_all(lcom,"/","_SUR_")))
  
  #Département 93 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="93"),]=mutate(x[which(x$dep=="93"),],lcom=case_when(
    lcom%in%c("BLANC_MESNIL","BOURGET","PRE_ST_GERVAIS","RAINCY")~paste("LE_",lcom,sep=""),
    lcom%in%c("COURNEUVE")~paste("LA_",lcom,sep=""),
    lcom%in%c("LILAS","PAVILLONS_SOUS_BOIS")~paste("LES_",lcom,sep=""),
    lcom%in%c("PIERREFITTE","ST_OUEN")~paste(lcom,"_SUR_SEINE",sep=""),
    lcom=="GOURNAY"~"GOURNAY_SUR_MARNE",
    lcom=="TREMBLAY"~"TREMBLAY_EN_FRANCE",
    T~lcom))
  
  #Département 94 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="94"),]=mutate(x[which(x$dep=="94"),],lcom=case_when(
    lcom%in%c("KREMLIN_BICETRE")~paste("LE_",lcom,sep=""),
    lcom%in%c("IVRY")~paste(lcom,"_SUR_SEINE",sep=""),
    lcom%in%c("CHENNEVIERES","ORMESSON")~paste(lcom,"_SUR_MARNE",sep=""),
    lcom=="CHARENTON"~"CHARENTON_LE_PONT",
    lcom=="LA_QUEUE_EN_BRYE"~"LA_QUEUE_EN_BRIE",
    lcom=="LE_PERRREUX_SUR_MARNE"~"LE_PERREUX_SUR_MARNE",
    lcom=="LHAY_LES_ROSES"~"L_HAY_LES_ROSES",
    lcom=="PERIGNY_SUR_YERRES"~"PERIGNY",
    T~str_replace(lcom,"VILLEUNEUVE","VILLENEUVE")))
  
  #Département 95 déjà entièrement traité, hors communautés de communes
  #et polices pluricommunales
  x[which(x$dep=="95"),]=mutate(x[which(x$dep=="95"),],lcom=case_when(
    lcom%in%c("FRETTE_SUR_SEINE")~paste("LA_",lcom,sep=""),
    lcom%in%c("HERBLAY")~paste(lcom,"_SUR_SEINE",sep=""),
    lcom=="ASNIERE_SUR_OISE"~"ASNIERES_SUR_OISE",
    lcom=="BEAUCHAMPS"~"BEAUCHAMP",
    lcom=="BOUQUEVAL_PM_LOCALE"~"BOUQUEVAL",
    lcom=="ERAGNY_SUR_OISE"~"ERAGNY",
    lcom=="HEROUVILLE"~"HEROUVILLE_EN_VEXIN",
    lcom=="MARINE"~"MARINES",
    lcom=="MONSOULT"~"MONTSOULT",
    str_detect(lcom,"SOISS?Y_S(OUS|UR)_MONTMORENCY")~"SOISY_SOUS_MONTMORENCY",
    lcom=="ST_MARTIN_DU_TRERTRE"~"ST_MARTIN_DU_TERTRE",
    lcom=="VAUD_HERLAND"~"VAUDHERLAND",
    str_detect(lcom,"^BR?UYERES(/|_SUR_)OISE")~"BRUYERES_SUR_OISE",
    T~str_remove(lcom,"_PM_LOCALE$")))
  
  return(x)
}

#Fonction pour les choix de redéfinition territoriales (interco,fusions)
redefinitions1=function(x){
  
  x[which(x$dep=="01"),]=mutate(x[which(x$dep=="01"),],ccom=case_when(
    str_detect(lcom,"BELLEGARD|^CHATILLON_EN_MICHAILLE")~"01033",
    T~ccom))
  
  x[which(x$dep=="14"),]=mutate(x[which(x$dep=="14"),],ccom=case_when(
    lcom=="VIRE"~"14762",
    T~ccom))
  
  x[which(x$dep=="22"),]=mutate(x[which(x$dep=="22"),],ccom=case_when(
    lcom=="LAMBALLE"~"22093",
    T~ccom))
  
  x[which(x$dep=="49"),]=mutate(x[which(x$dep=="49"),],ccom=case_when(
    lcom=="VILLEDIEU_LA_BLOUERE"~"49023",
    lcom%in%c("CHEMILLE","CHEMILLE_MELAY","STE_CHRISTINE","ST_GEORGES_DES_GARDES","VALANJOU")~"49092",
    lcom=="CORNE"~"49307",
    lcom=="ST_QUENTIN_EN_MAUGES"~"49218",
    str_detect(lcom,"SEGRE")~"49331",
    T~ccom))
  
  x[which(x$dep=="50"),]=mutate(x[which(x$dep=="50"),],ccom=case_when(
    lcom%in%c("CHERBOURG_OCTEVILLE","EQUEURDREVILLE_HAINNEVILLE","LA_GLACERIE","QUERQUEVILLE","TOURLAVILLE")~"50129",
    T~ccom))
  
  x[which(x$dep=="53"),]=mutate(x[which(x$dep=="53"),],ccom=case_when(
    lcom%in%c("CHATEAU_GONTIER","AZE")~"53062",
    T~ccom))
  
  x[which(x$dep=="57"),]=mutate(x[which(x$dep=="57"),],ccom=case_when(
    lcom=="THIONVILLE_TERVILLE"~"57672",
    T~ccom))
  
  x[which(x$dep=="59"),]=mutate(x[which(x$dep=="59"),],ccom=case_when(
    lcom=="ARMENTIERES_LA_CHAPELLE_D_ARMENTIERES"~"59017",
    str_detect(lcom,"^HEM")~"59299",
    T~ccom))
  
  x[which(x$dep=="69"),]=mutate(x[which(x$dep=="69"),],ccom=case_when(
    lcom%in%c("OULLINS","PIERRE_BENITE")~"69149",
    T~ccom))
  
  x[which(x$dep=="74"),]=mutate(x[which(x$dep=="74"),],ccom=case_when(
    lcom%in%c("ANNECY_LE_VIEUX","CRAN_GEVRIER","MEYTHET","SEYNOD")~"74010",
    str_detect(lcom,"JULIEN|_DU_SALEVE")~"74243",
    T~ccom))
  
  x[which(x$dep=="77"),]=mutate(x[which(x$dep=="77"),],ccom=case_when(
    str_detect(lcom,"SEINE_ECOLE")~"77407",
    T~ccom))
  
  x[which(x$dep=="78"),]=mutate(x[which(x$dep=="78"),],ccom=case_when(
    lcom%in%c("LE_CHESNAY","ROCQUENCOURT")~"78158",
    lcom%in%c("ST_GERMAIN_EN_LAYE/MAREIL_MARLY","FOURQUEUX")~"78551",
    T~ccom))
  
  x[which(x$dep=="85"),]=mutate(x[which(x$dep=="85"),],ccom=case_when(
    lcom%in%c("MONTAIGU","TERRES_DE_MONTAIGU")~"85146",
    lcom%in%c("CHATEAU_D_OLONNE","OLONNE_SUR_MER")~"85194",
    T~ccom))
  
  x[which(x$dep=="91"),]=mutate(x[which(x$dep=="91"),],ccom=case_when(
    lcom%in%c("EVRY","COURCOURONNES")~"91228",
    T~ccom))
  
  x[which(x$dep=="93"),]=mutate(x[which(x$dep=="93"),],ccom=case_when(
    lcom%in%c("ST_DENIS","PIERREFITTE_SUR_SEINE")~"93066",
    T~ccom))
  
  x[which(x$dep=="94"),]=mutate(x[which(x$dep=="94"),],ccom=case_when(
    str_detect(lcom,"ABLON|VILLENEUVE_LE_ROI")~"94077",
    T~ccom))
  
  x[which(x$dep=="95"),]=mutate(x[which(x$dep=="95"),],ccom=case_when(
    lcom=="DOMONT_/_PISCOP"~"95199",
    T~ccom))
  
  x=mutate(x,
           miseadispointerco=case_when(
             #ça devrait probablement être 1 : cf note indiquant partage de 
             #capacités par CHATILLON_EN_MICHAILLE, intégrée à VALSERHONE en 2019.
             #Mais 0 est indiqué pour 2022-2024
             #dep=="01"&annee%inc(2018,2019)%&str_detect(lcom,"VALSERHONE")~1,
             #ça devrait être 1 pour les deux suivants, afin de prendre en compte 
             #que j'ai attribué à la commune principale ces deux polices en 
             #fait intercommunales
             #dep=="74"&annee>=2018&str_detect(lcom,"ST_JULIEN_EN_GENEVOIS")~1,
             #dep=="85"&annee>=2018&str_detect(lcom,"MONTAIGU_VENDEE")~1,
             T~miseadispointerco
           )
  )
  
  return(x)
  
}

#Fonction pour procéder à la somme des effectifs sur les territoires réunifiés, 
#une fois les libellés modifiés par la fonction précédente
redefinitions2=function(x,fusions){
  
  x[which(x$ccom%in%fusions),]=x[which(x$ccom%in%fusions),] %>% 
    group_by(ccom,annee) %>% 
    #Choix contestable de définition pour la variable miseadispo
    #D'aucuns pourraient préférer max à prod ("il suffit qu'une commune"
    #au lieu de "si toutes les communes")
    mutate(miseadispointerco=prod(miseadispointerco,na.rm=F),
           across((which(names(x)%in%c(
             "polmun","asvp","maitrechien","chien","brigcanine","gardechamp"
           ))-2),sum)
    ) %>% 
    ungroup()
  
  return(x)
  
}

#Fonction changeant les valeurs absentes en zéros. Les fichiers originaux 
#contiennent fréquemment des vides qui sont clairement censés être interprétés 
#de la sorte. ça n'est donc pas une simplification abusive à mon sens.
transfona0<-function(x){
  case_when(is.na(x)~0,
            T~x)
}

#Fonction d'épuration de la liste départementale (notamment pour enlever le
#chef-lieu lorsqu'il a été inscrit dans la même colonne)
epurdep=function(x){
  case_when(x%in%c(departements$ldep,
                   "POLYNESIE_FRANCAISE",
                   "NOUVELLE_CALEDONIE",
                   "ST_BARTHELEMY",
                   "ST_MARTIN",
                   "ST_PIERRE_ET_MIQUELON"
  )~x,
  T~NA)
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


#Téléchargement : https://www.insee.fr/fr/information/7766585 (section
#"Départements", fichier csv)
departements<-read.csv("Données/departements.csv",encoding = "UTF-8") %>% 
  select(1,5) %>% 
  set_names("dep","ldep") %>% 
  mutate(ldep=standardiserlibelles(ldep,1))

#Stocker les fichiers relatifs aux communes nouvelles dans un sous-dossier de "Données" nommé 
#"Liste communes nouvelles". Téléchargement : https://www.insee.fr/fr/information/2549968
fichierscommunesnouvelles=list.files(paste(getwd(),"/Données/Liste communes nouvelles",sep=""))

communesnouvelles=list()

for(i in 1:length(fichierscommunesnouvelles)){
  communesnouvelles[[i]]=read_excel(
    paste(getwd(),
          "/Données/Liste communes nouvelles/",
          fichierscommunesnouvelles[i],sep=""),
    range = cell_cols("A:I"),
    col_types = c(rep("text",9))) %>% 
    select(c(1,2,4,"Date2")) %>% 
    set_names(c("ccom","lcom","excommune","temps")) %>% 
    filter(rowSums(is.na(.))!=ncol(.)) %>% 
    group_by(ccom) %>% 
    mutate(temps=case_when(
      sum(!is.na(temps))==0~paste("01/01/",2015+i,sep=""),
      T~temps[which(!is.na(temps))[1]]
    )) %>% 
    ungroup()
}

communesnouvelles=bind_rows(communesnouvelles) %>% 
  mutate(temps=case_when(
    nchar(temps)!=5~as.character(as.Date(temps,format="%d/%m/%Y")),
    T~substr(as.character(as.Date(as.numeric(temps),origin="1900-01-01")),1,10)
  ),
  across(which(names(.)%in%c("lcom","excommune")),
         ~standardisationcomplementaire(standardiserlibelles(.,1))),
  dep=substr(ccom,1,2)) %>% 
  select(dep,everything()) %>% 
  #L'année de fusion est décalée d'un an, par conformité avec le décalage des
  #résultats d'enquête. En effet, si l'on considère une fusion au 1er janvier n,
  #l'ancienne commune aura rempli l'enquête de décembre n-1, donc la donnée de
  #l'année n sera légitimement au nom de l'ancienne commune, ce qui implique que
  #pour mon usage la fusion ne prend effet que sur les données en n+1
  mutate(anneefusion=as.numeric(substr(temps,1,4))+1,
         mois=as.numeric(substr(temps,6,7))) %>% 
  arrange(ccom)

#Populations communales. La donnée parisienne doit être obtenue par somme 
#des arrondissements
#Téléchargement : https://www.insee.fr/fr/statistiques/3698339
popcom<-read_excel("Données/popcommunale.xlsx",
                   skip = 5) %>% 
  rbind(c("75056","11","75","Paris",rep(NA,ncol(.)-4))) %>% 
  set_names(tolower(str_remove_all(names(.),"PMUN"))) %>% 
  rename(ccom=codgeo,lcom=libgeo) %>% 
  mutate(across(5:ncol(.),as.numeric),
         across(5:ncol(.),~case_when(
           ccom=="75056"~sum(.[which(str_detect(ccom,"75[0-9]{3}")&ccom!="75056")]),
           T~.))) %>% 
  mutate(`2024`=`2023`)

#Base communale du Ministère de l'Intérieur. Téléchargement :
#https://www.data.gouv.fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales
#Fichier "COM COMPL" parmi les fichiers principaux. Une base à jour des 
#noms et codes des communes marche aussi pour cet usage
classificationcommunes<-read_excel("Données/classificationcommunes.xlsx") %>% 
  set_names(tolower(names(.))) %>% 
  rename(lcom=libgeo,ccom=codgeo) %>% 
  mutate(lcom=standardiserlibelles(lcom,0)) %>% 
  left_join((popcom %>% select(c(ccom,as.character(2014:2023)))),by=join_by(ccom)) %>% 
  arrange(-`2023`)

#nchar(dep)<3 est la condition éliminant les outres-mers
communesplus15000=classificationcommunes %>% 
  filter(`2023`>=15000&!str_detect(lcom,"_ARRONDISSEMENT")&nchar(dep)<3) %>% 
  select(dep,ccom,lcom,`2023`) %>% 
  rename(pop=`2023`)




#IMPORTATION DES FICHIERS D'EFFECTIFS UN A UN
#Téléchargement : https://www.data.gouv.fr/datasets/police-municipale-effectifs-par-commune
#On les suppose ici stockées dans un sous-dossier "Effectifs de police municipale"
#et renommés selon le format "effectifspolicemunicipaleXXXX" où XXXX est l'année

#Les données pour Paris en 2013
polmun2014<-read_excel(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2013.xlsx", 
  skip = 7) %>% 
  select(1:6) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","brigcanine") %>% 
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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

#Les données pour St-Pierre-et-Miquelon sont manquantes
polmun2021<-read_ods(
  "Données/Effectifs de police municipale/effectifspolicemunicipale2020.ods",
  skip=7) %>% 
  select(c(2,4,6:10)) %>% 
  set_names("ldep","lcom","polmun","asvp","gardechamp","maitrechien","chien") %>% 
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
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
  mutate(across(1:2,~standardiserlibelles(.,1)),
         ldep=str_remove_all(ldep,"COLLECTIVITE_DE_"),
         ldep=standardiserlibelles(ldep,1),
         across(3:8,as.numeric),
         ldep=epurdep(ldep)) %>% 
  filter(rowSums(is.na(.))!=ncol(.)) %>% 
  rolldep("ldep") %>% 
  filter(!is.na(lcom)) %>% 
  mutate(annee=2024,
         lcom=standardisationcomplementaire(lcom)) %>% 
  unique()




#UNIFICATION DES FICHIERS
polmun=list()

for(i in 2014:2024){
  polmun[[as.character(i)]]=get(paste("polmun",i,sep=""))
}

polmun=lapply(polmun,function(x){
  
  x=x %>% left_join(departements,by=join_by(ldep)) %>% 
    mutate(across(which(names(.)%in%c("polmun","asvp","gardechamp","miseadispointerco",
                                      "brigcanine","maitrechien","chien")),
                  transfona0))
  
  for(i in 1:4){
    if(!c("miseadispointerco","brigcanine","maitrechien",
          "chien")[i]%in%names(x)){
      x=mutate(x,nvllecol=NA) %>% 
        set_names(c(names(x),
                    c("miseadispointerco","brigcanine","maitrechien","chien")[i]
        ))
    }
  }
  
  return(x %>% select(dep,ldep,annee,lcom,
                      miseadispointerco,polmun,asvp,gardechamp,
                      brigcanine,maitrechien,chien))
}) %>% bind_rows() %>% 
  mutate(dep=factor(dep,levels=c(str_pad(as.character(1:19),2,pad="0"),
                                 "2A","2B",as.character(21:96)))) %>% 
  arrange(annee) %>% 
  arrange(lcom) %>% 
  arrange(dep)

#polmpdn = polmun purgé des notes. Il s'agit surtout d'enlever les notes entre 
#parenthèses (souvent pour préciser un lien intercommunal, l'armement d'un garde
#champêtre, la présence seulement saisonnière d'un ASVP, etc ...)
#En pratique, polmun servira de base de référence pour des travaux de "ciselage" 
#du jeu de données, et polmunpdn sera le produit fini. Les corrections orthographiques 
#auxquelles j'ai déjà procédé sont apportées sur la base purgée des notes
polmunpdn=polmun %>% 
  mutate(lcom=standardiserlibelles(str_remove_all(lcom,"\\(.*\\)?"),1),
         lcom=standardisationcomplementaire(lcom)) %>% 
  correctionsortho() %>% 
  #On enlève et réinsère les libellés départementaux car correctionsortho
  #a permis la modification de certains numéros départementaux erronés sans
  #effet sur les libellés, désormais mal ajustés
  select(-c("ldep")) %>% 
  left_join(departements,by=join_by(dep)) %>% 
  left_join(classificationcommunes %>% 
              select(lcom,ccom,dep),
            by=join_by(lcom,dep)) %>% 
  select(dep,ccom,lcom,annee,everything()) %>% 
  #Elimine un doublon avec erreur de données et une ligne vierge
  filter(!(lcom=="MOUGINS"&annee=="2022"&polmun==0)&
           !(dep=="75"&!str_detect(lcom,"PARIS"))) %>% 
  #Les redéfinitions permettent de combiner les forces policières pré-fusions,
  #pour assurer une définition géographique constante au travers du temps
  redefinitions1() %>% 
  redefinitions2(c("01033","49023","49092","50129","53062","59183","59350",
                   "69149","74010","74243","78158","78551","85194","91228",
                   "93066","94077")) %>% 
  filter(ccom%in%communesplus15000$ccom) %>% 
  #On doit à nouveau joindre les libellés communes car redefinitions1 n'a
  #modifié que les codes communes
  select(-c("lcom")) %>% 
  left_join(classificationcommunes %>% 
              select(lcom,ccom,dep),
            by=join_by(ccom,dep)) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2013:2023))) %>% 
              pivot_longer(cols=2:12,values_to="popn",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)+1),
            by=join_by(ccom,annee)) %>% 
  left_join(popcom %>% 
              select(c("ccom",as.character(2014:2024))) %>% 
              pivot_longer(cols=2:12,values_to="popnplusun",names_to="annee") %>% 
              mutate(annee=as.numeric(annee)),
            by=join_by(ccom,annee)) %>% 
  unique() %>% 
  select(dep,ldep,ccom,lcom,annee,everything()) %>% 
  mutate(communenouvelle=if_else(ccom%in%communesnouvelles$ccom,1,0),
         dep=factor(dep,levels=c(str_pad(as.character(1:19),2,pad="0"),
                                 "2A","2B",as.character(21:96)))) %>% 
  arrange(annee) %>% 
  arrange(lcom) %>% 
  arrange(dep)




#DETECTION DE DOUBLONS
#Les doublons peuvent être causés ou bien par des doublons dans les fichiers 
#originaux, ou bien par l'oubli de réunifier certaines données via la fonction 
#redefinitions2 après avoir pratiqué une fusion via redefinitions1, ou via
#correctionsortho (cas de DUNKERQUE et de LILLE à cette heure). Il faut alors
#veiller à ajouter le code commune nécessaire en argument de redefinitions2
#lors de la création de polmunpdn
polmunpdn %>% 
  group_by(ccom,annee) %>% 
  mutate(occurences=1,
         occurences=sum(occurences)) %>% 
  filter(row_number()==1&occurences>1) %>% 
  ungroup() %>% 
  select(ccom,lcom,annee,occurences) %>% 
  #nrow()
  View()




#EXPORTATION DU FICHIER
#Ici, on revient aux dates d'enquête
write.csv(x=polmunpdn %>% mutate(annee=annee-1) %>% 
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
                "Population au 1er janvier de l'année suivante (donnée 2024 égale au recensement 2023)",
                "Commune nouvelle (résultats obtenus, les années pertinentes, par addition des effectifs des communes ayant fusionné)")
            ),
          file="effectifspolicemunicipale.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")





