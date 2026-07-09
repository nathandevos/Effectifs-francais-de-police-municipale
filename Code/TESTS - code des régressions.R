library(tidyverse)
library(readxl)
library(readODS)
library(estimatr)
library(rlist)

#Téléchargement du jeu depuis datagouv s'il n'a pas été recréé via les fichiers 
#"2 - jeupourreg"
jeupourreg=read_csv("jeupolmunpourreg.csv")

#Le jeu resserré ne sélectionne que des villes ayant suffisamment d'observations
#mathématiquement valables après passage au logarithme pour permettre 
#l'estimation des tendances par commune. Comme on ne constate à la condition, 
#j'impose 1° l'existence d'une observation définie en 2016 ou 2017,
#2° la même chose en 2021 ou 2022, 3° au moins 4 observations définies.
#Sans ces règles, certains coefficients de la régression pourraient être non 
#définis, et c'est bête d'avoir perdu 10 minutes à faire tourner une batterie 
#pour aboutir à un gruyère.
jeupourregresserre=jeupourreg %>% 
  group_by(ccom) %>% 
  mutate(valide=if_else(!is.na(polmun)&polmun>0,1,0),
         valide=prod(max(valide[which(annee%in%2016:2017)]),
                     max(valide[which(annee%in%2021:2022)]),
                     if_else(sum(valide)>=4,1,0))) %>% 
  filter(valide==1) %>% 
  ungroup() %>% 
  select(-valide)

jeupourregresserre=jeupourregresserre %>% 
  filter(!is.na(logpolmun)) %>% 
  mutate(residufdo=lm(data=.,fdomun~polmun)$residuals,
         residulogfdo=lm(data=.,logfdomun~logpolmun)$residuals,
         residulincarfdo=lm(data=.,lincarfdomun~lincarpolmun)$residuals,
         residulog2fdo=lm(data=.,log2fdomun~log2polmun)$residuals) %>% 
  select(ccom,annee,residufdo,residulogfdo,residulincarfdo,residulog2fdo) %>% 
  left_join(jeupourregresserre,.,by=join_by(ccom,annee))

#Le jeu intermédiaire préserve les valeurs 0 pour les régressions en format 
#lin-lin
jeupourregintermediaire=jeupourreg %>% 
  group_by(ccom) %>% 
  mutate(valide=if_else(!is.na(polmun),1,0),
         valide=prod(max(valide[which(annee%in%2016:2017)]),
                     max(valide[which(annee%in%2021:2022)]),
                     if_else(sum(valide)>=4,1,0))) %>% 
  filter(valide==1) %>% 
  ungroup() %>% 
  select(-valide)

varcibles=c("violencesextrafam",
            "volscontrepers",
            "escroqueries",
            "cambriolages",
            "volsdevehicule",
            "volsdansvehicule",
            "volssurvehicule",
            "degradations",
            "usagestup")

vardinteret=c("polmun","polmundac","fdomun","fdomundac")

regresseursfisc=c("revparhab","residuirppparhab",
                  "partsalaries","partretraites",
                  "partpauvres","partaises")

regresseursfisc2=c("revparhab","residuirppparhab",
                   "partsalaries","partretraites",
                   "partpauvres","partaises")

regresseursdc=tolower(c("NAISD","DECESD","ENCITOT","ENCTOT",
                        "ETCBE","ETCFZ","ETCGI","ETCMN", 
                        "ETCOQ","ETCRU","DENS"))

regresseursdvf=c("nbremaisons","nbremutations","surfacemoy","prixmoym2")

#Inclure dgf dans les régressions logarithmiques ampute 12 communes de 3 obser-
#vations ou plus. Or il est tout à fait évident, comme dgf est corrélé au niveau 
#de pauvreté, que la censure de ces 12 communes serait une censure endogène 
#biaisant potentiellement les coefficients estimés.
regresseursbudmun=c("epargnebrute","annuitedette",
                    "depfonc","depinv")

regresseursbudmun2=c("epargnebrute","annuitedette",
                     "depfonc","depinv")

#Les modèles seront dénotés Mxxxxx. Chacun des 5 chiffres (donc caractères 2 à 6
#dans le nom du modèle) décrivant une dimension du modèle selon la table
#interprétative. Ainsi, en se référant à la table, un modèle Mxxx0x est un
#modèle où la dimension 5 vaut 0, donc un modèle où toutes les observations
#d'une même commune sont englobées en un même cluster. Une batterie de modèles
#Mxxxxx prendra la forme d'une liste de 9 tests, un par indicateur de
#délinquance : la 7e dimension dans la table rappelle la correspondance
#indice-indicateur (M11120[[2]] est le test de paramètres 11120 portant sur 
#le deuxième indicateur, savoir les vols contre personnes)
Tableinterprétative=data.frame(
  numérodimension=c(rep(1,3),rep(2,5),rep(3,2),rep(4,3),rep(5,2),rep(6,9))+1,
  libellédimension=c("type de modèle",
                     "régresseur d'intérêt",
                     "forme du second terme policier",
                     "cluster des erreurs",
                     "inclusion des régresseurs DVF",
                     "variable cible")[
                       c(rep(1,3),rep(2,5),rep(3,2),rep(4,3),rep(5,2),rep(6,9))
                     ],
  signification=c("log-log",
                  "lin-lin",
                  "lin-log",
                  "sans",
                  "polmun",
                  "polmundac",
                  "fdomun",
                  "fdomundac",
                  "logarithmique",
                  "linéaire",
                  "commune",
                  "pseudoaire",
                  "aireurbainehorsIDF",
                  "non",
                  "oui",
                  varcibles),
  valeur=c(1:3,0:4,1:2,0:2,0:1,1:9)
)

#On remarquera que j'exclus dans ces formulations, peu importe la configuration,
#toute mobilisation du cas parisien. On remarquera aussi que, en ce qui concerne
#les variables de contrôle hors estimations des effectifs de forces de l'ordre
#nationales, je n'ai recours qu'à des formulations "x+x^2" ou "log(x)+log(x)^2"
#: pour mobiliser les autres transformations, il faudra modifier ce code. Enfin,
#pour réduire les intervalles de confiance, j'ai laissé sur le carreau certains
#termes de second ordre pour lesquels l'endogénéité n'était pas évidente ou
#directe : les variables économiques de revenu et d'inégalité (IRCOM) ainsi que 
#celles de dépense publique locale sont les seules outre celles relatives aux 
#forces de l'ordre nationales à se voir adjoindre le second terme.

#Le M veut juste dire "modèle", mais n'a aucun rôle dans le code. Il peut être 
#substitué à souhait pour différencier les sorties de régressions introduites 
#par l'usager des miennes
code="M11120"

if(exists(code)){
  print("Cette batterie de tests existe déjà (vierge ou calculée)")
  } else {
    assign(code,list())
  }

#Le temps de calcul d'une batterie de 9 tests, avec la fonction ci-dessous, 
#est de 10 minutes pour une formulation Mxxx2x, et de carrément 1 h 30 pour 
#des variantes Mxxx1x. Naturellement, la variante Mxxx0x, avec des clusters par 
#communes, même si elle est vraisemblablement moins rigoureuse, est beaucoup plus 
#courte à faire tourner. M11120 est le test qui me semble le plus approprié.
if(length(get(code))>0){
  print("Tout ou partie des tests de cette batterie sont déjà calculés. Utiliser la fonction ci-dessous peut amener à décorréler le numéro de l'indicateur dans varcibles et son numéro dans la batterie.")
} else {
  for(i in 1:9){
    assign(
      code,
      list.append(
        get(code),
        lm_robust(
          data=if(substr(code,2,2)==2){
            if(substr(code,5,5)==2){
              jeupourregintermediaire %>% 
                filter(!dep%in%c(
                  "75","77","78","91","92","93","94","95"
                )&!is.na(polmun))
            } else {
              jeupourregintermediaire %>% 
                filter(dep!="75"&!is.na(polmun))
            }
          } else {
            if(substr(code,5,5)==2){
              jeupourregresserre %>% 
                filter(!dep%in%c(
                  "75","77","78","91","92","93","94","95"
                )&polmun>0&!is.na(polmun))
            } else {
              jeupourregresserre %>% filter(dep!="75"&polmun>0&!is.na(polmun))
            }},
          fixed_effects=ccom,
          clusters=if(substr(code,5,5)==0){ccom} else {pseudoaire},
          formula=as.formula(paste(
            if_else(substr(code,2,2)==1,"log",""),
            varcibles[i],"~",
            #Le premier cas, pour la dimension 3 valant 0, est le modèle étalon sans
            #intégration des valeurs de police municipale
            if_else(substr(code,3,3)==0,"",
                    #Pour Mxx1xx, second terme policier log, sinon lin
                    #Pour M2xxxx, les régresseurs sont lin, sinon log
                    if(substr(code,4,4)==1){
                      if(substr(code,2,2)==2){
                        #modèle linéaire et second terme logarithmique
                        paste(vardinteret[as.numeric(substr(code,3,3))],
                              "+lin2",
                              vardinteret[as.numeric(substr(code,3,3))],
                              sep="")
                      }
                      else{
                        #modèle logarithmique et second terme logarithmique
                        paste("log",
                              vardinteret[as.numeric(substr(code,3,3))],
                              "+log2",
                              vardinteret[as.numeric(substr(code,3,3))],
                              sep="")
                      }
                    } else{
                      if(substr(code,2,2)==2){
                        #modèle linéaire et second terme linéaire
                        paste(vardinteret[as.numeric(substr(code,3,3))],
                              "+lincar",
                              vardinteret[as.numeric(substr(code,3,3))],
                              sep="")
                      }
                      else{
                        #modèle logarithmique et second terme linéaire
                        paste("log",
                              vardinteret[as.numeric(substr(code,3,3))],
                              "+",
                              vardinteret[as.numeric(substr(code,3,3))],
                              sep="")
                      }
                    }
            ),if_else(substr(code,3,3)==0,"","+"),
            #A noter que "A*B" est un opérateur qui équivaut à "A+B+A:B", de
            #sorte qu'en supprimant ces lignes on n'enlèverait pas seulement le
            #terme d'intersection, mais aussi les termes simples, qu'il faudrait
            #alors réintroduire
            if_else(substr(code,2,2)==2,
                    "violencesintrafam*gendarmes+violencesintrafam*polnat+",
                    "logviolencesintrafam*loggendarmes+logviolencesintrafam*logpolnat+"),
            if(substr(code,4,4)==1){
              if(substr(code,2,2)==2){
                #modèle linéaire et second terme logarithmique
                "lin2polnat+lin2gendarmes"
              }
              else{
                #modèle logarithmique et second terme logarithmique
                "log2polnat+log2gendarmes"
              }
            } else{
              if(substr(code,2,2)==2){
                #modèle linéaire et second terme linéaire
                "lincarpolnat+lincargendarmes"
              }
              else{
                #modèle logarithmique et second terme linéaire
                #en effet, on veut dire que la dérivée donne x/x=1
                "polnat+gendarmes"
              }
            },"+",
            paste(if_else(substr(code,2,2)==2,"","log"),
                  c(regresseursdc,regresseursfisc,regresseursbudmun,"txchom"),
                  sep="",collapse="+"),"+",
            paste(if_else(substr(code,2,2)==2,"lincar","log2"),
                  c(regresseursfisc2,regresseursbudmun2),sep="",collapse="+"),
            "+d2020+ccom:annee",
            if_else(substr(code,6,6)==0,"",
                    paste("+",
                          if_else(substr(code,2,2)==2,
                                  paste(regresseursdvf,collapse="+"),
                                  paste("log",regresseursdvf,sep="",collapse="+")),
                          sep="")),
            sep="")))
      )
    )
  }
  }

#Pour les variantes où le cluster est selon le zonage plutôt que selon la
#commune, le faible nombre de clusters (environ 200) rend imprécis les
#intervalles de confiance. Les résultats pour, par exemple, le modèle M11120,
#sont cependant dans les clous des méta-analyses connues pour d'autres données
#(voir M. Carriaga & J. Worrall 2015 et Y. Lee, J. Eck & N. Corsaro 2016), qui
#concluent à des élasticités (quand elles sont négatives) du niveau de
#délinquance par rapport aux effectifs de police comprises entre 0 et -0.15 (
#avec une exception : sur une mesure "aggrégée" regroupant de nombreux
#indicateurs, une étude isolée pousse Carriaga et Worrall à conclure à une
#valeur de -0.25), et des résultats qui peinent à être significatifs. Les 
#élasticités constatées pour M11120 (coefficient de logpolmun) ne tombent 
#jamais sous -0.18, et ne sont significatives que dans le cas des destructions 
#et dégradations volontaires (tags, incendies de véhicules, etc ...) avec une 
#valeur de -0.134 et un écart-type de 0.034874

# 1 "violencesextrafam"
# 2 "volscontrepers"
# 3 "escroqueries"
# 4 "cambriolages"
# 5 "volsdevehicule"
# 6 "volsdansvehicule"
# 7 "volssurvehicule"
# 8 "degradations"
# 9 "usagestup"

variable=8

summary(get(code)[[variable]])

