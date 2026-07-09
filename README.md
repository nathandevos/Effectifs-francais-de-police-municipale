Ici est consigné le code R à l'origine des données et publications présentées sur : https://www.data.gouv.fr/datasets/effectifs-de-police-municipale-par-commune-france-entiere-2013-2024

Le fichier débutant par TESTS sert à conduire des batteries de régressions sur 9 indicateurs de délinquance à partir du jeu "Délinquance et polices municipales"

Les autres fichiers servent à reproduire les jeux de données. Ils sont nommés "numéro - branche - (sous-branche dans le cas "3 - nettoyage") - titre".
La branche "nettoyage" permet de nettoyer les jeux de données ministériels relatifs aux effectifs de police municipale, 
tandis que la branche "jeupourreg" permet de reproduire le jeu de données pour régressions.
Le "fichier préliminaire" est commun aux deux branches, mais sauf erreur de ma part il n'est pas nécessaire de l'employer pour utiliser le fichier TESTS.
Les numéros donnent l'ordre d'exécution, et pour réaliser un jeu donné il n'y a toujours besoin que d'un fichier par numéro au plus.
Par exemple, pour reconstituer le jeu des données semi-brutes, on utilisera : "1 - fichier préliminaire", "2 - nettoyage - corrections orthographiques", "3 - nettoyage - dsb - donnees semi brutes", "4 - nettoyage - exportation fichiers".

Le code devrait être suffisamment commenté quant au reste. Prendre particulièrement gare au décalage dans les dates dans les fichiers de nettoyage. Pour toute question : nathan2vos12@gmail.com
