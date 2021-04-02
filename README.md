Cette application vise à faciliter la manipulation de données de trajectoires à partir du logiciel de traitement statistique *'R'*. 
Elle permet aux personnes qui ne sont pas familières de ce langage de pouvoir utiliser un certain nombre de fonctions particulièrement utiles pour la manipulation de données de trajectoires.

Les fonctions spécifiques incluses dans l'application proviennent surtout des paquets suivants : 

 - TraMineR: http://traminer.unige.ch/

   - Gabadinho, A., Ritschard, G., Müller, N. S., & Studer, M. (2011). Analyzing and Visualizing State
       Sequences in R with TraMineR. Journal of Statistical Software, 40(4), 1-37. DOI
       http://dx.doi.org/10.18637/jss.v040.i04.

   - Studer, M. & Ritschard, G. (2016). What matters in differences between life trajectories: A
       comparative review of sequence dissimilarity measures, Journal of the Royal Statistical Society,
       Series A, 179(2), 481-511. DOI http://dx.doi.org/10.1111/rssa.12125

 - WeightedCluster : 

     https://cran.r-project.org/package=WeightedCluster

     Studer, Matthias (2013). WeightedCluster Library Manual: A practical guide to creating typologies
     of trajectories in the social sciences with R. LIVES Working Papers, 24. DOI:
     10.12682/lives.2296-1658.2013.24.

## 1. Construction d'un jeu de données :    

L'application permet dans un premier temps de construire des trajectoires à partir de données 'brutes' (dans l'onglet *'Les données'*).

Trois types de fichiers peuvent être chargés dans l'application :

1. un fichier .csv avec des données au format 'wide' : une colonne par variable et par date, et à la suite une colonne par variable complémentaire.    

   | VARIABLE_IDENTIFIANT | RSA_1 | RSA_2  | RSA_3 | Mois_ ... | SEXE | COMMUNE_HABITATION | ...  |
   | -------------------- | ----- | ------ | ----- | --------- | ---- | ------------------ | ---- |
   | 1                    | RSA   | RSA    | RSA   | ...       | M    | Paris              | ...  |
   | 2                    | NA    | RSA    | RSA   | ...       | F    | Lyon               | ...  |
   | 3                    | RSA   | Sortie | NA    | ...       | NA   | Grenoble           | ...  |

2. un fichier .RData comportant une liste (`list()`) avec un objet 'trajectoire' créé par la fonction `seqdef()` et un data.frame comportant des données complémentaires. Dans ce cas, les noms de ligne (`row.names()`) des trajectoires doivent correspondre avec une colonne de variable d'identifiant individuel dans le data.frame (variable à spécifier dans l'application).    

   *Exemple de data.frame pour les données complémentaires :*

   | VARIABLE_IDENTIFIANT | SEXE | COMMUNE_HABITATION | ...  |
   | -------------------- | ---- | ------------------ | ---- |
   | 1                    | M    | Paris              | ...  |
   | 2                    | F    | Lyon               | ...  |
   | 3                    | NA   | Grenoble           | ...  |

   

3. un fichier .RData comportant une liste ('list()') avec pour chaque date (chaque mois, chaque année, chaque jour, etc) un data.frame avec les valeurs correspondantes. Dans ce cas, les trajectoires pourront être générées à partir de n'importe nom de variable commun aux différentes dates.  

   *Exemple de liste de data.frames:*

   | VARIABLE_IDENTIFIANT_1 | RSA_1 | SEXE_1 | COMMUNE_HABITATION_1 | ...  |
   | ---------------------- | ----- | ------ | -------------------- | ---- |
   | 1                      | RSA   | M      | Paris                | ...  |
   | 3                      | RSA   | NA     | Grenoble             | ...  |

   | VARIABLE_IDENTIFIANT_2 | RSA_2  | SEXE_2 | COMMUNE_HABITATION_2 | ...  |
   | ---------------------- | ------ | ------ | -------------------- | ---- |
   | 1                      | RSA    | M      | Paris                | ...  |
   | 2                      | RSA    | F      | Lyon                 | ...  |
   | 3                      | Sortie | NA     | Grenoble             | ...  |

   | VARIABLE_IDENTIFIANT_3 | RSA_3 | SEXE_3 | COMMUNE_HABITATION_3 | ...  |
   | ---------------------- | ----- | ------ | -------------------- | ---- |
   | 1                      | RSA   | M      | Paris                | ...  |
   | 2                      | RSA   | F      | Lyon                 | ...  |

   Les individus peuvent absents de certains tableaux de données pour certains mois, ou être présents avec  des valeurs manquantes (NA). 



A partir de ces différentes sources, des possibilités de sélection d'individus sont offertes (sélection multi-critères sur tous les types de données). 

Chaque sélection constituée dans l'application et qui a donné lieu à la constitution de trajectoires peut être enregistrée localement et réutilisée (dans R ou dans l'application).

### Note sur le format des données

* Pour plus d'infos sur la structure des données (*"wide"* ou *"long"* ) et comment bien organiser ses tables, consultez la page d'explication sur les données *"tidy"* : 

  https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

* Pour plus d'infos sur les types de format de données compatibles avec la fonction `seqdef()`voir les explications très complète set très claires dans l'aide de TraMineR : 

  http://mephisto.unige.ch/pub/TraMineR/doc/TraMineR-Users-Guide.pdf



## 2. Indicateurs statistiques :

Une fois que les trajectoires sont construites et validées, différents onglets permettent d'obtenir des indicateurs statistiques sur les trajectoires et leurs données complémentaires.

- Les principaux indicateurs de base fournis dans le package R **'TraMineR'**, croisés avec les données complémentaires (taux de transition, chronogramme des états, temps moyen passé dans chaque état, événements, états modaux, représentations en tapis, graphiques de flux, etc.)

- Des tableaux de donnés (tris à plat ou tableau croisés) sur l'ensemble des données générées, y compris les données complémentaires.

## 3. Classification des trajectoires :

L'application permet aussi de générer des classes de trajectoires à partir de méthodes basiques (Optimal Matching et ses dérivés principalement), et d'utiliser ces groupes pour visualiser et comparer les données.

Les groupes peuvent être enregistrés localement et réutilisés dans R  ou dans l'application. 

Néanmoins, pour une classification fine et pleinement paramétrable, il serais préférable d'utiliser directement les diverses fonctions existantes dans une interface R classique.
