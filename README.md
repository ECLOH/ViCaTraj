Cette application vise à faciliter la manipulation de données de trajectoires à partir du logiciel de traitement statistique 'R'. Elle permet aux personnes qui ne sont pas familières de ce langage de pouvoir utiliser un certain nombre de fonctions particulièrement utiles pour la manipulation de données de trajectoires.

1. Construction d'un jeu de données :
L'application permet dans un premier temps de construire des trajectoires à partir de données 'brutes' (dans l'onglet 'les données').

Trois types de fichiers peuvent être chargés dans l'application :

 - un fichier .csv avec des données au format 'wide' : une colonne par variable et par date, et à la suite une colonne par variable complémentaire.    
 
 - un fichier .Rdata comportant une liste (`list()`) avec un objet 'trajectoire' créé par la fonction `seqdef()` et une data.frame comportant des données complémentaires. Dans ce cas, les noms de ligne (`row.names()`) des trajectoires doivent correspondre avec une colonne de variable d'identifiant individuel dans le data.frame.    
 
 - un fichier .Rdata comportant une liste ('list()') avec pour chaque date (chaque mois, chaque année, chaque jour, etc) un data.frame avec les valeurs correspondantes. Dans ce cas, les trajectoires pourront être générées à partir de n'importe quel nom de variable commun aux différentes dates.  
 
A partir de ces différentes sources, certaines possibilités de sélection d'individus sont offertes. Chaque sélection constituée dans l'application et qui a donné lieu à la constitution de trajectoires peut être enregistrée localement et réutilisée.

2. Indicateurs statistiques :
Une fois que les trajectoires sont construites et validées différents onglets permettent d'obtenir des indicateurs statistiques sur les trajectoires et les données complémentaires.

Les taux de transition : calcul des taux de transitions entre états selon différents paramètres.
Les principaux indicateurs de base fournis dans le package R 'TraMineR', croisés avec les données complémentaires (chronogramme des états, temps moyen passé dans chaque état, événements, états modaux, représentations en tapis, graphiques de flux, etc.
Des tableaux de donnés (tris à plat ou tableau croisés) sur l'ensemble des données générées, y compris les données complémentaires.
3. Classification des trajectoires :
L'application permet aussi de générer des classes de trajectoires à partir de méthodes basiques (Optimal Matching et ses dérivés principalement). Néanmoins, pour une classification fine et pleinement paramétrable, il serais préférable d'utiliser directement les diverses fonctions existantes dans une interface R classique.
