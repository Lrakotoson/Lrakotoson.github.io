

# **Manipulation de données - fonctions usuelles de R**

## 1. **Données chd**


Le fichier **chd.csv** renseigne, sur n = 100 individus, l’âge de l’individu (variable age) et le fait qu’il soit porteur d’une maladie ou non (variable nommée **chd**, codée 1 pour oui, 0 pour non). On s’intéresse à l’influence de l’âge sur le fait d’être porteur ou non de la maladie. L’objet de l’exercice est en particulier de former des classes d’âge (sous forme d’intervalles contigus) et de calculer la proportion de malades dans chacune des classes. On cherche aussi à représenter graphiquement la situation et les résultats obtenus.

Plus généralement, ce type de données peut être utilisé pour essayer de voir si la variable âge (quantitative) permet d’expliquer l’appartenance à l’une des modalités d’une variable binaire (ici variable chd de modalités 1 ou 0). C’est un problème typique de la régression dite logistique. Nous ne rentrons pas ici dans la méthode. Cependant, précisons pour information que la dernière question consiste à appliquer les résultats d’un modèle logistique aux données.

### 1.1 Exploitation des données


1.  Importer et calculer le résumé des données.

``` r
donnees <- read.csv("chd.csv", sep = ";")
summary(donnees)
```

    ##       age             chd      
    ##  Min.   :20.00   Min.   :0.00  
    ##  1st Qu.:34.75   1st Qu.:0.00  
    ##  Median :44.00   Median :0.00  
    ##  Mean   :44.38   Mean   :0.43  
    ##  3rd Qu.:55.00   3rd Qu.:1.00  
    ##  Max.   :69.00   Max.   :1.00

2.  Calculer la moyenne de la variable chd. A quoi correspond cette valeur ?

``` r
mean(donnees$chd)
```

    ## [1] 0.43

3.  La variable chd est interprétée comme quantitative : ajouter aux données une colonne appelée *chd.quali* qui contient la conversion de la variable chd en facteur (fonction **factor()**). Recalculer le résumé des données et retrouver ainsi que 43% des individus sont porteurs de la maladie.

``` r
chd.quali = factor(donnees$chd, levels = c(0,1), labels = c("sain", "malade"))
donnees = cbind(donnees, chd.quali)
summary(donnees)
```

    ##       age             chd        chd.quali 
    ##  Min.   :20.00   Min.   :0.00   sain  :57  
    ##  1st Qu.:34.75   1st Qu.:0.00   malade:43  
    ##  Median :44.00   Median :0.00              
    ##  Mean   :44.38   Mean   :0.43              
    ##  3rd Qu.:55.00   3rd Qu.:1.00              
    ##  Max.   :69.00   Max.   :1.00

4.  Calculer la moyenne d’âge d’un individu malade, d’un individu sain.

``` r
by(donnees$age,donnees$chd.quali,mean)
```

    ## donnees$chd.quali: sain
    ## [1] 39.17544
    ## -------------------------------------------------------- 
    ## donnees$chd.quali: malade
    ## [1] 51.27907

5.  On se propose de former des classes d’âge puis de calculer la proportion d’individus malades dans chaque classe.

<!-- -->

**a.**  Utiliser la fonction **cut()** pour regrouper les valeurs d’âge dans les intervalles suivants : <br> **\[20, 29\]  \]29, 34\]  \]34, 39\]  \]39, 44\]  \]44, 49\]  \]49, 45\]  \]54, 59\]  \]59, 69\]**

Indications :<br> + On s’assurera que tous les individus sont bien associés.<br> + On pourra, comme à la question 2, créer une colonne supplémentaire dans le jeu de données, appelée par exemple *age.quali*, qui recevra la classe d’appartenance de chaque individu.

``` r
age.quali = cut(donnees$age, b=c(20,29,34,39,44,49,54,59,69), right = T, include.lowest = T)
donnees = cbind(donnees, age.quali)
by(donnees$chd, donnees$age.quali, mean) #moyenne de malades par age.quali
```

    ## donnees$age.quali: [20,29]
    ## [1] 0.1
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (29,34]
    ## [1] 0.1333333
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (34,39]
    ## [1] 0.25
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (39,44]
    ## [1] 0.3333333
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (44,49]
    ## [1] 0.4615385
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (49,54]
    ## [1] 0.625
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (54,59]
    ## [1] 0.7647059
    ## -------------------------------------------------------- 
    ## donnees$age.quali: (59,69]
    ## [1] 0.8

**b.**  Calculer la proportion d’individus malades dans chaque classe d’âge.

``` r
table(donnees$chd.quali, donnees$age.quali) #effectifs par age.quali
```

    ##         
    ##          [20,29] (29,34] (34,39] (39,44] (44,49] (49,54] (54,59] (59,69]
    ##   sain         9      13       9      10       7       3       4       2
    ##   malade       1       2       3       5       6       5      13       8

----------
### 1.2 Graphe


1.  Représenter dans un plan les données d’âge en abscisse et celle de la variable quantitative chd en ordonnée, sans oublier de légender les axes.

``` r
plot(x = donnees$age, y = donnees$chd, xlab = "age", ylab = "chd")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-7-1.png)

2.  Ajouter par un trait horizontal en pointillés, la proportion des individus malades (sur l’ensemble des données).

``` r
plot(x = donnees$age, y = donnees$chd, xlab = "age", ylab = "chd") # Question 1
abline(h = mean(donnees$chd), col ="blue", lty = "dotted")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-8-1.png)

3.  On souhaite ajouter à̀ ce graphe une représentation de la proportion d’individus malades par classe d’âge. Pour cela, on pourra tracer des segments horizontaux de couleur rouge, de bornes correspondantes aux extrémités des classes d’âge, et les placer à la hauteur correspondant à la proportion d’individus malades dans la classe.

``` r
plot(x = donnees$age, y = donnees$chd, xlab = "age", ylab = "chd") # Question 1
abline(h = mean(donnees$chd), col ="blue", lty = "dotted") # Question 2
##proportion de malade par classe d'age
segments(x0 = c(20,29,34,39,44,49,54,59),
         y0 = c(by(donnees$chd, donnees$age.quali, mean)),
         x1 = c(29,34,39,44,49,54,59,69),
         y1 = c(by(donnees$chd, donnees$age.quali, mean)),
         col = "red")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-9-1.png)

4.  On considère la fonction f : R → \[0, 1\] telle que
    $$f(x)=\\frac{exp(B\_1 + B\_2 x)} {1 + exp(B\_1 + B\_2 x)}$$
     Représenter sur le graphe la courbe de f pour *B*<sub>1</sub>= −5, 30 et *B*<sub>2</sub> = 0, 11. Cette fonction est le résultat de l’application d’un modèle logistique aux données : *f*(*x*) donne une estimation de la probabilité qu’un individu soit porteur de la maladie à l'âge x.

``` r
plot(x = donnees$age, y = donnees$chd, xlab = "age", ylab = "chd") # Question 1
abline(h = mean(donnees$chd), col ="blue", lty = "dotted") # Question 2
segments(x0 = c(20,29,34,39,44,49,54,59), y0 = c(by(donnees$chd, donnees$age.quali, mean)), x1 = c(29,34,39,44,49,54,59,69), y1 = c(by(donnees$chd, donnees$age.quali, mean)), col = "red") #Question 3

#_________________________________________________________________
x <- seq(min(donnees$age), max(donnees$age))
lines(x,(exp(-5.3 + 0.11*x))/(1+exp(-5.3 + 0.11*x)), col = "blue")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-10-1.png)

## 2. **Température de surface de l'océan**

Vous disposez de 2 jeux de données de températures de surface de l’océan (en degré Celsius) : données observées et données modélisées. Elles se trouvent dans les fichiers **observations.csv** et **modelise.csv** respectivement. Ces fichiers contiennent des données SST pour 5 régions océaniques proches de la Bretagne : dans la Manche au nord (Manche Nord), et au sud (Manche Sud), en Mer d’Iroise au large (Mer Iroise Zone Large), ou près de la côte (Mer Iroise Zone Côtière) et enfin dans l’Atlantique au large des côtes du Morbihan (Plateau Armoricain). Ces régions sont localisées sur la figure ci-dessous :<br> <img src="resume_rgit_files/figure-markdown_github/temperature_ocean.jpg" alt="Température océan" style="width:50.0%" />

**Données observées** Chaque groupe possède les données de Mer Iroise Zone Large et celles d’une autre région. Il y a 5 colonnes :
<ul>
<li>
année,
</li>
<li>
mois (numéroté de 1 à 12),
</li>
<li>
jour (numéroté de 1 à 31),
</li>
<li>
SST pour Mer Iroise Zone Large
</li>
<li>
SST de l’autre région
</li>
</ul>
Ce sont des données satellites à 5km de résolution moyennée spatialement, au pas de temps journalier de 1986 à 2013.

**Données modélisées** Ce sont des données issues de 3 modèles : CNRM ; MPILR MPIMR à différentes résolutions. Ce sont des valeurs, moyennées sur quelques points qui correspondent à la Mer Iroise Zone Large dans chacun des modèles. Les données sont au pas de temps journalier de 1980 à 2005.

2.1 Importation et préparation des données
------------------------------------------

1.  Importer dans R les données. On conservera les colonnes jour, mois et année au format numérique.

``` r
observations <- read.csv2("observations.csv")
modelise <- read.csv2("modelise.csv")
```

2.  Dans les données d’observations, changer le nom des deux dernières colonnes : *SST MerIroiseZL* en region1 et *SST Plateau Armor* en region2.

``` r
names(observations) #pour connaitre les noms de colonnes
```

    ## [1] "annee"            "mois"             "jour"            
    ## [4] "SST_MerIroiseZL"  "SST_PlateauArmor"

``` r
names(observations)[4] <- "region1"
names(observations)[5] <- "region2"
names(observations) #pour vérifier
```

    ## [1] "annee"   "mois"    "jour"    "region1" "region2"

3.  Dans les données modélisées, changer le nom des trois dernières colonnes en les nommant : model1, model2 et model3.

``` r
names(modelise)
```

    ## [1] "jour"  "mois"  "annee" "CNRM"  "MPILR" "MPIMR"

``` r
names(modelise)[(ncol(modelise)-2):ncol(modelise)] <- c("model1","model2","model3")
names(modelise)
```

    ## [1] "jour"   "mois"   "annee"  "model1" "model2" "model3"

4.  Ajouter aux deux data frame de données une colonne intitulée date résultant de la chaîne de caractères obtenue en “collant” les informations d’année, de mois, de jour, dans cet ordre.<br> **NB. La date servira de clé de jointure pour la fusion des tableaux ; il est préférable que le nom de la variable date soit identique dans les deux data frame**

``` r
modelise$date <- paste(modelise$jour, modelise$mois, modelise$annee, sep = "/")
observations$date <- paste(observations$jour, observations$mois, observations$annee, sep = "/")
```

5.  Dans chaque data frame créer une variable intitulée saison, à 4 modalités,
    <ul>
    <li>
    H associée aux mois de décembre, janvier et février
    </li>
    <li>
    P associée aux mois de mars, d’avril et mai
    </li>
    <li>
    E associée aux mois de juin, juillet et août
    </li>
    <li>
    A associée aux mois de septembre, octobre et novembre
    </li>
    </ul>

``` r
#Méthode 1 ___________ Observations Data
observations$saison <- factor(observations$mois, labels = c("H","H",
                                                            "P","P","P",
                                                            "E","E","E",
                                                            "A","A","A",
                                                            "H"))
```

``` r
#Méthode 2 ___________ Modelise Data
modelise$saison <- rep("default",nrow(modelise))
modelise$saison[modelise$mois==12 | modelise$mois== 1 | modelise$mois== 2] <- "H"
modelise$saison[modelise$mois== 3 | modelise$mois== 4 | modelise$mois== 5] <- "P"
modelise$saison[modelise$mois== 6 | modelise$mois== 7 | modelise$mois== 8] <- "E"
modelise$saison[modelise$mois== 9 | modelise$mois==10 | modelise$mois==11] <- "A"
modelise$saison <- factor(modelise$saison)
```

2.2 Fusion des tableaux
-----------------------

L’opération de fusion de tableau est toujours une opération délicate : il faut avancer avec prudence et autant que possible mettre en place quelques procédures de vérifications. Les dates de mesures ne sont pas identiques dans les deux tableaux. Cependant, les deux tableaux possèdent des dates communes. Nous souhaitons créer un tableau qui comporte toutes les colonnes (i.e. les colonnes des deux tableaux) avec comme informations les lignes correspondant aux dates communes de mesures.

1.  L’opérateur **%in%** qui s’applique à deux vecteurs **A** et **B** selon la syntaxe **A *%in%* B** renvoie un vecteur de booléens, de longueur identique à la longueur du vecteur **A**, précisant l’appartenance ou non de chaque coordonnée de **A** à l’ensemble défini par les coordonnées de **B**.

``` r
A <- c(1,2,5,30)
B <- 1:10
A %in% B
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
B %in% A
```

    ##  [1]  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE

2.  Déterminer le nombre de lignes dans chacun des deux tableaux. Utiliser l’opérateur défini en question précédente pour déterminer le nombre de dates communes dans les deux tableaux.

``` r
nrow(modelise) #nombre de lignes dans modelise
```

    ## [1] 9490

``` r
nrow(observations) #nombre de lignes dans observations
```

    ## [1] 10220

``` r
table(modelise$date %in% observations$date) #même nombre de TRUE que:
```

    ## 
    ## FALSE  TRUE 
    ##  2190  7300

``` r
table(observations$date %in% modelise$date)
```

    ## 
    ## FALSE  TRUE 
    ##  2920  7300

3.  Faire fusionner les deux tableaux dans un tableau résultat appelé **tab.merge** en utilisant date comme clé de jointure. Assurez-vous que le nombre de lignes est bien celui attendu.

``` r
tab.merge <- merge(modelise,observations[,-c(1:3,7)],by="date")
# [,-c(1:3,7)] supprime les redondances des trois premières colonnes (jours, mois, années) et de la septième (saison)
```

4.  Identifier quelques lignes des deux tableaux initiaux qui doivent être présentes dans le tableau fusionné. En revenant aux données, vérifier, sur quelques individus, que le tableau fusionné a pris les bonnes informations.

5.  Certaines colonnes du tableau fusionné sont redondantes : vérifiez-le puis supprimez les redondances.

``` r
#Cette correction a été faite à la question 3
#La suppression de la colonne jour.y de tab.merge s'effectue de la manière suivante:
tab.merge$jour.y <- NULL 
```

2.3 Statistiques descriptives
-----------------------------

Nous travaillons bien sûr désormais sur le tableau fusionné.

1.  Présenter les boxplots des 3 variables modélisées toutes dates confondues.

``` r
boxplot(tab.merge$model1, tab.merge$model2, tab.merge$model3,
        names = c("M1", "M2", "M3"), ylab = "température")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-21-1.png)

2.  Présenter les boxplots des 3 variables modélisées, saison par saison.

``` r
boxplot(model1~saison, data = tab.merge, ylab = "température") # pour le 1er modèle
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-22-1.png)

3.  Présenter les boxplots des 3 variables modélisées, mois par mois.

``` r
boxplot(model1~mois, data = tab.merge, ylab = "température") # pour le 1er modèle
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-23-1.png)

4.  Calculer les résumés numériques classiques des variables modélisées toutes dates confondues.

``` r
summary(tab.merge$model1) # pour le 1er modèle
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.755  10.400  12.358  13.021  15.597  20.211

5.  Calculer les résumés numériques classiques des variables modélisées saison par saison.

``` r
#Méthode 1 _______by() Saison
by(tab.merge$model1, tab.merge$saison, summary) # pour le 1er modèle
```

    ## tab.merge$saison: A
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.36   13.26   14.59   14.71   16.06   19.72 
    ## -------------------------------------------------------- 
    ## tab.merge$saison: E
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.01   14.62   16.54   16.21   17.74   20.21 
    ## -------------------------------------------------------- 
    ## tab.merge$saison: H
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.876  10.019  10.709  10.715  11.307  13.317 
    ## -------------------------------------------------------- 
    ## tab.merge$saison: P
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.755   9.663  10.134  10.419  10.919  14.064

6.  Calculer les résumés numériques classiques des variables modélisées mois par mois.

``` r
#Méthode 2 _______tapply() summary par mois
tapply(tab.merge$model1, tab.merge$mois, summary) # pour le 1er modèle
```

    ## $`1`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    9.22   10.15   10.56   10.61   10.88   12.66 
    ## 
    ## $`2`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.876   9.475   9.836   9.868  10.197  11.433 
    ## 
    ## $`3`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.755   9.386   9.618   9.660   9.978  10.792 
    ## 
    ## $`4`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   9.189   9.689  10.001  10.085  10.441  12.830 
    ## 
    ## $`5`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   9.702  10.765  11.444  11.502  12.108  14.064 
    ## 
    ## $`6`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.01   13.06   13.82   14.00   14.75   17.66 
    ## 
    ## $`7`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   13.67   15.90   16.75   16.84   17.80   19.89 
    ## 
    ## $`8`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   15.15   16.91   17.68   17.73   18.46   20.21 
    ## 
    ## $`9`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   14.58   15.83   16.58   16.55   17.11   19.72 
    ## 
    ## $`10`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   12.94   13.97   14.57   14.66   15.23   17.66 
    ## 
    ## $`11`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.36   12.40   12.81   12.90   13.32   14.68 
    ## 
    ## $`12`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   10.53   11.16   11.47   11.59   11.91   13.32



## 3. **Traitement de valeurs manquantes : données bebe**


Le jeu de données **bebe.txt** renseigne des variables mesurées dans une maternité, les individus sont ici les naissances (ou les bébés). Certaines données ne sont pas renseignées. Il s’agit dans cet exercice de repérer les individus pour lesquels au moins une variable n’est pas renseignée dans le but d’éliminer ces individus avant analyse.

3.1 Importation, prise en main des données
------------------------------------------

1.  Importer les données.

``` r
bebe <- read.csv("bebe.txt", sep=";")
```

2.  Calculer le résumé des données. Quelles informations donne la fonction **summary()** quant aux valeurs manquantes ?

``` r
summary(bebe) # summary() donne le nombre de NA
```

    ##      Nbsem         Sexe        PoidsBB        TailleBB    PoidsPlacenta   
    ##  Min.   :33.00   F   :242   Min.   :1810   Min.   :42.0   Min.   : 210.0  
    ##  1st Qu.:39.00   M   :255   1st Qu.:3030   1st Qu.:48.0   1st Qu.: 510.0  
    ##  Median :40.00   NA's:  1   Median :3300   Median :50.0   Median : 572.5  
    ##  Mean   :39.53              Mean   :3303   Mean   :49.4   Mean   : 593.8  
    ##  3rd Qu.:40.00              3rd Qu.:3588   3rd Qu.:51.0   3rd Qu.: 662.5  
    ##  Max.   :43.00              Max.   :5030   Max.   :56.0   Max.   :1200.0  
    ##                                            NA's   :10     NA's   :30      
    ##     Operant    JourNaiss    SitMat     AgedelaMere      NaissMere    
    ##  Medecin:134   diM:67    celiba: 43   Min.   :19.00   Min.   :50.00  
    ##  SF     :364   jeu:87    conc  :160   1st Qu.:26.00   1st Qu.:63.00  
    ##                lun:66    Mariee:286   Median :28.00   Median :66.00  
    ##                Mar:73    NA's  :  9   Mean   :28.58   Mean   :65.42  
    ##                Mer:51                 3rd Qu.:31.00   3rd Qu.:68.00  
    ##                saM:72                 Max.   :44.00   Max.   :75.00  
    ##                ven:82                                                
    ##     TailMere       PoidsMere        Agedupere        NaisPere    
    ##  Min.   :145.0   Min.   : 36.00   Min.   :21.00   Min.   :41.00  
    ##  1st Qu.:159.0   1st Qu.: 50.00   1st Qu.:28.00   1st Qu.:59.00  
    ##  Median :163.0   Median : 56.00   Median :30.00   Median :64.00  
    ##  Mean   :162.9   Mean   : 57.61   Mean   :31.53   Mean   :62.47  
    ##  3rd Qu.:167.0   3rd Qu.: 62.00   3rd Qu.:35.00   3rd Qu.:66.00  
    ##  Max.   :186.0   Max.   :117.00   Max.   :53.00   Max.   :73.00  
    ##  NA's   :66      NA's   :39       NA's   :8       NA's   :8      
    ##     TailPere       PoidsPere        NbGrossess       NbEnfants     
    ##  Min.   :158.0   Min.   : 48.00   Min.   : 1.000   Min.   : 1.000  
    ##  1st Qu.:171.0   1st Qu.: 65.00   1st Qu.: 1.000   1st Qu.: 1.000  
    ##  Median :175.0   Median : 72.00   Median : 2.000   Median : 2.000  
    ##  Mean   :175.7   Mean   : 74.33   Mean   : 2.226   Mean   : 1.847  
    ##  3rd Qu.:180.0   3rd Qu.: 80.00   3rd Qu.: 3.000   3rd Qu.: 2.000  
    ##  Max.   :194.0   Max.   :710.00   Max.   :13.000   Max.   :11.000  
    ##  NA's   :75      NA's   :76       NA's   :25       NA's   :26      
    ##      NbIVG             NbFC             TypeAllait       ModeAccouc 
    ##  Min.   :0.0000   Min.   :0.0000   Artificiel:313   Autre     :  4  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   Maternel  :182   Cesarienne: 76  
    ##  Median :0.0000   Median :0.0000   NA's      :  3   forceps   : 39  
    ##  Mean   :0.1423   Mean   :0.1687                    VBS       :379  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000                                    
    ##  Max.   :2.0000   Max.   :5.0000                                    
    ##  NA's   :6        NA's   :6                                         
    ##       ModeTravai  Peridurale   DureeTrava        IMCMere      PoidsQuart
    ##  arti      : 67   non :156   Min.   : 0.000   Min.   :15.24   P1:126    
    ##  autre     :  6   oui :338   1st Qu.: 3.000   1st Qu.:19.27   P2:125    
    ##  Cesarienne: 31   NA's:  4   Median : 5.000   Median :20.83   P3:122    
    ##  ocytosine : 51              Mean   : 5.682   Mean   :21.69   P4:125    
    ##  spontane  :334              3rd Qu.: 7.000   3rd Qu.:23.01             
    ##  NA's      :  9              Max.   :60.000   Max.   :48.70             
    ##                              NA's   :1        NA's   :69

3.  Paramétrer convenablement l’appel de la fonction **mean()** de sorte à calculer la taille moyenne d’un bébé.

``` r
mean(bebe$TailleBB, na.rm = T) # sans considérer les NA ou
```

    ## [1] 49.40061

``` r
mean(na.omit(bebe$TailleBB)) # en supprimant les lignes avec NA
```

    ## [1] 49.40061

3.2 Repérage des individus non totalement renseignés
----------------------------------------------------

1.  Combien y-a-t’il de données manquantes ?

``` r
table(is.na(bebe)) # nombre de TRUE ou
```

    ## 
    ## FALSE  TRUE 
    ## 12975   471

``` r
sum(is.na(bebe)) # somme des TRUE
```

    ## [1] 471

2.  En combinant les fonctions **is.na()**, **which()** *(préciser le paramètre arr.ind)* et **unique()**, déterminer les individus non totalement renseignés.

``` r
#Méthode 1____________ partie de data frame
manque <- unique( bebe [which(is.na(bebe), arr.ind = T) [,1] ,] )
```

3.  Retrouver le résultat précédent au moyen d’une boucle sur les colonnes du tableau.

``` r
#Méthode 2____________ boucle for et conditionnelle if
manque <- data.frame() # data frame de réception
for (i in 1:ncol(bebe)) {
  if ( length( which( is.na(bebe[,i]) ) ) > 0 ){
    manque <- unique( rbind( manque, bebe[ which( is.na(bebe[,i]) ),]) )
  }
}
```

3.3 Export des données “nettoyées”
----------------------------------

Etant donné le grand nombre d’individus concernés, les enlever serait sans trop doute trop radical. Mais pour l’exercice, nous construisons un jeu de données “nettoyé” et l’exportons.

1.  Créer le tableau regroupant les individus totalement renseignés en utilisant les résultats obtenus.

``` r
#Méthode 1____________ suppression par ligne
newBebe <- bebe[ - as.integer(row.names(manque)),]
```

2.  Obtenir le même tableau de manière directe en utilisant la fonction **na.omit()**.

``` r
#Méthode 2____________ suppression des lignes contenant NA
newBebe <- na.omit(bebe)
```

3.  Exporter le tableau ainsi nettoyé au moyen de la fonction **write.table()**.

``` r
write.table(newBebe, "newBebe.txt")
```

<br><br>

------------------------------------------------------------------------
------------------------------------------------------------------------

# **Statistique inférentielle**

------------------------------------------------------------------------
------------------------------------------------------------------------

## 4. **Simulation avec R**

En statistique inférentielle, on est amené à examiner les propriétés théoriques d’estimateurs (i.e. de variables aléatoires fonction des données) pour juger de leur capacité à bien estimer, dans le cadre de l’estimation paramétrique par exemple, le(s) paramètre(s) inconnu(s) de la loi postulée par le modèle. Ainsi, si on prouve qu’un estimateur a de bonnes propriétés (consistance, faible biais, faible variance...), on a une plus grande confiance en l’estimation du paramètre, estimation qui correspond à la seule réalisation de cette variable aléatoire dont on dispose.

Cependant, étudier les propriétés de telles variables n’est pas toujours simple. En outre, dans certains cas, il peut être utile de tenter de vérifier empiriquement ces propriétés avant de se lancer dans leur étude théorique.

Le logiciel R intègre ainsi un générateur de nombres aléatoires. Plusieurs fonctions prédéfinies permettent ainsi de générer des nombres aléatoires selon la loi usuelle souhaitée. Nous les utilisons dans cette section pour illustrer deux résultats fondateurs en probabilité (Loi des grands nombres, Théorème central limite) et revenir sur le sens de quelques éléments classiques en estimation (estimation par intervalles, biais-variance...).

4.1 Fonctions de base pour générer des nombres aléatoires
---------------------------------------------------------

1.  Commenter les instructions suivantes:<br>

fonction **sample()**

``` r
sample(100) # Tirage sans remise de 100 nombres de 0 à 100.
```

    ##   [1]  15 100  47  90  14   8  67  45   5  39   9  64  18  29  58  70  36
    ##  [18]  59  69  72  56  79  52  61   7  27  71  75  55  83  21  26  95  76
    ##  [35]  37  28  98  85  48  19  94  49  91  34  96  31  78  86  38  44  63
    ##  [52]  46   4  62  74  80  43  92  35  40  99  22  12  88   6  20  97  13
    ##  [69]   3  65  41  66  82  24  60  50  30  81  16  32  25  23  77  53   1
    ##  [86]  73  42  87  57  33  93  68   2  54  10  89  84  17  51  11

``` r
table(sample(x=1:10, size=100, replace=T)) # Tirage de 100 nombres de 1 à 10 avec remise.
```

    ## 
    ##  1  2  3  4  5  6  7  8  9 10 
    ##  4 12  9 10 11 15 10  7 14  8

fonction **rbinom()**

``` r
rbinom(n = 10, size = 3, prob = 0.5) # échantillon de taille 10 d'une loi binomiale B(3, 0.5)
```

    ##  [1] 0 3 1 1 1 1 2 2 0 0

``` r
rbinom(n = 10, size = 5, prob = 0.2) # échantillon de taille 10 d'une loi binomiale B(5, 0.2)
```

    ##  [1] 0 1 3 1 2 1 1 0 1 3

fonction **rnorm()**

``` r
rnorm(n = 10, mean = 0, sd = 1) # échantillon de taille 10 d'une loi normale N(0, 1)
```

    ##  [1] -0.6847497 -0.1969471  1.0230990  0.3688477 -0.1785399  1.0751883
    ##  [7] -1.4643950  0.4366729 -0.6314237  0.6532715

``` r
rnorm(n = 10, mean = 2, sd = 5) # échantillon de taille 10 d'une loi normale N(2, 5)
```

    ##  [1]  6.751329  4.954189 -1.582016  1.285454 -1.181854  8.419256  3.881823
    ##  [8]  2.790414 -4.633369  2.377050

fonction **runif()**

``` r
U <- runif(n = 1000, min = 0, max = 15)
hist(U, freq = F) # histogramme d'une loi uniforme d'échantillon de taille 1000
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-39-1.png)

2.  Générer des nombres aléatoires en utilisant les fonctions **rpois()** puis **rexp()**.

``` r
rpois(n = 10, lambda = 5) # échantillon de taille 10 d'une loi Poisson P(5)
```

    ##  [1] 6 5 3 4 4 5 4 5 7 5

``` r
rexp(n = 10, rate = 5) # échantillon de taille 10 d'une loi exponentielle E(5)
```

    ##  [1] 0.016471108 0.066240848 0.331300897 0.458191812 0.073373639
    ##  [6] 0.011742616 0.250503151 0.030362501 0.009474237 0.073639105

3.  Générer n = 1000 nombres aléatoires suivant une loi *N*(0, 1).<br>

<!-- -->

4.  Représenter un histogramme de la distribution des valeurs.

``` r
valeurs <- rnorm(1000, 0, 1)
hist(valeurs, freq = F, breaks = 30)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-41-1.png)

5.  Au graphe précédent, ajouter la courbe de densité de la loi *N*(0, 1) (fonctions **dnorm()** et **lines()**).

``` r
hist(valeurs, freq = F, breaks = 30) # Question a

x <- seq(-3,3, length=100)
y <- dnorm(x,0,1)
lines(x,y,col="red")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-42-1.png)

6.  Représenter la fonction de répartition empirique de ces valeurs aléatoires (fonction **ecdf()**).

``` r
Fchap <- ecdf(valeurs)
plot(x, Fchap(x), type = "l")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-43-1.png)

7.  Sur le même graphe, ajouter la courbe de la fonction de répartition de la loi *N*(0, 1) (fonction **pnorm()**).

``` r
Fchap <- ecdf(valeurs)
plot(x, Fchap(x), type = "l") #Question c

lines(x,pnorm(x,0,1), col = "blue")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-44-1.png)

4.2 Génération d’échantillons
-----------------------------

Nous venons de voir comment générer un échantillon (par définition un échantillon est aléatoire) selon une loi particulière. On est souvent intéressé par la moyenne d’un échantillon. En composant la fonction **mean()** avec les fonctions de génération des nombres aléatoires vues au-dessus, cela est très simple. Par exemple : <br>`mean( rnorm( 10, mean=0, sd=1 ) )`<br> On propose dans la suite de générer *K* échantillons de taille *n* selon une loi commune et calculer chacune des *K* moyennes associées, au moyen d’une boucle tout d’abord, puis de manière plus appropriée ensuite.

1.  Au moyen d’une boucle.<br> *M* recueillera les moyennes. <br> On pose *K*=10 et *n*=10

``` r
M <- NULL
K <- 5
n <- 10
for (i in 1:K){
  M[i] <- mean( rnorm(n,0, 1) )
}
print(M)
```

    ## [1] -0.04425004 -0.21311029 -0.08303914 -0.21262615  0.04456302

2.  En combinant les fonctions **matrix()** et **apply()**.<br> *valeurs* est un tirage d'échantillon de taille *K* \* *n*<br> *M* est une matrice où chaque ligne correspond à un échantillon. <br> On calcule la moyenne par ligne de la matrice.

``` r
valeurs <- rnorm(K*n, 0,1)
M <- matrix(valeurs, K, n)
apply( M, 1, mean )
```

    ## [1]  0.3398157 -0.4265161 -0.4280138 -0.5822027  0.2144254

4.3 Loi des grands nombres
--------------------------

On rappelle la **loi faible des grands nombres** :<br> Soit (X<sub>n</sub>)<sub>n∈N </sub>une suite de variables aléatoires d’espérance commune E\[X\] = µ et de variance commune var(X) = σ<sup>2</sup> . Alors:
$$\\forall\\epsilon &lt; 0, \\hspace{20pt} lim\_{n\\to\\infty} P (|\\frac{X\_1+...+X\_n}{n}-\\mu | &gt; \\epsilon)=0$$
 Autrement-dit, pour un échantillon i.i.d. (c’est le cas courant), la probabilité que la (variable aléatoire) moyenne d’échantillon s’éloigne de plus de ε de l’espérance commune E\[X\] converge vers 0 quand *n* → ∞.<br> Il ne s’agit pas ici de calculer la probabilité en question, mais d’en obtenir une valeur approchée en comptant la proportion des moyennes d’échantillon $\\bar{x}=\\frac{x\_1+...+x\_n}{n}$ telles que
$$|\\bar{x}-\\mu|&gt;\\epsilon$$
 On prendra les *X*<sub>*i*</sub> i.i.d. selon une loi normale 𝒩(*μ*, *σ*<sup>2</sup>).<br> 1. Proposer une programmation qui permettent de faire varier les constantes (µ, σ, n, ε et *K* le nombre d’échantillons) au clavier et qui compte, parmi les *K* échantillons, la proportion de ceux dont les moyennes $\\bar{x}=\\frac{x\_1+...+x\_n}{n}$ vérifient:
$$|\\bar{x}-\\mu|&gt;\\epsilon$$

Déclaration des constantes

``` r
n <- 10
K <- 500
mu <- 0
sigma <- 1
epsilon <- 0.2
```

Tirage, avec une boucle, de *K* echantillons de loi 𝒩(*μ*, *σ*) et réception de leur moyenne dans *resultat*

``` r
resultat <- rep(0,K)
for (i in 1:K) {
  echantillon <- rnorm( n, mean = mu, sd = sigma)
  resultat[i] <- mean(echantillon)
}
summary(resultat)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -0.950838 -0.204312  0.005625 -0.001081  0.199229  0.864693

Calcul des pourcentages

``` r
sum( resultat > epsilon )/K
```

    ## [1] 0.25

1.  Faire varier les constantes de sorte à illustrer la loi des grands nombres.<br>
2.  Représenter graphiquement la situation.
    <ul>
    <li>
    Choix d'une grille de valeur de *n* (ici **NN**: 10, 50, 100, 200, 500)
    </li>
    <li>
    Initialisation du graphique (entre -1 et 1 sur cette grille de *n*)
    </li>
    <li>
    Pour chaque valeur de *n*, tirage de *K* = 500 echantillons dont la moyenne est calculée et représentation en *points*
    </li>
    <li>
    Rajout des droites horizontales
    </li>

``` r
NN <- c( 10, 50, 100, 200, 500)

plot( rep( NN, each=2 ), rep( c( -1, 1 ), length(NN) ), type="n", xlab="n", ylab="Moyenne empirique")

for (n in 1:length(NN) ) {
  resultat <- rep( 0, K )
  
  for (i in 1:K) {
    echantillon <- rnorm( NN[n], mean = mu, sd = sigma)
    resultat[i] <- mean(echantillon)
  }
  
  summary (resultat)
  points( rep( NN[n], K ), resultat, col = n ) 
}


abline(h=c(mu,mu+epsilon,mu-epsilon),col=c("black","grey70","grey70"))
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-50-1.png)

**Remarque**\* Dans cet exercice, nous admettons que la proportion des moyennes d’échantillons telles que $|\\bar{x}-\\mu|&gt;\\epsilon$ est proche de $P(|\\bar{X}-\\mu|&gt;\\epsilon)$ . Nous l’admettons car c’est un résultat conforme à l’intuition. Cependant sa justification relève... de la loi des grands nombres...

4.4 Théorème Central Limite
---------------------------

On rappelle le théorème : Soit (*X*<sub>*n*</sub>) une suite de variables aléatoires i.i.d. selon une loi commune *X* d’espérance *μ* et de variance *σ*<sup>2</sup> . Alors :
$$\\frac{1}{\\sqrt{n}}(\\frac{X\_1+...X\_n - n\\mu}{\\sigma})\\to\_{\\mathcal{L}}\\mathcal{N}(0,1)\\hspace{50pt}(1)$$

1.  Vérifier que la variable aléatoire dans l’équation (1) peut s’écrire $\\frac{\\bar{X}-\\mu}{\\sigma/\\sqrt{n}}$<br>
2.  En pratique, pour n assez grand, par quelle loi peut-on approcher la loi de $\\bar{X}$ ?<br>

<h4>
Illustration avec *X*~ 𝒰<sub>\[*a*, *b*\]</sub>
</h4>
1.  Rappeler les expressions de **E\[X\]** et **var(X)** pour *X*~ 𝒰<sub>\[*a*, *b*\]</sub><br>
2.  Générer *K* moyennes d’échantillon *X*<sub>1</sub>, . . . , *X*<sub>*n*</sub> avec les *X*<sub>*i*</sub> i.i.d. selon la loi 𝒰<sub>\[*a*, *b*\]</sub>.

``` r
# Affectations paramètres
a <- 0
b <- 1
mu <- (b+a)/2
sigma <- sqrt((b-a)^2/12)
n <- 10
K <- 500

# Tirages des echantillons et calculs de moyenne
echantillon <- matrix( runif( n*K, a, b), nrow = K, ncol = n)
resultat <- apply( echantillon, 1, mean)
```

1.  Sur un même graphe représenter la distribution de ces *K* moyennes d’échantillon et la distribution théorique par laquelle on peut l’approcher.

``` r
x <- seq ( mu-3.5*sigma/sqrt(n), mu+3.5*sigma/sqrt(n), length = 1000)
y <- dnorm( x, mean = mu, sd = sigma/sqrt(n) )

hist( resultat, freq = F, main = "Illustration du TCL", xlab = expression(bar(X)), ylab = "densite")

lines( x, y, col = "red") # distribution théorique
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-52-1.png)

4.5 Notion d’intervalle de confiance
------------------------------------

### Données *Nile*

Les données Nile sont disponibles sous **R**. Elles donnent la valeur du débit annuel du Nil, entre 1871 et 1970, à Assouan.

1.  Quel est le débit moyen, mesuré sur ces 100 années ? Quel est l’écart-type ?

``` r
mean(Nile) # débit moyen
```

    ## [1] 919.35

``` r
sd(Nile) # écart-type
```

    ## [1] 169.2275

1.  En utilisant la fonction **t.test()**, obtenir un intervalle de confiance pour le débit moyen, au niveau de confiance 95%, au niveau de confiance 99%.

``` r
t.test(Nile, conf.level = 0.95)$conf.int
```

    ## [1] 885.7716 952.9284
    ## attr(,"conf.level")
    ## [1] 0.95

``` r
t.test(Nile, conf.level = 0.99)$conf.int
```

    ## [1] 874.904 963.796
    ## attr(,"conf.level")
    ## [1] 0.99

1.  Vérifier que les bornes des intervalles de confiance sont données par:
    $$\\bar{x}\\pm\\frac{\\hat{\\sigma}}{\\sqrt{n}}t\_{n-1}(1-\\alpha/2)$$

``` r
#Pour un niveau de confiance à 95%
#Les bornes sont
inferieur <- mean(Nile) - (sd(Nile) * qt( 0.975, df = length(Nile) - 1) )/sqrt( length(Nile) )
inferieur
```

    ## [1] 885.7716

``` r
superieur <- mean(Nile) + (sd(Nile) * qt( 0.975, df = length(Nile) - 1) )/sqrt( length(Nile) )
superieur
```

    ## [1] 952.9284

1.  Quelles conditions supposent l’emploi de ces formules ?

### Données simulées

1.  Générer K échantillons de taille *n*, i.i.d. selon une loi 𝒩(*μ*, *σ*<sup>2</sup>)
2.  Pour chacun d'eux, calculer et stocker les bornes d'intervalle de confiance pour *μ* au niveau de confiance 95%

``` r
K <- 1000
n <- 10
mu <- 0
sigma <- 1
```

``` r
InBornes <- rep(FALSE, K)
for (i in 1:K) {
  echantillon <- rnorm(n, mean = mu, sd = sigma)
  bornes <- t.test(echantillon, level=0.95)$conf.int
  InBornes[i] <- (mu <= bornes[2]) & (mu >= bornes[1])
}
table(InBornes)
```

    ## InBornes
    ## FALSE  TRUE 
    ##    56   944

1.  Calculer la proportion des intervalles contenant *μ*

``` r
sum(InBornes)/length(InBornes)
```

    ## [1] 0.944

1.  Représenter la situation

``` r
barplot(table(InBornes)/length(InBornes))
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-59-1.png)

4.6 Quelques tests
------------------

### Introduction par la simulation

A l'approche d'une élection mettant en concurrence 2 candidats, un sondage a été réalisé sur 1 000 personnes: celui-ci donne 48,2% d'intentions de votes pour le candidat *A*. En notant 𝓅<sub>*A*</sub> la proportion d'électeurs à voter pour *A* dans la population, on se propose d'examiner l'hypothèse:
ℋ<sub>0</sub> : 𝓅<sub>*A*</sub> = 0.5

1.  Commenter les instructions suivantes:

``` r
table(rbinom(n = 1000, size = 1, prob = 0.8)) # Effectifs de succès et d'échec d'une loi de Bernoulli B(0.8)
table(rbinom(n = 1000, size = 1, prob = 0.8))/1000 # Proportion de succès et d'échec ... de Bernoulli B(0.8)
table(rbinom(n = 1000, size = 1, prob = 0.8))[2]/1000 # Proportion de succès d'une loi de Bernoulli B(0.8)
```

1.  En vous aidant des instructions précédentes, simuler les résultats d'un sondage effectué sur 1000 personnes où, dans la population des électeurs, l'hypothèse ℋ<sub>0</sub> : 𝓅<sub>*A*</sub> = 0.5 est vraie

``` r
table(rbinom(n = 1000, size = 1, prob = 0.5))[2]/1000
```

    ##     1 
    ## 0.476

1.  Au moyen d'une boucle (par exemple), simuler (et conserver) les résultats de 100 sondages effectués sur 1000 personnes où, dans la population des électeurs, l'hypothèse ℋ<sub>0</sub> : 𝓅<sub>*A*</sub> = 0.5 est vraie.<br>

*Resultats* : tableau de contingence pour chaque échantillon.

``` r
K <- 100
resultats <- matrix(0, K, 2)
```

Boucle sur les *K* = 100 sondages

``` r
for (k in 1:K){
  echantillon <- rbinom(n = 1000, size = 1, prob = 0.5)
  resultats[k,] <- table(echantillon)
}
```

1.  Evaluer, à l'aide de ces résultats,
    $$\\mathbb{P} (\\hat{\\mathcal{p}\_A} \\leqslant 0.482 | \\mathcal{H}\_0) $$
     Pour les *K* = 100 échantillons, la probabilité d'avoir 1 est estimée par

``` r
resultats[,1]/1000
```

Parmis ces resultats, on calcule la fréquence (sur *K*) des probabilités ≤0.482

``` r
sum( (resultats[,1]/1000) <= 0.482 ) / K
```

    ## [1] 0.08

1.  Comparer ce résultat à la probabilité critique donnée par l'instruction:

``` r
prop.test(x=482, n=1000, p=0.5, alternative = "less")[3] # [3] correspond au p.value dans cette liste
```

    ## $p.value
    ## [1] 0.1341908

### Données **Nile**

On reprend les données **Nile**<br> 1. Tester la normalité des données.

``` r
shapiro.test(Nile)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  Nile
    ## W = 0.97343, p-value = 0.04072

*p-value* n'est pas significatf donc **Nile** se rapproche d'une loi normale (C'est n'est pas *normal*).<br>

1.  On appelle *μ* l'espérance du débit. Tester, au seuil 95% puis 99%, l'hypothèse ℋ<sub>0</sub> : *μ* = 950 contre ℋ<sub>1</sub> : *μ* ≠ 950.<br>

<!-- -->

1.  Test au niveau 95%

``` r
t.test( Nile, mu = 0.950, conf.level = 0.95 )
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Nile
    ## t = 54.27, df = 99, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0.95
    ## 95 percent confidence interval:
    ##  885.7716 952.9284
    ## sample estimates:
    ## mean of x 
    ##    919.35

1.  Test au niveau 99%

``` r
t.test( Nile, mu = 0.950, conf.level = 0.99 )
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Nile
    ## t = 54.27, df = 99, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0.95
    ## 99 percent confidence interval:
    ##  874.904 963.796
    ## sample estimates:
    ## mean of x 
    ##    919.35

1.  Tester, au seuil 95%, l'hypothèse ℋ<sub>0</sub> : *μ* = 950 contre ℋ<sub>1</sub> : *μ* &gt; 950.

``` r
t.test( Nile, mu = 0.950, alternative = "greater", conf.level = 0.95 )
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Nile
    ## t = 54.27, df = 99, p-value < 2.2e-16
    ## alternative hypothesis: true mean is greater than 0.95
    ## 95 percent confidence interval:
    ##  891.2516      Inf
    ## sample estimates:
    ## mean of x 
    ##    919.35

1.  Tester, au seuil 95%, l'hypothèse ℋ<sub>0</sub> : *μ* = 950 contre ℋ<sub>1</sub> : *μ* &lt; 950.

``` r
t.test( Nile, mu = 0.950, alternative = "less", conf.level = 0.95 )
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Nile
    ## t = 54.27, df = 99, p-value = 1
    ## alternative hypothesis: true mean is less than 0.95
    ## 95 percent confidence interval:
    ##      -Inf 947.4484
    ## sample estimates:
    ## mean of x 
    ##    919.35

### Indépendance de deux variables qualitatives

L'instruction suivante permet d'obtenir le tableau croisant la couleur des cheveux et la couleur des yeux pour 592 étudiants:

``` r
donnees <- margin.table(HairEyeColor, c(1,2))
donnees
```

    ##        Eye
    ## Hair    Brown Blue Hazel Green
    ##   Black    68   20    15     5
    ##   Brown   119   84    54    29
    ##   Red      26   17    14    14
    ##   Blond     7   94    10    16

1.  Obtenir les distributions marginales<br> Distribution marginale de la couleur des yeux, **Eye**, la somme des colonnes

``` r
colSums(donnees)
```

    ## Brown  Blue Hazel Green 
    ##   220   215    93    64

Distribution marginale de la couleur des cheveux, **Hair**, la somme des lignes

``` r
rowSums(donnees)
```

    ## Black Brown   Red Blond 
    ##   108   286    71   127

1.  Obtenir les distributions conditionnelles
    </ol>
    *P*(*X* = *i*|*Y* = *j*) est estimée par *n*<sub>*i**j*</sub>/*n*<sub>.*j*</sub>, alors que *P*(*Y* = *j*|*X* = *i*) est estimée par *n*<sub>*i**j*</sub>/*n*<sub>*i*.</sub> <br><br> **Méthode 1: Méthode directe**

``` r
prop.table(donnees, margin = 2)
```

    ##        Eye
    ## Hair         Brown       Blue      Hazel      Green
    ##   Black 0.30909091 0.09302326 0.16129032 0.07812500
    ##   Brown 0.54090909 0.39069767 0.58064516 0.45312500
    ##   Red   0.11818182 0.07906977 0.15053763 0.21875000
    ##   Blond 0.03181818 0.43720930 0.10752688 0.25000000

**Méthode 2: A la main avec sweep**

``` r
nJ <- colSums(donnees)
sweep( donnees, MARGIN = 2, STATS = nJ, FUN = "/")
```

    ##        Eye
    ## Hair         Brown       Blue      Hazel      Green
    ##   Black 0.30909091 0.09302326 0.16129032 0.07812500
    ##   Brown 0.54090909 0.39069767 0.58064516 0.45312500
    ##   Red   0.11818182 0.07906977 0.15053763 0.21875000
    ##   Blond 0.03181818 0.43720930 0.10752688 0.25000000

**Méthode 3: Avec les matrices** <br> On affecte dans une matrice les totaux par colonnes

``` r
nJ <- colSums(donnees)
matriceNj <- matrix(nJ, nrow = nrow(donnees), ncol = ncol(donnees), byrow = TRUE)
matriceNj
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]  220  215   93   64
    ## [2,]  220  215   93   64
    ## [3,]  220  215   93   64
    ## [4,]  220  215   93   64

Et on divise les données par ces sommes

``` r
donnees / matriceNj
```

    ##        Eye
    ## Hair         Brown       Blue      Hazel      Green
    ##   Black 0.30909091 0.09302326 0.16129032 0.07812500
    ##   Brown 0.54090909 0.39069767 0.58064516 0.45312500
    ##   Red   0.11818182 0.07906977 0.15053763 0.21875000
    ##   Blond 0.03181818 0.43720930 0.10752688 0.25000000

1.  Tester l'indépendance des deux variables en présence.

``` r
chisq.test(donnees)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  donnees
    ## X-squared = 138.29, df = 9, p-value < 2.2e-16

1.  Obtenir le tableau des valeurs attendues sours l'hypothèse d'indépendance.
    </ol>
    On calcule la matrice des *n*<sub>.*j*</sub>/*n*

``` r
# Déjà vu plus haut dans la Méthode 3
nJ <- colSums(donnees)
matriceNj <- matrix(nJ, nrow = nrow(donnees), ncol = ncol(donnees), byrow = TRUE)
```

Et la matrice des *n*<sub>*i*.</sub>/*n*

``` r
nI <- rowSums(donnees)
matriceNi <- matrix(nI, nrow = nrow(donnees), ncol = ncol(donnees), byrow=FALSE)
matriceNi
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]  108  108  108  108
    ## [2,]  286  286  286  286
    ## [3,]   71   71   71   71
    ## [4,]  127  127  127  127

La matrice des effectifs attendues $n\\times \\frac{n\_{i.}}{n} \\times \\frac{n\_{.j}}{n}$:

``` r
matriceN <- matriceNi * matriceNj / sum(donnees)
matriceN
```

    ##           [,1]      [,2]     [,3]      [,4]
    ## [1,]  40.13514  39.22297 16.96622 11.675676
    ## [2,] 106.28378 103.86824 44.92905 30.918919
    ## [3,]  26.38514  25.78547 11.15372  7.675676
    ## [4,]  47.19595  46.12331 19.95101 13.729730

1.  La composante **residuals** de l'objet résultant de la fonction **chisq.test()** donne une indication sur les croisements contribuant le plus aux écarts à l'indépendance: quel croisement est le plus représenté par raport à l'indépendance ? Le moins représenté ?

``` r
test <- chisq.test(donnees)
residus <- test$residuals
```

On cherche les résidus les plus faibles (en valeur absolue)

``` r
num <- which( abs(residus) == min( abs(residus) ), arr.ind = T)
num
```

    ##     Hair Eye
    ## Red    3   1

``` r
paste("Hair:", rownames(donnees)[num[1]], "--Eye:", colnames(donnees)[num[2]])
```

    ## [1] "Hair: Red --Eye: Brown"

On cherche les résidus les plus fort (en valeur absolue)

``` r
num <- which( abs(residus) == max( abs(residus) ), arr.ind = T)
num
```

    ##       Hair Eye
    ## Blond    4   2

``` r
paste("Hair:", rownames(donnees)[num[1]], "--Eye:", colnames(donnees)[num[2]])
```

    ## [1] "Hair: Blond --Eye: Blue"

4.7 Biais - Variance
--------------------

L'absence de biais (ou du moins un biais faible) est une des bonnes propriétés que l'on souhaite avoir en estimation. Un estimateur $\\hat{\\theta}$ d'un paramètre *θ* est dit sans biais si $\\mathbb{E}\[\\hat{\\theta}\] = \\theta$. L'espérance mathématique de l'estimateur $\\mathbb{E}\[\\hat{\\theta}\]$ est la moyenne des estimations, sur tous les échantillons possibles. Ainsi, un estimateur est sans biais lorsque, en moyenne, il tombe sur le paramètre. Une autre propriété souhaitable d’un estimateur est une petite variance. La variance d’un estimateur $\\hat{\\theta}$ est $var(\\hat{\\theta}) = \\mathbb{E}\[(\\hat{\\theta} - \\mathbb{E}\[\\hat{\\theta}\])^2\]$. Cette quantité mesure, sur tous les échantillons possibles, la moyenne des carrés des écarts entre les estimations et l’espérance de l’estimateur. Ainsi, lorsqu’un estimateur a une petite variance, c’est qu’il y a peu de variabilité entre les estimations possibles : ainsi l’estimation dont on dispose est proche de celles que l’on aurait avec d’autres jeux de données. Examiner les propriétés de biais et variance des estimateurs suppose donc de calculer des espérances, espérances dont le calcul théorique n’est pas toujours simple. La simulation permettant de générer un grand nombre d’estimations, elle autorise à visualiser la distribution de ces estimations donc à illustrer les propriétés des estimateurs et calculer de manière approchée des espérances.

**Deux estimateurs de variance** Pour une suite de variable *X*<sub>1</sub>, ..., *X*<sub>*n*</sub> i.i.d. selon une loi d'espérance commune *μ* et de variance *σ*<sup>2</sup>. On considère deux estimateurs de la variance commune *σ*<sup>2</sup>:
$$S\_n = \\frac{1}{n}\\sum\_{i = 1}^n(X\_i - \\bar{X})^2 \\qquad \\qquad \\qquad \\tilde{S\_n} = \\frac{1}{n-1}\\sum\_{i = 1}^n(X\_i - \\bar{X})^2$$
 Comparer les propriétés de biais de ces deux estimateurs de la variance, en prenant la loi de votre choix.

### Maximum des lois uniformes

Soit *U*<sub>*i*</sub>, ..., *U*<sub>*n*</sub> des variables aléatoires indépendantes de même loi uniforme sur \[0, *θ*\]. On envisage deux estimateurs de la borne supérieure *θ* de l'intervalle: L'estimateur du maximum de vraisemblance $T\_n^{(1)} = 2\\bar{X}$ et la plus grande valeur de l'échantillon *T*<sub>*n*</sub><sup>(2)</sup> = *s**u**p*(*U*<sub>*i*</sub>, ..., *U*<sub>*n*</sub>).<br> 1. Générer K échantillons de taille *n* = 10 en donnant une valeur à *θ*

``` r
K <- 10000
n <- 10
theta <- 20
resultats <- matrix(0, K, 2)

for (k in 1:K){
  echantillon <- runif(n, max = theta)
  resultats[k,1] <- 2 * mean(echantillon)
  resultats[k,2] <- max(echantillon)
}
```

1.  Visualiser la distribution des K estimations de *θ* avec chacun des deux estimateurs

``` r
colnames(resultats) <- c("T1", "T2") # Nom des colonnes
data.frame(resultats) # Pour un affichage optimal
```

    ##              T1        T2
    ## 1     16.711311 17.272508
    ## 2     22.550235 19.983817
    ## 3     25.205644 19.913868
    ## 4     23.190941 19.422243
    ## 5     19.791498 18.290055
    ## 6     13.557668 13.188663
    ## 7     16.967044 17.419301
    ## 8     14.192071 12.984942
    ## 9     24.999213 18.350979
    ## 10    21.928438 18.026718
    ## 11    16.298091 18.113477
    ## 12    16.490176 15.082385
    ## 13    23.982839 18.242861
    ## 14    23.786247 17.460924
    ## 15    25.693710 19.751193
    ## 16    16.552501 16.583672
    ## 17    19.062952 17.366460
    ## 18    22.550334 18.809866
    ## 19    14.989783 17.550534
    ## 20    16.575451 16.324152
    ## 21    13.855622 17.376042
    ## 22    16.439492 19.180339
    ## 23    24.765117 19.146585
    ## 24    19.134486 17.922333
    ## 25    19.383052 19.536586
    ## 26    19.386276 18.008389
    ## 27    20.201252 19.094691
    ## 28    22.795031 19.313083
    ## 29    14.716252 19.820926
    ## 30    22.422590 18.005506
    ## 31    16.885295 16.996767
    ## 32    14.029353 18.741965
    ## 33    25.513139 19.324969
    ## 34    18.026165 19.176467
    ## 35    20.903388 19.095492
    ## 36    15.031194 14.941261
    ## 37    20.694740 17.656417
    ## 38    19.182736 17.118634
    ## 39    13.140962 14.349405
    ## 40     8.148479 12.315735
    ## 41    18.404064 14.597997
    ## 42    18.580458 16.719336
    ## 43    19.874932 19.436659
    ## 44    15.990332 14.913602
    ## 45    20.377630 19.479623
    ## 46    27.153853 19.043994
    ## 47    16.972180 16.738750
    ## 48    22.967637 19.635573
    ## 49    20.444032 19.063292
    ## 50    17.820946 16.131850
    ## 51    14.083165 12.342103
    ## 52    19.816183 17.749545
    ## 53    22.955501 19.333087
    ## 54    24.835215 19.929165
    ## 55    20.909658 18.376943
    ## 56    22.168959 19.168752
    ## 57    19.333770 18.075449
    ## 58    21.226713 18.454374
    ## 59    21.806392 19.822658
    ## 60    21.492988 19.359758
    ## 61    17.728618 18.152709
    ## 62    22.447579 17.660378
    ## 63    18.909851 18.542076
    ## 64    22.949297 19.422708
    ## 65    21.998192 19.367830
    ## 66    19.631918 18.017186
    ## 67    17.105046 17.159197
    ## 68    16.852046 18.028408
    ## 69    18.452013 18.337653
    ## 70    21.151848 18.764773
    ## 71    20.693408 18.782060
    ## 72    22.023237 19.457515
    ## 73    22.921690 19.988953
    ## 74    15.710583 17.114361
    ## 75    23.820211 19.382906
    ## 76    21.957066 18.303583
    ## 77    24.028637 19.581777
    ## 78    19.903126 16.697177
    ## 79    22.033213 16.002991
    ## 80    14.201509 17.730602
    ## 81    22.683704 19.262276
    ## 82    11.697111 14.647985
    ## 83    21.185641 16.491251
    ## 84    13.320522 15.213973
    ## 85    17.807832 19.460472
    ## 86    17.700136 19.796180
    ## 87    18.076356 18.690551
    ## 88    18.326485 18.227524
    ## 89    19.012817 19.493248
    ## 90    21.531042 19.907434
    ## 91    25.106100 18.761418
    ## 92    24.151161 19.015018
    ## 93    23.931826 19.572035
    ## 94    19.623271 18.594120
    ## 95    16.756248 16.894971
    ## 96    24.228580 18.162705
    ## 97    22.732742 19.924384
    ## 98    16.190875 19.868730
    ## 99    22.180058 19.395261
    ## 100   21.251031 18.437806
    ## 101   22.236734 19.211204
    ## 102   14.501350 19.258239
    ## 103   20.184895 18.106004
    ## 104   14.986713 12.259450
    ## 105   21.596458 17.951756
    ## 106   15.745744 19.400595
    ## 107   22.603095 18.596859
    ## 108   21.550488 19.557211
    ## 109   21.680634 19.021213
    ## 110   18.567133 18.910594
    ## 111   24.650206 18.651756
    ## 112   18.775101 18.829076
    ## 113   19.118599 17.378674
    ## 114   17.182220 17.763022
    ## 115   18.454186 18.994702
    ## 116   23.878132 19.637101
    ## 117   26.466070 19.583485
    ## 118   18.713443 18.714818
    ## 119   13.620310 17.548173
    ## 120   15.197186 14.870575
    ## 121   22.500603 19.431969
    ## 122   22.210314 19.605006
    ## 123   16.109138 18.652881
    ## 124   22.415688 18.395615
    ## 125   24.286702 19.974756
    ## 126   25.134779 17.190254
    ## 127   20.430777 19.714974
    ## 128   15.301157 19.788737
    ## 129   22.924906 19.289542
    ## 130   25.842110 18.405838
    ## 131   26.424919 19.298796
    ## 132   20.671495 19.742273
    ## 133   17.800648 19.168387
    ## 134   20.976109 19.666937
    ## 135   14.969430 19.381756
    ## 136   16.857123 16.313224
    ## 137   17.298302 15.383909
    ## 138   22.519405 19.995331
    ## 139   16.617410 19.698089
    ## 140   10.269871 10.513110
    ## 141   12.775529 17.962124
    ## 142   16.602064 14.342500
    ## 143   22.036244 18.980945
    ## 144   21.505689 18.998117
    ## 145   20.804627 19.622568
    ## 146   26.502859 17.350866
    ## 147   21.306216 17.652878
    ## 148   15.542633 18.494614
    ## 149   19.012352 17.835782
    ## 150   16.544348 18.425019
    ## 151   16.922737 17.278957
    ## 152   26.329873 19.835546
    ## 153   20.424424 18.564929
    ## 154   22.311744 19.430041
    ## 155   15.881644 19.684645
    ## 156   21.681707 19.225475
    ## 157   25.576434 19.678764
    ## 158   23.875383 19.956039
    ## 159   17.440069 19.622820
    ## 160   25.388023 19.715050
    ## 161   16.021382 16.444968
    ## 162   17.055585 15.749897
    ## 163   18.627302 18.224252
    ## 164   24.677074 19.010851
    ## 165   21.134441 19.991571
    ## 166   15.224713 19.791789
    ## 167   18.424169 17.416418
    ## 168   24.147897 18.116388
    ## 169   23.117101 17.426595
    ## 170   21.495424 18.906261
    ## 171   19.657618 16.468186
    ## 172   19.472858 16.609442
    ## 173   16.743129 13.497811
    ## 174   14.947441 16.774136
    ## 175   22.819545 19.895262
    ## 176   22.631700 19.791914
    ## 177   19.790730 19.823433
    ## 178   18.730404 19.784993
    ## 179   14.231655 17.085397
    ## 180   24.106483 18.190956
    ## 181   24.644865 19.939528
    ## 182   23.911229 19.650711
    ## 183   17.670362 18.627966
    ## 184   18.302166 19.987198
    ## 185   17.063597 17.461501
    ## 186   25.730841 19.648744
    ## 187   20.738015 16.686616
    ## 188   19.805362 19.456609
    ## 189   20.089136 17.377629
    ## 190   14.166984 19.303988
    ## 191   22.672137 18.072304
    ## 192   19.522714 17.302372
    ## 193   25.574050 19.496591
    ## 194   21.524922 16.263559
    ## 195   17.807444 14.829709
    ## 196   19.915565 17.371633
    ## 197   22.879774 17.534917
    ## 198   18.826359 18.458978
    ## 199   20.966576 19.756721
    ## 200   14.335601 15.453372
    ## 201   15.729329 18.251483
    ## 202   13.810966 18.172301
    ## 203   21.848150 19.934175
    ## 204   19.362304 19.681839
    ## 205   22.347247 19.045807
    ## 206   15.353442 19.419305
    ## 207   12.183829 18.289401
    ## 208   19.965207 19.078246
    ## 209   20.391818 19.043467
    ## 210   17.167811 19.715863
    ## 211   25.157324 19.442808
    ## 212   20.523669 19.307354
    ## 213   22.480326 19.351546
    ## 214   19.307158 18.679273
    ## 215   20.076867 18.053741
    ## 216   27.065095 19.947959
    ## 217   25.462566 17.955409
    ## 218   22.740168 19.135423
    ## 219   21.720638 19.737470
    ## 220   24.071298 17.384829
    ## 221   27.009492 19.931257
    ## 222   23.677980 19.811902
    ## 223   20.531600 17.707055
    ## 224   21.361075 19.816800
    ## 225   14.805669 17.427840
    ## 226   22.532703 17.278709
    ## 227   23.583938 19.962302
    ## 228   23.663229 19.260894
    ## 229   25.237203 19.206618
    ## 230   17.089641 16.539785
    ## 231   17.855662 19.290763
    ## 232   20.240496 17.887542
    ## 233   17.240800 18.352612
    ## 234   19.980144 18.884301
    ## 235   16.074095 17.402697
    ## 236   16.832663 19.259317
    ## 237   21.288443 18.203125
    ## 238   18.619740 17.191575
    ## 239   20.884321 18.388144
    ## 240   16.831209 19.199779
    ## 241   20.325608 17.299085
    ## 242   22.371489 19.060922
    ## 243   19.673535 17.666708
    ## 244   21.460455 18.750692
    ## 245   20.590946 18.653520
    ## 246   21.505417 19.031730
    ## 247   16.189095 18.994536
    ## 248   17.434716 19.442441
    ## 249   17.062956 18.154731
    ## 250   17.197692 19.432830
    ## 251   19.464121 19.420941
    ## 252   22.482528 17.811679
    ## 253   20.097419 19.189813
    ## 254   24.538626 19.775536
    ## 255   21.370403 19.656946
    ## 256   16.840723 16.255293
    ## 257   16.639546 19.766828
    ## 258   21.442050 19.532164
    ## 259   15.286242 16.423057
    ## 260   20.744950 17.705741
    ## 261   19.729057 17.249805
    ## 262   13.363697 15.962303
    ## 263   16.400748 15.678432
    ## 264   22.524568 19.031616
    ## 265   16.675530 16.221548
    ## 266   24.013985 17.605386
    ## 267   23.981759 19.140994
    ## 268   18.680414 17.003143
    ## 269   16.991829 19.944768
    ## 270   20.694833 18.713513
    ## 271   17.058353 18.721694
    ## 272   16.831767 18.813371
    ## 273   20.184066 19.054536
    ## 274   22.097212 18.562099
    ## 275   20.813265 19.778167
    ## 276   16.605566 18.358753
    ## 277   22.578223 19.708257
    ## 278   17.936494 17.537015
    ## 279   21.306251 18.506364
    ## 280   25.085077 19.334701
    ## 281   23.216370 19.316913
    ## 282   25.235585 19.346401
    ## 283   25.955307 17.288638
    ## 284   21.695582 16.742535
    ## 285   13.506954 11.311585
    ## 286   29.619064 19.856034
    ## 287   16.535587 18.328464
    ## 288   16.833260 18.051047
    ## 289   17.358108 17.490913
    ## 290   20.320157 17.748622
    ## 291   23.378473 18.419564
    ## 292   21.935162 19.100750
    ## 293   16.450135 19.129351
    ## 294   27.214077 19.855603
    ## 295   13.260063 16.525199
    ## 296   18.581756 18.862677
    ## 297   24.309597 18.004152
    ## 298   25.837867 17.452214
    ## 299   17.870717 18.296544
    ## 300   14.741398 18.843766
    ## 301   19.210945 19.035409
    ## 302   26.287133 19.579493
    ## 303   23.573207 18.289328
    ## 304   22.643595 19.981515
    ## 305   25.616473 18.552671
    ## 306   21.229520 17.475193
    ## 307   16.955622 18.777646
    ## 308   14.763846 19.459083
    ## 309   19.350463 19.296065
    ## 310   24.708620 19.737212
    ## 311   20.005108 19.801110
    ## 312   25.254414 19.751138
    ## 313   16.657991 18.984390
    ## 314   21.202602 19.580349
    ## 315   19.900098 19.806452
    ## 316   24.833450 18.881344
    ## 317   23.564842 18.954390
    ## 318   17.055655 18.042393
    ## 319   13.713051 15.924118
    ## 320   17.219879 15.588491
    ## 321   16.604343 17.638015
    ## 322   23.046735 18.236731
    ## 323   23.374347 19.896180
    ## 324   13.594386 12.928647
    ## 325   23.755518 19.943739
    ## 326   12.644900 18.066545
    ## 327   19.809560 19.625446
    ## 328   18.315357 16.821317
    ## 329   20.177032 19.712816
    ## 330   19.032389 17.149252
    ## 331   22.783345 18.652161
    ## 332   15.474600 17.740819
    ## 333   21.894329 18.180775
    ## 334   20.569093 19.828602
    ## 335   17.784994 16.716791
    ## 336   20.349883 17.673042
    ## 337   18.387552 18.004137
    ## 338   20.918077 19.218110
    ## 339   16.210088 15.420976
    ## 340   21.326101 19.260468
    ## 341   19.205191 19.061231
    ## 342   25.653955 19.126340
    ## 343   13.482884 14.975392
    ## 344   18.632782 17.225402
    ## 345   21.221875 19.375617
    ## 346   25.110814 18.279291
    ## 347   26.063604 19.425629
    ## 348   20.524703 19.245371
    ## 349   22.597438 19.582802
    ## 350   24.797146 19.650808
    ## 351   20.550896 19.695005
    ## 352   20.862585 19.556395
    ## 353   18.666130 19.328228
    ## 354   19.165636 16.435979
    ## 355   24.041765 19.163487
    ## 356   22.281680 19.508951
    ## 357   24.852556 18.753369
    ## 358   21.740823 19.088306
    ## 359   14.298966 14.884716
    ## 360   24.377285 18.466375
    ## 361   16.790964 19.452443
    ## 362   18.462619 16.564288
    ## 363   19.982348 17.373762
    ## 364   13.872939 18.362533
    ## 365   23.652923 19.410326
    ## 366   22.030948 18.922885
    ## 367   24.899041 19.975485
    ## 368   17.089978 17.915619
    ## 369   19.332646 18.162245
    ## 370   21.081517 19.013651
    ## 371   21.750435 19.605013
    ## 372   14.312476 17.125699
    ## 373   16.870044 19.738513
    ## 374   24.886106 19.955122
    ## 375   22.509793 19.762936
    ## 376   24.583548 19.077967
    ## 377   14.167724 15.995194
    ## 378   20.377919 18.066845
    ## 379   21.253276 15.903498
    ## 380   19.691615 17.749856
    ## 381   16.388991 18.609535
    ## 382   18.586571 15.445286
    ## 383   19.959676 17.399837
    ## 384   16.923504 17.727349
    ## 385   14.764212 17.225860
    ## 386   21.228050 19.026330
    ## 387   21.436860 17.807472
    ## 388   19.228477 16.430496
    ## 389   21.309755 19.919179
    ## 390   17.497367 18.644800
    ## 391   24.451698 16.860874
    ## 392   19.467034 19.328617
    ## 393   21.321613 17.209072
    ## 394   22.267928 19.417885
    ## 395   17.019449 18.973398
    ## 396   18.286047 18.586463
    ## 397   18.348848 19.397812
    ## 398   28.436111 19.710064
    ## 399   21.119171 19.748407
    ## 400   15.839838 14.858986
    ## 401   25.688245 19.113078
    ## 402   20.907987 19.482984
    ## 403   27.395496 19.733542
    ## 404   26.458234 19.401882
    ## 405   17.095628 18.946267
    ## 406   21.480236 19.887752
    ## 407   22.015241 19.340829
    ## 408   15.642863 16.542562
    ## 409   20.016132 19.998061
    ## 410   16.473933 18.062278
    ## 411   18.918483 19.789433
    ## 412   22.169121 18.762001
    ## 413   23.409090 18.653282
    ## 414   25.114062 19.513784
    ## 415   18.031475 19.445717
    ## 416   15.750051 19.253810
    ## 417   25.306806 19.338370
    ## 418   23.476599 17.790364
    ## 419   21.465734 18.856331
    ## 420   23.672861 17.739146
    ## 421   21.048540 18.275008
    ## 422   18.646089 16.263808
    ## 423   26.167056 19.025535
    ## 424   23.944295 18.040800
    ## 425   22.168406 19.200354
    ## 426   14.741822 16.964532
    ## 427   14.684593 19.355539
    ## 428   19.368437 19.419463
    ## 429   20.978093 18.494979
    ## 430   26.407676 19.153363
    ## 431   14.133417 17.502793
    ## 432   16.983491 17.229247
    ## 433   20.166992 19.734191
    ## 434   16.655602 17.616693
    ## 435   19.106498 18.971604
    ## 436   20.853244 18.005631
    ## 437   17.466623 19.348604
    ## 438   16.820446 19.467526
    ## 439   13.876799 16.435966
    ## 440   21.002514 19.177213
    ## 441   18.049433 17.239495
    ## 442   18.729494 16.572208
    ## 443   20.849568 18.542028
    ## 444   17.400835 16.107778
    ## 445   27.081904 18.831096
    ## 446   20.886085 15.216614
    ## 447   22.812886 18.144570
    ## 448   21.123079 19.233473
    ## 449   25.415238 17.525739
    ## 450   17.304069 17.605275
    ## 451   18.588698 15.585502
    ## 452   14.989463 19.509566
    ## 453   16.090765 19.922029
    ## 454   23.472336 17.646337
    ## 455   21.657937 18.908140
    ## 456   18.734088 18.209684
    ## 457   23.397317 18.068531
    ## 458   18.492050 17.917039
    ## 459   19.022160 17.089040
    ## 460   15.245171 19.285946
    ## 461   18.615630 16.972985
    ## 462   21.045506 19.812219
    ## 463   20.904231 16.098346
    ## 464   16.936528 19.367189
    ## 465   24.284362 19.721620
    ## 466   23.845672 19.961656
    ## 467   25.363841 18.923016
    ## 468   18.285867 16.987398
    ## 469   17.038137 17.498194
    ## 470   19.377583 18.091777
    ## 471   16.191955 18.733933
    ## 472   25.333745 19.146011
    ## 473   21.627208 17.372180
    ## 474   15.648705 14.215312
    ## 475   15.202849 17.053099
    ## 476   22.350872 18.062966
    ## 477   20.668593 19.811158
    ## 478   19.113243 19.221679
    ## 479   22.128421 18.495673
    ## 480   24.359158 19.424428
    ## 481   23.104147 17.082448
    ## 482   15.158471 16.665881
    ## 483   13.075182 19.376587
    ## 484   22.761872 17.670212
    ## 485   17.715243 17.767092
    ## 486   22.572325 19.332411
    ## 487   18.637560 15.813847
    ## 488   24.732995 18.193094
    ## 489   19.788968 18.665450
    ## 490   19.821484 17.506034
    ## 491   15.924393 19.258633
    ## 492   20.302958 19.375463
    ## 493   20.580556 19.930415
    ## 494   23.415530 19.536538
    ## 495   17.936147 18.790089
    ## 496   14.264060 17.457876
    ## 497   17.388049 16.065614
    ## 498   14.464101 17.181147
    ## 499   21.141684 18.420032
    ## 500   17.124560 18.752422
    ## 501   26.207974 18.746204
    ## 502   18.681763 18.100194
    ## 503   15.625939 17.107344
    ## 504   14.229370 18.445318
    ## 505   25.122831 18.719703
    ## 506   22.861970 18.146468
    ## 507   23.114618 17.837212
    ## 508   23.188423 19.669850
    ## 509   20.573065 18.114613
    ## 510   18.750516 18.073781
    ## 511   20.237169 18.409693
    ## 512   21.535488 18.957084
    ## 513   22.273179 18.826861
    ## 514   19.699495 18.883751
    ## 515   23.901546 19.642995
    ## 516   15.737971 17.543889
    ## 517   15.805272 19.706528
    ## 518   12.400298 17.275559
    ## 519   18.568513 17.883362
    ## 520   21.179759 17.472089
    ## 521   13.623491 18.213884
    ## 522   21.917097 19.803768
    ## 523   20.839154 19.387478
    ## 524   24.958191 19.555275
    ## 525   19.212663 17.723611
    ## 526   21.132570 17.008027
    ## 527   24.105266 19.427701
    ## 528   16.253752 17.530637
    ## 529   14.307365 15.646104
    ## 530   20.898939 19.759425
    ## 531   24.069052 18.140193
    ## 532   22.925484 18.398700
    ## 533   19.346096 17.696092
    ## 534   23.553982 19.617190
    ## 535   22.381121 19.269053
    ## 536   11.589914 15.552065
    ## 537   16.595029 19.501419
    ## 538   21.091603 17.904348
    ## 539   18.873449 18.272015
    ## 540   13.213362 18.210702
    ## 541   19.504195 19.370985
    ## 542   21.081662 19.846864
    ## 543   20.790625 19.395377
    ## 544   17.702444 16.783899
    ## 545   23.107797 18.704161
    ## 546   18.365917 15.795839
    ## 547   19.075137 17.774130
    ## 548   23.008663 19.606908
    ## 549   17.133641 18.447896
    ## 550   19.186830 19.972322
    ## 551   25.318181 19.975735
    ## 552   16.693289 17.779516
    ## 553   18.870242 18.663473
    ## 554   19.699835 19.092012
    ## 555   17.497902 17.642356
    ## 556   20.271921 18.351902
    ## 557   16.804372 17.137633
    ## 558   13.585389 12.742833
    ## 559   24.177509 19.417359
    ## 560   20.322438 19.462331
    ## 561   19.000249 18.308755
    ## 562   18.902021 18.944989
    ## 563   19.905481 15.746081
    ## 564   19.340787 18.542072
    ## 565   21.607374 18.174354
    ## 566   17.074237 18.220640
    ## 567   18.822951 16.718462
    ## 568   15.289912 13.611227
    ## 569   19.365563 18.688642
    ## 570   18.004917 18.262774
    ## 571   21.258322 18.554286
    ## 572   28.722740 19.293112
    ## 573   19.325501 16.910896
    ## 574   27.572276 19.384057
    ## 575   21.009246 17.201929
    ## 576   22.231460 17.808910
    ## 577   20.694238 17.274734
    ## 578   18.393049 19.467664
    ## 579   20.355329 17.352090
    ## 580   24.931356 18.275690
    ## 581   20.879578 16.811819
    ## 582   22.351062 18.031163
    ## 583   20.726878 17.521613
    ## 584   18.303890 17.253055
    ## 585   22.978837 19.648804
    ## 586   21.782268 18.961640
    ## 587   21.979212 19.002036
    ## 588   16.497849 16.853161
    ## 589   23.102647 18.785083
    ## 590   21.864168 18.648903
    ## 591   17.431622 16.302225
    ## 592   25.128421 18.353201
    ## 593   21.771391 19.503513
    ## 594   21.210562 19.022333
    ## 595   13.901124 17.080167
    ## 596   18.499992 16.307303
    ## 597   20.055738 17.086775
    ## 598   19.610843 17.547878
    ## 599   19.809751 18.685952
    ## 600   18.778403 17.567273
    ## 601   21.424293 19.248236
    ## 602   27.820425 19.926887
    ## 603   20.421805 17.861221
    ## 604   19.123762 18.896615
    ## 605   18.048406 16.974266
    ## 606   19.297556 18.759878
    ## 607   21.645412 17.245727
    ## 608   25.204020 19.179698
    ## 609   22.132743 19.023022
    ## 610   23.560029 18.760729
    ## 611   17.295358 18.350626
    ## 612   21.848360 19.188607
    ## 613   17.526586 19.837634
    ## 614   16.762612 18.022682
    ## 615   13.391062 14.059908
    ## 616   19.801063 18.427058
    ## 617   17.751443 19.288031
    ## 618   21.022793 17.336912
    ## 619   24.998002 19.491960
    ## 620   17.494757 15.330264
    ## 621   17.620770 18.951866
    ## 622   13.830030 17.023454
    ## 623   21.224459 19.607395
    ## 624   17.037825 17.037476
    ## 625   25.663364 19.849591
    ## 626   16.824159 18.155727
    ## 627   16.519772 16.051566
    ## 628   17.417280 16.837207
    ## 629   22.378641 19.206880
    ## 630   16.366790 13.317814
    ## 631   18.652169 17.689721
    ## 632   23.225553 19.589480
    ## 633   18.780800 16.123234
    ## 634   20.561317 19.561135
    ## 635   27.061771 18.553645
    ## 636   25.661332 19.712118
    ## 637   17.598781 18.011259
    ## 638   18.055757 18.389002
    ## 639   21.581019 17.740783
    ## 640   24.948934 19.370695
    ## 641   14.767488 13.439979
    ## 642   21.512719 17.662923
    ## 643   12.683389 17.821380
    ## 644   24.269661 19.870946
    ## 645   26.074028 19.656154
    ## 646   19.601683 16.654855
    ## 647   19.321067 18.720841
    ## 648   23.040040 19.371483
    ## 649   17.524629 19.996823
    ## 650   12.646807 11.978518
    ## 651   16.885562 19.053190
    ## 652   24.469905 19.634310
    ## 653   17.503360 16.258765
    ## 654   23.932994 18.090087
    ## 655   19.010312 19.693072
    ## 656   23.891235 18.845227
    ## 657   11.117499 10.541569
    ## 658   15.516530 17.826387
    ## 659   11.227453  9.915991
    ## 660   16.775714 18.459189
    ## 661   23.686292 19.708056
    ## 662   27.038557 19.004739
    ## 663   21.491470 19.532180
    ## 664   20.868567 19.238925
    ## 665   26.910809 19.247857
    ## 666   18.379593 17.186603
    ## 667   22.959015 19.829370
    ## 668   21.979251 18.188888
    ## 669   20.301846 19.488052
    ## 670   23.422073 19.028516
    ## 671   18.320570 17.691910
    ## 672   31.493985 19.937749
    ## 673   16.670213 19.111576
    ## 674   19.766269 17.028816
    ## 675   15.547513 14.442886
    ## 676   20.415820 19.765753
    ## 677   18.188356 17.956961
    ## 678   25.860017 18.618145
    ## 679   19.149561 19.853816
    ## 680   18.114627 16.786912
    ## 681   23.315753 19.012028
    ## 682   20.789358 18.405266
    ## 683   19.342301 19.759256
    ## 684   26.383792 17.793112
    ## 685   18.524512 15.740907
    ## 686   20.073117 17.732789
    ## 687   17.060032 18.627272
    ## 688   18.874356 18.765004
    ## 689   24.956559 18.651494
    ## 690   24.156019 19.641896
    ## 691   17.758269 14.790998
    ## 692   17.563628 17.539949
    ## 693   19.562897 18.581833
    ## 694   19.818077 17.149800
    ## 695   20.080148 19.668881
    ## 696   28.787681 18.973801
    ## 697   16.168327 18.459265
    ## 698   18.949023 19.905426
    ## 699   18.593014 19.117300
    ## 700   14.698225 17.665725
    ## 701   12.876722 16.507560
    ## 702   19.672152 19.766161
    ## 703   19.141780 19.438374
    ## 704   14.384935 19.287607
    ## 705   17.766312 17.454266
    ## 706   20.658708 18.232608
    ## 707   19.849056 15.558951
    ## 708   15.254494 15.851972
    ## 709   15.517614 18.382496
    ## 710   19.416223 15.645439
    ## 711   14.028834 15.888400
    ## 712   22.538992 19.948174
    ## 713   22.059405 17.777992
    ## 714   21.071998 18.830131
    ## 715   24.981493 19.096591
    ## 716   15.754157 19.638757
    ## 717   16.709018 16.190696
    ## 718   21.609457 17.620215
    ## 719   17.956555 19.959816
    ## 720   19.444656 18.989613
    ## 721   16.352746 19.403899
    ## 722   20.033078 15.739833
    ## 723   28.815161 19.504458
    ## 724   29.496350 19.881869
    ## 725   17.398120 19.764015
    ## 726   17.781641 19.854998
    ## 727   22.918284 16.385177
    ## 728   23.920037 17.827493
    ## 729   17.629597 19.368793
    ## 730   23.048405 19.710941
    ## 731   22.701893 19.239816
    ## 732   16.079181 17.139586
    ## 733   23.968960 17.671451
    ## 734   19.143345 19.202858
    ## 735   25.359505 19.274802
    ## 736   21.219205 17.478712
    ## 737   17.194995 18.599654
    ## 738   16.587867 19.640592
    ## 739   21.123373 18.797179
    ## 740   24.812264 19.991722
    ## 741   26.380194 19.269278
    ## 742   24.129731 19.701078
    ## 743   20.997117 19.881964
    ## 744   18.879786 19.594354
    ## 745   18.881933 18.524137
    ## 746   15.836335 14.795156
    ## 747   17.216094 17.264755
    ## 748   18.998298 18.516123
    ## 749   14.399665 14.448344
    ## 750   26.785903 19.564566
    ## 751   20.999898 19.213537
    ## 752   17.512170 18.695268
    ## 753   22.160320 18.706549
    ## 754   20.859512 17.252899
    ## 755   18.943010 17.209868
    ## 756   20.031081 19.485285
    ## 757   19.044407 16.153480
    ## 758   24.492019 19.956035
    ## 759   18.037700 18.587746
    ## 760   16.901519 19.691628
    ## 761   11.925153 13.191492
    ## 762   21.676633 17.728679
    ## 763   23.823105 19.328797
    ## 764   16.350323 18.032597
    ## 765   20.474456 19.401228
    ## 766   16.009452 16.956940
    ## 767   21.016080 16.424991
    ## 768   20.729509 18.911506
    ## 769   23.024102 18.354115
    ## 770   27.327082 19.343037
    ## 771   12.026961 17.288034
    ## 772   18.759654 18.803478
    ## 773   19.716403 19.673774
    ## 774   17.130525 14.125308
    ## 775   12.219618 17.761062
    ## 776   18.871006 15.004389
    ## 777   22.438425 19.903939
    ## 778   23.764875 17.755038
    ## 779   23.835021 18.869005
    ## 780   19.286233 17.509217
    ## 781   19.750925 17.278786
    ## 782   19.321399 16.183077
    ## 783   19.130593 19.390879
    ## 784   18.684496 18.757448
    ## 785   22.759555 16.275525
    ## 786   15.360383 19.437927
    ## 787   20.970328 18.226870
    ## 788   20.065013 17.463031
    ## 789   15.525617 19.421061
    ## 790   18.417427 19.633106
    ## 791   16.305935 19.553853
    ## 792   29.732038 19.857692
    ## 793   13.164298 11.758207
    ## 794   23.299051 19.562519
    ## 795   14.288913 12.413038
    ## 796   24.435274 18.821194
    ## 797   16.148767 14.968875
    ## 798   15.106536 17.216667
    ## 799   11.847113 12.715544
    ## 800   20.223507 18.942601
    ## 801   17.282749 19.956921
    ## 802   15.834687 12.812505
    ## 803   26.524431 19.759781
    ## 804   20.456745 19.149808
    ## 805   29.928634 19.992824
    ## 806   20.640530 19.897993
    ## 807   16.424328 17.526939
    ## 808   18.713375 17.779719
    ## 809   19.729600 19.744411
    ## 810   22.919273 19.640256
    ## 811   18.968891 19.472880
    ## 812   26.576155 17.356133
    ## 813   18.986895 17.466850
    ## 814   19.487997 18.905579
    ## 815   23.179182 17.562448
    ## 816   14.914725 17.200671
    ## 817   19.411262 18.127140
    ## 818   24.288057 19.666393
    ## 819   18.821943 16.465853
    ## 820   25.409764 19.815677
    ## 821   15.761013 17.053392
    ## 822   16.115293 14.797595
    ## 823   21.371488 18.312301
    ## 824   18.122723 19.319398
    ## 825   16.246976 19.208887
    ## 826   26.979030 19.229488
    ## 827   18.341306 19.371136
    ## 828   16.553180 18.996034
    ## 829   28.199940 19.857621
    ## 830   21.954373 19.183461
    ## 831   20.450528 19.886325
    ## 832   22.648173 18.882717
    ## 833   18.351247 19.605555
    ## 834   18.302580 15.883484
    ## 835   27.214275 18.493131
    ## 836   22.891659 19.570678
    ## 837   17.809813 14.918829
    ## 838   12.343220 19.846970
    ## 839   18.389000 18.850121
    ## 840   20.256378 17.658493
    ## 841   22.666405 19.779502
    ## 842   22.947818 18.179686
    ## 843   19.610684 19.340646
    ## 844   13.624803 16.009883
    ## 845   23.155848 19.804303
    ## 846   17.664285 17.179739
    ## 847   18.421547 13.742098
    ## 848   20.435641 16.835290
    ## 849   21.501493 19.486276
    ## 850   19.849891 19.278695
    ## 851   21.407642 19.196140
    ## 852   24.964205 19.156780
    ## 853   18.804090 17.700180
    ## 854   17.848624 19.765873
    ## 855   16.981981 17.562710
    ## 856   20.986606 19.426517
    ## 857   18.608203 19.697218
    ## 858   19.238751 18.270224
    ## 859   15.573802 12.385278
    ## 860   22.856427 18.175717
    ## 861   24.060677 19.684694
    ## 862   18.333404 15.858689
    ## 863   24.776300 19.686276
    ## 864   17.783605 14.822979
    ## 865   20.966629 19.608219
    ## 866   22.922537 19.941528
    ## 867   23.104904 19.868030
    ## 868   20.894505 16.583217
    ## 869   22.859263 18.298922
    ## 870   22.035088 17.191662
    ## 871   20.852199 17.625673
    ## 872   16.600682 17.403492
    ## 873   16.038073 19.475685
    ## 874   21.137872 17.959147
    ## 875   17.596191 16.009812
    ## 876   25.413396 19.368620
    ## 877   20.137633 19.524712
    ## 878   15.379004 18.985361
    ## 879   19.280316 16.722695
    ## 880   26.508070 19.104845
    ## 881   25.914552 18.941377
    ## 882   19.895511 19.480193
    ## 883   19.373274 16.593214
    ## 884   27.993851 19.636685
    ## 885   24.724396 19.574659
    ## 886   27.074550 19.065401
    ## 887   16.976588 17.479160
    ## 888   24.675457 19.743034
    ## 889   19.649305 16.190198
    ## 890   16.926231 19.824069
    ## 891   20.428877 19.115007
    ## 892   18.546362 18.401320
    ## 893   14.521877 11.157091
    ## 894   12.042217 19.146128
    ## 895   18.895650 17.922503
    ## 896   19.459887 17.442125
    ## 897   21.227979 19.124601
    ## 898   28.361445 19.362345
    ## 899   22.699482 19.192448
    ## 900   14.954767 17.030778
    ## 901   16.216981 14.574229
    ## 902   21.102561 18.435599
    ## 903   18.446152 18.490931
    ## 904   26.892225 19.542201
    ## 905   23.651524 17.563172
    ## 906   20.066392 19.262179
    ## 907   18.190319 19.397099
    ## 908   18.073945 19.332388
    ## 909   24.824566 19.604368
    ## 910   19.215629 19.291943
    ## 911   18.203356 18.300187
    ## 912   20.376901 19.951031
    ## 913   25.796284 18.837405
    ## 914   16.408887 19.549790
    ## 915   22.999232 19.192373
    ## 916   17.916611 14.902988
    ## 917   28.479854 19.702038
    ## 918   22.559327 18.307035
    ## 919   11.293285 12.863426
    ## 920   19.218706 19.130408
    ## 921   19.702775 17.567514
    ## 922   17.829273 19.120257
    ## 923   17.374313 18.390852
    ## 924   14.010378 18.160010
    ## 925   24.218038 18.973938
    ## 926   18.769653 18.534781
    ## 927   22.737853 18.263721
    ## 928   14.778422 19.104344
    ## 929   19.796332 15.903773
    ## 930   22.039675 19.108690
    ## 931   18.683677 15.690218
    ## 932   26.864129 19.675645
    ## 933   16.146057 17.946177
    ## 934   21.979926 19.773399
    ## 935   22.420390 19.215847
    ## 936   22.243368 18.474871
    ## 937   23.547169 18.835053
    ## 938   26.182340 19.772093
    ## 939   19.310558 19.337081
    ## 940   21.341887 19.911859
    ## 941   24.766015 19.939685
    ## 942   22.986229 19.777732
    ## 943   23.430635 19.205666
    ## 944   16.564535 18.698937
    ## 945   28.943205 18.969463
    ## 946   19.084554 19.023266
    ## 947   20.271783 17.899164
    ## 948   18.475904 16.725776
    ## 949   20.646148 16.658339
    ## 950   15.620777 18.235959
    ## 951   16.477838 15.785505
    ## 952   22.342321 19.605330
    ## 953   23.604782 18.446092
    ## 954   18.247310 17.274287
    ## 955   29.646331 19.904630
    ## 956   15.844263 18.201396
    ## 957   22.550508 19.839622
    ## 958   23.094482 19.167900
    ## 959   21.071931 19.110932
    ## 960   20.037064 19.863840
    ## 961   15.056828 15.051566
    ## 962   20.991721 19.122953
    ## 963   24.165496 18.965454
    ## 964   15.480851 19.264147
    ## 965   27.871499 19.328795
    ## 966   16.644360 17.993909
    ## 967   17.262049 18.514431
    ## 968   22.131315 19.803053
    ## 969   21.357579 19.731482
    ## 970   24.603003 18.230625
    ## 971   23.989811 18.068153
    ## 972   23.107632 19.377116
    ## 973   16.250156 19.721313
    ## 974   24.983021 19.388056
    ## 975   22.045680 19.615556
    ## 976   19.708698 16.433354
    ## 977   20.969978 17.294681
    ## 978   19.838856 19.459769
    ## 979   25.306832 19.516471
    ## 980   12.571080 15.264028
    ## 981   25.640936 19.379471
    ## 982   15.065287 17.875970
    ## 983   21.445750 18.803294
    ## 984   25.262612 16.276323
    ## 985   21.273412 19.913621
    ## 986   23.868305 19.565956
    ## 987   27.572611 19.681051
    ## 988   13.665033 19.751671
    ## 989   18.556323 19.802158
    ## 990   18.488923 14.205251
    ## 991   17.088290 19.075691
    ## 992   12.695036 18.616821
    ## 993   22.943708 19.805902
    ## 994   27.992297 19.923509
    ## 995   16.060984 16.098300
    ## 996   22.663369 18.365126
    ## 997   18.472145 14.601038
    ## 998   21.363045 19.360843
    ## 999   23.841196 18.653891
    ## 1000  24.818267 19.547640
    ## 1001  25.315099 19.027358
    ## 1002  24.278755 19.583432
    ## 1003  19.946716 16.958701
    ## 1004  20.080462 18.377071
    ## 1005  28.612097 19.989295
    ## 1006  23.285798 19.395297
    ## 1007  21.294251 18.684797
    ## 1008  15.925592 17.954059
    ## 1009  15.402664 19.846846
    ## 1010  16.495471 19.570741
    ## 1011  18.055556 15.670976
    ## 1012  20.221011 16.767767
    ## 1013   6.907744 14.570955
    ## 1014  18.907004 15.745064
    ## 1015  22.055288 18.988130
    ## 1016  26.412781 19.732157
    ## 1017  23.085134 18.426750
    ## 1018  16.386871 14.487581
    ## 1019  21.706574 18.161608
    ## 1020  15.860240 16.555205
    ## 1021  18.698594 18.145755
    ## 1022  24.211025 18.906572
    ## 1023  21.141629 19.092260
    ## 1024  17.950716 19.465202
    ## 1025  22.698404 19.863936
    ## 1026  23.363505 19.980746
    ## 1027  20.794437 19.908074
    ## 1028  21.567513 16.828384
    ## 1029  21.295979 19.470799
    ## 1030  16.762407 15.197280
    ## 1031  15.621449 14.137077
    ## 1032  22.431729 19.923033
    ## 1033   9.558028 10.591776
    ## 1034  14.986754 19.451829
    ## 1035  21.603728 17.522530
    ## 1036  17.149318 19.114099
    ## 1037  18.442947 19.106441
    ## 1038  18.098082 18.391248
    ## 1039  22.751282 19.020889
    ## 1040  17.465967 16.145744
    ## 1041  15.871144 19.417443
    ## 1042  23.169808 19.825080
    ## 1043  23.783690 17.744528
    ## 1044  27.919610 19.073054
    ## 1045  22.693697 18.582293
    ## 1046  21.059199 18.646281
    ## 1047  21.855800 18.971120
    ## 1048  18.984304 17.593239
    ## 1049  26.269495 18.325324
    ## 1050  20.285453 19.271842
    ## 1051  22.192621 19.437701
    ## 1052  16.011185 16.361899
    ## 1053  19.570894 18.124920
    ## 1054  18.687575 19.735705
    ## 1055  22.366428 19.606040
    ## 1056  24.927820 19.810689
    ## 1057  23.820787 18.884468
    ## 1058  22.611191 18.498347
    ## 1059  16.317262 19.808604
    ## 1060  22.812643 19.103929
    ## 1061  19.372841 19.375467
    ## 1062  21.341871 19.888859
    ## 1063  18.908532 19.753744
    ## 1064  11.647236 15.965371
    ## 1065  22.657214 19.601550
    ## 1066  22.049026 17.071707
    ## 1067  25.685280 19.394350
    ## 1068  16.865777 19.374698
    ## 1069  23.212975 19.206617
    ## 1070  25.347561 18.252381
    ## 1071  25.371460 19.705958
    ## 1072  20.815199 19.956577
    ## 1073  19.296991 19.505358
    ## 1074  15.990047 16.978709
    ## 1075  22.113096 19.572828
    ## 1076  20.715922 18.846992
    ## 1077  25.531808 18.777061
    ## 1078  23.051393 19.782216
    ## 1079  13.752676 18.691911
    ## 1080  25.123028 19.469647
    ## 1081  16.188204 19.157925
    ## 1082  16.332036 19.507582
    ## 1083  21.235676 17.716718
    ## 1084  19.557511 19.588302
    ## 1085  20.550964 18.588123
    ## 1086  25.501680 19.529852
    ## 1087  18.668563 19.287488
    ## 1088  18.962663 19.363533
    ## 1089  15.222887 19.662350
    ## 1090  12.684375 16.759829
    ## 1091  16.019747 18.511846
    ## 1092  18.886767 19.063250
    ## 1093  24.017523 19.736615
    ## 1094  24.843720 19.977187
    ## 1095  17.885833 18.802402
    ## 1096  24.240591 17.254523
    ## 1097  19.826975 18.372840
    ## 1098  21.222734 19.703498
    ## 1099  16.008087 15.013327
    ## 1100  20.514347 19.137333
    ## 1101  23.712144 19.073780
    ## 1102  18.611744 16.908594
    ## 1103  24.817736 19.422854
    ## 1104  22.886450 18.272039
    ## 1105  17.116865 17.038120
    ## 1106  29.131153 19.866667
    ## 1107  11.549403 11.981590
    ## 1108  24.255598 18.941043
    ## 1109  24.878950 18.962645
    ## 1110  18.065238 19.740190
    ## 1111  23.134418 19.076330
    ## 1112  25.552357 19.901063
    ## 1113  21.364838 19.956925
    ## 1114  25.272921 19.037594
    ## 1115  19.019676 18.332265
    ## 1116  22.569578 19.869615
    ## 1117  23.614307 19.576191
    ## 1118  21.099955 19.262717
    ## 1119  25.698575 19.553846
    ## 1120  20.696496 18.457830
    ## 1121  24.318167 18.404048
    ## 1122  18.454121 18.854685
    ## 1123  21.243516 18.688013
    ## 1124  18.343195 19.291083
    ## 1125  15.026734 14.611497
    ## 1126  21.276327 19.751501
    ## 1127  27.231752 17.323752
    ## 1128  18.569655 15.611731
    ## 1129  20.353988 19.607882
    ## 1130  19.932722 19.634695
    ## 1131  16.886478 17.173339
    ## 1132  21.235257 16.472142
    ## 1133  21.392925 16.854397
    ## 1134  13.959787 13.070390
    ## 1135  20.057219 18.934908
    ## 1136  19.113172 18.075799
    ## 1137  22.915039 19.397124
    ## 1138  18.559641 19.482127
    ## 1139  21.435307 16.374122
    ## 1140  16.946426 17.205238
    ## 1141  25.041195 19.883371
    ## 1142  25.054068 18.711254
    ## 1143  20.557829 17.898743
    ## 1144  18.004910 18.361300
    ## 1145  16.708954 14.212001
    ## 1146  18.611350 18.539723
    ## 1147  20.800273 19.859728
    ## 1148  15.442406 15.002738
    ## 1149  20.206571 19.395091
    ## 1150  21.206028 19.509086
    ## 1151  19.442658 17.751440
    ## 1152  20.617716 18.084239
    ## 1153  20.430923 18.848108
    ## 1154  21.198853 18.780188
    ## 1155  18.159934 18.768483
    ## 1156  18.762276 17.992596
    ## 1157  24.251126 18.684496
    ## 1158  14.237924 13.914315
    ## 1159  23.058880 19.360455
    ## 1160  15.476524 14.462860
    ## 1161  17.965387 19.970647
    ## 1162  25.917103 19.113460
    ## 1163  20.459288 16.560547
    ## 1164  26.345856 19.778067
    ## 1165  11.665848 17.394995
    ## 1166  24.958497 19.913874
    ## 1167  15.080014 17.935736
    ## 1168  21.089279 17.067165
    ## 1169  21.653581 17.936438
    ## 1170  18.885345 18.199275
    ## 1171  16.902034 15.552632
    ## 1172  25.457297 18.591055
    ## 1173  21.720865 19.797013
    ## 1174  19.746932 18.431264
    ## 1175  25.846330 19.928759
    ## 1176  24.939033 18.681111
    ## 1177  14.465526 15.267377
    ## 1178  13.183080  9.604069
    ## 1179  20.642327 19.249090
    ## 1180  17.888817 19.574461
    ## 1181  19.820908 19.687890
    ## 1182  19.419476 18.488165
    ## 1183  16.623397 16.248579
    ## 1184  22.245230 19.019352
    ## 1185  22.096073 16.959727
    ## 1186  18.761083 14.963824
    ## 1187  21.183020 17.246825
    ## 1188  19.385325 19.455814
    ## 1189  25.811801 18.325636
    ## 1190  21.696752 19.282344
    ## 1191  11.875965 14.040981
    ## 1192  20.911306 17.675572
    ## 1193  23.479860 19.625167
    ## 1194  19.462110 18.207930
    ## 1195  14.717120 18.560104
    ## 1196  17.896845 18.153805
    ## 1197  18.449292 19.707147
    ## 1198  15.747515 17.532393
    ## 1199  28.217485 18.648904
    ## 1200  20.137494 18.891741
    ## 1201  25.129894 19.412418
    ## 1202  13.294815 14.728972
    ## 1203  17.397137 19.725145
    ## 1204  16.910150 18.693816
    ## 1205  28.225482 17.762970
    ## 1206  21.873326 18.671840
    ## 1207  15.048305 13.671982
    ## 1208  22.295041 15.896257
    ## 1209  19.886932 16.704536
    ## 1210  15.281182 19.990149
    ## 1211  15.250205 18.989854
    ## 1212  22.340620 18.640786
    ## 1213  17.998867 18.295466
    ## 1214  27.575951 19.793072
    ## 1215  21.685344 19.375719
    ## 1216  23.282311 19.834645
    ## 1217  19.366916 18.439231
    ## 1218  21.925275 19.588428
    ## 1219  20.590534 18.753194
    ## 1220  18.064009 16.345175
    ## 1221  21.972749 19.348355
    ## 1222  22.304120 19.060265
    ## 1223  27.191241 19.209318
    ## 1224  18.701321 19.857209
    ## 1225  18.699186 17.337343
    ## 1226  17.072554 14.026645
    ## 1227  23.850333 19.787359
    ## 1228  16.118193 14.018820
    ## 1229  16.616954 15.967798
    ## 1230  20.693525 19.678912
    ## 1231  18.267059 18.903068
    ## 1232  18.662472 19.193009
    ## 1233  14.965584 19.132266
    ## 1234  28.723263 18.717646
    ## 1235  22.195212 18.872302
    ## 1236  22.365287 19.172248
    ## 1237  21.938792 18.402896
    ## 1238  26.745505 19.235656
    ## 1239  19.644538 19.905588
    ## 1240  18.971031 15.918833
    ## 1241  18.568411 16.715797
    ## 1242  18.640117 18.829981
    ## 1243  19.081712 19.941020
    ## 1244  18.443910 19.284920
    ## 1245  20.121189 18.412219
    ## 1246  17.747339 14.249297
    ## 1247  19.640366 19.799684
    ## 1248  27.134959 19.480433
    ## 1249  14.284752 15.415190
    ## 1250  19.629857 19.401450
    ## 1251  23.109994 17.905949
    ## 1252  18.940939 19.953468
    ## 1253  24.377743 17.715825
    ## 1254  15.311010 17.579515
    ## 1255  18.869800 18.342231
    ## 1256  18.654768 19.827102
    ## 1257  20.819592 18.508911
    ## 1258  21.342247 17.305195
    ## 1259  20.598933 19.418163
    ## 1260  19.588559 18.005018
    ## 1261  20.464789 18.605291
    ## 1262  24.945657 19.826652
    ## 1263  23.547871 19.193725
    ## 1264  28.100689 18.546725
    ## 1265  19.299966 18.371350
    ## 1266  20.875728 19.924868
    ## 1267  21.554617 19.753976
    ## 1268  24.617700 19.602981
    ## 1269  16.112786 16.242888
    ## 1270  19.960028 19.979624
    ## 1271  11.943620 15.295148
    ## 1272  23.335227 18.781624
    ## 1273  21.366638 18.253847
    ## 1274  19.071336 16.811079
    ## 1275  22.111889 19.305315
    ## 1276  22.530650 18.324988
    ## 1277  15.899024 15.693457
    ## 1278  17.778184 16.693696
    ## 1279  13.676309 18.593710
    ## 1280  20.742122 17.986179
    ## 1281  16.595292 18.556244
    ## 1282  21.103177 19.203728
    ## 1283  22.042367 19.239561
    ## 1284  26.353107 19.556238
    ## 1285  14.370336 15.678287
    ## 1286  22.950571 19.385806
    ## 1287  20.413643 16.583031
    ## 1288  17.611866 19.340751
    ## 1289  13.308734 14.771542
    ## 1290  27.415526 18.428544
    ## 1291  22.451539 18.369241
    ## 1292  23.406331 19.048275
    ## 1293  14.866153 18.112243
    ## 1294  23.479534 19.230542
    ## 1295  13.753431 17.686867
    ## 1296  18.271575 18.231112
    ## 1297  17.822021 16.186629
    ## 1298  21.463010 19.768856
    ## 1299  21.705659 18.097424
    ## 1300  19.129141 18.960729
    ## 1301  19.054277 17.016438
    ## 1302  19.191870 17.813960
    ## 1303  18.542467 15.174197
    ## 1304  15.787392 18.473779
    ## 1305  17.704308 19.395533
    ## 1306  16.705921 15.710244
    ## 1307  26.207301 19.998282
    ## 1308  14.647195 11.512617
    ## 1309  14.532903 17.745739
    ## 1310  21.643877 18.547095
    ## 1311  22.212983 19.676016
    ## 1312  17.532167 18.803336
    ## 1313  17.006927 19.244709
    ## 1314  20.023184 19.345947
    ## 1315  18.101044 19.618466
    ## 1316  16.291284 19.788533
    ## 1317  14.683744 16.823081
    ## 1318  17.000563 15.043592
    ## 1319  18.876027 15.737350
    ## 1320  17.506863 19.452954
    ## 1321  21.551772 19.953167
    ## 1322  22.134948 19.806616
    ## 1323  20.252066 19.283085
    ## 1324  20.116749 18.429455
    ## 1325  25.672298 17.466252
    ## 1326  18.755669 16.468956
    ## 1327  23.982801 19.799572
    ## 1328  17.235819 19.560094
    ## 1329  16.238469 19.102798
    ## 1330  14.857918 14.800442
    ## 1331  18.710730 18.730924
    ## 1332  19.051296 18.075459
    ## 1333  23.004759 19.201776
    ## 1334  14.878226 12.884341
    ## 1335  14.930198 16.175835
    ## 1336  16.735313 19.973452
    ## 1337  17.107733 15.675861
    ## 1338  24.855414 18.333392
    ## 1339  17.797458 13.558927
    ## 1340  23.025093 18.555711
    ## 1341  24.093882 19.659254
    ## 1342  21.150321 16.798890
    ## 1343  24.274553 18.480350
    ## 1344  10.695559 15.384358
    ## 1345  14.803214 18.886628
    ## 1346  18.490643 18.199612
    ## 1347  18.168320 16.547593
    ## 1348  17.400657 17.935008
    ## 1349  18.335056 18.343231
    ## 1350  26.253528 19.802029
    ## 1351  19.101105 19.798436
    ## 1352  15.988306 17.909788
    ## 1353  20.702992 17.891837
    ## 1354  22.847742 19.619319
    ## 1355  12.693920 17.553190
    ## 1356  14.636202 17.435987
    ## 1357  14.374941 15.043746
    ## 1358  19.776613 19.112123
    ## 1359  21.601829 18.917058
    ## 1360  20.052221 19.395368
    ## 1361  23.494987 19.987188
    ## 1362  18.330726 18.326409
    ## 1363  18.981863 19.746499
    ## 1364  17.574359 17.262287
    ## 1365  14.404135 15.290425
    ## 1366  22.416822 15.471240
    ## 1367  18.373295 19.737000
    ## 1368  23.592888 19.049422
    ## 1369  18.162109 19.373985
    ## 1370  13.383684 13.597908
    ## 1371  20.573310 19.793540
    ## 1372  22.044788 19.457393
    ## 1373  17.377363 18.178300
    ## 1374  14.564107 18.057630
    ## 1375  16.736337 18.668408
    ## 1376  16.008010 15.960646
    ## 1377  17.373844 18.912538
    ## 1378  26.363601 19.493719
    ## 1379  21.763589 18.782046
    ## 1380  25.580819 19.926697
    ## 1381  19.453782 18.850844
    ## 1382  23.070087 19.822781
    ## 1383  23.368061 15.872399
    ## 1384  16.911420 19.272207
    ## 1385  17.112622 17.241106
    ## 1386  16.973899 19.720019
    ## 1387  18.504901 17.776376
    ## 1388  21.208691 19.255770
    ## 1389  16.327692 16.226632
    ## 1390  19.293378 15.963596
    ## 1391  20.194278 18.025353
    ## 1392  21.001979 19.681655
    ## 1393  23.434162 19.451301
    ## 1394  19.682174 15.587063
    ## 1395  22.679074 17.539563
    ## 1396  14.641623 13.117282
    ## 1397  13.596567 14.438948
    ## 1398  20.515027 17.768851
    ## 1399  24.230791 19.537875
    ## 1400  13.709985 15.064571
    ## 1401  18.645507 18.656476
    ## 1402  17.460591 19.506875
    ## 1403  22.132462 19.986943
    ## 1404  23.051133 19.721982
    ## 1405  20.350972 18.690866
    ## 1406  23.201295 19.854589
    ## 1407  21.476302 18.558759
    ## 1408  19.722686 19.666680
    ## 1409  16.198406 15.773690
    ## 1410  15.217876 15.948944
    ## 1411  24.009149 19.361044
    ## 1412  23.025330 19.716923
    ## 1413  23.359052 16.987356
    ## 1414  23.728973 19.320634
    ## 1415  19.450650 19.688666
    ## 1416  16.122948 16.451169
    ## 1417  23.232986 18.146395
    ## 1418  14.695936 19.841722
    ## 1419  19.153490 14.381639
    ## 1420  16.456839 17.237282
    ## 1421  16.854563 17.584814
    ## 1422  23.549031 18.824841
    ## 1423  22.495738 18.277743
    ## 1424  17.030004 19.770318
    ## 1425  20.727674 17.631694
    ## 1426  24.996620 19.633982
    ## 1427  18.541417 17.094258
    ## 1428  23.715764 19.729487
    ## 1429  24.156454 19.629496
    ## 1430  13.584753 14.744396
    ## 1431  19.550006 19.941035
    ## 1432  20.687511 19.193553
    ## 1433  18.391388 17.947408
    ## 1434  27.397447 19.012296
    ## 1435  20.680299 19.259317
    ## 1436  15.549534 12.242487
    ## 1437  24.059438 19.425808
    ## 1438  22.424162 16.725432
    ## 1439  22.688627 19.206960
    ## 1440  17.858562 16.703463
    ## 1441  23.772930 19.444176
    ## 1442  19.004475 18.953933
    ## 1443  15.082525 16.547992
    ## 1444  19.759747 16.018389
    ## 1445  20.383760 16.752652
    ## 1446  14.724024 14.888724
    ## 1447  22.854385 19.558876
    ## 1448  21.387970 19.508976
    ## 1449  22.367197 19.238004
    ## 1450  26.257397 19.603387
    ## 1451  18.390799 19.668349
    ## 1452  20.936323 19.580402
    ## 1453  16.163614 16.384647
    ## 1454  16.309789 13.914408
    ## 1455  20.235221 19.733992
    ## 1456  17.388623 17.977525
    ## 1457  19.472774 19.572213
    ## 1458  12.790614 13.815480
    ## 1459  21.573450 16.371551
    ## 1460  25.969205 19.935655
    ## 1461  17.148214 19.871423
    ## 1462  15.852169 16.254270
    ## 1463  19.191135 15.388076
    ## 1464  21.931307 18.574868
    ## 1465  24.018811 18.879083
    ## 1466  15.609480 16.812330
    ## 1467  13.428435 16.202149
    ## 1468  16.743980 18.966248
    ## 1469  18.265324 17.190771
    ## 1470  17.753857 14.587478
    ## 1471  21.704196 18.025246
    ## 1472  21.197761 18.337410
    ## 1473  22.375335 19.344860
    ## 1474  19.181567 19.762182
    ## 1475  24.400400 19.794067
    ## 1476  23.328902 16.625729
    ## 1477  18.068718 19.567830
    ## 1478  20.922728 16.078336
    ## 1479  18.446981 19.051680
    ## 1480  18.454152 19.668440
    ## 1481  25.248265 19.677942
    ## 1482  22.611656 19.153930
    ## 1483  16.121833 18.129708
    ## 1484  16.371455 18.550639
    ## 1485  22.587447 18.878465
    ## 1486   9.766709 12.808976
    ## 1487  19.829697 18.778982
    ## 1488  14.440862 18.761978
    ## 1489  19.418833 15.460243
    ## 1490  16.924812 15.320667
    ## 1491  20.880570 17.612133
    ## 1492  22.542184 17.972068
    ## 1493  22.395015 15.510463
    ## 1494  13.348326 17.200847
    ## 1495  25.957697 19.181158
    ## 1496  20.325783 18.566552
    ## 1497  23.564461 17.318290
    ## 1498  21.681852 17.891458
    ## 1499  25.027759 19.638608
    ## 1500  19.441600 17.315418
    ## 1501  19.775485 18.961244
    ## 1502  26.654155 19.902934
    ## 1503  20.454549 16.203255
    ## 1504  20.118056 15.792060
    ## 1505  16.662417 18.384081
    ## 1506  20.692226 19.963132
    ## 1507  17.225710 14.616257
    ## 1508  19.753468 18.180469
    ## 1509  24.802792 19.859460
    ## 1510  11.581737 17.136466
    ## 1511  20.124392 18.735727
    ## 1512  25.920088 18.652225
    ## 1513  11.777972 15.172826
    ## 1514  25.479631 19.514566
    ## 1515  24.568932 19.199439
    ## 1516  16.664246 19.733823
    ## 1517  13.772154 17.001193
    ## 1518  20.723218 17.352841
    ## 1519  17.479476 19.924506
    ## 1520  18.621774 19.618191
    ## 1521  17.122602 19.775338
    ## 1522  18.357215 18.217861
    ## 1523  27.254337 19.964347
    ## 1524  20.526403 15.978709
    ## 1525  11.069556 10.766609
    ## 1526  20.366350 18.939361
    ## 1527  18.802268 19.767956
    ## 1528  17.065527 14.946650
    ## 1529  19.117707 18.254591
    ## 1530  18.908220 19.782096
    ## 1531  18.763637 19.259709
    ## 1532  20.959062 18.946106
    ## 1533  20.651825 18.796932
    ## 1534  18.580591 16.221810
    ## 1535  16.077407 17.005760
    ## 1536  13.048061 12.514622
    ## 1537  20.540251 19.239649
    ## 1538  21.073747 19.972917
    ## 1539  18.258540 18.108206
    ## 1540  14.501300 18.068185
    ## 1541  17.624531 14.769250
    ## 1542  20.824624 17.893572
    ## 1543  18.904661 16.187632
    ## 1544  18.465827 18.695587
    ## 1545  20.502523 19.892021
    ## 1546  20.998343 16.753953
    ## 1547  16.919982 17.521542
    ## 1548  19.569458 16.995908
    ## 1549  22.186484 19.505551
    ## 1550  18.238816 18.825711
    ## 1551  14.996389 13.339658
    ## 1552  21.931192 18.659938
    ## 1553  18.497402 19.355034
    ## 1554  12.026131 14.258130
    ## 1555  21.422987 18.759937
    ## 1556  19.064674 19.168851
    ## 1557  20.483542 18.387285
    ## 1558  18.228244 16.580840
    ## 1559  23.064503 19.598001
    ## 1560  19.249440 19.741566
    ## 1561  20.367360 18.074890
    ## 1562  22.985143 19.828129
    ## 1563  21.751197 19.202207
    ## 1564  18.645671 18.057565
    ## 1565  24.269956 19.917317
    ## 1566  16.175091 15.221785
    ## 1567  18.423965 15.890088
    ## 1568  23.270528 19.412218
    ## 1569  19.196028 18.499582
    ## 1570  13.136550 18.567979
    ## 1571  21.553419 18.987987
    ## 1572  21.800310 18.783588
    ## 1573  23.267918 19.496263
    ## 1574  24.195100 19.448772
    ## 1575  17.167407 19.886847
    ## 1576  13.857428 17.473357
    ## 1577  22.988898 17.482290
    ## 1578  20.129928 19.340742
    ## 1579  17.104376 19.510106
    ## 1580  20.574751 18.763204
    ## 1581  16.831647 16.075065
    ## 1582  25.902793 19.335806
    ## 1583  26.337828 18.959922
    ## 1584  16.919753 18.170837
    ## 1585  21.983572 18.594991
    ## 1586  21.635286 19.040721
    ## 1587  26.670052 18.956055
    ## 1588  21.604209 17.662865
    ## 1589  17.012584 19.607768
    ## 1590  26.108183 19.197930
    ## 1591  30.319154 19.893591
    ## 1592  19.470913 17.045987
    ## 1593  21.836200 14.432897
    ## 1594  22.914940 19.592433
    ## 1595  18.576765 19.602036
    ## 1596  16.014664 16.790281
    ## 1597  16.222811 19.210853
    ## 1598  28.868533 19.937944
    ## 1599  23.574731 16.791883
    ## 1600  15.052655 17.942188
    ## 1601  19.466173 19.311222
    ## 1602  16.289147 16.024189
    ## 1603  19.631655 16.735063
    ## 1604  19.017801 18.082193
    ## 1605  25.256720 19.674978
    ## 1606  21.195231 19.893617
    ## 1607  23.590427 19.238480
    ## 1608  21.914352 19.625751
    ## 1609  17.426865 18.587091
    ## 1610  16.362159 19.627314
    ## 1611  13.960497 14.625334
    ## 1612  19.325308 16.217482
    ## 1613  17.793893 14.488925
    ## 1614  16.266981 17.842018
    ## 1615  17.192356 19.139890
    ## 1616  18.359877 18.608963
    ## 1617  17.267357 18.173917
    ## 1618  14.596210 15.075110
    ## 1619  19.933972 19.924626
    ## 1620  17.724187 15.805939
    ## 1621  18.932764 17.806961
    ## 1622  22.150063 18.030620
    ## 1623  18.527683 16.525658
    ## 1624  22.081271 17.976376
    ## 1625  14.267990 16.723826
    ## 1626  20.996798 19.974081
    ## 1627  16.501433 17.005281
    ## 1628  19.376500 19.560320
    ## 1629  21.871780 16.487463
    ## 1630  17.498878 15.033659
    ## 1631  19.545934 17.156927
    ## 1632  18.248243 19.372540
    ## 1633  18.942607 19.093536
    ## 1634  23.735424 17.008842
    ## 1635  21.131150 15.495771
    ## 1636  19.014106 18.571562
    ## 1637  16.373569 18.054066
    ## 1638  15.703929 15.550784
    ## 1639  21.865390 16.989468
    ## 1640  20.139248 19.344280
    ## 1641  20.192047 18.218753
    ## 1642  18.439026 15.690304
    ## 1643  19.166253 19.199737
    ## 1644  22.093279 15.577778
    ## 1645  23.475969 18.165043
    ## 1646  27.090557 19.190200
    ## 1647  25.739230 17.179589
    ## 1648  24.868417 18.677722
    ## 1649  16.843536 18.092311
    ## 1650  19.510036 19.943754
    ## 1651  18.006909 18.242277
    ## 1652  26.215331 19.903898
    ## 1653  16.666865 18.542239
    ## 1654  18.796947 17.733807
    ## 1655  21.721222 19.446310
    ## 1656  19.570425 16.126020
    ## 1657  21.769931 19.867987
    ## 1658  20.989957 18.584604
    ## 1659  11.268387 16.967608
    ## 1660  29.334855 19.553822
    ## 1661  18.670397 19.974991
    ## 1662  13.428924 12.063097
    ## 1663  18.706059 19.693890
    ## 1664  18.594262 19.735854
    ## 1665  19.209183 19.230149
    ## 1666  17.665833 17.393107
    ## 1667  23.266065 19.943276
    ## 1668  10.594163  9.889789
    ## 1669  21.811014 19.992416
    ## 1670  28.438695 19.651056
    ## 1671  19.805439 19.953223
    ## 1672  15.543697 16.772007
    ## 1673  23.507884 19.883648
    ## 1674  24.783096 18.756664
    ## 1675  25.535892 19.278763
    ## 1676  21.009421 16.951577
    ## 1677  25.141905 19.358390
    ## 1678  19.459360 16.011095
    ## 1679  18.317194 19.481663
    ## 1680  24.944911 18.611204
    ## 1681  20.334321 19.097894
    ## 1682  15.994074 16.110613
    ## 1683  23.669631 16.309841
    ## 1684  26.135475 18.570265
    ## 1685  18.370095 19.806486
    ## 1686  23.367683 19.507034
    ## 1687  20.320909 17.903875
    ## 1688  24.314941 19.953328
    ## 1689  21.780056 18.122320
    ## 1690  27.458218 19.238179
    ## 1691  24.386900 19.819927
    ## 1692  20.505040 17.214046
    ## 1693  20.102182 19.189090
    ## 1694  24.458790 17.859006
    ## 1695  21.196887 19.662849
    ## 1696  14.086801 16.630556
    ## 1697  23.184854 19.180718
    ## 1698  23.695807 16.865427
    ## 1699  20.298486 17.480269
    ## 1700  17.205849 17.736342
    ## 1701  17.061386 19.173209
    ## 1702  17.874817 18.054396
    ## 1703  19.475452 17.797746
    ## 1704  15.479034 17.771995
    ## 1705  16.610930 18.392454
    ## 1706  23.226188 18.514808
    ## 1707  19.277801 19.694700
    ## 1708  22.942667 19.936519
    ## 1709  22.105087 18.459578
    ## 1710  14.451663 19.606687
    ## 1711  21.550389 18.598366
    ## 1712  19.843557 18.616947
    ## 1713  22.086802 17.738821
    ## 1714  24.597360 19.383730
    ## 1715  19.140681 18.678613
    ## 1716  22.742520 18.304275
    ## 1717  21.926600 16.025447
    ## 1718  16.077496 19.919482
    ## 1719  10.843451 17.720355
    ## 1720  14.669062 17.468145
    ## 1721  18.400679 18.802783
    ## 1722  25.495956 19.638237
    ## 1723  23.016625 19.461411
    ## 1724  21.756921 16.420616
    ## 1725  19.045588 17.345216
    ## 1726  21.616223 19.765074
    ## 1727  21.624135 18.764642
    ## 1728  26.300698 19.713956
    ## 1729  19.692218 16.988129
    ## 1730  22.695497 18.432861
    ## 1731  21.714805 18.368671
    ## 1732  15.279964 17.647166
    ## 1733  20.627482 19.375366
    ## 1734  18.429675 19.273603
    ## 1735  27.619327 19.898167
    ## 1736  22.571188 18.249076
    ## 1737  18.393582 19.533828
    ## 1738  22.789722 19.843058
    ## 1739  21.001152 14.923655
    ## 1740  23.141855 18.431698
    ## 1741  25.262260 19.079802
    ## 1742  18.299191 17.766222
    ## 1743  22.771973 16.306707
    ## 1744  21.744150 19.611364
    ## 1745  13.991522 18.502032
    ## 1746  22.011040 19.705334
    ## 1747  20.167879 17.938494
    ## 1748  17.404787 15.833472
    ## 1749  19.193919 16.604764
    ## 1750  22.368325 19.399830
    ## 1751  11.828494 14.925081
    ## 1752  11.742897 14.900383
    ## 1753  21.925472 19.653199
    ## 1754  16.427574 19.194834
    ## 1755  19.532368 19.103588
    ## 1756  20.863877 19.070528
    ## 1757  18.949036 19.387505
    ## 1758  17.418942 18.747461
    ## 1759  18.254859 19.809129
    ## 1760  17.176135 18.229273
    ## 1761  21.534164 19.682253
    ## 1762  21.434277 16.159058
    ## 1763  19.589304 19.931499
    ## 1764  15.837642 19.805060
    ## 1765  19.701543 19.814586
    ## 1766  20.760441 19.077861
    ## 1767  13.716957 17.321121
    ## 1768  16.642635 18.361487
    ## 1769  23.591367 17.261794
    ## 1770  21.440092 18.459640
    ## 1771  22.729889 19.001757
    ## 1772  17.119403 15.168625
    ## 1773  17.274425 18.986096
    ## 1774  26.116047 18.480193
    ## 1775  18.062614 19.485176
    ## 1776  25.930666 19.503245
    ## 1777  26.132668 18.135022
    ## 1778  23.410795 18.771385
    ## 1779  25.248535 19.580643
    ## 1780  19.762859 19.835005
    ## 1781  19.619912 17.598172
    ## 1782  14.741688 18.110127
    ## 1783  17.203158 19.433924
    ## 1784  21.030510 18.370325
    ## 1785  20.223789 19.847096
    ## 1786  20.328942 18.960636
    ## 1787  21.616612 17.621812
    ## 1788  20.232271 15.721525
    ## 1789  19.631883 19.383736
    ## 1790  12.641674 18.980955
    ## 1791  20.761394 16.962538
    ## 1792  17.602624 19.145591
    ## 1793  23.781634 19.317140
    ## 1794  19.944245 15.864578
    ## 1795  17.737316 19.756485
    ## 1796  14.161894 16.312169
    ## 1797  13.809605 17.082573
    ## 1798  18.947618 18.604014
    ## 1799  20.686766 19.829122
    ## 1800  11.563690 15.761834
    ## 1801  13.883922 14.007377
    ## 1802  21.606849 19.419539
    ## 1803  25.790893 18.951131
    ## 1804  15.330494 15.262475
    ## 1805  15.628262 19.994634
    ## 1806  10.381909 16.527021
    ## 1807  20.366084 18.710659
    ## 1808  23.953537 18.847326
    ## 1809  19.545841 18.104518
    ## 1810  19.593864 18.876187
    ## 1811  14.303284 13.690995
    ## 1812  15.232365 12.096159
    ## 1813  24.401711 19.786293
    ## 1814  17.990502 16.552464
    ## 1815  23.617827 19.947981
    ## 1816  15.562842 18.310625
    ## 1817  12.292851 18.075347
    ## 1818  19.098510 18.802973
    ## 1819  21.157498 19.879280
    ## 1820  19.091808 18.075284
    ## 1821  21.133122 19.971239
    ## 1822  17.469837 18.922639
    ## 1823  23.238102 18.971301
    ## 1824  15.539832 13.589056
    ## 1825  19.993784 18.692085
    ## 1826  28.182961 18.793072
    ## 1827  17.991102 16.861424
    ## 1828  18.689920 18.817449
    ## 1829  16.457962 19.625393
    ## 1830  20.710331 18.930342
    ## 1831  20.672310 19.898076
    ## 1832  15.540403 16.953074
    ## 1833  20.779935 19.368897
    ## 1834  26.166771 18.862252
    ## 1835  19.208133 19.546786
    ## 1836  12.359323 16.573651
    ## 1837  18.378029 18.439211
    ## 1838  19.583680 18.346450
    ## 1839  16.068919 13.781866
    ## 1840  17.475271 17.478355
    ## 1841  19.313653 18.867976
    ## 1842  17.827640 19.309913
    ## 1843  16.347213 18.185851
    ## 1844  18.393675 17.675359
    ## 1845  18.877975 17.949364
    ## 1846  24.228843 18.146316
    ## 1847  22.504749 19.511102
    ## 1848  20.336273 13.573645
    ## 1849  22.119855 19.971135
    ## 1850  19.910060 18.255853
    ## 1851  23.424218 19.005963
    ## 1852  18.614211 19.121046
    ## 1853  22.644771 19.573139
    ## 1854  19.841449 19.782444
    ## 1855  19.033159 15.579343
    ## 1856  20.879768 19.111128
    ## 1857  22.608263 19.952021
    ## 1858  23.335348 19.169348
    ## 1859  16.202925 16.991139
    ## 1860  13.959221 17.884104
    ## 1861  19.476943 17.669158
    ## 1862  19.280963 13.717126
    ## 1863  25.090796 16.479706
    ## 1864  18.620201 19.070714
    ## 1865  21.765237 18.342884
    ## 1866  24.812999 19.922143
    ## 1867  21.280694 18.651158
    ## 1868  16.881612 13.817956
    ## 1869  17.770397 18.083004
    ## 1870  17.118362 16.628931
    ## 1871  22.650917 18.113288
    ## 1872  25.476141 19.157418
    ## 1873  18.759002 16.732507
    ## 1874  14.182219 11.616974
    ## 1875  21.239181 19.899965
    ## 1876  18.577927 19.147793
    ## 1877  20.543037 18.743023
    ## 1878  21.490167 18.375473
    ## 1879  19.350530 19.799422
    ## 1880  12.924050 17.997749
    ## 1881  23.111844 19.180314
    ## 1882  18.130170 19.690482
    ## 1883  18.403550 17.512257
    ## 1884  20.549591 16.746556
    ## 1885  16.345581 19.867147
    ## 1886  13.114366 13.365967
    ## 1887  20.666877 17.548665
    ## 1888  21.021718 19.094528
    ## 1889  19.391946 17.172224
    ## 1890  17.293529 19.377588
    ## 1891  15.078157 18.378031
    ## 1892  21.517631 17.460417
    ## 1893  17.310842 15.826023
    ## 1894  23.355771 18.746427
    ## 1895  23.207956 19.950402
    ## 1896  23.095708 19.840053
    ## 1897  25.405412 18.540455
    ## 1898  17.903721 18.021362
    ## 1899  24.630983 19.656443
    ## 1900  20.493869 17.680004
    ## 1901  20.431620 17.336703
    ## 1902  19.537309 15.374572
    ## 1903  17.161191 16.867140
    ## 1904  16.139152 18.455311
    ## 1905  22.458316 17.042313
    ## 1906  20.808485 18.858658
    ## 1907  23.359775 18.644334
    ## 1908  26.221233 18.184414
    ## 1909  21.394088 18.620451
    ## 1910  16.633268 18.967951
    ## 1911  27.607929 19.175835
    ## 1912  21.743202 19.972797
    ## 1913  15.317593 17.654711
    ## 1914  16.267308 19.197926
    ## 1915  15.190512 14.803968
    ## 1916  19.019611 19.169218
    ## 1917  23.915641 19.191085
    ## 1918  18.652949 17.862311
    ## 1919  17.520265 15.150646
    ## 1920  14.800194 16.679340
    ## 1921  25.270139 19.218396
    ## 1922  26.191451 17.985036
    ## 1923  20.123482 19.536819
    ## 1924  19.283244 18.425242
    ## 1925  17.085898 19.021320
    ## 1926  18.404582 18.382581
    ## 1927  25.281286 18.131191
    ## 1928  13.384916 19.202610
    ## 1929  16.909361 15.803660
    ## 1930  19.171854 16.013650
    ## 1931  19.724336 17.767264
    ## 1932  22.050109 17.406167
    ## 1933  18.513430 19.648843
    ## 1934  23.303173 19.299081
    ## 1935  16.357598 19.752906
    ## 1936  19.492783 18.236951
    ## 1937  21.395810 16.006317
    ## 1938  20.891543 19.985306
    ## 1939  24.212539 18.077645
    ## 1940  18.787049 16.185000
    ## 1941  20.830336 19.844263
    ## 1942  28.401586 18.645315
    ## 1943  22.703432 19.175924
    ## 1944  21.864163 18.747575
    ## 1945  16.689642 19.415538
    ## 1946  27.246772 18.917970
    ## 1947  16.095820 19.759120
    ## 1948  20.640704 19.696143
    ## 1949  20.287273 18.450916
    ## 1950  22.451440 19.047879
    ## 1951  18.251287 17.814201
    ## 1952  25.160489 19.836176
    ## 1953  23.714050 19.896567
    ## 1954  13.561542 16.558343
    ## 1955  17.456766 16.980109
    ## 1956  16.115435 15.218351
    ## 1957  16.183814 17.244559
    ## 1958  19.832875 19.499744
    ## 1959  24.713815 19.379568
    ## 1960  20.200461 16.757666
    ## 1961  17.649055 19.945904
    ## 1962  19.490216 19.934265
    ## 1963  17.645505 18.347221
    ## 1964  19.441516 18.499267
    ## 1965  12.132407 17.920327
    ## 1966  17.466402 13.956823
    ## 1967  18.196926 17.988787
    ## 1968  22.669307 19.613029
    ## 1969  17.926423 19.568183
    ## 1970  13.883374 14.799291
    ## 1971  20.039143 15.508738
    ## 1972  18.205875 19.466866
    ## 1973  26.688432 18.920535
    ## 1974  21.112645 19.834437
    ## 1975  23.435889 19.401037
    ## 1976  24.297654 19.450768
    ## 1977  15.855087 14.921255
    ## 1978  24.758691 19.722233
    ## 1979  15.667139 16.191960
    ## 1980  22.219959 17.009207
    ## 1981  22.254967 17.001129
    ## 1982  25.673697 18.798028
    ## 1983  21.555943 18.638397
    ## 1984  17.474491 19.580326
    ## 1985  21.085248 17.682679
    ## 1986  19.339567 16.094970
    ## 1987  29.190451 19.111146
    ## 1988  15.800283 14.131287
    ## 1989  22.882091 19.375306
    ## 1990  21.038312 19.227740
    ## 1991  22.219653 19.031542
    ## 1992  18.334630 18.509529
    ## 1993  17.766359 14.967356
    ## 1994  21.570550 18.635440
    ## 1995  21.380301 18.433343
    ## 1996  13.694972 12.748584
    ## 1997  21.189193 18.837057
    ## 1998  11.727726 13.619881
    ## 1999  21.082316 19.724357
    ## 2000  21.812639 19.648659
    ## 2001  19.724957 18.749025
    ## 2002  15.257931 13.111360
    ## 2003  18.971635 18.648668
    ## 2004  22.694967 15.933609
    ## 2005  17.711306 18.977224
    ## 2006  18.342951 17.679732
    ## 2007  16.218647 19.974273
    ## 2008  22.010658 19.631253
    ## 2009  16.034822 18.792743
    ## 2010  20.674751 19.014253
    ## 2011  19.870475 19.990593
    ## 2012  20.978921 17.734933
    ## 2013  23.143118 18.870657
    ## 2014  20.907495 18.539431
    ## 2015  18.117429 19.351696
    ## 2016  15.973448 19.899345
    ## 2017  19.904496 16.186077
    ## 2018  24.232339 17.697659
    ## 2019  18.099866 19.493225
    ## 2020  22.451323 18.944044
    ## 2021  27.224247 19.037848
    ## 2022  21.526359 19.559172
    ## 2023  15.970776 18.933013
    ## 2024  25.807618 17.272201
    ## 2025  17.970894 18.892182
    ## 2026  26.952764 18.576371
    ## 2027  13.455909 18.124304
    ## 2028  17.384008 19.743500
    ## 2029  20.278989 18.131767
    ## 2030  21.725312 19.872432
    ## 2031  19.502341 19.917769
    ## 2032  17.735800 19.321606
    ## 2033  20.825675 19.552200
    ## 2034  21.754323 19.329415
    ## 2035  18.616536 19.283831
    ## 2036  23.264633 17.917819
    ## 2037  19.624986 17.503874
    ## 2038  22.704565 19.389297
    ## 2039  24.073309 18.728649
    ## 2040  18.949624 18.100095
    ## 2041  22.515376 18.885206
    ## 2042  19.576762 19.159466
    ## 2043  17.367128 19.669919
    ## 2044  30.077485 19.584315
    ## 2045  16.008159 18.009297
    ## 2046  21.918909 19.809215
    ## 2047  19.824341 16.780985
    ## 2048  23.063021 19.634714
    ## 2049  20.882482 19.496816
    ## 2050  20.787781 19.315293
    ## 2051  26.950144 19.725003
    ## 2052  14.738437 14.277102
    ## 2053  23.058247 18.085366
    ## 2054  18.755925 15.242470
    ## 2055  22.631723 17.752062
    ## 2056  21.575180 17.212935
    ## 2057  26.133121 18.644449
    ## 2058  15.841638 18.680471
    ## 2059  19.744856 18.256326
    ## 2060  21.054475 19.837718
    ## 2061  23.163480 16.407832
    ## 2062  24.547117 19.515933
    ## 2063  15.380131 18.277223
    ## 2064  26.603242 18.500586
    ## 2065  24.983514 17.812342
    ## 2066  16.055177 17.624593
    ## 2067  19.843683 19.849993
    ## 2068  21.630936 19.281428
    ## 2069  25.566771 18.445109
    ## 2070  20.301006 17.324643
    ## 2071  17.053728 18.298958
    ## 2072  25.422495 18.846439
    ## 2073  15.525984 17.836966
    ## 2074  20.485111 19.699694
    ## 2075  19.454843 19.939663
    ## 2076  22.153868 17.805023
    ## 2077  20.815610 19.866999
    ## 2078  23.480053 19.692059
    ## 2079  21.508551 17.668981
    ## 2080  17.721670 19.867000
    ## 2081  19.766778 19.139564
    ## 2082  25.729600 18.851961
    ## 2083  21.709299 16.461105
    ## 2084  19.242974 18.515085
    ## 2085  18.638906 18.053664
    ## 2086  22.258280 18.228947
    ## 2087  20.513239 18.113894
    ## 2088  22.278029 18.334411
    ## 2089  22.705875 18.583253
    ## 2090  26.434648 18.802170
    ## 2091  22.043587 18.402463
    ## 2092  20.560388 19.246383
    ## 2093  13.127844 16.181963
    ## 2094  22.998765 19.442537
    ## 2095  17.918882 19.634122
    ## 2096  13.019858 14.069541
    ## 2097  19.401617 19.436734
    ## 2098  16.758542 19.766369
    ## 2099  16.996922 17.596948
    ## 2100  20.384559 17.813244
    ## 2101  13.911439 19.614149
    ## 2102  21.680251 17.503142
    ## 2103  25.993718 19.905487
    ## 2104  13.367740 18.241404
    ## 2105  23.076665 19.693721
    ## 2106  23.232439 17.041276
    ## 2107  22.972184 17.068284
    ## 2108  16.548355 18.474022
    ## 2109  18.598330 16.977257
    ## 2110  16.362001 17.492363
    ## 2111  17.942817 16.762521
    ## 2112  22.688146 19.713556
    ## 2113  16.268415 13.876277
    ## 2114  22.340981 19.911990
    ## 2115  19.750765 15.905935
    ## 2116  24.897566 18.394467
    ## 2117  20.501417 16.716285
    ## 2118  17.311161 18.044760
    ## 2119  18.959155 19.381893
    ## 2120  15.931654 17.227084
    ## 2121  24.540398 19.276115
    ## 2122  22.444925 18.583771
    ## 2123  25.804671 18.620196
    ## 2124  19.138182 17.741980
    ## 2125  16.686960 16.783339
    ## 2126  22.072739 19.349462
    ## 2127  25.076444 18.620717
    ## 2128  26.109487 19.989899
    ## 2129  17.647426 17.349528
    ## 2130  19.592672 19.311726
    ## 2131  20.312969 19.419665
    ## 2132  20.656455 19.682167
    ## 2133  18.082696 15.184787
    ## 2134  20.754158 18.227659
    ## 2135  19.930618 17.464357
    ## 2136  15.818578 15.431518
    ## 2137  19.405207 19.143318
    ## 2138  24.142275 19.478529
    ## 2139  23.174029 19.984854
    ## 2140  15.565729 17.584449
    ## 2141  16.687211 14.430391
    ## 2142  21.405564 19.729970
    ## 2143  15.916008 16.795977
    ## 2144  23.329444 19.311332
    ## 2145  18.733409 18.780591
    ## 2146  21.405447 18.798865
    ## 2147  17.704272 17.299353
    ## 2148  14.661291 18.208317
    ## 2149  21.589383 17.016145
    ## 2150  22.879865 18.396827
    ## 2151  25.655508 19.643102
    ## 2152  19.734247 15.956050
    ## 2153  17.548178 19.158657
    ## 2154  18.553913 18.593723
    ## 2155  13.216304 18.116472
    ## 2156  21.777144 18.652039
    ## 2157  15.957472 17.465419
    ## 2158  15.525762 15.367157
    ## 2159  17.150283 16.407009
    ## 2160  14.314428 16.713646
    ## 2161  24.381700 17.186339
    ## 2162  13.545960 19.767283
    ## 2163  21.309692 17.605829
    ## 2164  18.567941 19.096015
    ## 2165  25.473281 17.585981
    ## 2166  23.161016 19.979226
    ## 2167  16.133844 13.295176
    ## 2168  20.159605 16.244928
    ## 2169  20.282313 17.934631
    ## 2170  22.251900 18.858079
    ## 2171  23.281478 18.622104
    ## 2172  15.885164 16.885420
    ## 2173  18.393914 16.171760
    ## 2174  20.965273 19.897516
    ## 2175  18.527427 18.886019
    ## 2176  24.586830 19.359251
    ## 2177  21.713445 18.466920
    ## 2178  27.170327 19.780366
    ## 2179  27.366871 19.716957
    ## 2180  20.050163 17.957092
    ## 2181  22.181483 19.767464
    ## 2182  17.510513 18.398426
    ## 2183  13.723269 19.178460
    ## 2184  24.784568 17.537872
    ## 2185  14.928859 17.218980
    ## 2186  21.460905 19.173356
    ## 2187  13.751127 19.463305
    ## 2188  21.457218 19.897799
    ## 2189  18.663781 17.777191
    ## 2190  19.523240 18.266042
    ## 2191  22.771119 19.351202
    ## 2192  19.986912 18.977838
    ## 2193  24.545752 19.898150
    ## 2194  21.625115 19.866517
    ## 2195  19.708203 19.413879
    ## 2196  18.797259 15.898382
    ## 2197  26.966492 19.667512
    ## 2198  22.520053 19.840175
    ## 2199  21.173745 18.071040
    ## 2200  11.166627 15.818179
    ## 2201  13.979580 13.684030
    ## 2202  26.627936 19.896201
    ## 2203  22.160559 17.362824
    ## 2204  25.273925 17.104111
    ## 2205  21.657201 19.387720
    ## 2206  13.192389 18.470781
    ## 2207  14.309074 15.893945
    ## 2208  20.284517 17.873202
    ## 2209  26.481291 18.820743
    ## 2210  23.359999 17.097228
    ## 2211  21.158406 18.940926
    ## 2212  16.999390 18.617145
    ## 2213  15.445065 19.868163
    ## 2214  16.270388 18.986609
    ## 2215  22.435467 16.550564
    ## 2216  19.947859 18.540293
    ## 2217  14.880511 18.818566
    ## 2218  18.632064 19.995663
    ## 2219  20.820992 17.301985
    ## 2220  26.802234 19.830944
    ## 2221  18.483300 17.401685
    ## 2222  19.267742 16.659989
    ## 2223  21.537129 17.864129
    ## 2224  20.410802 18.498601
    ## 2225  16.263845 17.815259
    ## 2226  17.387402 16.904867
    ## 2227  26.819935 19.800911
    ## 2228  20.828721 18.205838
    ## 2229  22.793645 18.056393
    ## 2230  23.883925 18.950526
    ## 2231  20.487731 18.032442
    ## 2232  16.260726 17.413461
    ## 2233  15.533255 17.899958
    ## 2234  16.934524 16.733815
    ## 2235  20.736416 18.958503
    ## 2236  22.938622 18.721175
    ## 2237  15.464734 18.330347
    ## 2238  20.532371 18.702329
    ## 2239  23.205581 16.414486
    ## 2240  20.690694 19.769553
    ## 2241  24.564451 19.109815
    ## 2242  18.729907 17.346849
    ## 2243  19.721347 18.674570
    ## 2244  20.765128 19.274250
    ## 2245  26.122227 18.744356
    ## 2246  21.109053 19.810611
    ## 2247  24.290283 19.409384
    ## 2248  20.824209 18.769715
    ## 2249  27.630803 19.995014
    ## 2250  19.528721 19.583701
    ## 2251  18.005692 17.093040
    ## 2252  23.474780 19.934432
    ## 2253  19.524356 16.262897
    ## 2254  15.905141 19.851794
    ## 2255  23.281692 18.210127
    ## 2256  24.994509 18.716025
    ## 2257  18.788495 18.644914
    ## 2258  16.388559 16.931295
    ## 2259  18.000602 17.045916
    ## 2260  27.233674 19.537073
    ## 2261  20.025155 12.945599
    ## 2262  18.021832 18.585837
    ## 2263  17.997542 18.147634
    ## 2264  18.480270 18.430685
    ## 2265  12.639035 13.990677
    ## 2266  12.826937 15.305098
    ## 2267  23.631289 18.040458
    ## 2268  17.329128 17.836430
    ## 2269  19.290553 18.760535
    ## 2270  14.218024 18.812858
    ## 2271  25.038041 19.853819
    ## 2272  21.014697 17.565419
    ## 2273  19.343285 17.608310
    ## 2274  16.574058 17.072734
    ## 2275  20.579438 19.792810
    ## 2276  21.801618 19.996372
    ## 2277  18.365748 19.561601
    ## 2278  21.209392 19.275806
    ## 2279  19.976357 19.677882
    ## 2280  20.423053 19.832830
    ## 2281  18.784091 19.315243
    ## 2282  18.832811 17.717204
    ## 2283  19.869923 19.762745
    ## 2284  17.281879 17.591255
    ## 2285  21.558875 19.027662
    ## 2286  20.322943 19.389365
    ## 2287  20.961284 18.690318
    ## 2288  18.016764 19.497730
    ## 2289  23.362141 18.928323
    ## 2290  17.487940 19.580293
    ## 2291  23.928976 19.766534
    ## 2292  19.823184 18.980966
    ## 2293  20.907892 19.129213
    ## 2294  26.882392 18.954881
    ## 2295  17.922875 18.309817
    ## 2296  19.533304 19.686655
    ## 2297  24.714458 19.787660
    ## 2298  20.913973 17.982499
    ## 2299  15.355610 17.747499
    ## 2300  22.554280 19.909839
    ## 2301  20.160874 18.014603
    ## 2302  10.931344 16.159152
    ## 2303  19.006618 15.255242
    ## 2304  23.314635 19.754855
    ## 2305  18.838342 18.668311
    ## 2306  17.109640 16.170736
    ## 2307  19.401945 17.063040
    ## 2308  24.193919 17.707674
    ## 2309  13.933311 11.826767
    ## 2310  21.421756 16.960016
    ## 2311  17.941084 18.278922
    ## 2312  20.262406 17.622542
    ## 2313  19.206876 17.926557
    ## 2314  15.051329 19.465219
    ## 2315  18.377680 16.061839
    ## 2316  22.814469 16.292058
    ## 2317  23.633132 17.173325
    ## 2318  21.862594 18.012808
    ## 2319  17.301101 19.028196
    ## 2320  16.819140 19.017889
    ## 2321  19.734616 17.475471
    ## 2322  21.393151 16.522966
    ## 2323  23.719788 19.316515
    ## 2324  15.075542 13.857319
    ## 2325  20.742353 15.609223
    ## 2326  17.291976 15.344472
    ## 2327  19.507005 19.553581
    ## 2328  14.765231 18.476224
    ## 2329  22.060850 19.544935
    ## 2330  23.130403 19.181793
    ## 2331  18.348186 16.960649
    ## 2332  17.647946 18.759579
    ## 2333  19.953956 17.568342
    ## 2334  25.821986 18.096784
    ## 2335  19.829137 16.678325
    ## 2336  16.982115 17.551998
    ## 2337  16.117830 19.094750
    ## 2338  22.384438 16.325756
    ## 2339  16.613573 19.723519
    ## 2340  16.671682 14.972511
    ## 2341  16.349806 18.779534
    ## 2342  12.634539 18.692270
    ## 2343  18.296831 16.423024
    ## 2344  23.260921 18.602641
    ## 2345  18.951538 16.398812
    ## 2346  24.704051 19.486213
    ## 2347  18.470253 14.866371
    ## 2348  17.285654 17.672099
    ## 2349  13.179157 16.634527
    ## 2350  16.976741 15.916694
    ## 2351  17.117107 19.987642
    ## 2352  18.503115 19.911749
    ## 2353  22.521807 19.438320
    ## 2354  21.403925 17.681014
    ## 2355  20.441437 19.465680
    ## 2356  17.599032 17.780850
    ## 2357  16.743371 18.155797
    ## 2358  20.478498 17.603967
    ## 2359  18.736969 19.014971
    ## 2360  21.162563 19.871483
    ## 2361  17.827192 19.458594
    ## 2362  21.061121 19.507283
    ## 2363  23.380284 18.694916
    ## 2364  17.982961 19.618174
    ## 2365  17.882237 18.497762
    ## 2366  20.193767 19.483883
    ## 2367  23.698882 18.824445
    ## 2368  14.343806 16.858342
    ## 2369  23.696279 19.262086
    ## 2370  17.106945 19.786880
    ## 2371  21.571723 17.044471
    ## 2372  13.393402 17.812925
    ## 2373  15.769338 16.553708
    ## 2374  17.879797 13.293430
    ## 2375  21.363854 17.553185
    ## 2376  20.268601 19.614873
    ## 2377  18.406905 19.006431
    ## 2378  17.211192 17.811948
    ## 2379  25.407891 19.989621
    ## 2380  20.059835 19.737687
    ## 2381  13.746024 18.643817
    ## 2382   9.257307 15.750604
    ## 2383  25.666395 18.213232
    ## 2384  15.699746 19.280255
    ## 2385  12.900503 11.940998
    ## 2386  18.225662 18.662412
    ## 2387  18.334633 19.741738
    ## 2388  22.022730 18.894252
    ## 2389  19.022284 19.629377
    ## 2390  15.291861 16.505632
    ## 2391  20.754537 17.372126
    ## 2392  18.142920 17.497512
    ## 2393  19.685274 16.901969
    ## 2394  17.953452 19.910851
    ## 2395  16.452710 14.736857
    ## 2396  23.498344 19.082376
    ## 2397  22.845237 18.034472
    ## 2398  21.082929 19.623263
    ## 2399  23.594846 18.348162
    ## 2400  18.444192 18.617162
    ## 2401  23.653304 19.651516
    ## 2402  18.048919 16.652920
    ## 2403  22.343598 18.894283
    ## 2404  19.717285 17.873426
    ## 2405  19.291925 19.920514
    ## 2406  17.447717 16.317074
    ## 2407  20.553433 17.631777
    ## 2408  19.019608 16.569525
    ## 2409  26.875716 19.891100
    ## 2410  16.610847 19.510793
    ## 2411  16.698869 15.786526
    ## 2412  24.972477 17.703619
    ## 2413  23.387031 18.372116
    ## 2414  12.734004 19.565310
    ## 2415  21.858910 18.910512
    ## 2416  19.484193 19.901286
    ## 2417  23.452437 19.797862
    ## 2418  21.793840 19.145031
    ## 2419  23.275272 18.437317
    ## 2420  19.574283 16.828132
    ## 2421  26.246309 18.715276
    ## 2422  22.222347 18.230916
    ## 2423  19.838267 19.405943
    ## 2424  20.757555 16.270522
    ## 2425  16.391646 13.808513
    ## 2426  16.207497 19.793880
    ## 2427  18.675441 17.826421
    ## 2428  13.624335 16.737031
    ## 2429  16.282429 17.281752
    ## 2430  14.909851 19.947466
    ## 2431  17.288986 19.148207
    ## 2432  16.850143 18.042572
    ## 2433  26.029089 18.353583
    ## 2434  18.308244 18.902370
    ## 2435  17.096799 17.167504
    ## 2436  21.247778 19.594234
    ## 2437  16.215631 16.568438
    ## 2438  17.942081 19.472060
    ## 2439  20.137239 19.130105
    ## 2440  16.647160 17.193145
    ## 2441  19.020196 17.110183
    ## 2442  21.602523 17.397058
    ## 2443  28.890556 19.494457
    ## 2444  16.861190 17.532661
    ## 2445  16.236780 19.797874
    ## 2446  17.029581 19.659393
    ## 2447  22.380937 18.040978
    ## 2448  25.881243 19.522291
    ## 2449  16.884805 19.118098
    ## 2450  21.863790 19.953349
    ## 2451  14.461001 18.314783
    ## 2452  24.119391 19.321778
    ## 2453  18.664508 18.079354
    ## 2454  16.822141 19.266778
    ## 2455  20.026952 17.518627
    ## 2456  16.142016 15.471026
    ## 2457  19.639510 19.960140
    ## 2458  23.430763 18.338382
    ## 2459  16.569422 13.683524
    ## 2460  18.737316 19.216428
    ## 2461  25.291999 19.903992
    ## 2462  18.388235 19.589419
    ## 2463  17.038153 19.791951
    ## 2464  15.627594 17.220770
    ## 2465  20.648896 17.274591
    ## 2466  18.839511 19.831286
    ## 2467  21.070677 18.228178
    ## 2468  19.109892 16.998005
    ## 2469  21.862477 18.573526
    ## 2470  22.207261 18.130059
    ## 2471   9.931395 19.037442
    ## 2472  20.634367 17.690222
    ## 2473  20.565831 19.887172
    ## 2474  20.354532 17.856288
    ## 2475  19.122688 16.265051
    ## 2476  23.061427 16.902151
    ## 2477  12.926439 19.397967
    ## 2478  27.463834 19.377492
    ## 2479  19.892248 18.780131
    ## 2480  18.205217 19.253283
    ## 2481  16.348047 13.180696
    ## 2482  15.131726 18.101504
    ## 2483  24.716151 19.548542
    ## 2484  19.248023 18.835361
    ## 2485  18.222966 16.544121
    ## 2486  17.121820 17.299882
    ## 2487  20.392073 19.947530
    ## 2488  22.031994 19.085206
    ## 2489  18.783583 15.547005
    ## 2490  19.973237 17.673388
    ## 2491  19.378228 15.079845
    ## 2492  19.828929 16.594132
    ## 2493  17.226711 17.816912
    ## 2494  19.100806 18.561504
    ## 2495  18.158462 16.410653
    ## 2496  20.220060 16.790800
    ## 2497  19.290476 17.727042
    ## 2498  26.661385 19.809483
    ## 2499  28.861756 19.350496
    ## 2500  24.507427 19.967260
    ## 2501  26.515298 19.682182
    ## 2502  21.115647 19.488844
    ## 2503  15.311280 15.391106
    ## 2504  15.917816 16.124154
    ## 2505  13.965774 18.944528
    ## 2506  25.330943 19.767152
    ## 2507  21.899689 17.274230
    ## 2508  16.745515 16.810213
    ## 2509  26.398912 19.672868
    ## 2510  18.549846 13.789690
    ## 2511  17.679318 18.519728
    ## 2512  22.041031 19.349060
    ## 2513  19.271088 14.905941
    ## 2514  15.930179 17.288131
    ## 2515  18.886949 17.887186
    ## 2516  14.942047 17.593469
    ## 2517  16.764435 18.556808
    ## 2518  20.597964 16.834051
    ## 2519  18.853802 16.980305
    ## 2520  23.019446 18.134159
    ## 2521  14.334999 14.821000
    ## 2522  18.325221 19.908832
    ## 2523  24.817161 19.453663
    ## 2524  15.722703 17.496695
    ## 2525  16.538166 18.348614
    ## 2526  22.072416 19.743444
    ## 2527  23.394063 19.028553
    ## 2528  10.536704 13.929812
    ## 2529  21.036992 18.778397
    ## 2530  19.387257 16.456514
    ## 2531  16.106626 15.428479
    ## 2532  24.184623 19.700146
    ## 2533  18.045484 18.548432
    ## 2534  21.548444 19.989922
    ## 2535  17.820182 16.093755
    ## 2536  22.431691 19.120549
    ## 2537  16.595954 19.510362
    ## 2538  20.091024 17.592822
    ## 2539  22.827290 18.732656
    ## 2540  21.960963 18.460357
    ## 2541  16.839659 16.179512
    ## 2542  16.277713 15.808685
    ## 2543  17.629486 16.867833
    ## 2544  30.302731 19.686030
    ## 2545  20.159189 16.936292
    ## 2546  28.903037 19.758769
    ## 2547  20.681819 17.390277
    ## 2548  19.466163 19.840812
    ## 2549  16.730979 18.359542
    ## 2550  23.051799 19.457436
    ## 2551  19.117670 17.538455
    ## 2552  24.098889 19.700050
    ## 2553  19.879096 16.896148
    ## 2554  22.002949 18.565026
    ## 2555  24.645992 17.560702
    ## 2556  23.961302 19.504938
    ## 2557  15.858716 18.289139
    ## 2558  24.066252 18.654572
    ## 2559  19.306008 19.199610
    ## 2560  13.753759 15.467626
    ## 2561  15.194985 18.860157
    ## 2562  19.131714 19.373915
    ## 2563  26.625198 18.046331
    ## 2564  25.496758 18.845365
    ## 2565  19.498102 14.878166
    ## 2566  16.182126 18.858602
    ## 2567  23.809645 18.770311
    ## 2568  18.344474 17.838660
    ## 2569  19.902991 19.255775
    ## 2570  20.562757 19.027580
    ## 2571  25.129589 19.411392
    ## 2572  22.349432 19.828141
    ## 2573  18.469059 17.791753
    ## 2574  17.805768 19.854537
    ## 2575  19.525596 19.668702
    ## 2576  20.230343 19.576259
    ## 2577  26.124406 19.524187
    ## 2578  17.364854 19.544590
    ## 2579  17.071823 17.983594
    ## 2580  15.702770 19.479648
    ## 2581  16.621525 15.348253
    ## 2582  23.556522 19.287015
    ## 2583  23.184555 19.440232
    ## 2584  14.752346 15.249123
    ## 2585  16.735685 19.354391
    ## 2586  13.193750 15.210659
    ## 2587  19.743907 18.799514
    ## 2588  22.669525 19.614475
    ## 2589  20.723762 15.160969
    ## 2590  20.527462 17.640766
    ## 2591  25.524214 19.670798
    ## 2592  25.429608 18.798495
    ## 2593  21.116977 19.410398
    ## 2594  22.369375 19.751437
    ## 2595  14.149200 12.804079
    ## 2596  24.889304 19.175639
    ## 2597  16.411899 16.934980
    ## 2598  25.844106 18.575802
    ## 2599  20.118510 15.784821
    ## 2600  25.060088 19.052833
    ## 2601  18.959346 19.210302
    ## 2602  19.535981 15.525186
    ## 2603  20.192363 17.506336
    ## 2604  17.628389 17.563429
    ## 2605  20.855790 18.311603
    ## 2606  24.812954 19.217802
    ## 2607  21.780318 18.825450
    ## 2608  20.975693 18.486563
    ## 2609  26.531131 17.929269
    ## 2610  20.259491 18.997257
    ## 2611  14.518047 13.297740
    ## 2612  20.182620 14.647971
    ## 2613  24.822068 19.009099
    ## 2614  19.905491 16.531268
    ## 2615  18.853511 18.039316
    ## 2616  14.352611 14.620102
    ## 2617  17.514531 18.967019
    ## 2618  22.485082 18.729540
    ## 2619  24.961236 19.221822
    ## 2620  21.203228 18.620037
    ## 2621  24.103639 19.853805
    ## 2622  18.816698 15.099625
    ## 2623  17.696472 18.171791
    ## 2624  22.089703 19.527155
    ## 2625  23.553900 19.682722
    ## 2626  14.525234 19.711559
    ## 2627  20.363607 17.824468
    ## 2628  17.573285 19.652637
    ## 2629  13.495342 12.628056
    ## 2630  21.086118 19.744255
    ## 2631  17.994742 15.192102
    ## 2632  19.623715 17.622327
    ## 2633  15.837978 14.631173
    ## 2634  16.413291 14.215818
    ## 2635  20.510871 19.118295
    ## 2636  22.263183 19.212680
    ## 2637  17.513205 19.963941
    ## 2638  18.817135 18.719624
    ## 2639  25.658757 19.295724
    ## 2640  13.109432 13.076547
    ## 2641  14.374475 17.718012
    ## 2642  18.439692 15.943421
    ## 2643  21.591179 18.439878
    ## 2644  21.825575 19.263061
    ## 2645  25.141538 19.752557
    ## 2646  15.012246 16.690130
    ## 2647  32.290302 18.350507
    ## 2648  19.602655 19.881157
    ## 2649  27.833410 18.912181
    ## 2650  25.327753 19.092110
    ## 2651  16.627982 19.966804
    ## 2652  21.585694 19.392989
    ## 2653  21.822306 17.658059
    ## 2654  23.276143 19.007617
    ## 2655  22.117661 17.219845
    ## 2656  18.257210 19.517225
    ## 2657  23.671157 18.535182
    ## 2658  18.046349 17.395722
    ## 2659  18.457731 18.006554
    ## 2660  17.798016 16.623807
    ## 2661  19.886647 18.780527
    ## 2662  17.381146 18.483366
    ## 2663  18.824465 17.839369
    ## 2664  24.175012 19.910297
    ## 2665  20.282998 15.837306
    ## 2666  18.903330 15.884177
    ## 2667  13.709206 19.213410
    ## 2668  18.531120 19.024856
    ## 2669  14.070345 15.526865
    ## 2670  18.012978 19.593050
    ## 2671  20.348126 18.498631
    ## 2672  12.034495 12.537166
    ## 2673  19.241378 16.499950
    ## 2674  19.083092 19.950610
    ## 2675  21.005042 19.356055
    ## 2676  19.116171 18.664891
    ## 2677  18.194958 18.319476
    ## 2678  20.705480 19.550129
    ## 2679  23.330937 19.546449
    ## 2680  22.499533 19.886143
    ## 2681  21.467162 18.056276
    ## 2682  26.989684 19.437603
    ## 2683  14.907115 15.866417
    ## 2684  19.747033 18.636264
    ## 2685  24.378218 18.981720
    ## 2686  17.417441 15.442855
    ## 2687  22.327063 18.446844
    ## 2688  17.505796 18.373300
    ## 2689  19.462776 16.821161
    ## 2690  23.978406 19.553138
    ## 2691  18.974517 15.206003
    ## 2692  25.927522 19.172636
    ## 2693  18.236409 17.021026
    ## 2694  22.696448 17.679822
    ## 2695  18.883142 15.308196
    ## 2696  20.842289 19.040691
    ## 2697  24.645369 19.832856
    ## 2698  24.146935 17.865631
    ## 2699  13.233749 19.371384
    ## 2700  22.407863 17.730578
    ## 2701  20.170823 19.971710
    ## 2702  25.076827 19.536336
    ## 2703  18.971970 19.464192
    ## 2704  20.146301 19.880134
    ## 2705  20.745495 18.121624
    ## 2706  20.493727 19.801516
    ## 2707  31.074355 19.198461
    ## 2708  25.474568 18.739651
    ## 2709  13.421413 18.029973
    ## 2710  11.562670 13.057590
    ## 2711  17.945766 19.914464
    ## 2712  16.943652 16.503562
    ## 2713  29.240645 19.886102
    ## 2714  19.323056 19.953739
    ## 2715  19.466279 16.171511
    ## 2716  19.831003 17.160770
    ## 2717  23.680771 19.750872
    ## 2718  22.421235 17.754871
    ## 2719  23.478954 19.688570
    ## 2720  20.896753 19.379702
    ## 2721  22.296234 19.688529
    ## 2722  19.057921 19.527947
    ## 2723  21.416278 18.858325
    ## 2724  17.874243 17.041029
    ## 2725  24.258883 19.342957
    ## 2726  21.163230 16.956473
    ## 2727  21.301021 19.131991
    ## 2728  23.779664 19.287537
    ## 2729  20.199714 19.914131
    ## 2730  21.291529 17.658369
    ## 2731  16.240243 18.097597
    ## 2732  23.799428 19.764760
    ## 2733  23.520397 18.638155
    ## 2734  18.781476 16.078966
    ## 2735  21.655734 18.895163
    ## 2736  18.586637 18.558246
    ## 2737  20.703922 17.500856
    ## 2738  22.892298 18.651063
    ## 2739  26.662511 19.885009
    ## 2740  22.214489 19.401007
    ## 2741  20.775253 16.984123
    ## 2742  21.806263 19.885131
    ## 2743  18.486780 16.636008
    ## 2744  21.679035 19.008790
    ## 2745  15.029169 19.289754
    ## 2746  32.115669 19.804147
    ## 2747  27.206257 19.903190
    ## 2748  16.265023 16.351752
    ## 2749  13.785080 18.784042
    ## 2750  24.607030 17.778734
    ## 2751  21.721232 19.493402
    ## 2752  26.971149 19.727390
    ## 2753  15.664809 18.830093
    ## 2754  17.427097 13.307664
    ## 2755  20.367353 17.995794
    ## 2756  19.952415 19.211974
    ## 2757  17.321995 18.339074
    ## 2758  21.178178 18.508811
    ## 2759  23.579187 19.607887
    ## 2760  22.891996 19.842727
    ## 2761  16.203905 18.639754
    ## 2762  19.227874 19.526696
    ## 2763  19.911117 18.586130
    ## 2764  23.127543 19.554443
    ## 2765  20.823631 18.468497
    ## 2766  18.800023 19.660573
    ## 2767  23.644120 19.671406
    ## 2768  26.024445 19.060491
    ## 2769  15.507400 14.478908
    ## 2770  21.059785 17.936585
    ## 2771  21.826566 19.674640
    ## 2772  28.081595 19.277199
    ## 2773  13.437283 16.024752
    ## 2774  20.679618 19.080895
    ## 2775  16.245874 19.109397
    ## 2776  18.757032 19.317554
    ## 2777  17.814253 12.608473
    ## 2778  16.422259 19.751832
    ## 2779  20.499412 19.814058
    ## 2780  18.246383 18.319731
    ## 2781  19.411847 19.630844
    ## 2782  17.852197 18.605662
    ## 2783  17.649299 18.373185
    ## 2784  22.608977 19.621555
    ## 2785  23.984216 19.831759
    ## 2786  23.855003 19.611676
    ## 2787  21.477005 18.004559
    ## 2788  27.756217 19.372713
    ## 2789  20.717958 19.615638
    ## 2790  20.261476 17.363361
    ## 2791  22.255369 16.521424
    ## 2792  16.934332 18.003470
    ## 2793  16.602440 19.397607
    ## 2794  21.445121 18.339692
    ## 2795  28.041367 19.739804
    ## 2796  23.025907 17.702579
    ## 2797  12.130634 15.751236
    ## 2798  20.029644 19.420640
    ## 2799  12.879641 16.706825
    ## 2800  21.379675 19.368331
    ## 2801  20.953815 18.303601
    ## 2802  18.735256 19.838359
    ## 2803  18.068036 19.897650
    ## 2804  23.488371 17.648064
    ## 2805  21.741193 19.305514
    ## 2806  21.358626 18.855821
    ## 2807  17.941655 19.380128
    ## 2808  21.210612 19.809912
    ## 2809  23.635324 19.276968
    ## 2810  15.176178 13.170404
    ## 2811  15.743557 13.815281
    ## 2812  19.676303 19.273375
    ## 2813  24.548834 17.349630
    ## 2814  20.461530 17.936296
    ## 2815  18.594038 19.097922
    ## 2816  19.679178 15.649011
    ## 2817  21.355567 17.348628
    ## 2818  23.808947 19.693098
    ## 2819  18.275716 19.839340
    ## 2820  17.096520 19.318927
    ## 2821  20.072217 18.858470
    ## 2822  21.983810 18.522489
    ## 2823  13.745576 14.213817
    ## 2824  24.687934 19.116958
    ## 2825  24.904663 19.191962
    ## 2826  16.098528 16.554011
    ## 2827  14.664489 16.533505
    ## 2828  18.706516 17.287162
    ## 2829  25.339012 19.970841
    ## 2830  15.773198 18.229390
    ## 2831  25.825135 19.099471
    ## 2832  20.077473 18.060433
    ## 2833  22.799160 19.607548
    ## 2834  21.299666 18.909542
    ## 2835  19.848193 19.295686
    ## 2836  20.228389 19.376256
    ## 2837  18.192520 18.778912
    ## 2838  20.467633 17.447924
    ## 2839  21.849241 16.911791
    ## 2840  17.155188 19.493618
    ## 2841  20.289183 18.767580
    ## 2842  18.244303 19.300982
    ## 2843  21.119163 14.938734
    ## 2844  23.934206 19.858321
    ## 2845  13.701651 15.297800
    ## 2846  24.375686 18.398837
    ## 2847  14.887152 13.651134
    ## 2848  23.048901 19.191996
    ## 2849  22.583833 19.184475
    ## 2850  18.933055 18.595028
    ## 2851  27.946299 19.276836
    ## 2852  23.958302 18.678891
    ## 2853  22.021469 16.831078
    ## 2854  26.225582 18.011439
    ## 2855  21.715379 19.673344
    ## 2856  16.888479 19.889206
    ## 2857  23.903818 17.231765
    ## 2858  17.684472 18.358453
    ## 2859  18.065502 19.441289
    ## 2860  16.805220 18.324325
    ## 2861  21.355089 19.176594
    ## 2862  20.965491 16.231886
    ## 2863  20.016167 16.267558
    ## 2864  21.772175 18.809052
    ## 2865  18.383838 17.686549
    ## 2866  19.370001 19.763458
    ## 2867  25.570006 19.566431
    ## 2868  17.787703 19.115359
    ## 2869  21.632114 19.472221
    ## 2870  17.595277 16.972907
    ## 2871  19.014346 19.236547
    ## 2872  19.041389 18.774488
    ## 2873  19.764499 15.176052
    ## 2874  18.710651 17.330615
    ## 2875  21.485791 19.555004
    ## 2876  20.585965 18.137063
    ## 2877  12.979258 16.144932
    ## 2878  17.358064 15.851729
    ## 2879  20.926240 19.258525
    ## 2880  20.885540 18.216910
    ## 2881  23.050711 19.891906
    ## 2882  21.640848 16.696003
    ## 2883  25.645936 18.363312
    ## 2884  16.571235 18.165420
    ## 2885  28.029109 19.022396
    ## 2886  23.134261 19.753836
    ## 2887  20.282090 15.694986
    ## 2888  16.366901 18.892485
    ## 2889  14.081521 18.486301
    ## 2890  21.369997 19.913381
    ## 2891  26.002934 17.879049
    ## 2892  21.179769 18.942793
    ## 2893  20.735874 16.652009
    ## 2894  21.837015 17.688476
    ## 2895  25.562437 19.730256
    ## 2896  23.570096 18.643262
    ## 2897  21.811105 17.642614
    ## 2898  14.999942 15.759574
    ## 2899  17.886340 18.026447
    ## 2900  20.170026 19.843962
    ## 2901  17.270293 16.653887
    ## 2902  20.181879 18.506097
    ## 2903  24.062178 18.315421
    ## 2904  21.353130 16.712687
    ## 2905  20.773636 18.773345
    ## 2906  20.409671 19.733505
    ## 2907  21.648903 19.929309
    ## 2908  19.967625 19.184563
    ## 2909  21.169827 16.511634
    ## 2910  18.795669 15.634885
    ## 2911  15.996166 18.021719
    ## 2912  19.300537 17.040076
    ## 2913  15.277362 17.397158
    ## 2914  22.700058 19.991024
    ## 2915  23.961027 18.962043
    ## 2916  24.258549 19.753607
    ## 2917  25.814421 19.431758
    ## 2918  19.198144 19.775608
    ## 2919  18.262632 17.019338
    ## 2920   8.598171 11.743978
    ## 2921  14.933614 15.850706
    ## 2922  20.999412 19.400506
    ## 2923  16.709875 16.486623
    ## 2924  15.090546 16.921679
    ## 2925  18.560768 19.588378
    ## 2926  20.610141 19.741353
    ## 2927  20.735616 19.151711
    ## 2928  14.339227 17.093756
    ## 2929  21.599771 16.207224
    ## 2930  19.819905 17.644256
    ## 2931  22.290704 18.325509
    ## 2932  19.331674 18.733987
    ## 2933  15.915493 16.035475
    ## 2934  20.006526 19.791412
    ## 2935  23.324142 18.609612
    ## 2936  24.262014 19.769904
    ## 2937  17.467449 17.023189
    ## 2938  20.247629 19.583497
    ## 2939  18.020044 15.558191
    ## 2940  18.856900 15.869656
    ## 2941  12.367713 17.765886
    ## 2942  25.441310 19.951552
    ## 2943  18.695345 19.867669
    ## 2944  24.410679 19.846197
    ## 2945  21.986689 18.095553
    ## 2946  16.703960 15.515946
    ## 2947  20.110221 17.199256
    ## 2948  20.712170 19.344505
    ## 2949  24.341121 19.197060
    ## 2950  14.680740 17.892099
    ## 2951  20.088907 18.074189
    ## 2952  22.624056 18.001830
    ## 2953  19.621008 19.996555
    ## 2954  25.429056 19.069208
    ## 2955  20.659678 18.959912
    ## 2956  21.309283 17.300734
    ## 2957  19.812439 18.397937
    ## 2958  18.726016 19.307234
    ## 2959  19.668018 18.204119
    ## 2960  28.456751 19.364039
    ## 2961  16.838165 16.966002
    ## 2962  14.374223 12.802651
    ## 2963  23.230766 18.354805
    ## 2964  21.038566 19.997167
    ## 2965  22.384273 19.772597
    ## 2966  18.268406 18.258032
    ## 2967  24.496578 19.200595
    ## 2968  16.939588 16.180315
    ## 2969  24.800614 19.528849
    ## 2970  25.678247 19.787741
    ## 2971  15.262895 13.844694
    ## 2972  18.352977 18.119283
    ## 2973  20.663382 19.672778
    ## 2974  20.956727 18.200960
    ## 2975  16.825279 15.363665
    ## 2976  22.101537 19.109326
    ## 2977  20.098599 19.583525
    ## 2978  17.306285 18.657911
    ## 2979  24.262876 18.803911
    ## 2980  20.131988 19.208770
    ## 2981  18.229957 19.324146
    ## 2982  19.079631 19.725503
    ## 2983  20.478653 18.950822
    ## 2984  19.280378 19.083584
    ## 2985  22.359118 17.122309
    ## 2986  24.863832 18.567722
    ## 2987  22.357145 18.597896
    ## 2988  20.676044 17.214108
    ## 2989  24.176153 19.471990
    ## 2990  20.774425 19.626845
    ## 2991  21.628581 18.433467
    ## 2992  23.904917 19.448058
    ## 2993  20.905069 18.833182
    ## 2994  16.475347 14.744694
    ## 2995  24.452401 19.978337
    ## 2996  23.229312 19.033851
    ## 2997  25.091710 19.365142
    ## 2998  20.809832 19.858905
    ## 2999  16.713031 19.289368
    ## 3000  14.399292 16.335520
    ## 3001  21.691661 17.247218
    ## 3002  17.318307 18.207167
    ## 3003  14.512507 19.194098
    ## 3004  14.102084 15.052669
    ## 3005  17.360575 18.379792
    ## 3006  15.462043 19.393032
    ## 3007  22.583785 18.684717
    ## 3008  16.115447 14.700608
    ## 3009  12.771184 19.671932
    ## 3010  19.172343 18.985398
    ## 3011  22.326462 19.755966
    ## 3012  26.444536 18.942938
    ## 3013  19.977639 17.601347
    ## 3014  22.401251 17.832700
    ## 3015  22.570909 19.659784
    ## 3016  16.272837 15.321537
    ## 3017  13.331421 13.429183
    ## 3018  25.318551 19.398106
    ## 3019  19.775062 19.603737
    ## 3020  21.788768 19.987364
    ## 3021  18.885246 18.093415
    ## 3022  25.276302 19.212827
    ## 3023  14.146144 15.738931
    ## 3024  25.875550 19.806158
    ## 3025  24.311259 18.171322
    ## 3026  17.101534 19.015742
    ## 3027  18.512660 18.446097
    ## 3028  23.377530 18.435722
    ## 3029  15.471174 16.894739
    ## 3030  20.065282 16.168010
    ## 3031  16.827994 16.511286
    ## 3032  23.102661 18.881184
    ## 3033  18.229716 17.373921
    ## 3034  26.441461 19.504309
    ## 3035  23.961834 19.731032
    ## 3036  17.533874 12.964149
    ## 3037  15.228247 14.264776
    ## 3038  19.772475 18.146234
    ## 3039  14.877279 19.415602
    ## 3040  21.421611 19.813756
    ## 3041  15.642461 14.979196
    ## 3042  21.320175 18.988739
    ## 3043  12.585772 19.436148
    ## 3044  23.371956 19.961984
    ## 3045  23.612384 18.900605
    ## 3046  15.869992 18.074347
    ## 3047  20.748045 19.615250
    ## 3048  25.677646 19.716331
    ## 3049  19.042583 18.535858
    ## 3050  19.838763 16.788288
    ## 3051  13.572503 12.744043
    ## 3052  21.621365 19.702711
    ## 3053  17.857080 17.504478
    ## 3054  20.692135 19.708312
    ## 3055  22.146017 17.145579
    ## 3056  20.996920 16.996233
    ## 3057  20.713556 19.576935
    ## 3058  19.357071 17.452277
    ## 3059  20.259853 19.805661
    ## 3060  12.002065 16.026939
    ## 3061  19.311965 18.961502
    ## 3062  21.341206 19.252723
    ## 3063  15.769354 17.689340
    ## 3064  17.002128 19.004467
    ## 3065  21.539078 18.036013
    ## 3066  23.431463 18.989737
    ## 3067  19.742386 19.054385
    ## 3068  21.888129 18.959267
    ## 3069  19.938257 17.306011
    ## 3070  19.975217 19.363288
    ## 3071  16.825348 14.681943
    ## 3072  18.960066 18.491644
    ## 3073  22.098204 16.908124
    ## 3074  22.913312 19.258205
    ## 3075  18.411063 16.100890
    ## 3076  20.999201 18.236867
    ## 3077  18.627059 16.545928
    ## 3078  15.016844 14.863280
    ## 3079  24.715818 19.840142
    ## 3080  23.114786 19.520955
    ## 3081  19.712422 18.815318
    ## 3082  18.478384 17.650829
    ## 3083  20.819227 19.917153
    ## 3084  14.166995 11.879712
    ## 3085  22.655262 18.249788
    ## 3086  18.850917 15.729144
    ## 3087  24.197386 19.574238
    ## 3088  23.757351 18.398257
    ## 3089  18.358195 19.546693
    ## 3090  12.533149 18.887183
    ## 3091  14.946791 17.855407
    ## 3092  18.426868 18.593333
    ## 3093  18.981953 18.466185
    ## 3094  20.739394 18.965504
    ## 3095  15.331121 13.704867
    ## 3096  18.626397 16.171373
    ## 3097  20.230433 18.102701
    ## 3098  14.158016 15.253069
    ## 3099  19.734170 18.145367
    ## 3100  24.693740 18.371825
    ## 3101  22.764527 19.493980
    ## 3102  23.927313 19.314618
    ## 3103  27.402707 18.991267
    ## 3104  18.997828 19.760655
    ## 3105  15.933616 17.956845
    ## 3106  19.735594 17.601200
    ## 3107  15.028491 17.099952
    ## 3108  18.907633 18.486501
    ## 3109  17.637358 16.978528
    ## 3110  18.412233 16.696446
    ## 3111  20.465521 19.495235
    ## 3112  15.672213 17.484755
    ## 3113  19.928712 19.575494
    ## 3114  20.834262 18.851714
    ## 3115  20.622564 18.844043
    ## 3116  23.070128 19.972285
    ## 3117  25.676836 19.165641
    ## 3118  16.126131 18.981459
    ## 3119  15.745910 16.634303
    ## 3120  23.508840 19.745265
    ## 3121  21.667776 19.459451
    ## 3122  23.283712 18.634991
    ## 3123  21.267996 18.665253
    ## 3124  20.062712 17.185850
    ## 3125  18.862662 19.597429
    ## 3126  23.934214 19.511860
    ## 3127  16.786745 18.353266
    ## 3128  22.009537 19.927932
    ## 3129  19.088996 18.754489
    ## 3130  21.357151 19.471407
    ## 3131  24.460295 19.671222
    ## 3132  11.485263 17.527600
    ## 3133  20.270782 19.827652
    ## 3134  26.395997 19.791012
    ## 3135  19.838060 18.669238
    ## 3136  21.437339 17.374113
    ## 3137  18.656063 18.593527
    ## 3138  20.791724 18.689479
    ## 3139  16.972966 16.738519
    ## 3140  17.264037 19.025196
    ## 3141  20.400345 18.344158
    ## 3142  26.052924 18.799581
    ## 3143  15.084720 17.615619
    ## 3144  16.416491 18.107670
    ## 3145  21.233284 19.842672
    ## 3146  15.257805 18.253520
    ## 3147  22.279160 17.916382
    ## 3148  15.370708 17.977435
    ## 3149  23.198989 19.562384
    ## 3150  19.339232 13.818331
    ## 3151  12.129524 10.909283
    ## 3152  19.737914 19.890908
    ## 3153  24.653037 19.217949
    ## 3154  15.865196 18.863433
    ## 3155  25.591369 18.131332
    ## 3156  26.053428 19.751520
    ## 3157  21.226868 18.093993
    ## 3158  24.206963 19.324112
    ## 3159  18.441599 19.222944
    ## 3160  16.610670 19.418685
    ## 3161  22.297690 18.220548
    ## 3162  19.889621 19.718959
    ## 3163  21.892573 18.100097
    ## 3164  19.255750 18.620735
    ## 3165  22.681406 18.723962
    ## 3166  16.475409 19.518226
    ## 3167  19.421321 15.109163
    ## 3168  23.435981 18.463032
    ## 3169  15.858431 17.141497
    ## 3170  21.555753 19.437849
    ## 3171  21.071123 19.965143
    ## 3172  12.173384 18.139698
    ## 3173  25.551258 19.518186
    ## 3174  26.252972 19.860574
    ## 3175  19.142629 17.738740
    ## 3176  20.755915 17.027508
    ## 3177  16.985203 18.399386
    ## 3178  18.636154 18.367076
    ## 3179  18.310821 16.160984
    ## 3180  19.423566 19.164512
    ## 3181  20.628379 18.079820
    ## 3182  25.312488 19.480311
    ## 3183  13.989586 12.998725
    ## 3184  20.883855 19.431931
    ## 3185  22.406741 19.947603
    ## 3186  22.080393 19.959810
    ## 3187  18.446929 19.143863
    ## 3188  25.782242 19.963651
    ## 3189  20.361999 19.627612
    ## 3190  19.511510 19.732611
    ## 3191  21.980497 19.441619
    ## 3192  22.152276 19.451002
    ## 3193  21.693402 18.837469
    ## 3194  14.183908 17.034202
    ## 3195  19.166435 17.859040
    ## 3196  17.080003 17.788192
    ## 3197  21.025626 17.773177
    ## 3198  22.110471 16.550199
    ## 3199  14.442716 16.945249
    ## 3200  22.526725 19.517035
    ## 3201  15.751578 17.002573
    ## 3202  19.994656 18.416641
    ## 3203  20.131194 19.614237
    ## 3204  22.233092 19.509729
    ## 3205  18.079610 17.381348
    ## 3206  22.459287 19.587126
    ## 3207  16.536904 18.846378
    ## 3208  23.520890 19.302137
    ## 3209  19.711397 15.518751
    ## 3210  12.992640 18.130040
    ## 3211  14.286424 19.611368
    ## 3212  21.880942 19.326259
    ## 3213  16.665019 18.487984
    ## 3214  20.407106 18.322456
    ## 3215  22.682293 19.501938
    ## 3216  21.822419 19.985081
    ## 3217  19.454068 17.634512
    ## 3218  18.785356 19.702951
    ## 3219  20.416095 18.340937
    ## 3220  15.919350 18.299414
    ## 3221  20.896165 17.673377
    ## 3222  17.852217 16.715439
    ## 3223  17.901295 18.469516
    ## 3224  19.361862 12.781819
    ## 3225  23.712380 18.492023
    ## 3226  24.095430 18.618266
    ## 3227  16.239196 19.814803
    ## 3228  23.681610 19.434141
    ## 3229  16.550960 19.311281
    ## 3230   9.541640 13.417174
    ## 3231  15.523382 16.947671
    ## 3232  24.900552 17.380283
    ## 3233  17.645757 16.383118
    ## 3234  16.480396 19.133406
    ## 3235  23.663406 18.340503
    ## 3236  19.051475 19.375905
    ## 3237  24.121912 19.746755
    ## 3238  28.260126 19.606934
    ## 3239  14.975725 17.146324
    ## 3240  21.961109 19.764920
    ## 3241  22.501676 19.268245
    ## 3242  17.056804 14.814980
    ## 3243  17.184436 15.543304
    ## 3244  18.142184 19.850300
    ## 3245  21.405273 18.054164
    ## 3246  15.317984 17.873383
    ## 3247  18.198282 19.265288
    ## 3248  20.318031 19.023351
    ## 3249  16.865590 16.561438
    ## 3250  14.632736 13.771784
    ## 3251  24.642736 19.568782
    ## 3252  21.530705 18.887354
    ## 3253  19.277316 19.617364
    ## 3254  24.547333 18.643943
    ## 3255  17.971764 16.802679
    ## 3256  22.217012 19.268960
    ## 3257  22.078341 17.506953
    ## 3258  18.760364 19.391860
    ## 3259  18.611650 18.203894
    ## 3260  11.781449 14.069080
    ## 3261  25.362036 19.637008
    ## 3262  21.917130 17.766530
    ## 3263  22.797539 18.853680
    ## 3264  22.726955 18.170353
    ## 3265  20.402641 14.837962
    ## 3266  21.147997 19.299861
    ## 3267  14.004383 19.132691
    ## 3268  24.783935 19.101545
    ## 3269  19.877949 18.860269
    ## 3270  18.778856 19.095112
    ## 3271  15.325175 18.524103
    ## 3272  17.552496 19.350838
    ## 3273  20.023466 19.966149
    ## 3274  15.131898 18.570048
    ## 3275  20.177876 19.821351
    ## 3276  23.672507 17.204464
    ## 3277  17.633285 18.889613
    ## 3278  20.598668 19.216145
    ## 3279  19.551494 17.739205
    ## 3280  24.785457 19.202406
    ## 3281  22.205569 17.524688
    ## 3282  21.401540 18.252169
    ## 3283  22.697484 18.909786
    ## 3284  17.935584 19.067228
    ## 3285  16.472929 19.070740
    ## 3286  19.882975 19.879491
    ## 3287  25.957598 19.014419
    ## 3288  23.668440 18.420213
    ## 3289  22.030896 17.659745
    ## 3290  15.718757 16.916823
    ## 3291  18.318901 17.451581
    ## 3292  17.383340 19.251184
    ## 3293  20.080932 19.346821
    ## 3294  11.656398 16.031260
    ## 3295  17.030373 18.542745
    ## 3296  15.547500 18.712468
    ## 3297  25.944658 16.137081
    ## 3298  27.191578 18.433166
    ## 3299  24.969829 19.866039
    ## 3300  19.175286 19.163251
    ## 3301  18.098915 18.914378
    ## 3302  24.011046 18.634435
    ## 3303  20.781432 19.125061
    ## 3304  22.167463 19.105565
    ## 3305  20.849467 18.012815
    ## 3306  17.824508 18.834121
    ## 3307  14.176342 18.623676
    ## 3308  21.098463 19.506701
    ## 3309  18.912015 19.123208
    ## 3310  16.990039 19.816952
    ## 3311  14.735445 16.576649
    ## 3312  18.227074 18.020815
    ## 3313  25.820398 19.360863
    ## 3314  15.864725 15.219925
    ## 3315  19.864999 19.007247
    ## 3316  14.462899 14.366302
    ## 3317  17.466142 17.666954
    ## 3318  18.544087 19.706749
    ## 3319  18.856935 19.730680
    ## 3320  25.387998 19.814538
    ## 3321  16.016716 16.436855
    ## 3322  16.938155 14.249582
    ## 3323  18.499916 18.589258
    ## 3324  22.241326 18.538502
    ## 3325  25.148153 19.088511
    ## 3326  19.217095 16.975731
    ## 3327  19.374913 16.172431
    ## 3328  14.154308 15.718734
    ## 3329  23.344525 19.063729
    ## 3330  18.426264 19.693954
    ## 3331  22.004873 17.680819
    ## 3332  22.023508 18.614053
    ## 3333  24.080784 19.145905
    ## 3334  22.807251 17.423088
    ## 3335  18.472543 14.082311
    ## 3336  19.250814 19.883254
    ## 3337  16.113103 15.443300
    ## 3338  20.362705 19.764887
    ## 3339  22.443127 19.529822
    ## 3340  15.450881 15.275862
    ## 3341  20.623867 19.623444
    ## 3342  19.440791 18.408665
    ## 3343  23.519110 17.748017
    ## 3344  21.461210 19.406605
    ## 3345  20.351393 19.907867
    ## 3346  19.346311 19.790056
    ## 3347  26.317812 18.759179
    ## 3348  13.546493 12.597415
    ## 3349  13.290728 16.460635
    ## 3350  25.949365 18.951117
    ## 3351  24.484542 18.849862
    ## 3352  17.519937 15.760816
    ## 3353  18.633236 19.701086
    ## 3354  18.715490 19.263853
    ## 3355  14.725193 17.397437
    ## 3356  14.855567 12.927997
    ## 3357  19.161284 19.466695
    ## 3358  21.160771 19.977146
    ## 3359  19.287291 19.232323
    ## 3360  19.507382 17.371752
    ## 3361  15.042632 15.851136
    ## 3362  18.209254 16.500422
    ## 3363  24.628181 19.792388
    ## 3364  13.473834 11.503802
    ## 3365  11.532669 19.771149
    ## 3366  27.582621 16.853554
    ## 3367  17.285693 19.632656
    ## 3368  24.020403 19.052685
    ## 3369  22.422778 17.782194
    ## 3370  20.136588 18.940013
    ## 3371  24.426018 18.522839
    ## 3372  20.357838 17.093196
    ## 3373  19.134918 19.737937
    ## 3374  19.351099 19.909925
    ## 3375  24.961283 19.957675
    ## 3376  22.104137 18.334603
    ## 3377  11.580322  9.317853
    ## 3378  20.766426 19.345999
    ## 3379  17.062648 18.602421
    ## 3380  18.499775 16.027323
    ## 3381  19.311421 19.571089
    ## 3382  18.839473 18.623225
    ## 3383  21.343688 19.427194
    ## 3384  12.864311 15.277437
    ## 3385  18.205406 16.502562
    ## 3386  17.414604 18.587283
    ## 3387  17.766351 18.544164
    ## 3388  19.071649 17.792500
    ## 3389  22.529585 19.456618
    ## 3390  14.623114 15.996483
    ## 3391  24.032767 18.434863
    ## 3392  18.957942 16.471347
    ## 3393  22.571831 19.617624
    ## 3394  21.637609 17.290905
    ## 3395  22.883043 18.899579
    ## 3396  16.828044 13.236110
    ## 3397  19.519807 16.663854
    ## 3398  16.115582 19.401498
    ## 3399  19.994699 17.059008
    ## 3400  19.789092 19.462209
    ## 3401  14.803804 12.958001
    ## 3402  22.147881 15.886606
    ## 3403  23.670481 19.575276
    ## 3404  21.768703 19.255131
    ## 3405  12.255482 19.132865
    ## 3406  18.266585 19.636694
    ## 3407  17.552295 17.406229
    ## 3408  21.670424 18.979140
    ## 3409  26.399727 19.281556
    ## 3410  21.487737 18.565443
    ## 3411  20.998014 17.849510
    ## 3412  10.954599 15.805015
    ## 3413  19.636779 15.515529
    ## 3414  22.823942 18.371092
    ## 3415  23.753314 19.369626
    ## 3416  24.266785 19.717544
    ## 3417  18.023310 18.352881
    ## 3418  22.972709 19.332265
    ## 3419  19.017580 19.697028
    ## 3420  15.889096 16.285676
    ## 3421  17.343066 17.987633
    ## 3422  18.329637 17.449194
    ## 3423  22.653846 19.507753
    ## 3424  21.148436 18.160831
    ## 3425  18.710101 18.669556
    ## 3426  16.030256 15.907358
    ## 3427  17.707607 12.679757
    ## 3428  17.670279 19.041757
    ## 3429  20.189247 19.011931
    ## 3430  18.555776 18.539382
    ## 3431  20.944388 18.201572
    ## 3432  22.869754 19.220872
    ## 3433  18.622923 19.658473
    ## 3434  21.876858 19.304256
    ## 3435  19.535197 19.620631
    ## 3436  19.311681 19.806300
    ## 3437  20.880962 19.659528
    ## 3438  19.771481 18.152281
    ## 3439  15.526605 18.406636
    ## 3440  18.309822 19.882750
    ## 3441  22.771443 19.647996
    ## 3442  10.646395 18.012366
    ## 3443  20.718344 19.116719
    ## 3444  21.450878 19.110056
    ## 3445  23.174570 17.403948
    ## 3446  18.688973 14.867505
    ## 3447  22.682351 18.929616
    ## 3448  23.642123 19.271634
    ## 3449  19.983473 19.265704
    ## 3450  18.718083 18.049423
    ## 3451  22.904139 19.221526
    ## 3452  19.340317 17.239790
    ## 3453  25.311100 19.619132
    ## 3454  15.425341 17.343864
    ## 3455  24.700839 19.445859
    ## 3456  23.082665 16.581684
    ## 3457  20.410193 19.798044
    ## 3458  19.835300 17.051604
    ## 3459  15.165248 15.766812
    ## 3460  26.731668 19.277401
    ## 3461  26.766322 19.548248
    ## 3462  13.234222 17.258741
    ## 3463  25.246268 19.932345
    ## 3464  24.174252 19.277569
    ## 3465  18.873590 15.951114
    ## 3466  24.908401 19.195751
    ## 3467  17.815915 16.946369
    ## 3468  18.715966 17.435360
    ## 3469  11.493683 13.509440
    ## 3470  14.960528 19.187003
    ## 3471  22.684984 18.751270
    ## 3472  19.518344 18.504920
    ## 3473  19.790171 18.529666
    ## 3474  24.193873 18.741982
    ## 3475  15.829527 18.105019
    ## 3476  22.324784 19.158043
    ## 3477  22.796756 19.782228
    ## 3478  18.912263 19.797377
    ## 3479  24.462617 17.444622
    ## 3480  19.978644 17.743500
    ## 3481  19.440831 16.085581
    ## 3482  21.036612 19.840571
    ## 3483  21.492257 18.159819
    ## 3484  26.310761 19.977382
    ## 3485  19.550514 16.512880
    ## 3486  23.040924 19.697497
    ## 3487  21.438728 18.479586
    ## 3488  23.364336 18.877442
    ## 3489  24.372914 19.378138
    ## 3490  23.042700 18.569649
    ## 3491  31.026711 19.518883
    ## 3492  18.779883 15.738326
    ## 3493  17.521825 15.730595
    ## 3494  22.900282 19.294755
    ## 3495  18.437179 16.143790
    ## 3496  18.351515 19.935420
    ## 3497  18.280926 19.314513
    ## 3498  17.271044 17.347984
    ## 3499  22.042477 18.886321
    ## 3500  15.271558 18.814245
    ## 3501  22.544456 17.329538
    ## 3502  21.710365 18.791346
    ## 3503  27.016323 19.872962
    ## 3504  15.000385 18.759902
    ## 3505  23.413824 19.664166
    ## 3506  25.555518 18.282792
    ## 3507  23.522859 17.899542
    ## 3508  20.543418 19.070984
    ## 3509  22.569007 19.647729
    ## 3510  19.092626 19.803529
    ## 3511  17.292939 16.409507
    ## 3512  21.544929 19.418085
    ## 3513  16.007150 17.516817
    ## 3514  12.464291 11.631843
    ## 3515  22.542295 17.025689
    ## 3516  14.328439 19.879112
    ## 3517  21.209148 19.511204
    ## 3518  18.299693 16.414928
    ## 3519  20.696522 19.742342
    ## 3520  21.256691 18.271875
    ## 3521  17.625544 17.390107
    ## 3522  22.078725 18.839155
    ## 3523  18.921096 19.370959
    ## 3524  24.253864 17.830049
    ## 3525  24.324296 17.892156
    ## 3526  20.080852 19.413169
    ## 3527  21.253131 19.360680
    ## 3528  15.329864 18.118155
    ## 3529  22.024904 19.129894
    ## 3530  17.710459 16.732054
    ## 3531  11.802495 15.800964
    ## 3532  18.654259 19.997881
    ## 3533  19.607664 17.803760
    ## 3534  15.624551 16.905392
    ## 3535  17.234633 17.852031
    ## 3536  14.970014 15.432072
    ## 3537  25.084858 18.686093
    ## 3538  17.791462 18.875625
    ## 3539  23.068769 19.547729
    ## 3540  15.214327 17.952237
    ## 3541  21.235695 17.757579
    ## 3542  24.947829 18.910907
    ## 3543  23.875735 18.914594
    ## 3544  24.913890 19.924507
    ## 3545  22.754358 18.230532
    ## 3546  25.627685 16.890433
    ## 3547  22.192753 18.240255
    ## 3548  19.448095 17.045043
    ## 3549  21.470921 16.678570
    ## 3550  21.156930 19.029872
    ## 3551  29.705170 19.842113
    ## 3552  19.658487 19.159001
    ## 3553  13.408879 19.384648
    ## 3554  14.629848 15.444233
    ## 3555  19.972357 19.123394
    ## 3556  22.995888 17.518594
    ## 3557  21.124265 17.356558
    ## 3558  16.415795 15.968452
    ## 3559  19.070916 17.263870
    ## 3560  28.737501 19.305751
    ## 3561  14.848498 15.813825
    ## 3562  15.902156 12.540111
    ## 3563  24.779266 19.190024
    ## 3564  18.875559 16.999889
    ## 3565  23.283305 19.780463
    ## 3566  22.316604 17.673348
    ## 3567  18.428185 19.034257
    ## 3568  26.427872 19.623758
    ## 3569  15.640529 15.035354
    ## 3570  17.138203 17.770727
    ## 3571  27.302271 19.911222
    ## 3572  14.750857 13.848430
    ## 3573  25.039663 18.069029
    ## 3574  21.289781 19.614972
    ## 3575  16.862377 17.974106
    ## 3576  24.699661 19.325998
    ## 3577  22.981586 19.791951
    ## 3578  15.039337 19.700062
    ## 3579  20.570331 19.940453
    ## 3580  14.718270 13.505503
    ## 3581  18.625626 17.752627
    ## 3582  17.332448 14.921487
    ## 3583  17.152637 17.990809
    ## 3584  16.759420 16.554562
    ## 3585  15.150876 19.036365
    ## 3586  26.259968 19.442951
    ## 3587  19.309191 15.656820
    ## 3588  20.517938 18.937051
    ## 3589  22.610869 19.751634
    ## 3590  19.457557 19.250158
    ## 3591  17.173366 18.924794
    ## 3592  17.451569 16.858101
    ## 3593  24.692030 19.740827
    ## 3594  25.752699 19.961856
    ## 3595  21.668940 18.277893
    ## 3596  21.974890 19.034058
    ## 3597  22.069138 15.298145
    ## 3598  24.052062 18.098867
    ## 3599  22.350208 16.462418
    ## 3600  21.068139 18.207345
    ## 3601  20.434330 17.191839
    ## 3602  20.011146 18.435994
    ## 3603  19.055363 19.898003
    ## 3604  20.368572 19.794929
    ## 3605  15.419799 12.948735
    ## 3606  16.841518 15.978486
    ## 3607  22.207338 19.436803
    ## 3608  22.195239 19.036917
    ## 3609  24.767755 19.404216
    ## 3610  22.213244 19.597636
    ## 3611  21.879714 18.764171
    ## 3612  24.451027 18.558666
    ## 3613  25.664832 18.071204
    ## 3614  15.742586 18.385231
    ## 3615  20.012915 18.538006
    ## 3616  21.256215 19.657311
    ## 3617  18.923631 18.127640
    ## 3618  20.616876 19.134519
    ## 3619  21.247506 16.870798
    ## 3620  19.375663 17.888075
    ## 3621  16.268269 18.391163
    ## 3622  16.968733 16.030636
    ## 3623  13.780192 15.747479
    ## 3624  24.961070 19.801439
    ## 3625  16.834190 15.738480
    ## 3626  19.399491 19.639872
    ## 3627  26.908881 19.461158
    ## 3628  18.945596 19.493942
    ## 3629  18.007409 17.599809
    ## 3630  22.441934 19.806572
    ## 3631  11.251348 10.255480
    ## 3632  19.283195 19.453911
    ## 3633  17.586029 18.954607
    ## 3634  22.667891 18.213878
    ## 3635  13.033300 14.906691
    ## 3636  20.207324 16.408729
    ## 3637  15.764155 15.956626
    ## 3638  20.870799 18.947895
    ## 3639  14.506467 15.544446
    ## 3640  17.614909 19.811572
    ## 3641  17.942198 18.160480
    ## 3642  21.408877 15.863388
    ## 3643  22.648617 18.168698
    ## 3644  21.145722 19.914083
    ## 3645  21.928694 18.261959
    ## 3646  20.501225 17.411138
    ## 3647  14.668580 15.117262
    ## 3648  19.253965 18.990877
    ## 3649  21.472102 19.392757
    ## 3650  15.959483 16.437682
    ## 3651  23.443419 19.733410
    ## 3652  27.065447 19.206575
    ## 3653  16.858701 17.938378
    ## 3654  27.214120 19.918444
    ## 3655  20.246061 17.820624
    ## 3656  23.207562 18.731516
    ## 3657  24.422341 19.602124
    ## 3658  18.375083 19.690119
    ## 3659  20.854493 19.324208
    ## 3660  21.743635 19.726969
    ## 3661  22.975579 19.883651
    ## 3662  26.832445 19.715115
    ## 3663  24.122753 19.862829
    ## 3664  17.118703 15.256969
    ## 3665  30.088680 19.427883
    ## 3666  19.166168 17.121134
    ## 3667  15.896306 19.372552
    ## 3668  16.302782 14.135491
    ## 3669  19.703277 19.495095
    ## 3670  22.433999 19.402892
    ## 3671  17.758450 16.368061
    ## 3672  24.968717 19.726400
    ## 3673  21.560023 19.533216
    ## 3674  18.270368 19.418291
    ## 3675  15.902400 19.920397
    ## 3676  24.059782 19.314902
    ## 3677  24.647274 19.979762
    ## 3678  23.719783 19.240143
    ## 3679  26.379222 18.040544
    ## 3680  19.074808 17.090220
    ## 3681  22.597847 19.412139
    ## 3682  25.974542 19.831739
    ## 3683  22.779602 19.673314
    ## 3684  17.823971 19.070709
    ## 3685  22.951340 19.132191
    ## 3686  19.284145 19.707941
    ## 3687  21.315685 18.293256
    ## 3688  17.951208 16.944124
    ## 3689  16.524348 14.256142
    ## 3690  16.214564 17.401459
    ## 3691  20.034469 15.888051
    ## 3692  19.252228 18.883946
    ## 3693  21.787892 18.710969
    ## 3694  22.901341 18.343974
    ## 3695  22.935170 18.580767
    ## 3696  23.270154 17.839448
    ## 3697  24.068272 19.187285
    ## 3698  22.036051 18.432421
    ## 3699  26.113773 19.981471
    ## 3700  22.355602 19.205892
    ## 3701  18.696761 19.340846
    ## 3702  25.996588 18.383486
    ## 3703  17.227652 19.195979
    ## 3704  21.727586 16.883013
    ## 3705  23.882117 18.935079
    ## 3706  12.863896 18.214271
    ## 3707  14.710917 16.409661
    ## 3708  21.163612 19.227039
    ## 3709  20.586181 16.853440
    ## 3710  17.060237 18.494446
    ## 3711  25.551258 19.278863
    ## 3712  18.164570 15.814427
    ## 3713  25.071813 17.309845
    ## 3714  22.627098 17.370252
    ## 3715  16.070715 17.139574
    ## 3716  14.780014 18.456790
    ## 3717  16.341023 18.671516
    ## 3718  23.457312 16.560994
    ## 3719  24.432736 19.579832
    ## 3720  17.013361 16.647784
    ## 3721  19.355436 17.055202
    ## 3722  19.037040 14.848776
    ## 3723  20.560567 16.979366
    ## 3724  20.639612 19.744140
    ## 3725  23.717914 19.630312
    ## 3726  22.343156 17.527764
    ## 3727  15.418052 17.261803
    ## 3728  19.665567 17.711894
    ## 3729  11.680382 14.638762
    ## 3730  16.925053 19.092390
    ## 3731  24.098558 18.151340
    ## 3732  16.611927 16.971340
    ## 3733  21.612460 18.283511
    ## 3734  25.506647 19.772932
    ## 3735  21.879565 14.866130
    ## 3736  15.289350 16.888269
    ## 3737  19.185551 18.808140
    ## 3738  22.906175 19.762109
    ## 3739  22.313211 18.330982
    ## 3740  17.601640 18.923283
    ## 3741  19.206787 18.960430
    ## 3742  23.040936 18.733935
    ## 3743  21.974819 19.683663
    ## 3744  10.749773  8.942427
    ## 3745  19.973880 18.498210
    ## 3746  23.929772 19.053432
    ## 3747  20.842639 15.083586
    ## 3748  20.576318 17.437784
    ## 3749  16.068137 19.293992
    ## 3750  19.488656 17.782801
    ## 3751  19.149244 16.376174
    ## 3752  23.833829 18.312271
    ## 3753  17.237576 16.097189
    ## 3754  21.995269 17.987502
    ## 3755  20.564396 18.012924
    ## 3756  21.667093 18.286353
    ## 3757  18.802046 18.229298
    ## 3758  14.611994 18.585067
    ## 3759  11.240175 16.565318
    ## 3760  23.238569 19.090310
    ## 3761  17.004637 17.210159
    ## 3762  18.834601 15.722472
    ## 3763  24.349325 19.712453
    ## 3764  14.972746 18.324369
    ## 3765  15.192814 16.740303
    ## 3766  17.794445 19.005833
    ## 3767  21.934817 18.894557
    ## 3768  18.972899 19.412926
    ## 3769  21.079629 18.895804
    ## 3770  17.968284 18.269996
    ## 3771  13.970300 13.813879
    ## 3772  25.210723 19.572942
    ## 3773  19.168473 19.421487
    ## 3774  21.118752 18.903670
    ## 3775  17.011000 18.016406
    ## 3776  25.723969 19.965485
    ## 3777  23.406130 17.951173
    ## 3778  22.156686 19.809006
    ## 3779  16.060220 17.534444
    ## 3780  20.785226 19.713634
    ## 3781  12.332776 12.002188
    ## 3782  15.271099 15.204185
    ## 3783  25.417882 19.156213
    ## 3784  19.307747 19.665939
    ## 3785  21.439830 18.571447
    ## 3786  20.120088 19.597945
    ## 3787  11.397267 12.628886
    ## 3788  17.964455 19.420706
    ## 3789  24.725500 19.995045
    ## 3790  21.992848 19.624836
    ## 3791  19.989578 19.399315
    ## 3792  17.790101 17.038427
    ## 3793  23.280634 19.525961
    ## 3794  21.193608 18.936221
    ## 3795  26.181696 19.236561
    ## 3796  16.834340 16.471848
    ## 3797  23.300071 17.254736
    ## 3798  21.144189 19.376181
    ## 3799  23.023691 19.677501
    ## 3800  18.278641 18.879174
    ## 3801  20.033034 19.461090
    ## 3802  20.205509 16.298621
    ## 3803  16.073696 17.389876
    ## 3804  23.517971 19.935056
    ## 3805  21.282928 19.356003
    ## 3806  19.047532 17.551507
    ## 3807  23.071375 18.059386
    ## 3808  15.282985 19.054083
    ## 3809  25.125593 17.896078
    ## 3810  19.642573 16.864993
    ## 3811  18.407488 16.303421
    ## 3812  20.904339 17.929792
    ## 3813  20.594757 18.756774
    ## 3814  17.179287 17.637803
    ## 3815  19.271143 19.290476
    ## 3816  19.339502 18.854067
    ## 3817  24.440828 19.447314
    ## 3818  18.228706 17.642371
    ## 3819  25.660091 19.595555
    ## 3820  20.146993 19.961359
    ## 3821  18.389356 19.192551
    ## 3822  20.882262 17.845532
    ## 3823  23.400152 19.757890
    ## 3824  20.942330 19.395891
    ## 3825  18.571104 17.641460
    ## 3826  18.155583 17.299630
    ## 3827  13.172950 19.511047
    ## 3828  19.961213 18.466921
    ## 3829  22.877259 16.188819
    ## 3830  25.192492 19.639229
    ## 3831  17.122300 17.872669
    ## 3832  16.372503 14.646841
    ## 3833  19.037926 17.788797
    ## 3834  23.446306 18.037547
    ## 3835  23.306358 19.939507
    ## 3836  22.441559 18.790154
    ## 3837  18.437017 18.736999
    ## 3838  21.053913 18.965598
    ## 3839  25.988751 19.303350
    ## 3840  23.528472 17.471691
    ## 3841  14.697662 18.617880
    ## 3842  18.521435 19.066635
    ## 3843  21.048978 18.168847
    ## 3844  19.418970 19.867002
    ## 3845  25.717782 16.882093
    ## 3846  23.807699 18.381971
    ## 3847  20.609440 19.281912
    ## 3848  19.180308 18.994419
    ## 3849  20.470352 18.180092
    ## 3850  26.796038 19.740821
    ## 3851  22.140186 17.087681
    ## 3852  20.858063 15.900802
    ## 3853  19.140996 18.762739
    ## 3854  18.907839 19.090811
    ## 3855  17.264965 19.604629
    ## 3856  17.083965 19.785247
    ## 3857  20.000984 18.873602
    ## 3858  26.840029 19.428911
    ## 3859  23.157595 18.947085
    ## 3860  16.935601 18.983204
    ## 3861  21.214604 19.341618
    ## 3862  16.349773 18.763272
    ## 3863  18.655695 17.932372
    ## 3864  20.907268 18.616879
    ## 3865  11.966677  9.561956
    ## 3866  24.698850 19.472384
    ## 3867  18.479220 18.549630
    ## 3868  19.276036 18.940052
    ## 3869  17.747194 19.533605
    ## 3870  18.642183 19.723649
    ## 3871  18.743197 15.917647
    ## 3872  15.435710 18.278561
    ## 3873  30.571773 19.268540
    ## 3874  23.711003 19.735131
    ## 3875  23.169092 16.573243
    ## 3876  15.442656 17.824283
    ## 3877  19.893258 18.978727
    ## 3878  20.209186 19.345252
    ## 3879  19.761955 18.080681
    ## 3880  20.255912 17.603706
    ## 3881  22.664229 19.813634
    ## 3882  22.176674 14.674928
    ## 3883  23.779958 18.927908
    ## 3884  16.649099 17.066259
    ## 3885  23.760842 18.609704
    ## 3886  25.914346 19.509861
    ## 3887  18.311723 18.770088
    ## 3888  23.028127 19.112223
    ## 3889  16.532073 17.809877
    ## 3890  18.701636 18.282298
    ## 3891  19.013485 19.386668
    ## 3892  21.422146 18.397023
    ## 3893  20.256866 18.077532
    ## 3894  24.933821 17.316765
    ## 3895  21.764146 19.100396
    ## 3896  18.504642 14.422510
    ## 3897  19.993169 18.046995
    ## 3898  20.423646 17.396196
    ## 3899  19.141386 17.728365
    ## 3900  20.017799 19.284337
    ## 3901  22.892418 18.892665
    ## 3902  22.408772 19.942953
    ## 3903  17.568938 19.733620
    ## 3904  20.618603 18.774559
    ## 3905  18.909355 18.563871
    ## 3906  16.694397 19.013432
    ## 3907  21.999981 18.808464
    ## 3908  20.046047 17.164917
    ## 3909  18.576989 16.994132
    ## 3910  23.131613 19.301133
    ## 3911  23.512553 19.586917
    ## 3912  26.003762 19.755943
    ## 3913  14.480509 15.311081
    ## 3914  20.779554 19.389272
    ## 3915  17.953825 18.026853
    ## 3916  24.572786 19.829701
    ## 3917  21.715606 16.880047
    ## 3918  15.781818 17.650232
    ## 3919  16.535765 18.939893
    ## 3920  19.183286 17.826186
    ## 3921  18.951194 18.089187
    ## 3922  20.485638 15.858754
    ## 3923  23.131901 18.214306
    ## 3924  22.575943 18.754951
    ## 3925  18.141802 19.114910
    ## 3926  17.870117 18.422232
    ## 3927  15.136503 18.850923
    ## 3928  24.420310 17.224731
    ## 3929  15.961871 16.862183
    ## 3930  17.826714 18.772966
    ## 3931  29.870454 19.277546
    ## 3932  24.205441 19.344689
    ## 3933  21.678416 17.048444
    ## 3934  25.142240 19.576573
    ## 3935  16.834319 17.659402
    ## 3936  18.510386 15.889467
    ## 3937  15.499172 15.640328
    ## 3938  18.460892 16.465079
    ## 3939  16.430566 17.239096
    ## 3940  18.227966 18.628714
    ## 3941  24.441307 19.886172
    ## 3942  21.353372 18.958412
    ## 3943  14.788030 15.418672
    ## 3944  20.813799 19.222234
    ## 3945  15.789294 16.979682
    ## 3946  19.375858 18.762816
    ## 3947  19.956227 18.399411
    ## 3948  17.108051 18.142151
    ## 3949  16.266518 17.637480
    ## 3950  19.748899 19.871868
    ## 3951  21.215152 18.818191
    ## 3952  30.954354 19.557180
    ## 3953  19.716958 18.624247
    ## 3954  16.582312 19.014965
    ## 3955  20.753063 18.941707
    ## 3956  16.958321 17.524781
    ## 3957  20.614294 16.454875
    ## 3958  18.554811 17.889734
    ## 3959  16.499009 19.997071
    ## 3960  15.915117 18.306344
    ## 3961  19.702254 19.261773
    ## 3962  20.696843 15.653556
    ## 3963  17.757279 18.821065
    ## 3964  24.703987 17.717464
    ## 3965  17.766417 16.604575
    ## 3966  18.022802 18.183943
    ## 3967  21.837241 16.102809
    ## 3968  19.230918 18.557236
    ## 3969  23.055172 18.649623
    ## 3970  23.002312 17.691350
    ## 3971  20.075360 17.524310
    ## 3972  15.647386 15.645423
    ## 3973  17.802912 18.478155
    ## 3974  19.299485 19.373234
    ## 3975  21.724153 19.153530
    ## 3976  20.928817 17.384551
    ## 3977  28.192405 18.402227
    ## 3978  19.879239 19.600988
    ## 3979  17.847072 17.401001
    ## 3980  20.212539 18.571272
    ## 3981  22.022629 19.210812
    ## 3982  12.201098 18.834295
    ## 3983  18.220173 18.840322
    ## 3984  14.304890 19.754573
    ## 3985  18.987877 19.993446
    ## 3986  20.468721 18.125443
    ## 3987  12.386238 13.667480
    ## 3988  19.669318 19.127770
    ## 3989  26.885847 19.604438
    ## 3990  16.364214 13.640965
    ## 3991  18.056749 18.176061
    ## 3992  26.130266 19.560709
    ## 3993  23.942676 19.969366
    ## 3994  19.149322 19.941348
    ## 3995  18.394162 19.698708
    ## 3996  18.036819 15.761331
    ## 3997  22.711756 19.493025
    ## 3998  21.848938 16.553063
    ## 3999  17.680150 17.512545
    ## 4000  21.378866 19.760456
    ## 4001  13.471164 16.822764
    ## 4002  20.362440 18.325814
    ## 4003  23.886761 16.196663
    ## 4004  23.760896 19.745333
    ## 4005  24.356233 17.643061
    ## 4006  20.776961 19.358174
    ## 4007  16.184359 19.761104
    ## 4008  20.103888 16.174286
    ## 4009  23.492234 18.921284
    ## 4010  23.974660 19.947587
    ## 4011  22.079938 19.903538
    ## 4012  19.682681 18.421959
    ## 4013  16.932882 14.844285
    ## 4014  22.901443 19.093219
    ## 4015  27.781323 19.316579
    ## 4016  22.173120 18.740694
    ## 4017  22.953286 17.126863
    ## 4018  24.632659 17.907750
    ## 4019  22.622495 19.467335
    ## 4020  12.324749 15.401833
    ## 4021  21.546959 19.764743
    ## 4022  14.416742 15.946597
    ## 4023  21.437721 19.663486
    ## 4024  18.417812 18.162509
    ## 4025  19.525946 18.496495
    ## 4026  19.112208 18.542461
    ## 4027  13.946948 17.551846
    ## 4028  15.659750 19.652336
    ## 4029  22.171569 18.308094
    ## 4030  19.078883 18.561817
    ## 4031  20.198656 19.272474
    ## 4032  18.091912 19.069138
    ## 4033  14.734646 16.342003
    ## 4034  15.529768 18.334012
    ## 4035  14.958837 13.110582
    ## 4036  17.259412 15.265091
    ## 4037  13.621163 17.005401
    ## 4038  19.976135 19.858544
    ## 4039  12.527553 13.415281
    ## 4040  12.879237 18.288067
    ## 4041  26.103719 18.732197
    ## 4042  21.548013 17.930853
    ## 4043  18.878077 17.512480
    ## 4044  24.864331 19.592165
    ## 4045  23.094350 19.737315
    ## 4046  22.490478 19.800584
    ## 4047  16.437819 18.655715
    ## 4048  19.162446 19.226815
    ## 4049  20.634414 18.964795
    ## 4050  22.834144 19.330723
    ## 4051  20.374974 16.233623
    ## 4052  21.585739 16.875774
    ## 4053  16.169985 19.227428
    ## 4054  16.919061 18.735561
    ## 4055  19.251879 16.174078
    ## 4056  22.380503 19.304888
    ## 4057  19.301252 19.735535
    ## 4058  18.013901 18.796299
    ## 4059  22.005523 18.716658
    ## 4060  17.027471 17.994253
    ## 4061  21.649356 18.665208
    ## 4062  18.435170 16.213577
    ## 4063  15.249982 14.967448
    ## 4064  22.836343 18.190484
    ## 4065  19.736231 18.982362
    ## 4066  13.471300 14.029759
    ## 4067  18.173517 17.671082
    ## 4068  19.216993 18.692896
    ## 4069  18.623031 19.139260
    ## 4070  20.623439 18.953170
    ## 4071  22.074500 19.328491
    ## 4072  23.212130 18.786101
    ## 4073  23.750133 19.786580
    ## 4074  19.674363 16.997051
    ## 4075  26.743700 19.506072
    ## 4076  19.046859 16.036991
    ## 4077  17.464514 15.885183
    ## 4078  14.024207 12.290205
    ## 4079  18.013826 19.211162
    ## 4080  15.663853 15.881837
    ## 4081  24.478164 19.840256
    ## 4082  23.243710 18.913325
    ## 4083  11.160829 12.580289
    ## 4084  21.989418 19.796782
    ## 4085  20.359383 19.670100
    ## 4086  16.078025 18.405708
    ## 4087  17.653707 17.626235
    ## 4088  18.006995 18.386897
    ## 4089  20.745278 19.917331
    ## 4090  14.485620 14.285196
    ## 4091  19.151226 19.093779
    ## 4092  15.417041 16.026183
    ## 4093  24.349761 19.620040
    ## 4094  18.781536 15.481998
    ## 4095  15.587355 14.206480
    ## 4096  14.497682 13.141215
    ## 4097  19.701420 19.430799
    ## 4098  21.582032 17.927792
    ## 4099  16.649932 18.670335
    ## 4100  11.121105 15.461990
    ## 4101  25.803840 19.587652
    ## 4102  27.731623 19.622570
    ## 4103  25.179412 16.777589
    ## 4104  14.167250 17.005184
    ## 4105  24.901697 19.337824
    ## 4106  19.232891 17.612244
    ## 4107  23.290583 18.142906
    ## 4108  10.609027 15.373262
    ## 4109  18.699325 16.491435
    ## 4110  19.666872 18.240267
    ## 4111  28.348295 19.589810
    ## 4112  14.643391 17.742351
    ## 4113  21.149168 17.204794
    ## 4114  18.745839 19.693691
    ## 4115  20.846488 19.283409
    ## 4116  19.902780 19.884678
    ## 4117  15.093845 12.790531
    ## 4118  17.756382 18.714454
    ## 4119  18.782616 19.628455
    ## 4120  18.101984 14.445278
    ## 4121  20.987689 18.687977
    ## 4122  15.049738 18.731865
    ## 4123  16.286688 11.209815
    ## 4124  20.230784 16.923375
    ## 4125  19.537430 15.146987
    ## 4126  15.149048 13.407527
    ## 4127  19.137694 18.144580
    ## 4128  20.604984 19.797527
    ## 4129  17.516096 19.541408
    ## 4130  15.189939 13.732885
    ## 4131  22.847820 19.999458
    ## 4132  23.684797 18.482530
    ## 4133  23.695796 19.379102
    ## 4134  16.462396 19.720581
    ## 4135  20.114922 18.040623
    ## 4136  21.102143 18.976218
    ## 4137  20.131450 19.533301
    ## 4138  22.950793 18.757960
    ## 4139  14.145089 14.912985
    ## 4140  22.984735 19.856920
    ## 4141  19.897154 19.720832
    ## 4142  20.901738 19.713820
    ## 4143  19.249461 19.228748
    ## 4144  21.598966 19.920378
    ## 4145  23.208952 18.198171
    ## 4146  17.648453 17.768928
    ## 4147  27.231613 19.068746
    ## 4148  24.578715 17.398960
    ## 4149  26.375608 19.254294
    ## 4150  20.042582 16.459690
    ## 4151  24.639451 19.739590
    ## 4152  23.414062 19.865933
    ## 4153  16.821932 17.715088
    ## 4154  15.755027 14.587017
    ## 4155  18.857269 19.406555
    ## 4156  27.273099 18.374661
    ## 4157  17.306642 19.509730
    ## 4158  26.195477 18.876413
    ## 4159  21.006374 19.463831
    ## 4160  22.203908 19.874590
    ## 4161  19.753474 18.718775
    ## 4162  19.182410 17.292308
    ## 4163  14.473612 16.901960
    ## 4164  19.322314 18.833791
    ## 4165  21.256168 19.322604
    ## 4166  14.726524 17.262543
    ## 4167  20.688780 17.225429
    ## 4168  26.745577 19.845351
    ## 4169  19.663789 15.614131
    ## 4170  16.078926 19.285472
    ## 4171  17.437163 19.719060
    ## 4172  21.001439 19.103970
    ## 4173  23.629132 18.731885
    ## 4174  18.570080 19.461439
    ## 4175  18.695850 18.421875
    ## 4176  23.837057 19.614529
    ## 4177  25.520995 19.945804
    ## 4178  25.387981 19.416588
    ## 4179  23.349811 16.035739
    ## 4180  17.440121 18.118590
    ## 4181  20.957543 18.351599
    ## 4182  16.602511 13.026674
    ## 4183  23.777883 18.900856
    ## 4184  26.749993 19.949859
    ## 4185  22.371677 18.672487
    ## 4186  19.160850 19.395447
    ## 4187  22.755997 19.211651
    ## 4188  16.765298 16.549096
    ## 4189  28.262097 17.979441
    ## 4190  21.577570 18.853320
    ## 4191  19.355572 19.749221
    ## 4192  19.874580 19.643658
    ## 4193  23.624191 18.734762
    ## 4194  23.231739 19.195795
    ## 4195  24.013157 19.206392
    ## 4196  20.811344 18.235081
    ## 4197  23.671538 18.084334
    ## 4198  18.136312 16.675524
    ## 4199  18.204838 19.240328
    ## 4200   9.441071  9.096768
    ## 4201  17.130847 19.835210
    ## 4202  24.567494 19.441507
    ## 4203  20.276185 18.917057
    ## 4204  20.121045 19.418509
    ## 4205  21.690413 18.337792
    ## 4206  17.670122 17.854123
    ## 4207  17.487710 19.891361
    ## 4208  18.050694 19.666145
    ## 4209  19.722219 19.053036
    ## 4210  17.645970 18.081736
    ## 4211  16.294781 19.367426
    ## 4212  21.696606 19.633423
    ## 4213  19.797642 17.798752
    ## 4214  15.961722 14.893829
    ## 4215  16.406504 15.345217
    ## 4216  22.444322 19.247211
    ## 4217  21.881647 18.682603
    ## 4218  22.531931 18.698111
    ## 4219  16.733165 16.995452
    ## 4220  18.911757 19.935441
    ## 4221  24.842514 19.400161
    ## 4222  17.707111 17.913838
    ## 4223  15.530921 19.905162
    ## 4224  23.871184 17.994147
    ## 4225  15.866633 19.942339
    ## 4226  14.650829 13.252257
    ## 4227  15.236486 18.085861
    ## 4228  28.012028 19.033301
    ## 4229  18.559276 18.091082
    ## 4230  24.976719 19.151395
    ## 4231  24.284263 19.141795
    ## 4232  19.432801 18.398281
    ## 4233  16.393983 18.229769
    ## 4234  18.554571 16.670635
    ## 4235  14.303749 16.680787
    ## 4236  26.241539 19.318225
    ## 4237  22.315728 19.407725
    ## 4238  16.966421 16.998957
    ## 4239  21.493532 19.531130
    ## 4240  18.465052 16.778484
    ## 4241  10.158933 12.141431
    ## 4242  17.991627 19.924933
    ## 4243  17.727927 19.922618
    ## 4244  16.590857 13.739143
    ## 4245  17.315630 18.700127
    ## 4246  24.009925 19.467007
    ## 4247  18.319913 18.739116
    ## 4248  21.703775 19.189842
    ## 4249  22.270073 18.342818
    ## 4250  18.698567 19.824879
    ## 4251  16.913400 19.926506
    ## 4252  16.556610 18.497978
    ## 4253  14.655049 16.165641
    ## 4254  25.647373 19.205995
    ## 4255  21.542795 19.467151
    ## 4256  18.683470 16.333947
    ## 4257  19.622571 19.706360
    ## 4258  25.580518 19.403737
    ## 4259  14.174759 19.606680
    ## 4260  17.677441 19.226805
    ## 4261  26.383664 19.576117
    ## 4262  24.723272 19.316268
    ## 4263  19.325123 19.283773
    ## 4264  16.809124 17.777502
    ## 4265  16.559162 17.862480
    ## 4266  24.257431 18.968556
    ## 4267  17.434758 18.888224
    ## 4268  22.744380 19.816807
    ## 4269  21.810589 18.789321
    ## 4270  21.523769 18.483977
    ## 4271  20.619378 18.512042
    ## 4272  22.811024 19.726948
    ## 4273  20.813487 14.903265
    ## 4274  26.914609 19.559794
    ## 4275  17.838108 16.347470
    ## 4276  23.169177 18.396913
    ## 4277  17.940077 13.492545
    ## 4278  15.066312 18.331674
    ## 4279  24.090592 18.714569
    ## 4280  20.164763 16.310857
    ## 4281  19.250743 16.481857
    ## 4282  22.741241 19.036086
    ## 4283  16.842265 18.131309
    ## 4284  18.344840 18.416603
    ## 4285  16.803651 18.874764
    ## 4286  27.796016 18.233427
    ## 4287  20.718319 19.655358
    ## 4288  19.081884 17.901010
    ## 4289  22.035405 18.668705
    ## 4290  18.812472 18.843535
    ## 4291  15.274914 19.916777
    ## 4292  13.803587 14.800053
    ## 4293  19.806009 19.678942
    ## 4294  24.216132 19.476308
    ## 4295  18.285201 12.463679
    ## 4296  13.103483 18.377978
    ## 4297  20.779441 19.919378
    ## 4298  24.585184 18.397608
    ## 4299  18.596357 19.714775
    ## 4300  22.514279 18.308279
    ## 4301  16.940444 16.675194
    ## 4302  20.241394 18.974074
    ## 4303  28.756516 18.903237
    ## 4304  16.313987 18.174985
    ## 4305  18.649814 19.999521
    ## 4306  15.646965 18.858469
    ## 4307  18.556233 19.064061
    ## 4308  13.464133 14.827822
    ## 4309  16.061292 15.771095
    ## 4310  21.015593 19.158267
    ## 4311  24.346286 17.339929
    ## 4312  19.984574 17.111697
    ## 4313  22.243482 17.129493
    ## 4314  20.083107 19.717878
    ## 4315  16.861571 19.125170
    ## 4316  14.567307 19.848065
    ## 4317  14.321025 18.794542
    ## 4318  14.329132 19.768342
    ## 4319  14.628279 17.052503
    ## 4320  24.492860 19.417872
    ## 4321  20.790452 18.577712
    ## 4322  24.016878 18.146857
    ## 4323  20.477768 18.159601
    ## 4324  23.967639 18.268686
    ## 4325  18.582089 17.222702
    ## 4326  19.533017 18.993676
    ## 4327  23.207727 15.741619
    ## 4328  20.194298 18.307291
    ## 4329  23.579578 18.879670
    ## 4330  15.875038 17.519820
    ## 4331  18.545596 15.457739
    ## 4332  19.777262 19.745508
    ## 4333  21.109790 16.559787
    ## 4334  17.033941 14.230760
    ## 4335  20.335726 18.024910
    ## 4336  17.935901 18.803231
    ## 4337  18.734640 18.669954
    ## 4338  17.942452 14.202540
    ## 4339  16.574881 16.806215
    ## 4340  19.768824 19.385042
    ## 4341  22.143594 18.510830
    ## 4342  20.524666 17.669863
    ## 4343  18.387172 13.571070
    ## 4344  14.644113 12.301026
    ## 4345  21.183713 18.478201
    ## 4346  21.909980 16.042926
    ## 4347  15.472972 16.596124
    ## 4348  20.549851 15.709257
    ## 4349  22.093942 18.757296
    ## 4350  19.154529 17.947524
    ## 4351  19.664679 18.581704
    ## 4352  13.748852 16.788938
    ## 4353  20.829096 19.036898
    ## 4354  19.711152 19.479883
    ## 4355  24.916159 19.722537
    ## 4356  21.407147 16.020149
    ## 4357  20.669194 18.625298
    ## 4358  17.161050 18.557675
    ## 4359  13.294481 17.756611
    ## 4360  26.451954 19.043583
    ## 4361  14.552016 17.658614
    ## 4362  13.734143 17.188866
    ## 4363  20.377249 19.322380
    ## 4364  12.071202 16.239572
    ## 4365  20.086465 19.650113
    ## 4366  17.456824 19.541660
    ## 4367  21.487696 19.463711
    ## 4368  16.355408 15.522721
    ## 4369  13.353613 19.046682
    ## 4370  20.467751 15.893965
    ## 4371  18.534297 19.685924
    ## 4372  20.458606 16.630425
    ## 4373  14.982232 16.359115
    ## 4374  17.807704 19.849314
    ## 4375  18.074799 19.202221
    ## 4376  16.275616 19.425510
    ## 4377  17.156467 18.267755
    ## 4378  21.093926 16.136224
    ## 4379  19.822617 17.861890
    ## 4380  20.757232 18.329677
    ## 4381  15.990038 18.244120
    ## 4382  19.491691 19.625563
    ## 4383  12.230693 15.128269
    ## 4384  29.211950 19.719062
    ## 4385  19.079705 15.134763
    ## 4386  18.216157 16.618950
    ## 4387  14.816267 16.240787
    ## 4388  16.516027 14.248700
    ## 4389  23.670354 19.782184
    ## 4390  10.781397 13.539537
    ## 4391  10.078084 13.546107
    ## 4392  12.778448 14.097418
    ## 4393  19.658600 18.699944
    ## 4394  24.242442 17.187913
    ## 4395  20.632477 19.075077
    ## 4396  15.756309 16.699521
    ## 4397  17.775746 15.502343
    ## 4398  18.917953 18.279778
    ## 4399  16.386245 14.963598
    ## 4400  21.068201 16.997787
    ## 4401  24.088493 18.936328
    ## 4402  16.725688 17.177468
    ## 4403  17.321916 16.988597
    ## 4404  28.845836 19.977119
    ## 4405  19.874530 19.493889
    ## 4406  20.066121 17.876652
    ## 4407  20.270079 19.736904
    ## 4408  24.293926 19.388882
    ## 4409  21.899230 19.071141
    ## 4410  24.410617 19.794587
    ## 4411  20.452961 16.844489
    ## 4412  20.650675 19.852460
    ## 4413  16.309704 19.456796
    ## 4414  21.986580 19.766777
    ## 4415  23.391188 17.279406
    ## 4416  17.808087 18.108191
    ## 4417  23.086559 19.802132
    ## 4418  18.388418 18.871875
    ## 4419  17.805314 17.558224
    ## 4420  16.146397 17.015536
    ## 4421  22.661880 19.618418
    ## 4422  16.510173 15.060466
    ## 4423  20.050242 18.813824
    ## 4424  18.661259 17.438880
    ## 4425  21.790299 19.149689
    ## 4426  23.202516 19.988884
    ## 4427  23.916039 19.051679
    ## 4428  20.887100 17.231236
    ## 4429  15.068296 18.138635
    ## 4430  11.348147 13.892466
    ## 4431  18.886122 16.872674
    ## 4432  18.736196 16.777892
    ## 4433  20.366321 16.577302
    ## 4434  21.385035 17.607500
    ## 4435  17.207835 16.222318
    ## 4436  25.719894 18.601375
    ## 4437  17.979155 18.777354
    ## 4438  19.712862 18.905925
    ## 4439  26.369267 17.473678
    ## 4440  23.640118 18.961087
    ## 4441  27.972145 19.921799
    ## 4442  17.837667 15.633082
    ## 4443  15.355032 18.447187
    ## 4444  19.698920 17.598969
    ## 4445  20.465001 18.023661
    ## 4446  23.060604 19.895412
    ## 4447  22.597575 18.366900
    ## 4448  15.857690 14.366451
    ## 4449  24.968794 19.507929
    ## 4450  19.533202 19.859286
    ## 4451  23.755444 19.915861
    ## 4452  23.484663 18.806634
    ## 4453  16.357295 17.620142
    ## 4454  18.336513 18.681488
    ## 4455  22.109947 19.678739
    ## 4456  14.796609 18.212269
    ## 4457  20.893932 18.466776
    ## 4458  18.225151 19.921624
    ## 4459  15.478876 19.800797
    ## 4460  20.851928 18.261984
    ## 4461  19.609422 16.644718
    ## 4462  25.121958 19.418477
    ## 4463  20.317517 18.156920
    ## 4464  20.987143 18.596209
    ## 4465  19.848615 17.195845
    ## 4466  16.325534 18.860459
    ## 4467  18.842072 15.422300
    ## 4468  17.059722 17.162878
    ## 4469  20.247708 17.833341
    ## 4470  19.950097 19.691677
    ## 4471  19.209281 15.503625
    ## 4472  20.520417 19.342448
    ## 4473  22.248363 16.619470
    ## 4474  21.573272 16.773435
    ## 4475  17.656103 17.624218
    ## 4476  22.308036 19.293130
    ## 4477  20.131258 19.466017
    ## 4478  16.896616 17.291170
    ## 4479  24.669380 19.847323
    ## 4480  18.985901 18.373225
    ## 4481  18.627664 16.913392
    ## 4482  15.746883 19.003560
    ## 4483  22.442152 19.982023
    ## 4484  22.863888 19.400977
    ## 4485  27.158049 19.916454
    ## 4486  16.251819 15.919437
    ## 4487  18.195617 17.235434
    ## 4488  24.452578 18.025111
    ## 4489  17.224891 19.512631
    ## 4490  24.254490 19.370256
    ## 4491  23.744971 19.247220
    ## 4492  18.512333 19.485762
    ## 4493  27.619837 19.459604
    ## 4494  24.643313 19.935683
    ## 4495  24.507662 18.786600
    ## 4496  18.991802 18.121622
    ## 4497  22.388559 18.475328
    ## 4498  13.414466 17.959185
    ## 4499  18.754905 19.639647
    ## 4500  23.822377 17.851923
    ## 4501  21.675142 17.621140
    ## 4502  16.174011 16.451257
    ## 4503  16.596592 17.254351
    ## 4504  26.353870 19.198007
    ## 4505  22.227838 19.886544
    ## 4506  23.508118 19.380870
    ## 4507  14.612746 18.323750
    ## 4508  17.265873 18.744079
    ## 4509  14.429878 16.175816
    ## 4510  21.766122 18.631539
    ## 4511  23.077854 17.055731
    ## 4512  20.163743 19.231091
    ## 4513  15.074399 19.459789
    ## 4514  21.874552 19.463521
    ## 4515  22.776996 19.205806
    ## 4516  18.349464 17.457387
    ## 4517  22.276903 19.116524
    ## 4518  21.467080 19.357488
    ## 4519  19.892794 18.892376
    ## 4520  19.223747 16.497943
    ## 4521  19.318444 18.767072
    ## 4522  19.584499 17.219735
    ## 4523  23.467806 18.478265
    ## 4524  23.969045 19.725380
    ## 4525  24.297624 19.624456
    ## 4526  21.364716 17.705492
    ## 4527  14.353304 15.101582
    ## 4528  24.913299 19.299401
    ## 4529  17.350314 18.477564
    ## 4530  18.969807 16.398152
    ## 4531  24.322516 19.070496
    ## 4532  18.622455 19.346564
    ## 4533  16.973383 18.131446
    ## 4534  14.605280 17.180615
    ## 4535  15.081577 13.010397
    ## 4536  25.137595 19.732606
    ## 4537  19.994081 19.125168
    ## 4538  18.494720 14.094929
    ## 4539  18.598472 18.101961
    ## 4540  19.533211 18.107078
    ## 4541  14.918760 14.939868
    ## 4542  18.395492 18.184519
    ## 4543  23.635376 19.701873
    ## 4544  13.670193 16.857036
    ## 4545  15.408281 16.876863
    ## 4546  23.588134 18.719606
    ## 4547  23.866100 19.698522
    ## 4548  19.212335 18.756628
    ## 4549  16.540222 13.357702
    ## 4550  13.267152 17.013958
    ## 4551  17.484298 19.092891
    ## 4552  24.390747 17.022028
    ## 4553  23.680571 18.953638
    ## 4554  24.506319 16.814907
    ## 4555  21.547638 19.908652
    ## 4556  24.862280 19.599774
    ## 4557  23.319644 19.096656
    ## 4558  17.940791 16.663931
    ## 4559  22.574767 19.137222
    ## 4560  15.996065 18.736477
    ## 4561  21.949129 18.745556
    ## 4562  28.548594 19.852310
    ## 4563  18.924979 19.353521
    ## 4564  19.603612 17.516864
    ## 4565  18.110078 16.666110
    ## 4566  23.783173 18.745707
    ## 4567  16.683340 16.077449
    ## 4568  26.187749 19.822614
    ## 4569  18.013959 19.908194
    ## 4570  26.564812 18.867055
    ## 4571  20.656038 18.901869
    ## 4572  15.267537 12.682529
    ## 4573  15.704201 18.956951
    ## 4574  18.562128 18.372567
    ## 4575  13.456076 13.415939
    ## 4576  20.855906 17.878503
    ## 4577  16.739853 19.111558
    ## 4578  25.225468 19.803892
    ## 4579  19.138609 19.390978
    ## 4580  18.329864 17.760165
    ## 4581  17.215428 19.546731
    ## 4582  20.321380 19.912551
    ## 4583  20.892084 17.631216
    ## 4584  21.435379 17.816342
    ## 4585  23.035976 18.386890
    ## 4586  18.956854 19.135363
    ## 4587  16.815019 17.745673
    ## 4588  23.547958 19.603239
    ## 4589  15.708404 12.904985
    ## 4590  18.290023 19.380503
    ## 4591  15.813007 19.301316
    ## 4592  19.891030 16.295727
    ## 4593  22.909859 19.200568
    ## 4594  16.521574 19.800003
    ## 4595  18.827321 18.792180
    ## 4596  19.638209 19.745010
    ## 4597  21.826497 18.287513
    ## 4598  13.472266 18.052581
    ## 4599  20.580534 18.281400
    ## 4600  24.187996 18.561864
    ## 4601  16.252965 17.876584
    ## 4602  19.734430 16.668406
    ## 4603  23.778218 19.684069
    ## 4604  17.137600 16.785699
    ## 4605  27.230885 19.624427
    ## 4606  14.198718 16.635333
    ## 4607  18.215346 14.869280
    ## 4608  23.900711 19.603590
    ## 4609  23.205772 19.104604
    ## 4610  21.481695 17.999293
    ## 4611  19.835334 19.050209
    ## 4612  18.034359 19.341236
    ## 4613  20.576940 15.576814
    ## 4614  20.007823 17.878516
    ## 4615  14.644343 19.217896
    ## 4616  18.533953 17.773744
    ## 4617  22.935937 17.896012
    ## 4618  18.227034 15.405652
    ## 4619  16.613339 18.107246
    ## 4620  17.170883 19.985287
    ## 4621  27.787826 19.454240
    ## 4622  23.435928 19.571628
    ## 4623  18.087127 18.113196
    ## 4624  25.763920 19.984433
    ## 4625  22.133403 19.593129
    ## 4626  16.688075 17.347386
    ## 4627  17.645115 19.322367
    ## 4628  19.928382 19.941132
    ## 4629  20.643672 18.750809
    ## 4630  18.149609 17.986625
    ## 4631  26.638809 19.847530
    ## 4632  14.374179 13.422420
    ## 4633  23.603072 18.891278
    ## 4634  20.793368 18.726841
    ## 4635  17.400621 19.221510
    ## 4636  15.676312 16.202781
    ## 4637  18.649049 18.939372
    ## 4638  14.591250 13.802610
    ## 4639  19.810657 19.829391
    ## 4640  12.044960 17.997331
    ## 4641  19.679830 19.711305
    ## 4642  12.123030 14.649780
    ## 4643  14.106047 16.120571
    ## 4644  20.098181 19.333586
    ## 4645  11.730188 11.025248
    ## 4646  19.264179 19.611970
    ## 4647  21.507054 16.386532
    ## 4648  15.574993 18.006760
    ## 4649  17.669642 18.708665
    ## 4650  21.329370 17.144238
    ## 4651  17.668511 17.274553
    ## 4652  15.623059 19.364136
    ## 4653  17.523838 19.992309
    ## 4654  24.006026 19.541549
    ## 4655  27.645948 19.884045
    ## 4656  22.310843 18.946764
    ## 4657  18.721187 19.186324
    ## 4658  26.044896 17.442232
    ## 4659  19.123505 18.981379
    ## 4660  30.338503 19.976741
    ## 4661  22.685068 17.364802
    ## 4662  15.941271 16.542720
    ## 4663  22.366844 18.612392
    ## 4664  19.293950 17.202114
    ## 4665  21.920682 19.108074
    ## 4666  19.340583 17.892710
    ## 4667  16.460487 16.158270
    ## 4668  23.467324 18.661845
    ## 4669  22.131430 19.779602
    ## 4670  19.510944 18.859203
    ## 4671  18.721567 19.982384
    ## 4672  18.038825 16.897172
    ## 4673  14.848564 18.097525
    ## 4674  12.731929 14.271166
    ## 4675  25.341596 19.726351
    ## 4676  24.340466 18.846924
    ## 4677  24.680148 18.791405
    ## 4678  19.611534 16.652272
    ## 4679  16.815452 18.061358
    ## 4680  21.574900 19.466332
    ## 4681  22.890653 17.490535
    ## 4682  23.863814 19.857999
    ## 4683  14.801871 17.327853
    ## 4684  20.454880 14.812647
    ## 4685  12.902860 12.114958
    ## 4686  20.646811 19.754159
    ## 4687  18.308936 19.098993
    ## 4688  15.959406 19.881954
    ## 4689  18.120130 18.299841
    ## 4690  14.195184 13.103989
    ## 4691  25.118239 19.196974
    ## 4692  18.306915 16.056848
    ## 4693  16.694614 19.254356
    ## 4694  13.488226 17.423805
    ## 4695  18.096629 18.590492
    ## 4696  12.839781 16.252335
    ## 4697  20.509859 18.616619
    ## 4698  20.723361 19.486783
    ## 4699  16.425359 19.998906
    ## 4700  18.468650 18.893861
    ## 4701  23.164153 19.598805
    ## 4702  19.142339 19.241421
    ## 4703  21.523689 19.368506
    ## 4704  18.219664 19.979638
    ## 4705  21.140210 19.176534
    ## 4706  21.536730 19.001561
    ## 4707  20.337849 19.768022
    ## 4708  24.453066 19.400884
    ## 4709  22.376005 17.462111
    ## 4710  20.543080 17.280536
    ## 4711  16.296230 18.147603
    ## 4712  15.497617 18.672897
    ## 4713  23.335968 18.967516
    ## 4714  19.317460 18.376211
    ## 4715  24.516158 19.592604
    ## 4716  19.884344 18.522677
    ## 4717  24.343822 19.763111
    ## 4718  19.947870 19.705520
    ## 4719  24.630552 19.529034
    ## 4720  22.264368 19.337458
    ## 4721  25.621370 19.724512
    ## 4722  14.972918 19.092166
    ## 4723  12.079143 14.191351
    ## 4724  19.275226 19.791217
    ## 4725  27.122459 18.531973
    ## 4726  14.474376 15.828475
    ## 4727  25.223748 18.478890
    ## 4728  25.161023 18.918748
    ## 4729  15.338151 16.917554
    ## 4730  22.078941 17.853022
    ## 4731  20.406355 19.510032
    ## 4732  18.471313 19.854273
    ## 4733  18.582587 18.478440
    ## 4734  20.243831 18.919300
    ## 4735  23.988777 19.364268
    ## 4736  19.500529 16.692853
    ## 4737  20.155527 17.952738
    ## 4738  21.645443 18.808280
    ## 4739  20.744560 18.643174
    ## 4740  19.133568 18.884501
    ## 4741  18.120252 18.071414
    ## 4742  21.387525 16.608947
    ## 4743  21.296878 18.963563
    ## 4744  17.654758 19.117614
    ## 4745  11.007002  9.686697
    ## 4746  18.887694 17.174376
    ## 4747  18.582793 18.056180
    ## 4748  22.936225 19.040201
    ## 4749  20.853768 18.541322
    ## 4750  18.830682 19.890887
    ## 4751  24.926795 19.133979
    ## 4752  23.190049 18.901700
    ## 4753  21.503777 17.838726
    ## 4754  26.717795 19.582013
    ## 4755  24.409751 19.601902
    ## 4756  22.673098 19.449010
    ## 4757  21.985353 18.974664
    ## 4758  19.371848 17.154095
    ## 4759  12.689812 15.164998
    ## 4760  14.662642 15.090516
    ## 4761  18.145545 14.174139
    ## 4762  14.846762 19.429884
    ## 4763  20.160625 17.584037
    ## 4764  17.806862 15.201326
    ## 4765   6.909853 10.094549
    ## 4766  22.170625 16.174082
    ## 4767  23.156485 18.973033
    ## 4768  18.459157 16.332848
    ## 4769  19.933398 18.847720
    ## 4770  14.234908 19.960565
    ## 4771  25.969194 18.956228
    ## 4772  19.615110 19.483365
    ## 4773  24.506629 19.298238
    ## 4774  16.850103 17.180654
    ## 4775  20.499413 18.763532
    ## 4776  21.758858 17.858824
    ## 4777  20.208438 19.788558
    ## 4778  17.204855 17.713489
    ## 4779  15.604973 15.887486
    ## 4780  20.896528 18.276785
    ## 4781  23.333137 19.684542
    ## 4782  21.680273 17.176484
    ## 4783  12.787994 16.722365
    ## 4784  18.949042 16.543570
    ## 4785  20.367151 18.923947
    ## 4786  11.744750 14.472393
    ## 4787  26.203336 19.839113
    ## 4788  21.697202 18.804506
    ## 4789  20.258081 19.727551
    ## 4790  13.906702 15.480942
    ## 4791  17.176090 18.597311
    ## 4792  26.334326 18.812897
    ## 4793  26.535013 19.916927
    ## 4794  17.044551 19.244071
    ## 4795  17.149915 17.155554
    ## 4796  18.398834 17.247229
    ## 4797  20.755676 19.752087
    ## 4798  16.787981 19.420500
    ## 4799  23.183657 17.895982
    ## 4800  18.413424 18.044055
    ## 4801  22.052201 19.814655
    ## 4802  19.501262 16.577816
    ## 4803  21.326793 17.298667
    ## 4804  18.315650 15.443143
    ## 4805  20.730089 16.173083
    ## 4806  18.826530 18.158499
    ## 4807  21.417213 19.975326
    ## 4808  16.812910 19.360690
    ## 4809  18.119646 18.740374
    ## 4810  21.529924 18.971483
    ## 4811  18.784967 19.652251
    ## 4812  12.466053 17.261111
    ## 4813  21.196740 17.050154
    ## 4814  19.944799 19.535692
    ## 4815  23.962758 19.621826
    ## 4816  16.900196 19.543100
    ## 4817  25.459790 19.446339
    ## 4818  20.119383 16.832238
    ## 4819  21.048175 19.957327
    ## 4820  17.187671 16.768380
    ## 4821  20.353841 19.867253
    ## 4822  21.332864 19.665269
    ## 4823  20.077709 15.267867
    ## 4824  20.147312 18.073297
    ## 4825  19.721813 19.466353
    ## 4826  19.249940 18.676816
    ## 4827  26.732472 19.750511
    ## 4828  14.151134 12.427570
    ## 4829  18.598958 19.473794
    ## 4830  18.229173 17.936465
    ## 4831  20.482785 19.677775
    ## 4832  19.751371 18.505225
    ## 4833  17.509535 15.846295
    ## 4834  14.106200 11.815087
    ## 4835  24.432483 19.244275
    ## 4836  27.309551 18.997496
    ## 4837  24.649000 19.087396
    ## 4838  22.345548 17.920735
    ## 4839  21.140538 18.628608
    ## 4840  17.761972 14.615369
    ## 4841  16.453527 17.125940
    ## 4842  23.402552 19.467908
    ## 4843  21.885936 18.026483
    ## 4844  21.553780 19.090036
    ## 4845  23.499177 19.544104
    ## 4846  22.887637 18.059464
    ## 4847  20.572344 19.444674
    ## 4848  22.823004 18.998120
    ## 4849  22.370267 18.933916
    ## 4850  17.549502 15.858360
    ## 4851  18.403106 19.936983
    ## 4852  18.832251 16.893385
    ## 4853  21.330210 18.883883
    ## 4854  14.101979 19.730063
    ## 4855  21.285799 19.889335
    ## 4856  23.034381 17.639662
    ## 4857  21.593659 18.610536
    ## 4858  18.705823 18.869833
    ## 4859  28.362041 19.878614
    ## 4860  19.242947 17.971308
    ## 4861  12.809554 10.501619
    ## 4862  22.539431 17.840097
    ## 4863  14.722737 16.232507
    ## 4864  18.905987 19.291867
    ## 4865  24.677452 18.838271
    ## 4866  20.434814 18.741727
    ## 4867  17.298017 18.912856
    ## 4868  28.651394 19.500254
    ## 4869  15.863218 16.654050
    ## 4870  13.685301 12.851009
    ## 4871  19.843345 18.648557
    ## 4872  19.105399 19.060738
    ## 4873  19.673434 19.491548
    ## 4874  22.900507 19.414558
    ## 4875  14.094495 17.690410
    ## 4876  19.625648 18.559512
    ## 4877  15.703989 19.152044
    ## 4878  15.940082 18.660672
    ## 4879  19.070028 19.322236
    ## 4880  21.018172 17.396660
    ## 4881  15.126444 16.360148
    ## 4882  21.468718 18.617364
    ## 4883  22.842466 19.395964
    ## 4884  20.455769 16.808314
    ## 4885  19.673488 16.034102
    ## 4886  18.647498 17.309826
    ## 4887  20.816530 18.893665
    ## 4888  20.315919 16.426963
    ## 4889  26.879350 19.775330
    ## 4890  22.333933 19.771781
    ## 4891  21.089482 19.863679
    ## 4892  20.112745 18.021951
    ## 4893  14.863687 17.700935
    ## 4894  14.993646 18.137589
    ## 4895  25.356786 16.699559
    ## 4896  22.290357 18.234725
    ## 4897  23.801775 19.312953
    ## 4898  16.025120 14.741928
    ## 4899  20.495990 19.220811
    ## 4900  19.527936 18.437739
    ## 4901  18.318719 18.218962
    ## 4902  22.934153 19.565299
    ## 4903  17.562994 13.979555
    ## 4904  21.175480 14.237253
    ## 4905  16.251406 19.960751
    ## 4906  24.158843 18.417631
    ## 4907  20.921566 17.566328
    ## 4908  24.538017 19.862498
    ## 4909  22.142515 17.333271
    ## 4910  21.988313 19.520404
    ## 4911  19.136254 19.625516
    ## 4912  16.991079 19.856063
    ## 4913  22.293558 19.797230
    ## 4914  23.108873 18.336373
    ## 4915  13.288301 13.836429
    ## 4916  17.021207 17.268118
    ## 4917  19.459947 19.068393
    ## 4918  21.855042 19.946687
    ## 4919  23.793748 18.256720
    ## 4920  17.100066 19.766679
    ## 4921  21.425869 15.957645
    ## 4922  18.422498 16.902550
    ## 4923  24.847088 19.971121
    ## 4924  23.773306 19.280591
    ## 4925  24.119958 19.280513
    ## 4926  15.317476 19.274762
    ## 4927  18.885564 19.166648
    ## 4928  23.463786 18.934972
    ## 4929  21.339667 18.849725
    ## 4930  18.192362 17.774481
    ## 4931  19.129814 19.939320
    ## 4932  18.330334 17.839542
    ## 4933  20.909225 19.376561
    ## 4934  16.149148 15.808972
    ## 4935  19.924788 19.585211
    ## 4936  18.363272 12.643526
    ## 4937  18.461322 19.818258
    ## 4938  25.890515 19.910939
    ## 4939  23.014940 19.827747
    ## 4940  25.452435 19.689385
    ## 4941  21.109660 18.432349
    ## 4942  19.511595 16.426946
    ## 4943  25.168170 19.677412
    ## 4944  16.932682 14.207574
    ## 4945  14.153212 19.558859
    ## 4946  24.580602 19.896680
    ## 4947  22.733139 19.705326
    ## 4948  21.449083 15.233397
    ## 4949  18.895604 17.954963
    ## 4950  14.244566 15.722112
    ## 4951  15.184570 19.152548
    ## 4952  12.725760 13.320331
    ## 4953  27.510691 18.659921
    ## 4954  18.561924 16.372058
    ## 4955  22.822227 18.305157
    ## 4956  22.060803 19.911651
    ## 4957  15.440527 17.470585
    ## 4958  28.549294 19.569101
    ## 4959  23.461976 17.398473
    ## 4960  21.487441 19.966882
    ## 4961  16.083257 16.097183
    ## 4962  25.046619 19.437237
    ## 4963  20.431035 16.814470
    ## 4964  13.756130 18.250003
    ## 4965  16.715327 19.899198
    ## 4966  22.423865 18.255889
    ## 4967  23.921666 19.237500
    ## 4968  23.481200 18.673073
    ## 4969  21.097279 17.995515
    ## 4970  21.372446 17.339976
    ## 4971  22.015082 19.089049
    ## 4972  18.142973 17.438479
    ## 4973  26.831477 19.718812
    ## 4974  17.774674 15.907461
    ## 4975  20.327897 19.155524
    ## 4976  20.758480 19.289996
    ## 4977  21.010200 19.937353
    ## 4978  19.265208 18.348836
    ## 4979  13.982362 13.549409
    ## 4980  13.790327 17.104307
    ## 4981  18.245799 15.045614
    ## 4982  17.439245 19.990132
    ## 4983  19.215796 16.634079
    ## 4984  16.972405 17.558001
    ## 4985  16.780399 15.132150
    ## 4986  14.191744 19.859452
    ## 4987  17.494787 19.903031
    ## 4988  25.103549 18.386625
    ## 4989  17.580035 19.271099
    ## 4990  26.575457 18.769043
    ## 4991  25.384945 19.763679
    ## 4992  14.733655 17.862589
    ## 4993  19.861399 17.784975
    ## 4994  24.465624 19.599409
    ## 4995  15.926323 14.300887
    ## 4996  18.173802 19.750501
    ## 4997  21.912127 17.543621
    ## 4998  22.803361 18.989173
    ## 4999  19.480193 14.765565
    ## 5000  22.315425 19.694884
    ## 5001  30.311092 18.507719
    ## 5002  23.059510 19.255937
    ## 5003  21.540715 15.559737
    ## 5004  17.255800 15.425817
    ## 5005  17.023605 14.640777
    ## 5006  19.559662 18.738401
    ## 5007  23.904540 19.656610
    ## 5008  20.457262 19.306225
    ## 5009  18.251249 19.901762
    ## 5010  20.116292 18.049099
    ## 5011  17.023415 15.351180
    ## 5012  23.757107 19.289613
    ## 5013  19.918312 19.827688
    ## 5014  16.304946 19.977151
    ## 5015  16.302438 18.736082
    ## 5016  17.403530 19.724630
    ## 5017  15.863435 14.584429
    ## 5018  20.547205 16.895775
    ## 5019  24.959446 17.753811
    ## 5020  21.879644 19.275466
    ## 5021  19.809186 17.967109
    ## 5022  16.134562 16.982802
    ## 5023  15.309247 15.291772
    ## 5024  20.929371 19.818182
    ## 5025  18.750284 18.757251
    ## 5026  16.497241 19.140261
    ## 5027  23.906937 19.741804
    ## 5028  18.645276 18.653211
    ## 5029  27.767326 18.036565
    ## 5030  13.795645 14.983352
    ## 5031  24.861106 19.323685
    ## 5032  18.640200 16.906730
    ## 5033  18.941159 19.672059
    ## 5034  24.752674 17.595647
    ## 5035  25.192116 17.713885
    ## 5036  20.556298 19.426647
    ## 5037  23.568683 18.722008
    ## 5038  25.744728 19.191760
    ## 5039  20.106796 18.852302
    ## 5040  18.752004 19.802236
    ## 5041  23.770280 18.970909
    ## 5042  15.018236 16.668765
    ## 5043  16.796503 17.783501
    ## 5044  21.813345 17.113586
    ## 5045  21.313354 18.559330
    ## 5046  19.806294 18.138866
    ## 5047  21.822415 16.491484
    ## 5048  21.202568 18.167662
    ## 5049  17.927787 17.466405
    ## 5050  25.001934 17.920157
    ## 5051  10.386351 15.249831
    ## 5052  19.001161 17.803185
    ## 5053  23.147884 19.320798
    ## 5054  22.695163 17.748038
    ## 5055  19.209523 18.572783
    ## 5056  23.148425 18.010572
    ## 5057  21.409964 17.682447
    ## 5058  20.024760 16.737140
    ## 5059  26.598833 19.287571
    ## 5060  25.581657 19.979717
    ## 5061  18.359412 19.774812
    ## 5062  20.258537 18.920067
    ## 5063  14.674111 19.029762
    ## 5064  25.402371 19.904506
    ## 5065  22.871107 19.964085
    ## 5066  20.529695 19.663562
    ## 5067  19.686037 17.218230
    ## 5068  23.138952 19.882994
    ## 5069  17.668927 19.481132
    ## 5070  16.383494 18.930142
    ## 5071  28.713797 19.237001
    ## 5072  18.233316 19.317644
    ## 5073  22.562009 18.763617
    ## 5074  25.404876 18.711006
    ## 5075  12.865642 14.550911
    ## 5076  25.993326 17.981018
    ## 5077  19.508964 19.043399
    ## 5078  22.064539 19.270817
    ## 5079  17.290991 16.233575
    ## 5080  16.085231 17.757060
    ## 5081  20.575778 19.746427
    ## 5082  14.469072 17.548590
    ## 5083  19.220833 15.957135
    ## 5084  17.613453 16.103775
    ## 5085  20.036276 19.941314
    ## 5086  23.373690 18.345748
    ## 5087  19.774460 19.284877
    ## 5088  24.578193 18.804641
    ## 5089  19.364918 19.746936
    ## 5090  20.062225 18.661376
    ## 5091  16.256912 19.809284
    ## 5092  19.800716 14.633587
    ## 5093  22.726546 19.510046
    ## 5094  23.303499 19.324203
    ## 5095  23.882250 17.887444
    ## 5096  14.444985 14.483779
    ## 5097  17.193251 19.107708
    ## 5098  22.117494 18.242766
    ## 5099  16.955809 17.955060
    ## 5100  22.063614 19.242686
    ## 5101  22.716073 19.853779
    ## 5102  16.555324 18.626949
    ## 5103  21.389016 18.557594
    ## 5104  25.785799 18.818498
    ## 5105  20.692692 18.300687
    ## 5106  23.233235 19.509799
    ## 5107  18.673266 17.704077
    ## 5108  15.539953 19.947228
    ## 5109  16.546875 14.655636
    ## 5110  17.067418 18.559057
    ## 5111  19.235296 19.349488
    ## 5112  23.001290 18.712898
    ## 5113  21.990430 19.577279
    ## 5114  23.589367 19.098418
    ## 5115  19.732820 19.617328
    ## 5116  20.418459 17.040401
    ## 5117  17.226364 15.748466
    ## 5118  25.928981 19.382398
    ## 5119  16.975706 18.243605
    ## 5120  18.488263 16.267006
    ## 5121  23.342013 19.157895
    ## 5122  22.797693 19.087838
    ## 5123  19.721385 18.629417
    ## 5124  21.810376 19.947147
    ## 5125  13.925443 16.030505
    ## 5126  23.146889 17.445089
    ## 5127  22.970672 19.868912
    ## 5128  20.620840 17.966977
    ## 5129  18.408664 17.321947
    ## 5130  19.964574 16.595499
    ## 5131  13.505274 19.249352
    ## 5132  21.060457 19.024457
    ## 5133  18.314035 17.651506
    ## 5134  23.639144 19.918134
    ## 5135  15.199982 11.463231
    ## 5136  21.590335 18.681802
    ## 5137  21.945811 19.918556
    ## 5138  17.702454 19.233445
    ## 5139  23.988860 19.839052
    ## 5140   9.254945 11.713141
    ## 5141  18.332685 17.947107
    ## 5142  22.645005 17.622082
    ## 5143  22.191206 18.850681
    ## 5144  25.508081 19.307819
    ## 5145  15.337585 17.747705
    ## 5146  22.113045 19.339363
    ## 5147  17.762927 19.545698
    ## 5148  12.886775 11.506867
    ## 5149  24.011526 19.983843
    ## 5150  27.828212 19.452956
    ## 5151  22.734666 17.969414
    ## 5152  14.281992 15.838419
    ## 5153  25.925928 19.273726
    ## 5154  11.832404 15.008006
    ## 5155  18.872865 18.912208
    ## 5156  21.930386 15.966479
    ## 5157  19.842433 19.790243
    ## 5158  21.050010 19.621065
    ## 5159  18.328503 15.638955
    ## 5160  20.496133 18.095547
    ## 5161  16.020252 19.945765
    ## 5162  24.246578 19.818092
    ## 5163  15.208445 16.987412
    ## 5164  15.914739 19.419572
    ## 5165  16.221601 13.114808
    ## 5166  26.335480 19.687817
    ## 5167  22.593137 18.460885
    ## 5168  27.870925 19.785500
    ## 5169  25.936893 19.369438
    ## 5170  23.743974 19.033531
    ## 5171  21.928361 19.816283
    ## 5172  22.022051 17.398526
    ## 5173  18.739871 19.732620
    ## 5174  20.137506 18.759356
    ## 5175  16.331200 17.968528
    ## 5176  17.989838 17.614183
    ## 5177  19.515865 15.595971
    ## 5178  23.425801 19.979671
    ## 5179  20.680982 19.206654
    ## 5180  20.877404 19.297577
    ## 5181  23.119316 18.934215
    ## 5182  23.037595 18.280842
    ## 5183  21.533854 18.395045
    ## 5184  21.090410 17.884827
    ## 5185  18.174061 19.929261
    ## 5186  16.364935 14.559742
    ## 5187  25.300714 19.761011
    ## 5188  29.128365 19.035771
    ## 5189  17.413101 16.795666
    ## 5190  20.582634 19.807280
    ## 5191  17.183887 18.276080
    ## 5192  26.686963 18.818163
    ## 5193  22.566954 19.771099
    ## 5194  20.946344 19.650304
    ## 5195  25.245416 18.550197
    ## 5196  17.811092 15.991498
    ## 5197  13.613658 14.521817
    ## 5198  23.459699 19.164713
    ## 5199  20.249145 16.685537
    ## 5200  12.937862 17.386645
    ## 5201  21.842378 18.667864
    ## 5202  21.181459 18.777149
    ## 5203  20.236520 18.334420
    ## 5204  21.029199 19.799686
    ## 5205  18.354106 16.146012
    ## 5206  14.076972 15.175384
    ## 5207  23.750717 18.482088
    ## 5208  17.006747 18.073503
    ## 5209  22.673496 19.578155
    ## 5210  19.431691 19.293111
    ## 5211  23.716171 19.758051
    ## 5212  22.129193 18.454224
    ## 5213  15.639395 19.604426
    ## 5214  22.799676 19.670974
    ## 5215  20.996698 18.782378
    ## 5216  16.571963 16.863705
    ## 5217  19.475447 18.651509
    ## 5218  25.459804 17.993056
    ## 5219  18.339051 17.027918
    ## 5220  18.416916 19.687173
    ## 5221  21.292092 19.353724
    ## 5222  19.168687 18.554680
    ## 5223  16.713485 19.646126
    ## 5224  24.037999 19.819383
    ## 5225  18.781666 19.781447
    ## 5226  15.533814 17.669037
    ## 5227  18.831189 19.390607
    ## 5228  23.811418 18.645891
    ## 5229  22.384280 18.602066
    ## 5230  21.630505 17.733706
    ## 5231  25.331815 19.527299
    ## 5232  21.001750 16.097196
    ## 5233  20.138128 19.053267
    ## 5234  19.125282 15.720478
    ## 5235  14.691120 15.509937
    ## 5236  14.419515 16.225189
    ## 5237  14.351895 13.477732
    ## 5238  15.453569 19.664632
    ## 5239  23.413405 16.802763
    ## 5240  17.069599 19.159757
    ## 5241  17.332679 14.380972
    ## 5242  24.130459 19.092521
    ## 5243  25.152164 19.609001
    ## 5244  22.312293 18.041518
    ## 5245  21.815013 19.774777
    ## 5246  16.773457 14.075694
    ## 5247  25.100374 19.667793
    ## 5248  21.925677 18.441511
    ## 5249  15.055458 16.431965
    ## 5250  23.959239 19.424981
    ## 5251  14.048931 15.517216
    ## 5252  12.216302 19.116459
    ## 5253  22.244656 18.564216
    ## 5254  24.031924 19.582904
    ## 5255  20.258532 19.539601
    ## 5256  19.462585 18.898326
    ## 5257  22.446730 19.406018
    ## 5258  21.926281 17.250885
    ## 5259  18.493851 17.996593
    ## 5260  22.655597 19.464392
    ## 5261  23.317566 18.298056
    ## 5262  18.656531 18.651374
    ## 5263  24.216231 19.906045
    ## 5264  17.606769 17.488647
    ## 5265  15.335809 18.847008
    ## 5266  21.456437 19.863647
    ## 5267  21.527477 17.780874
    ## 5268  20.423267 19.102739
    ## 5269  18.087981 15.573431
    ## 5270  15.341174 19.000619
    ## 5271  18.059280 18.572328
    ## 5272  25.712641 17.879310
    ## 5273  19.155131 19.839192
    ## 5274  13.043417 19.326174
    ## 5275  26.367095 19.624040
    ## 5276  27.500946 19.751680
    ## 5277  19.257327 14.799012
    ## 5278  18.251288 18.695906
    ## 5279  16.085679 18.816195
    ## 5280  19.821208 18.367098
    ## 5281  17.060802 18.769862
    ## 5282  17.991276 18.412993
    ## 5283  20.013318 17.591710
    ## 5284  13.854116 13.522246
    ## 5285  13.990312 15.580852
    ## 5286  13.744875 19.908763
    ## 5287  21.099214 19.937972
    ## 5288  23.003724 16.891722
    ## 5289  23.298558 19.866086
    ## 5290  18.850299 14.885653
    ## 5291  23.479723 19.776775
    ## 5292  17.511578 14.833263
    ## 5293  22.749546 18.607905
    ## 5294  23.281720 19.072178
    ## 5295  17.791469 19.907924
    ## 5296  21.548673 18.652667
    ## 5297  23.240510 19.661015
    ## 5298  15.371700 16.643437
    ## 5299  24.789725 19.839238
    ## 5300  14.931577 16.380828
    ## 5301  23.424686 18.764442
    ## 5302  15.730037 14.300389
    ## 5303  18.228519 17.641239
    ## 5304  21.096458 18.518042
    ## 5305  17.504571 19.512009
    ## 5306  19.517229 15.600282
    ## 5307  10.651078 17.830774
    ## 5308  18.688197 19.391686
    ## 5309  20.287995 19.654327
    ## 5310  20.429224 19.971452
    ## 5311  15.341374 19.658769
    ## 5312  16.170294 14.862857
    ## 5313  22.097597 18.387948
    ## 5314  19.854134 16.568380
    ## 5315  16.195099 18.072359
    ## 5316  18.076125 19.375663
    ## 5317  28.554196 19.790031
    ## 5318  25.146280 18.665650
    ## 5319  16.549890 15.070449
    ## 5320  17.258365 16.821456
    ## 5321  17.716501 19.566076
    ## 5322  22.191431 17.931799
    ## 5323  18.866429 18.942114
    ## 5324  17.613050 17.292889
    ## 5325  23.071237 19.993071
    ## 5326  17.717698 18.695551
    ## 5327  19.894891 18.389857
    ## 5328  22.568127 19.468819
    ## 5329  12.608533 14.653342
    ## 5330  20.862458 16.412768
    ## 5331  21.104163 19.673269
    ## 5332  17.350348 16.066084
    ## 5333  16.448063 18.850777
    ## 5334  22.730014 18.081883
    ## 5335  12.288113 16.120718
    ## 5336  22.320391 18.880568
    ## 5337  18.587336 17.648040
    ## 5338  16.519634 17.504983
    ## 5339  17.282695 15.371121
    ## 5340  21.340194 19.211465
    ## 5341  16.447445 17.362722
    ## 5342  25.784763 19.836847
    ## 5343  17.914380 13.616065
    ## 5344  14.787637 15.255280
    ## 5345  20.404730 19.493479
    ## 5346  17.600707 19.769163
    ## 5347  18.210590 18.826393
    ## 5348  11.573851 14.750356
    ## 5349  27.899839 19.810985
    ## 5350  25.396669 19.945656
    ## 5351  20.390675 18.343124
    ## 5352  21.921918 19.140725
    ## 5353  20.845025 19.439111
    ## 5354  23.456737 18.373437
    ## 5355  19.942838 19.393758
    ## 5356  26.403370 19.583503
    ## 5357  19.465415 16.526884
    ## 5358  22.124230 19.479334
    ## 5359  19.150564 18.270563
    ## 5360  26.089904 17.768946
    ## 5361  28.780835 19.698738
    ## 5362  24.711478 16.708705
    ## 5363  22.209763 18.482020
    ## 5364  25.146752 18.678398
    ## 5365  18.701166 17.075901
    ## 5366  20.948844 19.702836
    ## 5367  21.554634 18.090322
    ## 5368  13.928671 12.972521
    ## 5369  18.503152 16.776666
    ## 5370  23.046277 18.253273
    ## 5371  24.154420 19.743701
    ## 5372  24.260193 17.286004
    ## 5373  22.342100 18.702700
    ## 5374  26.207722 19.809895
    ## 5375  18.780174 19.400258
    ## 5376  23.867216 19.017814
    ## 5377  25.240246 19.025145
    ## 5378  23.029249 17.152039
    ## 5379  23.856174 19.066033
    ## 5380  23.826742 18.661466
    ## 5381  24.919058 19.830180
    ## 5382  16.599120 19.586160
    ## 5383  18.433401 14.633843
    ## 5384  16.354805 18.016663
    ## 5385  17.982879 19.699902
    ## 5386  22.446129 18.296803
    ## 5387  25.320457 19.338225
    ## 5388  24.697593 18.005270
    ## 5389  23.676371 19.808091
    ## 5390  22.564480 17.913725
    ## 5391  24.501386 19.324721
    ## 5392  20.805592 18.656666
    ## 5393  15.330168 18.207065
    ## 5394  25.456606 19.289555
    ## 5395  18.075812 17.445581
    ## 5396  17.862795 17.260211
    ## 5397  14.496784 19.352412
    ## 5398  24.134074 19.383196
    ## 5399  18.447650 16.668448
    ## 5400  25.126391 18.013888
    ## 5401  19.331993 15.325420
    ## 5402  21.758741 18.319397
    ## 5403  25.471230 19.159833
    ## 5404  16.245492 17.852801
    ## 5405  24.977089 19.161770
    ## 5406  23.955819 19.488720
    ## 5407  24.788003 17.737245
    ## 5408  29.186639 19.528164
    ## 5409  11.477815 14.664804
    ## 5410  23.191582 19.051801
    ## 5411  25.653202 16.767215
    ## 5412  30.695322 18.397820
    ## 5413  23.334058 19.001358
    ## 5414  19.706560 18.142091
    ## 5415  28.732316 19.772892
    ## 5416  21.950754 19.007964
    ## 5417  25.091467 18.928636
    ## 5418  23.266943 17.080924
    ## 5419  22.052767 19.782829
    ## 5420  22.801219 19.560627
    ## 5421  19.317885 19.824355
    ## 5422  18.584615 17.987835
    ## 5423  23.658292 19.312640
    ## 5424  23.115779 19.510168
    ## 5425  19.557548 19.860551
    ## 5426  22.144354 18.664546
    ## 5427  15.947592 18.553389
    ## 5428  25.291759 18.343688
    ## 5429  22.898084 19.766984
    ## 5430  18.892272 18.833404
    ## 5431  28.649123 18.548142
    ## 5432  21.318107 17.432377
    ## 5433  16.687095 15.624766
    ## 5434  16.392614 19.520266
    ## 5435  14.477474 19.889826
    ## 5436  20.339741 18.382374
    ## 5437  25.413125 19.861087
    ## 5438  22.930154 18.538427
    ## 5439  22.799722 19.924856
    ## 5440  20.162001 19.457687
    ## 5441  16.805472 14.519640
    ## 5442  15.021348 16.386137
    ## 5443  19.139999 19.098150
    ## 5444  16.545997 19.394022
    ## 5445  17.167356 17.773336
    ## 5446  19.394102 19.312592
    ## 5447  19.367627 18.062288
    ## 5448  17.524000 18.776279
    ## 5449  18.108309 18.024340
    ## 5450  19.592395 18.323627
    ## 5451   9.797365 10.688115
    ## 5452  22.709431 18.765520
    ## 5453  20.274253 18.884379
    ## 5454  23.909348 19.656466
    ## 5455  21.523165 18.684894
    ## 5456  24.002157 17.095120
    ## 5457  17.569746 18.918889
    ## 5458  14.378133 19.030757
    ## 5459  22.180381 17.495558
    ## 5460  18.789465 17.887457
    ## 5461  16.716658 19.116821
    ## 5462  18.814572 18.360415
    ## 5463  23.741901 18.748767
    ## 5464  15.879292 18.373834
    ## 5465  17.947358 18.737850
    ## 5466  21.840252 18.259028
    ## 5467  18.828699 18.071605
    ## 5468  13.818588 19.538132
    ## 5469  23.709091 19.400061
    ## 5470  14.424204 17.596419
    ## 5471  16.721251 17.704163
    ## 5472  22.829563 17.565101
    ## 5473  24.371442 17.429295
    ## 5474  18.549513 16.993303
    ## 5475  19.891727 16.899178
    ## 5476  21.858394 17.050023
    ## 5477  25.365464 16.740325
    ## 5478  16.491176 16.432760
    ## 5479  15.839785 16.259488
    ## 5480  24.610642 19.836858
    ## 5481  19.511905 17.009770
    ## 5482  18.095014 16.089803
    ## 5483  16.643712 16.627861
    ## 5484  18.250375 16.269317
    ## 5485  23.926231 19.491973
    ## 5486  17.848900 19.728937
    ## 5487  23.557206 16.759459
    ## 5488  19.656862 17.456614
    ## 5489  23.135713 19.159876
    ## 5490  18.223842 17.328330
    ## 5491  16.160957 19.826916
    ## 5492  24.388721 18.027407
    ## 5493  24.286739 19.919468
    ## 5494  17.907099 19.784270
    ## 5495  17.069009 18.900310
    ## 5496  18.037031 19.086482
    ## 5497  24.824279 18.667906
    ## 5498  24.308724 16.417840
    ## 5499  21.150712 17.509114
    ## 5500  16.051913 18.850696
    ## 5501  15.859872 17.805971
    ## 5502  18.365171 19.943350
    ## 5503  13.894822 11.541439
    ## 5504  23.386568 19.948006
    ## 5505  23.633678 17.351300
    ## 5506  22.642980 19.839866
    ## 5507  20.549396 18.395583
    ## 5508  15.953877 15.376704
    ## 5509  17.610126 18.408341
    ## 5510  22.395872 17.413925
    ## 5511  21.634870 18.828542
    ## 5512  22.019874 19.857486
    ## 5513  17.919182 18.945465
    ## 5514  20.149019 19.621865
    ## 5515  18.877784 18.130527
    ## 5516  23.485447 19.522474
    ## 5517  19.958652 19.338317
    ## 5518  18.892166 19.857448
    ## 5519  23.605033 19.175780
    ## 5520  25.507509 18.658987
    ## 5521  22.286690 18.666188
    ## 5522  21.620397 19.700963
    ## 5523  22.670490 19.623696
    ## 5524  19.198397 19.545561
    ## 5525  14.302223 19.229334
    ## 5526  21.381271 19.808543
    ## 5527  19.650917 19.818770
    ## 5528  16.558618 17.540215
    ## 5529  23.535658 19.374545
    ## 5530  20.972333 19.691986
    ## 5531  24.640698 19.769370
    ## 5532  22.387626 19.941981
    ## 5533  21.662437 18.934114
    ## 5534  24.751679 18.753580
    ## 5535  21.349462 18.672757
    ## 5536  17.894026 17.470639
    ## 5537  21.669027 18.154167
    ## 5538  22.229766 19.744980
    ## 5539  16.034194 16.892457
    ## 5540  22.395550 19.633463
    ## 5541  19.058238 18.496725
    ## 5542  22.186904 18.881760
    ## 5543  21.479159 19.814273
    ## 5544  20.251354 19.578978
    ## 5545  23.683631 19.030176
    ## 5546  19.398617 18.802169
    ## 5547  16.271231 16.380093
    ## 5548  20.451875 19.162882
    ## 5549  22.073376 18.541710
    ## 5550  17.708509 18.492212
    ## 5551  13.394050 19.716684
    ## 5552  21.218887 18.496293
    ## 5553  17.895375 18.540855
    ## 5554  21.980210 18.674186
    ## 5555  16.014079 15.839797
    ## 5556  13.235972 19.851461
    ## 5557  20.617144 15.998492
    ## 5558  16.958566 15.924996
    ## 5559  18.747878 15.611721
    ## 5560  15.689729 18.870558
    ## 5561  20.804372 18.749249
    ## 5562  20.279928 17.386137
    ## 5563  27.933456 18.558030
    ## 5564  20.542138 19.303761
    ## 5565  14.672644 16.602323
    ## 5566  22.543010 19.778537
    ## 5567  21.362820 19.473233
    ## 5568  17.207450 13.975324
    ## 5569  21.990406 16.782000
    ## 5570  29.066509 19.298943
    ## 5571  23.318627 19.997331
    ## 5572  17.617466 17.468261
    ## 5573  20.569120 19.532702
    ## 5574  16.711179 16.697202
    ## 5575  14.996746 12.385287
    ## 5576  21.781112 19.011533
    ## 5577  22.814447 17.515044
    ## 5578  24.998112 19.560013
    ## 5579  22.312459 19.531605
    ## 5580  17.169422 16.870055
    ## 5581  17.986263 17.062667
    ## 5582  16.850663 19.714722
    ## 5583  22.097038 17.773813
    ## 5584  15.988164 14.553902
    ## 5585  22.760845 17.826402
    ## 5586  16.107251 18.504115
    ## 5587  20.141460 17.619835
    ## 5588  20.201010 19.650924
    ## 5589  26.119154 19.144260
    ## 5590  24.299402 19.696697
    ## 5591  16.866468 19.922935
    ## 5592  18.850654 16.757945
    ## 5593  25.165701 19.262037
    ## 5594  21.452351 17.380968
    ## 5595  17.980630 16.882569
    ## 5596  22.534728 19.409749
    ## 5597  15.880251 14.135914
    ## 5598  20.383350 18.794725
    ## 5599  24.069707 17.939009
    ## 5600  15.531103 18.376382
    ## 5601  17.021754 16.695461
    ## 5602  20.084733 19.678148
    ## 5603  12.474332 12.547873
    ## 5604  21.915039 16.700954
    ## 5605  16.633373 17.562334
    ## 5606  20.804239 19.902307
    ## 5607  10.340474 15.877256
    ## 5608  22.071653 18.667670
    ## 5609  20.833593 19.390460
    ## 5610  16.742398 19.953417
    ## 5611  16.635334 16.771849
    ## 5612  17.010362 18.660368
    ## 5613  25.520958 18.603926
    ## 5614  16.645473 19.901448
    ## 5615  17.235920 18.335270
    ## 5616  26.515618 18.955225
    ## 5617  22.232213 19.453483
    ## 5618  20.061764 19.369404
    ## 5619  23.830533 18.229132
    ## 5620  22.318942 18.355210
    ## 5621  17.026216 17.278383
    ## 5622  15.489173 17.087840
    ## 5623  25.701121 19.700053
    ## 5624  17.725171 17.545899
    ## 5625  14.428206 17.752833
    ## 5626  24.385081 19.832577
    ## 5627  27.235440 18.349159
    ## 5628  17.746506 18.636080
    ## 5629  28.832678 19.406613
    ## 5630  17.756241 15.441447
    ## 5631   9.549947 16.217112
    ## 5632  19.788840 19.932725
    ## 5633  16.367845 17.593855
    ## 5634  22.445240 18.755155
    ## 5635  21.107236 17.541347
    ## 5636  18.939519 16.397290
    ## 5637  21.519549 19.107623
    ## 5638  18.796978 19.843937
    ## 5639  20.778743 15.750598
    ## 5640  22.166942 18.013514
    ## 5641  18.913563 18.489351
    ## 5642  19.336299 18.406331
    ## 5643  24.151368 19.354119
    ## 5644  13.184508 12.541114
    ## 5645  20.570912 19.221637
    ## 5646  13.098085 18.940469
    ## 5647  20.032269 18.403411
    ## 5648  15.732532 13.853089
    ## 5649  22.270215 19.122285
    ## 5650  15.702601 19.156545
    ## 5651  25.285017 19.207454
    ## 5652  11.814442 11.618030
    ## 5653  17.607337 16.347406
    ## 5654  20.263764 16.930095
    ## 5655  23.978520 19.529943
    ## 5656  16.777465 18.728834
    ## 5657  22.372784 18.286557
    ## 5658  16.705202 19.643475
    ## 5659  17.994358 19.863561
    ## 5660  18.235515 16.509072
    ## 5661  16.613089 18.927626
    ## 5662  20.407313 16.930365
    ## 5663  22.481370 19.693499
    ## 5664  17.656031 17.973940
    ## 5665  21.471160 19.771313
    ## 5666  13.104794 15.396416
    ## 5667  21.092536 18.818800
    ## 5668  15.186117 15.388855
    ## 5669  18.684924 18.102584
    ## 5670  22.579195 18.255456
    ## 5671  22.136897 18.022807
    ## 5672  16.744973 15.803226
    ## 5673  19.754728 17.536141
    ## 5674  16.946663 16.948935
    ## 5675  21.301749 19.208423
    ## 5676  19.427157 19.614051
    ## 5677  17.966436 16.708114
    ## 5678  18.881533 17.750235
    ## 5679  23.121835 18.402436
    ## 5680  19.455029 19.007623
    ## 5681  19.477286 18.682371
    ## 5682  22.253267 19.603747
    ## 5683  19.185739 19.190882
    ## 5684  18.579932 18.198984
    ## 5685  15.483049 16.468101
    ## 5686  19.585317 19.010195
    ## 5687  22.905386 17.436294
    ## 5688  23.425983 19.528325
    ## 5689  19.782847 19.614445
    ## 5690  19.795689 19.923785
    ## 5691  25.338902 19.324042
    ## 5692  23.612771 19.301756
    ## 5693  20.284532 17.811979
    ## 5694  14.233957 17.065752
    ## 5695  22.938005 19.371940
    ## 5696  17.498547 19.097034
    ## 5697  18.232804 17.746971
    ## 5698  13.351461 15.936913
    ## 5699  11.581549 11.445502
    ## 5700  19.444423 17.806420
    ## 5701  23.839420 19.898791
    ## 5702  17.668852 18.024163
    ## 5703  19.483384 16.534336
    ## 5704  25.440346 18.833572
    ## 5705  20.704435 19.802056
    ## 5706  22.663455 18.821562
    ## 5707  21.643816 19.533412
    ## 5708  27.274483 19.922525
    ## 5709  22.050999 19.816795
    ## 5710  20.447179 19.032312
    ## 5711  16.856086 16.575292
    ## 5712  22.516567 17.006076
    ## 5713  20.470625 17.287414
    ## 5714  16.162909 17.343421
    ## 5715  15.629968 14.262041
    ## 5716  11.408737 17.098507
    ## 5717  15.275908 15.673121
    ## 5718  18.959546 18.535542
    ## 5719  16.897527 17.488126
    ## 5720  19.219046 19.880683
    ## 5721  13.176446 12.877420
    ## 5722  20.454740 19.896183
    ## 5723  17.296899 19.488322
    ## 5724  15.420324 16.512452
    ## 5725  20.410903 17.315358
    ## 5726  23.126741 18.866151
    ## 5727  18.539286 19.282258
    ## 5728  12.855620 18.692966
    ## 5729  16.023901 18.857661
    ## 5730  21.162358 19.675412
    ## 5731  18.380306 15.933574
    ## 5732  23.505017 19.400277
    ## 5733  21.762892 18.979726
    ## 5734  12.169982 16.097390
    ## 5735  16.398892 19.765167
    ## 5736  30.778671 19.764038
    ## 5737  18.900141 18.195881
    ## 5738  23.793281 18.600918
    ## 5739  10.641571 12.936724
    ## 5740  22.082777 16.532805
    ## 5741  14.665647 14.275088
    ## 5742  18.757103 19.956785
    ## 5743  17.338523 18.823017
    ## 5744  22.901133 18.925424
    ## 5745  21.526084 19.731056
    ## 5746  21.268921 17.493975
    ## 5747  20.545478 18.206505
    ## 5748  16.162062 15.924289
    ## 5749  16.212783 19.626024
    ## 5750  14.791445 15.212666
    ## 5751  18.837462 18.831548
    ## 5752  20.074783 17.781643
    ## 5753  20.094017 17.766462
    ## 5754  23.424317 18.658421
    ## 5755  20.373025 18.858916
    ## 5756  14.709752 14.013266
    ## 5757  18.945345 17.636249
    ## 5758  17.960751 18.707763
    ## 5759  16.989060 18.743554
    ## 5760  23.893478 19.065155
    ## 5761  28.220699 19.403828
    ## 5762  21.475468 19.818220
    ## 5763  21.197858 17.254095
    ## 5764  24.641895 19.852274
    ## 5765  20.224404 18.214876
    ## 5766  21.966156 16.516641
    ## 5767  22.232553 18.033597
    ## 5768  21.870154 19.898915
    ## 5769  15.852067 19.796662
    ## 5770  16.655901 16.552728
    ## 5771  23.338691 19.483341
    ## 5772  17.169849 19.230210
    ## 5773  25.659955 19.998933
    ## 5774  19.118936 17.901609
    ## 5775  19.549507 18.327288
    ## 5776  24.390545 18.859041
    ## 5777  19.368388 16.075178
    ## 5778  15.039469 16.204947
    ## 5779  22.693494 19.924861
    ## 5780  25.724441 18.038693
    ## 5781  23.414731 19.416446
    ## 5782  22.405820 17.966921
    ## 5783  24.433323 19.283648
    ## 5784  24.609947 18.971043
    ## 5785  18.408581 18.847604
    ## 5786  18.890818 18.196236
    ## 5787  19.854705 19.959925
    ## 5788  23.264419 18.394937
    ## 5789  21.022214 17.583644
    ## 5790  12.788610 12.530931
    ## 5791  20.551588 19.394951
    ## 5792  24.887697 19.277187
    ## 5793  21.471608 18.329357
    ## 5794  19.693972 19.275182
    ## 5795  17.330132 19.236460
    ## 5796  22.051380 18.345243
    ## 5797  24.322633 18.752971
    ## 5798  21.308409 19.791369
    ## 5799  16.598310 19.076681
    ## 5800  17.231874 19.416265
    ## 5801  21.441348 19.724162
    ## 5802  25.511570 18.329566
    ## 5803  19.113769 17.773040
    ## 5804  23.682879 18.657456
    ## 5805  12.407457 16.088390
    ## 5806  20.089995 19.930781
    ## 5807  21.716074 19.257034
    ## 5808  24.268547 19.813403
    ## 5809  20.067141 17.954607
    ## 5810  20.306403 17.485483
    ## 5811  24.035199 19.396689
    ## 5812  24.963418 19.830182
    ## 5813  15.673244 18.063733
    ## 5814  13.098466 14.783990
    ## 5815  17.111938 19.460952
    ## 5816  15.761192 13.673483
    ## 5817  13.561715 17.670008
    ## 5818  16.037681 19.343077
    ## 5819  19.218440 17.167139
    ## 5820  22.013570 19.173356
    ## 5821  22.609378 18.136195
    ## 5822  26.820311 19.560067
    ## 5823  16.151444 18.978543
    ## 5824  18.519473 19.724453
    ## 5825  16.093827 18.043755
    ## 5826  14.890856 16.468609
    ## 5827  27.448344 17.651739
    ## 5828  25.250291 17.769165
    ## 5829  22.043418 19.724535
    ## 5830  19.023267 19.680125
    ## 5831  18.328365 16.555676
    ## 5832  16.110166 15.974757
    ## 5833  25.804147 18.954831
    ## 5834  22.238886 19.508694
    ## 5835  23.940973 18.941313
    ## 5836  15.513553 14.506968
    ## 5837  20.630760 19.596779
    ## 5838  22.218029 19.556614
    ## 5839  12.982150 16.888098
    ## 5840  23.848300 19.380074
    ## 5841  15.282124 15.380420
    ## 5842  22.058509 17.093139
    ## 5843  18.733185 19.877547
    ## 5844  20.325212 19.679028
    ## 5845  22.833799 19.530359
    ## 5846  21.850241 18.774561
    ## 5847  21.510506 19.498362
    ## 5848  16.032809 12.665806
    ## 5849  21.265058 18.821411
    ## 5850  28.227374 19.648519
    ## 5851  19.631865 19.356383
    ## 5852  17.552404 17.985487
    ## 5853  23.204763 19.692003
    ## 5854  22.091505 19.839019
    ## 5855  23.162598 19.707126
    ## 5856  18.202103 15.704155
    ## 5857  18.088302 16.911642
    ## 5858  27.413265 19.781666
    ## 5859  14.252135 15.770769
    ## 5860  26.947101 18.123428
    ## 5861  25.206918 18.524039
    ## 5862  18.559135 19.264477
    ## 5863  19.184522 19.087877
    ## 5864  19.811507 18.311600
    ## 5865  17.989121 19.489965
    ## 5866  20.995230 19.607291
    ## 5867  15.568347 15.995271
    ## 5868  23.701066 18.576116
    ## 5869  28.994920 19.628737
    ## 5870  17.658471 16.142533
    ## 5871  24.933715 19.853160
    ## 5872  22.005334 18.041880
    ## 5873  19.067132 18.860340
    ## 5874  27.573007 19.815047
    ## 5875  19.557652 19.378221
    ## 5876  21.158942 19.786746
    ## 5877  20.194369 19.444516
    ## 5878  22.891896 18.902187
    ## 5879  19.334022 17.767319
    ## 5880  22.395228 18.992723
    ## 5881  19.370785 17.679428
    ## 5882  21.914382 19.352658
    ## 5883  21.075196 18.788650
    ## 5884  15.419767 19.971318
    ## 5885  18.634827 17.891823
    ## 5886  17.134426 19.964777
    ## 5887  23.420798 16.366077
    ## 5888  24.177597 19.646363
    ## 5889  19.554424 18.943074
    ## 5890  16.779616 19.576395
    ## 5891  20.871814 19.840287
    ## 5892  23.954507 18.189469
    ## 5893  23.419853 18.767064
    ## 5894  14.642783 16.450228
    ## 5895  17.179817 18.178608
    ## 5896  24.332316 19.027834
    ## 5897  12.000650 12.526423
    ## 5898  15.900757 18.874241
    ## 5899  23.078358 19.697861
    ## 5900  19.143597 16.470218
    ## 5901  22.924792 18.489306
    ## 5902  16.830511 18.111531
    ## 5903  17.068589 14.702199
    ## 5904  24.560062 19.563698
    ## 5905  22.517881 19.750470
    ## 5906  18.623879 17.533462
    ## 5907  17.422479 16.505368
    ## 5908  22.406481 17.973782
    ## 5909  15.276558 16.569741
    ## 5910  18.000378 17.441469
    ## 5911  16.009297 17.492586
    ## 5912  19.039747 15.549445
    ## 5913  21.595315 19.017416
    ## 5914  16.941087 19.149890
    ## 5915  22.102041 16.380113
    ## 5916  19.762439 19.396392
    ## 5917  20.513825 19.559816
    ## 5918  18.300134 18.124172
    ## 5919  26.865460 18.926625
    ## 5920  23.101197 19.120517
    ## 5921  23.305806 17.783167
    ## 5922  14.590585 18.344571
    ## 5923  25.387477 19.722010
    ## 5924  12.437016 11.407677
    ## 5925   7.623876  6.943641
    ## 5926  14.693976 16.566672
    ## 5927  24.402751 18.977675
    ## 5928  24.368850 19.388412
    ## 5929  23.486210 19.106078
    ## 5930  22.662847 19.221194
    ## 5931  25.748538 19.558621
    ## 5932  27.183581 19.975646
    ## 5933  16.198885 15.653911
    ## 5934  15.122240 16.042312
    ## 5935  20.039462 17.798081
    ## 5936  13.836031 15.882264
    ## 5937  15.666753 18.865388
    ## 5938  20.725786 18.310595
    ## 5939  20.539975 16.997926
    ## 5940  20.951046 19.813894
    ## 5941  16.670189 18.247003
    ## 5942  20.701599 16.872524
    ## 5943  19.410361 16.620542
    ## 5944  21.983313 19.126542
    ## 5945  27.029737 18.822276
    ## 5946  18.163665 18.210705
    ## 5947  24.680780 17.828151
    ## 5948  17.686889 18.595764
    ## 5949  21.556031 19.692024
    ## 5950  15.556204 19.443452
    ## 5951  24.583745 19.825223
    ## 5952  22.039856 19.931100
    ## 5953  20.909924 19.490915
    ## 5954  22.682716 19.909586
    ## 5955  18.340924 14.156243
    ## 5956  14.283943 15.033899
    ## 5957  17.018355 14.232793
    ## 5958  14.286320 13.267428
    ## 5959  20.483240 19.996685
    ## 5960  20.725960 19.836831
    ## 5961  22.666970 18.320514
    ## 5962  21.903434 18.908904
    ## 5963  19.358705 19.833145
    ## 5964  25.041756 19.076556
    ## 5965  25.289906 19.922444
    ## 5966  19.759392 18.491688
    ## 5967  20.106513 19.416947
    ## 5968  20.377409 17.475190
    ## 5969  23.080150 18.539291
    ## 5970  17.107138 19.175403
    ## 5971  19.857643 17.001954
    ## 5972  18.756147 19.711747
    ## 5973  21.731387 18.702030
    ## 5974  24.945820 19.974618
    ## 5975  24.023109 18.914475
    ## 5976  19.526947 14.927830
    ## 5977  21.057443 18.272202
    ## 5978  20.696328 19.845886
    ## 5979  19.984732 19.602325
    ## 5980  18.855173 19.839052
    ## 5981  19.073944 17.541281
    ## 5982  15.394687 16.329234
    ## 5983  26.114721 19.163309
    ## 5984  17.910462 19.981782
    ## 5985  21.983457 19.843187
    ## 5986  28.730614 17.831133
    ## 5987  26.739181 17.429105
    ## 5988  22.806277 19.730939
    ## 5989  22.551017 19.978814
    ## 5990  21.530213 16.959814
    ## 5991  24.939545 19.966407
    ## 5992  15.871584 13.372990
    ## 5993  24.106953 19.996620
    ## 5994  16.573464 19.237803
    ## 5995  17.034654 18.660459
    ## 5996  21.841204 17.603951
    ## 5997  24.379763 18.846361
    ## 5998  20.865487 17.221503
    ## 5999  20.363044 19.389514
    ## 6000  23.434926 19.377539
    ## 6001  26.806228 19.846884
    ## 6002  21.162100 17.422319
    ## 6003  15.406491 19.248341
    ## 6004  18.881865 16.073624
    ## 6005  13.357202 19.552708
    ## 6006  14.814712 15.946345
    ## 6007  24.349928 18.950355
    ## 6008   9.489664 11.292964
    ## 6009  15.983090 17.880989
    ## 6010  27.514592 18.120250
    ## 6011  20.713109 19.199688
    ## 6012  26.506455 18.265167
    ## 6013  22.697280 19.177624
    ## 6014  17.763805 15.990586
    ## 6015  16.428573 17.050858
    ## 6016  21.133835 18.311616
    ## 6017  27.237244 18.375173
    ## 6018  19.603488 18.307701
    ## 6019  14.013124 17.201222
    ## 6020  20.591184 18.538257
    ## 6021  19.000963 18.764306
    ## 6022  22.049053 16.499986
    ## 6023  19.623760 19.366719
    ## 6024  23.963390 19.935831
    ## 6025  21.206648 19.173463
    ## 6026  21.231010 18.074393
    ## 6027  27.091574 19.017005
    ## 6028  12.129644 19.837006
    ## 6029  17.116125 17.832830
    ## 6030  21.023555 19.232111
    ## 6031  17.477724 15.956885
    ## 6032  22.861964 19.939446
    ## 6033  21.311539 19.064709
    ## 6034  22.362602 19.662873
    ## 6035  20.539454 17.289108
    ## 6036  18.784325 18.390878
    ## 6037  11.081554 15.491478
    ## 6038  16.277417 19.808152
    ## 6039  20.205936 17.930059
    ## 6040  23.018030 19.810493
    ## 6041  23.259915 19.641778
    ## 6042  17.637752 15.309649
    ## 6043  12.147317 12.200791
    ## 6044  20.288898 19.652088
    ## 6045  19.996337 19.821545
    ## 6046  24.376998 17.875397
    ## 6047  21.962082 18.449343
    ## 6048  19.660074 14.737427
    ## 6049  27.132638 19.571285
    ## 6050  15.476202 16.652783
    ## 6051  18.667503 16.583525
    ## 6052   9.289748 13.338268
    ## 6053  22.927530 19.040560
    ## 6054  20.138858 16.506150
    ## 6055  20.556546 19.597707
    ## 6056  16.570992 19.342529
    ## 6057  22.409491 18.434269
    ## 6058  18.759371 18.137410
    ## 6059  18.090812 17.369934
    ## 6060  21.202907 16.871082
    ## 6061  28.478095 19.925547
    ## 6062  27.489363 17.839579
    ## 6063  21.394090 14.182609
    ## 6064  21.534576 18.880625
    ## 6065  12.334131 17.295259
    ## 6066  12.447279 19.686988
    ## 6067  17.813070 17.063475
    ## 6068  23.999596 18.857150
    ## 6069  18.834357 17.060800
    ## 6070  22.290667 18.539436
    ## 6071  18.228086 19.442237
    ## 6072  17.448818 19.329543
    ## 6073  24.082562 17.164458
    ## 6074  23.246734 19.558623
    ## 6075  22.920303 18.273534
    ## 6076  26.681938 19.630464
    ## 6077  24.583239 19.033032
    ## 6078  12.735596 19.224034
    ## 6079  24.014319 19.517799
    ## 6080  25.734033 18.846975
    ## 6081  17.947232 19.796067
    ## 6082  19.045414 19.679906
    ## 6083  15.503913 17.108559
    ## 6084  16.334793 12.098234
    ## 6085  28.480749 19.936289
    ## 6086  23.679881 18.556576
    ## 6087  20.552820 17.430178
    ## 6088  18.856493 16.919478
    ## 6089  25.738535 19.084600
    ## 6090  25.372992 17.929754
    ## 6091  18.607079 17.795103
    ## 6092  20.177334 16.726095
    ## 6093  17.811052 16.877572
    ## 6094  16.379691 15.879420
    ## 6095  25.575158 18.384287
    ## 6096  20.319838 19.190917
    ## 6097  18.599321 19.271953
    ## 6098  21.564238 17.774375
    ## 6099  13.802410 11.553859
    ## 6100  11.452774 16.665379
    ## 6101  19.204687 16.975469
    ## 6102  22.319232 17.024081
    ## 6103  17.411887 16.853853
    ## 6104  22.220579 18.906892
    ## 6105  16.704847 16.990473
    ## 6106  24.549750 18.750297
    ## 6107  18.011230 18.517132
    ## 6108  12.741829 14.434155
    ## 6109  18.113773 18.111896
    ## 6110  20.716542 18.472722
    ## 6111  18.480971 19.946463
    ## 6112  22.940776 19.414457
    ## 6113  25.683770 19.980126
    ## 6114  22.763391 17.757412
    ## 6115  22.831149 18.884080
    ## 6116  19.029567 15.677080
    ## 6117  20.890867 18.542723
    ## 6118  16.013498 19.968128
    ## 6119  23.261994 19.428506
    ## 6120  21.748833 17.873778
    ## 6121  19.079713 19.932011
    ## 6122  18.946955 19.116058
    ## 6123  14.714671 15.446553
    ## 6124  19.415126 16.440413
    ## 6125  21.026934 18.474107
    ## 6126  19.381520 18.441084
    ## 6127  21.254810 17.925336
    ## 6128  15.949346 13.511660
    ## 6129  23.696816 18.279249
    ## 6130  22.547481 18.682236
    ## 6131  21.397344 19.639365
    ## 6132  17.232987 19.804890
    ## 6133  15.269212 16.329629
    ## 6134  18.473891 19.740713
    ## 6135  20.449477 19.691069
    ## 6136  19.039018 18.340333
    ## 6137  21.504302 17.485215
    ## 6138  17.572814 19.815321
    ## 6139  21.838839 16.800731
    ## 6140  20.854848 19.431177
    ## 6141  16.514948 19.565941
    ## 6142  16.393232 14.846685
    ## 6143  24.488070 19.064258
    ## 6144  28.296984 19.909452
    ## 6145  19.012597 17.099732
    ## 6146  15.202859 17.661072
    ## 6147  23.071594 19.317200
    ## 6148  20.664877 17.933580
    ## 6149  22.979176 18.426636
    ## 6150  22.619023 18.061539
    ## 6151  26.412881 19.541395
    ## 6152  16.335335 18.771976
    ## 6153  17.412540 17.145779
    ## 6154  13.840052 16.485481
    ## 6155  19.991732 17.172621
    ## 6156  18.089607 19.222540
    ## 6157  19.587272 18.432145
    ## 6158  16.998935 18.474632
    ## 6159  17.591348 16.662836
    ## 6160  18.058948 15.024709
    ## 6161  22.916776 19.411231
    ## 6162  22.943370 19.264306
    ## 6163  21.746634 19.224585
    ## 6164  19.529407 16.897585
    ## 6165  23.934438 19.035915
    ## 6166  20.691975 18.526355
    ## 6167  21.290621 19.427555
    ## 6168  26.733769 19.683528
    ## 6169  14.095403 18.435927
    ## 6170  17.825955 19.139400
    ## 6171  18.890156 19.771154
    ## 6172  13.817025 16.405556
    ## 6173  17.928960 13.789106
    ## 6174  11.259192 17.073180
    ## 6175  20.425988 17.676115
    ## 6176  15.685587 15.876815
    ## 6177  19.225868 16.255358
    ## 6178  17.890550 18.061833
    ## 6179  22.741152 16.555982
    ## 6180  18.682730 17.714759
    ## 6181  21.902192 19.004207
    ## 6182  20.112132 14.445942
    ## 6183  24.683327 19.829815
    ## 6184  23.760901 19.602048
    ## 6185  19.882378 19.639380
    ## 6186  12.645326 18.312208
    ## 6187  24.375158 19.864492
    ## 6188  25.084830 19.428984
    ## 6189  15.873036 14.384859
    ## 6190  14.681835 18.699054
    ## 6191  22.767954 18.448071
    ## 6192  17.998232 18.010869
    ## 6193  18.087068 19.236981
    ## 6194  26.526736 18.159293
    ## 6195  14.274177 17.258067
    ## 6196  23.852526 19.571702
    ## 6197  23.552619 19.529220
    ## 6198  22.127684 16.953682
    ## 6199  18.066836 17.503322
    ## 6200  18.193346 17.196736
    ## 6201  22.174456 16.720602
    ## 6202  27.121570 19.755090
    ## 6203  20.646394 18.119590
    ## 6204  21.915160 18.420387
    ## 6205  20.456668 16.846059
    ## 6206  21.722869 18.310294
    ## 6207  22.747725 16.998258
    ## 6208  24.089487 18.905125
    ## 6209  15.470379 15.375259
    ## 6210  19.264951 18.777203
    ## 6211  16.362411 17.151380
    ## 6212  19.015055 19.937908
    ## 6213  23.453662 19.713617
    ## 6214  17.079026 17.782479
    ## 6215  17.017392 16.250244
    ## 6216  26.705302 19.987411
    ## 6217  22.818166 18.543093
    ## 6218  16.442544 18.539334
    ## 6219  22.071747 19.145763
    ## 6220  20.633847 19.982549
    ## 6221  22.998815 17.902920
    ## 6222  15.830085 16.524309
    ## 6223  17.797108 17.376029
    ## 6224  21.475426 17.499804
    ## 6225  23.589068 18.304267
    ## 6226  26.564748 18.082402
    ## 6227  25.650829 19.387262
    ## 6228  17.355604 19.040363
    ## 6229  25.889389 19.462636
    ## 6230  28.530089 19.148111
    ## 6231  20.382660 17.166225
    ## 6232  25.971100 18.069490
    ## 6233  20.034066 19.695678
    ## 6234  11.971291 15.632105
    ## 6235  18.162643 17.093512
    ## 6236  18.952728 16.000529
    ## 6237  15.865394 18.060371
    ## 6238  19.787195 17.315374
    ## 6239  17.431642 15.007230
    ## 6240  23.196146 18.947074
    ## 6241  18.438742 17.900210
    ## 6242  26.156249 18.687716
    ## 6243  20.029642 19.566592
    ## 6244  22.517403 17.634760
    ## 6245  22.338333 16.428831
    ## 6246  15.900455 18.477599
    ## 6247  18.836045 15.585221
    ## 6248  24.742683 19.591356
    ## 6249  19.123033 19.129437
    ## 6250  23.016369 19.454321
    ## 6251  14.044082 13.585985
    ## 6252  18.507528 18.702811
    ## 6253  19.688658 18.139685
    ## 6254  17.994951 18.747115
    ## 6255  15.092425 17.270837
    ## 6256  18.195499 19.327835
    ## 6257  17.890210 17.459845
    ## 6258  20.165264 19.031928
    ## 6259  19.085389 16.125214
    ## 6260  15.735896 14.963385
    ## 6261  21.414930 19.962378
    ## 6262  20.866193 19.576899
    ## 6263  24.551598 19.932875
    ## 6264  17.660278 18.111673
    ## 6265  27.538976 17.873734
    ## 6266  21.329940 19.737440
    ## 6267  19.895482 19.048275
    ## 6268  20.660878 16.807685
    ## 6269  22.968256 19.871630
    ## 6270  13.847970 18.907796
    ## 6271  18.088907 19.882828
    ## 6272  26.572657 19.539740
    ## 6273  17.615438 18.968364
    ## 6274  23.819489 19.105070
    ## 6275  13.790876 16.776519
    ## 6276  22.737609 18.752723
    ## 6277  17.831475 17.983334
    ## 6278  25.733311 18.271511
    ## 6279  21.629394 18.347684
    ## 6280  16.978998 19.398608
    ## 6281  16.584864 19.728675
    ## 6282  17.791347 16.444494
    ## 6283  17.845233 17.375145
    ## 6284  21.953219 19.369813
    ## 6285  26.652859 17.974191
    ## 6286  20.521156 18.267818
    ## 6287  23.204853 18.749740
    ## 6288  19.832746 19.701272
    ## 6289  22.724618 19.851617
    ## 6290  17.144535 19.313083
    ## 6291  16.260282 15.971749
    ## 6292  13.846867 17.964041
    ## 6293  18.807135 15.957840
    ## 6294  14.773723 17.181205
    ## 6295  18.295804 14.677962
    ## 6296  19.438353 14.780424
    ## 6297  24.709259 19.227750
    ## 6298  27.286178 18.658009
    ## 6299  13.881887 19.743416
    ## 6300  24.547217 19.801087
    ## 6301  16.991681 19.284956
    ## 6302  13.844989 16.010791
    ## 6303  17.668958 17.679209
    ## 6304  18.580428 15.123205
    ## 6305  20.593251 16.909764
    ## 6306  14.407692 18.982170
    ## 6307  19.391844 17.818799
    ## 6308  25.001143 19.832333
    ## 6309  21.333668 19.244568
    ## 6310  19.113251 17.390898
    ## 6311  16.790156 18.208637
    ## 6312  12.535465 17.240536
    ## 6313  22.759180 17.975901
    ## 6314  17.474594 14.862344
    ## 6315  20.868114 18.112041
    ## 6316  20.651680 18.708902
    ## 6317  12.574356 15.408088
    ## 6318  18.308822 17.076837
    ## 6319  23.827798 17.700198
    ## 6320  25.020506 19.255548
    ## 6321  20.453972 19.734232
    ## 6322  17.323117 15.342001
    ## 6323  20.498424 19.479627
    ## 6324  17.511530 17.180704
    ## 6325  24.850114 19.941451
    ## 6326  23.055379 19.162195
    ## 6327  22.869999 19.269182
    ## 6328  25.429820 18.478796
    ## 6329  25.813630 19.864664
    ## 6330  17.054246 18.212180
    ## 6331  24.182835 19.856021
    ## 6332  22.972241 19.740031
    ## 6333  27.479093 19.202317
    ## 6334  18.707596 15.625469
    ## 6335  22.700336 18.786836
    ## 6336  23.044532 19.554578
    ## 6337  17.244086 17.798237
    ## 6338  24.919153 19.055086
    ## 6339  20.621821 19.993102
    ## 6340  24.363538 19.809553
    ## 6341  23.693098 19.639908
    ## 6342  18.459656 17.775641
    ## 6343  18.915810 18.392773
    ## 6344  19.546777 19.768144
    ## 6345  17.265967 17.187521
    ## 6346  22.710154 19.592272
    ## 6347  17.507334 16.409496
    ## 6348  21.795903 16.018956
    ## 6349  17.664153 15.945308
    ## 6350  20.958891 16.870292
    ## 6351  19.543899 18.979113
    ## 6352  21.544018 18.680106
    ## 6353  21.183682 18.955132
    ## 6354  20.417177 19.508739
    ## 6355  16.180747 16.476943
    ## 6356  17.309848 17.470700
    ## 6357  17.311902 19.121985
    ## 6358  20.388915 18.755919
    ## 6359  18.059723 17.761611
    ## 6360  25.802854 19.184936
    ## 6361  19.301440 19.030427
    ## 6362  17.458841 19.995842
    ## 6363  16.812473 18.085463
    ## 6364  19.371671 18.561754
    ## 6365  19.311414 16.694255
    ## 6366  14.903767 15.119789
    ## 6367  19.421166 19.957569
    ## 6368  18.918748 18.645021
    ## 6369  11.842715 19.624534
    ## 6370  20.729425 19.986881
    ## 6371  27.438389 19.614734
    ## 6372  22.423701 19.846523
    ## 6373  21.901724 19.207348
    ## 6374  14.429157 17.002756
    ## 6375  18.486832 19.309098
    ## 6376  15.261968 13.975249
    ## 6377  17.367748 18.370514
    ## 6378  26.661491 19.704086
    ## 6379  15.403956 17.653273
    ## 6380  20.477362 15.402906
    ## 6381  17.945461 15.491184
    ## 6382  20.121974 19.859366
    ## 6383  19.390803 17.404067
    ## 6384  22.228681 19.275891
    ## 6385  17.849189 16.296075
    ## 6386  16.573049 18.031318
    ## 6387  23.100826 17.782314
    ## 6388  23.481951 19.046758
    ## 6389  16.684511 19.617815
    ## 6390  19.957733 19.254731
    ## 6391  13.159323 17.960604
    ## 6392  16.283778 18.938274
    ## 6393  17.211644 18.648688
    ## 6394  17.537633 16.093000
    ## 6395  19.689522 17.758853
    ## 6396  18.617357 19.105452
    ## 6397  18.016556 18.864769
    ## 6398  16.059328 19.631172
    ## 6399  16.977744 16.021888
    ## 6400  22.841140 18.124008
    ## 6401  20.779870 18.732821
    ## 6402  21.428690 18.072576
    ## 6403  20.400391 15.893073
    ## 6404  18.412882 16.140884
    ## 6405  19.992059 19.627563
    ## 6406  21.300001 18.390068
    ## 6407  20.663191 19.746511
    ## 6408  21.953915 16.741256
    ## 6409  20.793943 17.630864
    ## 6410  19.741497 16.174562
    ## 6411  16.939127 18.889291
    ## 6412  18.187605 18.354060
    ## 6413  16.954763 16.974406
    ## 6414  18.994938 18.948356
    ## 6415  19.947202 18.868622
    ## 6416  22.082866 19.541833
    ## 6417  22.253425 18.159738
    ## 6418  21.800683 18.650414
    ## 6419  23.753740 19.478625
    ## 6420  17.187503 18.587809
    ## 6421  22.224303 17.234127
    ## 6422  18.808402 14.785658
    ## 6423  20.124915 19.612190
    ## 6424  20.220860 17.905861
    ## 6425  16.202529 14.624425
    ## 6426  21.124034 18.498844
    ## 6427  22.500132 19.095157
    ## 6428  16.750967 19.183923
    ## 6429  19.463142 16.385198
    ## 6430  16.726411 19.998524
    ## 6431  21.523943 18.465306
    ## 6432  17.982299 19.721911
    ## 6433  18.490261 17.837700
    ## 6434  19.505646 16.938799
    ## 6435  18.284868 19.929531
    ## 6436  20.596877 17.765873
    ## 6437  20.926082 19.848671
    ## 6438  15.345083 19.979702
    ## 6439  22.715738 18.466366
    ## 6440  20.033208 17.964835
    ## 6441  23.413169 17.845290
    ## 6442  20.711481 16.337157
    ## 6443  23.458654 18.086364
    ## 6444  21.571209 19.769672
    ## 6445  10.764639 13.526472
    ## 6446  18.042447 16.002702
    ## 6447  21.774716 19.751638
    ## 6448  17.091428 19.917091
    ## 6449  20.075128 19.868604
    ## 6450  20.793676 16.838815
    ## 6451  18.693759 16.048563
    ## 6452  22.478163 19.140187
    ## 6453  15.918310 17.239731
    ## 6454  23.506242 18.987192
    ## 6455  19.223272 19.038183
    ## 6456  16.871417 17.684860
    ## 6457  18.744287 19.625674
    ## 6458  20.233594 16.786202
    ## 6459  11.325702 11.355374
    ## 6460  18.995694 19.059166
    ## 6461  15.788755 17.605938
    ## 6462  18.692666 19.769863
    ## 6463  24.121518 18.258986
    ## 6464  22.276129 18.306785
    ## 6465  20.210220 15.960663
    ## 6466  18.217058 19.768374
    ## 6467  20.460639 19.402526
    ## 6468  15.939644 19.697353
    ## 6469  21.528147 18.398180
    ## 6470  17.911287 19.133531
    ## 6471  20.217734 18.430502
    ## 6472  18.926107 18.614979
    ## 6473  21.333903 18.061214
    ## 6474  20.911940 18.376091
    ## 6475  16.317619 18.005815
    ## 6476  24.700351 19.323116
    ## 6477  23.152427 18.727020
    ## 6478  22.262647 18.484056
    ## 6479  24.032098 19.354294
    ## 6480  16.913820 18.080563
    ## 6481  20.704793 19.500023
    ## 6482  13.330155 13.632443
    ## 6483  23.002464 19.531610
    ## 6484  22.284866 19.513863
    ## 6485  22.704083 19.801398
    ## 6486  21.455058 18.141917
    ## 6487  20.599006 19.480849
    ## 6488  16.633746 16.351264
    ## 6489  22.037819 19.555026
    ## 6490  24.664150 19.823719
    ## 6491  21.575843 16.842224
    ## 6492  20.417601 17.819800
    ## 6493  19.533262 19.061636
    ## 6494  22.230361 19.814830
    ## 6495  20.215453 17.007117
    ## 6496  21.166010 19.410402
    ## 6497  25.505620 19.797709
    ## 6498  19.722445 18.362184
    ## 6499  27.197039 17.731012
    ## 6500  22.302551 19.915065
    ## 6501   9.156393 12.760472
    ## 6502  24.647705 18.076439
    ## 6503  17.821271 19.098405
    ## 6504  21.106888 19.750282
    ## 6505  21.246051 18.195017
    ## 6506  20.727392 18.028953
    ## 6507  15.170332 16.315033
    ## 6508  24.255849 17.011321
    ## 6509  17.629258 19.096215
    ## 6510  19.770016 18.596543
    ## 6511  20.755714 17.771660
    ## 6512  18.022414 19.826851
    ## 6513  20.985365 18.466535
    ## 6514  20.573407 17.151206
    ## 6515  16.431476 17.056878
    ## 6516  21.686031 19.928918
    ## 6517  23.330491 17.592056
    ## 6518  19.831175 19.240378
    ## 6519  21.328204 19.753074
    ## 6520  20.210595 19.943192
    ## 6521  20.779361 17.012549
    ## 6522  21.829230 19.261857
    ## 6523  22.282676 19.642948
    ## 6524  24.537039 18.307804
    ## 6525  19.087934 19.295753
    ## 6526  19.002476 19.578231
    ## 6527  24.590619 19.788411
    ## 6528  22.398025 19.734490
    ## 6529  21.244062 18.100848
    ## 6530  23.130585 19.081013
    ## 6531  19.528933 17.801702
    ## 6532  20.198080 19.994434
    ## 6533  17.101510 17.940792
    ## 6534  20.212172 18.642927
    ## 6535  22.920601 19.932129
    ## 6536  12.946100 17.131001
    ## 6537  16.166791 19.409459
    ## 6538  22.558712 19.592668
    ## 6539  21.318783 18.848622
    ## 6540  24.801920 18.522706
    ## 6541  26.181187 19.302427
    ## 6542  22.196566 18.989620
    ## 6543  15.495585 15.967456
    ## 6544  19.346652 18.551410
    ## 6545  15.608769 16.749263
    ## 6546  26.229558 19.018705
    ## 6547  25.566364 18.696239
    ## 6548  19.245063 19.539777
    ## 6549  19.168164 17.298510
    ## 6550  12.583963 11.722259
    ## 6551  15.915698 14.226566
    ## 6552  21.969523 18.621726
    ## 6553  17.506997 19.490119
    ## 6554  25.065761 18.705913
    ## 6555  13.941321 16.984486
    ## 6556   8.748189  7.995372
    ## 6557  16.195413 19.639299
    ## 6558  23.935408 18.669024
    ## 6559  22.242687 18.660506
    ## 6560  16.689529 19.138284
    ## 6561  21.897014 19.707434
    ## 6562  23.543186 18.185548
    ## 6563  17.091490 19.832626
    ## 6564  19.209713 19.784252
    ## 6565  20.032154 18.387830
    ## 6566  19.297340 19.273090
    ## 6567  24.681686 18.732745
    ## 6568  31.450118 19.974572
    ## 6569  12.845836 10.977436
    ## 6570  20.766716 19.536727
    ## 6571  19.135208 14.649795
    ## 6572  20.293969 16.650908
    ## 6573  16.417804 18.001185
    ## 6574  21.592566 17.876271
    ## 6575  13.578838 18.703584
    ## 6576  20.462408 19.201144
    ## 6577  20.420550 17.676197
    ## 6578  20.962129 18.938117
    ## 6579  21.042361 19.477621
    ## 6580  21.266589 18.011630
    ## 6581  18.657754 19.396603
    ## 6582  21.612858 19.161246
    ## 6583  22.914616 18.600549
    ## 6584  18.414400 17.296461
    ## 6585  19.149643 16.053428
    ## 6586  16.161099 19.695954
    ## 6587  17.572142 17.514191
    ## 6588  19.455240 15.437492
    ## 6589  19.147861 19.994113
    ## 6590  16.904172 19.318539
    ## 6591  17.365805 19.797254
    ## 6592  20.036023 19.659574
    ## 6593  15.254906 16.546107
    ## 6594  22.014484 19.798237
    ## 6595  23.225909 19.883040
    ## 6596  19.422511 17.681717
    ## 6597  17.216668 16.795999
    ## 6598  20.594999 18.796829
    ## 6599  16.658133 16.946275
    ## 6600  22.958538 19.994659
    ## 6601  15.088642 14.769628
    ## 6602  17.613468 18.812408
    ## 6603  23.159387 19.944681
    ## 6604  23.034584 17.803224
    ## 6605  20.112357 18.658763
    ## 6606  19.684134 18.986386
    ## 6607  22.507945 19.459109
    ## 6608  20.250083 18.859816
    ## 6609  21.149282 16.938579
    ## 6610  18.876012 18.545225
    ## 6611  14.518828 18.788678
    ## 6612  21.123586 19.768913
    ## 6613  14.826149 16.851644
    ## 6614  14.725005 16.631475
    ## 6615  24.177187 19.047182
    ## 6616  18.769761 19.283421
    ## 6617  19.882725 18.096344
    ## 6618  14.264764 16.939334
    ## 6619  14.900774 15.268782
    ## 6620  25.677505 19.543968
    ## 6621  21.109934 18.444620
    ## 6622  21.661737 19.044297
    ## 6623  12.255871 14.863477
    ## 6624  14.359655 16.541060
    ## 6625  23.817813 19.709282
    ## 6626  25.251148 19.355853
    ## 6627  23.876543 17.918571
    ## 6628  22.793495 17.107523
    ## 6629  20.727248 18.286040
    ## 6630  15.739460 16.186290
    ## 6631  19.990491 18.172794
    ## 6632  16.448852 19.996185
    ## 6633  16.096139 18.449472
    ## 6634  20.675704 19.591227
    ## 6635  21.598647 18.065052
    ## 6636  13.575812 19.436932
    ## 6637  18.879435 19.361280
    ## 6638  17.603335 18.768092
    ## 6639  19.818163 19.896532
    ## 6640  14.661520 14.589169
    ## 6641  17.201016 17.509012
    ## 6642  17.447682 17.092357
    ## 6643  24.671773 19.454734
    ## 6644  18.919265 18.817573
    ## 6645  13.023424 17.489090
    ## 6646  12.471680 17.678277
    ## 6647  21.807352 15.933604
    ## 6648  15.211515 17.116685
    ## 6649  15.484533 19.636454
    ## 6650  16.635058 16.685024
    ## 6651  13.010526 19.005983
    ## 6652  16.310862 15.959436
    ## 6653  24.987312 19.219514
    ## 6654  26.837713 19.874373
    ## 6655  19.226736 19.669820
    ## 6656  22.910408 17.141973
    ## 6657  22.653581 19.226800
    ## 6658  22.122620 15.832109
    ## 6659  14.709838 15.516226
    ## 6660  24.939375 17.690360
    ## 6661  17.947823 16.301956
    ## 6662  20.615644 19.886165
    ## 6663  13.585654 19.206375
    ## 6664  20.182507 19.560631
    ## 6665  10.370839 13.171141
    ## 6666  20.135234 19.369819
    ## 6667  14.887126 19.100127
    ## 6668  20.990833 19.853865
    ## 6669  15.391872 16.350655
    ## 6670  22.409254 17.307181
    ## 6671  10.912909 10.389835
    ## 6672  13.296208 14.029943
    ## 6673  25.480055 18.636342
    ## 6674  13.913438 14.855713
    ## 6675  13.694063 16.017319
    ## 6676  17.693899 16.546558
    ## 6677  22.893030 18.964407
    ## 6678  25.051536 19.025396
    ## 6679  14.662836 13.398281
    ## 6680  25.325319 18.873537
    ## 6681  14.757172 15.784542
    ## 6682  23.552461 19.135702
    ## 6683  23.379773 17.938077
    ## 6684  18.111745 19.025016
    ## 6685  15.041878 17.498725
    ## 6686  16.339649 17.336132
    ## 6687  25.318551 19.162719
    ## 6688  18.334737 16.580939
    ## 6689  28.031514 19.836675
    ## 6690  19.901193 17.767777
    ## 6691  20.271787 19.591741
    ## 6692  16.696012 11.132882
    ## 6693  17.791018 19.092960
    ## 6694  16.199841 16.414468
    ## 6695  26.053659 19.930531
    ## 6696  18.680100 19.542473
    ## 6697  14.434669 17.465659
    ## 6698  24.015750 18.878129
    ## 6699  25.243784 17.283402
    ## 6700  20.607259 19.734487
    ## 6701  23.960371 18.155921
    ## 6702  15.859073 14.385272
    ## 6703  17.356068 18.797254
    ## 6704  23.621048 18.936705
    ## 6705  14.830318 17.559052
    ## 6706  17.886787 18.876052
    ## 6707  18.605336 16.940628
    ## 6708  20.873235 17.841181
    ## 6709  18.078370 18.031842
    ## 6710  23.932935 18.554027
    ## 6711  25.940186 19.767927
    ## 6712  17.358521 18.397952
    ## 6713  22.221108 19.673994
    ## 6714  19.138215 17.401580
    ## 6715  19.942186 18.253192
    ## 6716  22.684189 17.454926
    ## 6717  26.063177 19.486273
    ## 6718  19.919243 17.939317
    ## 6719  16.261601 18.550829
    ## 6720  18.745005 18.707382
    ## 6721  19.637893 17.571254
    ## 6722  15.767733 19.840718
    ## 6723  14.316295 15.126616
    ## 6724  16.888482 16.256501
    ## 6725  17.146851 15.062295
    ## 6726  20.754035 19.908849
    ## 6727  21.219501 16.492868
    ## 6728  22.548380 15.964762
    ## 6729  17.792661 19.783997
    ## 6730  20.276334 17.345880
    ## 6731  19.646836 17.386568
    ## 6732  19.355343 15.167273
    ## 6733  17.364979 15.424572
    ## 6734  19.597820 19.403354
    ## 6735  21.638847 17.426212
    ## 6736  17.718849 19.375004
    ## 6737  21.332679 19.715684
    ## 6738  22.671944 18.404066
    ## 6739  26.447262 18.833606
    ## 6740  16.847340 19.202723
    ## 6741  17.300097 18.301204
    ## 6742  11.519668 10.934512
    ## 6743  20.816476 19.862427
    ## 6744  18.367528 17.458946
    ## 6745  17.883416 16.141504
    ## 6746  17.141970 15.530955
    ## 6747  15.937385 15.051807
    ## 6748  14.889684 16.994573
    ## 6749  17.024554 19.707388
    ## 6750  20.474005 18.042239
    ## 6751  18.490426 17.288515
    ## 6752  21.484485 18.856739
    ## 6753  18.083781 16.450567
    ## 6754  15.376353 16.307524
    ## 6755  23.359867 19.805401
    ## 6756  23.304253 19.529966
    ## 6757  24.273877 18.973618
    ## 6758  15.986937 14.733786
    ## 6759  19.414779 19.070397
    ## 6760  29.001248 19.831416
    ## 6761  18.189006 17.486761
    ## 6762  19.339240 18.113382
    ## 6763  18.186289 16.460157
    ## 6764  18.681988 15.485949
    ## 6765  28.338057 19.258301
    ## 6766  16.455789 16.703136
    ## 6767  17.738427 17.716029
    ## 6768  20.057069 19.215806
    ## 6769  20.219047 19.772858
    ## 6770  18.512165 18.727218
    ## 6771  18.300826 19.205905
    ## 6772  24.422178 19.944845
    ## 6773  16.611099 17.847348
    ## 6774  22.693763 19.587758
    ## 6775  18.954415 19.690767
    ## 6776  20.260222 16.631377
    ## 6777  22.532422 19.400222
    ## 6778  17.590597 18.119733
    ## 6779  17.459251 18.146717
    ## 6780  15.549974 15.170828
    ## 6781  15.507707 18.543073
    ## 6782  19.376901 16.762339
    ## 6783  16.829593 19.638920
    ## 6784  18.638522 19.385778
    ## 6785  20.637826 19.895944
    ## 6786  21.289536 19.757216
    ## 6787  21.726885 19.467340
    ## 6788  19.165559 19.205725
    ## 6789  21.680196 19.504033
    ## 6790  17.946614 18.150830
    ## 6791  19.720088 19.443651
    ## 6792  21.459765 19.623195
    ## 6793  20.557337 18.591860
    ## 6794  20.906404 18.408957
    ## 6795  27.622008 19.806138
    ## 6796  11.069320 11.016582
    ## 6797  22.346553 17.800362
    ## 6798  17.809058 15.415399
    ## 6799  19.590980 19.112429
    ## 6800  17.312888 17.469793
    ## 6801  18.939682 18.145346
    ## 6802  22.765173 19.981798
    ## 6803  12.049124 14.814663
    ## 6804  23.121835 19.735214
    ## 6805  22.387965 19.213696
    ## 6806  23.041970 19.784468
    ## 6807  29.181101 19.660780
    ## 6808  21.513819 19.135785
    ## 6809  23.822245 18.068944
    ## 6810  17.039599 16.718956
    ## 6811  15.440459 17.089481
    ## 6812  23.670695 18.224498
    ## 6813  18.954921 16.986871
    ## 6814  19.603443 17.772683
    ## 6815  18.495927 17.291043
    ## 6816  16.743279 19.058632
    ## 6817  14.003428 18.686191
    ## 6818  20.446273 19.058309
    ## 6819  20.238227 17.631585
    ## 6820  15.123124 17.921549
    ## 6821  26.195909 19.923835
    ## 6822  23.416286 19.307455
    ## 6823  17.960884 19.109478
    ## 6824  23.417968 17.737207
    ## 6825  12.557966 10.963424
    ## 6826  19.628206 19.492918
    ## 6827  15.536327 17.734124
    ## 6828  16.935065 15.776769
    ## 6829  22.873421 19.323046
    ## 6830  20.079105 16.881848
    ## 6831  22.542349 18.330109
    ## 6832  25.998045 19.708149
    ## 6833  17.748314 17.322638
    ## 6834  26.905436 19.525619
    ## 6835  15.993988 17.412388
    ## 6836  19.647644 19.890489
    ## 6837  19.666775 19.839426
    ## 6838  17.969628 19.924005
    ## 6839  20.093294 19.850462
    ## 6840  19.068032 17.522368
    ## 6841  24.230442 18.948785
    ## 6842  21.504862 17.411440
    ## 6843  19.833218 19.903655
    ## 6844  21.833989 17.631839
    ## 6845  18.275456 19.913769
    ## 6846  24.560952 19.727339
    ## 6847  19.146404 17.610547
    ## 6848  21.806725 19.534260
    ## 6849  22.277807 19.267339
    ## 6850  20.945036 19.760399
    ## 6851  20.890870 17.945950
    ## 6852  25.025107 19.716892
    ## 6853  26.296736 19.694449
    ## 6854  23.621993 19.399334
    ## 6855  20.691021 19.487926
    ## 6856   9.773001  9.502387
    ## 6857  16.097018 17.532432
    ## 6858  19.596234 19.568366
    ## 6859  22.170659 18.881584
    ## 6860  23.420945 19.507978
    ## 6861  23.158468 19.057576
    ## 6862  15.951550 14.943266
    ## 6863  20.558872 17.414982
    ## 6864  20.690180 18.033672
    ## 6865  20.089173 18.482821
    ## 6866  17.855160 19.093412
    ## 6867  24.937587 19.916301
    ## 6868  21.642088 18.371873
    ## 6869  18.277533 16.443631
    ## 6870  26.103302 18.957195
    ## 6871  11.808411 13.028235
    ## 6872  15.863431 19.635308
    ## 6873  17.541479 17.653852
    ## 6874  26.018707 18.462593
    ## 6875  23.530867 19.567299
    ## 6876  22.267014 19.253574
    ## 6877  18.144034 19.892336
    ## 6878  10.904244 14.261362
    ## 6879  21.729261 19.950632
    ## 6880  18.873600 17.783708
    ## 6881  20.518012 18.097721
    ## 6882  20.174705 17.201161
    ## 6883  25.450236 17.421170
    ## 6884  22.975170 18.297367
    ## 6885  16.618270 19.667419
    ## 6886  20.241186 18.110873
    ## 6887  19.124033 16.214230
    ## 6888  20.975274 18.042507
    ## 6889  24.796545 19.242982
    ## 6890  18.466037 17.008425
    ## 6891  15.753027 18.677143
    ## 6892  17.042036 16.253156
    ## 6893  23.774571 18.670068
    ## 6894  16.219558 15.752677
    ## 6895  22.734400 18.677833
    ## 6896  17.562583 17.692091
    ## 6897  21.583126 19.655276
    ## 6898  15.556466 16.717049
    ## 6899  19.803923 19.816611
    ## 6900  18.293949 19.262848
    ## 6901  22.130652 18.950726
    ## 6902  17.000906 19.713466
    ## 6903  22.815857 19.763799
    ## 6904  17.803496 17.801238
    ## 6905  18.749101 18.960347
    ## 6906  18.078030 18.643745
    ## 6907  21.116631 19.132878
    ## 6908  26.998895 18.791410
    ## 6909  14.774837 16.439368
    ## 6910  26.247908 18.227018
    ## 6911  15.504438 13.982996
    ## 6912  15.899079 17.751912
    ## 6913  19.598171 17.889785
    ## 6914  24.828426 19.778052
    ## 6915  21.618485 19.762065
    ## 6916  21.452411 19.075122
    ## 6917  23.916485 18.183414
    ## 6918  16.076957 13.097179
    ## 6919  23.023323 19.822790
    ## 6920  16.505002 14.681715
    ## 6921  15.609489 18.640941
    ## 6922  26.163298 17.789702
    ## 6923  19.223531 19.835056
    ## 6924  18.430159 16.259272
    ## 6925  17.758261 17.103559
    ## 6926  19.971858 19.896290
    ## 6927  27.116620 19.248117
    ## 6928  22.080985 18.381591
    ## 6929  18.083461 18.723970
    ## 6930  15.993359 17.122190
    ## 6931  30.163871 19.278755
    ## 6932  27.392208 18.861280
    ## 6933  20.234405 19.164047
    ## 6934  17.683188 17.107274
    ## 6935  15.632669 19.399833
    ## 6936  17.825246 15.127256
    ## 6937  23.900686 19.904874
    ## 6938  21.897208 17.586707
    ## 6939  17.893479 19.457341
    ## 6940  20.988598 16.652643
    ## 6941  28.913765 19.519169
    ## 6942  20.628521 17.636600
    ## 6943  23.981387 19.921953
    ## 6944  16.743837 18.193156
    ## 6945  22.019751 19.494497
    ## 6946  28.825661 19.684668
    ## 6947  26.103581 19.084195
    ## 6948  17.235805 17.860594
    ## 6949  24.711408 18.707453
    ## 6950  14.882169 19.327055
    ## 6951  20.767109 19.370552
    ## 6952  19.448876 19.622786
    ## 6953  20.708141 18.844202
    ## 6954  21.587637 19.907269
    ## 6955  29.367195 19.775973
    ## 6956  23.437719 18.712428
    ## 6957  19.049735 18.924856
    ## 6958  21.892303 18.122319
    ## 6959  18.556506 18.302098
    ## 6960  15.683630 19.969650
    ## 6961  25.159575 19.586132
    ## 6962  17.786558 17.966301
    ## 6963  23.108844 19.547461
    ## 6964  20.651590 19.549997
    ## 6965  19.198644 16.536438
    ## 6966  19.953401 19.323798
    ## 6967  15.353262 19.987795
    ## 6968  18.918731 15.213340
    ## 6969  13.477473 19.319205
    ## 6970  23.352519 18.894414
    ## 6971  23.460203 19.676713
    ## 6972  16.907676 19.374964
    ## 6973  20.864978 19.003800
    ## 6974  22.480287 19.634616
    ## 6975  25.459157 19.896883
    ## 6976  16.454764 16.268724
    ## 6977  20.971012 18.785887
    ## 6978  16.087949 18.331376
    ## 6979  13.436616 16.558846
    ## 6980  20.948573 19.393146
    ## 6981  20.527692 17.750280
    ## 6982  18.734147 17.743997
    ## 6983  14.857249 19.788895
    ## 6984  17.318836 19.638994
    ## 6985  20.547977 19.172980
    ## 6986  24.273191 19.123012
    ## 6987  18.480767 18.683651
    ## 6988  19.569789 19.811638
    ## 6989  18.601064 16.330126
    ## 6990  15.985376 18.211451
    ## 6991  23.527006 18.424156
    ## 6992  17.162531 19.821125
    ## 6993  20.159819 19.494540
    ## 6994  19.849564 17.146518
    ## 6995  23.259469 19.667033
    ## 6996  14.309169 12.242522
    ## 6997  19.397444 16.178678
    ## 6998  20.949029 18.417332
    ## 6999  21.741784 18.340891
    ## 7000  17.755768 12.826159
    ## 7001  19.029720 18.541867
    ## 7002  17.761861 18.364462
    ## 7003  21.322338 19.984119
    ## 7004  15.387294 19.889002
    ## 7005  21.620914 19.006507
    ## 7006  22.783172 19.571612
    ## 7007  13.821579 16.761796
    ## 7008  16.890377 15.805712
    ## 7009  18.390196 19.891545
    ## 7010  18.290686 16.003921
    ## 7011  14.820893 15.433416
    ## 7012  20.910076 19.569906
    ## 7013  27.311379 18.874105
    ## 7014  15.071657 18.502251
    ## 7015  20.256333 19.486100
    ## 7016  20.604716 16.893080
    ## 7017  17.710485 18.062800
    ## 7018  18.954867 19.644564
    ## 7019  24.472659 19.543183
    ## 7020  22.078235 19.250341
    ## 7021  18.788853 17.524384
    ## 7022  16.114537 18.229572
    ## 7023   9.306313 10.801660
    ## 7024  27.308217 18.295528
    ## 7025  21.231417 19.546743
    ## 7026  15.589009 14.116640
    ## 7027  16.610799 16.801901
    ## 7028  27.384156 19.141499
    ## 7029  27.099155 19.142217
    ## 7030  16.541690 19.650422
    ## 7031  18.782876 19.606862
    ## 7032  22.925778 18.812364
    ## 7033  18.887670 19.353602
    ## 7034  19.754575 19.493014
    ## 7035  25.918137 19.709883
    ## 7036  22.192045 19.729698
    ## 7037  17.892535 16.759388
    ## 7038  24.130753 19.837594
    ## 7039  23.804468 19.144609
    ## 7040  11.203441 14.670094
    ## 7041  22.369354 17.318553
    ## 7042  18.241850 17.396345
    ## 7043  15.426692 15.222629
    ## 7044  22.409117 17.226374
    ## 7045  21.015194 18.847310
    ## 7046  24.340230 19.268821
    ## 7047  17.971538 16.683869
    ## 7048  20.859824 19.501768
    ## 7049  19.724626 16.864060
    ## 7050  21.412432 19.629676
    ## 7051  19.126335 14.700728
    ## 7052  20.397181 18.669042
    ## 7053  21.037099 19.703897
    ## 7054  23.169994 19.987468
    ## 7055  21.356658 19.354859
    ## 7056  14.527958 17.443064
    ## 7057  15.768531 18.788543
    ## 7058  24.387399 19.938449
    ## 7059  13.643521 14.680874
    ## 7060  17.868490 18.641879
    ## 7061  15.523178 13.712261
    ## 7062  15.757789 17.693081
    ## 7063  21.784652 17.490187
    ## 7064  24.882845 19.688382
    ## 7065  20.748971 15.296511
    ## 7066  16.511385 17.662960
    ## 7067  18.380668 13.993340
    ## 7068  11.809041 16.054444
    ## 7069  20.745868 19.722243
    ## 7070  19.650855 19.094977
    ## 7071  14.613143 15.834479
    ## 7072  20.269087 19.949273
    ## 7073  22.758314 19.973764
    ## 7074  20.326853 18.249961
    ## 7075  18.202563 17.051292
    ## 7076  21.677627 19.199098
    ## 7077  18.364448 16.368488
    ## 7078  15.417855 19.690972
    ## 7079  18.142815 18.917641
    ## 7080  23.260796 19.878235
    ## 7081  20.998928 19.083285
    ## 7082  16.012132 16.486115
    ## 7083  18.057514 18.852259
    ## 7084  15.837086 15.179757
    ## 7085  15.801600 15.294236
    ## 7086  17.497570 18.121328
    ## 7087  21.947753 17.059094
    ## 7088  20.031707 19.454708
    ## 7089  18.579381 19.175135
    ## 7090  17.403065 18.747567
    ## 7091  20.530404 15.868406
    ## 7092  12.284551 13.386702
    ## 7093  14.605381 18.185773
    ## 7094  20.886438 19.013037
    ## 7095  21.261203 19.132941
    ## 7096  15.596447 19.125246
    ## 7097  26.650039 19.219549
    ## 7098  22.876985 19.075459
    ## 7099  24.825401 19.524471
    ## 7100  19.564222 16.545507
    ## 7101  21.259392 18.668906
    ## 7102  20.670413 16.466637
    ## 7103  19.658444 18.501172
    ## 7104  17.355343 17.223406
    ## 7105  17.859703 15.537914
    ## 7106  16.472598 19.617178
    ## 7107  20.742302 18.612307
    ## 7108  23.197098 17.793724
    ## 7109  17.146702 18.029081
    ## 7110  18.922507 17.947743
    ## 7111  23.411552 19.954141
    ## 7112  21.391548 18.308394
    ## 7113  22.140113 18.954089
    ## 7114  19.798751 19.415865
    ## 7115  18.815203 13.015971
    ## 7116  26.305730 18.759227
    ## 7117  19.625014 16.260632
    ## 7118  19.352212 18.405219
    ## 7119  19.006348 18.528660
    ## 7120  23.592775 19.650358
    ## 7121  21.832716 19.928814
    ## 7122  22.832856 19.979368
    ## 7123  17.960881 18.059438
    ## 7124  16.918048 17.677800
    ## 7125  16.578327 19.425476
    ## 7126  20.897100 17.597425
    ## 7127  15.018317 15.108186
    ## 7128  21.339794 18.554788
    ## 7129  24.472114 19.363470
    ## 7130  20.549674 18.615250
    ## 7131  14.349944 19.113428
    ## 7132  24.973782 19.819166
    ## 7133  20.318747 18.774865
    ## 7134  22.346974 19.958679
    ## 7135  21.222023 18.664034
    ## 7136  20.033770 16.956406
    ## 7137  22.622454 18.480584
    ## 7138  15.758134 14.162357
    ## 7139  20.738899 18.497155
    ## 7140  26.076278 18.282439
    ## 7141  21.561299 18.107175
    ## 7142  13.701564 12.555944
    ## 7143  19.788779 17.268408
    ## 7144  21.920120 17.918640
    ## 7145  23.966006 19.868604
    ## 7146  21.154449 19.788069
    ## 7147  24.724728 18.234607
    ## 7148  24.347737 17.548938
    ## 7149  19.622399 19.336283
    ## 7150  21.437582 19.489034
    ## 7151  10.730413 10.745904
    ## 7152  20.911708 17.698925
    ## 7153  21.116718 18.222158
    ## 7154  19.251581 17.285466
    ## 7155  18.478267 19.567337
    ## 7156  22.645616 19.093331
    ## 7157  21.355173 19.599438
    ## 7158  18.318818 16.497486
    ## 7159  12.947206 14.504001
    ## 7160  18.725335 19.460129
    ## 7161  22.465605 18.785364
    ## 7162  16.869744 19.557909
    ## 7163  23.180355 17.021889
    ## 7164  18.583985 17.831941
    ## 7165  16.079876 15.504797
    ## 7166  24.189934 16.646670
    ## 7167  18.048413 19.136112
    ## 7168  16.107921 16.959456
    ## 7169  19.083938 19.268634
    ## 7170  19.501694 17.254128
    ## 7171  23.569482 16.656235
    ## 7172  12.735780 15.372288
    ## 7173  21.616874 17.877665
    ## 7174  24.735447 18.212432
    ## 7175  16.684005 16.254570
    ## 7176  20.542548 19.362110
    ## 7177  17.247531 17.962424
    ## 7178  19.254786 18.634682
    ## 7179  17.273524 17.753797
    ## 7180  22.723122 19.331228
    ## 7181  19.202358 19.140201
    ## 7182  15.279050 14.282800
    ## 7183  26.659717 19.726219
    ## 7184  19.792595 19.385725
    ## 7185  18.425518 19.393664
    ## 7186  19.028569 17.334115
    ## 7187  19.056256 16.610160
    ## 7188  21.300688 18.357083
    ## 7189  18.412893 19.356983
    ## 7190  21.241027 19.913330
    ## 7191  17.111321 18.871850
    ## 7192  17.541668 15.305717
    ## 7193  19.167880 18.791123
    ## 7194  26.255655 18.648795
    ## 7195  19.895598 18.368428
    ## 7196  18.562601 19.050376
    ## 7197  19.698635 18.816828
    ## 7198  16.779104 18.856126
    ## 7199  19.482577 18.312721
    ## 7200  14.553835 14.798709
    ## 7201  16.089246 18.215216
    ## 7202  19.913413 18.888444
    ## 7203  18.877005 18.487900
    ## 7204  16.995618 19.969126
    ## 7205  22.850091 19.818780
    ## 7206  13.086987 18.590057
    ## 7207  16.030720 16.305790
    ## 7208  19.451162 16.537661
    ## 7209  25.792600 17.314174
    ## 7210  20.797540 18.517382
    ## 7211  23.734073 19.746108
    ## 7212  18.416949 18.876117
    ## 7213  15.807449 15.483601
    ## 7214  17.404704 18.901460
    ## 7215  19.022732 19.873264
    ## 7216  23.566532 18.289899
    ## 7217  26.083362 19.721844
    ## 7218  20.706483 19.139160
    ## 7219  21.570271 19.306063
    ## 7220  17.330961 15.172067
    ## 7221  20.983742 18.268493
    ## 7222  23.405909 18.572790
    ## 7223  17.169878 17.347093
    ## 7224  18.715778 17.173316
    ## 7225  17.237589 16.015622
    ## 7226  11.655256 15.573868
    ## 7227  22.056219 19.877474
    ## 7228  17.887101 19.785335
    ## 7229  21.594581 17.858487
    ## 7230  24.585508 19.689586
    ## 7231  22.425355 17.444063
    ## 7232  18.627768 19.414122
    ## 7233  18.126968 19.108109
    ## 7234  22.683566 19.101110
    ## 7235  23.619325 16.902051
    ## 7236  18.338082 19.500085
    ## 7237  25.608230 19.667724
    ## 7238  19.827324 19.154357
    ## 7239  22.731819 18.272744
    ## 7240  17.208616 15.131051
    ## 7241  22.201934 18.458469
    ## 7242  20.317377 17.846246
    ## 7243  21.751163 19.881454
    ## 7244  16.016913 18.321350
    ## 7245  19.282080 17.357216
    ## 7246  16.390053 16.660548
    ## 7247  15.562471 16.306480
    ## 7248  21.035919 19.932582
    ## 7249  18.809796 18.356430
    ## 7250  23.214704 18.039305
    ## 7251  15.584305 18.052635
    ## 7252  19.341592 19.095168
    ## 7253  22.547243 19.418081
    ## 7254  19.682826 19.009525
    ## 7255  24.570296 19.039160
    ## 7256  18.870351 17.041641
    ## 7257  19.975635 14.391517
    ## 7258  24.535283 18.429631
    ## 7259  14.529018 17.905987
    ## 7260  15.214862 14.885085
    ## 7261  24.617283 18.095605
    ## 7262  22.886681 19.632321
    ## 7263  20.555067 19.567030
    ## 7264  25.462072 19.263961
    ## 7265  19.248557 18.997077
    ## 7266  22.082834 16.578545
    ## 7267  13.971187 19.773233
    ## 7268  21.939145 19.458799
    ## 7269  13.983613 13.597270
    ## 7270  18.562705 16.162766
    ## 7271  25.164764 18.588620
    ## 7272  23.230745 19.032339
    ## 7273  13.719492 18.505653
    ## 7274  16.699279 18.257173
    ## 7275  20.261800 19.635731
    ## 7276  20.937613 19.700813
    ## 7277  18.529363 19.929805
    ## 7278  18.192276 17.459248
    ## 7279  22.817389 19.487620
    ## 7280  12.971279 12.303770
    ## 7281  17.105098 19.050114
    ## 7282  16.558019 16.981729
    ## 7283  19.875236 17.150676
    ## 7284  23.825632 19.860037
    ## 7285  29.423892 19.742075
    ## 7286  21.395533 19.826586
    ## 7287  25.865483 19.377955
    ## 7288  22.985612 19.076092
    ## 7289  19.627724 18.998874
    ## 7290  20.054310 18.408950
    ## 7291  24.311964 19.697314
    ## 7292  23.171037 19.967439
    ## 7293  16.026806 16.501383
    ## 7294  30.007465 19.981522
    ## 7295  21.169679 18.677784
    ## 7296  13.409686 18.617099
    ## 7297  21.221136 19.975795
    ## 7298  15.806532 15.217615
    ## 7299  21.393687 19.927584
    ## 7300  14.574799 12.354236
    ## 7301  16.288920 14.652541
    ## 7302  24.388923 17.127704
    ## 7303  19.922501 17.190673
    ## 7304  26.347658 19.291901
    ## 7305  19.542132 16.206427
    ## 7306  19.508196 15.835848
    ## 7307  18.125004 17.933253
    ## 7308  23.915369 19.708733
    ## 7309  23.006770 16.696219
    ## 7310  27.147574 19.876919
    ## 7311  20.498158 19.644797
    ## 7312  15.563786 18.091417
    ## 7313  21.913314 16.186195
    ## 7314  16.288001 17.382741
    ## 7315  19.711491 16.897961
    ## 7316  22.962767 18.706194
    ## 7317  16.770843 16.556565
    ## 7318  20.931911 18.787886
    ## 7319  21.228064 15.603653
    ## 7320  19.462293 17.821637
    ## 7321  17.467498 16.821436
    ## 7322  11.939467 12.798850
    ## 7323  14.833610 17.175621
    ## 7324  10.797247 14.082259
    ## 7325  13.471420 17.341130
    ## 7326  18.536032 14.343262
    ## 7327  14.959658 14.918217
    ## 7328  23.111591 19.913774
    ## 7329  18.974137 17.058437
    ## 7330  20.140455 19.820180
    ## 7331  25.014723 18.178012
    ## 7332  22.863935 17.592898
    ## 7333  21.446203 15.224803
    ## 7334  17.546142 17.581948
    ## 7335  18.852962 19.731795
    ## 7336  19.379683 17.854620
    ## 7337  18.149794 15.646557
    ## 7338  17.511836 18.444099
    ## 7339  19.041384 18.001796
    ## 7340  23.555410 19.459642
    ## 7341  23.093451 18.608034
    ## 7342  27.484156 19.606650
    ## 7343  20.312920 18.156187
    ## 7344  16.015795 17.226780
    ## 7345  21.625165 17.698023
    ## 7346  20.388193 19.782110
    ## 7347  17.036544 17.667687
    ## 7348  19.042764 17.888004
    ## 7349  19.658686 19.975211
    ## 7350  20.157798 19.270205
    ## 7351  24.747423 18.535125
    ## 7352  25.011995 18.267250
    ## 7353  16.376391 19.069867
    ## 7354  21.177294 19.590789
    ## 7355  24.180191 18.932629
    ## 7356  17.954878 19.190428
    ## 7357  28.779512 18.572954
    ## 7358  20.746552 17.182386
    ## 7359  24.543940 19.318206
    ## 7360  16.053289 18.842153
    ## 7361  18.163617 17.697134
    ## 7362  17.338371 16.936819
    ## 7363  18.564835 17.197224
    ## 7364  25.147837 19.967462
    ## 7365  16.790740 19.861865
    ## 7366  12.780367 11.437551
    ## 7367  21.260794 18.796447
    ## 7368  24.696925 19.968690
    ## 7369  27.545231 18.573752
    ## 7370  23.778018 19.872284
    ## 7371  26.189534 19.645172
    ## 7372  17.784973 17.233098
    ## 7373  20.190963 19.846257
    ## 7374  21.218578 19.495932
    ## 7375  22.578519 18.774296
    ## 7376  21.776792 17.625496
    ## 7377  25.949101 18.914756
    ## 7378  23.701515 19.713022
    ## 7379  13.545268 19.686339
    ## 7380  21.312435 17.513244
    ## 7381  15.445844 18.846513
    ## 7382  14.202575 15.328966
    ## 7383  18.224157 15.732127
    ## 7384  18.643678 13.962492
    ## 7385  16.194423 15.775631
    ## 7386  21.905806 19.894858
    ## 7387  26.356683 17.824203
    ## 7388  21.128144 15.838123
    ## 7389  16.101771 19.630351
    ## 7390  22.246450 19.777597
    ## 7391  14.810367 17.616531
    ## 7392  21.008383 19.501733
    ## 7393  18.759871 17.853590
    ## 7394  17.650158 18.285332
    ## 7395  27.518670 19.954765
    ## 7396  15.036598 15.614807
    ## 7397  15.107850 15.036363
    ## 7398  14.052945 19.699139
    ## 7399  22.658674 17.576825
    ## 7400  19.742844 18.906790
    ## 7401  18.569750 17.422617
    ## 7402  17.322902 19.397551
    ## 7403  22.323440 18.177106
    ## 7404  23.956706 18.637780
    ## 7405  23.755414 18.698815
    ## 7406  17.645101 19.133271
    ## 7407  23.075770 17.740751
    ## 7408  21.356769 18.367566
    ## 7409  18.867829 18.511539
    ## 7410  20.726850 19.866829
    ## 7411  19.356190 19.019949
    ## 7412  24.592205 19.326791
    ## 7413  26.241806 19.197269
    ## 7414  18.496340 18.548328
    ## 7415  16.522079 14.944334
    ## 7416  25.452103 18.123697
    ## 7417  17.886705 19.860248
    ## 7418  16.430344 19.895364
    ## 7419  17.811192 19.578177
    ## 7420  14.915221 16.129311
    ## 7421  22.727126 18.245550
    ## 7422  24.613366 18.929915
    ## 7423  18.078983 17.071877
    ## 7424  15.304666 17.548355
    ## 7425  18.345768 18.665868
    ## 7426  27.302974 19.776319
    ## 7427  24.624205 19.688475
    ## 7428  21.659257 19.931134
    ## 7429  26.222536 19.473678
    ## 7430  23.778228 19.072396
    ## 7431  20.985496 19.596235
    ## 7432  21.658128 19.060834
    ## 7433  14.602931 15.363056
    ## 7434  13.299487 19.884029
    ## 7435  21.259497 16.484239
    ## 7436  23.812216 17.942669
    ## 7437  15.188199 17.299276
    ## 7438  24.821123 19.931322
    ## 7439  21.095301 17.696981
    ## 7440  18.719996 17.836108
    ## 7441  18.798674 15.335878
    ## 7442  15.613914 15.282515
    ## 7443  13.681692 19.204624
    ## 7444  20.519203 18.152465
    ## 7445  17.164910 18.744185
    ## 7446  19.291043 18.849325
    ## 7447  13.480765 16.812195
    ## 7448  20.400212 18.343534
    ## 7449  18.625375 18.471502
    ## 7450  20.722450 19.698660
    ## 7451  20.098765 19.253963
    ## 7452  23.323855 19.659167
    ## 7453  18.247510 17.460299
    ## 7454  20.891105 18.660340
    ## 7455  16.356731 15.681879
    ## 7456  19.349578 19.683424
    ## 7457  23.065522 18.765684
    ## 7458  16.244586 15.293672
    ## 7459  18.553513 19.911658
    ## 7460  27.197730 19.677681
    ## 7461  17.078456 18.964587
    ## 7462  18.964144 17.860395
    ## 7463  20.099776 18.537817
    ## 7464  24.470673 19.465812
    ## 7465  11.049943 13.919402
    ## 7466  20.767043 19.545934
    ## 7467  20.740274 19.588487
    ## 7468  21.396703 18.431938
    ## 7469  19.009629 19.252096
    ## 7470  25.230753 19.222761
    ## 7471  23.473811 18.674490
    ## 7472  21.351727 17.911725
    ## 7473  20.118984 18.136996
    ## 7474  12.417579 16.173818
    ## 7475  19.824216 18.620232
    ## 7476  24.543262 19.573693
    ## 7477  23.581690 19.029072
    ## 7478  23.237739 16.760255
    ## 7479  20.878819 18.672642
    ## 7480  19.308075 19.372353
    ## 7481  25.090017 19.807363
    ## 7482  20.673268 19.269826
    ## 7483  18.781827 19.136402
    ## 7484  20.075097 18.980013
    ## 7485  23.438962 19.500768
    ## 7486  14.005573 13.458312
    ## 7487  17.299252 19.593373
    ## 7488  21.574157 18.093124
    ## 7489  23.486226 18.434363
    ## 7490  19.823902 18.571515
    ## 7491  18.121445 17.880783
    ## 7492  26.601503 19.080514
    ## 7493  18.143470 19.750433
    ## 7494  21.925583 18.310538
    ## 7495  22.116707 19.271862
    ## 7496  16.239931 16.799339
    ## 7497  23.243270 19.961935
    ## 7498  21.452269 17.721858
    ## 7499  13.277157 19.061882
    ## 7500  16.554367 17.396040
    ## 7501  20.190299 19.695723
    ## 7502  21.876972 18.978166
    ## 7503  24.044259 18.900561
    ## 7504  21.622424 17.299746
    ## 7505  20.213905 17.752940
    ## 7506  18.657589 16.807936
    ## 7507  14.891949 17.324659
    ## 7508  15.428000 17.225566
    ## 7509  17.471437 17.359608
    ## 7510  21.901562 17.569697
    ## 7511  18.933296 18.645877
    ## 7512  18.059617 19.203437
    ## 7513  15.004103 18.571350
    ## 7514  23.455619 19.585248
    ## 7515  16.439077 16.842107
    ## 7516  18.772105 19.427572
    ## 7517  19.153839 17.109639
    ## 7518  22.705595 19.823625
    ## 7519  19.370766 19.419792
    ## 7520  15.980799 16.402282
    ## 7521  19.282758 19.982654
    ## 7522  17.163117 13.759054
    ## 7523  19.755628 17.928912
    ## 7524  18.702469 16.839154
    ## 7525  21.909216 18.686946
    ## 7526  18.900079 18.833347
    ## 7527  27.216160 19.029672
    ## 7528  20.068220 17.330863
    ## 7529  22.152442 18.835074
    ## 7530  18.453128 19.779803
    ## 7531  19.808084 19.329310
    ## 7532  20.424168 18.853708
    ## 7533  19.309395 18.530191
    ## 7534  20.545938 19.910207
    ## 7535  24.913886 18.880724
    ## 7536  15.909937 18.722908
    ## 7537  22.483013 19.530143
    ## 7538  16.932829 13.827240
    ## 7539  20.182043 19.393399
    ## 7540  23.277580 17.776601
    ## 7541  13.584054 15.547211
    ## 7542  22.917693 17.818322
    ## 7543  15.384142 13.790309
    ## 7544  22.035482 19.436589
    ## 7545  26.077473 18.710478
    ## 7546  20.954818 19.286450
    ## 7547  15.870574 19.722487
    ## 7548  17.860910 18.894359
    ## 7549  19.207426 15.761924
    ## 7550  19.706833 17.779964
    ## 7551  20.349139 19.500724
    ## 7552  19.457983 14.246384
    ## 7553  15.684729 18.491919
    ## 7554  16.369191 19.018596
    ## 7555  21.401111 19.526890
    ## 7556  23.236309 19.953746
    ## 7557  19.813730 19.462491
    ## 7558  22.987200 18.995967
    ## 7559  24.900956 19.914931
    ## 7560  22.882428 19.101453
    ## 7561  15.545478 16.153716
    ## 7562  19.846155 17.490021
    ## 7563  19.073526 19.445523
    ## 7564  16.141048 18.427843
    ## 7565  19.731332 18.794607
    ## 7566  17.552794 15.558523
    ## 7567  21.298797 18.887363
    ## 7568  15.991134 18.562251
    ## 7569  29.508803 19.747613
    ## 7570  19.054169 19.268640
    ## 7571  16.881228 19.214450
    ## 7572  26.965261 19.605495
    ## 7573  21.094875 17.461866
    ## 7574  17.024348 19.530187
    ## 7575  20.010748 18.739055
    ## 7576  28.005574 18.362581
    ## 7577  22.475001 19.321105
    ## 7578  17.627951 15.381217
    ## 7579  22.974472 18.979058
    ## 7580  21.623667 19.917179
    ## 7581  18.903930 14.647274
    ## 7582  25.947581 19.835911
    ## 7583  23.642167 19.413845
    ## 7584  22.167142 19.173499
    ## 7585  17.615993 19.078083
    ## 7586  20.445408 16.571817
    ## 7587  19.944063 18.933767
    ## 7588  21.483702 17.471671
    ## 7589  12.311328 11.582750
    ## 7590  18.829316 18.638382
    ## 7591  20.694943 18.575893
    ## 7592  23.332071 19.383662
    ## 7593  13.454440 15.046546
    ## 7594  22.684244 19.205303
    ## 7595  25.410630 18.328794
    ## 7596  19.525014 18.329404
    ## 7597  22.468151 19.637901
    ## 7598  25.607020 18.919751
    ## 7599  20.332377 17.887255
    ## 7600  19.289980 18.696966
    ## 7601  23.758869 19.168854
    ## 7602  19.862050 19.148386
    ## 7603  18.598699 17.863964
    ## 7604  13.600150 15.778999
    ## 7605  23.985798 19.372898
    ## 7606  17.318901 17.443989
    ## 7607  22.660610 18.199408
    ## 7608  10.384963 13.426760
    ## 7609  14.702320 15.517755
    ## 7610  27.570771 19.931997
    ## 7611  17.649686 17.876568
    ## 7612  23.699237 16.911963
    ## 7613  17.915584 19.789509
    ## 7614  16.146508 18.568217
    ## 7615  12.896113 14.662539
    ## 7616  13.495271 15.084239
    ## 7617  18.131034 17.753721
    ## 7618  18.273168 19.642724
    ## 7619  19.780026 19.776999
    ## 7620  20.581462 18.925021
    ## 7621  17.301246 17.549348
    ## 7622  20.306378 19.880012
    ## 7623  24.828876 19.498066
    ## 7624  19.074722 13.680967
    ## 7625  28.161792 16.746026
    ## 7626  24.294249 19.213637
    ## 7627  23.748074 16.123360
    ## 7628  16.758194 15.697293
    ## 7629  19.596406 19.084239
    ## 7630  22.024690 16.225995
    ## 7631  23.028994 19.878303
    ## 7632  21.923018 18.268277
    ## 7633  18.657552 19.492560
    ## 7634  20.359876 19.943761
    ## 7635  19.691804 19.460689
    ## 7636  17.694974 19.111044
    ## 7637  23.847365 19.393913
    ## 7638  14.153521 11.295841
    ## 7639  21.307121 16.699324
    ## 7640  15.294986 18.791595
    ## 7641  21.251454 19.975213
    ## 7642  15.337128 18.679503
    ## 7643  20.188690 19.029328
    ## 7644  20.288315 16.136949
    ## 7645  15.035656 15.946625
    ## 7646  17.092895 19.514441
    ## 7647  17.708283 18.276206
    ## 7648  18.051354 17.725135
    ## 7649  19.176741 18.564569
    ## 7650  15.983359 19.665631
    ## 7651  26.248944 19.327589
    ## 7652  18.158073 16.332068
    ## 7653  15.708237 18.685440
    ## 7654  23.226032 17.846694
    ## 7655  16.433716 18.250613
    ## 7656  14.680280 16.570307
    ## 7657  22.508932 17.225114
    ## 7658  17.115811 19.292191
    ## 7659  21.514456 18.779141
    ## 7660  18.973763 18.280264
    ## 7661  21.246849 19.089933
    ## 7662  25.373252 17.901784
    ## 7663  21.958328 17.769201
    ## 7664  19.970572 19.720994
    ## 7665  20.397605 18.371487
    ## 7666  18.194810 19.263927
    ## 7667  21.032981 17.442814
    ## 7668  25.425020 19.420585
    ## 7669  19.840281 18.481790
    ## 7670  19.083604 18.287406
    ## 7671  20.862605 19.666130
    ## 7672  13.572140 14.197802
    ## 7673  18.842044 18.536312
    ## 7674  19.529385 15.286938
    ## 7675  18.632350 17.633707
    ## 7676  21.092044 18.730858
    ## 7677  21.694933 19.761193
    ## 7678  22.835228 18.632163
    ## 7679  27.121140 19.792712
    ## 7680  16.134545 19.185256
    ## 7681  19.064247 19.034943
    ## 7682  18.858186 19.656893
    ## 7683  17.441354 14.579857
    ## 7684  18.851112 18.448727
    ## 7685  19.822837 19.063497
    ## 7686  16.592254 18.568426
    ## 7687  21.012630 15.569268
    ## 7688  25.852832 17.590465
    ## 7689  26.446859 19.629763
    ## 7690  16.291289 18.282043
    ## 7691  13.316312 11.093090
    ## 7692  21.064325 19.964274
    ## 7693  27.168006 19.517184
    ## 7694  10.959274  9.591955
    ## 7695  18.905777 19.382682
    ## 7696  25.900702 19.774651
    ## 7697  19.564884 18.347800
    ## 7698  19.513473 19.470310
    ## 7699  14.911497 18.033008
    ## 7700  22.583662 18.679661
    ## 7701  22.497419 19.564551
    ## 7702  24.468017 17.940401
    ## 7703  16.397442 15.457007
    ## 7704  18.618398 18.773640
    ## 7705  19.087510 17.780964
    ## 7706  26.283822 17.690390
    ## 7707  23.258644 18.752339
    ## 7708  12.857176 12.492850
    ## 7709  18.843069 19.601070
    ## 7710  19.492623 18.093937
    ## 7711  21.325506 19.875541
    ## 7712  13.276859 16.498616
    ## 7713  26.855286 19.260948
    ## 7714  18.924299 19.710681
    ## 7715  19.681528 18.073734
    ## 7716  24.869030 18.347581
    ## 7717  21.576138 19.650542
    ## 7718  21.812608 19.574460
    ## 7719  22.786397 18.974220
    ## 7720  23.481931 19.070272
    ## 7721  18.987603 18.344747
    ## 7722  18.465808 19.386724
    ## 7723  22.859122 18.706761
    ## 7724  20.004735 19.835619
    ## 7725  16.961500 17.786438
    ## 7726  22.870163 19.662371
    ## 7727  22.711372 19.943948
    ## 7728  21.871009 17.807016
    ## 7729  29.708900 18.899222
    ## 7730  21.149024 18.824857
    ## 7731  20.784667 17.436967
    ## 7732  17.854403 14.097719
    ## 7733  18.192138 18.573329
    ## 7734  25.765165 18.991250
    ## 7735  22.916444 18.974297
    ## 7736  16.406706 19.551089
    ## 7737  18.321912 19.869429
    ## 7738  19.293161 18.304419
    ## 7739  23.584675 19.939672
    ## 7740  18.402313 19.200087
    ## 7741  18.926242 16.014967
    ## 7742  12.839775 13.167780
    ## 7743  26.141190 19.878545
    ## 7744  21.699881 17.681685
    ## 7745  24.732115 18.782094
    ## 7746  18.675723 19.455670
    ## 7747  20.972952 16.871271
    ## 7748  23.883395 19.297924
    ## 7749  13.508985 15.826329
    ## 7750  14.445681 13.419925
    ## 7751  16.765671 18.658845
    ## 7752  15.001156 18.034323
    ## 7753  27.100202 19.030241
    ## 7754  17.435382 18.857420
    ## 7755  26.813571 19.767789
    ## 7756  18.882766 17.430124
    ## 7757  22.011443 19.269846
    ## 7758  16.886438 19.402831
    ## 7759  23.989798 17.910743
    ## 7760  20.501068 19.296505
    ## 7761  21.537413 19.373662
    ## 7762  25.139519 17.874983
    ## 7763  23.967890 18.756448
    ## 7764  21.650480 17.476790
    ## 7765  19.898355 16.542752
    ## 7766  19.799816 19.135406
    ## 7767  23.150196 19.173798
    ## 7768  17.955489 19.597901
    ## 7769  25.389994 19.908111
    ## 7770  19.332969 17.625195
    ## 7771  14.715451 16.546377
    ## 7772  18.586171 18.877260
    ## 7773  18.309268 17.461887
    ## 7774  19.815533 18.521819
    ## 7775  19.011114 17.192283
    ## 7776  23.012691 16.747640
    ## 7777  21.705998 19.732138
    ## 7778  19.293348 17.713910
    ## 7779  15.004192 15.779996
    ## 7780  17.159250 17.642559
    ## 7781  16.058376 17.556947
    ## 7782  20.090191 19.540184
    ## 7783  18.760563 19.286776
    ## 7784  25.298435 19.855908
    ## 7785  19.428645 17.793973
    ## 7786  19.257356 18.380122
    ## 7787  20.401535 17.394772
    ## 7788  19.595011 18.693882
    ## 7789  20.726361 17.888282
    ## 7790  23.607184 19.067242
    ## 7791  21.602316 17.010325
    ## 7792  19.387684 17.621922
    ## 7793  26.414488 19.758932
    ## 7794  23.047372 17.596577
    ## 7795  17.859726 16.195020
    ## 7796  24.491186 19.992823
    ## 7797  22.494771 19.193963
    ## 7798  24.266839 18.785524
    ## 7799  19.374575 18.941260
    ## 7800  23.198891 18.858152
    ## 7801  22.556109 17.980355
    ## 7802  14.454251 15.074205
    ## 7803  19.001460 16.184194
    ## 7804  21.369046 19.718508
    ## 7805  21.739944 19.646129
    ## 7806  20.869263 18.751093
    ## 7807  19.021444 19.482035
    ## 7808  21.969959 19.381410
    ## 7809  21.225661 18.674084
    ## 7810  15.165473 19.799705
    ## 7811  21.901064 19.269927
    ## 7812  20.972068 18.332897
    ## 7813  15.845894 18.272456
    ## 7814  22.702550 19.584340
    ## 7815  20.685468 19.173042
    ## 7816  14.537019 16.706948
    ## 7817  27.740922 19.493358
    ## 7818  22.575787 19.259607
    ## 7819  19.180884 18.146213
    ## 7820  21.919076 18.685490
    ## 7821  15.532951 18.015023
    ## 7822  15.784945 16.136214
    ## 7823  27.831835 19.567088
    ## 7824  22.318457 19.935490
    ## 7825  13.910457 18.095375
    ## 7826  15.619150 17.596925
    ## 7827  28.424822 19.219724
    ## 7828  24.745341 16.662252
    ## 7829  21.932719 18.592191
    ## 7830  17.371053 19.828833
    ## 7831  18.672040 15.797488
    ## 7832  20.339653 19.338012
    ## 7833  20.057476 18.162109
    ## 7834  21.808805 19.791263
    ## 7835  16.956206 19.764153
    ## 7836  20.913741 18.875059
    ## 7837  28.211680 19.968774
    ## 7838  19.537885 17.385138
    ## 7839  19.933457 18.418895
    ## 7840  18.810040 19.875723
    ## 7841  13.312653 15.772574
    ## 7842  16.452910 17.325629
    ## 7843  20.009617 19.942687
    ## 7844  22.499320 18.596473
    ## 7845  24.104580 19.900856
    ## 7846  22.205302 19.827551
    ## 7847  16.134771 15.826460
    ## 7848  20.998510 16.327977
    ## 7849  14.602471 15.263141
    ## 7850  21.205833 18.976066
    ## 7851  12.155342 14.585152
    ## 7852  16.358006 18.304157
    ## 7853  25.120670 19.940118
    ## 7854  21.348876 16.614444
    ## 7855  16.035605 18.680696
    ## 7856  21.636495 17.360751
    ## 7857  25.718828 17.231337
    ## 7858  25.330249 19.972262
    ## 7859  21.909769 19.019606
    ## 7860  21.846173 16.572829
    ## 7861  17.995789 17.780602
    ## 7862  20.031108 17.068731
    ## 7863  16.656380 19.242318
    ## 7864  18.150683 15.482412
    ## 7865  18.983683 19.940027
    ## 7866  22.021394 19.802351
    ## 7867  29.785662 19.782917
    ## 7868  16.919229 18.103931
    ## 7869  22.099597 19.118162
    ## 7870  19.913060 17.892021
    ## 7871  17.370688 16.260882
    ## 7872  20.036002 19.975645
    ## 7873  16.396899 18.957454
    ## 7874  22.990600 17.997616
    ## 7875  13.541278 15.626407
    ## 7876  22.313656 19.083893
    ## 7877  18.295020 18.641793
    ## 7878  20.335377 19.937486
    ## 7879  19.488335 16.547465
    ## 7880  24.391940 19.530077
    ## 7881  18.682264 18.089399
    ## 7882  17.124719 18.724517
    ## 7883  20.864223 18.624600
    ## 7884  21.747882 19.956472
    ## 7885  19.493301 17.688779
    ## 7886  17.620377 18.224929
    ## 7887  15.555833 15.149123
    ## 7888  17.737281 17.264530
    ## 7889  13.728443 13.735042
    ## 7890  19.961594 19.715145
    ## 7891  12.081337 19.363821
    ## 7892  16.732189 19.613926
    ## 7893  28.680704 18.639945
    ## 7894  21.500376 19.184015
    ## 7895  22.480820 18.238386
    ## 7896  17.185948 18.850253
    ## 7897  21.066012 18.936241
    ## 7898  16.722663 16.003590
    ## 7899  19.679729 18.933495
    ## 7900  18.739488 16.258122
    ## 7901  23.207042 19.250161
    ## 7902  16.663118 16.180628
    ## 7903  17.906419 16.063349
    ## 7904  23.827125 18.786797
    ## 7905  17.909312 18.856311
    ## 7906  24.246423 17.424838
    ## 7907  26.040328 19.412732
    ## 7908  15.778047 19.068282
    ## 7909  12.702336 15.737959
    ## 7910  17.849097 19.073223
    ## 7911  21.898514 19.142783
    ## 7912  23.492490 17.678576
    ## 7913  21.552161 19.321961
    ## 7914  21.551937 19.819075
    ## 7915  18.714465 16.964424
    ## 7916  24.962338 19.860316
    ## 7917  27.433301 19.095591
    ## 7918  19.969389 19.441900
    ## 7919  22.402221 19.075616
    ## 7920  20.689626 18.447595
    ## 7921  25.757528 19.439023
    ## 7922  14.983949 18.165498
    ## 7923  20.951581 17.094042
    ## 7924  22.930054 19.714379
    ## 7925  23.059460 19.056326
    ## 7926  22.039068 17.184751
    ## 7927  19.759862 19.394933
    ## 7928  19.871328 17.764314
    ## 7929  14.882377 13.557535
    ## 7930  17.192471 15.320067
    ## 7931  19.204660 16.904650
    ## 7932  19.576611 18.733868
    ## 7933  16.725443 18.630574
    ## 7934  21.105407 18.106669
    ## 7935  15.619295 13.288672
    ## 7936  19.560793 19.057152
    ## 7937  16.184318 15.673321
    ## 7938  20.648349 19.022362
    ## 7939  17.813951 18.728416
    ## 7940  26.408136 17.624555
    ## 7941  14.374201 14.429329
    ## 7942  12.896144 15.135009
    ## 7943  20.408048 19.212087
    ## 7944  22.860251 19.148803
    ## 7945  20.640039 19.319356
    ## 7946  17.017440 19.478980
    ## 7947  22.017253 19.674340
    ## 7948  19.520113 18.896916
    ## 7949  25.516033 19.161424
    ## 7950  25.501866 19.840131
    ## 7951  17.470606 17.143651
    ## 7952  17.990642 19.624163
    ## 7953  18.737144 19.530241
    ## 7954  19.501558 18.831793
    ## 7955  29.881422 19.998367
    ## 7956  21.295246 16.787500
    ## 7957  19.424300 17.317519
    ## 7958  14.334086 18.643468
    ## 7959  23.141194 18.342764
    ## 7960  17.695188 19.533603
    ## 7961  10.063963 11.570191
    ## 7962  16.573820 19.197916
    ## 7963  19.707285 18.395110
    ## 7964  16.281856 19.033755
    ## 7965  25.694758 19.933276
    ## 7966  15.369216 14.766203
    ## 7967  20.342371 19.212078
    ## 7968  17.734019 19.401001
    ## 7969  20.116217 16.781095
    ## 7970  22.793766 19.479676
    ## 7971  15.414083 18.487345
    ## 7972  27.699985 19.981777
    ## 7973  21.511989 19.967995
    ## 7974  12.706895 18.148065
    ## 7975  21.546460 18.698404
    ## 7976  17.718064 18.269474
    ## 7977  16.029883 19.536050
    ## 7978  24.122098 19.819070
    ## 7979  16.326729 15.081923
    ## 7980  20.744090 19.589492
    ## 7981  20.867379 19.007825
    ## 7982  20.125850 19.911229
    ## 7983  22.314112 19.177238
    ## 7984  20.860322 15.855304
    ## 7985  21.126613 18.060525
    ## 7986  29.433221 19.180472
    ## 7987  18.446128 17.445104
    ## 7988  16.801288 18.204476
    ## 7989  23.752588 18.435215
    ## 7990  15.850733 17.241681
    ## 7991  18.975803 18.703584
    ## 7992  26.552511 18.990686
    ## 7993  24.482841 19.298012
    ## 7994  20.441685 17.362276
    ## 7995  19.674618 19.779720
    ## 7996  19.540178 17.649551
    ## 7997  13.985510 10.195765
    ## 7998  20.913613 19.203523
    ## 7999  26.099112 19.193330
    ## 8000  18.284924 19.978176
    ## 8001  25.331550 19.605836
    ## 8002  25.036032 19.970315
    ## 8003  25.388498 18.639522
    ## 8004  19.092755 19.445188
    ## 8005  21.321392 17.049388
    ## 8006  17.838678 17.308612
    ## 8007  15.354235 14.046900
    ## 8008  22.335733 19.107898
    ## 8009  18.203964 19.965691
    ## 8010  20.726713 19.305567
    ## 8011  26.035656 18.751492
    ## 8012  18.383312 15.721462
    ## 8013  22.493763 17.965627
    ## 8014  25.593220 19.180475
    ## 8015  15.772515 18.565601
    ## 8016  22.359628 19.828374
    ## 8017  15.803606 17.217947
    ## 8018  23.349624 18.089365
    ## 8019  20.432203 18.812159
    ## 8020  23.129187 19.305438
    ## 8021  14.694476 15.166769
    ## 8022  13.904964 12.819843
    ## 8023  16.310058 13.914222
    ## 8024  19.779635 19.680225
    ## 8025  24.792204 19.370794
    ## 8026  16.589780 19.764357
    ## 8027  18.609383 18.815187
    ## 8028  24.562767 17.757004
    ## 8029  18.722522 14.295472
    ## 8030  21.818150 19.683061
    ## 8031  22.391113 19.872195
    ## 8032  17.502008 16.789946
    ## 8033  20.519675 17.773600
    ## 8034  26.352076 19.851763
    ## 8035  15.532906 14.764712
    ## 8036  23.143053 19.780806
    ## 8037  19.387697 18.824574
    ## 8038  24.491248 18.818992
    ## 8039  18.202182 19.266377
    ## 8040  20.657985 19.722146
    ## 8041  12.508934 11.275107
    ## 8042  17.852901 18.094054
    ## 8043  21.467647 19.998494
    ## 8044  24.353939 18.864118
    ## 8045  16.725841 16.195989
    ## 8046  13.832732 15.757077
    ## 8047  22.699537 18.319271
    ## 8048  24.543070 19.700039
    ## 8049  16.825078 19.915325
    ## 8050  25.566824 18.583104
    ## 8051  20.303055 17.319537
    ## 8052  16.129014 17.245157
    ## 8053  20.258985 17.373582
    ## 8054  17.862994 18.527692
    ## 8055  21.630603 16.372081
    ## 8056  17.256931 19.510280
    ## 8057  19.666550 19.066833
    ## 8058  15.219750 13.754880
    ## 8059  16.630305 19.332171
    ## 8060  16.453753 17.395733
    ## 8061  20.729973 19.720753
    ## 8062  19.330909 18.716521
    ## 8063  22.336509 18.384426
    ## 8064  14.578392 14.181849
    ## 8065  15.018593 18.209681
    ## 8066  17.586031 16.152534
    ## 8067  18.779374 19.206769
    ## 8068  26.150940 19.480713
    ## 8069  25.647973 19.922546
    ## 8070  25.430746 19.867615
    ## 8071  20.566406 17.406978
    ## 8072  21.253906 19.510363
    ## 8073  18.171417 19.710896
    ## 8074  21.049623 16.999866
    ## 8075  20.513290 18.815507
    ## 8076  15.935592 18.448732
    ## 8077  16.210151 14.193844
    ## 8078  21.520582 19.447438
    ## 8079  17.616294 18.566775
    ## 8080  24.500189 19.562601
    ## 8081  21.083811 14.550367
    ## 8082  23.058767 18.319338
    ## 8083  19.641535 18.334506
    ## 8084  19.175288 19.975537
    ## 8085  26.174944 18.503941
    ## 8086  29.099770 19.014737
    ## 8087  19.463547 19.919879
    ## 8088  13.099911 18.664661
    ## 8089  19.767566 14.940455
    ## 8090  14.257914 16.565452
    ## 8091  17.464037 17.653387
    ## 8092  22.594498 18.167965
    ## 8093  19.720729 17.996014
    ## 8094  23.903208 17.530249
    ## 8095  21.452788 19.072265
    ## 8096  20.977606 18.550555
    ## 8097  18.936327 17.022068
    ## 8098  17.791067 18.747300
    ## 8099  20.944699 15.379261
    ## 8100  27.875358 19.645403
    ## 8101  25.874266 19.781534
    ## 8102  23.205059 19.455639
    ## 8103  23.794951 18.395505
    ## 8104  19.775542 17.103232
    ## 8105  16.748322 19.998852
    ## 8106  30.172967 19.180824
    ## 8107  18.895573 19.959090
    ## 8108  14.583659 18.421795
    ## 8109  20.966584 19.264548
    ## 8110  14.676544 14.872968
    ## 8111  22.129979 19.284732
    ## 8112  13.339263 14.883140
    ## 8113  22.808812 19.277282
    ## 8114  17.437485 18.205765
    ## 8115  16.856798 19.283823
    ## 8116  13.602730 18.978956
    ## 8117  21.661672 18.222642
    ## 8118  14.827887 17.287942
    ## 8119  20.915772 17.771413
    ## 8120  22.307619 17.213185
    ## 8121  14.490350 16.842416
    ## 8122  26.489957 19.143706
    ## 8123  17.204727 18.819425
    ## 8124  18.123208 17.722788
    ## 8125  14.735785 13.368685
    ## 8126  24.862838 19.964847
    ## 8127  22.552071 18.326782
    ## 8128  21.304687 19.714108
    ## 8129  14.577917 19.894038
    ## 8130  22.668617 18.550584
    ## 8131  19.523472 18.481483
    ## 8132  17.731702 19.744839
    ## 8133  14.660478 18.374443
    ## 8134  23.973534 17.304476
    ## 8135  20.048421 19.148329
    ## 8136  21.150594 19.942814
    ## 8137  14.752813 13.539789
    ## 8138  25.829786 19.408577
    ## 8139  17.934220 16.900598
    ## 8140  20.433249 19.751350
    ## 8141  12.973584 15.519541
    ## 8142  20.311406 17.899752
    ## 8143  15.140353 18.733606
    ## 8144  15.669830 17.195200
    ## 8145  20.054798 19.510490
    ## 8146  22.024398 19.762396
    ## 8147  17.387301 16.899801
    ## 8148  21.372971 19.523380
    ## 8149  19.460579 19.489771
    ## 8150  23.248143 19.742027
    ## 8151  12.688342 14.786132
    ## 8152  27.100582 19.606887
    ## 8153  23.578066 16.912032
    ## 8154  23.830765 19.318466
    ## 8155  19.646960 16.030194
    ## 8156  18.066960 16.477202
    ## 8157  16.920330 17.301676
    ## 8158  24.520727 19.735280
    ## 8159  15.044266 15.688555
    ## 8160  20.632829 18.088437
    ## 8161  26.237277 18.056220
    ## 8162  22.202923 19.001012
    ## 8163  19.231060 17.563019
    ## 8164  22.165473 17.811872
    ## 8165  21.197179 16.531661
    ## 8166  17.367047 18.351868
    ## 8167  16.113305 15.338515
    ## 8168  21.766891 19.189033
    ## 8169  21.816576 18.402126
    ## 8170  17.946827 18.665359
    ## 8171  22.587416 17.882591
    ## 8172  19.315585 18.186476
    ## 8173  21.667857 18.202388
    ## 8174  15.939740 19.800818
    ## 8175  24.175647 18.313660
    ## 8176  24.703965 19.198535
    ## 8177  19.847380 18.139653
    ## 8178  10.627811 16.459332
    ## 8179  19.222522 16.307894
    ## 8180  14.341955 18.865249
    ## 8181  23.003321 19.920536
    ## 8182  19.639221 17.756203
    ## 8183  18.222497 19.757465
    ## 8184  20.051804 18.145563
    ## 8185  23.072215 19.943155
    ## 8186  18.410940 18.911520
    ## 8187  15.380455 15.524155
    ## 8188  22.561952 19.852998
    ## 8189  16.606781 19.100334
    ## 8190  14.351102 15.915350
    ## 8191  22.362253 19.194690
    ## 8192  18.635902 19.043645
    ## 8193  25.040712 19.083345
    ## 8194  23.764504 19.334446
    ## 8195  23.473415 18.375826
    ## 8196  24.241976 19.685932
    ## 8197  20.570031 18.653473
    ## 8198  21.675066 19.942248
    ## 8199  15.758228 14.142499
    ## 8200  19.675761 19.359996
    ## 8201  15.439886 16.625798
    ## 8202  12.277104 12.302867
    ## 8203  20.879353 18.645802
    ## 8204  20.729764 19.455494
    ## 8205  20.386471 19.203075
    ## 8206  23.318291 19.343209
    ## 8207  15.002692 18.345775
    ## 8208  18.784105 19.865514
    ## 8209  24.502022 16.793409
    ## 8210  18.539543 19.196130
    ## 8211  22.784554 19.512860
    ## 8212  18.585972 18.659281
    ## 8213  20.596441 17.109770
    ## 8214  15.474184 15.279162
    ## 8215  20.754506 19.534045
    ## 8216  13.967291 19.317716
    ## 8217  29.619893 19.177493
    ## 8218  14.679333 16.152084
    ## 8219  23.998322 18.275698
    ## 8220  20.152792 19.820529
    ## 8221  23.717766 19.651933
    ## 8222  19.387012 16.186893
    ## 8223  18.958544 15.881526
    ## 8224  17.013439 16.495488
    ## 8225  26.421115 19.651272
    ## 8226  18.088184 14.950950
    ## 8227  28.095166 19.835289
    ## 8228  24.664606 19.965651
    ## 8229  26.355166 19.876154
    ## 8230  25.277932 19.905639
    ## 8231  23.184733 18.916419
    ## 8232  21.889912 17.436904
    ## 8233  17.768271 18.721327
    ## 8234  18.746645 16.288077
    ## 8235  12.550268 12.910625
    ## 8236  28.484602 19.716041
    ## 8237  21.561038 19.409060
    ## 8238  19.207009 19.913603
    ## 8239  16.218360 16.126300
    ## 8240  18.287880 14.946010
    ## 8241  16.438141 16.814696
    ## 8242  17.787087 17.833066
    ## 8243  18.680313 18.537067
    ## 8244  22.459289 15.587702
    ## 8245  17.704504 19.058236
    ## 8246  16.799291 19.790175
    ## 8247  21.962592 19.066297
    ## 8248  16.523473 16.613274
    ## 8249  16.724364 19.987761
    ## 8250  21.511797 19.230988
    ## 8251  19.755419 19.405356
    ## 8252  21.122452 16.319280
    ## 8253  19.986103 18.411510
    ## 8254  18.647438 19.897132
    ## 8255  15.931321 16.326154
    ## 8256  21.468434 17.834222
    ## 8257  17.499297 19.526872
    ## 8258  24.400267 19.059222
    ## 8259  24.972496 19.793193
    ## 8260  20.752539 17.955079
    ## 8261  23.006916 19.329762
    ## 8262  20.727742 19.020635
    ## 8263  16.590126 16.562555
    ## 8264  24.346344 18.346070
    ## 8265  15.481401 16.384624
    ## 8266  20.118239 19.044213
    ## 8267  19.250251 19.823950
    ## 8268  21.932999 18.749358
    ## 8269  20.796332 13.189176
    ## 8270  24.404004 19.616683
    ## 8271  13.012863 17.085708
    ## 8272  22.728645 19.075757
    ## 8273  25.690574 19.666196
    ## 8274  21.460967 19.859806
    ## 8275  17.396400 18.127662
    ## 8276  16.586975 19.515409
    ## 8277  21.336050 19.604539
    ## 8278  24.938102 19.119496
    ## 8279  19.485265 19.142878
    ## 8280  28.207822 19.073961
    ## 8281  25.068866 19.874517
    ## 8282  19.062583 19.223486
    ## 8283  16.363516 13.869313
    ## 8284  20.830920 16.508341
    ## 8285  18.401184 16.519383
    ## 8286  23.522366 19.778080
    ## 8287  24.981816 17.066980
    ## 8288  25.753850 18.538640
    ## 8289  24.411733 19.630831
    ## 8290  20.071985 17.045974
    ## 8291  19.236181 18.298634
    ## 8292  12.284562 10.644836
    ## 8293  21.088611 19.037840
    ## 8294  13.257955 17.281559
    ## 8295  22.776642 17.008975
    ## 8296  17.544927 18.322168
    ## 8297  20.103957 16.149244
    ## 8298  17.513151 19.483581
    ## 8299  17.221232 19.336873
    ## 8300  22.403234 19.400126
    ## 8301  19.067251 18.566543
    ## 8302  18.310407 18.671805
    ## 8303  14.025334 17.184458
    ## 8304  23.125775 19.679252
    ## 8305  19.181569 15.763112
    ## 8306  19.965315 19.902349
    ## 8307  19.017465 17.016812
    ## 8308  18.565849 17.451082
    ## 8309  18.613350 19.862698
    ## 8310  16.571738 14.281100
    ## 8311  15.112740 18.912120
    ## 8312  16.845629 15.982960
    ## 8313  14.393966 13.921644
    ## 8314  21.301697 18.664658
    ## 8315  24.881373 19.106737
    ## 8316  19.121658 18.228885
    ## 8317  24.621137 18.887810
    ## 8318  28.045389 19.926757
    ## 8319  19.318230 18.775311
    ## 8320  18.543442 17.453550
    ## 8321  20.018392 19.118831
    ## 8322  18.464210 15.466571
    ## 8323  17.758790 18.215625
    ## 8324  18.547617 17.629155
    ## 8325  16.413240 14.748745
    ## 8326  23.070089 19.749708
    ## 8327  17.952491 17.912408
    ## 8328  15.521996 19.943578
    ## 8329  17.696700 18.537257
    ## 8330  20.929299 19.063021
    ## 8331  21.447504 18.945457
    ## 8332  15.599230 18.268546
    ## 8333  20.945563 19.509128
    ## 8334  18.835883 19.932042
    ## 8335  16.459681 19.351070
    ## 8336  16.453496 14.274785
    ## 8337  29.103232 19.100434
    ## 8338  23.583721 19.950245
    ## 8339  19.602984 19.196614
    ## 8340  20.215389 17.165047
    ## 8341  21.295944 17.887945
    ## 8342  17.989906 17.323941
    ## 8343  22.092074 19.925254
    ## 8344  23.196548 17.488892
    ## 8345  25.523289 19.436334
    ## 8346  20.179598 19.886857
    ## 8347  17.450652 19.552744
    ## 8348  15.685299 19.212848
    ## 8349  28.463802 19.955473
    ## 8350  17.194601 19.348091
    ## 8351  21.504691 18.346248
    ## 8352  18.904474 14.348990
    ## 8353  24.182694 19.963593
    ## 8354  11.718963 19.211689
    ## 8355  15.384023 15.267864
    ## 8356  24.260853 18.885018
    ## 8357  16.065758 19.578645
    ## 8358  16.790899 19.231336
    ## 8359  20.765455 19.002380
    ## 8360  16.795748 16.318336
    ## 8361  17.692433 18.544016
    ## 8362  20.362935 19.691752
    ## 8363  12.746605 15.000600
    ## 8364  22.281104 18.795686
    ## 8365  21.709766 18.670239
    ## 8366  12.916897 14.741896
    ## 8367  23.289473 18.984047
    ## 8368  13.536118 17.061903
    ## 8369  26.349265 19.009029
    ## 8370  16.745157 19.024362
    ## 8371  19.753276 17.214241
    ## 8372  29.409171 19.330422
    ## 8373  18.474147 16.197825
    ## 8374  15.572254 16.364911
    ## 8375  23.816166 19.792612
    ## 8376  19.190252 18.682664
    ## 8377  25.209296 19.721779
    ## 8378  20.208959 19.859974
    ## 8379  18.800236 19.032908
    ## 8380  17.616412 19.683137
    ## 8381  20.715506 19.489455
    ## 8382  15.483400 14.960422
    ## 8383  17.421898 19.450035
    ## 8384  20.488871 18.463138
    ## 8385  15.652199 14.092178
    ## 8386  25.542478 19.664081
    ## 8387  13.188310 15.848511
    ## 8388  16.698081 19.297009
    ## 8389  19.368598 17.333893
    ## 8390  21.716201 19.591263
    ## 8391  27.779078 19.944426
    ## 8392  23.420745 19.651195
    ## 8393  18.714692 18.514264
    ## 8394  22.461552 19.798456
    ## 8395  14.300988 18.112682
    ## 8396  24.249800 19.755703
    ## 8397  24.201583 19.685702
    ## 8398  24.473699 18.892235
    ## 8399  21.443695 19.234766
    ## 8400  23.257352 19.842265
    ## 8401  19.641651 17.001560
    ## 8402  22.695425 19.584316
    ## 8403  20.428881 18.192106
    ## 8404  14.735606 17.717301
    ## 8405  19.820952 18.440511
    ## 8406  24.348761 19.168324
    ## 8407  20.244143 19.879657
    ## 8408  17.293692 16.902932
    ## 8409  18.058736 18.522457
    ## 8410  19.187045 17.827394
    ## 8411  21.764998 18.542964
    ## 8412  13.788490 15.486171
    ## 8413  19.366172 18.898645
    ## 8414  17.729615 18.841968
    ## 8415  21.072994 16.157434
    ## 8416  24.389910 19.414800
    ## 8417  22.726019 18.059649
    ## 8418  22.205400 19.845835
    ## 8419  21.929650 19.949760
    ## 8420  14.864299 19.430259
    ## 8421  16.414783 14.701869
    ## 8422  15.899466 16.227341
    ## 8423  14.722348 19.851360
    ## 8424  18.681434 19.221374
    ## 8425  18.990029 17.931980
    ## 8426  22.223916 17.552951
    ## 8427  21.127241 14.185583
    ## 8428  21.751726 15.408153
    ## 8429  17.308301 19.070653
    ## 8430  22.302780 19.148234
    ## 8431  18.882594 15.672071
    ## 8432  17.597996 15.942106
    ## 8433  20.186168 19.942028
    ## 8434  25.848936 19.144147
    ## 8435  17.017419 14.162054
    ## 8436  22.394191 18.146051
    ## 8437  14.803056 13.888923
    ## 8438  23.328907 19.386818
    ## 8439  20.994856 17.316300
    ## 8440  23.246457 15.812263
    ## 8441  17.102768 18.677524
    ## 8442  17.403288 18.524442
    ## 8443  18.092725 19.972396
    ## 8444  22.462900 19.992580
    ## 8445  29.074308 18.223102
    ## 8446  18.379679 18.648476
    ## 8447  28.779990 19.846166
    ## 8448  18.097619 18.076850
    ## 8449  16.184200 15.820038
    ## 8450  16.831797 19.425262
    ## 8451  14.895138 12.607948
    ## 8452  20.436434 17.425915
    ## 8453  16.455342 18.538611
    ## 8454  22.385715 19.472168
    ## 8455  22.698349 19.382823
    ## 8456  20.289137 18.537090
    ## 8457  18.537126 19.895751
    ## 8458  20.825514 17.619698
    ## 8459  18.920931 18.664471
    ## 8460  24.801998 19.224101
    ## 8461  16.799404 19.652511
    ## 8462  18.188980 19.817795
    ## 8463  23.521137 18.066488
    ## 8464  22.766589 18.327809
    ## 8465  15.610849 13.282745
    ## 8466  21.323593 18.778523
    ## 8467  16.804877 17.580710
    ## 8468  17.257580 19.040575
    ## 8469  16.339905 18.661005
    ## 8470  19.742359 18.713482
    ## 8471  19.582699 19.603017
    ## 8472  20.793104 19.962878
    ## 8473  19.223538 16.141696
    ## 8474  27.267830 19.659639
    ## 8475  27.916495 18.852631
    ## 8476  17.988567 17.762598
    ## 8477  28.236715 19.920573
    ## 8478  24.858293 19.802731
    ## 8479  21.633998 19.598543
    ## 8480  14.801918 17.035111
    ## 8481  14.462543 18.231660
    ## 8482  20.194747 19.942632
    ## 8483  15.684405 13.820031
    ## 8484  22.072247 17.519552
    ## 8485  25.155155 19.545202
    ## 8486  23.895065 19.159957
    ## 8487  18.865148 18.955125
    ## 8488  15.643839 18.360447
    ## 8489  24.337711 17.365963
    ## 8490  16.905139 17.320072
    ## 8491  15.973562 18.452891
    ## 8492  19.525751 18.140876
    ## 8493  16.277545 16.382116
    ## 8494  21.536927 19.703128
    ## 8495  22.617906 18.474283
    ## 8496  21.224343 18.906888
    ## 8497  22.840462 19.911487
    ## 8498  18.381487 19.814033
    ## 8499  19.372665 16.534439
    ## 8500  20.441683 18.248709
    ## 8501  18.610703 17.430867
    ## 8502  18.777858 15.372430
    ## 8503  20.425631 19.064952
    ## 8504  14.230997 16.928114
    ## 8505  12.620021 14.602120
    ## 8506  18.946324 18.289946
    ## 8507  17.457741 17.276946
    ## 8508  20.486246 18.513639
    ## 8509  18.880144 17.066074
    ## 8510  26.285901 19.967746
    ## 8511  17.624319 16.600153
    ## 8512  16.003844 16.914423
    ## 8513  18.777375 17.864556
    ## 8514  15.023837 17.534154
    ## 8515  23.877974 19.561640
    ## 8516  18.228257 16.371078
    ## 8517  20.482542 19.020520
    ## 8518  21.308826 19.798416
    ## 8519  25.319050 19.447893
    ## 8520  23.269212 19.974710
    ## 8521  19.775675 18.707437
    ## 8522  22.735904 16.033917
    ## 8523  20.433209 17.204643
    ## 8524  20.582576 18.120446
    ## 8525  24.997859 19.888217
    ## 8526  22.235086 17.645435
    ## 8527  20.151404 19.114370
    ## 8528  21.440824 16.167240
    ## 8529  19.024748 17.386485
    ## 8530  21.318499 17.951919
    ## 8531  20.401934 19.502512
    ## 8532  17.584137 16.034440
    ## 8533  22.752276 18.880221
    ## 8534  19.960832 19.311992
    ## 8535  20.479529 19.430962
    ## 8536  13.406876 17.220210
    ## 8537  16.032649 19.560692
    ## 8538  19.777978 17.380228
    ## 8539  17.684543 19.272599
    ## 8540  18.885074 18.479247
    ## 8541  22.021025 17.394466
    ## 8542  13.780018 17.281949
    ## 8543  18.615326 17.844175
    ## 8544  27.049261 19.037307
    ## 8545  20.039495 16.165260
    ## 8546  20.542954 19.306692
    ## 8547  15.316067 17.338452
    ## 8548  16.928974 19.763694
    ## 8549  22.982817 18.490003
    ## 8550  21.715610 19.522694
    ## 8551  19.199161 17.834219
    ## 8552  17.878596 16.472109
    ## 8553  20.430529 17.617983
    ## 8554  22.770081 19.706980
    ## 8555  19.710758 17.758010
    ## 8556  19.625495 19.748644
    ## 8557  20.892356 19.525172
    ## 8558  23.149054 18.419879
    ## 8559  18.907604 17.128339
    ## 8560  31.551752 19.799687
    ## 8561  23.516426 19.899122
    ## 8562  17.575580 16.861439
    ## 8563  16.056718 17.646433
    ## 8564  20.164683 16.196763
    ## 8565  22.551624 19.181300
    ## 8566  22.186051 19.879210
    ## 8567  20.566457 19.124772
    ## 8568  21.272655 18.452780
    ## 8569  15.705972 19.411286
    ## 8570  17.192980 18.113576
    ## 8571  17.667773 16.965345
    ## 8572  18.825839 17.869152
    ## 8573  18.993183 19.552446
    ## 8574  24.097330 19.485987
    ## 8575  27.670224 19.540536
    ## 8576  19.076246 16.664586
    ## 8577  18.867276 18.313963
    ## 8578  21.221240 19.515660
    ## 8579  24.042362 19.866449
    ## 8580  14.038271 19.115199
    ## 8581  19.613592 17.085925
    ## 8582  11.338332 15.674827
    ## 8583  21.671813 18.308668
    ## 8584  22.414562 18.742633
    ## 8585  21.447371 19.032395
    ## 8586  15.560535 16.030780
    ## 8587  16.479880 15.671013
    ## 8588  17.068630 18.758278
    ## 8589  21.836928 18.633143
    ## 8590  14.473181 17.311801
    ## 8591  24.289925 19.509372
    ## 8592  13.913587 15.107033
    ## 8593  23.761112 17.608418
    ## 8594  23.570622 19.912262
    ## 8595  10.206848 14.219231
    ## 8596  20.254537 19.970331
    ## 8597  17.285745 18.542408
    ## 8598  17.289276 18.817155
    ## 8599  18.712696 17.956809
    ## 8600  19.981640 17.572381
    ## 8601  25.946091 19.289829
    ## 8602  17.989125 17.249496
    ## 8603  21.949694 19.461443
    ## 8604  23.107256 19.010356
    ## 8605  15.985960 16.080966
    ## 8606  19.614754 19.795489
    ## 8607  23.692820 18.834727
    ## 8608  14.957074 16.471763
    ## 8609  17.337507 15.887603
    ## 8610  20.417792 16.406985
    ## 8611  25.330717 19.034126
    ## 8612  17.117576 16.497964
    ## 8613  20.523136 15.962160
    ## 8614  21.341888 18.857571
    ## 8615  24.737995 19.208563
    ## 8616  14.534785 17.203520
    ## 8617  14.520815 14.671321
    ## 8618  25.481142 19.808221
    ## 8619  28.277526 19.837258
    ## 8620  15.389348 14.798208
    ## 8621  15.876008 19.913486
    ## 8622  26.033411 18.871325
    ## 8623  18.076414 19.761602
    ## 8624  22.508368 18.665279
    ## 8625  12.906914 13.667937
    ## 8626  20.400233 18.160565
    ## 8627  21.970970 18.983505
    ## 8628  21.757326 17.770520
    ## 8629  22.960512 18.783461
    ## 8630  24.994610 19.707959
    ## 8631  24.187534 19.912655
    ## 8632  21.436375 17.213431
    ## 8633  20.353668 18.047170
    ## 8634  22.118624 19.742774
    ## 8635  20.624153 19.394588
    ## 8636  11.859499 19.712839
    ## 8637  19.618017 18.873995
    ## 8638  17.607500 17.398644
    ## 8639  20.735244 19.810724
    ## 8640  16.320102 16.369325
    ## 8641  24.889285 19.636962
    ## 8642  16.235988 14.102152
    ## 8643  16.313297 16.700047
    ## 8644  14.149529 19.928154
    ## 8645  22.070050 17.604713
    ## 8646  18.443226 19.730110
    ## 8647  19.810611 19.354324
    ## 8648  22.884226 18.541623
    ## 8649  22.964100 19.936361
    ## 8650  19.476628 17.570672
    ## 8651  22.276866 18.056152
    ## 8652  25.502568 19.120944
    ## 8653  21.332843 19.716523
    ## 8654  21.553838 19.737482
    ## 8655  24.112329 19.186594
    ## 8656  18.842766 18.520155
    ## 8657  21.503985 18.051702
    ## 8658  19.026785 17.929523
    ## 8659  29.393089 19.687588
    ## 8660  28.208024 18.974010
    ## 8661  21.528544 17.981965
    ## 8662  23.744251 19.643212
    ## 8663  20.483152 17.621664
    ## 8664  23.166590 19.749835
    ## 8665  16.720306 16.990725
    ## 8666  26.242594 19.784647
    ## 8667  20.813559 19.475180
    ## 8668  22.307598 18.198833
    ## 8669  19.759610 17.476899
    ## 8670  17.695299 17.002536
    ## 8671  25.114812 18.562937
    ## 8672  22.388298 19.064977
    ## 8673  15.816942 16.432401
    ## 8674  21.660009 16.850376
    ## 8675  21.383292 19.404169
    ## 8676  19.811430 19.803032
    ## 8677  18.029364 19.820680
    ## 8678  16.003513 16.628045
    ## 8679  18.896827 19.147993
    ## 8680  19.129565 19.755005
    ## 8681  17.265956 17.128695
    ## 8682  24.271500 19.578878
    ## 8683  13.750301 19.595694
    ## 8684  17.583624 18.629830
    ## 8685  17.791664 18.488993
    ## 8686  21.195712 15.820135
    ## 8687  16.203056 17.021322
    ## 8688  21.183496 19.089371
    ## 8689  16.466712 18.147383
    ## 8690  16.513575 17.993777
    ## 8691  16.785295 12.850025
    ## 8692  17.073208 13.207863
    ## 8693  19.178072 14.756786
    ## 8694  12.463430 15.091384
    ## 8695  22.860481 19.835418
    ## 8696  26.316224 18.280219
    ## 8697  23.053103 19.122846
    ## 8698  19.086823 15.327137
    ## 8699  24.776751 19.468412
    ## 8700  26.484401 19.909695
    ## 8701  20.666893 19.633317
    ## 8702  23.453456 19.811968
    ## 8703  14.714110 15.468702
    ## 8704  14.815095 18.242622
    ## 8705  17.914901 17.334548
    ## 8706  21.963835 19.765924
    ## 8707  25.284937 19.630641
    ## 8708  21.376790 19.286338
    ## 8709  17.705886 18.551412
    ## 8710  17.849609 18.968742
    ## 8711  26.446363 19.555974
    ## 8712  19.616110 18.695899
    ## 8713  16.359292 19.486477
    ## 8714  31.753407 18.995790
    ## 8715  23.945712 17.966953
    ## 8716  20.520567 18.870037
    ## 8717  19.046699 17.378842
    ## 8718  15.195999 16.816466
    ## 8719  22.769883 19.781379
    ## 8720  19.700890 18.968819
    ## 8721  17.754129 18.789985
    ## 8722  16.183450 14.798805
    ## 8723  22.996824 19.510708
    ## 8724  25.150378 15.667359
    ## 8725  17.371173 17.372955
    ## 8726  18.518733 16.253480
    ## 8727  15.524299 14.831604
    ## 8728  13.610498 15.661414
    ## 8729  18.874228 17.118035
    ## 8730  18.247395 19.115642
    ## 8731  12.583739 12.020998
    ## 8732  19.985308 19.578210
    ## 8733  17.329496 19.840213
    ## 8734  25.846465 19.681266
    ## 8735  14.883603 17.255778
    ## 8736  18.786301 18.818305
    ## 8737  21.370289 19.364144
    ## 8738  19.631189 19.900816
    ## 8739  17.502543 18.675650
    ## 8740  27.852672 19.457921
    ## 8741  14.298834 18.523978
    ## 8742  24.219935 17.979238
    ## 8743  19.729999 15.721214
    ## 8744  22.001703 18.806052
    ## 8745  20.489086 19.754886
    ## 8746  24.811155 18.582656
    ## 8747  17.623270 19.010032
    ## 8748  16.392625 15.984282
    ## 8749  20.557099 14.610319
    ## 8750  16.465063 16.916283
    ## 8751  21.721699 17.254773
    ## 8752  16.453440 18.804360
    ## 8753  20.915828 19.799176
    ## 8754  16.987185 16.613511
    ## 8755  15.202608 19.963085
    ## 8756  23.066728 19.484249
    ## 8757  23.944769 17.334040
    ## 8758  25.178314 19.465904
    ## 8759  15.660919 18.957505
    ## 8760  18.657933 17.350151
    ## 8761  21.943464 19.461297
    ## 8762  22.670843 19.831377
    ## 8763  29.453204 19.334291
    ## 8764  18.583887 19.336233
    ## 8765  15.254662 19.767063
    ## 8766  20.506030 19.694817
    ## 8767  19.303619 17.386992
    ## 8768  18.807800 16.531175
    ## 8769  21.369434 18.028447
    ## 8770  16.143376 18.854346
    ## 8771  22.357777 17.195482
    ## 8772  20.387124 16.275711
    ## 8773  20.660593 17.381203
    ## 8774  20.132315 19.701430
    ## 8775  21.384799 17.572898
    ## 8776  12.966399 13.572898
    ## 8777  12.922932 17.522514
    ## 8778  18.416550 19.597346
    ## 8779  22.939070 18.548093
    ## 8780  22.820800 18.722159
    ## 8781  21.071301 17.958686
    ## 8782  18.212650 18.184932
    ## 8783  20.980274 18.782221
    ## 8784  18.819288 19.284063
    ## 8785  27.448854 18.650475
    ## 8786  22.441980 18.396835
    ## 8787  19.016165 17.918643
    ## 8788  23.708481 19.862020
    ## 8789  25.143239 19.119821
    ## 8790  20.154182 19.115542
    ## 8791  19.403921 19.934019
    ## 8792  17.656821 18.979339
    ## 8793  21.969343 19.338257
    ## 8794  17.673027 19.840333
    ## 8795  21.347055 18.340130
    ## 8796  13.485397 16.083205
    ## 8797  19.887148 18.706132
    ## 8798  23.023389 19.462490
    ## 8799  22.774827 18.568416
    ## 8800  20.436826 17.663730
    ## 8801  18.263980 17.338777
    ## 8802  23.024638 18.827408
    ## 8803  24.927530 18.788934
    ## 8804  21.262221 18.837734
    ## 8805  19.258759 19.697762
    ## 8806  16.415738 19.491204
    ## 8807  19.440817 19.316951
    ## 8808  21.631425 19.192935
    ## 8809  16.644310 16.959494
    ## 8810  19.744505 19.208219
    ## 8811  17.744344 19.207462
    ## 8812  22.358119 19.917751
    ## 8813  16.870141 19.387998
    ## 8814  21.153716 17.162886
    ## 8815  25.378709 19.570886
    ## 8816  21.107001 18.811646
    ## 8817  21.877964 17.771139
    ## 8818  19.195996 18.300843
    ## 8819  18.313569 18.829041
    ## 8820  21.543640 19.487097
    ## 8821  20.083268 16.837337
    ## 8822  24.814841 19.784553
    ## 8823  16.473013 17.836535
    ## 8824  11.914945 19.015986
    ## 8825  22.775664 18.991384
    ## 8826  28.230987 19.016993
    ## 8827  23.475902 19.933261
    ## 8828  23.716775 18.615129
    ## 8829  17.317628 18.031499
    ## 8830  23.994598 19.735479
    ## 8831  22.519109 19.114670
    ## 8832  17.068505 19.896266
    ## 8833  14.490502 17.119909
    ## 8834  19.429582 14.351948
    ## 8835  21.334207 16.367825
    ## 8836  18.615742 16.845387
    ## 8837  16.426792 17.444493
    ## 8838  19.892091 19.735909
    ## 8839  22.456297 19.853236
    ## 8840  14.271741 16.624770
    ## 8841  20.024072 17.886447
    ## 8842  16.991409 17.983845
    ## 8843  12.677999 18.543920
    ## 8844  18.990502 17.324356
    ## 8845  22.265694 19.805640
    ## 8846  22.133510 19.965217
    ## 8847  22.297432 17.391867
    ## 8848  23.342540 18.359508
    ## 8849  17.925598 19.065425
    ## 8850  18.884060 15.702268
    ## 8851  23.734215 17.694720
    ## 8852  20.261198 19.996667
    ## 8853  15.498510 17.604329
    ## 8854  14.385566 19.044722
    ## 8855  22.779617 19.894380
    ## 8856  21.077501 18.822657
    ## 8857  23.490243 18.352296
    ## 8858  23.340449 19.633261
    ## 8859  17.231682 19.640177
    ## 8860  18.857275 15.815575
    ## 8861  25.943424 19.480810
    ## 8862  19.938870 18.439563
    ## 8863  23.549988 19.926056
    ## 8864  16.995455 14.683580
    ## 8865  19.646118 17.028895
    ## 8866  23.445955 18.977498
    ## 8867  16.127953 18.843369
    ## 8868  20.635491 19.240709
    ## 8869  16.744560 19.358113
    ## 8870  23.714626 19.545045
    ## 8871  18.983333 19.961389
    ## 8872  24.210738 19.591347
    ## 8873  21.919885 19.694167
    ## 8874  24.506576 19.993514
    ## 8875  21.048112 18.512810
    ## 8876  13.911096 13.749134
    ## 8877  23.745254 19.215897
    ## 8878  21.219229 19.257508
    ## 8879  22.565942 19.900081
    ## 8880  27.003167 18.942937
    ## 8881  16.104970 14.553716
    ## 8882  29.415766 19.598997
    ## 8883  20.810492 16.479877
    ## 8884  14.417940 16.945208
    ## 8885  19.441049 18.290545
    ## 8886  16.689744 16.059124
    ## 8887  23.271503 19.951253
    ## 8888  19.879533 17.719327
    ## 8889  29.834051 18.825283
    ## 8890  22.587928 18.341772
    ## 8891  12.356672 15.133634
    ## 8892  16.617195 15.567420
    ## 8893  23.299646 18.155446
    ## 8894  19.130958 19.607775
    ## 8895  17.850229 19.910832
    ## 8896  16.897247 17.391772
    ## 8897  27.333181 19.835689
    ## 8898  16.510577 14.742939
    ## 8899  19.675625 19.017865
    ## 8900  17.673770 18.321694
    ## 8901  25.525378 19.421021
    ## 8902  22.408698 19.531658
    ## 8903  20.275267 19.616806
    ## 8904  17.345495 15.213387
    ## 8905  17.601020 18.281362
    ## 8906  22.442268 18.500238
    ## 8907  27.808655 19.832979
    ## 8908  20.048926 19.368097
    ## 8909  23.958597 19.597975
    ## 8910  17.916576 18.082172
    ## 8911  22.085704 18.426159
    ## 8912  21.207027 19.408097
    ## 8913  27.236573 19.944494
    ## 8914  13.297926 19.115154
    ## 8915  15.000607 16.564110
    ## 8916  11.515715 12.169473
    ## 8917  21.011233 19.907979
    ## 8918  24.195014 19.392488
    ## 8919  14.851199 19.152789
    ## 8920  15.434971 17.460169
    ## 8921  22.732040 18.912580
    ## 8922  17.006117 17.347126
    ## 8923  17.825382 19.178387
    ## 8924  22.477158 19.733847
    ## 8925  26.017318 19.166119
    ## 8926  21.358479 19.466179
    ## 8927  22.332140 19.864149
    ## 8928  24.884743 18.264575
    ## 8929  19.632859 18.443646
    ## 8930  22.266484 18.833733
    ## 8931  25.528991 19.693921
    ## 8932  20.090547 19.431494
    ## 8933  18.586004 17.385824
    ## 8934  21.339684 19.501033
    ## 8935  18.272037 19.297929
    ## 8936  15.779773 16.682864
    ## 8937  14.915759 19.215465
    ## 8938  20.039867 16.180142
    ## 8939  17.787316 16.252951
    ## 8940  23.847587 19.692256
    ## 8941  16.722754 16.247042
    ## 8942  17.867238 18.328514
    ## 8943  13.753482 19.391093
    ## 8944  15.387748 17.323347
    ## 8945  19.452610 19.692577
    ## 8946  13.271104 15.811923
    ## 8947  17.229707 18.881664
    ## 8948  15.867192 15.189702
    ## 8949  19.701910 17.562034
    ## 8950  15.514226 15.157487
    ## 8951  19.249659 17.746865
    ## 8952  17.280676 18.704923
    ## 8953  22.578887 18.391466
    ## 8954  17.538544 16.734420
    ## 8955  21.494445 19.416647
    ## 8956  29.418318 19.297194
    ## 8957  19.476325 17.993018
    ## 8958  19.272752 19.802901
    ## 8959  21.372421 16.501458
    ## 8960  16.479908 14.372420
    ## 8961  14.202532 16.930781
    ## 8962  15.587156 17.114811
    ## 8963  12.664163 10.531078
    ## 8964  11.288189 13.663538
    ## 8965  20.710305 19.307059
    ## 8966  21.819920 19.810272
    ## 8967  19.561978 18.622470
    ## 8968  19.636833 17.809393
    ## 8969  22.631779 19.896212
    ## 8970  20.251098 17.420540
    ## 8971  21.486382 19.111431
    ## 8972  21.259940 15.159072
    ## 8973  20.367111 19.775336
    ## 8974  20.657385 19.768852
    ## 8975  23.606384 16.669074
    ## 8976  22.052792 19.040648
    ## 8977  23.789441 19.378409
    ## 8978  26.965371 19.092127
    ## 8979  23.876326 18.610886
    ## 8980  19.704036 19.992168
    ## 8981  17.402766 18.048009
    ## 8982  24.515961 17.346385
    ## 8983  17.639786 19.750539
    ## 8984  17.847683 18.933698
    ## 8985  17.818110 17.497982
    ## 8986  19.639280 19.100034
    ## 8987  20.611159 19.005595
    ## 8988  18.226432 19.197529
    ## 8989  19.118421 19.873114
    ## 8990  21.781314 19.648424
    ## 8991  15.676350 16.376707
    ## 8992  23.161443 18.631269
    ## 8993  21.126386 18.411570
    ## 8994  24.404043 19.785303
    ## 8995  24.182903 17.140892
    ## 8996  21.282854 19.796059
    ## 8997  15.016612 16.125790
    ## 8998  16.793091 17.973600
    ## 8999  17.559569 19.938860
    ## 9000  19.213637 18.677296
    ## 9001  22.519374 18.556467
    ## 9002  24.860933 19.781045
    ## 9003  19.921715 19.123560
    ## 9004  16.338230 15.674140
    ## 9005  10.431968 11.682286
    ## 9006  25.316211 18.485687
    ## 9007  21.051075 19.066777
    ## 9008  27.347907 19.639844
    ## 9009  18.569466 17.475407
    ## 9010  20.314752 17.880071
    ## 9011  13.051630 11.130318
    ## 9012  23.830385 19.094707
    ## 9013  21.376122 16.067099
    ## 9014  23.706981 19.822831
    ## 9015  16.589037 19.477246
    ## 9016  27.769419 19.225750
    ## 9017  25.523277 18.334041
    ## 9018  17.821149 18.816670
    ## 9019  16.000414 13.752931
    ## 9020  26.382866 19.382467
    ## 9021  20.148808 17.077090
    ## 9022  21.115541 17.194909
    ## 9023  27.229580 17.600993
    ## 9024  22.133551 17.723696
    ## 9025  18.267023 16.721188
    ## 9026  22.570595 19.941669
    ## 9027  12.352604 15.718394
    ## 9028  19.197564 18.933116
    ## 9029  13.954501 13.654171
    ## 9030  11.756369 13.411703
    ## 9031  18.533055 19.821076
    ## 9032  24.380921 18.641897
    ## 9033  18.955927 18.575478
    ## 9034  24.261465 19.817902
    ## 9035  19.340974 19.834050
    ## 9036  16.542765 18.836022
    ## 9037  24.074165 15.585284
    ## 9038  22.415987 18.800996
    ## 9039  13.519407 18.645149
    ## 9040  16.055217 16.468543
    ## 9041  27.581256 19.595868
    ## 9042  14.960467 16.717789
    ## 9043  17.764483 18.427255
    ## 9044  24.527608 19.713010
    ## 9045  23.417838 18.778331
    ## 9046  18.566365 14.833754
    ## 9047   9.875346 12.196365
    ## 9048  24.866707 19.688833
    ## 9049  22.157514 18.531212
    ## 9050  18.226193 19.111391
    ## 9051  19.550870 19.952194
    ## 9052  25.584074 19.269997
    ## 9053  24.259276 19.029935
    ## 9054  15.513409 19.707134
    ## 9055  16.822838 15.859889
    ## 9056  28.509117 19.825190
    ## 9057  14.818097 18.995819
    ## 9058  16.422721 19.891921
    ## 9059  17.243294 18.278203
    ## 9060  16.632189 16.298922
    ## 9061  25.690241 19.603852
    ## 9062  19.827039 18.052843
    ## 9063  17.930361 13.201645
    ## 9064  10.639118 19.510913
    ## 9065  22.078743 16.816614
    ## 9066  20.037140 18.541035
    ## 9067  17.898366 19.170202
    ## 9068  20.415336 19.447269
    ## 9069  21.530934 18.025284
    ## 9070  13.649385 15.293628
    ## 9071  19.854250 16.482602
    ## 9072  25.143748 18.221476
    ## 9073  18.215229 19.075545
    ## 9074  21.208745 19.808514
    ## 9075  19.896299 19.633445
    ## 9076  15.619533 18.224163
    ## 9077  21.463896 17.139645
    ## 9078  18.411289 17.240896
    ## 9079  26.095403 18.806918
    ## 9080  17.625929 18.875661
    ## 9081  17.113543 16.389508
    ## 9082  15.884924 19.964694
    ## 9083  15.971226 18.829380
    ## 9084  13.295839 15.606915
    ## 9085  17.995024 16.261107
    ## 9086  21.658757 19.781800
    ## 9087  14.412756 13.952586
    ## 9088  16.332113 16.610242
    ## 9089  18.391722 18.706438
    ## 9090  22.261739 17.350739
    ## 9091  15.463387 16.572850
    ## 9092  27.026595 19.844921
    ## 9093  23.985742 19.213926
    ## 9094  20.202077 17.444217
    ## 9095  17.671137 18.996693
    ## 9096  23.903754 19.872714
    ## 9097  18.310739 19.707967
    ## 9098  24.785438 18.830527
    ## 9099  27.001557 19.749103
    ## 9100  17.120180 18.126026
    ## 9101  18.721714 18.938703
    ## 9102  24.188417 18.817168
    ## 9103  23.856136 19.290933
    ## 9104  19.959721 18.553350
    ## 9105  16.692042 18.366633
    ## 9106  16.774095 16.251870
    ## 9107  20.134488 18.199018
    ## 9108   8.838287 10.787456
    ## 9109  20.369898 15.898137
    ## 9110  19.653930 17.942412
    ## 9111  19.875561 19.602318
    ## 9112  20.455772 18.237065
    ## 9113  17.522319 16.716961
    ## 9114  14.948185 19.363175
    ## 9115  23.991191 19.782552
    ## 9116  20.239921 19.854875
    ## 9117  13.296590 16.595782
    ## 9118  15.301740 19.536716
    ## 9119  27.321756 19.444103
    ## 9120  21.338971 18.669489
    ## 9121  14.498607 14.538020
    ## 9122  19.053256 19.787201
    ## 9123  22.234162 17.549650
    ## 9124  18.615289 19.757019
    ## 9125  19.975128 19.458810
    ## 9126  15.502004 16.648230
    ## 9127  17.423687 19.866584
    ## 9128  18.416786 16.649271
    ## 9129  20.960991 19.840154
    ## 9130  17.143170 17.452472
    ## 9131   9.829176 12.244982
    ## 9132  21.787697 19.296603
    ## 9133  18.816649 19.350261
    ## 9134  23.972906 18.085888
    ## 9135  18.862402 19.792229
    ## 9136  22.658952 19.629507
    ## 9137  17.616183 19.067355
    ## 9138  22.329120 16.056815
    ## 9139  21.675402 19.261295
    ## 9140  19.630101 15.720250
    ## 9141  21.775149 19.764193
    ## 9142  24.381612 19.826231
    ## 9143  15.844457 17.959541
    ## 9144  17.819319 17.333148
    ## 9145  16.678076 15.412568
    ## 9146  19.081435 18.020035
    ## 9147  27.347881 19.944713
    ## 9148  19.444458 17.904296
    ## 9149  18.222871 17.296410
    ## 9150  15.770583 17.838501
    ## 9151  17.946344 15.876593
    ## 9152  23.840747 19.877191
    ## 9153  19.435187 18.980126
    ## 9154  20.864172 19.656742
    ## 9155  21.540919 19.899485
    ## 9156  20.121800 19.089791
    ## 9157  25.490725 19.603788
    ## 9158  18.771987 19.755315
    ## 9159  23.039179 19.112882
    ## 9160  24.386447 19.279837
    ## 9161  18.369843 17.465313
    ## 9162  17.104857 17.488411
    ## 9163  26.259193 19.877401
    ## 9164  20.618115 19.351124
    ## 9165  26.302708 19.804806
    ## 9166  23.535229 19.954506
    ## 9167  19.296552 17.342086
    ## 9168  15.290286 17.431134
    ## 9169  15.917726 16.820966
    ## 9170  19.610423 17.896737
    ## 9171  24.526424 19.541385
    ## 9172  19.389481 17.965992
    ## 9173  14.133605 17.475676
    ## 9174  17.712661 19.135213
    ## 9175  19.959876 17.959554
    ## 9176  12.145069 16.890083
    ## 9177  19.639283 18.605630
    ## 9178  21.135479 19.422903
    ## 9179  11.569160 18.520640
    ## 9180  23.594189 19.570487
    ## 9181  16.314426 19.333490
    ## 9182  13.518985 16.097699
    ## 9183  19.407754 17.416881
    ## 9184  22.282939 18.341946
    ## 9185  18.614342 17.163606
    ## 9186  21.255184 19.269847
    ## 9187  18.487295 19.052867
    ## 9188  23.482862 19.981925
    ## 9189  18.925605 15.621290
    ## 9190  16.387463 18.381934
    ## 9191  18.846956 16.717168
    ## 9192  16.852186 19.326036
    ## 9193  26.373512 18.445789
    ## 9194  26.842926 18.756311
    ## 9195  24.323561 17.334884
    ## 9196  25.793317 19.552467
    ## 9197  15.237458 19.538440
    ## 9198  19.755658 19.641869
    ## 9199  20.090299 15.737104
    ## 9200  19.400682 18.641995
    ## 9201  21.442094 19.340084
    ## 9202  24.345502 17.379049
    ## 9203  15.086795 18.557423
    ## 9204  22.211029 19.327607
    ## 9205  22.886780 17.065409
    ## 9206  28.368410 19.953649
    ## 9207  17.946312 17.485462
    ## 9208  21.006265 17.757862
    ## 9209  14.770565 16.903210
    ## 9210  23.894385 18.256750
    ## 9211  22.221607 18.527649
    ## 9212  25.119825 19.523549
    ## 9213  19.405532 16.995404
    ## 9214  15.866227 19.794656
    ## 9215  21.069800 19.432528
    ## 9216  19.312747 19.207984
    ## 9217  20.942758 18.318102
    ## 9218  16.562655 19.002788
    ## 9219  18.305429 18.196876
    ## 9220  22.600802 19.448801
    ## 9221  18.078868 19.159861
    ## 9222  21.762359 19.735301
    ## 9223  16.222189 18.346872
    ## 9224  16.186178 16.862599
    ## 9225  15.189411 18.658025
    ## 9226  21.792015 16.596146
    ## 9227  24.890638 19.693422
    ## 9228  27.883969 19.932839
    ## 9229  23.984443 17.617106
    ## 9230  15.286503 14.562075
    ## 9231  18.972469 14.334113
    ## 9232  17.150351 17.284554
    ## 9233  16.330237 19.136169
    ## 9234  18.450873 19.025651
    ## 9235  16.920641 18.070823
    ## 9236  20.382141 16.096709
    ## 9237  16.415364 14.387462
    ## 9238  20.997970 19.990615
    ## 9239  13.384456 11.900261
    ## 9240  21.878042 19.630405
    ## 9241  17.060185 19.034632
    ## 9242  14.584506 16.069490
    ## 9243  10.355759 17.376696
    ## 9244  22.011253 18.770147
    ## 9245  14.708835 17.443256
    ## 9246  22.024286 19.267024
    ## 9247  20.154265 19.056378
    ## 9248  21.600718 19.699393
    ## 9249  19.364592 18.983614
    ## 9250  30.059193 19.287351
    ## 9251  17.540997 19.409649
    ## 9252  19.422167 19.492438
    ## 9253  23.249268 19.966926
    ## 9254  16.375326 15.160731
    ## 9255  17.017638 18.033945
    ## 9256  25.402215 17.654154
    ## 9257  18.396782 17.760527
    ## 9258  20.382409 19.184381
    ## 9259   9.932618 15.836667
    ## 9260  24.544291 19.246965
    ## 9261  17.687360 14.452910
    ## 9262  23.487954 19.073375
    ## 9263  26.805379 19.562861
    ## 9264  15.002088 16.720149
    ## 9265  24.045743 19.777243
    ## 9266  19.945403 17.122499
    ## 9267  14.713669 19.538118
    ## 9268  22.090994 18.064883
    ## 9269  15.470056 17.208329
    ## 9270  19.633262 19.390709
    ## 9271  20.224371 19.812486
    ## 9272  21.024441 19.849275
    ## 9273  17.417497 17.980273
    ## 9274  21.622323 17.615766
    ## 9275  21.123999 15.761816
    ## 9276  19.639388 18.031696
    ## 9277  18.092207 18.796041
    ## 9278  17.356959 16.515825
    ## 9279  12.846069 18.113774
    ## 9280  21.912760 19.875766
    ## 9281  18.209614 18.756151
    ## 9282  18.389446 17.908155
    ## 9283  21.580220 18.614085
    ## 9284  20.713720 18.414797
    ## 9285  21.628068 19.899337
    ## 9286  16.875943 17.502958
    ## 9287  24.463621 18.327011
    ## 9288  17.285924 17.204700
    ## 9289  24.794856 19.313617
    ## 9290  20.638381 17.090340
    ## 9291  23.252739 19.992299
    ## 9292  19.252151 16.264067
    ## 9293  24.191407 19.753352
    ## 9294  17.932410 19.686373
    ## 9295  21.584012 16.985989
    ## 9296  21.272604 18.984929
    ## 9297  22.066181 19.246311
    ## 9298  22.578434 16.185990
    ## 9299  25.970190 19.608332
    ## 9300  19.144855 17.524303
    ## 9301  20.050815 18.739946
    ## 9302  25.793352 19.338931
    ## 9303  17.573410 19.665595
    ## 9304  21.799262 18.386838
    ## 9305  20.319867 19.623555
    ## 9306  23.906757 18.846093
    ## 9307  23.249218 17.844060
    ## 9308  12.673162 15.950706
    ## 9309  17.204411 17.040190
    ## 9310  25.969880 19.935846
    ## 9311  16.892755 18.656455
    ## 9312  18.763613 17.097750
    ## 9313  23.203168 19.601135
    ## 9314  17.009990 19.979821
    ## 9315  24.972326 19.935324
    ## 9316  26.300783 17.731696
    ## 9317  24.787451 19.825157
    ## 9318  19.951724 19.758799
    ## 9319  20.564477 18.791407
    ## 9320  21.378111 18.912753
    ## 9321  23.715405 19.096814
    ## 9322  17.731906 16.827303
    ## 9323  23.169944 19.830553
    ## 9324  14.714892 15.912651
    ## 9325  18.064475 19.898146
    ## 9326  16.706113 19.139573
    ## 9327  13.792943 15.882582
    ## 9328  19.654810 17.630197
    ## 9329  22.898004 17.761910
    ## 9330  15.355722 17.065444
    ## 9331  13.198144 19.494873
    ## 9332  17.423785 18.470746
    ## 9333  15.717530 19.626411
    ## 9334  25.958333 19.156511
    ## 9335  24.399932 19.255331
    ## 9336  22.074912 18.320350
    ## 9337  16.886457 19.571784
    ## 9338  20.264923 19.829904
    ## 9339  20.607520 18.453896
    ## 9340  20.817503 19.923386
    ## 9341  25.641503 18.583496
    ## 9342  22.050185 18.916271
    ## 9343  20.487723 18.788005
    ## 9344  27.195999 17.922455
    ## 9345  17.787261 17.779291
    ## 9346  22.016870 17.304217
    ## 9347  18.753506 18.332867
    ## 9348  13.933201 15.261331
    ## 9349  21.848326 18.889617
    ## 9350  22.188174 17.183061
    ## 9351  31.382675 19.126162
    ## 9352  19.851653 17.924652
    ## 9353  17.552561 19.763741
    ## 9354  18.730537 17.711028
    ## 9355  20.517028 19.383282
    ## 9356  17.431332 17.944613
    ## 9357  18.006957 18.581256
    ## 9358  18.269936 16.025851
    ## 9359  21.829613 19.094225
    ## 9360  11.194564 17.568665
    ## 9361  18.392110 15.361927
    ## 9362  21.870269 18.959046
    ## 9363  18.957179 19.317035
    ## 9364  23.738234 19.331655
    ## 9365  19.142292 19.516820
    ## 9366  21.983894 19.828155
    ## 9367  21.164841 19.831979
    ## 9368  20.580442 18.951480
    ## 9369  20.691575 18.514764
    ## 9370  17.073397 18.604793
    ## 9371  16.124487 18.123543
    ## 9372  25.934999 19.616165
    ## 9373  22.512511 17.871971
    ## 9374  23.384338 17.686727
    ## 9375  19.906967 19.815731
    ## 9376  16.414085 15.691815
    ## 9377  24.056128 19.907237
    ## 9378  18.171334 18.986096
    ## 9379  20.675999 19.881658
    ## 9380  16.827511 13.969176
    ## 9381  22.650832 17.603866
    ## 9382  23.952121 18.779489
    ## 9383  24.548481 19.950267
    ## 9384  17.390843 19.107702
    ## 9385  25.576687 19.429873
    ## 9386  18.197178 19.552557
    ## 9387  17.537333 16.425713
    ## 9388  26.056106 18.868221
    ## 9389  25.830395 19.827209
    ## 9390  17.466419 16.912741
    ## 9391  17.550514 18.136750
    ## 9392  18.293337 14.244759
    ## 9393  16.420740 19.999599
    ## 9394  24.309771 19.404756
    ## 9395  18.509760 17.962326
    ## 9396  25.219990 18.110804
    ## 9397  21.301483 17.681960
    ## 9398  20.342827 18.502998
    ## 9399  20.710544 18.435483
    ## 9400  26.438988 19.451417
    ## 9401  15.047475 17.359620
    ## 9402  15.913181 14.645390
    ## 9403  22.986826 19.362364
    ## 9404  22.676539 16.196626
    ## 9405  25.658670 17.998247
    ## 9406  17.097090 17.216252
    ## 9407  27.275155 19.868926
    ## 9408  21.269150 19.706163
    ## 9409  14.473353 17.184929
    ## 9410  17.684932 19.753048
    ## 9411  24.716682 19.285850
    ## 9412  21.011959 17.958593
    ## 9413  21.464422 19.219920
    ## 9414  16.681632 16.797978
    ## 9415  15.792909 18.659330
    ## 9416  22.356011 18.521249
    ## 9417  13.809492 13.151336
    ## 9418  19.742336 17.603366
    ## 9419  23.843114 19.805456
    ## 9420  20.631724 18.445690
    ## 9421  22.679298 18.117500
    ## 9422  21.750998 18.391122
    ## 9423  16.278709 17.501798
    ## 9424  20.264412 18.688391
    ## 9425  23.059756 19.650111
    ## 9426  20.444904 17.929875
    ## 9427  18.439386 19.963544
    ## 9428  19.592452 18.551303
    ## 9429  15.239757 19.913515
    ## 9430  23.095605 17.065621
    ## 9431  16.396669 17.920259
    ## 9432  22.804403 19.933901
    ## 9433  21.276794 18.401626
    ## 9434  19.359083 19.053675
    ## 9435  18.390138 19.803434
    ## 9436  20.701710 19.104616
    ## 9437  27.245329 19.386271
    ## 9438  16.591004 19.950071
    ## 9439  22.775368 17.695203
    ## 9440  23.720421 19.699118
    ## 9441  20.247111 19.939201
    ## 9442  15.479557 17.232432
    ## 9443  20.448760 15.246077
    ## 9444  20.235271 17.852881
    ## 9445  18.187344 17.487478
    ## 9446  22.045961 16.674845
    ## 9447  22.321794 18.586765
    ## 9448  19.768708 18.040404
    ## 9449  23.874986 18.952009
    ## 9450  19.136425 19.239389
    ## 9451  21.486552 16.679660
    ## 9452  18.736994 19.845130
    ## 9453  16.179754 17.462080
    ## 9454  17.946024 17.215449
    ## 9455  17.904765 17.487582
    ## 9456  19.910580 17.850595
    ## 9457  18.371346 15.758452
    ## 9458  19.368808 18.055506
    ## 9459  18.107537 13.908186
    ## 9460  21.428925 17.189126
    ## 9461  16.698160 16.341861
    ## 9462  18.928269 18.003820
    ## 9463  19.827465 17.051215
    ## 9464  16.002715 17.596201
    ## 9465  19.011230 18.877419
    ## 9466  23.226614 19.934603
    ## 9467  25.444112 19.638794
    ## 9468  22.126625 19.467133
    ## 9469  17.808374 17.980983
    ## 9470  18.961747 19.196635
    ## 9471  14.632404 19.218549
    ## 9472  23.911384 19.584406
    ## 9473  15.313626 16.905056
    ## 9474  24.876720 18.354356
    ## 9475  17.047166 18.103408
    ## 9476  24.496816 18.655680
    ## 9477  18.192894 16.910973
    ## 9478  24.102887 19.102348
    ## 9479  25.502274 16.181475
    ## 9480  24.788180 19.049881
    ## 9481  17.033470 17.186461
    ## 9482  15.091563 19.413242
    ## 9483  17.046246 17.047843
    ## 9484  17.255544 19.418906
    ## 9485  26.943596 18.641982
    ## 9486  20.254015 19.658538
    ## 9487  19.629371 19.952649
    ## 9488  13.713081 19.014385
    ## 9489  20.780464 18.764132
    ## 9490  25.344799 19.678007
    ## 9491  23.450931 19.626779
    ## 9492  15.266483 15.074083
    ## 9493  25.272905 18.311256
    ## 9494  14.794445 13.191263
    ## 9495  20.365720 17.890700
    ## 9496  17.688617 19.674826
    ## 9497  18.969006 16.427498
    ## 9498  23.160824 19.288957
    ## 9499  13.702234 18.431474
    ## 9500  17.412616 19.668741
    ## 9501  19.337502 19.754778
    ## 9502  19.759670 18.160040
    ## 9503  17.748142 18.900094
    ## 9504  21.798166 17.285791
    ## 9505  18.570629 17.315812
    ## 9506  18.645844 13.611112
    ## 9507  20.468624 19.631257
    ## 9508  25.714896 19.002897
    ## 9509  18.313167 19.493032
    ## 9510  24.566784 19.763324
    ## 9511  14.894410 12.837913
    ## 9512  17.444928 15.049245
    ## 9513  24.247912 19.313293
    ## 9514  26.260673 19.468237
    ## 9515  23.812487 19.290081
    ## 9516  17.952704 14.948559
    ## 9517  18.875639 15.869710
    ## 9518  26.216585 18.790236
    ## 9519  24.159142 18.637761
    ## 9520  16.226090 18.353007
    ## 9521  23.445555 19.192738
    ## 9522  14.840349 18.460388
    ## 9523  22.707387 18.836151
    ## 9524  15.640865 15.769193
    ## 9525  18.216574 19.428287
    ## 9526  19.478068 19.026161
    ## 9527  13.232190 13.734348
    ## 9528  24.344864 19.985160
    ## 9529  24.508127 18.698602
    ## 9530  16.745127 14.491995
    ## 9531  20.997619 18.293856
    ## 9532  15.858804 16.896771
    ## 9533  22.971107 18.758247
    ## 9534  19.789079 19.554511
    ## 9535  22.481752 19.318369
    ## 9536  22.048091 19.410208
    ## 9537  14.550369 19.300129
    ## 9538  19.752518 18.915879
    ## 9539  18.390977 19.669713
    ## 9540  21.678248 19.495190
    ## 9541  24.689840 18.912712
    ## 9542  17.974983 17.092164
    ## 9543  16.338690 17.892787
    ## 9544  23.949339 19.016471
    ## 9545  15.855941 18.273410
    ## 9546  15.641055 12.254361
    ## 9547  19.469952 17.656664
    ## 9548  14.630348 15.151292
    ## 9549  24.981570 18.151293
    ## 9550  18.070238 18.976113
    ## 9551  19.318122 17.630491
    ## 9552  23.726000 19.547992
    ## 9553  20.473443 19.484989
    ## 9554  20.807088 16.581414
    ## 9555  17.118380 16.830632
    ## 9556  20.365083 18.373590
    ## 9557  12.497790 19.094250
    ## 9558  25.790785 19.918200
    ## 9559  15.978943 15.975161
    ## 9560  17.318363 19.138290
    ## 9561  25.383187 18.990142
    ## 9562  21.358733 19.784614
    ## 9563  15.784228 18.588851
    ## 9564  22.183630 19.686288
    ## 9565  18.063359 15.868126
    ## 9566  19.299029 15.032322
    ## 9567  24.484708 19.429501
    ## 9568  23.753177 19.726568
    ## 9569  24.454874 19.050715
    ## 9570  19.473980 19.609853
    ## 9571  20.408307 16.452244
    ## 9572  24.482660 19.301919
    ## 9573  15.677184 18.418835
    ## 9574  16.494721 14.209823
    ## 9575  17.361026 17.545728
    ## 9576  21.884475 18.205802
    ## 9577  16.229786 19.257416
    ## 9578  18.561906 16.228454
    ## 9579  19.446514 18.388330
    ## 9580  22.574695 18.197258
    ## 9581  23.245599 18.444986
    ## 9582  15.841598 13.632424
    ## 9583  13.269154 19.005885
    ## 9584  22.554008 18.914886
    ## 9585  20.222725 17.190973
    ## 9586  13.986172 13.087283
    ## 9587  18.159529 19.382767
    ## 9588  20.260935 15.423641
    ## 9589  16.320651 18.622464
    ## 9590  24.562098 19.611761
    ## 9591  18.580954 16.321729
    ## 9592  22.080812 19.992713
    ## 9593  23.733342 18.661296
    ## 9594  17.861592 16.939185
    ## 9595  21.309780 19.978483
    ## 9596  17.789920 16.330799
    ## 9597  29.473241 19.954554
    ## 9598  17.849435 17.484610
    ## 9599  14.295447 13.939577
    ## 9600  19.726444 19.160197
    ## 9601  28.832462 19.195270
    ## 9602  25.692426 17.698128
    ## 9603  25.260107 19.348357
    ## 9604  23.865364 16.501154
    ## 9605  24.846385 19.088120
    ## 9606  20.400753 19.169671
    ## 9607  19.144075 19.113696
    ## 9608  17.823743 19.443202
    ## 9609  18.303494 17.778914
    ## 9610  21.570225 18.296059
    ## 9611  16.999020 17.447686
    ## 9612  18.197288 18.972649
    ## 9613  25.228627 19.866416
    ## 9614  15.501839 13.675839
    ## 9615  15.134155 17.358981
    ## 9616  17.650097 18.292296
    ## 9617  22.469188 19.937638
    ## 9618  21.951879 18.941109
    ## 9619  16.323889 19.620170
    ## 9620  15.375392 17.440700
    ## 9621  18.396692 15.805280
    ## 9622  23.227672 19.399620
    ## 9623  14.360252 16.169894
    ## 9624  14.680192 17.230054
    ## 9625  21.216018 18.753847
    ## 9626  16.504196 18.295694
    ## 9627  20.982361 16.359461
    ## 9628  23.530901 18.929004
    ## 9629  19.939422 18.666564
    ## 9630  17.814952 19.654150
    ## 9631  16.071576 19.329980
    ## 9632  19.947932 15.014271
    ## 9633  17.746825 16.411302
    ## 9634  25.946862 18.600137
    ## 9635  17.035177 15.442900
    ## 9636  20.423661 18.398048
    ## 9637  17.984455 17.720975
    ## 9638  14.211476 18.901564
    ## 9639  26.674153 18.407213
    ## 9640  16.474535 15.631580
    ## 9641  19.503191 16.916824
    ## 9642  17.155225 16.075524
    ## 9643  18.976384 19.766592
    ## 9644  14.370930 15.647974
    ## 9645  17.985795 15.613273
    ## 9646  20.631275 16.290862
    ## 9647  21.166625 18.769438
    ## 9648  18.388119 18.071209
    ## 9649  22.101444 17.125051
    ## 9650  20.745221 19.343662
    ## 9651  20.607299 19.616070
    ## 9652  19.132875 19.386142
    ## 9653  23.725977 17.376942
    ## 9654  19.799884 19.706584
    ## 9655  18.480603 19.914918
    ## 9656  27.240059 19.992738
    ## 9657  21.748989 19.990139
    ## 9658  22.068740 19.014056
    ## 9659  22.956209 19.629161
    ## 9660  19.035918 18.873335
    ## 9661  21.825600 19.568269
    ## 9662  21.376526 19.334650
    ## 9663  23.362984 19.552472
    ## 9664  17.936928 18.985753
    ## 9665  23.558503 18.758484
    ## 9666  23.966103 19.430706
    ## 9667  20.355130 17.897893
    ## 9668  14.931332 18.349344
    ## 9669  22.308342 18.477838
    ## 9670  21.128624 19.309088
    ## 9671  26.749998 19.126233
    ## 9672  16.848403 15.955391
    ## 9673  22.383866 19.381371
    ## 9674  23.982525 19.628410
    ## 9675  17.107954 15.675447
    ## 9676  22.965259 19.031281
    ## 9677  14.120198 14.927175
    ## 9678  14.548204 16.402716
    ## 9679  19.532080 18.226264
    ## 9680  22.823978 17.569565
    ## 9681  23.383378 17.904837
    ## 9682  18.542147 17.590407
    ## 9683  19.803213 19.492811
    ## 9684  26.145700 19.227350
    ## 9685  18.905806 18.557146
    ## 9686  16.094089 18.930403
    ## 9687  19.685343 19.777480
    ## 9688  18.325192 15.111789
    ## 9689  19.030574 17.464729
    ## 9690  25.608785 19.667761
    ## 9691  17.646220 17.000275
    ## 9692  27.240318 19.776849
    ## 9693  23.086325 19.645214
    ## 9694  23.367446 19.677148
    ## 9695  25.106170 19.994313
    ## 9696  22.555397 18.454082
    ## 9697  19.745927 19.577429
    ## 9698  23.835978 19.058411
    ## 9699  19.632008 17.281125
    ## 9700  25.334415 18.256702
    ## 9701  18.688291 18.772802
    ## 9702  24.758103 17.898863
    ## 9703  22.757840 19.551926
    ## 9704  19.905213 19.957989
    ## 9705  24.988304 17.712337
    ## 9706  15.951796 15.547668
    ## 9707  19.140274 19.451847
    ## 9708  21.674183 18.813250
    ## 9709  19.115813 19.276781
    ## 9710  21.748994 18.173840
    ## 9711  16.787939 17.648359
    ## 9712  13.237857 17.708080
    ## 9713  22.983680 19.366429
    ## 9714  23.009798 19.502083
    ## 9715  26.435404 19.815160
    ## 9716  19.682397 17.089886
    ## 9717  18.174267 17.484949
    ## 9718  17.098537 17.245555
    ## 9719  19.687124 18.022522
    ## 9720  17.630289 17.699369
    ## 9721  20.344536 17.810791
    ## 9722  27.991454 18.868195
    ## 9723  13.735280 17.573117
    ## 9724  19.953405 18.034243
    ## 9725  22.982433 17.035321
    ## 9726  18.968946 19.177299
    ## 9727  21.328874 19.193854
    ## 9728  19.672967 18.078499
    ## 9729  22.622410 18.077848
    ## 9730  14.266356 11.400357
    ## 9731  17.329073 19.928659
    ## 9732  22.905751 19.315225
    ## 9733  16.177075 17.219641
    ## 9734  24.289595 18.352244
    ## 9735  22.883083 18.557266
    ## 9736  17.655693 16.859426
    ## 9737  16.717637 17.282465
    ## 9738  29.507967 19.540661
    ## 9739  11.281855 12.635445
    ## 9740  21.770140 19.951946
    ## 9741  19.743181 19.960307
    ## 9742  25.057484 18.416766
    ## 9743  23.181717 19.474702
    ## 9744  22.543605 18.796452
    ## 9745  20.201132 17.493348
    ## 9746  25.903864 18.759978
    ## 9747  16.271740 15.942517
    ## 9748  18.348650 15.938504
    ## 9749  13.130205 18.200862
    ## 9750  23.102143 19.328387
    ## 9751  22.163726 18.225295
    ## 9752  18.185534 19.539643
    ## 9753  24.296997 18.861187
    ## 9754  19.657494 16.398449
    ## 9755  18.004837 16.699063
    ## 9756  21.056656 15.678928
    ## 9757  23.563558 19.615652
    ## 9758  22.123873 19.039951
    ## 9759  26.209391 17.989326
    ## 9760  18.346381 17.626615
    ## 9761  22.825052 17.562917
    ## 9762  25.102586 19.712650
    ## 9763  22.491338 18.282044
    ## 9764  22.563015 18.712659
    ## 9765  23.410807 19.526236
    ## 9766  14.961498 19.801643
    ## 9767  15.979465 19.689795
    ## 9768  17.065027 19.627919
    ## 9769  18.720654 17.799480
    ## 9770  24.891522 19.468505
    ## 9771  19.359001 18.645503
    ## 9772  20.345886 19.929970
    ## 9773  16.980894 19.833777
    ## 9774  20.852309 19.874272
    ## 9775  21.489133 19.252620
    ## 9776  21.588420 19.546820
    ## 9777  16.465344 18.197349
    ## 9778  18.277422 18.755762
    ## 9779  12.028097 12.474637
    ## 9780  17.481050 17.123602
    ## 9781  19.076192 19.834939
    ## 9782  23.546456 17.622934
    ## 9783  30.397636 19.561114
    ## 9784  20.678405 18.886081
    ## 9785  18.287987 18.562467
    ## 9786  11.734567 18.762745
    ## 9787  14.426358 17.196820
    ## 9788  23.834561 19.389779
    ## 9789  30.566913 18.778201
    ## 9790  16.352292 15.346860
    ## 9791  17.015961 17.706600
    ## 9792  19.486565 19.388073
    ## 9793  21.535369 18.916619
    ## 9794  20.398140 18.406850
    ## 9795  20.856947 19.476191
    ## 9796  19.950519 19.963139
    ## 9797  24.803905 18.474052
    ## 9798  18.305548 19.063828
    ## 9799  16.908551 19.003476
    ## 9800  19.813895 19.568742
    ## 9801  19.625205 19.370762
    ## 9802  20.016617 18.147360
    ## 9803   7.075753  7.301681
    ## 9804  18.270156 17.825148
    ## 9805  20.308998 19.003512
    ## 9806  15.098786 16.042551
    ## 9807  17.999585 16.645446
    ## 9808  13.951671 19.921144
    ## 9809  21.137839 17.078788
    ## 9810  20.576873 17.344247
    ## 9811  18.264899 19.453721
    ## 9812  17.425210 14.040979
    ## 9813  14.497349 13.458433
    ## 9814  20.825225 18.180587
    ## 9815  21.383500 18.307843
    ## 9816  19.649385 19.847724
    ## 9817  23.395858 18.531522
    ## 9818  19.213253 19.253422
    ## 9819  22.254818 19.390447
    ## 9820  26.504649 19.991684
    ## 9821  20.342443 19.957928
    ## 9822  22.629811 18.271405
    ## 9823  21.385046 17.612096
    ## 9824  24.219799 18.591188
    ## 9825  15.049149 18.499699
    ## 9826  16.105992 15.255385
    ## 9827  24.978166 19.651359
    ## 9828  17.679956 15.428973
    ## 9829  24.571011 18.910865
    ## 9830  22.784612 18.828157
    ## 9831  22.420716 19.806696
    ## 9832  19.569327 18.799767
    ## 9833  22.165245 18.709426
    ## 9834  22.307459 19.391179
    ## 9835  16.709110 19.994255
    ## 9836  22.996474 18.895198
    ## 9837  18.522859 19.373752
    ## 9838  24.158512 18.137254
    ## 9839  16.910512 16.438830
    ## 9840  16.988660 16.953202
    ## 9841  18.703404 15.981784
    ## 9842  20.658080 18.360519
    ## 9843  24.154201 18.994843
    ## 9844  10.867364  9.410286
    ## 9845  29.306731 19.928898
    ## 9846  21.627641 19.128339
    ## 9847  27.287066 19.977354
    ## 9848  18.597014 16.813893
    ## 9849  19.887006 19.949946
    ## 9850  25.227289 19.948566
    ## 9851  19.743199 15.736823
    ## 9852  26.367763 19.412121
    ## 9853  22.408961 17.359820
    ## 9854  21.878170 17.992734
    ## 9855  24.461605 19.558567
    ## 9856  25.325449 19.459221
    ## 9857  25.791371 19.326275
    ## 9858  20.703314 17.076154
    ## 9859  20.825938 19.126768
    ## 9860  23.117607 19.919332
    ## 9861  16.519245 16.229257
    ## 9862  19.551165 18.891550
    ## 9863  20.007986 18.311990
    ## 9864  21.113477 19.599757
    ## 9865  16.976826 16.919116
    ## 9866  18.773469 19.841644
    ## 9867  16.192338 17.771193
    ## 9868  24.389803 18.207268
    ## 9869  21.105598 14.938966
    ## 9870  13.310738 15.915392
    ## 9871  24.267360 19.707408
    ## 9872  14.655479 19.046432
    ## 9873  21.028627 18.580100
    ## 9874  20.028803 18.266823
    ## 9875  21.222520 18.528378
    ## 9876  17.850974 18.839224
    ## 9877  18.157661 19.575314
    ## 9878  20.143002 15.084797
    ## 9879  26.916787 18.714925
    ## 9880  14.950579 13.733519
    ## 9881  10.875447 14.030827
    ## 9882  18.079650 16.428256
    ## 9883  12.847218 14.305299
    ## 9884  13.986267 16.216436
    ## 9885  22.094327 19.489244
    ## 9886  20.153352 17.503232
    ## 9887  23.210576 19.627517
    ## 9888  17.730366 19.322936
    ## 9889  18.241582 18.982314
    ## 9890  17.662985 18.970976
    ## 9891  21.031345 19.554976
    ## 9892  18.482931 18.089552
    ## 9893  18.594461 19.602324
    ## 9894  17.418922 19.900365
    ## 9895  23.340345 19.232321
    ## 9896  18.762933 19.953656
    ## 9897  20.743766 19.432782
    ## 9898  19.953977 16.670104
    ## 9899  24.331474 19.168353
    ## 9900  15.844228 18.998426
    ## 9901  17.324567 17.986083
    ## 9902  22.480888 16.841473
    ## 9903  16.720437 16.829671
    ## 9904  22.660591 19.523069
    ## 9905  17.811634 19.244788
    ## 9906  21.250482 17.895697
    ## 9907  16.790108 18.094214
    ## 9908  18.716706 18.357423
    ## 9909  19.051567 18.933725
    ## 9910  20.404712 18.170215
    ## 9911  19.080617 16.124225
    ## 9912  19.013832 19.820413
    ## 9913  23.813229 19.431866
    ## 9914  23.443365 18.245289
    ## 9915  18.426710 17.494944
    ## 9916  15.017721 16.617540
    ## 9917  19.096901 18.146661
    ## 9918  21.488742 18.826959
    ## 9919  18.281127 18.490396
    ## 9920  19.210544 17.836733
    ## 9921  17.055086 14.904323
    ## 9922  16.488960 14.912216
    ## 9923  15.899946 18.249929
    ## 9924  19.857614 17.069118
    ## 9925  19.918842 18.795795
    ## 9926  19.900954 18.014115
    ## 9927  20.886584 19.076266
    ## 9928  23.793076 19.131511
    ## 9929  29.539363 19.880909
    ## 9930  18.909421 18.767568
    ## 9931  15.403472 17.401194
    ## 9932  18.548007 19.436536
    ## 9933  26.648970 19.978130
    ## 9934  18.860345 14.866667
    ## 9935  13.229149 15.930456
    ## 9936  19.795598 18.937771
    ## 9937  17.192235 14.244941
    ## 9938  18.428247 19.040923
    ## 9939  20.603258 19.617404
    ## 9940  17.343360 18.791502
    ## 9941  22.491508 19.237521
    ## 9942  14.286122 12.013487
    ## 9943  22.582407 18.833038
    ## 9944  19.731850 18.496731
    ## 9945  22.290829 19.593552
    ## 9946  21.689441 18.462245
    ## 9947  19.735803 15.702123
    ## 9948  19.018398 18.113892
    ## 9949  18.680542 18.285294
    ## 9950  14.054401 16.773149
    ## 9951  14.128927 19.126649
    ## 9952  22.869082 16.978770
    ## 9953  20.498502 19.775546
    ## 9954  24.511227 18.907029
    ## 9955  25.709249 19.024607
    ## 9956  19.227777 19.699970
    ## 9957  14.260481 18.900673
    ## 9958  20.555582 18.824649
    ## 9959  21.059571 16.361178
    ## 9960  20.201737 18.380591
    ## 9961  23.983144 18.782179
    ## 9962  19.606675 19.394809
    ## 9963  24.578882 19.944559
    ## 9964  20.354893 19.734284
    ## 9965  20.925102 19.278389
    ## 9966  23.470615 17.730177
    ## 9967  21.478208 18.019645
    ## 9968  23.424898 19.579895
    ## 9969  24.885322 19.261847
    ## 9970  23.591932 19.733561
    ## 9971  21.211507 19.171039
    ## 9972  15.959889 17.671491
    ## 9973  12.928335 14.271176
    ## 9974  14.651035 14.262590
    ## 9975  15.726993 17.901308
    ## 9976  23.223883 19.436613
    ## 9977  19.486678 16.831002
    ## 9978  15.421873 13.346353
    ## 9979  20.535199 18.996488
    ## 9980  24.796686 18.774394
    ## 9981  16.257050 18.810335
    ## 9982  23.941745 19.066803
    ## 9983  21.566878 19.629834
    ## 9984  18.544934 16.642676
    ## 9985  23.636177 19.777963
    ## 9986  21.097880 19.916772
    ## 9987  25.447701 19.161482
    ## 9988  19.879003 17.245409
    ## 9989  21.551964 19.892816
    ## 9990  19.555475 16.163547
    ## 9991  17.290602 17.139385
    ## 9992  16.399411 15.363056
    ## 9993  19.045655 18.042371
    ## 9994  29.180854 19.146859
    ## 9995  21.379972 19.702250
    ## 9996  22.082350 17.335774
    ## 9997  21.392823 19.240015
    ## 9998  18.239169 19.228609
    ## 9999  17.635181 18.299851
    ## 10000 15.129826 15.398479

1.  Au regard des estimations simulées, comparer les propriétés de biais et de variance des deux estimateurs

<!-- -->

1.  Calcul du Biais pour les 2 estimateurs

``` r
# Méthode 1 : Calcul de la moyenne empirique
Biais = colSums(resultats)/K - theta
Biais
```

    ##          T1          T2 
    ##  0.01276632 -1.83496764

``` r
# Méthode 2 : Utilisation de apply
Biais = apply(resultats, 2, mean) - theta
```

1.  Calcul des Variances des 2 estimateurs

``` r
Variance = apply(resultats, 2, var)
Variance
```

    ##       T1       T2 
    ## 13.33975  2.85417

1.  L’erreur quadratique moyenne d’un estimateur est une mesure de qualité d’un estimateur qui résulte d’un compromis biais-variance. Elle est définie pour un estimateur $\\hat{\\theta}$ par:
    $$\\mathbb{E}\[(\\hat{\\theta} - \\theta)^2\] = biais^2(\\hat{\\theta}) + var(\\hat{\\theta})$$
     Calculer pour les estimateurs *T*<sub>*n*</sub><sup>(1)</sup> et *T*<sub>*n*</sub><sup>(2)</sup> une valeur approchée de cette quantité.

``` r
EQ = Biais**2 + Variance
EQ
```

    ##        T1        T2 
    ## 13.339918  6.221276

Visionnons graphiquement

``` r
boxplot(resultats)
abline(h = theta, col = "green")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-92-1.png)

5. **Régression linéaire simple**
=================================

5.1 Simulation d'un modèle de régression linéaire simple
--------------------------------------------------------

On considère le nombre de régression
*Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *ϵ*<sub>*i*</sub>,   *i* = 1, ..., *n*
 avec les *x*<sub>*i*</sub> non aléatoires équirépartis sur \[0, 1\] et les variables *ϵ*<sub>*i*</sub> i.i.d. selon une loi 𝒩(0, *σ*<sup>2</sup>). Les paramètres (*β*<sub>0</sub>, *β*<sub>1</sub>) et *σ*<sup>2</sup> sont inconnus en pratique. Pour illustrer on prend
(*β*<sub>0</sub>, *β*<sub>1</sub>)=(1, 2)  *σ*<sup>2</sup> = 0.1

1.  Simuler des données selon ce modèle

``` r
n <- 100
x <- seq(0, 1, length = n)
Beta0 <- 1
Beta1 <- 2
sigma <- 0.1
epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma))

Y <- Beta0 + Beta1 * x + epsilon

data.frame(Y) # Affichage de Y
```

    ##             Y
    ## 1   0.6179033
    ## 2   0.8921596
    ## 3   0.5525326
    ## 4   1.4580884
    ## 5   1.3236008
    ## 6   1.1815602
    ## 7   1.0478252
    ## 8   1.0823348
    ## 9   1.6797097
    ## 10  1.4073617
    ## 11  1.9316400
    ## 12  1.5615247
    ## 13  1.0349075
    ## 14  1.2491520
    ## 15  1.3529539
    ## 16  1.1354273
    ## 17  1.4913407
    ## 18  1.6004295
    ## 19  1.7206179
    ## 20  1.0467739
    ## 21  1.7170591
    ## 22  1.4424903
    ## 23  1.3722795
    ## 24  1.3380007
    ## 25  1.2089609
    ## 26  1.2288986
    ## 27  1.9116208
    ## 28  1.5767313
    ## 29  1.9254163
    ## 30  2.2013284
    ## 31  1.4628385
    ## 32  1.8491937
    ## 33  1.8861614
    ## 34  0.7089077
    ## 35  1.5153656
    ## 36  1.3896977
    ## 37  1.9491616
    ## 38  1.6847780
    ## 39  2.2630901
    ## 40  1.7215001
    ## 41  2.3064459
    ## 42  1.1092662
    ## 43  1.8940355
    ## 44  1.7807070
    ## 45  2.7433158
    ## 46  2.1014866
    ## 47  1.8323377
    ## 48  2.2093491
    ## 49  2.2881164
    ## 50  1.9419695
    ## 51  1.5135072
    ## 52  2.2509120
    ## 53  1.9951539
    ## 54  2.3063653
    ## 55  2.3573831
    ## 56  2.2873188
    ## 57  2.1880213
    ## 58  2.0539022
    ## 59  2.2402093
    ## 60  1.7302430
    ## 61  2.2612557
    ## 62  2.1917482
    ## 63  2.6817041
    ## 64  2.0967690
    ## 65  1.9973594
    ## 66  1.9684251
    ## 67  2.4077254
    ## 68  2.2577397
    ## 69  1.6827598
    ## 70  1.8184168
    ## 71  2.8054769
    ## 72  2.4603026
    ## 73  2.7837333
    ## 74  2.3258805
    ## 75  2.5420230
    ## 76  2.6029223
    ## 77  2.6382348
    ## 78  2.4465262
    ## 79  2.6809343
    ## 80  2.3436804
    ## 81  2.5367934
    ## 82  2.4558556
    ## 83  2.2012934
    ## 84  2.5054143
    ## 85  2.5732354
    ## 86  2.9730948
    ## 87  3.2666170
    ## 88  2.6677763
    ## 89  3.2780717
    ## 90  2.9224144
    ## 91  2.3470657
    ## 92  2.2952338
    ## 93  2.7709164
    ## 94  2.8878970
    ## 95  2.8856603
    ## 96  2.7803164
    ## 97  3.3318244
    ## 98  3.5606543
    ## 99  2.9154377
    ## 100 3.2090205

1.  Représenter les points (*x*<sub>*i*</sub>, *Y*<sub>*i*</sub>)

``` r
plot(x,Y)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-94-1.png)

1.  Sur le même graphique représenter la droite inconnue de paramètre (*β*<sub>0</sub>, *β*<sub>1</sub>)

``` r
plot(x, Y)
abline(Beta0, Beta1, col = "red")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-95-1.png)

5.2 Un estimateur: la droite des points extrêmes
------------------------------------------------

On se propose d'ajuster au nuage la droite passant par les points (*x*<sub>*i*</sub>, *y*<sub>*i*</sub>) et (*x*<sub>*n*</sub>, *y*<sub>*n*</sub>) 1. Calculer les paramètres de la droite ajustée par cette méthode L'équation de la droite est de la forme *y*<sub>*i*</sub> = *a**x*<sub>*i*</sub> + *b*, donc on cherche *a* et *b*

``` r
b = Y[1]
a = Y[n] - Y[1]
```

1.  Représenter la droite sur le graphe précédent

``` r
plot(x, Y)
abline(Beta0, Beta1, col = "red") # Question 5.1.3

abline(b, a, col = "purple")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-97-1.png)

1.  Calculer l'erreur quadratique moyenne d'ajustement
    $$S(\\tilde{\\beta\_0}, \\tilde{\\beta\_1}) = \\frac{1}{n} \\sum\_{i = 1}^n (y\_i - \\tilde{y\_i})^2$$
     où $\\tilde{y\_i}$ est la valeur ajustée en *x*<sub>*i*</sub> par cette méthode.

On calcule $\\tilde{Y}$ comme précédemment et l'erreur quadratique moyenne d'ajustement.

``` r
tildeY <- b + a * x + epsilon

tilde_EQM <- (1/n) * sum( (Y - tildeY)**2 )
tilde_EQM
```

    ## [1] 0.03719539

5.3 Un autre estimateur
-----------------------

On considère maintenant la méthode consistant à prendre la droite passant par le point moyen ($\\bar{x}, \\bar{y}$) et ayant comme pente définie par les points extrêmes. Reprendre les questions de l'exercice précédent avec ce nouvel estimateur.

Calcul des nouveaux coefficients. (On garde le *a* précédent)

``` r
bar_a = a
bar_b = mean(Y) - bar_a * mean(x)
```

Représentation graphique de la droite

``` r
plot(x, Y)
abline(Beta0, Beta1, col = "red") # Question 5.1.3
abline(b, a, col = "purple") # Question 5.2.2

abline(bar_b, bar_a, col = "greenyellow", lwd = 3)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-100-1.png)

Calcul de l'erreur quadratique moyenne d'ajustement

``` r
bar_Y <- bar_b + bar_a * x + epsilon

bar_EQM <- (1/n) * sum( (Y - bar_Y)**2 )
bar_EQM
```

    ## [1] 0.03019477

Au regard des données, quel ajustement est préférable (au sens quadratique moyen) ? On garde l'ajustement avec l'erreur quadratique moyenne d'ajustement minimal.

``` r
min(tilde_EQM, bar_EQM)
```

    ## [1] 0.03019477

Donc on garde la méthode passant par le point moyen.

5.4 Estimateur des moindres carrés ordinaires
---------------------------------------------

On rappelle que l'estimateur des moindres carrés est défini par
$$\\hat{\\beta\_1} = \\frac{cov(X,Y)}{var(X)} \\qquad \\hat{\\beta\_0} = \\bar{y} - \\hat{\\beta\_1}\\bar{x} $$
 où *c**o**v*(*X*, *Y*) et *v**a**r*(*X*) désignent respectivement covariance et variance empirique.

Reprendre la démarche proposée dans les exercices précédents avec l'estimateur des moindres carrés ordinaires. Quel ajustement est préférable (au sens quadratique moyen) ?

``` r
MC_Beta1 <- cov(x, Y) / var(x)
MC_Beta0 <- mean(Y) - MC_Beta1 * mean(x)
```

Représentation graphique de la droite

``` r
plot(x, Y)
abline(Beta0, Beta1, col = "red") # Question 5.1.3

abline(MC_Beta0, MC_Beta1, col = "purple", lwd = 3)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-104-1.png)

Calcul de l'erreur quadratique moyenne

``` r
MC_Y <- MC_Beta0 + MC_Beta1 * x

MC_EQM <- (1/n) * sum( (Y - MC_Y)**2 )
MC_EQM
```

    ## [1] 0.1037432

``` r
min(tilde_EQM, bar_EQM, MC_EQM) 
```

    ## [1] 0.03019477

Donc on garde l'ajustement des moindres carrés.

5.5 Fonctions **lm**
--------------------

1.  La commande **lm** pour retrouver l’estimation des paramètres par les moindres carrés.

``` r
lm(Y~x)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##       1.065        1.913

1.  Stocker le résultat de la fonction **lm()** dans un objet puis appliquer à cet objet les fonctions : **mode()**, **names()**, **summary()**, **plot()**.

``` r
modele <- lm(Y~x)
mode(modele)
```

    ## [1] "list"

``` r
names(modele)
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"         
    ##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
    ##  [9] "xlevels"       "call"          "terms"         "model"

``` r
summary(modele)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.99427 -0.17269 -0.00351  0.20903  0.82753 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.06535    0.06459   16.50   <2e-16 ***
    ## x            1.91349    0.11159   17.15   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3254 on 98 degrees of freedom
    ## Multiple R-squared:   0.75,  Adjusted R-squared:  0.7475 
    ## F-statistic: 294.1 on 1 and 98 DF,  p-value: < 2.2e-16

``` r
plot(modele)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-108-1.png)![](resume_rgit_files/figure-markdown_github/unnamed-chunk-108-2.png)![](resume_rgit_files/figure-markdown_github/unnamed-chunk-108-3.png)![](resume_rgit_files/figure-markdown_github/unnamed-chunk-108-4.png)

1.  Utilisation standard de la fonction **lm()**

Construire un data frame regroupant les observations de *x* et de *Y*.

``` r
observations <- data.frame(Y, x)
```

Retrouver les résultats précédents en utilisant le paramètre *data* de la fonction **lm()**

``` r
modele <- lm(data = observations)
summary(modele)
```

    ## 
    ## Call:
    ## lm(data = observations)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.99427 -0.17269 -0.00351  0.20903  0.82753 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.06535    0.06459   16.50   <2e-16 ***
    ## x            1.91349    0.11159   17.15   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3254 on 98 degrees of freedom
    ## Multiple R-squared:   0.75,  Adjusted R-squared:  0.7475 
    ## F-statistic: 294.1 on 1 and 98 DF,  p-value: < 2.2e-16

5.6 Comparaison d'estimateurs en régression linéaire simple
-----------------------------------------------------------

Dans cet exercice, on se propose de comparer les 3 estimateurs vus précédemment de la pente de la droite de régression dans un modèle de régression linéaire simple.

1.  Les instructions qui suivent permettent de simuler *n* = 100 données (*x*<sub>*i*</sub>, *y*<sub>*i*</sub>) selon le modèle *Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *ϵ*<sub>*i*</sub> avec (*β*<sub>0</sub>, *β*<sub>1</sub>)=(1, 2) et les *ϵ*<sub>*i*</sub> i.i.d. selon une loi 𝒩(0, *σ*) où *σ* = 0, 5:

``` r
beta0 <- 1 ; beta1 <- 2
n <- 100
sigma <- 0.5
x <- seq(0, 1, length = n)
eps <- rnorm(n, mean = 0, sd = sigma)
y <- beta0 + beta1 * x + eps
```

Représenter les données

``` r
plot(x, y)
abline(beta0, beta1, col = "red", lwd = 3)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-112-1.png)

Calculer l'estimation de (*β*<sub>0</sub>, *β*<sub>1</sub>) par les 3 méthodes:

-   **Points extrêmes**

``` r
PE_beta1 <- (y[n] - y[1]) / (x[n] - x[1])
PE_beta0 <- y[1] - PE_beta1 * x[1]

PE_beta0; PE_beta1
```

    ## [1] 0.6913959

    ## [1] 2.664986

-   **Point moyen**

``` r
PM_beta1 <- PE_beta1
PM_beta0 <- mean(y) - PM_beta1 * mean(x)

PM_beta0; PM_beta1
```

    ## [1] 0.6120291

    ## [1] 2.664986

-   **Moindre carrés ordinaires**

``` r
MC_beta1 <- cov(x,y) / var(x)
MC_beta0 <- mean(y) - MC_beta1 * mean(x)

MC_beta0; MC_beta1
```

    ## [1] 0.8990563

    ## [1] 2.090931

Tracer les droites associées

``` r
plot(x,y)
abline(beta0, beta1, col = "red", lwd = 3) # Question 5.6.1.a

abline(PE_beta0, PE_beta1, col = "greenyellow", lwd = 2) # Points extrêmes
abline(PM_beta0, PM_beta1, col = "chocolate", lwd = 2) # Point moyen
abline(MC_beta0, MC_beta1, col = "blue", lwd = 2) # Moindre carrés
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-116-1.png)

1.  Au moyen d’une boucle, simuler *K* = 1000 jeux de données suivant le modèle précédent et stockerles valeurs de pente des 3 estimateurs.

``` r
K <- 1000
hat_beta0 <- matrix(0, K, 3)
hat_beta1 <- matrix(0, K, 3)

for (i in 1:K) {
  # Echantillons
  eps <- rnorm(n, mean = 0, sd = sigma)
  y <- beta0 + beta1 * x + eps
  
  # Points extrêmes
  PE_beta1 <- (y[n] - y[1]) / (x[n] - x[1])
  PE_beta0 <- y[1] - PE_beta1 * x[1]
  
  # Point moyen
  PM_beta1 <- PE_beta1
  PM_beta0 <- mean(y) - PM_beta1 * mean(x)
  
  # Moindre carrés ordinaires
  MC_beta1 <- cov(x,y) / var(x)
  MC_beta0 <- mean(y) - MC_beta1 * mean(x)
  
  # Affectations
  hat_beta1[i,] <- c(PE_beta1, PM_beta1, MC_beta1)
  hat_beta0[i,] <- c(PE_beta0, PM_beta0, MC_beta0)
}
```

Représenter les distributions de ces pentes

``` r
boxplot(hat_beta1, names = c("PE", "PM", "MCO"))
title(main = "Estimation de Beta 1")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-118-1.png)

``` r
boxplot(hat_beta0, names = c("PE", "PM", "MCO"))
title(main = "Estimation de Beta 0")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-119-1.png)

Evaluer le biais des estimateurs de pente pour les 3 méthodes

``` r
apply(hat_beta1, 2, mean) - rep(beta1, 3) # Biais pour les estimations de Beta 1
```

    ## [1]  0.024814040  0.024814040 -0.002705681

``` r
apply(hat_beta0, 2, mean) - rep(beta0, 3) # Biais pour les estimations de Beta 0
```

    ## [1] -0.008735921 -0.012180801  0.001579060

Evaluer la variance des estimateurs de pente pour les 3 méthodes

``` r
apply(hat_beta1, 2, var)
```

    ## [1] 0.47018109 0.47018109 0.03153096

``` r
apply(hat_beta0, 2, var)
```

    ## [1] 0.23169003 0.11879475 0.01011775

Evaluer l'EQM

``` r
( apply(hat_beta1, 2, mean) - rep(beta1, 3) )^2 + apply(hat_beta1, 2, var)
```

    ## [1] 0.47079682 0.47079682 0.03153828

``` r
( apply(hat_beta0, 2, mean) - rep(beta0, 3) )^2 + apply(hat_beta0, 2, var)
```

    ## [1] 0.23176635 0.11894313 0.01012024

Le troisième estimateur, *MCO*, a l'EQM le plus faible donc on le garde.

5.7 Test de nullité d'un coefficient en regréssion linéaire
-----------------------------------------------------------

Dans un modèle de régression linéaire simple
*Y* = *β*<sub>0</sub> + *β*<sub>1</sub>*X* + *ϵ*
 Les hypothèses sur 𝔼\[*ϵ*\]=0 et *c**o**v*(*ϵ*<sub>*i*</sub>, *ϵ*<sub>*j*</sub>)=0 pour *i* ≠ *j* permettent de justifier de manière théoriques les propriétés d'absence de biais et de faible variance de l'estimateur des moindres carrés. Ces hypothèses ont été implicitement respectées en générant les réalisations de *ϵ* dans le modèle simulé en section précédente et les propriétés en découlant ont ainsi été observées.

L'hypothèse de normalité des résidus *ϵ*<sub>*i*</sub> = 𝒩(0, *σ*<sup>2</sup>) qui est traditionnellement formulée ensuite permet de faire de l'inférence sur les paramètres *β*<sub>0</sub> et *β*<sub>1</sub> du modèle: tests, calculs d'intervalles de confiance,...

Nous nous focalisons en particulier sur les tests de nullité des paramètres. Par défaut, la colonne **p-value** de la sortie fournie par la fonction **lm()** donne les probabilités critiques associées aux tests
ℋ<sub>0</sub> : *β*<sub>*j*</sub> = 0  *v**s*  ℋ<sub>1</sub> : *β*<sub>*j*</sub> ≠ 0  *p**o**u**r* *j* = 0, 1

1.  Quel est l'intérêt de tester l'hypothèse *β*<sub>1</sub> = 0 dans un tel modèle ?
2.  Simuler un modèle tel qu'à la section précédente. Faire varier *n*, *σ*<sup>2</sup> ainsi que les valeur du paramètre *β*<sub>1</sub> et commenter les probabilités critiques sur le test de nullité de *β*<sub>1</sub>.

**Modèle A**: n = 100, *σ* = 0, 1 et *β*<sub>1</sub> = 2

``` r
K <- 1000
alpha <- 0.05

n.a <- 100
x <- seq(0, 1, length = n.a)
Beta0 <- 0
Beta1.a <- 2
sigma.a <- 0.1
resultat.A <- matrix(0,K)

for (i in 1:K) {
  epsilon <- rnorm(n.a, mean = 0, sd = sigma.a)
  Y <- Beta0 + Beta1.a * x + epsilon
  modele <- lm(Y~x)
  resultat.A[i,] <- summary(modele)$coefficients[2,4]
}

table(resultat.A < alpha) / K
```

    ## 
    ## TRUE 
    ##    1

**Modèle B**: n = 100, *σ* = 0, 1 et *β*<sub>1</sub> = 0

``` r
Beta1.b <- 0
resultat.B <- matrix(0, K)

for (i in 1:K) {
  epsilon <- rnorm(n.a, mean = 0, sd = sigma.a)
  Y <- Beta0 + Beta1.b * x + epsilon
  modele <- lm(Y~x)
  resultat.B[i,] <- summary(modele)$coefficients[2,4]
}

table(resultat.B < alpha) / K
```

    ## 
    ## FALSE  TRUE 
    ## 0.949 0.051

L'erreur *β* *(risque de 2e espèce)* est égale à la proportion **FALSE**

**Modèle C**: n = 50, *σ* = 1 et *β*<sub>1</sub> = 2

``` r
n.c <- 50
x <- seq(0, 1, length = n.c)
Beta1.c <- 2
sigma.c <- 1
resultat.C <- matrix(0, K)

for (i in 1:K) {
  epsilon <- rnorm(n.c, mean = 0, sd = sigma.c)
  Y <- Beta0 + Beta1.c * x + epsilon
  modele <- lm(Y~x)
  resultat.C[i,] <- summary(modele)$coefficients[2,4]
}

table(resultat.C < alpha) / K
```

    ## 
    ## FALSE  TRUE 
    ##  0.02  0.98

5.8 Régression linéaire sur données réelles: données **ozone**
--------------------------------------------------------------

Les données **ozone** proviennent d’une étude sur les pics de pollution en Bretagne en 2001. Le receuil de ces données avait pour but d’expliquer la pollution de l’air, en construisant en particulier des modèles de prévisions du pic d’ozone du jour (variable de nom **max03**). Le jeu de données renseigne également plusieurs variables potentiellement explicatives, en particulier des variables météo. On s’intéresse en particulier à l’influence des variables
<ul>
<li>
**T9**, **T12** et **T15** qui donnent les températures du jour à 9h, 12h, 15h;
</li>
<li>
**Ne9**, **Ne12** et **Ne15** qui donnent les indices de nébulosité du jour à 9h, 12h, 15h;
</li>
<li>
**Vx9**, **Vx12** et **Vx15** qui sont des indicateurs de force du vent du jour à 9h, 12h, 15h.
</li>
</ul>
1.  Importer et résumer les données. On précise que les identifiants sont les dates de mesures.

``` r
ozone <- read.csv("ozone.txt", row.names = 1, sep = "")
summary(ozone)
```

    ##      maxO3              T9             T12             T15       
    ##  Min.   : 42.00   Min.   :11.30   Min.   :14.00   Min.   :14.90  
    ##  1st Qu.: 70.75   1st Qu.:16.20   1st Qu.:18.60   1st Qu.:19.27  
    ##  Median : 81.50   Median :17.80   Median :20.55   Median :22.05  
    ##  Mean   : 90.30   Mean   :18.36   Mean   :21.53   Mean   :22.63  
    ##  3rd Qu.:106.00   3rd Qu.:19.93   3rd Qu.:23.55   3rd Qu.:25.40  
    ##  Max.   :166.00   Max.   :27.00   Max.   :33.50   Max.   :35.50  
    ##       Ne9             Ne12            Ne15           Vx9         
    ##  Min.   :0.000   Min.   :0.000   Min.   :0.00   Min.   :-7.8785  
    ##  1st Qu.:3.000   1st Qu.:4.000   1st Qu.:3.00   1st Qu.:-3.2765  
    ##  Median :6.000   Median :5.000   Median :5.00   Median :-0.8660  
    ##  Mean   :4.929   Mean   :5.018   Mean   :4.83   Mean   :-1.2143  
    ##  3rd Qu.:7.000   3rd Qu.:7.000   3rd Qu.:7.00   3rd Qu.: 0.6946  
    ##  Max.   :8.000   Max.   :8.000   Max.   :8.00   Max.   : 5.1962  
    ##       Vx12             Vx15            maxO3v          vent      pluie   
    ##  Min.   :-7.878   Min.   :-9.000   Min.   : 42.00   Est  :10   Pluie:43  
    ##  1st Qu.:-3.565   1st Qu.:-3.939   1st Qu.: 71.00   Nord :31   Sec  :69  
    ##  Median :-1.879   Median :-1.550   Median : 82.50   Ouest:50             
    ##  Mean   :-1.611   Mean   :-1.691   Mean   : 90.57   Sud  :21             
    ##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:106.00                        
    ##  Max.   : 6.578   Max.   : 5.000   Max.   :166.00

1.  Calculer la matrice de corrélation entre les variables quantitatives en présence

Les classes des variables dans ozone

``` r
sapply(ozone, class)
```

    ##     maxO3        T9       T12       T15       Ne9      Ne12      Ne15 
    ## "integer" "numeric" "numeric" "numeric" "integer" "integer" "integer" 
    ##       Vx9      Vx12      Vx15    maxO3v      vent     pluie 
    ## "numeric" "numeric" "numeric" "integer"  "factor"  "factor"

Extraction des variables quantitatives

``` r
Qozone <- ozone [ sapply(ozone, class) == "integer"| sapply(ozone, class) == "numeric"]
summary(Qozone)
```

    ##      maxO3              T9             T12             T15       
    ##  Min.   : 42.00   Min.   :11.30   Min.   :14.00   Min.   :14.90  
    ##  1st Qu.: 70.75   1st Qu.:16.20   1st Qu.:18.60   1st Qu.:19.27  
    ##  Median : 81.50   Median :17.80   Median :20.55   Median :22.05  
    ##  Mean   : 90.30   Mean   :18.36   Mean   :21.53   Mean   :22.63  
    ##  3rd Qu.:106.00   3rd Qu.:19.93   3rd Qu.:23.55   3rd Qu.:25.40  
    ##  Max.   :166.00   Max.   :27.00   Max.   :33.50   Max.   :35.50  
    ##       Ne9             Ne12            Ne15           Vx9         
    ##  Min.   :0.000   Min.   :0.000   Min.   :0.00   Min.   :-7.8785  
    ##  1st Qu.:3.000   1st Qu.:4.000   1st Qu.:3.00   1st Qu.:-3.2765  
    ##  Median :6.000   Median :5.000   Median :5.00   Median :-0.8660  
    ##  Mean   :4.929   Mean   :5.018   Mean   :4.83   Mean   :-1.2143  
    ##  3rd Qu.:7.000   3rd Qu.:7.000   3rd Qu.:7.00   3rd Qu.: 0.6946  
    ##  Max.   :8.000   Max.   :8.000   Max.   :8.00   Max.   : 5.1962  
    ##       Vx12             Vx15            maxO3v      
    ##  Min.   :-7.878   Min.   :-9.000   Min.   : 42.00  
    ##  1st Qu.:-3.565   1st Qu.:-3.939   1st Qu.: 71.00  
    ##  Median :-1.879   Median :-1.550   Median : 82.50  
    ##  Mean   :-1.611   Mean   :-1.691   Mean   : 90.57  
    ##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:106.00  
    ##  Max.   : 6.578   Max.   : 5.000   Max.   :166.00

Calcul de la matrice de corrélation de **Qozone**

``` r
CorOzone <- cor(Qozone)
data.frame(CorOzone) # Affichage
```

    ##             maxO3         T9        T12        T15        Ne9       Ne12
    ## maxO3   1.0000000  0.6993865  0.7842623  0.7745700 -0.6217042 -0.6407513
    ## T9      0.6993865  1.0000000  0.8829672  0.8464460 -0.4838636 -0.4722475
    ## T12     0.7842623  0.8829672  1.0000000  0.9461930 -0.5842709 -0.6601002
    ## T15     0.7745700  0.8464460  0.9461930  1.0000000 -0.5861683 -0.6492261
    ## Ne9    -0.6217042 -0.4838636 -0.5842709 -0.5861683  1.0000000  0.7883411
    ## Ne12   -0.6407513 -0.4722475 -0.6601002 -0.6492261  0.7883411  1.0000000
    ## Ne15   -0.4783021 -0.3251386 -0.4580991 -0.5746817  0.5502490  0.7098668
    ## Vx9     0.5276234  0.2506896  0.4301045  0.4530892 -0.4976361 -0.4926581
    ## Vx12    0.4307959  0.2223857  0.3126291  0.3437506 -0.5287752 -0.5103204
    ## Vx15    0.3918989  0.1703215  0.2706802  0.2866028 -0.4939010 -0.4322695
    ## maxO3v  0.6845160  0.5822451  0.5636289  0.5678887 -0.2765500 -0.3619227
    ##              Ne15        Vx9       Vx12       Vx15     maxO3v
    ## maxO3  -0.4783021  0.5276234  0.4307959  0.3918989  0.6845160
    ## T9     -0.3251386  0.2506896  0.2223857  0.1703215  0.5822451
    ## T12    -0.4580991  0.4301045  0.3126291  0.2706802  0.5636289
    ## T15    -0.5746817  0.4530892  0.3437506  0.2866028  0.5678887
    ## Ne9     0.5502490 -0.4976361 -0.5287752 -0.4939010 -0.2765500
    ## Ne12    0.7098668 -0.4926581 -0.5103204 -0.4322695 -0.3619227
    ## Ne15    1.0000000 -0.4014717 -0.4318633 -0.3782896 -0.3084755
    ## Vx9    -0.4014717  1.0000000  0.7501775  0.6822608  0.3403172
    ## Vx12   -0.4318633  0.7501775  1.0000000  0.8371720  0.2236755
    ## Vx15   -0.3782896  0.6822608  0.8371720  1.0000000  0.1899220
    ## maxO3v -0.3084755  0.3403172  0.2236755  0.1899220  1.0000000

1.  Proposer une méthodologie pour déterminer la "meilleure" variable explicative quantitative dans une modèle de regréssion linéaire simple.

La meilleure variable explicative est la variable ayant la corrélation maximale avec **max03**.

``` r
which( abs(CorOzone[-1,1]) == max( abs(CorOzone[-1,1]) ) )
```

    ## T12 
    ##   2

La "meilleure" variable explicative est **T12**.

5.9 Une mesure de la qualité de l'ajustement: R²
------------------------------------------------

Dans un modèle de régression linéaire, si on note *Y*<sub>*i*</sub> les observations de la variable à expliquer et $\\hat{Y}\_i = \\hat{\\beta}\_0 + \\hat{\\beta}\_1 X\_i$ les valeurs ajustées, le coefficient dit de détermination, noté *R*<sup>2</sup>, est défini par:
$$R^2 = \\frac{var(\\hat{Y})}{var(Y)} $$
 Ce coefficient, compris entre 0 et 1 mesure la qualité d’ajustement du modèle: plus il est proche de 1 et plus les points du nuage sont proches de la droite ajustée.

On continue avec les données **ozone**

1.  Retrouver les sorties associées au modèle linéaire liant **maxO3** et **T12**

On teste le modèle: **maxO3** = 1 + **T12**

``` r
modele <- lm(maxO3 ~ 1 + T12, data = ozone)
resume <- summary( modele )
resume
```

    ## 
    ## Call:
    ## lm(formula = maxO3 ~ 1 + T12, data = ozone)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.079 -12.735   0.257  11.003  44.671 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -27.4196     9.0335  -3.035    0.003 ** 
    ## T12           5.4687     0.4125  13.258   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.57 on 110 degrees of freedom
    ## Multiple R-squared:  0.6151, Adjusted R-squared:  0.6116 
    ## F-statistic: 175.8 on 1 and 110 DF,  p-value: < 2.2e-16

1.  Retrouver dans les sorties la valeur de *R*<sup>2</sup>.

Le coefficient de détermination *R*<sup>2</sup> et sa valeur ajustée sont, respectivement, le 8e et le 9e élément de la sortie du modèle.

``` r
data.frame(resume[8], resume[9]) # data.frame pour affichage
```

    ##   r.squared adj.r.squared
    ## 1 0.6150674      0.611568

5.10 Données **ozone**: influence de **T12** selon les conditions de pluie
--------------------------------------------------------------------------

Le fichier **ozone** enseigne également la variable **pluie**, variable qualitative qui indique s’il pleuvait ou pas lejour de la mesure.

1.  Représenter dans un même nuage de points les variables **maxO3** et **T12** en introduisant un code couleur permettant de distinguer les observations de la variable **pluie**.

``` r
plot(maxO3 ~ 1+T12, col = pluie, data = ozone)
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-133-1.png)

1.  Ajuster les droites de régression obtenues selon les conditions de pluie.

On sépare les données dans une liste selon les modalités de **pluie**, ensuite on affecte à la variable ***modele*** la sortie du modèle linéaire liant **maxO3** et **T12** aux deux éléments de la liste.

``` r
Pluie_Sec <- split( ozone[, c("maxO3","T12")], list(ozone$pluie) )
modele <- lapply(Pluie_Sec, FUN = function(donnees) {return( lm(maxO3 ~ T12, data = donnees) )} )

lapply(modele, FUN = summary) # Affichage du résumé pour chaque modalité
```

    ## $Pluie
    ## 
    ## Call:
    ## lm(formula = maxO3 ~ T12, data = donnees)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -27.137 -10.607  -1.630   7.565  43.319 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.0386    17.5636   0.401 0.690687    
    ## T12           3.4419     0.9033   3.810 0.000457 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.94 on 41 degrees of freedom
    ## Multiple R-squared:  0.2615, Adjusted R-squared:  0.2435 
    ## F-statistic: 14.52 on 1 and 41 DF,  p-value: 0.0004574
    ## 
    ## 
    ## $Sec
    ## 
    ## Call:
    ## lm(formula = maxO3 ~ T12, data = donnees)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -36.357 -11.978   0.141  11.596  42.406 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -21.2601    12.2970  -1.729   0.0884 .  
    ## T12           5.3255     0.5278  10.091 4.44e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.19 on 67 degrees of freedom
    ## Multiple R-squared:  0.6031, Adjusted R-squared:  0.5972 
    ## F-statistic: 101.8 on 1 and 67 DF,  p-value: 4.445e-15

On trace la droite de régression pour chacune des modalités

``` r
plot(maxO3 ~ 1+T12, col = pluie, data = ozone) # Question 5.10.1

abline( coef(modele$Pluie) )
abline( coef(modele$Sec), col = "red")
```

![](resume_rgit_files/figure-markdown_github/unnamed-chunk-135-1.png)

<br><br><br>

------------------------------------------------------------------------

Tu es arrivé jusqu'au boût !
