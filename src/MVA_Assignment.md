---
title: "MVA Assignment"
author: "Edward Baleni, BLNEDW003, Thabo Dube, DBXTHA030"
date: "2023-05-17"
output:
  pdf_document:
    fig_caption: yes
    extra_dependencies:
    - float
    - subfig
    keep_md: yes
  html_document:
    df_print: paged
header-includes: \usepackage{amsmath}
always_allow_html: yes
---



# Introduction





```r
# Load in data
load("FinData.RData")
# Change row names
rownames(FinData) <- FinData$Name
# Get X matrix that does not include potential response variables and categorical variables
X <- FinData
rownames(X) <- X$Name
X <- scale(X[,c(-1,-5, -6, -7, -8,-3,-4, -35, -34, -36, -37)])

# Look for outliers
par(mar = c(8, 4, 0.5, 2))
boxplot(X, las = 2)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Data-1} \end{center}

```r
# Define colour scheme
colors <- c("#900000", "#E79300", "#59A6F7") # DF, FW, MF
colors <- colors[FinData$Position]
```

# Exploratory Data Analysis



# Methodology

```r
# Quickly check correlations as this may inform our thinking on principle components
co <- cor(X)
corrplot((co),method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Correlations-1} \end{center}

```r
# Now we see how much of the data is correlated and how much of it is not
co[upper.tri(co)] <- NA 
co <- co - diag(nrow(co))
# How many are correlated strongly
(pres <- sum(abs(co) > 0.5, na.rm = T))#/(ncol(combn(26, 2))))
```

```
## [1] 108
```

```r
# How many correlations were checked
ncol(combn(26, 2))
```

```
## [1] 325
```

```r
# The code above shows us that out of the 325 correlations not including correlation with itself that only 54 correlations are present in our dataset. This may indicate the possibility that the PCA may not be able to decorrelate our data very as it is already mostly uncorrelated.
```



```r
# Run a Principle Component Analysis
PC <- prcomp(X, scale. = F)

# Check levels of information
plot(PC$sdev[1:16]/sum(PC$sdev), pch = 19, cex = 0.7, ylab = "Proportion of Explained Variation", xlab = "Principle Component", type = "b")
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/PCA-1} \end{center}

```r
# Check elbows
sum(PC$sdev[1:4])/sum(PC$sdev)
```

```
## [1] 0.4395029
```

```r
sum(PC$sdev[1:6])/sum(PC$sdev)
```

```
## [1] 0.5467268
```

```r
sum(PC$sdev[1:10])/sum(PC$sdev)
```

```
## [1] 0.7284074
```

```r
# Plot most important Principle components
pairs(PC$x[,1:6], pch = 16, cex = 0.75, col = colors)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/PCA-2} \end{center}

```r
# Biplot for PCA
autoplot(PC, data=FinData, colour=colors,  loadings=TRUE, loadings.label = TRUE, loadings.label.size = 2, loadings.colour = 'grey', loadings.label.colour="black", loadings.label.angle = 90) +
  theme_light()
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/PCA-3} \end{center}

```r
# Loadings
melted_cormat <- melt(PC$rotation)
colnames(melted_cormat) <- c("Var", "PC", "Correlation")
melted_cormat$Correlation <- ifelse(abs(melted_cormat$Correlation) < 0.3, 0, melted_cormat$Correlation)
ggplot(data = melted_cormat, aes(x=Var, y=PC, fill=Correlation)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)+ 
  theme(axis.text.x = element_text(angle = 90))+
  xlab("")+ ylab("")
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/PCA-4} \end{center}


```r
# Perform Robust PCA
robPCA <- PcaHubert(X)

# Rename Columns of Scores
colnames(robPCA@scores) <- paste0("RPC", 1:6)

# Get variable importance
varimp <- summary(robPCA)@importance[2,]

# Plot Scree Plot
plot(varimp, pch = 19, cex = 0.7, ylab = "Proportion of Explained Variation", xlab = "Robust Principle Component", type = "b")
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Robust PCA-1} \end{center}

```r
# Plot Scores
pairs(robPCA@scores, col = colors, pch = 16, cex = 0.75)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Robust PCA-2} \end{center}

```r
# Proper visible biplot
ggplot(data=20*robPCA$loadings, aes(PC1, PC2))+
  geom_vector( col = "grey") +
  geom_point(data=robPCA$scores, aes(x=RPC1, y=RPC2), col=colors) +
  geom_text(label = rownames(robPCA$loadings), size = 2.5)+
  xlab(paste("PC1 (", round(varimp[1]*100,1), "%)"))+
  ylab(paste("PC2 (", round(varimp[2]*100,1), "%)"))
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Robust PCA-3} \end{center}

```r
# Outlier Map
plot(robPCA)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Robust PCA-4} \end{center}

```r
# Obtain outliers
FinData[which(robPCA$flag == F),]
```

```
##                                            Name age Market Value
## Amine Gouiri                       Amine Gouiri  22         42.0
## Anthony Modeste                 Anthony Modeste  34          3.0
## Antonio Candreva               Antonio Candreva  35          2.5
## Arkadiusz Reca                   Arkadiusz Reca  27          3.0
## Arnaud Kalimuendo             Arnaud Kalimuendo  20         18.0
## Conor Coady                         Conor Coady  29         25.0
## David Raum                           David Raum  24         20.0
## Dedryck Boyata                   Dedryck Boyata  31          3.5
## Dominik Kohr                       Dominik Kohr  28          5.0
## Erling Haaland                   Erling Haaland  21        150.0
## Ethan Ampadu                       Ethan Ampadu  21         13.0
## Francesco Acerbi               Francesco Acerbi  34          4.0
## Gianluca Caprari               Gianluca Caprari  28         10.0
## Gianluca Scamacca             Gianluca Scamacca  23         30.0
## Giovanni Simeone               Giovanni Simeone  27         17.0
## Hannibal Mejbri                 Hannibal Mejbri  19          6.0
## Harry Winks                         Harry Winks  26         15.0
## Houboulang Mendes             Houboulang Mendes  24          3.0
## Ilaix Moriba                       Ilaix Moriba  19          9.0
## Jan Bednarek                       Jan Bednarek  26         22.0
## Jeff Hendrick                     Jeff Hendrick  30          5.0
## Jimmy Giraudon                   Jimmy Giraudon  30          1.5
## Joel Pohjanpalo                 Joel Pohjanpalo  27          2.5
## Johan Mojica                       Johan Mojica  30          5.0
## Jonathan Clauss                 Jonathan Clauss  29         15.0
## Jordan Beyer                       Jordan Beyer  22          4.5
## Jordan Veretout                 Jordan Veretout  29         17.0
## Jules Koundé                       Jules Koundé  23         60.0
## Kalidou Koulibaly             Kalidou Koulibaly  31         35.0
## Konstantinos Mavropanos Konstantinos Mavropanos  24         15.0
## Leandro Paredes                 Leandro Paredes  28         17.0
## Lorenzo Insigne                 Lorenzo Insigne  31         25.0
## Luca Kilian                         Luca Kilian  22          4.5
## Mattia Viti                         Mattia Viti  20          7.0
## Maya Yoshida                       Maya Yoshida  33          1.5
## Mehdi Chahiri                     Mehdi Chahiri  25          1.2
## Nahuel Molina                     Nahuel Molina  24         20.0
## Nayef Aguerd                       Nayef Aguerd  26         12.0
## Nemanja Matić                     Nemanja Matić  33          5.0
## Nico Schlotterbeck           Nico Schlotterbeck  22         33.0
## Niklas Stark                       Niklas Stark  27          6.5
## Nordi Mukiele                     Nordi Mukiele  24         20.0
## Paul Pogba                           Paul Pogba  29         48.0
## Paulo Dybala                       Paulo Dybala  28         35.0
## Pervis Estupiñán               Pervis Estupiñán  24         20.0
## Robert Lewandowski           Robert Lewandowski  33         45.0
## Romain Saïss                       Romain Saïss  32          8.0
## Romain Thomas                     Romain Thomas  34          1.0
## Sadio Mané                           Sadio Mané  30         70.0
## Salis Abdul Samed             Salis Abdul Samed  22          3.0
## Stefan Posch                       Stefan Posch  25         10.0
## Tyler Adams                         Tyler Adams  23         17.0
## Wout Faes                             Wout Faes  24         10.0
##                         Transfer Fee (Millions) Origin Position           Club
## Amine Gouiri                               28.0    FRA       FW           Nice
## Anthony Modeste                             5.1    FRA       FW           Köln
## Antonio Candreva                            0.0    ITA       MF      Sampdoria
## Arkadiusz Reca                              2.0    POL       DF         Spezia
## Arnaud Kalimuendo                          20.0    FRA       FW           Lens
## Conor Coady                                 0.0    ENG       DF         Wolves
## David Raum                                 26.0    GER       DF     Hoffenheim
## Dedryck Boyata                              2.0    BEL       DF     Hertha BSC
## Dominik Kohr                                1.5    GER       MF       Mainz 05
## Erling Haaland                             60.0    NOR       FW       Dortmund
## Ethan Ampadu                                0.0    WAL       MF        Venezia
## Francesco Acerbi                            0.0    ITA       DF          Lazio
## Gianluca Caprari                            3.0    ITA       MF  Hellas Verona
## Gianluca Scamacca                          36.0    ITA       FW       Sassuolo
## Giovanni Simeone                            3.5    ARG       FW  Hellas Verona
## Hannibal Mejbri                             0.0    TUN       FW Manchester Utd
## Harry Winks                                 0.0    ENG       MF      Tottenham
## Houboulang Mendes                           0.0    FRA       DF        Lorient
## Ilaix Moriba                                0.0    GUI       MF     RB Leipzig
## Jan Bednarek                                0.0    POL       DF    Southampton
## Jeff Hendrick                               0.0    IRL       MF  Newcastle Utd
## Jimmy Giraudon                              0.0    FRA       DF         Troyes
## Joel Pohjanpalo                             2.5    FIN       FW     Leverkusen
## Johan Mojica                                5.5    COL       DF          Elche
## Jonathan Clauss                             7.5    FRA       DF           Lens
## Jordan Beyer                                0.0    GER       DF     M'Gladbach
## Jordan Veretout                            11.0    FRA       MF           Roma
## Jules Koundé                               50.0    FRA       DF        Sevilla
## Kalidou Koulibaly                          38.0    SEN       DF         Napoli
## Konstantinos Mavropanos                     3.2    GRE       DF      Stuttgart
## Leandro Paredes                             0.0    ARG       MF      Paris S-G
## Lorenzo Insigne                             0.0    ITA       FW         Napoli
## Luca Kilian                                 2.0    GER       FW       Mainz 05
## Mattia Viti                                13.0    ITA       DF         Empoli
## Maya Yoshida                                0.0    JPN       DF      Sampdoria
## Mehdi Chahiri                               0.0    MAR       DF     Strasbourg
## Nahuel Molina                              20.0    ARG       DF        Udinese
## Nayef Aguerd                               35.0    MAR       DF         Rennes
## Nemanja Matić                               0.0    SRB       MF Manchester Utd
## Nico Schlotterbeck                         20.0    GER       DF       Freiburg
## Niklas Stark                                0.0    GER       DF     Hertha BSC
## Nordi Mukiele                              12.0    FRA       DF     RB Leipzig
## Paul Pogba                                  0.0    FRA       MF Manchester Utd
## Paulo Dybala                                0.0    ARG       FW       Juventus
## Pervis Estupiñán                           17.8    ECU       DF     Villarreal
## Robert Lewandowski                         45.0    POL       FW  Bayern Munich
## Romain Saïss                                0.0    MAR       DF         Wolves
## Romain Thomas                               0.0    FRA       DF         Angers
## Sadio Mané                                 32.0    SEN       FW      Liverpool
## Salis Abdul Samed                           5.0    GHA       MF  Clermont Foot
## Stefan Posch                                0.0    AUT       DF     Hoffenheim
## Tyler Adams                                17.0    USA       MF     RB Leipzig
## Wout Faes                                  17.0    BEL       DF          Reims
##                                 League Matches  Played Goals Assists
## Amine Gouiri                   Ligue 1              38    10       9
## Anthony Modeste             Bundesliga              32    20       4
## Antonio Candreva               Serie A              36     7      10
## Arkadiusz Reca                 Serie A              25     0       2
## Arnaud Kalimuendo              Ligue 1              32    12       0
## Conor Coady             Premier League              38     4       0
## David Raum                  Bundesliga              32     3      11
## Dedryck Boyata              Bundesliga              23     0       0
## Dominik Kohr                Bundesliga              22     0       3
## Erling Haaland              Bundesliga              24    22       8
## Ethan Ampadu                   Serie A              29     0       3
## Francesco Acerbi               Serie A              30     4       0
## Gianluca Caprari               Serie A              35    12       7
## Gianluca Scamacca              Serie A              36    16       0
## Giovanni Simeone               Serie A              35    17       5
## Hannibal Mejbri         Premier League               2     0       0
## Harry Winks             Premier League              19     0       1
## Houboulang Mendes              Ligue 1              36     0       1
## Ilaix Moriba                Bundesliga               2     0       0
## Jan Bednarek            Premier League              31     4       0
## Jeff Hendrick           Premier League               3     1       0
## Jimmy Giraudon                 Ligue 1              19     0       0
## Joel Pohjanpalo             Bundesliga               2     0       0
## Johan Mojica                   La Liga              33     2       5
## Jonathan Clauss                Ligue 1              37     5      11
## Jordan Beyer                Bundesliga              17     0       0
## Jordan Veretout                Serie A              36     4       8
## Jules Koundé                   La Liga              32     2       1
## Kalidou Koulibaly              Serie A              27     3       1
## Konstantinos Mavropanos     Bundesliga              31     4       0
## Leandro Paredes                Ligue 1              15     1       2
## Lorenzo Insigne                Serie A              32    11       8
## Luca Kilian                 Bundesliga               1     0       0
## Mattia Viti                    Serie A              20     0       0
## Maya Yoshida                   Serie A              26     2       3
## Mehdi Chahiri                  Ligue 1               1     0       0
## Nahuel Molina                  Serie A              35     7       2
## Nayef Aguerd                   Ligue 1              31     2       2
## Nemanja Matić           Premier League              23     0       4
## Nico Schlotterbeck          Bundesliga              32     4       1
## Niklas Stark                Bundesliga              26     1       0
## Nordi Mukiele               Bundesliga              28     1       3
## Paul Pogba              Premier League              20     1       9
## Paulo Dybala                   Serie A              29    10       5
## Pervis Estupiñán               La Liga              28     0       1
## Robert Lewandowski          Bundesliga              34    35       3
## Romain Saïss            Premier League              31     2       0
## Romain Thomas                  Ligue 1              30     2       0
## Sadio Mané              Premier League              34    16       2
## Salis Abdul Samed              Ligue 1              31     1       0
## Stefan Posch                Bundesliga              28     2       0
## Tyler Adams                 Bundesliga              24     0       1
## Wout Faes                      Ligue 1              37     4       0
##                         Goals+Assists Total Shots Shots on Target SOT %
## Amine Gouiri                       19          71              33  46.5
## Anthony Modeste                    24         106              43  40.6
## Antonio Candreva                   17          72              19  26.4
## Arkadiusz Reca                      2           8               4  50.0
## Arnaud Kalimuendo                  12          60              29  48.3
## Conor Coady                         4          12               4  33.3
## David Raum                         14          25               8  32.0
## Dedryck Boyata                      0          10               2  20.0
## Dominik Kohr                        3          18               2  11.1
## Erling Haaland                     30          74              31  41.9
## Ethan Ampadu                        3           7               1  14.3
## Francesco Acerbi                    4          15               7  46.7
## Gianluca Caprari                   19          65              26  40.0
## Gianluca Scamacca                  16          78              36  46.2
## Giovanni Simeone                   22          69              32  46.4
## Hannibal Mejbri                     0           1               1 100.0
## Harry Winks                         1           3               1  33.3
## Houboulang Mendes                   1          21               5  23.8
## Ilaix Moriba                        0           1               1 100.0
## Jan Bednarek                        4          19               7  36.8
## Jeff Hendrick                       1           1               1 100.0
## Jimmy Giraudon                      0           0               0   0.0
## Joel Pohjanpalo                     0           0               0   0.0
## Johan Mojica                        7          16               3  18.8
## Jonathan Clauss                    16          40              16  40.0
## Jordan Beyer                        0           7               0   0.0
## Jordan Veretout                    12          27               8  29.6
## Jules Koundé                        3          13               3  23.1
## Kalidou Koulibaly                   4          10               7  70.0
## Konstantinos Mavropanos             4          33              11  33.3
## Leandro Paredes                     3           8               2  25.0
## Lorenzo Insigne                    19          81              24  29.6
## Luca Kilian                         0           0               0   0.0
## Mattia Viti                         0           4               0   0.0
## Maya Yoshida                        5           9               4  44.4
## Mehdi Chahiri                       0           0               0   0.0
## Nahuel Molina                       9          44              16  36.4
## Nayef Aguerd                        4          25               7  28.0
## Nemanja Matić                       4           6               1  16.7
## Nico Schlotterbeck                  5          36              16  44.4
## Niklas Stark                        1          11               4  36.4
## Nordi Mukiele                       4          22               7  31.8
## Paul Pogba                         10          32               8  25.0
## Paulo Dybala                       15          97              36  37.1
## Pervis Estupiñán                    1          14               4  28.6
## Robert Lewandowski                 38         156              75  48.1
## Romain Saïss                        2          18               6  33.3
## Romain Thomas                       2          14               4  28.6
## Sadio Mané                         18          98              37  37.8
## Salis Abdul Samed                   1          18               5  27.8
## Stefan Posch                        2          14               7  50.0
## Tyler Adams                         1           2               0   0.0
## Wout Faes                           4          22               9  40.9
##                         Yellow Cards Red Cards Fouls Committed Fouls Drawn
## Amine Gouiri                       3         0              30          32
## Anthony Modeste                    4         0              38          33
## Antonio Candreva                   6         1              19          48
## Arkadiusz Reca                     3         0              23          13
## Arnaud Kalimuendo                  1         1              13          32
## Conor Coady                        4         0              20           1
## David Raum                         5         0              27          32
## Dedryck Boyata                     1         1              11           6
## Dominik Kohr                      10         2              46          20
## Erling Haaland                     3         0              17          14
## Ethan Ampadu                      13         2              46          27
## Francesco Acerbi                   2         1              18          16
## Gianluca Caprari                   4         0              42          74
## Gianluca Scamacca                  7         0              49          20
## Giovanni Simeone                   8         0              56          58
## Hannibal Mejbri                    2         0               3           0
## Harry Winks                        2         0              11          13
## Houboulang Mendes                  5         1              40          18
## Ilaix Moriba                       0         0               1           3
## Jan Bednarek                      10         0              30          15
## Jeff Hendrick                      0         0               1           0
## Jimmy Giraudon                     5         1              14           5
## Joel Pohjanpalo                    0         0               1           0
## Johan Mojica                       4         1              32          50
## Jonathan Clauss                    3         0              38          14
## Jordan Beyer                       5         0              16          21
## Jordan Veretout                    6         0              18          22
## Jules Koundé                       2         2              15          28
## Kalidou Koulibaly                  5         1              28          12
## Konstantinos Mavropanos            3         0              22          20
## Leandro Paredes                    3         0              10           8
## Lorenzo Insigne                    2         0              20          22
## Luca Kilian                        0         0               1           0
## Mattia Viti                        3         1              15          13
## Maya Yoshida                       2         0              20           9
## Mehdi Chahiri                      0         0               0           0
## Nahuel Molina                      7         1              31          16
## Nayef Aguerd                       3         1              20          16
## Nemanja Matić                      3         0              26           3
## Nico Schlotterbeck                 5         0              32          35
## Niklas Stark                       3         0              19          20
## Nordi Mukiele                      3         0              19          25
## Paul Pogba                         7         1              39          29
## Paulo Dybala                       3         0              16          48
## Pervis Estupiñán                   3         0              24          11
## Robert Lewandowski                 2         0              22          37
## Romain Saïss                       6         0              30          15
## Romain Thomas                      9         0              38          19
## Sadio Mané                         5         0              50          50
## Salis Abdul Samed                 12         3              43          37
## Stefan Posch                       5         0              36          13
## Tyler Adams                        5         0              19          15
## Wout Faes                          2         0              21          24
##                         Offsides Ball Recovery Tackles Made Tackles Won Blocks
## Amine Gouiri                   6            89           22          13     22
## Anthony Modeste               36            48           10           6     11
## Antonio Candreva              14           193           27          16     23
## Arkadiusz Reca                 0            98           31          19     20
## Arnaud Kalimuendo             14            51            9           8      6
## Conor Coady                    2           162           31          14     46
## David Raum                     7           201           52          29     38
## Dedryck Boyata                 3            93           15          13     17
## Dominik Kohr                   0           149           40          20     21
## Erling Haaland                14            40            2           1      8
## Ethan Ampadu                   0           157           67          43     49
## Francesco Acerbi               4           162           25          12     37
## Gianluca Caprari              14           164           43          25     32
## Gianluca Scamacca             10            46           19          14     16
## Giovanni Simeone              34            81           27          15     37
## Hannibal Mejbri                1             1            2           1      0
## Harry Winks                    0            47           15          10     14
## Houboulang Mendes              4           178           58          28     41
## Ilaix Moriba                   0             4            0           0      2
## Jan Bednarek                   1           177           32          15     48
## Jeff Hendrick                  0             2            1           1      0
## Jimmy Giraudon                 0           105           14           6     24
## Joel Pohjanpalo                1             0            0           0      0
## Johan Mojica                   4           234           46          25     22
## Jonathan Clauss                4           203           63          34     43
## Jordan Beyer                   0            92           46          27     18
## Jordan Veretout                1           147           34          25     12
## Jules Koundé                   2           199           40          17     33
## Kalidou Koulibaly              0           167           39          23     41
## Konstantinos Mavropanos        2           214           57          39     46
## Leandro Paredes                0            86           23          13     10
## Lorenzo Insigne                5           142           23          14     30
## Luca Kilian                    0             0            0           0      0
## Mattia Viti                    1            83           28          16     28
## Maya Yoshida                   0           119           17          10     27
## Mehdi Chahiri                  0             2            1           1      0
## Nahuel Molina                  1           158           41          24     24
## Nayef Aguerd                   1           154           27          18     24
## Nemanja Matić                  0           124           31          18     21
## Nico Schlotterbeck             1           238           72          43     41
## Niklas Stark                   1           115           28          16     27
## Nordi Mukiele                  8           125           35          22     31
## Paul Pogba                     0            98           13           9     11
## Paulo Dybala                   6           103           30          13     19
## Pervis Estupiñán              11           126           36          22     13
## Robert Lewandowski            21            72           10           3     17
## Romain Saïss                   3           157           40          27     41
## Romain Thomas                  0           153           49          37     35
## Sadio Mané                    19           109           33          20     30
## Salis Abdul Samed              0           186           43          23     18
## Stefan Posch                   1           110           48          30     43
## Tyler Adams                    0            85           23           7     24
## Wout Faes                      0           199           40          24     47
##                         Shots Blocked Passes Blocked Clearnaces Interceptions
## Amine Gouiri                        5             17         11            17
## Anthony Modeste                     2              9          4            29
## Antonio Candreva                    2             21         33            19
## Arkadiusz Reca                      7             13         32            47
## Arnaud Kalimuendo                   1              5          8             0
## Conor Coady                        40              6         28           140
## David Raum                          5             33         27            39
## Dedryck Boyata                     13              4         17           134
## Dominik Kohr                        2             19         35            22
## Erling Haaland                      1              7          1            31
## Ethan Ampadu                       19             30         51            76
## Francesco Acerbi                   28              9         34            92
## Gianluca Caprari                    0             32          4             7
## Gianluca Scamacca                   1             15          5            22
## Giovanni Simeone                    3             34          6            23
## Hannibal Mejbri                     0              0          0             0
## Harry Winks                         3             11         16            10
## Houboulang Mendes                  21             20         41           128
## Ilaix Moriba                        0              2          1             0
## Jan Bednarek                       27             21         72           141
## Jeff Hendrick                       0              0          0             1
## Jimmy Giraudon                     14             10         37            68
## Joel Pohjanpalo                     0              0          0             0
## Johan Mojica                        3             19         25            61
## Jonathan Clauss                     1             42         73            51
## Jordan Beyer                        5             13         15            52
## Jordan Veretout                     1             11         27            22
## Jules Koundé                       18             15         21           101
## Kalidou Koulibaly                  25             16         29            90
## Konstantinos Mavropanos            23             23         69           174
## Leandro Paredes                     6              4         10             5
## Lorenzo Insigne                     1             29         20             3
## Luca Kilian                         0              0          0             0
## Mattia Viti                        25              3         29            69
## Maya Yoshida                       20              7         28            72
## Mehdi Chahiri                       0              0          0             0
## Nahuel Molina                       3             21         22            44
## Nayef Aguerd                       14             10         24           127
## Nemanja Matić                       6             15         31            21
## Nico Schlotterbeck                 32              9         56           154
## Niklas Stark                       17             10         36           122
## Nordi Mukiele                       5             26         31            44
## Paul Pogba                          0             11         13            16
## Paulo Dybala                        0             19          9             4
## Pervis Estupiñán                    5              8         25            29
## Robert Lewandowski                  2             15          6             8
## Romain Saïss                       21             20         58           158
## Romain Thomas                      19             16         44           126
## Sadio Mané                          1             29          8            15
## Salis Abdul Samed                   1             17         42            15
## Stefan Posch                       18             25         28            71
## Tyler Adams                         1             23         16            19
## Wout Faes                          37             10         53           169
##                         Defensive Errors Passes Made Passes Attempted Pass Rate
## Amine Gouiri                           1         786             1044      75.3
## Anthony Modeste                        0         289              450      64.2
## Antonio Candreva                       0        1296             1948      66.5
## Arkadiusz Reca                         2         622              879      70.8
## Arnaud Kalimuendo                      0         404              483      83.6
## Conor Coady                            0        1514             1692      89.5
## David Raum                             1        1463             2003      73.0
## Dedryck Boyata                         1         675              805      83.9
## Dominik Kohr                           0         621              840      73.9
## Erling Haaland                         0         331              476      69.5
## Ethan Ampadu                           0         867             1165      74.4
## Francesco Acerbi                       0        1837             2043      89.9
## Gianluca Caprari                       0         960             1283      74.8
## Gianluca Scamacca                      1         399              550      72.5
## Giovanni Simeone                       0         465              679      68.5
## Hannibal Mejbri                        0          17               21      81.0
## Harry Winks                            2         585              683      85.7
## Houboulang Mendes                      4        1174             1446      81.2
## Ilaix Moriba                           0          17               21      81.0
## Jan Bednarek                           0        1201             1490      80.6
## Jeff Hendrick                          0          27               29      93.1
## Jimmy Giraudon                         1         755              882      85.6
## Joel Pohjanpalo                        0           1                3      33.3
## Johan Mojica                           3        1400             1863      75.1
## Jonathan Clauss                        0        1243             1883      66.0
## Jordan Beyer                           2         823              937      87.8
## Jordan Veretout                        1        1215             1440      84.4
## Jules Koundé                           2        1894             2153      88.0
## Kalidou Koulibaly                      1        1429             1652      86.5
## Konstantinos Mavropanos                2        1493             1859      80.3
## Leandro Paredes                        2         896              937      95.6
## Lorenzo Insigne                        1        1208             1509      80.1
## Luca Kilian                            0           0                1       0.0
## Mattia Viti                            1         568              679      83.7
## Maya Yoshida                           1         814              966      84.3
## Mehdi Chahiri                          0           2                6      33.3
## Nahuel Molina                          2         860             1228      70.0
## Nayef Aguerd                           1        1959             2204      88.9
## Nemanja Matić                          3         911             1053      86.5
## Nico Schlotterbeck                     1        1403             1750      80.2
## Niklas Stark                           2         829             1009      82.2
## Nordi Mukiele                          2         851             1147      74.2
## Paul Pogba                             0         736              893      82.4
## Paulo Dybala                           3         912             1137      80.2
## Pervis Estupiñán                       2         820             1061      77.3
## Robert Lewandowski                     0         575              767      75.0
## Romain Saïss                           2        1437             1808      79.5
## Romain Thomas                          0        1471             1706      86.2
## Sadio Mané                             0         818             1100      74.4
## Salis Abdul Samed                      0        1535             1701      90.2
## Stefan Posch                           2        1134             1416      80.1
## Tyler Adams                            2         794              918      86.5
## Wout Faes                              2        1254             1472      85.2
##                         Key Passes Position2.y OVR Overall Percentile
## Amine Gouiri                    49        FWMF  78      79          2
## Anthony Modeste                 20          FW  72      79          3
## Antonio Candreva                78        MFFW  79      80          1
## Arkadiusz Reca                  13          DF  70      72          4
## Arnaud Kalimuendo               21          FW  73      76          3
## Conor Coady                      3          DF  79      79          1
## David Raum                      83        DFFW  73      81          3
## Dedryck Boyata                   5          DF  78      76          2
## Dominik Kohr                    10          MF  75      76          2
## Erling Haaland                  23          FW  88      88          1
## Ethan Ampadu                    15        MFDF  68      73          4
## Francesco Acerbi                 5          DF  83      82          1
## Gianluca Caprari                63        MFFW  75      78          2
## Gianluca Scamacca               25          FW  74      80          3
## Giovanni Simeone                24          FW  75      78          2
## Hannibal Mejbri                  0          FW  62      64          4
## Harry Winks                     11          MF  77      76          2
## Houboulang Mendes               13          DF  67      71          4
## Ilaix Moriba                     0          MF  73      73          3
## Jan Bednarek                     6          DF  76      76          2
## Jeff Hendrick                    1          MF  74      71          3
## Jimmy Giraudon                   3          DF  70      70          4
## Joel Pohjanpalo                  0          FW  73      73          3
## Johan Mojica                    29          DF  74      77          3
## Jonathan Clauss                 74          DF  77      80          2
## Jordan Beyer                     5          DF  70      74          4
## Jordan Veretout                 55          MF  81      79          1
## Jules Koundé                    17          DF  83      84          1
## Kalidou Koulibaly               11          DF  86      87          1
## Konstantinos Mavropanos         10          DF  73      78          3
## Leandro Paredes                  7          MF  81      80          1
## Lorenzo Insigne                 63          FW  86      84          1
## Luca Kilian                      0          FW  69      74          4
## Mattia Viti                      2          DF  64      71          4
## Maya Yoshida                     7          DF  73      73          3
## Mehdi Chahiri                    0          DF  69      69          4
## Nahuel Molina                   39          DF  73      78          3
## Nayef Aguerd                     4          DF  76      80          2
## Nemanja Matić                   13          MF  79      79          1
## Nico Schlotterbeck              12          DF  72      82          3
## Niklas Stark                     4          DF  76      75          2
## Nordi Mukiele                   14        DFFW  81      79          1
## Paul Pogba                      24        MFFW  87      85          1
## Paulo Dybala                    38        FWMF  87      86          1
## Pervis Estupiñán                13          DF  79      79          1
## Robert Lewandowski              37          FW  92      91          1
## Romain Saïss                     7          DF  78      77          2
## Romain Thomas                    2          DF  76      75          2
## Sadio Mané                      41          FW  89      89          1
## Salis Abdul Samed               17          MF  60      71          4
## Stefan Posch                     5          DF  75      75          2
## Tyler Adams                      7          MF  77      76          2
## Wout Faes                        3          DF  73      77          3
```

```r
  # Score distance > 4
#robPCA$sd
  # Orthogonal distance > 4
#robPCA$od

# Get tables of good and bad outliers to compare with original data
FinData[which(robPCA$sd > robPCA$cutoff.sd & robPCA$od > robPCA$cutoff.od), c("Name", "age", "Position", "Market Value", "OVR")]
```

```
##                                  Name age Position Market Value OVR
## Anthony Modeste       Anthony Modeste  34       FW          3.0  72
## Antonio Candreva     Antonio Candreva  35       MF          2.5  79
## Conor Coady               Conor Coady  29       DF         25.0  79
## David Raum                 David Raum  24       DF         20.0  73
## Erling Haaland         Erling Haaland  21       FW        150.0  88
## Gianluca Scamacca   Gianluca Scamacca  23       FW         30.0  74
## Giovanni Simeone     Giovanni Simeone  27       FW         17.0  75
## Jan Bednarek             Jan Bednarek  26       DF         22.0  76
## Jules Koundé             Jules Koundé  23       DF         60.0  83
## Nico Schlotterbeck Nico Schlotterbeck  22       DF         33.0  72
## Robert Lewandowski Robert Lewandowski  33       FW         45.0  92
## Wout Faes                   Wout Faes  24       DF         10.0  73
```

```r
FinData[which((robPCA$sd < robPCA$cutoff.sd & robPCA$od > robPCA$cutoff.od)| (robPCA$sd > robPCA$cutoff.sd & robPCA$od < robPCA$cutoff.od)),c("Name", "age", "Position","Market Value", "OVR")]
```

```
##                                            Name age Position Market Value OVR
## Amine Gouiri                       Amine Gouiri  22       FW         42.0  78
## Arkadiusz Reca                   Arkadiusz Reca  27       DF          3.0  70
## Arnaud Kalimuendo             Arnaud Kalimuendo  20       FW         18.0  73
## Dedryck Boyata                   Dedryck Boyata  31       DF          3.5  78
## Dominik Kohr                       Dominik Kohr  28       MF          5.0  75
## Ethan Ampadu                       Ethan Ampadu  21       MF         13.0  68
## Francesco Acerbi               Francesco Acerbi  34       DF          4.0  83
## Gianluca Caprari               Gianluca Caprari  28       MF         10.0  75
## Hannibal Mejbri                 Hannibal Mejbri  19       FW          6.0  62
## Harry Winks                         Harry Winks  26       MF         15.0  77
## Houboulang Mendes             Houboulang Mendes  24       DF          3.0  67
## Ilaix Moriba                       Ilaix Moriba  19       MF          9.0  73
## Jeff Hendrick                     Jeff Hendrick  30       MF          5.0  74
## Jimmy Giraudon                   Jimmy Giraudon  30       DF          1.5  70
## Joel Pohjanpalo                 Joel Pohjanpalo  27       FW          2.5  73
## Johan Mojica                       Johan Mojica  30       DF          5.0  74
## Jonathan Clauss                 Jonathan Clauss  29       DF         15.0  77
## Jordan Beyer                       Jordan Beyer  22       DF          4.5  70
## Jordan Veretout                 Jordan Veretout  29       MF         17.0  81
## Kalidou Koulibaly             Kalidou Koulibaly  31       DF         35.0  86
## Konstantinos Mavropanos Konstantinos Mavropanos  24       DF         15.0  73
## Leandro Paredes                 Leandro Paredes  28       MF         17.0  81
## Lorenzo Insigne                 Lorenzo Insigne  31       FW         25.0  86
## Luca Kilian                         Luca Kilian  22       FW          4.5  69
## Mattia Viti                         Mattia Viti  20       DF          7.0  64
## Maya Yoshida                       Maya Yoshida  33       DF          1.5  73
## Mehdi Chahiri                     Mehdi Chahiri  25       DF          1.2  69
## Nahuel Molina                     Nahuel Molina  24       DF         20.0  73
## Nayef Aguerd                       Nayef Aguerd  26       DF         12.0  76
## Nemanja Matić                     Nemanja Matić  33       MF          5.0  79
## Niklas Stark                       Niklas Stark  27       DF          6.5  76
## Nordi Mukiele                     Nordi Mukiele  24       DF         20.0  81
## Paul Pogba                           Paul Pogba  29       MF         48.0  87
## Paulo Dybala                       Paulo Dybala  28       FW         35.0  87
## Pervis Estupiñán               Pervis Estupiñán  24       DF         20.0  79
## Romain Saïss                       Romain Saïss  32       DF          8.0  78
## Romain Thomas                     Romain Thomas  34       DF          1.0  76
## Sadio Mané                           Sadio Mané  30       FW         70.0  89
## Salis Abdul Samed             Salis Abdul Samed  22       MF          3.0  60
## Stefan Posch                       Stefan Posch  25       DF         10.0  75
## Tyler Adams                         Tyler Adams  23       MF         17.0  77
```

```r
FinData[which(robPCA$sd > robPCA$cutoff.sd & robPCA$od < robPCA$cutoff.od),c("Name", "age", "Position","Market Value", "OVR")]
```

```
##                                            Name age Position Market Value OVR
## Amine Gouiri                       Amine Gouiri  22       FW         42.0  78
## Arkadiusz Reca                   Arkadiusz Reca  27       DF          3.0  70
## Dedryck Boyata                   Dedryck Boyata  31       DF          3.5  78
## Francesco Acerbi               Francesco Acerbi  34       DF          4.0  83
## Gianluca Caprari               Gianluca Caprari  28       MF         10.0  75
## Hannibal Mejbri                 Hannibal Mejbri  19       FW          6.0  62
## Harry Winks                         Harry Winks  26       MF         15.0  77
## Houboulang Mendes             Houboulang Mendes  24       DF          3.0  67
## Ilaix Moriba                       Ilaix Moriba  19       MF          9.0  73
## Jeff Hendrick                     Jeff Hendrick  30       MF          5.0  74
## Jimmy Giraudon                   Jimmy Giraudon  30       DF          1.5  70
## Joel Pohjanpalo                 Joel Pohjanpalo  27       FW          2.5  73
## Johan Mojica                       Johan Mojica  30       DF          5.0  74
## Jordan Beyer                       Jordan Beyer  22       DF          4.5  70
## Jordan Veretout                 Jordan Veretout  29       MF         17.0  81
## Kalidou Koulibaly             Kalidou Koulibaly  31       DF         35.0  86
## Konstantinos Mavropanos Konstantinos Mavropanos  24       DF         15.0  73
## Leandro Paredes                 Leandro Paredes  28       MF         17.0  81
## Lorenzo Insigne                 Lorenzo Insigne  31       FW         25.0  86
## Luca Kilian                         Luca Kilian  22       FW          4.5  69
## Mattia Viti                         Mattia Viti  20       DF          7.0  64
## Maya Yoshida                       Maya Yoshida  33       DF          1.5  73
## Mehdi Chahiri                     Mehdi Chahiri  25       DF          1.2  69
## Nahuel Molina                     Nahuel Molina  24       DF         20.0  73
## Nayef Aguerd                       Nayef Aguerd  26       DF         12.0  76
## Nemanja Matić                     Nemanja Matić  33       MF          5.0  79
## Niklas Stark                       Niklas Stark  27       DF          6.5  76
## Nordi Mukiele                     Nordi Mukiele  24       DF         20.0  81
## Paulo Dybala                       Paulo Dybala  28       FW         35.0  87
## Pervis Estupiñán               Pervis Estupiñán  24       DF         20.0  79
## Romain Saïss                       Romain Saïss  32       DF          8.0  78
## Romain Thomas                     Romain Thomas  34       DF          1.0  76
## Sadio Mané                           Sadio Mané  30       FW         70.0  89
## Stefan Posch                       Stefan Posch  25       DF         10.0  75
## Tyler Adams                         Tyler Adams  23       MF         17.0  77
```

```r
FinData[which(robPCA$sd < robPCA$cutoff.sd & robPCA$od < robPCA$cutoff.od),]
```

```
##                                          Name age Market Value
## Aaron Connolly                 Aaron Connolly  22          6.0
## Aaron Hickey                     Aaron Hickey  20         18.0
## Aaron Ramsey                     Aaron Ramsey  31          3.0
## Abdou Diallo                     Abdou Diallo  26         18.0
## Adam Ounas                         Adam Ounas  25          7.0
## Ademola Lookman               Ademola Lookman  24         10.0
## Adnan Januzaj                   Adnan Januzaj  27         12.0
## Ainsley Maitland-Niles Ainsley Maitland-Niles  25         10.0
## Akim Zedadka                     Akim Zedadka  27          4.0
## Alessandro Florenzi       Alessandro Florenzi  31          5.5
## Alessio Romagnoli           Alessio Romagnoli  27         17.0
## Alexander Isak                 Alexander Isak  22         30.0
## Alexandre Lacazette       Alexandre Lacazette  31         15.0
## Alexis Claude-Maurice   Alexis Claude-Maurice  24          6.0
## Alexis Sánchez                 Alexis Sánchez  33          3.5
## Amadou Diawara                 Amadou Diawara  25          6.0
## Amadou Onana                     Amadou Onana  20         10.0
## Amine Harit                       Amine Harit  25         10.0
## Anderson Lucoqui             Anderson Lucoqui  25          1.8
## Andi Zeqiri                       Andi Zeqiri  23          2.3
## Andrea Belotti                 Andrea Belotti  28         20.0
## Andrea La Mantia             Andrea La Mantia  31          1.3
## Andrea Petagna                 Andrea Petagna  27         12.0
## Andrea Pinamonti             Andrea Pinamonti  23         20.0
## Andreas Christensen       Andreas Christensen  26         35.0
## Andreas Voglsammer         Andreas Voglsammer  30          1.2
## Andriy Yarmolenko           Andriy Yarmolenko  32          2.5
## Ángel Di María                 Ángel Di María  34         12.0
## Angelo Fulgini                 Angelo Fulgini  25         12.0
## Anthony Caci                     Anthony Caci  25          7.0
## Antonio Rüdiger               Antonio Rüdiger  29         40.0
## Anwar El Ghazi                 Anwar El Ghazi  27          9.0
## Arkadiusz Milik               Arkadiusz Milik  28         16.0
## Armando Izzo                     Armando Izzo  30          2.8
## Arne Maier                         Arne Maier  23          5.5
## Arthur Theate                   Arthur Theate  22         15.0
## Aster Vranckx                   Aster Vranckx  19         10.0
## Axel Witsel                       Axel Witsel  33          4.0
## Aymen Barkok                     Aymen Barkok  24          1.8
## Ben Davies                         Ben Davies  26          2.5
## Bertrand Traoré               Bertrand Traoré  26         16.0
## Billy Gilmour                   Billy Gilmour  21         12.0
## Boubacar Kamara               Boubacar Kamara  22         25.0
## Boulaye Dia                       Boulaye Dia  25         12.0
## Brandon Soppy                   Brandon Soppy  20          5.0
## Breel Embolo                     Breel Embolo  25         15.0
## Brian Brobbey                   Brian Brobbey  20          9.0
## Bryan Reynolds                 Bryan Reynolds  21          3.5
## Callum Hudson-Odoi         Callum Hudson-Odoi  21         25.0
## Calvin Stengs                   Calvin Stengs  23         13.0
## Cédric Hountondji           Cédric Hountondji  28          3.0
## Cengiz Ünder                     Cengiz Ünder  24         22.0
## Cenk Tosun                         Cenk Tosun  31          2.0
## Cheick Doucouré               Cheick Doucouré  22         15.0
## Cheikhou Kouyaté             Cheikhou Kouyaté  32          4.0
## Chris Richards                 Chris Richards  22          7.5
## Christian Benteke           Christian Benteke  31          6.0
## Christian Eriksen           Christian Eriksen  30         20.0
## Ciaran Clark                     Ciaran Clark  32          1.8
## Clément Lenglet               Clément Lenglet  27         12.0
## Colin Dagba                       Colin Dagba  23          7.0
## Corentin Jean                   Corentin Jean  26          1.2
## Corentin Tolisso             Corentin Tolisso  27         15.0
## Cristian Romero               Cristian Romero  24         48.0
## Dan Ndoye                           Dan Ndoye  21          4.0
## Daniel James                     Daniel James  24         18.0
## Daniel Maldini                 Daniel Maldini  20          2.0
## Daniel Wass                       Daniel Wass  33          6.0
## Danny da Costa                 Danny da Costa  28          2.3
## David Nemeth                     David Nemeth  21          2.3
## Dele Alli                           Dele Alli  26         16.0
## Denis Cheryshev               Denis Cheryshev  31          2.5
## Denis Vavro                       Denis Vavro  26          3.0
## Denis Zakaria                   Denis Zakaria  25         27.0
## Diego Lainez                     Diego Lainez  22          2.5
## Divock Origi                     Divock Origi  27         12.0
## Dominique Heintz             Dominique Heintz  28          2.5
## Dries Mertens                   Dries Mertens  35          4.0
## Dwight Gayle                     Dwight Gayle  32          1.5
## Dylan Chambost                 Dylan Chambost  24          1.2
## Ebrima Colley                   Ebrima Colley  22          3.2
## Eddie Salcedo                   Eddie Salcedo  20          3.5
## Edinson Cavani                 Edinson Cavani  35          4.0
## El Bilal Touré                 El Bilal Touré  20          6.5
## Enzo Ebosse                       Enzo Ebosse  23          2.0
## Eric Bailly                       Eric Bailly  28          6.0
## Erick Cabaco                     Erick Cabaco  27          1.8
## Erik Durm                           Erik Durm  30          1.2
## Erik Thommy                       Erik Thommy  27          1.8
## Evann Guessand                 Evann Guessand  21          3.0
## Fabio Depaoli                   Fabio Depaoli  25          2.8
## Federico Bernardeschi   Federico Bernardeschi  28         10.0
## Federico Bonazzoli         Federico Bonazzoli  25          5.0
## Federico Chiesa               Federico Chiesa  24         65.0
## Federico Fernández         Federico Fernández  33          1.5
## Filippo Melegoni             Filippo Melegoni  23          2.0
## Flavius Daniliuc             Flavius Daniliuc  21          8.0
## Florent Mollet                 Florent Mollet  30          6.0
## Francesco Caputo             Francesco Caputo  34          2.5
## Francesco Di Mariano     Francesco Di Mariano  26          2.0
## Gaëtan Laborde                 Gaëtan Laborde  28         18.0
## Gareth Bale                       Gareth Bale  32          3.0
## Georginio Wijnaldum       Georginio Wijnaldum  31         18.0
## Ghislain Konan                 Ghislain Konan  26          7.0
## Giacomo Raspadori           Giacomo Raspadori  22         28.0
## Gianluca Frabotta           Gianluca Frabotta  23          2.0
## Gideon Mensah                   Gideon Mensah  24          2.5
## Giorgio Chiellini           Giorgio Chiellini  37          1.5
## Giovani Lo Celso             Giovani Lo Celso  26         22.0
## Giulian Biancone             Giulian Biancone  22          4.0
## Giulio Maggiore               Giulio Maggiore  24          8.5
## Gonzalo Escalante           Gonzalo Escalante  29          2.5
## Grischa Prömel                 Grischa Prömel  27          7.5
## Hamza Choudhury               Hamza Choudhury  24          4.0
## Héctor Herrera                 Héctor Herrera  32          5.0
## Henrikh Mkhitaryan         Henrikh Mkhitaryan  33          8.0
## Ignatius Ganago               Ignatius Ganago  23          5.0
## Ilan Kebbal                       Ilan Kebbal  24          4.5
## Issa Diop                           Issa Diop  25         10.0
## Jack Stephens                   Jack Stephens  28          7.0
## Jakub Jankto                     Jakub Jankto  26          4.0
## Jamie Shackleton             Jamie Shackleton  22          2.5
## Janik Haberer                   Janik Haberer  28          3.5
## Janis Antiste                   Janis Antiste  20          4.0
## Jannes Horn                       Jannes Horn  25          1.8
## Jarrad Branthwaite         Jarrad Branthwaite  20          3.0
## Jason Berthomier             Jason Berthomier  32          1.2
## Javairô Dilrosun             Javairô Dilrosun  24          6.0
## Jean-Eudes Aholou           Jean-Eudes Aholou  28          3.0
## Jean-Kévin Augustin       Jean-Kévin Augustin  25          1.5
## Jean-Philippe Gbamin     Jean-Philippe Gbamin  26          9.0
## Jens Petter Hauge           Jens Petter Hauge  22          8.0
## Jens Stryger Larsen       Jens Stryger Larsen  31          2.0
## Jesse Lingard                   Jesse Lingard  29         18.0
## Jessic Ngankam                 Jessic Ngankam  21          2.0
## Jimmy Cabot                       Jimmy Cabot  28          5.0
## Joaquín Correa                 Joaquín Correa  27         23.0
## Jonas Martin                     Jonas Martin  32          2.5
## Jonathan Silva                 Jonathan Silva  28          1.8
## Jonjoe Kenny                     Jonjoe Kenny  25          4.0
## Jordan Amavi                     Jordan Amavi  28          6.0
## Jordan Torunarigha         Jordan Torunarigha  24          5.0
## Julian Draxler                 Julian Draxler  28         18.0
## Junior Dina Ebimbe         Junior Dina Ebimbe  21          5.0
## Junior Sambia                   Junior Sambia  25          4.5
## Jurgen Ekkelenkamp         Jurgen Ekkelenkamp  22          3.5
## Justin Kluivert               Justin Kluivert  23         15.0
## Kalvin Phillips               Kalvin Phillips  26         50.0
## Kasper Dolberg                 Kasper Dolberg  24         17.0
## Keinan Davis                     Keinan Davis  24          2.5
## Kevin Mbabu                       Kevin Mbabu  27          9.0
## Kevin Stöger                     Kevin Stöger  28          1.6
## Kevin Strootman               Kevin Strootman  32          1.5
## Ki-Jana Hoever                 Ki-Jana Hoever  20          6.0
## Kingsley Ehizibue           Kingsley Ehizibue  27          1.3
## Kortney Hause                   Kortney Hause  27          4.0
## Kristoffer Askildsen     Kristoffer Askildsen  21          1.8
## Landry Dimata                   Landry Dimata  24          2.2
## László Bénes                     László Bénes  24          3.0
## Leander Dendoncker         Leander Dendoncker  27         28.0
## Leonardo Mancuso             Leonardo Mancuso  30          2.7
## Louis Schaub                     Louis Schaub  27          1.8
## Luca Pellegrini               Luca Pellegrini  23          9.0
## Lucas Alario                     Lucas Alario  29          8.0
## Lucas Ocampos                   Lucas Ocampos  28         25.0
## Lucas Perrin                     Lucas Perrin  23          5.0
## Lucas Torreira                 Lucas Torreira  26         20.0
## Ludwig Augustinsson       Ludwig Augustinsson  28          5.0
## Luis Binks                         Luis Binks  20          1.5
## Lukas Klünter                   Lukas Klünter  26          1.2
## Luuk de Jong                     Luuk de Jong  31          4.0
## Malang Sarr                       Malang Sarr  23          8.0
## Mamadou Coulibaly           Mamadou Coulibaly  23          2.5
## Manuel Akanji                   Manuel Akanji  27         30.0
## Marco John                         Marco John  20          2.5
## Marcus Forss                     Marcus Forss  23          4.0
## Marcus Ingvartsen           Marcus Ingvartsen  26          2.5
## Marko Pjaca                       Marko Pjaca  27          3.5
## Martin Braithwaite         Martin Braithwaite  31          6.0
## Mateo Klimowicz               Mateo Klimowicz  22          2.0
## Mathías Olivera               Mathías Olivera  24         15.0
## Mathias Pereira Lage     Mathias Pereira Lage  25          2.5
## Matija Nastasić               Matija Nastasić  29          1.5
## Matt Miazga                       Matt Miazga  27          2.0
## Matt Targett                     Matt Targett  26         17.0
## Mattéo Guendouzi             Mattéo Guendouzi  23         25.0
## Matteo Lovato                   Matteo Lovato  22          7.0
## Matteo Pessina                 Matteo Pessina  25         16.0
## Matthew Hoppe                   Matthew Hoppe  21          1.0
## Matthias Ginter               Matthias Ginter  28         18.0
## Matthijs de Ligt             Matthijs de Ligt  22         70.0
## Mattia Zaccagni               Mattia Zaccagni  27         18.0
## Mattias Svanberg             Mattias Svanberg  23         15.0
## Maxim Leitsch                   Maxim Leitsch  24          5.0
## Mehdi Bourabia                 Mehdi Bourabia  30          0.9
## Merih Demiral                   Merih Demiral  24         25.0
## Mihailo Ristić                 Mihailo Ristić  26          3.5
## Mikkel Damsgaard             Mikkel Damsgaard  22         15.0
## Morgan Gibbs-White         Morgan Gibbs-White  22         11.0
## Moritz Jenz                       Moritz Jenz  23          2.5
## Morten Thorsby                 Morten Thorsby  26          7.0
## Moussa Doumbia                 Moussa Doumbia  27          1.2
## Munir El Haddadi             Munir El Haddadi  26          8.0
## Nathan Tella                     Nathan Tella  23          1.8
## Neal Maupay                       Neal Maupay  26         18.0
## Neco Williams                   Neco Williams  21          8.0
## Nehuén Pérez                     Nehuén Pérez  22          6.0
## Nemanja Radonjić             Nemanja Radonjić  26          4.0
## Nicolas Pépé                     Nicolas Pépé  27         25.0
## Nicolò Casale                   Nicolò Casale  24          7.5
## Nicolò Rovella                 Nicolò Rovella  20         10.0
## Niklas Süle                       Niklas Süle  26         35.0
## Oleksandr Zinchenko       Oleksandr Zinchenko  25         25.0
## Omar Richards                   Omar Richards  24          7.0
## Orbelín Pineda                 Orbelín Pineda  26          4.0
## Orel Mangala                     Orel Mangala  24         15.0
## Oussama Idrissi               Oussama Idrissi  26          4.0
## Ozan Kabak                         Ozan Kabak  22         10.0
## Patrick Berg                     Patrick Berg  24          4.5
## Patrick Cutrone               Patrick Cutrone  24          3.5
## Paul Nebel                         Paul Nebel  19          1.3
## Philipp Förster               Philipp Förster  27          2.5
## Pierre-Yves Hamel           Pierre-Yves Hamel  28          1.2
## Pietro Pellegri               Pietro Pellegri  21          4.5
## Ragnar Ache                       Ragnar Ache  23          1.5
## Raheem Sterling               Raheem Sterling  27         70.0
## Randal Kolo Muani           Randal Kolo Muani  23         16.0
## Remo Freuler                     Remo Freuler  30         20.0
## Renato Steffen                 Renato Steffen  30          2.0
## Rey Manaj                           Rey Manaj  25          1.5
## Ricardo Pepi                     Ricardo Pepi  19          9.0
## Riccardo Calafiori         Riccardo Calafiori  20          3.5
## Roberto Piccoli               Roberto Piccoli  21          4.0
## Rolando Mandragora         Rolando Mandragora  25         10.0
## Romelu Lukaku                   Romelu Lukaku  29         70.0
## Ronaël Pierre-Gabriel   Ronaël Pierre-Gabriel  24          5.0
## Salih Özcan                       Salih Özcan  24         13.0
## Sam Lammers                       Sam Lammers  25          3.0
## Samuel Umtiti                   Samuel Umtiti  28          2.0
## Samuele Ricci                   Samuele Ricci  20         12.0
## Santiago Ascacíbar         Santiago Ascacíbar  25          6.5
## Sargis Adamyan                 Sargis Adamyan  29          3.5
## Sebastian Polter             Sebastian Polter  31          2.2
## Sebastiano Luperto         Sebastiano Luperto  25          3.0
## Sergiño Dest                     Sergiño Dest  21         18.0
## Shane Duffy                       Shane Duffy  30          5.0
## Simon Banza                       Simon Banza  25          5.0
## Sofian Kiyine                   Sofian Kiyine  24          2.1
## Sofiane Diop                     Sofiane Diop  22         20.0
## Stefano Sensi                   Stefano Sensi  26          7.0
## Steffen Tigges                 Steffen Tigges  23          1.5
## Stéphane Bahoken             Stéphane Bahoken  30          4.0
## Steven Bergwijn               Steven Bergwijn  24         18.0
## Sven Botman                       Sven Botman  22         30.0
## Taiwo Awoniyi                   Taiwo Awoniyi  24         20.0
## Takefusa Kubo                   Takefusa Kubo  21          7.5
## Takumi Minamino               Takumi Minamino  27         12.0
## Tanguy Nianzou                 Tanguy Nianzou  20          9.0
## Tariqe Fosu                       Tariqe Fosu  26          2.0
## Thilo Kehrer                     Thilo Kehrer  25         22.0
## Thomas Monconduit           Thomas Monconduit  31          1.5
## Timo Werner                       Timo Werner  26         35.0
## Tino Kadewere                   Tino Kadewere  26          6.0
## Uroš Račić                         Uroš Račić  24          7.0
## Vedat Muriqi                     Vedat Muriqi  28          7.0
## Wesley Fofana                   Wesley Fofana  21         40.0
## Willy Boly                         Willy Boly  31          6.0
## Xaver Schlager                 Xaver Schlager  24         27.0
## Xavi Simons                       Xavi Simons  19          4.0
## Yan Valery                         Yan Valery  23          2.5
## Yangel Herrera                 Yangel Herrera  24         14.0
## Youssouf Koné                   Youssouf Koné  27          2.0
## Yves Bissouma                   Yves Bissouma  25         35.0
## Zinho Vanheusden             Zinho Vanheusden  22          8.0
##                        Transfer Fee (Millions) Origin Position            Club
## Aaron Connolly                            0.00    IRL       FW        Brighton
## Aaron Hickey                             16.50    SCO       DF         Bologna
## Aaron Ramsey                              0.00    WAL       MF        Juventus
## Abdou Diallo                              1.50    SEN       DF       Paris S-G
## Adam Ounas                                3.00    ALG       MF          Napoli
## Ademola Lookman                           9.00    NGA       FW  Leicester City
## Adnan Januzaj                             0.00    BEL       FW   Real Sociedad
## Ainsley Maitland-Niles                    0.00    ENG       MF         Arsenal
## Akim Zedadka                              0.00    ALG       DF   Clermont Foot
## Alessandro Florenzi                       2.70    ITA       DF           Milan
## Alessio Romagnoli                         0.00    ITA       DF           Milan
## Alexander Isak                           70.00    SWE       FW   Real Sociedad
## Alexandre Lacazette                       0.00    FRA       FW         Arsenal
## Alexis Claude-Maurice                     0.00    FRA       MF            Nice
## Alexis Sánchez                            0.00    CHI       FW           Inter
## Amadou Diawara                            1.50    GUI       MF            Roma
## Amadou Onana                             35.00    BEL       MF           Lille
## Amine Harit                               0.00    MAR       FW       Marseille
## Anderson Lucoqui                          0.00    ANG       DF        Mainz 05
## Andi Zeqiri                               0.00    SUI       FW        Augsburg
## Andrea Belotti                            0.00    ITA       FW          Torino
## Andrea La Mantia                          0.00    ITA       FW          Empoli
## Andrea Petagna                            2.50    ITA       FW          Napoli
## Andrea Pinamonti                          0.00    ITA       FW          Empoli
## Andreas Christensen                       0.00    DEN       DF         Chelsea
## Andreas Voglsammer                        0.00    GER       FW    Union Berlin
## Andriy Yarmolenko                         0.00    UKR       FW        West Ham
## Ángel Di María                            0.00    ARG       FW       Paris S-G
## Angelo Fulgini                            6.00    FRA       MF          Angers
## Anthony Caci                              0.00    FRA       DF      Strasbourg
## Antonio Rüdiger                           0.00    GER       DF         Chelsea
## Anwar El Ghazi                            2.50    NED       FW         Everton
## Arkadiusz Milik                           0.90    POL       FW       Marseille
## Armando Izzo                              0.00    ITA       DF          Torino
## Arne Maier                                5.00    GER       MF        Augsburg
## Arthur Theate                            19.00    BEL       DF         Bologna
## Aster Vranckx                             2.00    BEL       MF       Wolfsburg
## Axel Witsel                               0.00    BEL       MF        Dortmund
## Aymen Barkok                              0.00    MAR       MF  Eint Frankfurt
## Ben Davies                                4.70    WAL       DF       Tottenham
## Bertrand Traoré                           0.00    BFA       MF     Aston Villa
## Billy Gilmour                            10.40    SCO       MF    Norwich City
## Boubacar Kamara                           0.00    FRA       MF       Marseille
## Boulaye Dia                               1.00    SEN       FW      Villarreal
## Brandon Soppy                             9.00    FRA       DF         Udinese
## Breel Embolo                             12.50    SUI       FW      M'Gladbach
## Brian Brobbey                            16.35    NED       FW      RB Leipzig
## Bryan Reynolds                            0.00    USA       DF            Roma
## Callum Hudson-Odoi                        0.00    ENG       MF         Chelsea
## Calvin Stengs                             0.00    NED       MF            Nice
## Cédric Hountondji                         2.00    BEN       DF   Clermont Foot
## Cengiz Ünder                              8.40    TUR       FW       Marseille
## Cenk Tosun                                0.00    TUR       MF         Everton
## Cheick Doucouré                          22.60    MLI       MF            Lens
## Cheikhou Kouyaté                          0.00    SEN       MF  Crystal Palace
## Chris Richards                           12.00    USA       DF   Bayern Munich
## Christian Benteke                         5.46    BEL       FW  Crystal Palace
## Christian Eriksen                         0.00    DEN       MF       Brentford
## Ciaran Clark                              0.00    IRL       DF   Newcastle Utd
## Clément Lenglet                           0.00    FRA       DF       Barcelona
## Colin Dagba                               0.00    FRA       DF       Paris S-G
## Corentin Jean                             1.09    FRA       FW            Lens
## Corentin Tolisso                          0.00    FRA       MF   Bayern Munich
## Cristian Romero                          50.00    ARG       DF       Tottenham
## Dan Ndoye                                 0.00    SUI       FW            Nice
## Daniel James                              0.00    WAL       FW    Leeds United
## Daniel Maldini                            0.00    ITA       MF           Milan
## Daniel Wass                               1.75    DEN       DF Atlético Madrid
## Danny da Costa                            0.00    GER       DF  Eint Frankfurt
## David Nemeth                              1.30    AUT       DF        Mainz 05
## Dele Alli                                 0.00    ENG       MF       Tottenham
## Denis Cheryshev                           0.00    RUS       MF        Valencia
## Denis Vavro                               4.50    SVK       DF           Lazio
## Denis Zakaria                             3.00    SUI       MF        Juventus
## Diego Lainez                              0.00    MEX       FW           Betis
## Divock Origi                              0.00    BEL       FW       Liverpool
## Dominique Heintz                          0.00    GER       DF        Freiburg
## Dries Mertens                             0.00    BEL       FW          Napoli
## Dwight Gayle                              0.00    ENG       FW   Newcastle Utd
## Dylan Chambost                            0.00    FRA       MF          Troyes
## Ebrima Colley                             0.00    GAM       FW          Spezia
## Eddie Salcedo                             0.00    ITA       MF          Spezia
## Edinson Cavani                            0.00    URU       FW  Manchester Utd
## El Bilal Touré                           10.00    MLI       FW           Reims
## Enzo Ebosse                               4.00    CMR       DF          Angers
## Eric Bailly                               2.00    CIV       DF  Manchester Utd
## Erick Cabaco                              0.00    URU       DF          Getafe
## Erik Durm                                 0.00    GER       DF  Eint Frankfurt
## Erik Thommy                               0.00    GER       FW       Stuttgart
## Evann Guessand                            0.00    FRA       FW            Nice
## Fabio Depaoli                             0.00    ITA       MF       Sampdoria
## Federico Bernardeschi                     0.00    ITA       MF        Juventus
## Federico Bonazzoli                        5.00    ITA       FW     Salernitana
## Federico Chiesa                          40.00    ITA       MF        Juventus
## Federico Fernández                        0.00    ARG       DF   Newcastle Utd
## Filippo Melegoni                          4.00    ITA       MF           Genoa
## Flavius Daniliuc                          5.00    AUT       DF            Nice
## Florent Mollet                            0.50    FRA       FW     Montpellier
## Francesco Caputo                          3.50    ITA       FW        Sassuolo
## Francesco Di Mariano                      1.00    ITA       FW         Venezia
## Gaëtan Laborde                           15.00    FRA       FW     Montpellier
## Gareth Bale                               0.00    WAL       FW     Real Madrid
## Georginio Wijnaldum                       0.00    NED       MF       Paris S-G
## Ghislain Konan                            0.00    CIV       DF           Reims
## Giacomo Raspadori                         5.00    ITA       MF        Sassuolo
## Gianluca Frabotta                         0.00    ITA       DF   Hellas Verona
## Gideon Mensah                             0.00    GHA       DF        Bordeaux
## Giorgio Chiellini                         0.00    ITA       DF        Juventus
## Giovani Lo Celso                          0.00    ARG       MF       Tottenham
## Giulian Biancone                         10.00    FRA       DF          Troyes
## Giulio Maggiore                           4.50    ITA       MF          Spezia
## Gonzalo Escalante                         0.00    ARG       MF           Lazio
## Grischa Prömel                            0.00    GER       MF    Union Berlin
## Hamza Choudhury                           0.00    ENG       DF  Leicester City
## Héctor Herrera                            0.00    MEX       MF Atlético Madrid
## Henrikh Mkhitaryan                        0.00    ARM       MF            Roma
## Ignatius Ganago                           4.40    CMR       FW            Lens
## Ilan Kebbal                               0.00    ALG       MF           Reims
## Issa Diop                                17.80    FRA       DF        West Ham
## Jack Stephens                             0.00    ENG       DF     Southampton
## Jakub Jankto                              0.00    CZE       DF          Getafe
## Jamie Shackleton                          0.00    ENG       DF    Leeds United
## Janik Haberer                             0.00    GER       MF        Freiburg
## Janis Antiste                             0.00    FRA       FW          Spezia
## Jannes Horn                               0.00    GER       DF            Köln
## Jarrad Branthwaite                        0.00    ENG       DF         Everton
## Jason Berthomier                          0.00    FRA       MF   Clermont Foot
## Javairô Dilrosun                          4.00    NED       MF        Bordeaux
## Jean-Eudes Aholou                         3.00    CIV       MF      Strasbourg
## Jean-Kévin Augustin                       0.00    FRA       FW          Nantes
## Jean-Philippe Gbamin                      0.00    CIV       MF         Everton
## Jens Petter Hauge                        10.00    NOR       MF  Eint Frankfurt
## Jens Stryger Larsen                       0.00    DEN       DF         Udinese
## Jesse Lingard                             0.00    ENG       FW  Manchester Utd
## Jessic Ngankam                            1.50    GER       FW  Greuther Fürth
## Jimmy Cabot                               2.00    FRA       DF          Angers
## Joaquín Correa                           23.60    ARG       FW           Inter
## Jonas Martin                              0.00    FRA       MF          Rennes
## Jonathan Silva                            0.00    ARG       DF          Getafe
## Jonjoe Kenny                              0.00    ENG       DF         Everton
## Jordan Amavi                              0.00    FRA       DF            Nice
## Jordan Torunarigha                        3.50    GER       DF      Hertha BSC
## Julian Draxler                            2.50    GER       FW       Paris S-G
## Junior Dina Ebimbe                        0.00    FRA       MF       Paris S-G
## Junior Sambia                             0.00    FRA       DF     Montpellier
## Jurgen Ekkelenkamp                        5.00    NED       MF      Hertha BSC
## Justin Kluivert                           0.00    NED       MF            Nice
## Kalvin Phillips                          49.00    ENG       MF    Leeds United
## Kasper Dolberg                            0.00    DEN       FW            Nice
## Keinan Davis                              0.00    ENG       FW     Aston Villa
## Kevin Mbabu                               5.50    SUI       DF       Wolfsburg
## Kevin Stöger                              0.00    AUT       MF        Mainz 05
## Kevin Strootman                           0.00    NED       MF        Cagliari
## Ki-Jana Hoever                            0.00    NED       DF          Wolves
## Kingsley Ehizibue                         1.00    NED       DF            Köln
## Kortney Hause                             0.00    ENG       DF     Aston Villa
## Kristoffer Askildsen                      0.00    NOR       MF       Sampdoria
## Landry Dimata                             0.00    BEL       FW        Espanyol
## László Bénes                              2.00    SVK       MF      M'Gladbach
## Leander Dendoncker                       15.00    BEL       MF          Wolves
## Leonardo Mancuso                          0.00    ITA       FW          Empoli
## Louis Schaub                              0.00    AUT       MF            Köln
## Luca Pellegrini                           0.00    ITA       DF        Juventus
## Lucas Alario                              6.00    ARG       FW      Leverkusen
## Lucas Ocampos                             4.00    ARG       FW         Sevilla
## Lucas Perrin                              1.50    FRA       DF      Strasbourg
## Lucas Torreira                            6.00    URU       MF      Fiorentina
## Ludwig Augustinsson                       0.50    SWE       DF         Sevilla
## Luis Binks                                0.00    ENG       DF         Bologna
## Lukas Klünter                             0.00    GER       DF      Hertha BSC
## Luuk de Jong                              3.00    NED       FW       Barcelona
## Malang Sarr                               1.00    FRA       DF         Chelsea
## Mamadou Coulibaly                         0.10    SEN       MF     Salernitana
## Manuel Akanji                            17.50    SUI       DF        Dortmund
## Marco John                                0.00    GER       DF      Hoffenheim
## Marcus Forss                              3.60    FIN       FW       Brentford
## Marcus Ingvartsen                         2.30    DEN       FW        Mainz 05
## Marko Pjaca                               0.00    CRO       MF          Torino
## Martin Braithwaite                        0.00    DEN       FW       Barcelona
## Mateo Klimowicz                           0.00    GER       MF       Stuttgart
## Mathías Olivera                          11.00    URU       DF          Getafe
## Mathias Pereira Lage                      0.00    POR       DF          Angers
## Matija Nastasić                           0.00    SRB       DF      Fiorentina
## Matt Miazga                               0.00    USA       DF          Alavés
## Matt Targett                             17.50    ENG       DF     Aston Villa
## Mattéo Guendouzi                         11.00    FRA       MF       Marseille
## Matteo Lovato                             7.00    ITA       DF        Cagliari
## Matteo Pessina                            0.00    ITA       MF        Atalanta
## Matthew Hoppe                             3.00    USA       FW        Mallorca
## Matthias Ginter                           0.00    GER       DF      M'Gladbach
## Matthijs de Ligt                         67.00    NED       DF        Juventus
## Mattia Zaccagni                           7.10    ITA       MF   Hellas Verona
## Mattias Svanberg                          9.00    SWE       MF         Bologna
## Maxim Leitsch                             3.50    GER       DF          Bochum
## Mehdi Bourabia                            1.50    MAR       MF          Spezia
## Merih Demiral                            20.00    TUR       DF        Atalanta
## Mihailo Ristić                            0.00    SRB       DF     Montpellier
## Mikkel Damsgaard                         15.00    DEN       MF       Sampdoria
## Morgan Gibbs-White                       29.50    ENG       DF          Wolves
## Moritz Jenz                               0.00    GER       DF         Lorient
## Morten Thorsby                            3.00    NOR       MF       Sampdoria
## Moussa Doumbia                            0.00    MLI       FW           Reims
## Munir El Haddadi                          0.00    MAR       FW         Sevilla
## Nathan Tella                              0.00    ENG       MF     Southampton
## Neal Maupay                              11.80    FRA       FW        Brighton
## Neco Williams                            20.00    WAL       DF       Liverpool
## Nehuén Pérez                              0.00    ARG       DF         Udinese
## Nemanja Radonjić                          0.00    SRB       FW       Marseille
## Nicolas Pépé                              0.00    CIV       FW         Arsenal
## Nicolò Casale                             7.00    ITA       DF   Hellas Verona
## Nicolò Rovella                            0.00    ITA       MF           Genoa
## Niklas Süle                               0.00    GER       DF   Bayern Munich
## Oleksandr Zinchenko                      35.00    UKR       DF Manchester City
## Omar Richards                             8.50    ENG       DF   Bayern Munich
## Orbelín Pineda                            0.00    MEX       MF      Celta Vigo
## Orel Mangala                             13.00    BEL       MF       Stuttgart
## Oussama Idrissi                           0.00    MAR       MF           Cádiz
## Ozan Kabak                                5.00    TUR       DF    Norwich City
## Patrick Berg                              4.50    NOR       MF            Lens
## Patrick Cutrone                           0.00    ITA       FW          Empoli
## Paul Nebel                                0.00    GER       MF        Mainz 05
## Philipp Förster                           0.50    GER       MF       Stuttgart
## Pierre-Yves Hamel                         1.00    FRA       FW   Clermont Foot
## Pietro Pellegri                           5.00    ITA       FW           Milan
## Ragnar Ache                               0.00    GER       MF  Eint Frankfurt
## Raheem Sterling                          56.20    ENG       FW Manchester City
## Randal Kolo Muani                         0.00    FRA       FW          Nantes
## Remo Freuler                              9.00    SUI       MF        Atalanta
## Renato Steffen                            0.75    SUI       FW       Wolfsburg
## Rey Manaj                                 0.00    ALB       FW          Spezia
## Ricardo Pepi                              0.50    USA       FW        Augsburg
## Riccardo Calafiori                        1.00    ITA       DF           Genoa
## Roberto Piccoli                           0.00    ITA       FW           Genoa
## Rolando Mandragora                        8.20    ITA       MF          Torino
## Romelu Lukaku                             8.00    BEL       FW         Chelsea
## Ronaël Pierre-Gabriel                     0.00    FRA       DF           Brest
## Salih Özcan                               5.00    TUR       MF            Köln
## Sam Lammers                               0.00    NED       FW        Atalanta
## Samuel Umtiti                             0.00    FRA       DF       Barcelona
## Samuele Ricci                             8.50    ITA       MF          Torino
## Santiago Ascacíbar                        0.00    ARG       MF      Hertha BSC
## Sargis Adamyan                            1.50    ARM       FW      Hoffenheim
## Sebastian Polter                          1.50    GER       FW          Bochum
## Sebastiano Luperto                        0.50    ITA       DF          Empoli
## Sergiño Dest                              0.00    USA       DF       Barcelona
## Shane Duffy                               0.00    IRL       DF        Brighton
## Simon Banza                               3.00    FRA       FW            Lens
## Sofian Kiyine                             0.00    MAR       MF         Venezia
## Sofiane Diop                             22.00    FRA       MF          Monaco
## Stefano Sensi                             0.00    ITA       MF           Inter
## Steffen Tigges                            1.50    GER       FW        Dortmund
## Stéphane Bahoken                          0.00    CMR       FW          Angers
## Steven Bergwijn                          31.25    NED       FW       Tottenham
## Sven Botman                              37.00    NED       DF           Lille
## Taiwo Awoniyi                            20.50    NGA       FW    Union Berlin
## Takefusa Kubo                             6.50    JPN       FW        Mallorca
## Takumi Minamino                          15.00    JPN       FW       Liverpool
## Tanguy Nianzou                           16.00    FRA       DF   Bayern Munich
## Tariqe Fosu                               0.00    GHA       DF       Brentford
## Thilo Kehrer                             12.00    GER       DF       Paris S-G
## Thomas Monconduit                         0.50    FRA       MF         Lorient
## Timo Werner                              20.00    GER       FW         Chelsea
## Tino Kadewere                             0.40    ZIM       FW            Lyon
## Uroš Račić                                0.00    SRB       MF        Valencia
## Vedat Muriqi                              7.70    KVX       FW           Lazio
## Wesley Fofana                            80.40    FRA       DF  Leicester City
## Willy Boly                                2.60    CIV       DF          Wolves
## Xaver Schlager                           12.00    AUT       MF       Wolfsburg
## Xavi Simons                               0.00    NED       MF       Paris S-G
## Yan Valery                                0.00    TUN       DF     Southampton
## Yangel Herrera                            0.00    VEN       MF        Espanyol
## Youssouf Koné                             0.00    MLI       DF          Troyes
## Yves Bissouma                            29.20    MLI       MF        Brighton
## Zinho Vanheusden                          0.00    BEL       DF           Genoa
##                                League Matches  Played Goals Assists
## Aaron Connolly         Premier League               4     0       0
## Aaron Hickey                  Serie A              36     5       1
## Aaron Ramsey                  Serie A               3     0       0
## Abdou Diallo                  Ligue 1              12     0       1
## Adam Ounas                    Serie A              15     0       0
## Ademola Lookman        Premier League              26     6       0
## Adnan Januzaj                 La Liga              33     3       0
## Ainsley Maitland-Niles Premier League               8     0       0
## Akim Zedadka                  Ligue 1              38     0       3
## Alessandro Florenzi           Serie A              24     2       0
## Alessio Romagnoli             Serie A              19     1       0
## Alexander Isak                La Liga              32     6       2
## Alexandre Lacazette    Premier League              30     4       7
## Alexis Claude-Maurice         Ligue 1              10     0       2
## Alexis Sánchez                Serie A              27     5       2
## Amadou Diawara                Serie A               4     0       0
## Amadou Onana                  Ligue 1              32     1       0
## Amine Harit                   Ligue 1              23     4       4
## Anderson Lucoqui           Bundesliga              13     1       0
## Andi Zeqiri                Bundesliga              22     2       1
## Andrea Belotti                Serie A              22     8       1
## Andrea La Mantia              Serie A              18     2       0
## Andrea Petagna                Serie A              24     3       1
## Andrea Pinamonti              Serie A              36    13       2
## Andreas Christensen    Premier League              19     0       0
## Andreas Voglsammer         Bundesliga              32     2       1
## Andriy Yarmolenko      Premier League              19     1       0
## Ángel Di María                Ligue 1              26     5       7
## Angelo Fulgini                Ligue 1              36     5       4
## Anthony Caci                  Ligue 1              37     1       3
## Antonio Rüdiger        Premier League              34     3       0
## Anwar El Ghazi         Premier League               2     0       0
## Arkadiusz Milik               Ligue 1              23     7       2
## Armando Izzo                  Serie A              11     0       0
## Arne Maier                 Bundesliga              29     1       6
## Arthur Theate                 Serie A              31     2       1
## Aster Vranckx              Bundesliga              24     2       0
## Axel Witsel                Bundesliga              29     2       0
## Aymen Barkok               Bundesliga               5     0       0
## Ben Davies             Premier League              29     1       1
## Bertrand Traoré        Premier League               9     0       0
## Billy Gilmour          Premier League              24     0       1
## Boubacar Kamara               Ligue 1              34     1       0
## Boulaye Dia                   La Liga              25     5       5
## Brandon Soppy                 Serie A              28     0       0
## Breel Embolo               Bundesliga              29     9       3
## Brian Brobbey              Bundesliga               9     0       2
## Bryan Reynolds                Serie A               1     0       0
## Callum Hudson-Odoi     Premier League              15     1       2
## Calvin Stengs                 Ligue 1              24     1       1
## Cédric Hountondji             Ligue 1              23     1       0
## Cengiz Ünder                  Ligue 1              32    10       2
## Cenk Tosun             Premier League               1     0       0
## Cheick Doucouré               Ligue 1              34     1       4
## Cheikhou Kouyaté       Premier League              27     0       1
## Chris Richards             Bundesliga               1     0       0
## Christian Benteke      Premier League              25     4       1
## Christian Eriksen      Premier League              11     1       4
## Ciaran Clark           Premier League              13     0       1
## Clément Lenglet               La Liga              21     0       1
## Colin Dagba                   Ligue 1               3     0       0
## Corentin Jean                 Ligue 1              17     1       0
## Corentin Tolisso           Bundesliga              15     2       1
## Cristian Romero        Premier League              22     1       0
## Dan Ndoye                     Ligue 1               3     0       1
## Daniel James           Premier League              32     4       4
## Daniel Maldini                Serie A               8     1       0
## Daniel Wass                   La Liga               1     0       0
## Danny da Costa             Bundesliga              11     0       0
## David Nemeth               Bundesliga               6     0       0
## Dele Alli              Premier League              10     1       0
## Denis Cheryshev               La Liga              17     0       1
## Denis Vavro                   Serie A               1     0       0
## Denis Zakaria                 Serie A               9     1       1
## Diego Lainez                  La Liga               7     0       0
## Divock Origi           Premier League               7     3       0
## Dominique Heintz           Bundesliga               1     0       0
## Dries Mertens                 Serie A              30    11       1
## Dwight Gayle           Premier League               8     0       0
## Dylan Chambost                Ligue 1              19     0       1
## Ebrima Colley                 Serie A              11     0       0
## Eddie Salcedo                 Serie A              12     0       0
## Edinson Cavani         Premier League              15     2       1
## El Bilal Touré                Ligue 1              21     2       4
## Enzo Ebosse                   Ligue 1              27     0       0
## Eric Bailly            Premier League               4     0       0
## Erick Cabaco                  La Liga              10     0       1
## Erik Durm                  Bundesliga               7     0       1
## Erik Thommy                Bundesliga              11     0       0
## Evann Guessand                Ligue 1              20     1       2
## Fabio Depaoli                 Serie A               6     0       0
## Federico Bernardeschi         Serie A              28     1       3
## Federico Bonazzoli            Serie A              32    10       0
## Federico Chiesa               Serie A              14     2       2
## Federico Fernández     Premier League               7     0       0
## Filippo Melegoni              Serie A              25     1       0
## Flavius Daniliuc              Ligue 1              24     0       0
## Florent Mollet                Ligue 1              35     6       2
## Francesco Caputo              Serie A               2     0       1
## Francesco Di Mariano          Serie A               1     0       0
## Gaëtan Laborde                Ligue 1               4     3       0
## Gareth Bale                   La Liga               5     1       0
## Georginio Wijnaldum           Ligue 1              31     1       3
## Ghislain Konan                Ligue 1              28     1       3
## Giacomo Raspadori             Serie A              36    10       4
## Gianluca Frabotta             Serie A               2     0       0
## Gideon Mensah                 Ligue 1              23     0       2
## Giorgio Chiellini             Serie A              21     0       0
## Giovani Lo Celso       Premier League               9     0       0
## Giulian Biancone              Ligue 1              33     1       2
## Giulio Maggiore               Serie A              35     2       3
## Gonzalo Escalante             Serie A               1     0       0
## Grischa Prömel             Bundesliga              29     8       1
## Hamza Choudhury        Premier League               6     0       0
## Héctor Herrera                La Liga              21     0       0
## Henrikh Mkhitaryan            Serie A              31     5       6
## Ignatius Ganago               Ligue 1              28     5       1
## Ilan Kebbal                   Ligue 1              31     1       4
## Issa Diop              Premier League              13     0       1
## Jack Stephens          Premier League              11     0       0
## Jakub Jankto                  La Liga              14     0       2
## Jamie Shackleton       Premier League              14     0       0
## Janik Haberer              Bundesliga              26     3       0
## Janis Antiste                 Serie A              18     1       0
## Jannes Horn                Bundesliga              11     0       0
## Jarrad Branthwaite     Premier League               6     1       0
## Jason Berthomier              Ligue 1              29     1       5
## Javairô Dilrosun              Ligue 1              32     2       6
## Jean-Eudes Aholou             Ligue 1              25     1       1
## Jean-Kévin Augustin           Ligue 1               7     0       0
## Jean-Philippe Gbamin   Premier League               3     0       0
## Jens Petter Hauge          Bundesliga              26     2       2
## Jens Stryger Larsen           Serie A              11     0       1
## Jesse Lingard          Premier League              16     2       0
## Jessic Ngankam             Bundesliga               6     2       1
## Jimmy Cabot                   Ligue 1              24     0       5
## Joaquín Correa                Serie A              26     6       1
## Jonas Martin                  Ligue 1              29     1       1
## Jonathan Silva                La Liga               9     0       0
## Jonjoe Kenny           Premier League              15     0       1
## Jordan Amavi                  Ligue 1               8     0       0
## Jordan Torunarigha         Bundesliga               7     0       1
## Julian Draxler                Ligue 1              18     2       1
## Junior Dina Ebimbe            Ligue 1              10     0       0
## Junior Sambia                 Ligue 1              30     1       0
## Jurgen Ekkelenkamp         Bundesliga              21     3       0
## Justin Kluivert               Ligue 1              27     4       5
## Kalvin Phillips        Premier League              20     0       1
## Kasper Dolberg                Ligue 1              26     6       2
## Keinan Davis           Premier League               1     0       0
## Kevin Mbabu                Bundesliga              24     0       0
## Kevin Stöger               Bundesliga              23     1       1
## Kevin Strootman               Serie A              10     0       0
## Ki-Jana Hoever         Premier League               8     0       0
## Kingsley Ehizibue          Bundesliga              16     0       1
## Kortney Hause          Premier League               7     1       0
## Kristoffer Askildsen          Serie A              19     0       0
## Landry Dimata                 La Liga              17     0       0
## László Bénes               Bundesliga              13     0       0
## Leander Dendoncker     Premier League              30     2       2
## Leonardo Mancuso              Serie A               8     1       0
## Louis Schaub               Bundesliga              28     1       2
## Luca Pellegrini               Serie A              18     0       1
## Lucas Alario               Bundesliga              27     6       0
## Lucas Ocampos                 La Liga              30     6       4
## Lucas Perrin                  Ligue 1              31     1       1
## Lucas Torreira                Serie A              31     5       1
## Ludwig Augustinsson           La Liga              19     0       1
## Luis Binks                    Serie A              15     0       0
## Lukas Klünter              Bundesliga               5     0       0
## Luuk de Jong                  La Liga              21     6       0
## Malang Sarr            Premier League               8     0       0
## Mamadou Coulibaly             Serie A              12     2       0
## Manuel Akanji              Bundesliga              26     1       0
## Marco John                 Bundesliga               2     0       0
## Marcus Forss           Premier League               7     0       1
## Marcus Ingvartsen          Bundesliga              26     6       0
## Marko Pjaca                   Serie A              24     3       0
## Martin Braithwaite            La Liga               4     2       1
## Mateo Klimowicz            Bundesliga              15     0       1
## Mathías Olivera               La Liga              32     1       3
## Mathias Pereira Lage          Ligue 1              24     4       2
## Matija Nastasić               Serie A               5     0       0
## Matt Miazga                   La Liga              11     0       0
## Matt Targett           Premier League              17     1       1
## Mattéo Guendouzi              Ligue 1              38     4       6
## Matteo Lovato                 Serie A              16     0       0
## Matteo Pessina                Serie A              27     1       1
## Matthew Hoppe                 La Liga               5     0       1
## Matthias Ginter            Bundesliga              28     1       0
## Matthijs de Ligt              Serie A              31     3       1
## Mattia Zaccagni               Serie A               2     2       0
## Mattias Svanberg              Serie A              36     3       3
## Maxim Leitsch              Bundesliga              19     1       0
## Mehdi Bourabia                Serie A               8     1       0
## Merih Demiral                 Serie A              28     1       2
## Mihailo Ristić                Ligue 1              29     1       3
## Mikkel Damsgaard              Serie A              11     0       0
## Morgan Gibbs-White     Premier League               2     0       0
## Moritz Jenz                   Ligue 1              17     1       0
## Morten Thorsby                Serie A              35     3       1
## Moussa Doumbia                Ligue 1               4     0       0
## Munir El Haddadi              La Liga              16     2       0
## Nathan Tella           Premier League              14     0       1
## Neal Maupay            Premier League              32     8       2
## Neco Williams          Premier League               1     0       1
## Nehuén Pérez                  Serie A              20     0       1
## Nemanja Radonjić              Ligue 1               1     0       0
## Nicolas Pépé           Premier League              20     1       2
## Nicolò Casale                 Serie A              36     0       1
## Nicolò Rovella                Serie A              21     0       3
## Niklas Süle                Bundesliga              28     1       2
## Oleksandr Zinchenko    Premier League              15     0       4
## Omar Richards              Bundesliga              12     0       0
## Orbelín Pineda                La Liga               7     0       0
## Orel Mangala               Bundesliga              28     1       3
## Oussama Idrissi               La Liga              12     1       1
## Ozan Kabak             Premier League              11     0       0
## Patrick Berg                  Ligue 1              14     0       0
## Patrick Cutrone               Serie A              28     3       0
## Paul Nebel                 Bundesliga              10     0       1
## Philipp Förster            Bundesliga              20     2       1
## Pierre-Yves Hamel             Ligue 1              17     0       0
## Pietro Pellegri               Serie A               6     0       0
## Ragnar Ache                Bundesliga              13     0       1
## Raheem Sterling        Premier League              30    13       5
## Randal Kolo Muani             Ligue 1              36    12       4
## Remo Freuler                  Serie A              29     1       3
## Renato Steffen             Bundesliga              20     0       1
## Rey Manaj                     Serie A              30     5       0
## Ricardo Pepi               Bundesliga              11     0       0
## Riccardo Calafiori            Serie A               3     0       0
## Roberto Piccoli               Serie A               5     0       0
## Rolando Mandragora            Serie A              21     0       2
## Romelu Lukaku          Premier League              26     8       0
## Ronaël Pierre-Gabriel         Ligue 1              30     0       1
## Salih Özcan                Bundesliga              31     2       0
## Sam Lammers                   Serie A               2     0       0
## Samuel Umtiti                 La Liga               1     0       0
## Samuele Ricci                 Serie A              12     0       0
## Santiago Ascacíbar         Bundesliga              28     0       1
## Sargis Adamyan             Bundesliga              13     1       0
## Sebastian Polter           Bundesliga              33    10       1
## Sebastiano Luperto            Serie A              24     0       1
## Sergiño Dest                  La Liga              21     0       3
## Shane Duffy            Premier League              18     1       0
## Simon Banza                   Ligue 1               4     1       1
## Sofian Kiyine                 Serie A              26     1       0
## Sofiane Diop                  Ligue 1              30     6       4
## Stefano Sensi                 Serie A               9     0       0
## Steffen Tigges             Bundesliga               9     3       0
## Stéphane Bahoken              Ligue 1              29     3       1
## Steven Bergwijn        Premier League              25     3       1
## Sven Botman                   Ligue 1              25     3       1
## Taiwo Awoniyi              Bundesliga              31    15       1
## Takefusa Kubo                 La Liga              28     1       0
## Takumi Minamino        Premier League              11     3       0
## Tanguy Nianzou             Bundesliga              17     1       1
## Tariqe Fosu            Premier League               1     0       0
## Thilo Kehrer                  Ligue 1              27     2       0
## Thomas Monconduit             Ligue 1              34     3       2
## Timo Werner            Premier League              21     4       1
## Tino Kadewere                 Ligue 1              15     1       0
## Uroš Račić                    La Liga              28     0       0
## Vedat Muriqi                  Serie A              11     0       0
## Wesley Fofana          Premier League               7     0       0
## Willy Boly             Premier League              10     0       0
## Xaver Schlager             Bundesliga              14     0       1
## Xavi Simons                   Ligue 1               6     0       0
## Yan Valery             Premier League               5     0       0
## Yangel Herrera                La Liga              23     1       1
## Youssouf Koné                 Ligue 1              21     0       1
## Yves Bissouma          Premier League              26     1       2
## Zinho Vanheusden              Serie A              14     0       0
##                        Goals+Assists Total Shots Shots on Target SOT %
## Aaron Connolly                     0           2               0   0.0
## Aaron Hickey                       6          21              13  61.9
## Aaron Ramsey                       0           0               0   0.0
## Abdou Diallo                       1           2               1  50.0
## Adam Ounas                         0          13               2  15.4
## Ademola Lookman                    6          26              12  46.2
## Adnan Januzaj                      3          40              13  32.5
## Ainsley Maitland-Niles             0           2               0   0.0
## Akim Zedadka                       3          31               6  19.4
## Alessandro Florenzi                2          22               6  27.3
## Alessio Romagnoli                  1          12               5  41.7
## Alexander Isak                     8          71              33  46.5
## Alexandre Lacazette               11          43              12  27.9
## Alexis Claude-Maurice              2           6               1  16.7
## Alexis Sánchez                     7          33              13  39.4
## Amadou Diawara                     0           1               0   0.0
## Amadou Onana                       1          15               6  40.0
## Amine Harit                        8          15               7  46.7
## Anderson Lucoqui                   1           8               4  50.0
## Andi Zeqiri                        3          30               8  26.7
## Andrea Belotti                     9          55              17  30.9
## Andrea La Mantia                   2          17               3  17.6
## Andrea Petagna                     4          13               6  46.2
## Andrea Pinamonti                  15          86              23  26.7
## Andreas Christensen                0           5               0   0.0
## Andreas Voglsammer                 3          33              15  45.5
## Andriy Yarmolenko                  1          11               5  45.5
## Ángel Di María                    12          51              22  43.1
## Angelo Fulgini                     9          58              17  29.3
## Anthony Caci                       4          15               4  26.7
## Antonio Rüdiger                    3          48              13  27.1
## Anwar El Ghazi                     0           0               0   0.0
## Arkadiusz Milik                    9          53              19  35.8
## Armando Izzo                       0           5               3  60.0
## Arne Maier                         7          17               8  47.1
## Arthur Theate                      3          21               6  28.6
## Aster Vranckx                      2          25               8  32.0
## Axel Witsel                        2           8               2  25.0
## Aymen Barkok                       0           2               1  50.0
## Ben Davies                         2          18               7  38.9
## Bertrand Traoré                    0           8               1  12.5
## Billy Gilmour                      1          18               3  16.7
## Boubacar Kamara                    1          22               3  13.6
## Boulaye Dia                       10          37              11  29.7
## Brandon Soppy                      0           6               2  33.3
## Breel Embolo                      12          48              23  47.9
## Brian Brobbey                      2           4               0   0.0
## Bryan Reynolds                     0           0               0   0.0
## Callum Hudson-Odoi                 3          18               7  38.9
## Calvin Stengs                      2          21               5  23.8
## Cédric Hountondji                  1           8               3  37.5
## Cengiz Ünder                      12          57              19  33.3
## Cenk Tosun                         0           0               0   0.0
## Cheick Doucouré                    5          31              11  35.5
## Cheikhou Kouyaté                   1           7               0   0.0
## Chris Richards                     0           0               0   0.0
## Christian Benteke                  5          39              13  33.3
## Christian Eriksen                  5          22               8  36.4
## Ciaran Clark                       1           7               1  14.3
## Clément Lenglet                    1           2               1  50.0
## Colin Dagba                        0           0               0   0.0
## Corentin Jean                      1           7               2  28.6
## Corentin Tolisso                   3          17               3  17.6
## Cristian Romero                    1           6               2  33.3
## Dan Ndoye                          1           3               0   0.0
## Daniel James                       8          50              15  30.0
## Daniel Maldini                     1           3               1  33.3
## Daniel Wass                        0           0               0   0.0
## Danny da Costa                     0           2               0   0.0
## David Nemeth                       0           0               0   0.0
## Dele Alli                          1           3               2  66.7
## Denis Cheryshev                    1          10               4  40.0
## Denis Vavro                        0           0               0   0.0
## Denis Zakaria                      2           9               4  44.4
## Diego Lainez                       0           1               1 100.0
## Divock Origi                       3          10               5  50.0
## Dominique Heintz                   0           0               0   0.0
## Dries Mertens                     12          44              19  43.2
## Dwight Gayle                       0           2               0   0.0
## Dylan Chambost                     1          10               1  10.0
## Ebrima Colley                      0          12               5  41.7
## Eddie Salcedo                      0          16               4  25.0
## Edinson Cavani                     3          18               6  33.3
## El Bilal Touré                     6          32              10  31.3
## Enzo Ebosse                        0           3               0   0.0
## Eric Bailly                        0           0               0   0.0
## Erick Cabaco                       1           1               0   0.0
## Erik Durm                          1           1               0   0.0
## Erik Thommy                        0          10               4  40.0
## Evann Guessand                     3          12               5  41.7
## Fabio Depaoli                      0           0               0   0.0
## Federico Bernardeschi              4          33               7  21.2
## Federico Bonazzoli                10          55              20  36.4
## Federico Chiesa                    4          28               9  32.1
## Federico Fernández                 0           6               1  16.7
## Filippo Melegoni                   1          13               7  53.8
## Flavius Daniliuc                   0           4               0   0.0
## Florent Mollet                     8          55              17  30.9
## Francesco Caputo                   1           4               0   0.0
## Francesco Di Mariano               0           0               0   0.0
## Gaëtan Laborde                     3          14               4  28.6
## Gareth Bale                        1          15               6  40.0
## Georginio Wijnaldum                4          22               2   9.1
## Ghislain Konan                     4          19               5  26.3
## Giacomo Raspadori                 14          71              25  35.2
## Gianluca Frabotta                  0           0               0   0.0
## Gideon Mensah                      2           2               0   0.0
## Giorgio Chiellini                  0           4               0   0.0
## Giovani Lo Celso                   0           9               0   0.0
## Giulian Biancone                   3          28               9  32.1
## Giulio Maggiore                    5          42               8  19.0
## Gonzalo Escalante                  0           0               0   0.0
## Grischa Prömel                     9          37              12  32.4
## Hamza Choudhury                    0           1               0   0.0
## Héctor Herrera                     0           8               3  37.5
## Henrikh Mkhitaryan                11          55              16  29.1
## Ignatius Ganago                    6          31              10  32.3
## Ilan Kebbal                        5          27               6  22.2
## Issa Diop                          1           2               2 100.0
## Jack Stephens                      0           0               0   0.0
## Jakub Jankto                       2           6               0   0.0
## Jamie Shackleton                   0           4               1  25.0
## Janik Haberer                      3          14               3  21.4
## Janis Antiste                      1          11               2  18.2
## Jannes Horn                        0           3               1  33.3
## Jarrad Branthwaite                 1           3               2  66.7
## Jason Berthomier                   6          23               4  17.4
## Javairô Dilrosun                   8          40              10  25.0
## Jean-Eudes Aholou                  2          17               2  11.8
## Jean-Kévin Augustin                0           2               0   0.0
## Jean-Philippe Gbamin               0           0               0   0.0
## Jens Petter Hauge                  4          19              10  52.6
## Jens Stryger Larsen                1           9               4  44.4
## Jesse Lingard                      2           8               6  75.0
## Jessic Ngankam                     3           9               5  55.6
## Jimmy Cabot                        5          11               3  27.3
## Joaquín Correa                     7          35              16  45.7
## Jonas Martin                       2          21               4  19.0
## Jonathan Silva                     0           1               1 100.0
## Jonjoe Kenny                       1           5               0   0.0
## Jordan Amavi                       0           0               0   0.0
## Jordan Torunarigha                 1           2               0   0.0
## Julian Draxler                     3           9               3  33.3
## Junior Dina Ebimbe                 0           4               1  25.0
## Junior Sambia                      1          20               6  30.0
## Jurgen Ekkelenkamp                 3          14               4  28.6
## Justin Kluivert                    9          30              10  33.3
## Kalvin Phillips                    1          15               3  20.0
## Kasper Dolberg                     8          38              13  34.2
## Keinan Davis                       0           0               0   0.0
## Kevin Mbabu                        0           5               0   0.0
## Kevin Stöger                       2          16               6  37.5
## Kevin Strootman                    0           1               0   0.0
## Ki-Jana Hoever                     0           1               0   0.0
## Kingsley Ehizibue                  1           6               0   0.0
## Kortney Hause                      1           2               2 100.0
## Kristoffer Askildsen               0           7               0   0.0
## Landry Dimata                      0           8               0   0.0
## László Bénes                       0           7               5  71.4
## Leander Dendoncker                 4          23              10  43.5
## Leonardo Mancuso                   1          12               5  41.7
## Louis Schaub                       3          14               2  14.3
## Luca Pellegrini                    1           6               1  16.7
## Lucas Alario                       6          22              13  59.1
## Lucas Ocampos                     10          51              16  31.4
## Lucas Perrin                       2           9               3  33.3
## Lucas Torreira                     6          29              13  44.8
## Ludwig Augustinsson                1           4               2  50.0
## Luis Binks                         0           2               0   0.0
## Lukas Klünter                      0           0               0   0.0
## Luuk de Jong                       6          24              10  41.7
## Malang Sarr                        0           1               0   0.0
## Mamadou Coulibaly                  2          10               4  40.0
## Manuel Akanji                      1          13               3  23.1
## Marco John                         0           0               0   0.0
## Marcus Forss                       1           5               0   0.0
## Marcus Ingvartsen                  6          24              11  45.8
## Marko Pjaca                        3          21              11  52.4
## Martin Braithwaite                 3           7               3  42.9
## Mateo Klimowicz                    1          11               3  27.3
## Mathías Olivera                    4          24               7  29.2
## Mathias Pereira Lage               6          19               6  31.6
## Matija Nastasić                    0           1               1 100.0
## Matt Miazga                        0           7               3  42.9
## Matt Targett                       2           7               2  28.6
## Mattéo Guendouzi                  10          50              18  36.0
## Matteo Lovato                      0           6               1  16.7
## Matteo Pessina                     2          28               9  32.1
## Matthew Hoppe                      1           2               0   0.0
## Matthias Ginter                    1          31              14  45.2
## Matthijs de Ligt                   4          20              10  50.0
## Mattia Zaccagni                    2           3               2  66.7
## Mattias Svanberg                   6          39              13  33.3
## Maxim Leitsch                      1           5               1  20.0
## Mehdi Bourabia                     1           1               1 100.0
## Merih Demiral                      3          25               6  24.0
## Mihailo Ristić                     4          18               5  27.8
## Mikkel Damsgaard                   0          11               2  18.2
## Morgan Gibbs-White                 0           2               0   0.0
## Moritz Jenz                        1           9               1  11.1
## Morten Thorsby                     4          26               7  26.9
## Moussa Doumbia                     0           3               2  66.7
## Munir El Haddadi                   2          12               7  58.3
## Nathan Tella                       1          12               5  41.7
## Neal Maupay                       10          59              14  23.7
## Neco Williams                      1           0               0   0.0
## Nehuén Pérez                       1          10               1  10.0
## Nemanja Radonjić                   0           0               0   0.0
## Nicolas Pépé                       3          23               7  30.4
## Nicolò Casale                      1          10               0   0.0
## Nicolò Rovella                     3          17               4  23.5
## Niklas Süle                        3          16               6  37.5
## Oleksandr Zinchenko                4          12               4  33.3
## Omar Richards                      0           4               0   0.0
## Orbelín Pineda                     0           5               1  20.0
## Orel Mangala                       4          17               8  47.1
## Oussama Idrissi                    2          21               8  38.1
## Ozan Kabak                         0           4               2  50.0
## Patrick Berg                       0           1               0   0.0
## Patrick Cutrone                    3          50              14  28.0
## Paul Nebel                         1           3               1  33.3
## Philipp Förster                    3          29               7  24.1
## Pierre-Yves Hamel                  0          13               4  30.8
## Pietro Pellegri                    0           3               1  33.3
## Ragnar Ache                        1          10               2  20.0
## Raheem Sterling                   18          54              25  46.3
## Randal Kolo Muani                 16          54              26  48.1
## Remo Freuler                       4          20               6  30.0
## Renato Steffen                     1          13               2  15.4
## Rey Manaj                          5          51              14  27.5
## Ricardo Pepi                       0           8               4  50.0
## Riccardo Calafiori                 0           1               0   0.0
## Roberto Piccoli                    0           2               0   0.0
## Rolando Mandragora                 2          23               5  21.7
## Romelu Lukaku                      8          42              14  33.3
## Ronaël Pierre-Gabriel              1           5               2  40.0
## Salih Özcan                        2          27               6  22.2
## Sam Lammers                        0           0               0   0.0
## Samuel Umtiti                      0           0               0   0.0
## Samuele Ricci                      0           4               1  25.0
## Santiago Ascacíbar                 1          13               1   7.7
## Sargis Adamyan                     1           3               2  66.7
## Sebastian Polter                  11          54              21  38.9
## Sebastiano Luperto                 1           9               3  33.3
## Sergiño Dest                       3          12               3  25.0
## Shane Duffy                        1          22               5  22.7
## Simon Banza                        2           6               4  66.7
## Sofian Kiyine                      1          20               5  25.0
## Sofiane Diop                      10          37              13  35.1
## Stefano Sensi                      0           3               0   0.0
## Steffen Tigges                     3           4               3  75.0
## Stéphane Bahoken                   4          24              14  58.3
## Steven Bergwijn                    4          19              10  52.6
## Sven Botman                        4          15               5  33.3
## Taiwo Awoniyi                     16          65              30  46.2
## Takefusa Kubo                      1          48               8  16.7
## Takumi Minamino                    3           7               4  57.1
## Tanguy Nianzou                     2           6               1  16.7
## Tariqe Fosu                        0           1               1 100.0
## Thilo Kehrer                       2           9               4  44.4
## Thomas Monconduit                  5          36              12  33.3
## Timo Werner                        5          45              16  35.6
## Tino Kadewere                      1          18               7  38.9
## Uroš Račić                         0          16               1   6.3
## Vedat Muriqi                       0           4               2  50.0
## Wesley Fofana                      0           3               1  33.3
## Willy Boly                         0           2               1  50.0
## Xaver Schlager                     1          10               1  10.0
## Xavi Simons                        0           2               0   0.0
## Yan Valery                         0           2               0   0.0
## Yangel Herrera                     2          15               2  13.3
## Youssouf Koné                      1           9               3  33.3
## Yves Bissouma                      3          21               5  23.8
## Zinho Vanheusden                   0           6               2  33.3
##                        Yellow Cards Red Cards Fouls Committed Fouls Drawn
## Aaron Connolly                    0         0               0           6
## Aaron Hickey                      7         0              39          71
## Aaron Ramsey                      0         0               2           1
## Abdou Diallo                      1         0              12           3
## Adam Ounas                        1         0               9          10
## Ademola Lookman                   2         0              27          33
## Adnan Januzaj                     5         0              28          51
## Ainsley Maitland-Niles            0         0               4           3
## Akim Zedadka                      5         0              28          89
## Alessandro Florenzi               2         0              10           8
## Alessio Romagnoli                 4         1              17          13
## Alexander Isak                    6         0              32          30
## Alexandre Lacazette               0         0              32          29
## Alexis Claude-Maurice             1         0               4           5
## Alexis Sánchez                    0         0              10          21
## Amadou Diawara                    0         0               5           0
## Amadou Onana                      2         1              31          25
## Amine Harit                       1         0              17          39
## Anderson Lucoqui                  2         0              17          16
## Andi Zeqiri                       2         0              20          15
## Andrea Belotti                    1         0              26          51
## Andrea La Mantia                  0         0               6           6
## Andrea Petagna                    1         0              15          14
## Andrea Pinamonti                  4         0              32          56
## Andreas Christensen               2         0              11           7
## Andreas Voglsammer                1         0              16          16
## Andriy Yarmolenko                 1         0               1           4
## Ángel Di María                    2         0               9          14
## Angelo Fulgini                    6         0              36          84
## Anthony Caci                      4         0              35          23
## Antonio Rüdiger                   9         0              35          12
## Anwar El Ghazi                    0         0               0           0
## Arkadiusz Milik                   1         0              19           8
## Armando Izzo                      4         0              17          12
## Arne Maier                        0         0              11          14
## Arthur Theate                     6         0              26          10
## Aster Vranckx                     2         0              40          16
## Axel Witsel                       4         0              26          30
## Aymen Barkok                      0         0               4           1
## Ben Davies                        6         0              31          18
## Bertrand Traoré                   0         0               0           4
## Billy Gilmour                     2         0              28          21
## Boubacar Kamara                   5         0              40          42
## Boulaye Dia                       3         0              23          13
## Brandon Soppy                     4         0              23          33
## Breel Embolo                      3         0              41          71
## Brian Brobbey                     0         0               3           2
## Bryan Reynolds                    0         0               0           0
## Callum Hudson-Odoi                1         0              10          17
## Calvin Stengs                     1         0              15           7
## Cédric Hountondji                 2         0              12           7
## Cengiz Ünder                      2         1              11          33
## Cenk Tosun                        0         0               0           0
## Cheick Doucouré                   7         1              42          24
## Cheikhou Kouyaté                  3         0              30          21
## Chris Richards                    0         0               0           0
## Christian Benteke                 3         0              16          16
## Christian Eriksen                 1         0               5           3
## Ciaran Clark                      2         1               6           5
## Clément Lenglet                   4         0              19           4
## Colin Dagba                       1         0               6           2
## Corentin Jean                     2         0               7           5
## Corentin Tolisso                  0         0              13           6
## Cristian Romero                   8         0              28          15
## Dan Ndoye                         0         0               3           2
## Daniel James                      7         1              30          38
## Daniel Maldini                    1         0               4           1
## Daniel Wass                       1         0               1           1
## Danny da Costa                    2         0               8           2
## David Nemeth                      0         0               1           1
## Dele Alli                         1         0              12          11
## Denis Cheryshev                   1         0              15           7
## Denis Vavro                       0         0               0           0
## Denis Zakaria                     1         0              11           4
## Diego Lainez                      0         0               4           1
## Divock Origi                      0         0               2           1
## Dominique Heintz                  0         0               0           0
## Dries Mertens                     2         0              26          22
## Dwight Gayle                      1         0               2           2
## Dylan Chambost                    0         0               7           5
## Ebrima Colley                     1         0              10          12
## Eddie Salcedo                     1         0               8           6
## Edinson Cavani                    0         0               7           5
## El Bilal Touré                    2         0              20          14
## Enzo Ebosse                       1         0              14          21
## Eric Bailly                       1         0               2           0
## Erick Cabaco                      4         1               7           4
## Erik Durm                         1         0               2           7
## Erik Thommy                       0         0               2           6
## Evann Guessand                    3         0              14           9
## Fabio Depaoli                     1         0               8           1
## Federico Bernardeschi             5         0              16          27
## Federico Bonazzoli                6         0              31          66
## Federico Chiesa                   0         0               9          16
## Federico Fernández                0         0               1           2
## Filippo Melegoni                  1         0              15          20
## Flavius Daniliuc                  4         0              17           4
## Florent Mollet                    6         0              34          50
## Francesco Caputo                  1         0               0           3
## Francesco Di Mariano              0         0               1           1
## Gaëtan Laborde                    1         0               3           3
## Gareth Bale                       2         0               3           0
## Georginio Wijnaldum               2         0               6          15
## Ghislain Konan                    1         0              17          33
## Giacomo Raspadori                 9         1              29          33
## Gianluca Frabotta                 0         0               2           0
## Gideon Mensah                     1         1              16           4
## Giorgio Chiellini                 2         0              15          22
## Giovani Lo Celso                  0         0               2           6
## Giulian Biancone                 10         0              34          32
## Giulio Maggiore                   9         0              64          67
## Gonzalo Escalante                 0         0               0           0
## Grischa Prömel                    4         0              37          57
## Hamza Choudhury                   1         0               4           5
## Héctor Herrera                    4         0              19           8
## Henrikh Mkhitaryan                4         1              41          51
## Ignatius Ganago                   2         0               9          10
## Ilan Kebbal                       1         0              12          50
## Issa Diop                         0         0               5           5
## Jack Stephens                     3         0               8           2
## Jakub Jankto                      1         1               3           7
## Jamie Shackleton                  4         0              13           6
## Janik Haberer                     1         0              20          10
## Janis Antiste                     0         0              15           6
## Jannes Horn                       2         0               4           8
## Jarrad Branthwaite                0         1               3           4
## Jason Berthomier                  0         0              14          15
## Javairô Dilrosun                  3         0              13          30
## Jean-Eudes Aholou                 2         0              21          20
## Jean-Kévin Augustin               1         0               1           1
## Jean-Philippe Gbamin              0         0               0           1
## Jens Petter Hauge                 4         0              16          10
## Jens Stryger Larsen               0         0               3           3
## Jesse Lingard                     1         0               5           4
## Jessic Ngankam                    0         0              12           5
## Jimmy Cabot                       4         0              32          32
## Joaquín Correa                    2         0              15          16
## Jonas Martin                      8         0              32          14
## Jonathan Silva                    4         0              10           5
## Jonjoe Kenny                      3         1               9           6
## Jordan Amavi                      1         0               2           4
## Jordan Torunarigha                1         0               4           4
## Julian Draxler                    0         0               4           1
## Junior Dina Ebimbe                2         0              11           4
## Junior Sambia                     5         1              18          26
## Jurgen Ekkelenkamp                1         0              11           5
## Justin Kluivert                   6         1              39          29
## Kalvin Phillips                   4         0              33          19
## Kasper Dolberg                    0         0              30          29
## Keinan Davis                      0         0               0           1
## Kevin Mbabu                       4         0              24          21
## Kevin Stöger                      3         0               9          10
## Kevin Strootman                   4         0              10          10
## Ki-Jana Hoever                    1         0               5           4
## Kingsley Ehizibue                 2         0              15          16
## Kortney Hause                     0         0               6           2
## Kristoffer Askildsen              5         0              17          14
## Landry Dimata                     0         0              14          20
## László Bénes                      0         0               1           9
## Leander Dendoncker                4         0              13           6
## Leonardo Mancuso                  0         0               1           2
## Louis Schaub                      0         0               8           8
## Luca Pellegrini                   6         0              23          21
## Lucas Alario                      2         0               8           8
## Lucas Ocampos                     7         0              45          41
## Lucas Perrin                      5         0              46          31
## Lucas Torreira                   10         1              38          44
## Ludwig Augustinsson               3         0              13          10
## Luis Binks                        3         0               7           6
## Lukas Klünter                     1         0               5           2
## Luuk de Jong                      1         0               8          12
## Malang Sarr                       2         0               8           1
## Mamadou Coulibaly                 1         0              16           9
## Manuel Akanji                     3         0              19          24
## Marco John                        0         0               0           0
## Marcus Forss                      0         0               3           1
## Marcus Ingvartsen                 2         0              16          14
## Marko Pjaca                       0         0              11          14
## Martin Braithwaite                0         0               5           2
## Mateo Klimowicz                   2         0               9           9
## Mathías Olivera                  10         0              55          46
## Mathias Pereira Lage              0         0              24          14
## Matija Nastasić                   1         0               2           0
## Matt Miazga                       2         0               6           2
## Matt Targett                      2         0              15          11
## Mattéo Guendouzi                  6         0              28          67
## Matteo Lovato                     4         0              16          14
## Matteo Pessina                    0         0              16          35
## Matthew Hoppe                     0         0               4           1
## Matthias Ginter                   2         0               9          29
## Matthijs de Ligt                  4         1              32          15
## Mattia Zaccagni                   0         0               1           7
## Mattias Svanberg                  7         0              37          63
## Maxim Leitsch                     2         0               9           0
## Mehdi Bourabia                    0         0               6           6
## Merih Demiral                     8         0              29          16
## Mihailo Ristić                    3         1              26          27
## Mikkel Damsgaard                  1         0              10           8
## Morgan Gibbs-White                1         0               2           0
## Moritz Jenz                       5         1              24           3
## Morten Thorsby                   10         0              81          37
## Moussa Doumbia                    0         0               0           2
## Munir El Haddadi                  3         0               7          13
## Nathan Tella                      2         0              17          22
## Neal Maupay                       7         0              33          31
## Neco Williams                     0         0               0           0
## Nehuén Pérez                      6         0              30           9
## Nemanja Radonjić                  0         0               0           0
## Nicolas Pépé                      0         0               8           3
## Nicolò Casale                    10         0              44          24
## Nicolò Rovella                    5         0              26          24
## Niklas Süle                       2         0              10           1
## Oleksandr Zinchenko               0         0              11           2
## Omar Richards                     1         0              15          22
## Orbelín Pineda                    0         0               2           4
## Orel Mangala                      4         0              26          27
## Oussama Idrissi                   2         0              10           3
## Ozan Kabak                        2         0              10          10
## Patrick Berg                      0         0               5           1
## Patrick Cutrone                   1         0              13          16
## Paul Nebel                        3         0               8           3
## Philipp Förster                   4         0              15           9
## Pierre-Yves Hamel                 2         0              13           9
## Pietro Pellegri                   0         0               7           2
## Ragnar Ache                       0         0               5           8
## Raheem Sterling                   1         0              19          37
## Randal Kolo Muani                 2         0              59          72
## Remo Freuler                     10         0              43          28
## Renato Steffen                    6         0              19          15
## Rey Manaj                         9         0              54          62
## Ricardo Pepi                      0         0               1           4
## Riccardo Calafiori                1         0               3           0
## Roberto Piccoli                   0         0               3           0
## Rolando Mandragora                7         1              25          22
## Romelu Lukaku                     1         0               8          12
## Ronaël Pierre-Gabriel             6         0              27          29
## Salih Özcan                       5         0              38          53
## Sam Lammers                       0         0               2           3
## Samuel Umtiti                     1         0               0           1
## Samuele Ricci                     1         0              14          13
## Santiago Ascacíbar                5         0              35          31
## Sargis Adamyan                    0         0               5           8
## Sebastian Polter                  2         0              75          36
## Sebastiano Luperto                8         1              30           5
## Sergiño Dest                      2         0              17          19
## Shane Duffy                       7         0              15           5
## Simon Banza                       0         0               3           7
## Sofian Kiyine                     8         1              49          11
## Sofiane Diop                      6         0              28          41
## Stefano Sensi                     0         0               4           5
## Steffen Tigges                    0         0               1           1
## Stéphane Bahoken                  0         0              23          14
## Steven Bergwijn                   1         0               9           9
## Sven Botman                       4         0              16          13
## Taiwo Awoniyi                     1         0              25          23
## Takefusa Kubo                     4         0              17          47
## Takumi Minamino                   1         0               4           4
## Tanguy Nianzou                    4         0              13           8
## Tariqe Fosu                       0         0               0           1
## Thilo Kehrer                      0         0              15           6
## Thomas Monconduit                 5         0              46          33
## Timo Werner                       1         0              11          12
## Tino Kadewere                     0         1               6           5
## Uroš Račić                        3         1              29          25
## Vedat Muriqi                      1         0               5           9
## Wesley Fofana                     2         0               7           7
## Willy Boly                        1         0               7           2
## Xaver Schlager                    2         0              23          10
## Xavi Simons                       1         0               2           4
## Yan Valery                        0         0               3           3
## Yangel Herrera                    7         1              44          34
## Youssouf Koné                     2         1               7          20
## Yves Bissouma                    10         0              30          19
## Zinho Vanheusden                  3         0              10          10
##                        Offsides Ball Recovery Tackles Made Tackles Won Blocks
## Aaron Connolly                0             7            0           0      2
## Aaron Hickey                  4           134           46          29     36
## Aaron Ramsey                  0             4            0           0      0
## Abdou Diallo                  0            45           22          14      6
## Adam Ounas                    0            23            6           1      3
## Ademola Lookman               6           102           32          14     19
## Adnan Januzaj                 4            93           17          11     15
## Ainsley Maitland-Niles        0            23            8           4      2
## Akim Zedadka                  5           232           72          44     32
## Alessandro Florenzi           3            79           28          14      9
## Alessio Romagnoli             3            79           24          14     21
## Alexander Isak               15            56            7           4     14
## Alexandre Lacazette           6            52           27          10     12
## Alexis Claude-Maurice         0            11            2           0      7
## Alexis Sánchez                8            36            9           8      4
## Amadou Diawara                0            12            7           2      2
## Amadou Onana                  2           100           48          31     10
## Amine Harit                   3            51           18          11     10
## Anderson Lucoqui              0            41           22          10     15
## Andi Zeqiri                  12            38           14           7     10
## Andrea Belotti               16            41            7           3     12
## Andrea La Mantia              3            11            2           0      3
## Andrea Petagna                2            15            3           3      1
## Andrea Pinamonti             19            62           11           6     24
## Andreas Christensen           0           110           14           8     14
## Andreas Voglsammer           14            27           14           9      9
## Andriy Yarmolenko             3             5            4           2      4
## Ángel Di María                7            71           29          18     17
## Angelo Fulgini                5           137           35          25     21
## Anthony Caci                  2           161           65          34     22
## Antonio Rüdiger               0           183           48          28     20
## Anwar El Ghazi                0             2            1           1      3
## Arkadiusz Milik               8            22            9           2      9
## Armando Izzo                  0            32           16           9      5
## Arne Maier                    1           127           20          13     18
## Arthur Theate                 1           181           33          19     29
## Aster Vranckx                 1            74           34          13     19
## Axel Witsel                   1           152           42          27     24
## Aymen Barkok                  0            11            6           2      4
## Ben Davies                    5           106           36          21     32
## Bertrand Traoré               2            11            3           3      0
## Billy Gilmour                 1           124           41          21     21
## Boubacar Kamara               0           183           68          38     29
## Boulaye Dia                   7            36            8           6      6
## Brandon Soppy                 0            47           20           9     10
## Breel Embolo                 20            57           16          11     14
## Brian Brobbey                 0             7            1           0      1
## Bryan Reynolds                0             0            0           0      0
## Callum Hudson-Odoi            4            39           14          10      8
## Calvin Stengs                 2            57           16           7     10
## Cédric Hountondji             0           101           33          17     23
## Cengiz Ünder                 12            72           18          13     13
## Cenk Tosun                    0             1            0           0      0
## Cheick Doucouré               0           218           75          43     30
## Cheikhou Kouyaté              0           136           52          38     40
## Chris Richards                0             1            0           0      1
## Christian Benteke            10            25            3           2      7
## Christian Eriksen             0            69           13           6      9
## Ciaran Clark                  1            49           13          10     14
## Clément Lenglet               0            60           20          11      7
## Colin Dagba                   0             7            4           2      4
## Corentin Jean                 2            13            9           6      5
## Corentin Tolisso              3            59           11           5      5
## Cristian Romero               2           148           62          39     46
## Dan Ndoye                     1             9            0           0      4
## Daniel James                 13            82           24          14     18
## Daniel Maldini                1             5            8           3      4
## Daniel Wass                   1             4            0           0      0
## Danny da Costa                0            48           15           4     14
## David Nemeth                  0            16            3           2      5
## Dele Alli                     2            40           14           8     22
## Denis Cheryshev               4            35           14           5      9
## Denis Vavro                   0             0            0           0      0
## Denis Zakaria                 0            27           11           7     16
## Diego Lainez                  0            10            1           1      3
## Divock Origi                  0             7            0           0      1
## Dominique Heintz              0             1            0           0      0
## Dries Mertens                 5            51           16           7      8
## Dwight Gayle                  0             2            1           0      0
## Dylan Chambost                1            47           16          11      9
## Ebrima Colley                 2            15            6           1      8
## Eddie Salcedo                 2            14            1           1      2
## Edinson Cavani                6            22            4           1      6
## El Bilal Touré                6            37           13           6     10
## Enzo Ebosse                   0           112           25          14     19
## Eric Bailly                   0            10            5           4      4
## Erick Cabaco                  0            14            2           1      5
## Erik Durm                     0            25           13           7      8
## Erik Thommy                   2            12            2           0      3
## Evann Guessand                0            19            9           8      4
## Fabio Depaoli                 0             4            3           1      2
## Federico Bernardeschi         4            82           16          10     12
## Federico Bonazzoli            6            76           24          12     17
## Federico Chiesa               1            47           26          19      7
## Federico Fernández            0            24            3           0     10
## Filippo Melegoni              2            72           20          12     10
## Flavius Daniliuc              1            98           46          31     13
## Florent Mollet               10           132           31          22     27
## Francesco Caputo              0             2            0           0      0
## Francesco Di Mariano          0             2            1           1      2
## Gaëtan Laborde                0            15            2           2      3
## Gareth Bale                   1             4            2           1      2
## Georginio Wijnaldum           0            74           15           9     13
## Ghislain Konan                2           176           35          19     35
## Giacomo Raspadori             4           126           29          18     29
## Gianluca Frabotta             0             2            0           0      1
## Gideon Mensah                 2           105           37          24     11
## Giorgio Chiellini             2            78           22          13     24
## Giovani Lo Celso              1            11            8           4      6
## Giulian Biancone              4           147           70          42     23
## Giulio Maggiore               1           206           58          32     30
## Gonzalo Escalante             0             1            1           0      2
## Grischa Prömel                2           187           36          22     18
## Hamza Choudhury               0            15            9           3      0
## Héctor Herrera                1            73           16           8     13
## Henrikh Mkhitaryan            5           195           45          29     33
## Ignatius Ganago              10            27            3           2      4
## Ilan Kebbal                   7           122           31          15     16
## Issa Diop                     1            55           10           8     14
## Jack Stephens                 0            68           19          12     12
## Jakub Jankto                  0            38           11           7      4
## Jamie Shackleton              0            50           27          16     11
## Janik Haberer                 1            51           14           8     11
## Janis Antiste                 5            36           18          11      7
## Jannes Horn                   0            33            6           4      1
## Jarrad Branthwaite            0            24            4           1      3
## Jason Berthomier              2            98           29          14     14
## Javairô Dilrosun              1            68           13           4     14
## Jean-Eudes Aholou             0            77           16           9      8
## Jean-Kévin Augustin           0             3            1           0      1
## Jean-Philippe Gbamin          0             4            1           1      0
## Jens Petter Hauge             1            56           14           5      7
## Jens Stryger Larsen           1            22            5           2      2
## Jesse Lingard                 1            19            9           7      4
## Jessic Ngankam                1            10            3           1      3
## Jimmy Cabot                   1           180           81          44     29
## Joaquín Correa                1            32           11           5      8
## Jonas Martin                  0           147           35          20     17
## Jonathan Silva                0            17           10           8      4
## Jonjoe Kenny                  1            58           17           7      8
## Jordan Amavi                  0            29           16          10      3
## Jordan Torunarigha            1            27            4           1      4
## Julian Draxler                1            26            3           2      3
## Junior Dina Ebimbe            0            12            9           4      5
## Junior Sambia                 0            86           22          13     22
## Jurgen Ekkelenkamp            0            48            8           3      9
## Justin Kluivert               5            95           19           9     13
## Kalvin Phillips               0           180           54          25     37
## Kasper Dolberg                3            38            9           4     15
## Keinan Davis                  0             0            0           0      0
## Kevin Mbabu                   2           109           31          20     25
## Kevin Stöger                  0            55           10           5      6
## Kevin Strootman               0            42            8           4     11
## Ki-Jana Hoever                1            13           10           6      6
## Kingsley Ehizibue             0            45           32          18     13
## Kortney Hause                 1            41            3           3      8
## Kristoffer Askildsen          0            43           18           6     14
## Landry Dimata                 3            14            2           2      0
## László Bénes                  0            17            5           3      5
## Leander Dendoncker            0           107           42          17     37
## Leonardo Mancuso              5            11            1           1      2
## Louis Schaub                  3            56           16          10     17
## Luca Pellegrini               0            73           23          10      9
## Lucas Alario                  7            13            6           5      5
## Lucas Ocampos                 2           114           45          24     35
## Lucas Perrin                  2           169           43          32     28
## Lucas Torreira                2           153           37          29     18
## Ludwig Augustinsson           0            41           18          14     10
## Luis Binks                    0            35            8           4      6
## Lukas Klünter                 0            12            1           1      1
## Luuk de Jong                  6            23            9           7      4
## Malang Sarr                   0            36           14           7      6
## Mamadou Coulibaly             0            74           15          12      9
## Manuel Akanji                 1           164           33          23     26
## Marco John                    0             2            0           0      1
## Marcus Forss                  0             4            3           3      5
## Marcus Ingvartsen             4            27            3           2      6
## Marko Pjaca                   2            60            7           4     10
## Martin Braithwaite            2             7            1           1      2
## Mateo Klimowicz               1            21           13           6      2
## Mathías Olivera               3           195           79          47     45
## Mathias Pereira Lage          1            63           21           7     18
## Matija Nastasić               0            13            1           1      1
## Matt Miazga                   0            35            7           7     14
## Matt Targett                  0            73           34          18     16
## Mattéo Guendouzi              0           178           45          30     15
## Matteo Lovato                 0           106           23          15     21
## Matteo Pessina                4           134           27          14     22
## Matthew Hoppe                 2             6            1           0      0
## Matthias Ginter               2           136           36          22     21
## Matthijs de Ligt              2           129           34          19     31
## Mattia Zaccagni               0             8            3           1      1
## Mattias Svanberg              0           132           39          15     31
## Maxim Leitsch                 1           148           18          10     31
## Mehdi Bourabia                0            20            5           1      7
## Merih Demiral                 3           122           31          17     26
## Mihailo Ristić                0           115           44          32     16
## Mikkel Damsgaard              0            41           27          13     16
## Morgan Gibbs-White            0             2            0           0      1
## Moritz Jenz                   0            66           26          12     19
## Morten Thorsby                0           151           46          26     37
## Moussa Doumbia                1             2            0           0      0
## Munir El Haddadi              3            23            8           4      7
## Nathan Tella                  3            54           13           9     13
## Neal Maupay                   6            76           17          10     21
## Neco Williams                 0             0            0           0      0
## Nehuén Pérez                  0           104           44          21     27
## Nemanja Radonjić              1             1            0           0      0
## Nicolas Pépé                  4            43            8           5      9
## Nicolò Casale                 0           197           46          30     31
## Nicolò Rovella                3           118           37          26     21
## Niklas Süle                   1           158           29          23     16
## Oleksandr Zinchenko           1            65           20          14     11
## Omar Richards                 0            28           19          12      7
## Orbelín Pineda                0            12            4           1      0
## Orel Mangala                  0           110           17          10     11
## Oussama Idrissi               2            48            8           4      8
## Ozan Kabak                    1            50           10           5     19
## Patrick Berg                  0            28           14           6      2
## Patrick Cutrone              20            40            5           2     11
## Paul Nebel                    1            18            5           3      0
## Philipp Förster               2            58           20          13     12
## Pierre-Yves Hamel             2             9           11           6      2
## Pietro Pellegri               2             4            2           2      0
## Ragnar Ache                   6             7            2           0      3
## Raheem Sterling              10            86           23          10     12
## Randal Kolo Muani            18           107           28          14     14
## Remo Freuler                  0           186           69          39     48
## Renato Steffen                0            57           20          16      7
## Rey Manaj                    11            57           19          12     16
## Ricardo Pepi                  3            11            4           2      7
## Riccardo Calafiori            0             2            1           0      2
## Roberto Piccoli               1             8            1           1      0
## Rolando Mandragora            1            87           21          10     18
## Romelu Lukaku                14            33            2           1     11
## Ronaël Pierre-Gabriel         3           129           79          45     22
## Salih Özcan                   1           225           42          30     37
## Sam Lammers                   0             0            1           0      0
## Samuel Umtiti                 1             6            2           2      1
## Samuele Ricci                 2            40            8           4      7
## Santiago Ascacíbar            2           192           93          54     31
## Sargis Adamyan                4            24            8           6      4
## Sebastian Polter              7            60            9           3     15
## Sebastiano Luperto            1            92           29          17     32
## Sergiño Dest                  1            93           32          15     22
## Shane Duffy                   1            68           15          12     24
## Simon Banza                   0             5            2           1      1
## Sofian Kiyine                 0            94           24          14     15
## Sofiane Diop                  4           134           38          20     10
## Stefano Sensi                 1            14            4           3      2
## Steffen Tigges                1             2            4           3      3
## Stéphane Bahoken              4            22           13           8     15
## Steven Bergwijn               1            22            3           2      3
## Sven Botman                   1           113           24          11     32
## Taiwo Awoniyi                21            82           11           7     11
## Takefusa Kubo                 5           101           31          19     24
## Takumi Minamino               0            13            3           2      2
## Tanguy Nianzou                0            32           11           7      7
## Tariqe Fosu                   0             1            0           0      1
## Thilo Kehrer                  0            70           21          14     19
## Thomas Monconduit             0           160           41          25     21
## Timo Werner                  11            37            8           4      9
## Tino Kadewere                 4            29            7           5      7
## Uroš Račić                    1            97           32          18     16
## Vedat Muriqi                  1             6            1           0      0
## Wesley Fofana                 0            43           12           8      5
## Willy Boly                    1            62           25          19     14
## Xaver Schlager                1           102           43          20     25
## Xavi Simons                   0             8            1           1      3
## Yan Valery                    0            27            8           6      6
## Yangel Herrera                1           128           41          31     27
## Youssouf Koné                 2            62           30          18      8
## Yves Bissouma                 0           161           76          49     33
## Zinho Vanheusden              0            47           18           6     19
##                        Shots Blocked Passes Blocked Clearnaces Interceptions
## Aaron Connolly                     0              2          0             0
## Aaron Hickey                      13             23         29            44
## Aaron Ramsey                       0              0          1             0
## Abdou Diallo                       2              4         10            19
## Adam Ounas                         0              3          2             3
## Ademola Lookman                    1             18         10             4
## Adnan Januzaj                      3             12         12            15
## Ainsley Maitland-Niles             0              2          4             4
## Akim Zedadka                       8             24         58            48
## Alessandro Florenzi                2              7         27            10
## Alessio Romagnoli                 12              9         19            45
## Alexander Isak                     2             12          3            10
## Alexandre Lacazette                1             11          5             6
## Alexis Claude-Maurice              0              7          3             1
## Alexis Sánchez                     0              4          2             3
## Amadou Diawara                     0              2          0             1
## Amadou Onana                       3              7         30            19
## Amine Harit                        0             10          4             2
## Anderson Lucoqui                   3             12         11            10
## Andi Zeqiri                        1              9          2            12
## Andrea Belotti                     4              8          2             9
## Andrea La Mantia                   1              2          1             6
## Andrea Petagna                     0              1          0             5
## Andrea Pinamonti                   3             21          7            23
## Andreas Christensen                6              8         29            57
## Andreas Voglsammer                 2              7          2             7
## Andriy Yarmolenko                  2              2          0             2
## Ángel Di María                     0             17          6             3
## Angelo Fulgini                     5             16         22             5
## Anthony Caci                       3             19         33            49
## Antonio Rüdiger                   11              9         28            91
## Anwar El Ghazi                     0              3          0             0
## Arkadiusz Milik                    1              8          3            10
## Armando Izzo                       4              1         17            21
## Arne Maier                         1             17         15            13
## Arthur Theate                     16             13         42            69
## Aster Vranckx                      7             12         13            16
## Axel Witsel                        9             15         27            38
## Aymen Barkok                       1              3          5             6
## Ben Davies                        19             13         30            75
## Bertrand Traoré                    0              0          0             1
## Billy Gilmour                      4             17         17            29
## Boubacar Kamara                   10             19         46            44
## Boulaye Dia                        1              5          5             4
## Brandon Soppy                      2              8          5             9
## Breel Embolo                       2             12          6             7
## Brian Brobbey                      0              1          0             0
## Bryan Reynolds                     0              0          0             1
## Callum Hudson-Odoi                 0              8          2             6
## Calvin Stengs                      0             10          8             6
## Cédric Hountondji                 12             11         34            76
## Cengiz Ünder                       2             11         14             9
## Cenk Tosun                         0              0          0             0
## Cheick Doucouré                    7             23         68            24
## Cheikhou Kouyaté                  14             26         38            39
## Chris Richards                     0              1          0             0
## Christian Benteke                  3              4          2            33
## Christian Eriksen                  1              8          9            14
## Ciaran Clark                      11              3         13            43
## Clément Lenglet                    3              4         13            21
## Colin Dagba                        2              2          2             1
## Corentin Jean                      0              5          1             0
## Corentin Tolisso                   1              4          9            10
## Cristian Romero                   21             25         33            74
## Dan Ndoye                          0              4          0             0
## Daniel James                       1             17          8            12
## Daniel Maldini                     0              4          1             0
## Daniel Wass                        0              0          0             3
## Danny da Costa                     1             13         11            18
## David Nemeth                       1              4          6            11
## Dele Alli                          5             17          6            16
## Denis Cheryshev                    1              8          9             9
## Denis Vavro                        0              0          0             0
## Denis Zakaria                      3             13         11             7
## Diego Lainez                       0              3          1             0
## Divock Origi                       0              1          1             2
## Dominique Heintz                   0              0          1             0
## Dries Mertens                      2              6          9             2
## Dwight Gayle                       0              0          1             0
## Dylan Chambost                     2              7          8             4
## Ebrima Colley                      0              8          3             2
## Eddie Salcedo                      1              1          0             7
## Edinson Cavani                     0              6          2             8
## El Bilal Touré                     3              7          6            19
## Enzo Ebosse                        9             10         15            42
## Eric Bailly                        2              2          3            11
## Erick Cabaco                       2              3          4            10
## Erik Durm                          1              7         10             7
## Erik Thommy                        0              3          4             0
## Evann Guessand                     0              4          0             1
## Fabio Depaoli                      1              1          1             2
## Federico Bernardeschi              4              8          5            15
## Federico Bonazzoli                 1             16          6             7
## Federico Chiesa                    0              7          4             3
## Federico Fernández                10              0          4            28
## Filippo Melegoni                   1              9         11             8
## Flavius Daniliuc                   7              6         14            34
## Florent Mollet                     4             23         24            13
## Francesco Caputo                   0              0          0             0
## Francesco Di Mariano               1              1          0             0
## Gaëtan Laborde                     0              3          5             5
## Gareth Bale                        0              2          2             1
## Georginio Wijnaldum                3             10          9             7
## Ghislain Konan                     8             27         28            45
## Giacomo Raspadori                  3             26          9            11
## Gianluca Frabotta                  0              1          1             0
## Gideon Mensah                      2              9         16            21
## Giorgio Chiellini                 18              6         22            59
## Giovani Lo Celso                   1              5          1             0
## Giulian Biancone                   8             15         53            71
## Giulio Maggiore                    5             25         29            33
## Gonzalo Escalante                  1              1          1             0
## Grischa Prömel                     3             15         23            22
## Hamza Choudhury                    0              0         12            10
## Héctor Herrera                     2             11         12            16
## Henrikh Mkhitaryan                 2             31         22            33
## Ignatius Ganago                    0              4          2             3
## Ilan Kebbal                        0             16         13             3
## Issa Diop                         13              1          7            44
## Jack Stephens                      7              5         19            42
## Jakub Jankto                       0              4          4             7
## Jamie Shackleton                   7              4          8            12
## Janik Haberer                      4              7         16            11
## Janis Antiste                      1              6          4             6
## Jannes Horn                        1              0          3             9
## Jarrad Branthwaite                 2              1          1            20
## Jason Berthomier                   2             12          8             4
## Javairô Dilrosun                   1             13         11             5
## Jean-Eudes Aholou                  1              7         19             7
## Jean-Kévin Augustin                0              1          0             0
## Jean-Philippe Gbamin               0              0          1             1
## Jens Petter Hauge                  0              7          3             5
## Jens Stryger Larsen                0              2          4            17
## Jesse Lingard                      0              4          0             1
## Jessic Ngankam                     0              3          0             1
## Jimmy Cabot                        0             29         32            30
## Joaquín Correa                     1              7          3             2
## Jonas Martin                       3             14         22            28
## Jonathan Silva                     0              4          3             7
## Jonjoe Kenny                       1              7         20            31
## Jordan Amavi                       1              2         15             7
## Jordan Torunarigha                 2              2         17            30
## Julian Draxler                     0              3          5             3
## Junior Dina Ebimbe                 3              2          2             1
## Junior Sambia                      3             19         21            31
## Jurgen Ekkelenkamp                 2              7          4             5
## Justin Kluivert                    1             12         14             6
## Kalvin Phillips                    9             28         24            18
## Kasper Dolberg                     1             14          6            17
## Keinan Davis                       0              0          0             0
## Kevin Mbabu                        8             17         35            35
## Kevin Stöger                       0              6          5             1
## Kevin Strootman                    2              9         10            15
## Ki-Jana Hoever                     0              6          6             8
## Kingsley Ehizibue                  4              9          9            26
## Kortney Hause                      5              3          6            13
## Kristoffer Askildsen               7              7         15            15
## Landry Dimata                      0              0          1             7
## László Bénes                       2              3          2             5
## Leander Dendoncker                10             27          9            38
## Leonardo Mancuso                   0              2          1             2
## Louis Schaub                       3             14          3             3
## Luca Pellegrini                    1              8          5            23
## Lucas Alario                       0              5          1             6
## Lucas Ocampos                      1             34          9            24
## Lucas Perrin                      11             17         42            67
## Lucas Torreira                     1             17         26            18
## Ludwig Augustinsson                3              7          5            21
## Luis Binks                         5              1         17            33
## Lukas Klünter                      0              1          2             2
## Luuk de Jong                       0              4          0            12
## Malang Sarr                        3              3         11            20
## Mamadou Coulibaly                  0              9         11             9
## Manuel Akanji                     15             11         35            58
## Marco John                         0              1          1             1
## Marcus Forss                       0              5          1             0
## Marcus Ingvartsen                  0              6          6             7
## Marko Pjaca                        2              8          4             2
## Martin Braithwaite                 0              2          1             1
## Mateo Klimowicz                    0              2          4             3
## Mathías Olivera                    4             41         55            67
## Mathias Pereira Lage               7             11         21            22
## Matija Nastasić                    1              0          5            11
## Matt Miazga                       12              2         11            55
## Matt Targett                       9              7         23            35
## Mattéo Guendouzi                   1             14         16            18
## Matteo Lovato                     10             11         28            43
## Matteo Pessina                     3             19         13             7
## Matthew Hoppe                      0              0          0             0
## Matthias Ginter                   10             11         33            76
## Matthijs de Ligt                  21             10         26           103
## Mattia Zaccagni                    0              1          4             0
## Mattias Svanberg                   9             22         28            34
## Maxim Leitsch                     19             12         31            74
## Mehdi Bourabia                     6              1          3            10
## Merih Demiral                     14             12         58            68
## Mihailo Ristić                     6             10         17            48
## Mikkel Damsgaard                   1             15          7             7
## Morgan Gibbs-White                 0              1          0             0
## Moritz Jenz                       11              8         28            55
## Morten Thorsby                    12             25         25            39
## Moussa Doumbia                     0              0          0             0
## Munir El Haddadi                   1              6          5             2
## Nathan Tella                       0             13          6             9
## Neal Maupay                        2             19          2             2
## Neco Williams                      0              0          0             0
## Nehuén Pérez                       9             18         21            47
## Nemanja Radonjić                   0              0          0             0
## Nicolas Pépé                       1              8          8             2
## Nicolò Casale                     12             19         49            95
## Nicolò Rovella                     1             20         20            20
## Niklas Süle                       12              4         28            49
## Oleksandr Zinchenko                3              8         14            15
## Omar Richards                      1              6          4             7
## Orbelín Pineda                     0              0          0             1
## Orel Mangala                       4              7         12            19
## Oussama Idrissi                    0              8          3             5
## Ozan Kabak                        11              8         16            26
## Patrick Berg                       1              1          4             3
## Patrick Cutrone                    1             10          5             1
## Paul Nebel                         0              0          0             1
## Philipp Förster                    1             11          3             2
## Pierre-Yves Hamel                  1              1          0             0
## Pietro Pellegri                    0              0          0             1
## Ragnar Ache                        0              3          0             1
## Raheem Sterling                    0             12         12             3
## Randal Kolo Muani                  3             11          6            31
## Remo Freuler                       8             40         35            26
## Renato Steffen                     0              7         13            14
## Rey Manaj                          0             16          4            13
## Ricardo Pepi                       0              7          3             8
## Riccardo Calafiori                 1              1          2             4
## Roberto Piccoli                    0              0          1             1
## Rolando Mandragora                 4             14         10            13
## Romelu Lukaku                      1             10          0            20
## Ronaël Pierre-Gabriel              7             15         48            74
## Salih Özcan                        5             32         51            35
## Sam Lammers                        0              0          0             0
## Samuel Umtiti                      1              0          0             4
## Samuele Ricci                      1              6         14             9
## Santiago Ascacíbar                 6             25         41            42
## Sargis Adamyan                     0              4          1             2
## Sebastian Polter                   5             10          2            24
## Sebastiano Luperto                26              6         45           132
## Sergiño Dest                       4             18          2            18
## Shane Duffy                       18              6         16            76
## Simon Banza                        0              1          1             2
## Sofian Kiyine                      1             14         10             9
## Sofiane Diop                       0             10         23             4
## Stefano Sensi                      0              2          0             2
## Steffen Tigges                     1              2          0             3
## Stéphane Bahoken                   0             15          3             3
## Steven Bergwijn                    0              3          1             8
## Sven Botman                       21             11         26           116
## Taiwo Awoniyi                      2              9          2            29
## Takefusa Kubo                      3             21          8             4
## Takumi Minamino                    0              2          1             2
## Tanguy Nianzou                     2              5          7            17
## Tariqe Fosu                        0              1          0             0
## Thilo Kehrer                      11              8         24            29
## Thomas Monconduit                 10             11         22            51
## Timo Werner                        0              9          2             4
## Tino Kadewere                      0              7          4             1
## Uroš Račić                         6             10         16            21
## Vedat Muriqi                       0              0          0             2
## Wesley Fofana                      2              3         12            26
## Willy Boly                         6              8         14            35
## Xaver Schlager                     2             23          9            14
## Xavi Simons                        0              3          0             0
## Yan Valery                         3              3          6            19
## Yangel Herrera                     5             22         19            33
## Youssouf Koné                      3              5         10            25
## Yves Bissouma                      3             30         50            49
## Zinho Vanheusden                  14              5         23            42
##                        Defensive Errors Passes Made Passes Attempted Pass Rate
## Aaron Connolly                        0          14               19      73.7
## Aaron Hickey                          0        1032             1289      80.1
## Aaron Ramsey                          0          59               66      89.4
## Abdou Diallo                          1         623              704      88.5
## Adam Ounas                            0         158              204      77.5
## Ademola Lookman                       0         405              536      75.6
## Adnan Januzaj                         0         486              749      64.9
## Ainsley Maitland-Niles                0         106              138      76.8
## Akim Zedadka                          0        1688             2101      80.3
## Alessandro Florenzi                   1         646              812      79.6
## Alessio Romagnoli                     0         893              999      89.4
## Alexander Isak                        0         316              440      71.8
## Alexandre Lacazette                   0         343              455      75.4
## Alexis Claude-Maurice                 0          88              103      85.4
## Alexis Sánchez                        0         348              456      76.3
## Amadou Diawara                        0          56               71      78.9
## Amadou Onana                          0         516              623      82.8
## Amine Harit                           0         611              703      86.9
## Anderson Lucoqui                      0         147              246      59.8
## Andi Zeqiri                           0         114              173      65.9
## Andrea Belotti                        0         252              369      68.3
## Andrea La Mantia                      0          63              105      60.0
## Andrea Petagna                        0         114              162      70.4
## Andrea Pinamonti                      0         381              636      59.9
## Andreas Christensen                   1        1201             1336      89.9
## Andreas Voglsammer                    0         145              246      58.9
## Andriy Yarmolenko                     0          80              109      73.4
## Ángel Di María                        0         881             1182      74.5
## Angelo Fulgini                        1        1108             1376      80.5
## Anthony Caci                          1        1260             1627      77.4
## Antonio Rüdiger                       1        2217             2534      87.5
## Anwar El Ghazi                        0           7                9      77.8
## Arkadiusz Milik                       1         299              377      79.3
## Armando Izzo                          0         240              307      78.2
## Arne Maier                            0         609              849      71.7
## Arthur Theate                         0        1231             1488      82.7
## Aster Vranckx                         0         335              441      76.0
## Axel Witsel                           0        1217             1305      93.3
## Aymen Barkok                          0          52               70      74.3
## Ben Davies                            1        1391             1648      84.4
## Bertrand Traoré                       0          61               84      72.6
## Billy Gilmour                         0         991             1243      79.7
## Boubacar Kamara                       1        2193             2425      90.4
## Boulaye Dia                           0         189              253      74.7
## Brandon Soppy                         1         167              236      70.8
## Breel Embolo                          0         398              541      73.6
## Brian Brobbey                         0          28               32      87.5
## Bryan Reynolds                        0           2                2     100.0
## Callum Hudson-Odoi                    1         317              399      79.4
## Calvin Stengs                         0         295              411      71.8
## Cédric Hountondji                     0         836              988      84.6
## Cengiz Ünder                          0         690              946      72.9
## Cenk Tosun                            0           1                1     100.0
## Cheick Doucouré                       0        1656             1904      87.0
## Cheikhou Kouyaté                      0         656              804      81.6
## Chris Richards                        0           3                3     100.0
## Christian Benteke                     0         196              305      64.3
## Christian Eriksen                     0         478              639      74.8
## Ciaran Clark                          1         290              370      78.4
## Clément Lenglet                       0         743              820      90.6
## Colin Dagba                           0         125              139      89.9
## Corentin Jean                         0          85              116      73.3
## Corentin Tolisso                      0         477              550      86.7
## Cristian Romero                       1        1149             1325      86.7
## Dan Ndoye                             0          37               52      71.2
## Daniel James                          0         326              508      64.2
## Daniel Maldini                        0          33               48      68.8
## Daniel Wass                           0          20               28      71.4
## Danny da Costa                        1         213              319      66.8
## David Nemeth                          1         146              174      83.9
## Dele Alli                             0         177              228      77.6
## Denis Cheryshev                       0         104              171      60.8
## Denis Vavro                           0           6                6     100.0
## Denis Zakaria                         0         129              168      76.8
## Diego Lainez                          0          43               56      76.8
## Divock Origi                          0          28               37      75.7
## Dominique Heintz                      0          20               21      95.2
## Dries Mertens                         0         400              548      73.0
## Dwight Gayle                          0          13               16      81.3
## Dylan Chambost                        0         379              492      77.0
## Ebrima Colley                         0          80              127      63.0
## Eddie Salcedo                         1         100              148      67.6
## Edinson Cavani                        0         176              233      75.5
## El Bilal Touré                        0         242              330      73.3
## Enzo Ebosse                           0         911             1086      83.9
## Eric Bailly                           0          68               85      80.0
## Erick Cabaco                          0         115              173      66.5
## Erik Durm                             0         183              278      65.8
## Erik Thommy                           0          87              117      74.4
## Evann Guessand                        0          51               76      67.1
## Fabio Depaoli                         0          22               36      61.1
## Federico Bernardeschi                 0         452              592      76.4
## Federico Bonazzoli                    0         407              547      74.4
## Federico Chiesa                       0         218              288      75.7
## Federico Fernández                    0         182              211      86.3
## Filippo Melegoni                      1         234              347      67.4
## Flavius Daniliuc                      1         771              877      87.9
## Florent Mollet                        0         951             1179      80.7
## Francesco Caputo                      0          28               36      77.8
## Francesco Di Mariano                  0          11               14      78.6
## Gaëtan Laborde                        0          57               97      58.8
## Gareth Bale                           0          45               65      69.2
## Georginio Wijnaldum                   0         669              746      89.7
## Ghislain Konan                        0         978             1296      75.5
## Giacomo Raspadori                     0         814             1045      77.9
## Gianluca Frabotta                     0           4               10      40.0
## Gideon Mensah                         0         638              790      80.8
## Giorgio Chiellini                     0         783              949      82.5
## Giovani Lo Celso                      1         128              161      79.5
## Giulian Biancone                      1         939             1279      73.4
## Giulio Maggiore                       0         834             1123      74.3
## Gonzalo Escalante                     0           4                5      80.0
## Grischa Prömel                        0         713              949      75.1
## Hamza Choudhury                       0         128              156      82.1
## Héctor Herrera                        0         560              668      83.8
## Henrikh Mkhitaryan                    0         959             1168      82.1
## Ignatius Ganago                       0         142              196      72.4
## Ilan Kebbal                           0         617              829      74.4
## Issa Diop                             0         407              478      85.1
## Jack Stephens                         0         361              474      76.2
## Jakub Jankto                          0         163              250      65.2
## Jamie Shackleton                      1         353              409      86.3
## Janik Haberer                         0         244              323      75.5
## Janis Antiste                         0         136              215      63.3
## Jannes Horn                           0         217              305      71.1
## Jarrad Branthwaite                    0          89              117      76.1
## Jason Berthomier                      1        1158             1427      81.1
## Javairô Dilrosun                      0         445              602      73.9
## Jean-Eudes Aholou                     0         339              412      82.3
## Jean-Kévin Augustin                   0          11               14      78.6
## Jean-Philippe Gbamin                  0          16               22      72.7
## Jens Petter Hauge                     0         305              426      71.6
## Jens Stryger Larsen                   0         206              277      74.4
## Jesse Lingard                         0         128              153      83.7
## Jessic Ngankam                        0          25               46      54.3
## Jimmy Cabot                           0        1081             1381      78.3
## Joaquín Correa                        0         280              344      81.4
## Jonas Martin                          1        1533             1695      90.4
## Jonathan Silva                        0          62               94      66.0
## Jonjoe Kenny                          0         351              478      73.4
## Jordan Amavi                          0         269              319      84.3
## Jordan Torunarigha                    0         194              239      81.2
## Julian Draxler                        0         275              306      89.9
## Junior Dina Ebimbe                    0         192              215      89.3
## Junior Sambia                         0         551              725      76.0
## Jurgen Ekkelenkamp                    0         165              241      68.5
## Justin Kluivert                       0         423              665      63.6
## Kalvin Phillips                       0         786              971      80.9
## Kasper Dolberg                        0         172              243      70.8
## Keinan Davis                          0           3                3     100.0
## Kevin Mbabu                           0         599              883      67.8
## Kevin Stöger                          1         244              329      74.2
## Kevin Strootman                       0         297              368      80.7
## Ki-Jana Hoever                        0         110              160      68.8
## Kingsley Ehizibue                     0         248              364      68.1
## Kortney Hause                         0         175              222      78.8
## Kristoffer Askildsen                  0         147              226      65.0
## Landry Dimata                         0          67               92      72.8
## László Bénes                          0         138              187      73.8
## Leander Dendoncker                    1         709              822      86.3
## Leonardo Mancuso                      0          51               77      66.2
## Louis Schaub                          0         205              320      64.1
## Luca Pellegrini                       0         521              682      76.4
## Lucas Alario                          0         115              171      67.3
## Lucas Ocampos                         1         656              864      75.9
## Lucas Perrin                          0        1221             1454      84.0
## Lucas Torreira                        0        1193             1368      87.2
## Ludwig Augustinsson                   0         437              551      79.3
## Luis Binks                            1         271              338      80.2
## Lukas Klünter                         0          67               96      69.8
## Luuk de Jong                          0         132              185      71.4
## Malang Sarr                           0         432              493      87.6
## Mamadou Coulibaly                     0         250              331      75.5
## Manuel Akanji                         0        1819             2013      90.4
## Marco John                            0          15               16      93.8
## Marcus Forss                          0          22               38      57.9
## Marcus Ingvartsen                     1         148              217      68.2
## Marko Pjaca                           0         322              420      76.7
## Martin Braithwaite                    0          44               56      78.6
## Mateo Klimowicz                       0         140              193      72.5
## Mathías Olivera                       1        1017             1430      71.1
## Mathias Pereira Lage                  0         363              525      69.1
## Matija Nastasić                       0         143              154      92.9
## Matt Miazga                           1         244              321      76.0
## Matt Targett                          0         592              823      71.9
## Mattéo Guendouzi                      0        1882             2178      86.4
## Matteo Lovato                         0         488              592      82.4
## Matteo Pessina                        0         718              880      81.6
## Matthew Hoppe                         0          24               34      70.6
## Matthias Ginter                       0        1451             1759      82.5
## Matthijs de Ligt                      0        1510             1704      88.6
## Mattia Zaccagni                       0          56               70      80.0
## Mattias Svanberg                      0         766             1037      73.9
## Maxim Leitsch                         1         685              835      82.0
## Mehdi Bourabia                        0          91              110      82.7
## Merih Demiral                         1         683              801      85.3
## Mihailo Ristić                        0         784             1055      74.3
## Mikkel Damsgaard                      0         196              264      74.2
## Morgan Gibbs-White                    0           7               10      70.0
## Moritz Jenz                           0         584              691      84.5
## Morten Thorsby                        0         705              992      71.1
## Moussa Doumbia                        0          28               35      80.0
## Munir El Haddadi                      0         102              133      76.7
## Nathan Tella                          0         103              177      58.2
## Neal Maupay                           0         479              622      77.0
## Neco Williams                         0           9                9     100.0
## Nehuén Pérez                          0         593              774      76.6
## Nemanja Radonjić                      0           9               10      90.0
## Nicolas Pépé                          0         283              365      77.5
## Nicolò Casale                         0         957             1195      80.1
## Nicolò Rovella                        0         762              977      78.0
## Niklas Süle                           0        1586             1767      89.8
## Oleksandr Zinchenko                   1         953             1073      88.8
## Omar Richards                         0         312              361      86.4
## Orbelín Pineda                        0          57               66      86.4
## Orel Mangala                          1         655              793      82.6
## Oussama Idrissi                       0         293              389      75.3
## Ozan Kabak                            0         350              431      81.2
## Patrick Berg                          0         179              218      82.1
## Patrick Cutrone                       0         161              248      64.9
## Paul Nebel                            0          60               97      61.9
## Philipp Förster                       0         377              444      84.9
## Pierre-Yves Hamel                     0          71               99      71.7
## Pietro Pellegri                       0          12               19      63.2
## Ragnar Ache                           0          37               64      57.8
## Raheem Sterling                       0         714              880      81.1
## Randal Kolo Muani                     0         368              547      67.3
## Remo Freuler                          1        1486             1728      86.0
## Renato Steffen                        0         317              429      73.9
## Rey Manaj                             0         329              448      73.4
## Ricardo Pepi                          0          67              101      66.3
## Riccardo Calafiori                    0          12               21      57.1
## Roberto Piccoli                       1          29               56      51.8
## Rolando Mandragora                    0         647              769      84.1
## Romelu Lukaku                         0         231              327      70.6
## Ronaël Pierre-Gabriel                 0        1198             1428      83.9
## Salih Özcan                           1        1128             1360      82.9
## Sam Lammers                           0          12               20      60.0
## Samuel Umtiti                         0          42               50      84.0
## Samuele Ricci                         0         367              395      92.9
## Santiago Ascacíbar                    0         959             1168      82.1
## Sargis Adamyan                        0          73              101      72.3
## Sebastian Polter                      0         259              489      53.0
## Sebastiano Luperto                    0         821              982      83.6
## Sergiño Dest                          0         876              999      87.7
## Shane Duffy                           1         906             1039      87.2
## Simon Banza                           0          18               31      58.1
## Sofian Kiyine                         0         392              532      73.7
## Sofiane Diop                          0         708              977      72.5
## Stefano Sensi                         0         113              129      87.6
## Steffen Tigges                        0          22               40      55.0
## Stéphane Bahoken                      0         157              218      72.0
## Steven Bergwijn                       0         144              177      81.4
## Sven Botman                           0        1340             1525      87.9
## Taiwo Awoniyi                         0         313              474      66.0
## Takefusa Kubo                         0         349              504      69.2
## Takumi Minamino                       0          58               69      84.1
## Tanguy Nianzou                        0         492              539      91.3
## Tariqe Fosu                           0           6                7      85.7
## Thilo Kehrer                          0        1142             1209      94.5
## Thomas Monconduit                     1         997             1280      77.9
## Timo Werner                           0         297              402      73.9
## Tino Kadewere                         1         133              164      81.1
## Uroš Račić                            0         370              506      73.1
## Vedat Muriqi                          0          42               64      65.6
## Wesley Fofana                         1         470              528      89.0
## Willy Boly                            0         468              558      83.9
## Xaver Schlager                        0         319              422      75.6
## Xavi Simons                           0          48               60      80.0
## Yan Valery                            1         108              155      69.7
## Yangel Herrera                        1         702              870      80.7
## Youssouf Koné                         0         322              439      73.3
## Yves Bissouma                         0        1061             1193      88.9
## Zinho Vanheusden                      0         333              421      79.1
##                        Key Passes Position2.y OVR Overall Percentile
## Aaron Connolly                  1        FWMF  70      69          4
## Aaron Hickey                   19          DF  69      75          4
## Aaron Ramsey                    2          MF  80      64          1
## Abdou Diallo                    4          DF  78      77          2
## Adam Ounas                     12        MFFW  74      73          3
## Ademola Lookman                23        FWMF  77      77          2
## Adnan Januzaj                  34        FWMF  81      79          1
## Ainsley Maitland-Niles          3          MF  75      75          2
## Akim Zedadka                   45          DF  73      74          3
## Alessandro Florenzi            11        DFFW  81      79          1
## Alessio Romagnoli               3          DF  82      80          1
## Alexander Isak                 20          FW  82      80          1
## Alexandre Lacazette            29          FW  82      81          1
## Alexis Claude-Maurice           8          MF  77      74          2
## Alexis Sánchez                 20          FW  80      79          1
## Amadou Diawara                  1          MF  76      74          2
## Amadou Onana                   15          MF  68      74          4
## Amine Harit                    26        FWMF  75      75          2
## Anderson Lucoqui                6          DF  71      71          4
## Andi Zeqiri                    10        FWMF  67      68          4
## Andrea Belotti                 10          FW  81      80          1
## Andrea La Mantia                8        FWMF  69      70          4
## Andrea Petagna                 10          FW  76      76          2
## Andrea Pinamonti               29          FW  68      75          4
## Andreas Christensen             6          DF  80      82          1
## Andreas Voglsammer             10        FWMF  72      73          3
## Andriy Yarmolenko               4        FWMF  76      75          2
## Ángel Di María                 54          FW  87      84          1
## Angelo Fulgini                 48        MFFW  78      77          2
## Anthony Caci                   28          DF  74      75          3
## Antonio Rüdiger                12          DF  83      87          1
## Anwar El Ghazi                  0        FWMF  76      75          2
## Arkadiusz Milik                15          FW  81      79          1
## Armando Izzo                    0          DF  79      74          1
## Arne Maier                     33          MF  74      76          3
## Arthur Theate                   6          DF  69      75          4
## Aster Vranckx                  11          MF  67      73          4
## Axel Witsel                     6        MFDF  83      79          1
## Aymen Barkok                    1        MFDF  73      72          3
## Ben Davies                     11          DF  73      73          3
## Bertrand Traoré                 0        MFFW  77      76          2
## Billy Gilmour                  25          MF  72      72          3
## Boubacar Kamara                24        MFDF  80      80          1
## Boulaye Dia                    17          FW  77      79          2
## Brandon Soppy                   6        DFMF  68      70          4
## Breel Embolo                   34          FW  77      77          2
## Brian Brobbey                   3        FWMF  73      76          3
## Bryan Reynolds                  0          DF  66      66          4
## Callum Hudson-Odoi             25        MFFW  77      78          2
## Calvin Stengs                  23          MF  77      71          2
## Cédric Hountondji               1          DF  72      73          3
## Cengiz Ünder                   34        FWMF  77      79          2
## Cenk Tosun                      0          MF  74      73          3
## Cheick Doucouré                41          MF  75      76          2
## Cheikhou Kouyaté                5          MF  78      77          2
## Chris Richards                  0          DF  71      74          4
## Christian Benteke              13          FW  75      74          2
## Christian Eriksen              30          MF  82      82          1
## Ciaran Clark                    4          DF  75      72          2
## Clément Lenglet                 2          DF  82      78          1
## Colin Dagba                     2          DF  76      74          2
## Corentin Jean                   2        FWMF  70      71          4
## Corentin Tolisso               16          MF  80      81          1
## Cristian Romero                 6          DF  82      83          1
## Dan Ndoye                       2        FWMF  69      69          4
## Daniel James                   24        FWMF  77      77          2
## Daniel Maldini                  1          MF  59      65          4
## Daniel Wass                     0        DFFW  80      79          1
## Danny da Costa                  2        DFMF  76      74          2
## David Nemeth                    0          DF  69      68          4
## Dele Alli                       2          MF  80      77          1
## Denis Cheryshev                 3        MFFW  76      75          2
## Denis Vavro                     0          DF  71      73          4
## Denis Zakaria                   3          MF  80      81          1
## Diego Lainez                    3          FW  74      74          3
## Divock Origi                    0          FW  76      76          2
## Dominique Heintz                0          DF  74      73          3
## Dries Mertens                  33        FWMF  84      84          1
## Dwight Gayle                    2        FWDF  73      72          3
## Dylan Chambost                  9        MFFW  66      68          4
## Ebrima Colley                   5        FWMF  67      67          4
## Eddie Salcedo                   5        MFFW  70      68          4
## Edinson Cavani                  7          FW  85      81          1
## El Bilal Touré                 11          FW  69      70          4
## Enzo Ebosse                     9          DF  65      70          4
## Eric Bailly                     0          DF  79      76          1
## Erick Cabaco                    2          DF  73      72          3
## Erik Durm                       3          DF  74      72          3
## Erik Thommy                     9        FWMF  72      72          3
## Evann Guessand                  5          FW  67      70          4
## Fabio Depaoli                   0        MFDF  72      73          3
## Federico Bernardeschi          17        MFFW  79      79          1
## Federico Bonazzoli             10        FWMF  69      73          4
## Federico Chiesa                17        MFFW  83      84          1
## Federico Fernández              1          DF  75      73          2
## Filippo Melegoni               15        MFFW  62      67          4
## Flavius Daniliuc                3          DF  73      73          3
## Florent Mollet                 32        FWMF  78      76          2
## Francesco Caputo                3          FW  82      78          1
## Francesco Di Mariano            1          FW  66      71          4
## Gaëtan Laborde                  4          FW  80      81          1
## Gareth Bale                     2          FW  82      81          1
## Georginio Wijnaldum            12          MF  84      80          1
## Ghislain Konan                 31          DF  75      76          2
## Giacomo Raspadori              43        MFFW  74      79          3
## Gianluca Frabotta               0        DFMF  71      69          4
## Gideon Mensah                  16          DF  71      70          4
## Giorgio Chiellini               7          DF  86      84          1
## Giovani Lo Celso                5        MFFW  81      81          1
## Giulian Biancone               14        DFMF  68      70          4
## Giulio Maggiore                31          MF  71      77          4
## Gonzalo Escalante               0          MF  77      76          2
## Grischa Prömel                 14          MF  72      76          3
## Hamza Choudhury                 0        DFMF  73      73          3
## Héctor Herrera                 10          MF  81      80          1
## Henrikh Mkhitaryan             41        MFFW  83      81          1
## Ignatius Ganago                11          FW  74      73          3
## Ilan Kebbal                    33        MFFW  67      73          4
## Issa Diop                       1          DF  77      77          2
## Jack Stephens                   4          DF  74      74          3
## Jakub Jankto                    5        DFMF  74      74          3
## Jamie Shackleton                1        DFMF  68      69          4
## Janik Haberer                   1          MF  73      73          3
## Janis Antiste                   6          FW  68      68          4
## Jannes Horn                     6          DF  72      72          3
## Jarrad Branthwaite              0          DF  66      68          4
## Jason Berthomier               46          MF  72      74          3
## Javairô Dilrosun               31        MFFW  73      72          3
## Jean-Eudes Aholou               6          MF  75      75          2
## Jean-Kévin Augustin             0        FWMF  67      65          4
## Jean-Philippe Gbamin            0          MF  77      75          2
## Jens Petter Hauge              12        MFFW  76      74          2
## Jens Stryger Larsen            10          DF  75      75          2
## Jesse Lingard                   8        FWMF  79      78          1
## Jessic Ngankam                  1          FW  68      69          4
## Jimmy Cabot                    18          DF  71      75          4
## Joaquín Correa                 19          FW  81      80          1
## Jonas Martin                    9          MF  72      75          3
## Jonathan Silva                  2        DFFW  74      74          3
## Jonjoe Kenny                   11          DF  74      74          3
## Jordan Amavi                    1          DF  77      74          2
## Jordan Torunarigha              2          DF  73      73          3
## Julian Draxler                  9        FWMF  80      79          1
## Junior Dina Ebimbe              1          MF  71      71          4
## Junior Sambia                  11        DFFW  75      73          2
## Jurgen Ekkelenkamp              8        MFFW  70      71          4
## Justin Kluivert                35          MF  76      76          2
## Kalvin Phillips                 9          MF  81      81          1
## Kasper Dolberg                 15          FW  79      77          1
## Keinan Davis                    0          FW  69      72          4
## Kevin Mbabu                    21          DF  79      77          1
## Kevin Stöger                   14          MF  73      72          3
## Kevin Strootman                 3          MF  76      73          2
## Ki-Jana Hoever                  1        DFMF  65      67          4
## Kingsley Ehizibue               9          DF  72      72          3
## Kortney Hause                   0          DF  75      75          2
## Kristoffer Askildsen            2          MF  59      68          4
## Landry Dimata                   5        FWMF  74      72          3
## László Bénes                    5          MF  73      73          3
## Leander Dendoncker             11          MF  77      76          2
## Leonardo Mancuso                3          FW  72      70          3
## Louis Schaub                   10        MFFW  73      73          3
## Luca Pellegrini                13          DF  74      76          3
## Lucas Alario                    7          FW  78      78          2
## Lucas Ocampos                  18          FW  83      81          1
## Lucas Perrin                    9          DF  69      74          4
## Lucas Torreira                 23          MF  80      81          1
## Ludwig Augustinsson             9        DFFW  76      77          2
## Luis Binks                      1          DF  69      69          4
## Lukas Klünter                   2          DF  70      70          4
## Luuk de Jong                    4          FW  79      79          1
## Malang Sarr                     3          DF  74      75          3
## Mamadou Coulibaly               5          MF  66      67          4
## Manuel Akanji                   2          DF  80      81          1
## Marco John                      0          DF  67      66          4
## Marcus Forss                    1        FWMF  68      68          4
## Marcus Ingvartsen               5        FWMF  71      73          4
## Marko Pjaca                    15          MF  75      75          2
## Martin Braithwaite              2          FW  77      76          2
## Mateo Klimowicz                 4        MFFW  69      67          4
## Mathías Olivera                31          DF  76      77          2
## Mathias Pereira Lage            9          DF  74      72          3
## Matija Nastasić                 1          DF  74      74          3
## Matt Miazga                     2          DF  72      72          3
## Matt Targett                   21          DF  77      77          2
## Mattéo Guendouzi               35          MF  77      80          2
## Matteo Lovato                   4          DF  72      73          3
## Matteo Pessina                 24        MFFW  78      77          2
## Matthew Hoppe                   3        FWMF  68      69          4
## Matthias Ginter                16          DF  84      82          1
## Matthijs de Ligt                9          DF  85      85          1
## Mattia Zaccagni                 5          MF  77      79          2
## Mattias Svanberg               35          MF  73      76          3
## Maxim Leitsch                   3          DF  72      74          3
## Mehdi Bourabia                  2          MF  68      67          4
## Merih Demiral                   4          DF  77      77          2
## Mihailo Ristić                 16          DF  74      74          3
## Mikkel Damsgaard               11        MFFW  77      76          2
## Morgan Gibbs-White              0        DFFW  71      75          4
## Moritz Jenz                     2          DF  66      68          4
## Morten Thorsby                 15          MF  73      74          3
## Moussa Doumbia                  0          FW  72      68          3
## Munir El Haddadi                7        FWMF  79      79          1
## Nathan Tella                    7        MFFW  66      68          4
## Neal Maupay                    23          FW  77      76          2
## Neco Williams                   1          DF  68      71          4
## Nehuén Pérez                    6          DF  75      75          2
## Nemanja Radonjić                1          FW  75      74          2
## Nicolas Pépé                   12        FWMF  81      79          1
## Nicolò Casale                  11          DF  64      76          4
## Nicolò Rovella                 30          MF  70      75          4
## Niklas Süle                    10          DF  82      85          1
## Oleksandr Zinchenko            16          DF  80      79          1
## Omar Richards                   3          DF  74      73          3
## Orbelín Pineda                  1        MFFW  77      76          2
## Orel Mangala                   29          MF  76      75          2
## Oussama Idrissi                21          MF  76      76          2
## Ozan Kabak                      2          DF  76      73          2
## Patrick Berg                    1          MF  73      74          3
## Patrick Cutrone                 9          FW  73      72          3
## Paul Nebel                      4        MFFW  64      65          4
## Philipp Förster                 9        MFFW  72      72          3
## Pierre-Yves Hamel               3          FW  71      68          4
## Pietro Pellegri                 3          FW  68      69          4
## Ragnar Ache                     3        MFFW  68      68          4
## Raheem Sterling                42          FW  88      86          1
## Randal Kolo Muani              40        FWMF  74      78          3
## Remo Freuler                   23          MF  80      80          1
## Renato Steffen                 13        FWMF  78      76          2
## Rey Manaj                      13          FW  69      72          4
## Ricardo Pepi                    5        FWMF  64      69          4
## Riccardo Calafiori              0          DF  67      68          4
## Roberto Piccoli                 4          FW  63      69          4
## Rolando Mandragora             31          MF  75      75          2
## Romelu Lukaku                  24          FW  88      86          1
## Ronaël Pierre-Gabriel          15          DF  73      73          3
## Salih Özcan                    19          MF  73      79          3
## Sam Lammers                     0        FWMF  74      71          3
## Samuel Umtiti                   0          DF  80      79          1
## Samuele Ricci                   4          MF  67      74          4
## Santiago Ascacíbar             11          MF  74      74          3
## Sargis Adamyan                  4        FWMF  74      73          3
## Sebastian Polter               28          FW  69      73          4
## Sebastiano Luperto              9          DF  71      73          4
## Sergiño Dest                   18        DFFW  81      77          1
## Shane Duffy                     4          DF  72      74          3
## Simon Banza                     2          FW  72      75          3
## Sofian Kiyine                  19        MFFW  68      70          4
## Sofiane Diop                   38        MFFW  77      78          2
## Stefano Sensi                   4        MFFW  80      78          1
## Steffen Tigges                  1        FWMF  66      68          4
## Stéphane Bahoken                8          FW  75      73          2
## Steven Bergwijn                 8        FWMF  80      80          1
## Sven Botman                     7          DF  79      80          1
## Taiwo Awoniyi                  27          FW  74      77          3
## Takefusa Kubo                  32        FWMF  75      74          2
## Takumi Minamino                 2        FWMF  75      75          2
## Tanguy Nianzou                  5          DF  71      73          4
## Tariqe Fosu                     0          DF  71      69          4
## Thilo Kehrer                    2          DF  76      76          2
## Thomas Monconduit              32          MF  71      72          4
## Timo Werner                     8        FWMF  84      82          1
## Tino Kadewere                   8          FW  77      73          2
## Uroš Račić                      5          MF  78      75          2
## Vedat Muriqi                    0          FW  75      75          2
## Wesley Fofana                   2          DF  78      79          2
## Willy Boly                      1          DF  79      78          1
## Xaver Schlager                  5          MF  80      80          1
## Xavi Simons                     1        MFFW  66      73          4
## Yan Valery                      1          DF  71      71          4
## Yangel Herrera                 12          MF  78      76          2
## Youssouf Koné                   8          DF  70      69          4
## Yves Bissouma                   8          MF  79      81          1
## Zinho Vanheusden                1          DF  74      72          3
```


```r
# Plot the PCA in 3 dimensions
pl <- scatterplot3d(PC$x[,1], PC$x[,2], PC$x[,3], color = colors, angle = 275, xlab = "PC1", ylab = "PC2", zlab = "PC3")
zz.coords <- pl$xyz.convert(PC$x[,1], PC$x[,2], PC$x[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = FinData$Name,               
     cex = .5, col =colors,
     pos = 4) 
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/3D_PCA-1} \end{center}

```r
# Plot the RPCA in 3 dimensions
pl <- scatterplot3d(robPCA$scores[,1], robPCA$scores[,2], robPCA$scores[,3], color = colors, angle = 275, xlab = "RPC1", ylab = "RPC2", zlab = "RPC3")
zz.coords <- pl$xyz.convert(robPCA$scores[,1], robPCA$scores[,2], robPCA$scores[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = FinData$Name,               
     cex = .5, col =colors,
     pos = 4) 
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/3D_PCA-2} \end{center}



We've tested polydot, vanilladot, splinedot, these all do not give a very nice display of the data for KPCA, but both rbfdot and laplacedot give the same type. And it diffferentiates quite well.


```r
# KPCA with radial basis
KPC2 <- kpca(~., data = data.frame(X), kernal = "rbfdot", kpar=list(sigma=0.01))

# Which components explain the most variation


# Plot of KPCA
plot(rotated(KPC2),col=FinData$Position,
xlab="1st Principal Component",ylab="2nd Principal Component", cex = 0)
text(rotated(KPC2), labels = FinData$Name, col = colors[FinData$Position], cex = 0.5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/KPCA-Radial-1} \end{center}


```r
# # KPCA with laplace
KPC4 <- kpca(~., data = data.frame(X), kernal = "laplacedot", kpar=list(sigma=0.01))

# Which components explain the most variation

# Plot of KPCA
plot(rotated(KPC4),col=FinData$Position,
xlab="1st Principal Component",ylab="2nd Principal Component", cex = 0)
text(rotated(KPC4), labels = FinData$Name, col = colors[FinData$Position], cex = 0.5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/KPCA-Laplace-1} \end{center}



```r
# Run various t-sne's
tsne10 <- Rtsne(X, 
               dims = 2, 
               perplexity = 10, 
               verbose = TRUE, 
               max_iter = 1000)
```

```
## Performing PCA
## Read the 327 x 26 data matrix successfully!
## OpenMP is working. 1 threads.
## Using no_dims = 2, perplexity = 10.000000, and theta = 0.500000
## Computing input similarities...
## Building tree...
## Done in 0.03 seconds (sparsity = 0.131302)!
## Learning embedding...
## Iteration 50: error is 64.461934 (50 iterations in 0.04 seconds)
## Iteration 100: error is 64.089607 (50 iterations in 0.04 seconds)
## Iteration 150: error is 64.080614 (50 iterations in 0.04 seconds)
## Iteration 200: error is 64.002647 (50 iterations in 0.03 seconds)
## Iteration 250: error is 64.029060 (50 iterations in 0.03 seconds)
## Iteration 300: error is 0.883456 (50 iterations in 0.04 seconds)
## Iteration 350: error is 0.804676 (50 iterations in 0.04 seconds)
## Iteration 400: error is 0.784886 (50 iterations in 0.03 seconds)
## Iteration 450: error is 0.778897 (50 iterations in 0.03 seconds)
## Iteration 500: error is 0.773616 (50 iterations in 0.04 seconds)
## Iteration 550: error is 0.771468 (50 iterations in 0.03 seconds)
## Iteration 600: error is 0.770868 (50 iterations in 0.03 seconds)
## Iteration 650: error is 0.769373 (50 iterations in 0.03 seconds)
## Iteration 700: error is 0.768151 (50 iterations in 0.03 seconds)
## Iteration 750: error is 0.767278 (50 iterations in 0.04 seconds)
## Iteration 800: error is 0.765927 (50 iterations in 0.03 seconds)
## Iteration 850: error is 0.765456 (50 iterations in 0.04 seconds)
## Iteration 900: error is 0.766979 (50 iterations in 0.04 seconds)
## Iteration 950: error is 0.764892 (50 iterations in 0.04 seconds)
## Iteration 1000: error is 0.765373 (50 iterations in 0.05 seconds)
## Fitting performed in 0.73 seconds.
```

```r
tsne40 <- Rtsne(X, 
                dims = 2, 
                perplexity = 40, 
                verbose = TRUE, 
                max_iter = 1000)
```

```
## Performing PCA
## Read the 327 x 26 data matrix successfully!
## OpenMP is working. 1 threads.
## Using no_dims = 2, perplexity = 40.000000, and theta = 0.500000
## Computing input similarities...
## Building tree...
## Done in 0.09 seconds (sparsity = 0.497770)!
## Learning embedding...
## Iteration 50: error is 52.906821 (50 iterations in 0.08 seconds)
## Iteration 100: error is 52.438174 (50 iterations in 0.05 seconds)
## Iteration 150: error is 52.271761 (50 iterations in 0.06 seconds)
## Iteration 200: error is 52.251834 (50 iterations in 0.04 seconds)
## Iteration 250: error is 52.263304 (50 iterations in 0.04 seconds)
## Iteration 300: error is 0.534546 (50 iterations in 0.04 seconds)
## Iteration 350: error is 0.520144 (50 iterations in 0.04 seconds)
## Iteration 400: error is 0.517245 (50 iterations in 0.05 seconds)
## Iteration 450: error is 0.514824 (50 iterations in 0.04 seconds)
## Iteration 500: error is 0.514618 (50 iterations in 0.04 seconds)
## Iteration 550: error is 0.514007 (50 iterations in 0.04 seconds)
## Iteration 600: error is 0.513306 (50 iterations in 0.04 seconds)
## Iteration 650: error is 0.513442 (50 iterations in 0.04 seconds)
## Iteration 700: error is 0.512678 (50 iterations in 0.04 seconds)
## Iteration 750: error is 0.511959 (50 iterations in 0.05 seconds)
## Iteration 800: error is 0.510278 (50 iterations in 0.04 seconds)
## Iteration 850: error is 0.509235 (50 iterations in 0.04 seconds)
## Iteration 900: error is 0.508714 (50 iterations in 0.04 seconds)
## Iteration 950: error is 0.508544 (50 iterations in 0.05 seconds)
## Iteration 1000: error is 0.506844 (50 iterations in 0.04 seconds)
## Fitting performed in 0.86 seconds.
```

```r
tsne70 <- Rtsne(X, 
                dims = 2, 
                perplexity = 70, 
                verbose = TRUE,
                max_iter = 1000)
```

```
## Performing PCA
## Read the 327 x 26 data matrix successfully!
## OpenMP is working. 1 threads.
## Using no_dims = 2, perplexity = 70.000000, and theta = 0.500000
## Computing input similarities...
## Building tree...
## Done in 0.15 seconds (sparsity = 0.795182)!
## Learning embedding...
## Iteration 50: error is 46.411219 (50 iterations in 0.07 seconds)
## Iteration 100: error is 46.387002 (50 iterations in 0.05 seconds)
## Iteration 150: error is 46.391872 (50 iterations in 0.05 seconds)
## Iteration 200: error is 46.393652 (50 iterations in 0.08 seconds)
## Iteration 250: error is 46.391523 (50 iterations in 0.06 seconds)
## Iteration 300: error is 0.427371 (50 iterations in 0.06 seconds)
## Iteration 350: error is 0.415918 (50 iterations in 0.05 seconds)
## Iteration 400: error is 0.416952 (50 iterations in 0.04 seconds)
## Iteration 450: error is 0.416771 (50 iterations in 0.04 seconds)
## Iteration 500: error is 0.416701 (50 iterations in 0.05 seconds)
## Iteration 550: error is 0.416721 (50 iterations in 0.05 seconds)
## Iteration 600: error is 0.416810 (50 iterations in 0.04 seconds)
## Iteration 650: error is 0.416487 (50 iterations in 0.04 seconds)
## Iteration 700: error is 0.416427 (50 iterations in 0.05 seconds)
## Iteration 750: error is 0.416426 (50 iterations in 0.05 seconds)
## Iteration 800: error is 0.416421 (50 iterations in 0.04 seconds)
## Iteration 850: error is 0.416621 (50 iterations in 0.04 seconds)
## Iteration 900: error is 0.416479 (50 iterations in 0.04 seconds)
## Iteration 950: error is 0.416399 (50 iterations in 0.04 seconds)
## Iteration 1000: error is 0.416424 (50 iterations in 0.04 seconds)
## Fitting performed in 0.99 seconds.
```

```r
tsne100 <- Rtsne(X,
                 dims = 2, 
                 perplexity = 100, 
                 verbose = TRUE,
                 max_iter = 1000)
```

```
## Performing PCA
## Read the 327 x 26 data matrix successfully!
## OpenMP is working. 1 threads.
## Using no_dims = 2, perplexity = 100.000000, and theta = 0.500000
## Computing input similarities...
## Building tree...
## Done in 0.23 seconds (sparsity = 0.984635)!
## Learning embedding...
## Iteration 50: error is 42.426390 (50 iterations in 0.07 seconds)
## Iteration 100: error is 42.270474 (50 iterations in 0.08 seconds)
## Iteration 150: error is 42.270947 (50 iterations in 0.06 seconds)
## Iteration 200: error is 42.273738 (50 iterations in 0.05 seconds)
## Iteration 250: error is 42.272633 (50 iterations in 0.06 seconds)
## Iteration 300: error is 0.303756 (50 iterations in 0.05 seconds)
## Iteration 350: error is 0.272411 (50 iterations in 0.05 seconds)
## Iteration 400: error is 0.271995 (50 iterations in 0.04 seconds)
## Iteration 450: error is 0.272471 (50 iterations in 0.04 seconds)
## Iteration 500: error is 0.272071 (50 iterations in 0.05 seconds)
## Iteration 550: error is 0.272519 (50 iterations in 0.05 seconds)
## Iteration 600: error is 0.272389 (50 iterations in 0.05 seconds)
## Iteration 650: error is 0.272295 (50 iterations in 0.04 seconds)
## Iteration 700: error is 0.272110 (50 iterations in 0.05 seconds)
## Iteration 750: error is 0.272345 (50 iterations in 0.05 seconds)
## Iteration 800: error is 0.272139 (50 iterations in 0.05 seconds)
## Iteration 850: error is 0.272142 (50 iterations in 0.05 seconds)
## Iteration 900: error is 0.272730 (50 iterations in 0.05 seconds)
## Iteration 950: error is 0.272623 (50 iterations in 0.05 seconds)
## Iteration 1000: error is 0.272330 (50 iterations in 0.05 seconds)
## Fitting performed in 1.04 seconds.
```

```r
# Plot t-SNE's
plot(tsne10$Y,
     col = colors[FinData$Position], 
     t = "n", 
     xlab = "t-SNE Dimension 1",
     ylab = "t-SNE Dimension 2")
text(tsne10$Y, labels = FinData$Name, col = colors[FinData$Position], cex = 0.5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Rtsne-1} \end{center}

```r
plot(tsne40$Y,
     col = colors[FinData$Position], 
     t = "n", 
     xlab = "t-SNE Dimension 1",
     ylab = "t-SNE Dimension 2")
text(tsne40$Y, labels = FinData$Name, col = colors[FinData$Position], cex = 0.5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Rtsne-2} \end{center}

```r
plot(tsne70$Y, 
     col = colors[FinData$Position], 
     t = "n", 
     xlab = "t-SNE Dimension 1",
     ylab = "t-SNE Dimension 2")
text(tsne70$Y, labels = FinData$Name, col = colors[FinData$Position], cex = 0.5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Rtsne-3} \end{center}

```r
plot(tsne100$Y,
     col = colors[FinData$Position], 
     t = "n", 
     xlab = "t-SNE Dimension 1",
     ylab = "t-SNE Dimension 2")
text(tsne100$Y, labels = FinData$Name, col = colors[FinData$Position], cex = 0.5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Rtsne-4} \end{center}



# Cluster Analysis

```r
# Hieararchical Clustering (Agglomerative)
cols <- c("darkgrey", "#679920", "#502491")
# Obtain the number of clusters
fviz_nbclust(robPCA$scores, hcut, method = "silhouette") + 
  theme_classic() 
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Hierarchical Agglomerative-1} \end{center}

```r
fviz_nbclust(robPCA$scores, hcut, method = "wss")+ theme_classic() +
  geom_vline(xintercept = 3, linetype = 2)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Hierarchical Agglomerative-2} \end{center}

```r
fviz_nbclust(robPCA$scores, hcut, method = "gap_stat") +
  theme_classic()
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Hierarchical Agglomerative-3} \end{center}

```r
# Obtain hierachical clustering
clusters <- hclust(dist(robPCA$scores),method ="complete" )
plot(clusters)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Hierarchical Agglomerative-4} \end{center}

```r
# Cut hierarchical tree
clusterCut <- cutree(clusters, 3)

# Plot Clusters
pairs(robPCA$scores[,1:4], col = cols[clusterCut],pch = 16, cex = 0.7)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Hierarchical Agglomerative-5} \end{center}



```r
cols <- c( "#502491","#679920", "darkgrey")
# Obtain the number of clusters
fviz_nbclust(robPCA$scores, kmeans, method = "silhouette") +
  theme_classic() 
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Kmeans-1} \end{center}

```r
fviz_nbclust(robPCA$scores, kmeans, method = "wss")+ theme_classic() +
  geom_vline(xintercept = 3, linetype = 2)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Kmeans-2} \end{center}

```r
fviz_nbclust(robPCA$scores, kmeans, method = "gap_stat") +
  theme_classic()
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Kmeans-3} \end{center}

```r
# Obtain kmeans clustering
kmeans1 <- kmeans(robPCA$scores, 3)

# Plot clusters
pairs(robPCA$scores[,1:4],col=cols[kmeans1$cluster], pch=16, cex=0.7) 
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/Kmeans-4} \end{center}

```r
#points(kmeans1$centers,col=1:4,pch=8)

# Cluster 1 is purple
# Cluster 2 is green
# Cluster 3 is grey
```





```r
# Choose between these 2 seeds # 3000 does work slightly better
set.seed(3000)

# Biclustering using ISA
isa.result <- isa(X)

# Turn result into a workable bicluster
biii <- isa.biclust(isa.result)

#plotclust(X, biii)

#parallelCoordinates(X, biii,number = 1)
#parallelCoordinates(X, biii,number = 2)
#parallelCoordinates(X, biii,number = 3)

############### Defensive Biclustering
  # 14 better than 3
#drawHeatmap(X,biii, 3)
  # Both 11 and 14 are informative but maybe just use 14
#drawHeatmap(X,biii, 11)
drawHeatmap(X,biii, 14)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/ISA-1} \end{center}

```r
  # 14 better than 17
#drawHeatmap(X,biii, 17)
  # Same as 17 but less info
#drawHeatmap(X,biii, 19)

############### Attacking Biclustering
drawHeatmap(X,biii, 5)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/ISA-2} \end{center}

```r
  # 16 is better than 12
#drawHeatmap(X,biii, 12)
  # 16 better than 12
#drawHeatmap(X,biii, 13)
drawHeatmap(X,biii, 16)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/ISA-3} \end{center}

```r
# Definitely include all above

############### Misc Biclustering
  # Not sure what yellow cards and red cards tell us. Maybe more aggressive players
drawHeatmap(X,biii, 2)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/ISA-4} \end{center}

```r
############### CDM Biclustering
  # 15 gives more information
drawHeatmap(X,biii, 4)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/ISA-5} \end{center}

```r
#drawHeatmap(X,biii, 15)
#drawHeatmap(X,biii, 18)

############### CAM Biclustering 
  # 10 holds more information than 1
#drawHeatmap(X,biii, 1) 
#drawHeatmap(X,biii, 6)
drawHeatmap(X,biii, 10)
```



\begin{center}\includegraphics[width=0.65\linewidth]{MVA_Assignment_files/figure-latex/ISA-6} \end{center}

```r
#kmeans1$cluster
#which(clusterCut==3)
# From the 19th bicluster onwards, biclusters were more broad and were difficult to interpret due to the number of players involved. So although there are 44 biclusters available it would not be worthwhile to look any further
```







```r
# Obtain original data
Data<-FinData
# Change position to numeric
Data$Position<- as.numeric(Data$Position)
# Change league to numeric
Data$League<- as.numeric(as.factor(Data$League))
# Obtain unique names of variables
names(Data) <- make.names(names(Data), unique=TRUE)

# Data that will be used for modelling
all<-Data[,c(3,35,1,2,6,8:11,15:24,27:29,33)]
# Specify link function for both response variables
fam<- c("gaussian", "gaussian") #defining the distributions of dependent variables

# Names
n0<-names(all)
# Response names
ny_all<- n0[1:2]
# Covariate names
nx_all<- n0[4:length(n0)]

# Specify model formuula
form_all<-  multivariateFormula(ny_all,nx_all)
# Use a subset for CV
sub <- sample(1:nrow(all),35,replace=FALSE)
# Obtain subset (Training set)
sub_fit <- (1:nrow(all))[-sub]

# Specify covariate design matrix of data data
X<- model.matrix(form_all, data=all)[,-1]
# Specify covariate design matrix for test data
xnew <- model.matrix(form_all, data=Data[sub,])
# Specify repsonse matrix of data
Y<-all[,ny_all]

# Perform MVGLM on training set
player_glm<- multivariateGlm.fit(Y[sub_fit,,drop=FALSE],
                                 X[sub_fit,,drop=FALSE],
                                 family=fam,size=NULL)

# Obtain coefficients
coefs <- as.matrix(sapply(player_glm,coef))

# Predict for test set
pred.glm <- multivariatePredictGlm(xnew,family=fam,beta=coefs)

# Obtain RMSE and comparison of results
sqrt(mean((Y[sub,1]-pred.glm[,1])^2))
```

```
## [1] 11.21581
```

```r
sqrt(mean((Y[sub,2]-pred.glm[,2])^2))
```

```
## [1] 5.067235
```

```r
MKV<-cbind(Y[sub,1],pred.glm[,1])
OVR<-cbind(Y[sub,2],pred.glm[,2])
```


