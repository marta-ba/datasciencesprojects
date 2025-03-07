
#leemos las BB.DDatos
#Establecemos el directorio de trabajo;
setwd("C:/Users/smans/Documents/sara/Master/Modulo 5/Examen")
datos <-read.csv("online_shoppers_intention.csv", header = T, sep=",", dec = ".")
head(datos)

##   Administrative Administrative_Duration Informational Informational_Duration
## 1              0                       0             0                      0
## 2              0                       0             0                      0
## 3              0                       0             0                      0
## 4              0                       0             0                      0
## 5              0                       0             0                      0
## 6              0                       0             0                      0
##   ProductRelated ProductRelated_Duration BounceRates ExitRates PageValues
## 1              1                0.000000  0.20000000 0.2000000          0
## 2              2               64.000000  0.00000000 0.1000000          0
## 3              1                0.000000  0.20000000 0.2000000          0
## 4              2                2.666667  0.05000000 0.1400000          0
## 5             10              627.500000  0.02000000 0.0500000          0
## 6             19              154.216667  0.01578947 0.0245614          0
##   SpecialDay Month OperatingSystems Browser Region TrafficType
## 1          0   Feb                1       1      1           1
## 2          0   Feb                2       2      1           2
## 3          0   Feb                4       1      9           3
## 4          0   Feb                3       2      2           4
## 5          0   Feb                3       3      1           4
## 6          0   Feb                2       2      1           3
##         VisitorType Weekend Revenue
## 1 Returning_Visitor   FALSE   FALSE
## 2 Returning_Visitor   FALSE   FALSE
## 3 Returning_Visitor   FALSE   FALSE
## 4 Returning_Visitor   FALSE   FALSE
## 5 Returning_Visitor    TRUE   FALSE
## 6 Returning_Visitor   FALSE   FALSE

str(datos)

## 'data.frame':    12330 obs. of  18 variables:
##  $ Administrative         : int  0 0 0 0 0 0 0 1 0 0 ...
##  $ Administrative_Duration: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Informational          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Informational_Duration : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ ProductRelated         : int  1 2 1 2 10 19 1 0 2 3 ...
##  $ ProductRelated_Duration: num  0 64 0 2.67 627.5 ...
##  $ BounceRates            : num  0.2 0 0.2 0.05 0.02 ...
##  $ ExitRates              : num  0.2 0.1 0.2 0.14 0.05 ...
##  $ PageValues             : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ SpecialDay             : num  0 0 0 0 0 0 0.4 0 0.8 0.4 ...
##  $ Month                  : Factor w/ 10 levels "Aug","Dec","Feb",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ OperatingSystems       : int  1 2 4 3 3 2 2 1 2 2 ...
##  $ Browser                : int  1 2 1 2 3 2 4 2 2 4 ...
##  $ Region                 : int  1 1 9 2 1 1 3 1 2 1 ...
##  $ TrafficType            : int  1 2 3 4 4 3 3 5 3 2 ...
##  $ VisitorType            : Factor w/ 3 levels "New_Visitor",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ Weekend                : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
##  $ Revenue                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...

datos_orig<-datos

summary(datos)

##  Administrative   Administrative_Duration Informational    
##  Min.   : 0.000   Min.   :   0.00         Min.   : 0.0000  
##  1st Qu.: 0.000   1st Qu.:   0.00         1st Qu.: 0.0000  
##  Median : 1.000   Median :   7.50         Median : 0.0000  
##  Mean   : 2.315   Mean   :  80.82         Mean   : 0.5036  
##  3rd Qu.: 4.000   3rd Qu.:  93.26         3rd Qu.: 0.0000  
##  Max.   :27.000   Max.   :3398.75         Max.   :24.0000  
##                                                            
##  Informational_Duration ProductRelated   ProductRelated_Duration
##  Min.   :   0.00        Min.   :  0.00   Min.   :    0.0        
##  1st Qu.:   0.00        1st Qu.:  7.00   1st Qu.:  184.1        
##  Median :   0.00        Median : 18.00   Median :  598.9        
##  Mean   :  34.47        Mean   : 31.73   Mean   : 1194.8        
##  3rd Qu.:   0.00        3rd Qu.: 38.00   3rd Qu.: 1464.2        
##  Max.   :2549.38        Max.   :705.00   Max.   :63973.5        
##                                                                 
##   BounceRates         ExitRates         PageValues        SpecialDay     
##  Min.   :0.000000   Min.   :0.00000   Min.   :  0.000   Min.   :0.00000  
##  1st Qu.:0.000000   1st Qu.:0.01429   1st Qu.:  0.000   1st Qu.:0.00000  
##  Median :0.003112   Median :0.02516   Median :  0.000   Median :0.00000  
##  Mean   :0.022191   Mean   :0.04307   Mean   :  5.889   Mean   :0.06143  
##  3rd Qu.:0.016813   3rd Qu.:0.05000   3rd Qu.:  0.000   3rd Qu.:0.00000  
##  Max.   :0.200000   Max.   :0.20000   Max.   :361.764   Max.   :1.00000  
##                                                                          
##      Month      OperatingSystems    Browser           Region     
##  May    :3364   Min.   :1.000    Min.   : 1.000   Min.   :1.000  
##  Nov    :2998   1st Qu.:2.000    1st Qu.: 2.000   1st Qu.:1.000  
##  Mar    :1907   Median :2.000    Median : 2.000   Median :3.000  
##  Dec    :1727   Mean   :2.124    Mean   : 2.357   Mean   :3.147  
##  Oct    : 549   3rd Qu.:3.000    3rd Qu.: 2.000   3rd Qu.:4.000  
##  Sep    : 448   Max.   :8.000    Max.   :13.000   Max.   :9.000  
##  (Other):1337                                                    
##   TrafficType               VisitorType     Weekend         Revenue       
##  Min.   : 1.00   New_Visitor      : 1694   Mode :logical   Mode :logical  
##  1st Qu.: 2.00   Other            :   85   FALSE:9462      FALSE:10422    
##  Median : 2.00   Returning_Visitor:10551   TRUE :2868      TRUE :1908     
##  Mean   : 4.07                                                            
##  3rd Qu.: 4.00                                                            
##  Max.   :20.00                                                            
## 

#Convierto a factor variables lógicas.
datos$Weekend<-as.numeric(datos$Weekend)
datos$Revenue<-as.numeric(datos$Revenue)
datos$Weekend<-ifelse(datos$Weekend =='0','no','yes')
datos$Revenue<-ifelse(datos$Revenue =='0', 'no', 'yes')
datos$Weekend<-as.factor(datos$Weekend)
datos$Revenue<-as.factor(datos$Revenue)
levels(datos$Revenue)

## [1] "no"  "yes"

levels(datos$Weekend)

## [1] "no"  "yes"

#Convierto a factor variables numéricas:
#specialDay consideramos 0: lejano a festivo 1: proximo a festivo
datos$SpecialDay<-ifelse(datos$SpecialDay == 0 , 1, 0)
datos$SpecialDay<-as.factor(datos$SpecialDay)

datos$OperatingSystems <-as.factor(datos$OperatingSystems)     
datos$Browser<-as.factor(datos$Browser)    
datos$Region<-as.factor(datos$Region)       
datos$TrafficType<-as.factor(datos$TrafficType)       
datos$VisitorType<-as.factor(datos$VisitorType)

#compruebo la estructura de la BB.DD
str(datos)

## 'data.frame':    12330 obs. of  18 variables:
##  $ Administrative         : int  0 0 0 0 0 0 0 1 0 0 ...
##  $ Administrative_Duration: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Informational          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Informational_Duration : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ ProductRelated         : int  1 2 1 2 10 19 1 0 2 3 ...
##  $ ProductRelated_Duration: num  0 64 0 2.67 627.5 ...
##  $ BounceRates            : num  0.2 0 0.2 0.05 0.02 ...
##  $ ExitRates              : num  0.2 0.1 0.2 0.14 0.05 ...
##  $ PageValues             : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ SpecialDay             : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 1 2 1 1 ...
##  $ Month                  : Factor w/ 10 levels "Aug","Dec","Feb",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ OperatingSystems       : Factor w/ 8 levels "1","2","3","4",..: 1 2 4 3 3 2 2 1 2 2 ...
##  $ Browser                : Factor w/ 13 levels "1","2","3","4",..: 1 2 1 2 3 2 4 2 2 4 ...
##  $ Region                 : Factor w/ 9 levels "1","2","3","4",..: 1 1 9 2 1 1 3 1 2 1 ...
##  $ TrafficType            : Factor w/ 20 levels "1","2","3","4",..: 1 2 3 4 4 3 3 5 3 2 ...
##  $ VisitorType            : Factor w/ 3 levels "New_Visitor",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ Weekend                : Factor w/ 2 levels "no","yes": 1 1 1 1 2 1 1 2 1 1 ...
##  $ Revenue                : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...

library(DataExplorer)

## Warning: package 'DataExplorer' was built under R version 3.6.3

#identificacion de las variables con valores missing
introduce(datos)

##    rows columns discrete_columns continuous_columns all_missing_columns
## 1 12330      18                9                  9                   0
##   total_missing_values complete_rows total_observations memory_usage
## 1                    0         12330             221940      1196336

#graficamente
plot_intro(datos)

sapply(datos, function(x) sum(is.na(x)))

##          Administrative Administrative_Duration           Informational 
##                       0                       0                       0 
##  Informational_Duration          ProductRelated ProductRelated_Duration 
##                       0                       0                       0 
##             BounceRates               ExitRates              PageValues 
##                       0                       0                       0 
##              SpecialDay                   Month        OperatingSystems 
##                       0                       0                       0 
##                 Browser                  Region             TrafficType 
##                       0                       0                       0 
##             VisitorType                 Weekend                 Revenue 
##                       0                       0                       0

ggplot(data = datos, aes(x = Revenue, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Ingresos") +
  theme_bw() +
  theme(legend.position = "bottom")

prop.table(table(datos$Revenue)) %>% round(2)

## 
##   no  yes 
## 0.85 0.15

#Weekend
ggplot(data = datos, aes(x = Weekend, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Weekend") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$Weekend, datos$Revenue), margin = 1) %>% round(digits = 2)

##      
##         no  yes
##   no  0.85 0.15
##   yes 0.83 0.17

table(datos$Weekend, datos$Revenue)

##      
##         no  yes
##   no  8053 1409
##   yes 2369  499

#Month
ggplot(data = datos, aes(x = Month, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Month") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$Month, datos$Revenue), margin = 1) %>% round(digits = 2)

##       
##          no  yes
##   Aug  0.82 0.18
##   Dec  0.87 0.13
##   Feb  0.98 0.02
##   Jul  0.85 0.15
##   June 0.90 0.10
##   Mar  0.90 0.10
##   May  0.89 0.11
##   Nov  0.75 0.25
##   Oct  0.79 0.21
##   Sep  0.81 0.19

table(datos$Month, datos$Revenue)

##       
##          no  yes
##   Aug   357   76
##   Dec  1511  216
##   Feb   181    3
##   Jul   366   66
##   June  259   29
##   Mar  1715  192
##   May  2999  365
##   Nov  2238  760
##   Oct   434  115
##   Sep   362   86

#VisitorType
ggplot(data = datos, aes(x = VisitorType, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "visitorType") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$VisitorType, datos$Revenue), margin = 1) %>% round(digits = 2)

##                    
##                       no  yes
##   New_Visitor       0.75 0.25
##   Other             0.81 0.19
##   Returning_Visitor 0.86 0.14

table(datos$VisitorType, datos$Revenue)

##                    
##                       no  yes
##   New_Visitor       1272  422
##   Other               69   16
##   Returning_Visitor 9081 1470

#Operating systems
ggplot(data = datos, aes(x = OperatingSystems, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Operating Systems") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$OperatingSystems, datos$Revenue), margin = 1) %>% round(digits = 2)

##    
##       no  yes
##   1 0.85 0.15
##   2 0.83 0.17
##   3 0.90 0.10
##   4 0.82 0.18
##   5 0.83 0.17
##   6 0.89 0.11
##   7 0.86 0.14
##   8 0.78 0.22

table(datos$OperatingSystems, datos$Revenue)

##    
##       no  yes
##   1 2206  379
##   2 5446 1155
##   3 2287  268
##   4  393   85
##   5    5    1
##   6   17    2
##   7    6    1
##   8   62   17

#Navegador
ggplot(data = datos, aes(x = Browser, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Browser") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$Browser, datos$Revenue), margin = 1) %>% round(digits = 2)

##     
##        no  yes
##   1  0.85 0.15
##   2  0.85 0.15
##   3  0.95 0.05
##   4  0.82 0.18
##   5  0.82 0.18
##   6  0.89 0.11
##   7  0.88 0.12
##   8  0.84 0.16
##   9  1.00 0.00
##   10 0.80 0.20
##   11 0.83 0.17
##   12 0.70 0.30
##   13 0.74 0.26

table(datos$Browser, datos$Revenue)

##     
##        no  yes
##   1  2097  365
##   2  6738 1223
##   3   100    5
##   4   606  130
##   5   381   86
##   6   154   20
##   7    43    6
##   8   114   21
##   9     1    0
##   10  131   32
##   11    5    1
##   12    7    3
##   13   45   16

#Region
ggplot(data = datos, aes(x = Region, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Region") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$Region, datos$Revenue), margin = 1) %>% round(digits = 2)

##    
##       no  yes
##   1 0.84 0.16
##   2 0.83 0.17
##   3 0.85 0.15
##   4 0.85 0.15
##   5 0.84 0.16
##   6 0.86 0.14
##   7 0.84 0.16
##   8 0.87 0.13
##   9 0.83 0.17

table(datos$Region, datos$Revenue)

##    
##       no  yes
##   1 4009  771
##   2  948  188
##   3 2054  349
##   4 1007  175
##   5  266   52
##   6  693  112
##   7  642  119
##   8  378   56
##   9  425   86

#TrafficType
ggplot(data = datos, aes(x = TrafficType, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Trafic Type") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$TrafficType, datos$Revenue), margin = 1) %>% round(digits = 2)

##     
##        no  yes
##   1  0.89 0.11
##   2  0.78 0.22
##   3  0.91 0.09
##   4  0.85 0.15
##   5  0.78 0.22
##   6  0.88 0.12
##   7  0.70 0.30
##   8  0.72 0.28
##   9  0.90 0.10
##   10 0.80 0.20
##   11 0.81 0.19
##   12 1.00 0.00
##   13 0.94 0.06
##   14 0.85 0.15
##   15 1.00 0.00
##   16 0.67 0.33
##   17 1.00 0.00
##   18 1.00 0.00
##   19 0.94 0.06
##   20 0.75 0.25

table(datos$TrafficType, datos$Revenue)

##     
##        no  yes
##   1  2189  262
##   2  3066  847
##   3  1872  180
##   4   904  165
##   5   204   56
##   6   391   53
##   7    28   12
##   8   248   95
##   9    38    4
##   10  360   90
##   11  200   47
##   12    1    0
##   13  695   43
##   14   11    2
##   15   38    0
##   16    2    1
##   17    1    0
##   18   10    0
##   19   16    1
##   20  148   50

#MSpecial Day
ggplot(data = datos, aes(x = SpecialDay, y = ..count.., fill = Revenue)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "special Day") +
  theme_bw() +
  theme(legend.position = "bottom")

#tabla de frecuencias

# Tabla de frecuencias 
prop.table(table(datos$SpecialDay, datos$Revenue), margin = 1) %>% round(digits = 2)

##    
##       no  yes
##   0 0.94 0.06
##   1 0.83 0.17

table(datos$SpecialDay, datos$Revenue)

##    
##       no  yes
##   0 1174   77
##   1 9248 1831

#distribucion de frecuencias para variables continuas
plot_histogram(datos)

# Estadísticos de ingresos en funcion de variables continuas
datos %>% group_by(Revenue) %>%
          summarise(media = mean(Administrative),
                    mediana = median(Administrative),
                    min = min(Administrative),
                    max = max(Administrative))

## # A tibble: 2 x 5
##   Revenue media mediana   min   max
##   <fct>   <dbl>   <dbl> <int> <int>
## 1 no       2.12       0     0    27
## 2 yes      3.39       2     0    26

datos %>% group_by(Revenue) %>%
          summarise(media = mean(Administrative_Duration),
                    mediana = median(Administrative_Duration),
                    min = min(Administrative_Duration),
                    max = max(Administrative_Duration))

## # A tibble: 2 x 5
##   Revenue media mediana   min   max
##   <fct>   <dbl>   <dbl> <dbl> <dbl>
## 1 no       73.7     0       0 3399.
## 2 yes     119.     52.4     0 2087.

datos %>% group_by(Revenue) %>%
          summarise(media = mean(Informational),
                    mediana = median(Informational),
                    min = min(Informational),
                    max = max(Informational))

## # A tibble: 2 x 5
##   Revenue media mediana   min   max
##   <fct>   <dbl>   <dbl> <int> <int>
## 1 no      0.452       0     0    24
## 2 yes     0.786       0     0    12

datos %>% group_by(Revenue) %>%
          summarise(media = mean(Informational_Duration),
                    mediana = median(Informational_Duration),
                    min = min(Informational_Duration),
                    max = max(Informational_Duration))

## # A tibble: 2 x 5
##   Revenue media mediana   min   max
##   <fct>   <dbl>   <dbl> <dbl> <dbl>
## 1 no       30.2       0     0 2549.
## 2 yes      57.6       0     0 1768.

datos %>% group_by(Revenue) %>%
          summarise(media = mean(ProductRelated),
                    mediana = median(ProductRelated),
                    min = min(ProductRelated),
                    max = max(ProductRelated))

## # A tibble: 2 x 5
##   Revenue media mediana   min   max
##   <fct>   <dbl>   <dbl> <int> <int>
## 1 no       28.7      16     0   705
## 2 yes      48.2      29     0   534

datos %>% group_by(Revenue) %>%
          summarise(media = mean(ProductRelated_Duration),
                    mediana = median(ProductRelated_Duration),
                    min = min(ProductRelated_Duration),
                    max = max(ProductRelated_Duration))

## # A tibble: 2 x 5
##   Revenue media mediana   min    max
##   <fct>   <dbl>   <dbl> <dbl>  <dbl>
## 1 no      1070.    510.     0 63974.
## 2 yes     1876.   1110.     0 27010.

datos %>% group_by(Revenue) %>%
          summarise(media = mean(BounceRates),
                    mediana = median(BounceRates),
                    min = min(BounceRates),
                    max = max(BounceRates))

## # A tibble: 2 x 5
##   Revenue   media mediana   min   max
##   <fct>     <dbl>   <dbl> <dbl> <dbl>
## 1 no      0.0253  0.00426     0   0.2
## 2 yes     0.00512 0           0   0.2

datos %>% group_by(Revenue) %>%
          summarise(media = mean(ExitRates),
                    mediana = median(ExitRates),
                    min = min(ExitRates),
                    max = max(ExitRates))

## # A tibble: 2 x 5
##   Revenue  media mediana   min   max
##   <fct>    <dbl>   <dbl> <dbl> <dbl>
## 1 no      0.0474  0.0286     0   0.2
## 2 yes     0.0196  0.016      0   0.2

datos %>% group_by(Revenue) %>%
          summarise(media = mean(PageValues),
                    mediana = median(PageValues),
                    min = min(PageValues),
                    max = max(PageValues))

## # A tibble: 2 x 5
##   Revenue media mediana   min   max
##   <fct>   <dbl>   <dbl> <dbl> <dbl>
## 1 no       1.98     0       0  247.
## 2 yes     27.3     16.8     0  362.

#vemos el reparto de la variable ingresos

CrossTable(datos$Revenue,prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE)

## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  12330 
## 
##  
##           |        no |       yes | 
##           |-----------|-----------|
##           |     10422 |      1908 | 
##           |     0.845 |     0.155 | 
##           |-----------|-----------|
## 
## 
## 
## 

correl <- cor(datos[,1:9])
correl

##                         Administrative Administrative_Duration Informational
## Administrative              1.00000000              0.60158334    0.37685043
## Administrative_Duration     0.60158334              1.00000000    0.30270971
## Informational               0.37685043              0.30270971    1.00000000
## Informational_Duration      0.25584814              0.23803079    0.61895486
## ProductRelated              0.43111934              0.28908662    0.37416429
## ProductRelated_Duration     0.37393901              0.35542195    0.38750531
## BounceRates                -0.22356263             -0.14417041   -0.11611362
## ExitRates                  -0.31648300             -0.20579776   -0.16366606
## PageValues                  0.09898959              0.06760848    0.04863169
##                         Informational_Duration ProductRelated
## Administrative                      0.25584814     0.43111934
## Administrative_Duration             0.23803079     0.28908662
## Informational                       0.61895486     0.37416429
## Informational_Duration              1.00000000     0.28004627
## ProductRelated                      0.28004627     1.00000000
## ProductRelated_Duration             0.34736358     0.86092684
## BounceRates                        -0.07406661    -0.20457763
## ExitRates                          -0.10527568    -0.29252628
## PageValues                          0.03086087     0.05628179
##                         ProductRelated_Duration BounceRates  ExitRates
## Administrative                       0.37393901 -0.22356263 -0.3164830
## Administrative_Duration              0.35542195 -0.14417041 -0.2057978
## Informational                        0.38750531 -0.11611362 -0.1636661
## Informational_Duration               0.34736358 -0.07406661 -0.1052757
## ProductRelated                       0.86092684 -0.20457763 -0.2925263
## ProductRelated_Duration              1.00000000 -0.18454112 -0.2519841
## BounceRates                         -0.18454112  1.00000000  0.9130044
## ExitRates                           -0.25198410  0.91300440  1.0000000
## PageValues                           0.05282306 -0.11938603 -0.1744983
##                          PageValues
## Administrative           0.09898959
## Administrative_Duration  0.06760848
## Informational            0.04863169
## Informational_Duration   0.03086087
## ProductRelated           0.05628179
## ProductRelated_Duration  0.05282306
## BounceRates             -0.11938603
## ExitRates               -0.17449831
## PageValues               1.00000000

#graficamente
ggcorr(datos[,1:9],method = c("pairwise", "pearson"),low = "darkred", mid = "white", high = "steelblue")

ggcorr(datos[,1:9],geom="circle",method = c("pairwise", "pearson"),low = "darkred", mid = "white", high = "darkgreen")

library(corrplot)
corrplot(correl,method= 'number')

#Variables muy correlacionadas; 
set.seed(7)
cutoff <- 0.85
correlations <- cor(datos[,1:9])
highlyCorrelated <- findCorrelation(correlations, cutoff=cutoff)
for (value in highlyCorrelated) {
  #mostramos las variables con alta correlacion
    print(names(datos)[value])
}

## [1] "ProductRelated_Duration"
## [1] "ExitRates"

# Creación de un nuevo fichero sin las variables con correlaciones altss
datos_sin_altas<- datos[,-highlyCorrelated]
dim(datos_sin_altas)

## [1] 12330    16

head(datos_sin_altas)

##   Administrative Administrative_Duration Informational Informational_Duration
## 1              0                       0             0                      0
## 2              0                       0             0                      0
## 3              0                       0             0                      0
## 4              0                       0             0                      0
## 5              0                       0             0                      0
## 6              0                       0             0                      0
##   ProductRelated BounceRates PageValues SpecialDay Month OperatingSystems
## 1              1  0.20000000          0          1   Feb                1
## 2              2  0.00000000          0          1   Feb                2
## 3              1  0.20000000          0          1   Feb                4
## 4              2  0.05000000          0          1   Feb                3
## 5             10  0.02000000          0          1   Feb                3
## 6             19  0.01578947          0          1   Feb                2
##   Browser Region TrafficType       VisitorType Weekend Revenue
## 1       1      1           1 Returning_Visitor      no      no
## 2       2      1           2 Returning_Visitor      no      no
## 3       1      9           3 Returning_Visitor      no      no
## 4       2      2           4 Returning_Visitor      no      no
## 5       3      1           4 Returning_Visitor     yes      no
## 6       2      1           3 Returning_Visitor      no      no

#varianza cero
varzero_datos <-datos%>% nearZeroVar(saveMetrics = TRUE)
varzero_datos

##                           freqRatio percentUnique zeroVar   nzv
## Administrative             4.259970     0.2189781   FALSE FALSE
## Administrative_Duration  105.410714    27.0478508   FALSE FALSE
## Informational              9.317003     0.1378751   FALSE FALSE
## Informational_Duration   300.757576    10.2027575   FALSE FALSE
## ProductRelated             1.337634     2.5223033   FALSE FALSE
## ProductRelated_Duration   35.952381    77.4614761   FALSE FALSE
## BounceRates                7.882857    15.1824818   FALSE FALSE
## ExitRates                  2.100592    38.7429035   FALSE FALSE
## PageValues              1600.000000    21.9302514   FALSE FALSE
## SpecialDay                 8.856115     0.0162206   FALSE FALSE
## Month                      1.122081     0.0811030   FALSE FALSE
## OperatingSystems           2.553578     0.0648824   FALSE FALSE
## Browser                    3.233550     0.1054339   FALSE FALSE
## Region                     1.989180     0.0729927   FALSE FALSE
## TrafficType                1.596491     0.1622060   FALSE FALSE
## VisitorType                6.228453     0.0243309   FALSE FALSE
## Weekend                    3.299163     0.0162206   FALSE FALSE
## Revenue                    5.462264     0.0162206   FALSE FALSE

#Escalado y Centrado de Datos Para evitar la influencia de los valores altos en determinadas variables, ya que no están todas en el mismo rango, debemos reescalarlos y centrarlos en media 0.
valores_normalizar <- preProcess(datos_sin_altas, method = c("center", "scale"))

datos_normal <- predict(valores_normalizar, datos_sin_altas)

summary(datos_normal)

##  Administrative    Administrative_Duration Informational    
##  Min.   :-0.6970   Min.   :-0.45717        Min.   :-0.3965  
##  1st Qu.:-0.6970   1st Qu.:-0.45717        1st Qu.:-0.3965  
##  Median :-0.3959   Median :-0.41475        Median :-0.3965  
##  Mean   : 0.0000   Mean   : 0.00000        Mean   : 0.0000  
##  3rd Qu.: 0.5072   3rd Qu.: 0.07036        3rd Qu.:-0.3965  
##  Max.   : 7.4312   Max.   :18.76880        Max.   :18.4988  
##                                                             
##  Informational_Duration ProductRelated     BounceRates        PageValues     
##  Min.   :-0.2449        Min.   :-0.7135   Min.   :-0.4577   Min.   :-0.3172  
##  1st Qu.:-0.2449        1st Qu.:-0.5561   1st Qu.:-0.4577   1st Qu.:-0.3172  
##  Median :-0.2449        Median :-0.3087   Median :-0.3935   Median :-0.3172  
##  Mean   : 0.0000        Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
##  3rd Qu.:-0.2449        3rd Qu.: 0.1409   3rd Qu.:-0.1109   3rd Qu.:-0.3172  
##  Max.   :17.8680        Max.   :15.1380   Max.   : 3.6670   Max.   :19.1656  
##                                                                              
##  SpecialDay     Month      OperatingSystems    Browser         Region    
##  0: 1251    May    :3364   2      :6601     2      :7961   1      :4780  
##  1:11079    Nov    :2998   1      :2585     1      :2462   3      :2403  
##             Mar    :1907   3      :2555     4      : 736   4      :1182  
##             Dec    :1727   4      : 478     5      : 467   2      :1136  
##             Oct    : 549   8      :  79     6      : 174   6      : 805  
##             Sep    : 448   6      :  19     10     : 163   7      : 761  
##             (Other):1337   (Other):  13     (Other): 367   (Other):1263  
##   TrafficType              VisitorType    Weekend    Revenue    
##  2      :3913   New_Visitor      : 1694   no :9462   no :10422  
##  1      :2451   Other            :   85   yes:2868   yes: 1908  
##  3      :2052   Returning_Visitor:10551                         
##  4      :1069                                                   
##  13     : 738                                                   
##  10     : 450                                                   
##  (Other):1657

#valores_normalizar2 <- preProcess(datos, method = c("center", "scale"))

#datos_normal2 <- predict(valores_normalizar2, datos)

#summary(datos_normal2)

#datos normalizados sin altas correlaciones

datos_reducidos<-SMOTE(Revenue~.,data = datos_normal,perc.over = 100,perc.under=200)
table(datos_reducidos$Revenue)

## 
##   no  yes 
## 3816 3816

#datos normalizados con altas correlaciones
#datos_reducidos2<-SMOTE(Revenue~.,data = datos_normal2,perc.over = 100,perc.under=200)
#table(datos_reducidos2$Revenue)

Resul_Modelo <- function( modelo ){

# Cálculos
  # Curvas ROC
  pred_prob_ent <- predict(modelo, entrenamiento, type="prob")
  pred_prob_val <- predict(modelo, validacion, type="prob")
  curvaROC_ent <- roc(entrenamiento$Revenue,pred_prob_ent[,"yes"])
  curvaROC_val <- roc(validacion$Revenue,pred_prob_val[,"yes"])
  # Predicciones del modelo
  pred_Y <- predict(modelo, validacion, type="raw")

# Resultados generales
  print(modelo$results)
  print(paste("Mejor modelo:"))
  print(modelo$bestTune)
  print(modelo$finalModel)
  print(paste(c("ROC del modelo con el fichero de validación:"), auc(curvaROC_val)))

# Gráfico de curvas ROC
  if (modelo$method == "treebag" | modelo$method == "xxx"){
    plot(curvaROC_val,col="red", main="Simulación con la curva ROC del modelo")
    legend("bottomright", legend = c("Validacion"), col = c("red"), lwd = 2)
  }else{
    plot(curvaROC_ent,col="blue", main="Simulación con la curva ROC del modelo")
    plot(curvaROC_val, col="red", add=TRUE)
    legend("bottomright", legend = c("Entrenamiento", "Validacion"), col = c("blue", "red"), lwd = 2)
  }

# Tabla de confusión e importancia de las variables
  print(confusionMatrix(modelo))
  print(CrossTable(pred_Y, validacion$Revenue, prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE))
  print(varImp(modelo))
}

CurvasROC <- function( modelos ){
  n_modelos = length(modelos)
  pred <- NULL
  nombresModelos <- NULL
  colores <- NULL
  
  pred[[1]] <- predict(modelos[1], validacion, type="prob")

  # Colores para cada modelo generados de forma aleatoria
  colores <- colours(sample(1:502, size = n_modelos))

  plot ( roc(validacion$Revenue,pred[[1]][[1]][,"yes"]), col = colores[1], main="Curvas ROC de todos los modelos" )

  for( j in 1:length(modelos))
  {
    nombresModelos[j] <- modelos[[j]]$method
  }
  
  legend("bottomright", legend = nombresModelos, col = colores, lwd = 2)
         
  for (i in 2:n_modelos){
    pred[[i]] <- predict(modelos[i], validacion, type="prob")
    plot ( roc(validacion$Revenue,pred[[i]][[1]][,"yes"]), col = colores[i], add=TRUE )
  }
}

Result <- function ( modelos ){
  n_modelos = length(modelos)
  comparativa <- matrix(0, n_modelos, 7)
  pred <- NULL

  for (i in 1:n_modelos){
    pred[[i]] <- predict(modelos[i], validacion, type="prob")
    comparativa[i,1] = modelos[[i]]$method

       comparativa[i,2] = modelos[[i]]$results[rownames(modelos[[i]]$bestTune), c("ROC")]
       comparativa[i,3] = modelos[[i]]$results[rownames(modelos[[i]]$bestTune), c("Sens")]
       comparativa[i,4] = modelos[[i]]$results[rownames(modelos[[i]]$bestTune), c("Spec")]
       comparativa[i,5] = modelos[[i]]$results[rownames(modelos[[i]]$bestTune), c("Accuracy")]
       comparativa[i,6] = modelos[[i]]$results[rownames(modelos[[i]]$bestTune), c("Kappa")]
    
    comparativa[i,7] = auc(roc(validacion$Revenue,pred[[i]][[1]][,"yes"]))
  }
  colnames(comparativa) <- c("Modelo", "ROC", "Sens", "Spec", "Accuracy", "Kappa", "ROC Validación")
  return(comparativa)
}

set.seed(107)

# Índice de partición
Indice_Particion <- createDataPartition(datos_reducidos$Revenue, p = 0.80, list = FALSE)

# Muestras de entrenamiento y test
entrenamiento <- datos_reducidos[ Indice_Particion, ]
validacion <- datos_reducidos[ -Indice_Particion, ]

#vemos el reparto de la variable dependiente en los distintos dataset de entrenamiento y validacion,
table(entrenamiento$Revenue)

## 
##   no  yes 
## 3053 3053

table(validacion$Revenue)

## 
##  no yes 
## 763 763

fiveStats = function(...) c (twoClassSummary(...), defaultSummary(...))
control <- trainControl(method = "repeatedcv", 
                        #numero de muestras
                        number = 5,
                        repeats = 1, 
                        classProbs = TRUE, 
                        preProc = c("center", "scale"),
                        summaryFunction = fiveStats,
                        returnResamp = "final",
                        allowParallel = TRUE)
metrica <- "ROC"

control_oob <- trainControl(method = "oob", #especifico para algoritmos de Baggin, no hace particionado de datos, no incluimos CV
                            verboseIter=FALSE,
                            classProbs = TRUE) #igualmente queremos info de probabilidades

#clusterCPU <- makePSOCKcluster(detectCores()-1)
#registerDoParallel(clusterCPU)

#durante el entrenamiento la variable entrena tomará valor 1; una vez finalizado, tomará valor 0 para leer los modelos almacenados.
#entrena <- 1
entrena <- 0

if (entrena == 1) {
# Perceptrón multicapa
#en una primera aproximacion tomamos los valores por defecto de caret.
#mlpGrid<- NULL
mlpGrid <- expand.grid(size = c(3,4))

mlpx <- train(Revenue ~ ., 
             data = entrenamiento, 
             method = "mlp", 
             metric = metrica,
             #preProc = c("center", "scale"),
             trControl = control,
             tuneGrid = mlpGrid)


saveRDS(mlpx, file ="mlpex5.rds")


#Lectura del ajuste
mlpx<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/mlpex5.rds")
plot(mlpx)
mlpx$results
stopCluster(clusterCPU)
}

#clusterCPU <- makePSOCKcluster(detectCores()-1)
#registerDoParallel(clusterCPU)

if (entrena == 1){
  mlpx$bestTune[1,1]
  
}
#REalizamos nuestro modelo con el mejor ajuste, en nuestro caso ha sido con 3 neuronas.


if (entrena == 1){
  
size = mlpx$bestTune[1,1]
  mlpGrid <-  expand.grid(size = size)
  mlpf <- train(Revenue ~ ., 
             data = entrenamiento, 
             method = "mlp", 
             metric = metrica,
             #preProc = c("center", "scale"),
             trControl = control,
             tuneGrid = mlpGrid)


saveRDS(mlpf, file ="mlpex5fm.rds")
mlpfm<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/mlpex5fm.rds")
}

#Lectura del modelo final
if (entrena == 0){

mlpf<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/mlpex5fm.rds")
}

#mostramos los resultados del modelo
mlpf_dataf <-as.data.frame(mlpf$results)
mlpf$results

##   size       ROC      Sens      Spec  Accuracy     Kappa       ROCSD     SensSD
## 1    3 0.9353028 0.8352601 0.8873135 0.8612864 0.7225729 0.008148886 0.03462967
##      SpecSD  AccuracySD    KappaSD
## 1 0.0234454 0.007233558 0.01447079

#Validacion
prediccion_prob_mlp <- predict(mlpf,validacion,type = "prob")


prediccion_mlp <- predict(mlpf,validacion)
prediccion_mlp <- factor(prediccion_mlp)


# Matriz de confusión

confusion_mlp <- confusionMatrix(data = prediccion_mlp, reference = validacion$Revenue)

confusion_mlp

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  no yes
##        no  673  90
##        yes  90 673
##                                           
##                Accuracy : 0.882           
##                  95% CI : (0.8648, 0.8978)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : <2e-16          
##                                           
##                   Kappa : 0.7641          
##                                           
##  Mcnemar's Test P-Value : 1               
##                                           
##             Sensitivity : 0.882           
##             Specificity : 0.882           
##          Pos Pred Value : 0.882           
##          Neg Pred Value : 0.882           
##              Prevalence : 0.500           
##          Detection Rate : 0.441           
##    Detection Prevalence : 0.500           
##       Balanced Accuracy : 0.882           
##                                           
##        'Positive' Class : no              
## 

#curva ROC
roc_mlp <- roc(validacion$Revenue,prediccion_prob_mlp[[1]])

## Setting levels: control = no, case = yes

## Setting direction: controls > cases

plot(roc_mlp,legacy.axes=TRUE,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("red", "blue"), max.auc.polygon=TRUE,
     auc.polygon.col="hotpink")

#probamos con distintos valores de coste y sigma.

if (entrena == 1){
  #svm

#svm1
#svmGrid <-  expand.grid(sigma = c(0.001, 0.005,0.01,0.05), C = c(1,20,50,100))
#svm2 
#svmGrid <-  expand.grid(sigma = c(0.001), C = c(50,100))
#svm3 0.0025 0.0050 0.0075 0.0100 0.0125 0.0150 0.0175 0.0200 0.0225 0.0250
#svmGrid <-  expand.grid(sigma = c(0.0025,0.01,0.02), C = c(0.1,0.5,1))
svmGrid <-  expand.grid(sigma = c(0.0100, 0.0125,0.015), C = c(1))
#svmGrid <- NULL
#svmGrid <-  expand.grid(sigma = 1:10/400,
#                       C = 1:10/10)
  svmx <- train(Revenue ~ ., 
               data = entrenamiento, 
               method= "svmRadial", 
               metric = metrica, 
               #preProc = c("center", "scale"), 
               trControl = control, 
               tuneGrid = svmGrid) 
  saveRDS(svmx, file ="svmex54.rds")
svmx<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/svmex54.rds")

plot(svmx)
svmx
svmx$results
}

#svm1<-svm
#svm3<-svm
#El mejor ajuste obtenido, 
if (entrena == 1)  {
sigma = svmx$bestTune[1,1]
c = svmx$bestTune[1,2]
svmGrid <-  expand.grid(sigma = sigma, C =c)
svmf <- train(Revenue ~ ., 
               data = entrenamiento, 
               method= "svmRadial", 
               metric = metrica, 
               #preProc = c("center", "scale"), 
               trControl = control, 
               tuneGrid = svmGrid) 
  saveRDS(svmf, file ="svmex5f.rds")
svmf<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/svmex5f.rds")
svmf
}

if (entrena == 0){
svmf<- readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/svmex5f.rds")
}

svmf$results

##    sigma C       ROC      Sens      Spec  Accuracy     Kappa      ROCSD
## 1 0.0125 1 0.9289908 0.8994371 0.8031365 0.8512937 0.7025831 0.01298585
##       SensSD     SpecSD AccuracySD   KappaSD
## 1 0.01518232 0.02166579 0.01457842 0.0291571

svmf_dataf <-as.data.frame(svmf$results)

#Validacion
prediccion_prob_svm <- predict(svmf,validacion,type = "prob")


prediccion_svm <- predict(svmf,validacion)
prediccion_svm <- factor(prediccion_svm)



# Matriz de confusión

confusion_svm <- confusionMatrix(data = prediccion_svm, reference = validacion$Revenue)

confusion_svm

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  no yes
##        no  693 139
##        yes  70 624
##                                           
##                Accuracy : 0.863           
##                  95% CI : (0.8448, 0.8799)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7261          
##                                           
##  Mcnemar's Test P-Value : 2.555e-06       
##                                           
##             Sensitivity : 0.9083          
##             Specificity : 0.8178          
##          Pos Pred Value : 0.8329          
##          Neg Pred Value : 0.8991          
##              Prevalence : 0.5000          
##          Detection Rate : 0.4541          
##    Detection Prevalence : 0.5452          
##       Balanced Accuracy : 0.8630          
##                                           
##        'Positive' Class : no              
## 

#curva ROC
roc_svm <- roc(validacion$Revenue,prediccion_prob_svm[[1]])

## Setting levels: control = no, case = yes

## Setting direction: controls > cases

plot(roc_svm,legacy.axes=TRUE,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("red", "blue"), max.auc.polygon=TRUE,
     auc.polygon.col="hotpink")

# k - vecinos
if (entrena == 1){
#knnGrid <-  expand.grid(k = c(120,150))
#knnGrid <-  expand.grid(k = c(60,80,100,150))
knnGrid <-  expand.grid(k = c(10,15,30,60,80,100,150))
  #knnGrid <- NULL
knnx = train(Revenue ~ ., 
            data = entrenamiento, 
            method = "knn", 
            metric = metrica, 
            #preProc = c("center", "scale"), 
            trControl = control,
            tuneGrid = knnGrid) 

saveRDS(knnx, file ="knnex5.rds")
knnx<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/knnex5.rds")
knnx

plot(knnx)
knnx$results
}

if (entrena == 1) {
#Modelo con mejor resultado
k = knnx$bestTune[1,1]
knnGrid <- expand.grid(k = k)
knnf = train(Revenue ~ ., 
            data = entrenamiento, 
            method = "knn", 
            metric = metrica, 
            #preProc = c("center", "scale"), 
            trControl = control,
            tuneGrid = knnGrid) 

saveRDS(knnf, file ="knnex5f.rds")
knnf<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/knnex5f.rds")
knnf$results
}

if (entrena == 0){
#Lectura del modelo final
knnf<- readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/knnex5f.rds")
}

knnf$results

##    k       ROC      Sens      Spec Accuracy     Kappa       ROCSD      SensSD
## 1 30 0.9067708 0.8778257 0.7530466 0.815431 0.6308666 0.009693414 0.009378602
##       SpecSD AccuracySD    KappaSD
## 1 0.02833641 0.01503632 0.03005872

knnf_dataf <-as.data.frame(knnf$results)

#Validacion
prediccion_prob_knn <- predict(knnf,validacion,type = "prob")


prediccion_knn <- predict(knnf,validacion)
prediccion_knn <- factor(prediccion_knn)



# Matriz de confusión

confusion_knn <- confusionMatrix(data = prediccion_knn, reference = validacion$Revenue)

confusion_knn

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  no yes
##        no  667 188
##        yes  96 575
##                                           
##                Accuracy : 0.8139          
##                  95% CI : (0.7934, 0.8331)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6278          
##                                           
##  Mcnemar's Test P-Value : 6.669e-08       
##                                           
##             Sensitivity : 0.8742          
##             Specificity : 0.7536          
##          Pos Pred Value : 0.7801          
##          Neg Pred Value : 0.8569          
##              Prevalence : 0.5000          
##          Detection Rate : 0.4371          
##    Detection Prevalence : 0.5603          
##       Balanced Accuracy : 0.8139          
##                                           
##        'Positive' Class : no              
## 

#curva ROC
roc_knn <- roc(validacion$Revenue,prediccion_prob_knn[[1]])

## Setting levels: control = no, case = yes

## Setting direction: controls > cases

plot(roc_knn,legacy.axes=TRUE,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("red", "blue"), max.auc.polygon=TRUE,
     auc.polygon.col="hotpink")

if (entrena == 1){
#boostGrid <-  expand.grid(nIter = 1:10*50, method = c("Adaboost.M1"))
#boostx
#boostGrid <-  expand.grid(nIter = c(50,300), method = c("Adaboost.M1"))
boostGrid <-  expand.grid(nIter = c(350,500), method = c("Adaboost.M1"))
clusterCPU <- makePSOCKcluster(detectCores()-1)
registerDoParallel(clusterCPU)
boosty <- train(Revenue ~ ., data = entrenamiento, 
              method = "adaboost", 
              metric = metrica, 
              #preProc = c("center", "scale"), 
              trControl = control,
              tuneGrid = boostGrid)
stopCluster(clusterCPU)
boosty
plot(boosty)
saveRDS(boosty,file ="boostex5y.rds")
boosty<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/boostex5y.rds")
}

#aumento el limite de la memoria, el algoritmo me da muchos problemas, a parte de tiempo de computación, al intentar almacenarlo, me daba error de memoria, perdiendo los datos y muuucho tiempo. 
memory.limit(56000)

## [1] 56000

memory.size () ### Comprobando el tamaño de su memoria

## [1] 305.15

memory.limit () ## Comprobación del límite establecido

## [1] 56000

if (entrena == 0){
boosty<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/boostex5y.rds")
#leo solo la prueba que mejores resultados me ha dado, puesto que esta lectura también colapsa en ocasiones el pc.
#boostx<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/boostex5.rds")



  
}

#De igual manera dejo asteriscados los plot y resultados
#plot(boosty)
#boosty$results
#boostx$results

#boosty$bestTune
#realizo el modelo con el bestune de la configuración boosty  = 500
if (entrena == 1){
#boostGrid <-  expand.grid(nIter = 1:10*50, method = c("Adaboost.M1"))
#boostGrid <-  expand.grid(nIter = c(50,300), method = c("Adaboost.M1"))
c = boosty$bestTune[1,1]
boostGrid <-  expand.grid(nIter = 500, method = c("Adaboost.M1"))
clusterCPU <- makePSOCKcluster(detectCores()-1)
registerDoParallel(clusterCPU)
boostf <- train(Revenue ~ ., data = entrenamiento, 
              method = "adaboost", 
              metric = metrica, 
              #preProc = c("center", "scale"), 
              trControl = control,
              tuneGrid = boostGrid)
saveRDS(boostf,file ="boostex5f.rds")

}


boostf<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/boostex5f.rds")

boostf$results

##   nIter      method      ROC      Sens      Spec  Accuracy     Kappa
## 1   500 Adaboost.M1 0.957383 0.8781471 0.8915843 0.8848668 0.7697331
##         ROCSD      SensSD      SpecSD  AccuracySD    KappaSD
## 1 0.002216413 0.009815899 0.006642934 0.002654584 0.00530966

boostf_dataf <-as.data.frame(boostf$results)

#Validacion
prediccion_prob_boost <- predict(boostf,validacion,type = "prob")


prediccion_boost <- predict(boostf,validacion)
prediccion_boost<- factor(prediccion_boost)



# Matriz de confusión

confusion_boost <- confusionMatrix(data = prediccion_boost, reference = validacion$Revenue)

confusion_boost

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  no yes
##        no  690  71
##        yes  73 692
##                                           
##                Accuracy : 0.9056          
##                  95% CI : (0.8898, 0.9198)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : <2e-16          
##                                           
##                   Kappa : 0.8113          
##                                           
##  Mcnemar's Test P-Value : 0.9336          
##                                           
##             Sensitivity : 0.9043          
##             Specificity : 0.9069          
##          Pos Pred Value : 0.9067          
##          Neg Pred Value : 0.9046          
##              Prevalence : 0.5000          
##          Detection Rate : 0.4522          
##    Detection Prevalence : 0.4987          
##       Balanced Accuracy : 0.9056          
##                                           
##        'Positive' Class : no              
## 

#curva ROC
roc_boost <- roc(validacion$Revenue,prediccion_prob_boost[[1]])

## Setting levels: control = no, case = yes

## Setting direction: controls > cases

plot(roc_boost,legacy.axes=TRUE,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("red", "blue"), max.auc.polygon=TRUE,
     auc.polygon.col="hotpink")

if (entrena == 1){
#ajuste del modelo Random Forest
clusterCPU <- makePSOCKcluster(detectCores()-1)
registerDoParallel(clusterCPU)
set.seed(9)
#rfx
#tune_grid = NULL#le da una serie de valores y busca el propio caret los mejores posibles. el propio caret lo realiza. y sobre ese mejor valor, ya podemos probar nosotros para afinar mas 
#frx2
tune_grid = expand.grid(mtry = 4)
rfx2 <- train( Revenue~., entrenamiento,  
                    method = "rf",  #random forest
                    metric = "ROC", 
                    trControl = control,
                    tuneGrid = tune_grid)

stopCluster(clusterCPU)
rfx2


saveRDS(rfx2, file ="rfex52.rds")

#Lectura del ajuste
rfx2 <-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/rfex52.rds")

}

if (entrena == 0){
  rff <-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/rfex52.rds")
}

rff$results

##   mtry       ROC      Sens      Spec  Accuracy    Kappa       ROCSD     SensSD
## 1    4 0.9500409 0.8847066 0.8948523 0.8897826 0.779564 0.006247455 0.01272881
##       SpecSD AccuracySD    KappaSD
## 1 0.01794825 0.01303621 0.02607016

rff_dataf<-as.data.frame(rff$results)

#Validacion
prediccion_prob_rf <- predict(rff,validacion,type = "prob")


prediccion_rf <- predict(rff,validacion)
prediccion_rf <- factor(prediccion_rf)



# Matriz de confusión

confusion_rf <- confusionMatrix(data = prediccion_rf, reference = validacion$Revenue)

confusion_rf

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  no yes
##        no  688  76
##        yes  75 687
##                                          
##                Accuracy : 0.901          
##                  95% CI : (0.885, 0.9156)
##     No Information Rate : 0.5            
##     P-Value [Acc > NIR] : <2e-16         
##                                          
##                   Kappa : 0.8021         
##                                          
##  Mcnemar's Test P-Value : 1              
##                                          
##             Sensitivity : 0.9017         
##             Specificity : 0.9004         
##          Pos Pred Value : 0.9005         
##          Neg Pred Value : 0.9016         
##              Prevalence : 0.5000         
##          Detection Rate : 0.4509         
##    Detection Prevalence : 0.5007         
##       Balanced Accuracy : 0.9010         
##                                          
##        'Positive' Class : no             
## 

#curva ROC
roc_rf <- roc(validacion$Revenue,prediccion_prob_rf[[1]])

## Setting levels: control = no, case = yes

## Setting direction: controls > cases

plot(roc_rf,legacy.axes=TRUE,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("red", "blue"), max.auc.polygon=TRUE,
     auc.polygon.col="hotpink")

if (entrena == 1){
#Modelo Baggin
# Para reproducir siempre igual la parte aleatoria
set.seed(9)
#tune_grid = expand.grid #cuantas variables aleatorias selecciona para hacer el split, para implementar el baggin basico coge todas las variables.(3 variables independeintes)
tune_grid = NULL
modelo_bagging <- train( Revenue~., datos_normal, 
                         method = "rf",
                         metric = "Accuracy",
                         trControl = control_oob,
                         tuneGrid=tune_grid)

saveRDS(modelo_bagging, file ="bagging.rds")
bg <-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/bagging.rds")
bg$results
plot(bg)
}

if (entrena == 0){
  bgf <-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/bagging.rds")
bgf$results
}

##    Accuracy     Kappa mtry
## 1 0.8452555 0.0000000    2
## 2 0.9021898 0.5984217   34
## 3 0.8991079 0.5861490   66

# Regresión logística
if (entrena == 1){
glm <- train( Revenue ~ ., 
              data = entrenamiento, 
              method = "glm", metric = 
              metrica,
              preProc = c("center", "scale"), 
              trControl = control)
saveRDS(glm, file ="glmex5.rds")
readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/glmex5.rds")
}
if (entrena == 0){
  glmf<-readRDS(file="C:/Users/smans/Documents/sara/Master/Modulo 5/Examen/glmex5.rds")
}

glmf$results

##   parameter       ROC      Sens      Spec  Accuracy     Kappa       ROCSD
## 1      none 0.9178277 0.8657009 0.8021582 0.8339302 0.6678601 0.003911404
##        SensSD     SpecSD  AccuracySD    KappaSD
## 1 0.009425271 0.01065271 0.007613656 0.01522171

glmf_datf <- as.data.frame(glmf$results)

#Validacion
prediccion_prob_glm <- predict(glmf,validacion,type = "prob")

## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
## prediction from a rank-deficient fit may be misleading

prediccion_glm <- predict(glmf,validacion)

## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
## prediction from a rank-deficient fit may be misleading

prediccion_glm <- factor(prediccion_glm)



# Matriz de confusión

confusion_glm <- confusionMatrix(data = prediccion_glm, reference = validacion$Revenue)

confusion_glm

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  no yes
##        no  669 150
##        yes  94 613
##                                           
##                Accuracy : 0.8401          
##                  95% CI : (0.8207, 0.8582)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6802          
##                                           
##  Mcnemar's Test P-Value : 0.0004299       
##                                           
##             Sensitivity : 0.8768          
##             Specificity : 0.8034          
##          Pos Pred Value : 0.8168          
##          Neg Pred Value : 0.8670          
##              Prevalence : 0.5000          
##          Detection Rate : 0.4384          
##    Detection Prevalence : 0.5367          
##       Balanced Accuracy : 0.8401          
##                                           
##        'Positive' Class : no              
## 

#curva ROC
roc_glm <- roc(validacion$Revenue,prediccion_prob_glm[[1]])

## Setting levels: control = no, case = yes

## Setting direction: controls > cases

plot(roc_glm,legacy.axes=TRUE,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("red", "blue"), max.auc.polygon=TRUE,
     auc.polygon.col="hotpink")

Resul_Modelo(mlpf)

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

##   size       ROC      Sens      Spec  Accuracy     Kappa       ROCSD     SensSD
## 1    3 0.9353028 0.8352601 0.8873135 0.8612864 0.7225729 0.008148886 0.03462967
##      SpecSD  AccuracySD    KappaSD
## 1 0.0234454 0.007233558 0.01447079
## [1] "Mejor modelo:"
##   size
## 1    3
## Class: mlp->rsnns
## Number of inputs: 66 
## Number of outputs: 2 
## Maximal iterations: 100 
## Initialization function: Randomize_Weights 
## Initialization function parameters: -0.3 0.3 
## Learning function: Std_Backpropagation 
## Learning function parameters: 0.2 0 
## Update function:Topological_Order
## Update function parameters: 0 
## Patterns are shuffled internally: TRUE 
## Compute error in every iteration: TRUE 
## Architecture Parameters:
## $size
## [1] 3
## 
## All members of model:
##  [1] "nInputs"               "maxit"                 "initFunc"             
##  [4] "initFuncParams"        "learnFunc"             "learnFuncParams"      
##  [7] "updateFunc"            "updateFuncParams"      "shufflePatterns"      
## [10] "computeIterativeError" "snnsObject"            "archParams"           
## [13] "IterativeFitError"     "fitted.values"         "nOutputs"             
## [16] "xNames"                "problemType"           "tuneValue"            
## [19] "obsLevels"             "param"                
## [1] "ROC del modelo con el fichero de validación: 0.94287998845696"

## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   no  yes
##        no  41.8  5.6
##        yes  8.2 44.4
##                             
##  Accuracy (average) : 0.8613
## 
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## | Chi-square contribution |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  1526 
## 
##  
##              | validacion$Revenue 
##       pred_Y |        no |       yes | Row Total | 
## -------------|-----------|-----------|-----------|
##           no |       673 |        90 |       763 | 
##              |   222.732 |   222.732 |           | 
##              |     0.882 |     0.118 |     0.500 | 
##              |     0.882 |     0.118 |           | 
##              |     0.441 |     0.059 |           | 
## -------------|-----------|-----------|-----------|
##          yes |        90 |       673 |       763 | 
##              |   222.732 |   222.732 |           | 
##              |     0.118 |     0.882 |     0.500 | 
##              |     0.118 |     0.882 |           | 
##              |     0.059 |     0.441 |           | 
## -------------|-----------|-----------|-----------|
## Column Total |       763 |       763 |      1526 | 
##              |     0.500 |     0.500 |           | 
## -------------|-----------|-----------|-----------|
## 
##  
## $t
##      y
## x      no yes
##   no  673  90
##   yes  90 673
## 
## $prop.row
##      y
## x            no       yes
##   no  0.8820446 0.1179554
##   yes 0.1179554 0.8820446
## 
## $prop.col
##      y
## x            no       yes
##   no  0.8820446 0.1179554
##   yes 0.1179554 0.8820446
## 
## $prop.tbl
##      y
## x             no        yes
##   no  0.44102228 0.05897772
##   yes 0.05897772 0.44102228
## 
## ROC curve variable importance
## 
##                         Importance
## PageValues                100.0000
## Administrative_Duration    35.3936
## ProductRelated             34.9337
## Administrative             34.3590
## BounceRates                31.3724
## VisitorType                28.9504
## Month                      21.0947
## Weekend                    16.8487
## Informational              14.1579
## Informational_Duration     13.9045
## OperatingSystems            9.7630
## TrafficType                 9.4021
## Browser                     0.9887
## SpecialDay                  0.1806
## Region                      0.0000

plot(varImp(mlpf))

Resul_Modelo(knnf)

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

##    k       ROC      Sens      Spec Accuracy     Kappa       ROCSD      SensSD
## 1 30 0.9067708 0.8778257 0.7530466 0.815431 0.6308666 0.009693414 0.009378602
##       SpecSD AccuracySD    KappaSD
## 1 0.02833641 0.01503632 0.03005872
## [1] "Mejor modelo:"
##    k
## 1 30
## 30-nearest neighbor model
## Training set outcome distribution:
## 
##   no  yes 
## 3053 3053 
## 
## [1] "ROC del modelo con el fichero de validación: 0.903745304198609"

## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   no  yes
##        no  43.9 12.3
##        yes  6.1 37.7
##                             
##  Accuracy (average) : 0.8154
## 
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## | Chi-square contribution |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  1526 
## 
##  
##              | validacion$Revenue 
##       pred_Y |        no |       yes | Row Total | 
## -------------|-----------|-----------|-----------|
##           no |       661 |       188 |       849 | 
##              |   131.760 |   131.760 |           | 
##              |     0.779 |     0.221 |     0.556 | 
##              |     0.866 |     0.246 |           | 
##              |     0.433 |     0.123 |           | 
## -------------|-----------|-----------|-----------|
##          yes |       102 |       575 |       677 | 
##              |   165.236 |   165.236 |           | 
##              |     0.151 |     0.849 |     0.444 | 
##              |     0.134 |     0.754 |           | 
##              |     0.067 |     0.377 |           | 
## -------------|-----------|-----------|-----------|
## Column Total |       763 |       763 |      1526 | 
##              |     0.500 |     0.500 |           | 
## -------------|-----------|-----------|-----------|
## 
##  
## $t
##      y
## x      no yes
##   no  661 188
##   yes 102 575
## 
## $prop.row
##      y
## x            no       yes
##   no  0.7785630 0.2214370
##   yes 0.1506647 0.8493353
## 
## $prop.col
##      y
## x            no       yes
##   no  0.8663172 0.2463958
##   yes 0.1336828 0.7536042
## 
## $prop.tbl
##      y
## x             no        yes
##   no  0.43315858 0.12319790
##   yes 0.06684142 0.37680210
## 
## ROC curve variable importance
## 
##                         Importance
## PageValues                100.0000
## Administrative_Duration    35.3936
## ProductRelated             34.9337
## Administrative             34.3590
## BounceRates                31.3724
## VisitorType                28.9504
## Month                      21.0947
## Weekend                    16.8487
## Informational              14.1579
## Informational_Duration     13.9045
## OperatingSystems            9.7630
## TrafficType                 9.4021
## Browser                     0.9887
## SpecialDay                  0.1806
## Region                      0.0000

plot(varImp(knnf))

Resul_Modelo(svmf)

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

##    sigma C       ROC      Sens      Spec  Accuracy     Kappa      ROCSD
## 1 0.0125 1 0.9289908 0.8994371 0.8031365 0.8512937 0.7025831 0.01298585
##       SensSD     SpecSD AccuracySD   KappaSD
## 1 0.01518232 0.02166579 0.01457842 0.0291571
## [1] "Mejor modelo:"
##    sigma C
## 1 0.0125 1
## Support Vector Machine object of class "ksvm" 
## 
## SV type: C-svc  (classification) 
##  parameter : cost C = 1 
## 
## Gaussian Radial Basis kernel function. 
##  Hyperparameter : sigma =  0.0125 
## 
## Number of Support Vectors : 2621 
## 
## Objective Function Value : -2389.469 
## Training error : 0.142647 
## Probability model included. 
## [1] "ROC del modelo con el fichero de validación: 0.935321186803145"

## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   no  yes
##        no  45.0  9.8
##        yes  5.0 40.2
##                             
##  Accuracy (average) : 0.8513
## 
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## | Chi-square contribution |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  1526 
## 
##  
##              | validacion$Revenue 
##       pred_Y |        no |       yes | Row Total | 
## -------------|-----------|-----------|-----------|
##           no |       693 |       139 |       832 | 
##              |   184.445 |   184.445 |           | 
##              |     0.833 |     0.167 |     0.545 | 
##              |     0.908 |     0.182 |           | 
##              |     0.454 |     0.091 |           | 
## -------------|-----------|-----------|-----------|
##          yes |        70 |       624 |       694 | 
##              |   221.121 |   221.121 |           | 
##              |     0.101 |     0.899 |     0.455 | 
##              |     0.092 |     0.818 |           | 
##              |     0.046 |     0.409 |           | 
## -------------|-----------|-----------|-----------|
## Column Total |       763 |       763 |      1526 | 
##              |     0.500 |     0.500 |           | 
## -------------|-----------|-----------|-----------|
## 
##  
## $t
##      y
## x      no yes
##   no  693 139
##   yes  70 624
## 
## $prop.row
##      y
## x            no       yes
##   no  0.8329327 0.1670673
##   yes 0.1008646 0.8991354
## 
## $prop.col
##      y
## x             no        yes
##   no  0.90825688 0.18217562
##   yes 0.09174312 0.81782438
## 
## $prop.tbl
##      y
## x             no        yes
##   no  0.45412844 0.09108781
##   yes 0.04587156 0.40891219
## 
## ROC curve variable importance
## 
##                         Importance
## PageValues                100.0000
## Administrative_Duration    35.3936
## ProductRelated             34.9337
## Administrative             34.3590
## BounceRates                31.3724
## VisitorType                28.9504
## Month                      21.0947
## Weekend                    16.8487
## Informational              14.1579
## Informational_Duration     13.9045
## OperatingSystems            9.7630
## TrafficType                 9.4021
## Browser                     0.9887
## SpecialDay                  0.1806
## Region                      0.0000

plot(varImp(svmf))

Resul_Modelo(rff)

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

##   mtry       ROC      Sens      Spec  Accuracy    Kappa       ROCSD     SensSD
## 1    4 0.9500409 0.8847066 0.8948523 0.8897826 0.779564 0.006247455 0.01272881
##       SpecSD AccuracySD    KappaSD
## 1 0.01794825 0.01303621 0.02607016
## [1] "Mejor modelo:"
##   mtry
## 1    4
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 4
## 
##         OOB estimate of  error rate: 10.91%
## Confusion matrix:
##       no  yes class.error
## no  2714  339   0.1110383
## yes  327 2726   0.1071078
## [1] "ROC del modelo con el fichero de validación: 0.958159056906156"

## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   no  yes
##        no  44.2  5.3
##        yes  5.8 44.7
##                             
##  Accuracy (average) : 0.8898
## 
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## | Chi-square contribution |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  1526 
## 
##  
##              | validacion$Revenue 
##       pred_Y |        no |       yes | Row Total | 
## -------------|-----------|-----------|-----------|
##           no |       688 |        76 |       764 | 
##              |   245.120 |   245.120 |           | 
##              |     0.901 |     0.099 |     0.501 | 
##              |     0.902 |     0.100 |           | 
##              |     0.451 |     0.050 |           | 
## -------------|-----------|-----------|-----------|
##          yes |        75 |       687 |       762 | 
##              |   245.764 |   245.764 |           | 
##              |     0.098 |     0.902 |     0.499 | 
##              |     0.098 |     0.900 |           | 
##              |     0.049 |     0.450 |           | 
## -------------|-----------|-----------|-----------|
## Column Total |       763 |       763 |      1526 | 
##              |     0.500 |     0.500 |           | 
## -------------|-----------|-----------|-----------|
## 
##  
## $t
##      y
## x      no yes
##   no  688  76
##   yes  75 687
## 
## $prop.row
##      y
## x             no        yes
##   no  0.90052356 0.09947644
##   yes 0.09842520 0.90157480
## 
## $prop.col
##      y
## x             no        yes
##   no  0.90170380 0.09960682
##   yes 0.09829620 0.90039318
## 
## $prop.tbl
##      y
## x             no        yes
##   no  0.45085190 0.04980341
##   yes 0.04914810 0.45019659
## 
## rf variable importance
## 
##   only 20 most important variables shown (out of 66)
## 
##                              Overall
## PageValues                   100.000
## BounceRates                   16.290
## ProductRelated                16.237
## Administrative                14.324
## Administrative_Duration       13.286
## VisitorTypeReturning_Visitor   9.679
## MonthNov                       5.481
## Informational                  5.341
## Informational_Duration         4.835
## Weekendyes                     3.237
## Browser2                       2.375
## TrafficType2                   2.298
## MonthMar                       2.181
## OperatingSystems3              2.059
## TrafficType3                   2.000
## MonthMay                       1.909
## OperatingSystems2              1.543
## TrafficType8                   1.499
## TrafficType13                  1.383
## Region3                        1.240

plot(varImp(rff))

Resul_Modelo(boostf)

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

##   nIter      method      ROC      Sens      Spec  Accuracy     Kappa
## 1   500 Adaboost.M1 0.957383 0.8781471 0.8915843 0.8848668 0.7697331
##         ROCSD      SensSD      SpecSD  AccuracySD    KappaSD
## 1 0.002216413 0.009815899 0.006642934 0.002654584 0.00530966
## [1] "Mejor modelo:"
##   nIter      method
## 1   500 Adaboost.M1
## fastAdaboost::adaboost(formula = .outcome ~ ., data = dat, nIter = param$nIter)
## .outcome ~ .
## <environment: 0x00000001477600c8>
## Dependent Variable: .outcome
## No of trees:500
## The weights of the trees are:1.2095831.042950.97814970.92471850.86462850.86400540.83567130.83925440.8300680.79851690.8038220.77716730.75022210.75197780.7554610.77307220.77511790.76450170.75801190.79884110.70298610.70448410.68137320.68664570.68444960.66260130.68155670.60153240.7680120.70664330.59620890.67390780.59766950.58629750.60685460.42342520.49566560.46976860.40282440.42485270.3849850.36086440.32238160.30604330.27115520.2605820.24087360.2639440.23445140.22492410.19763650.20912090.1869960.1890620.18803780.1797910.16867450.16689990.15229010.14976660.14727310.13669170.13832870.13360870.15653470.1354220.11670160.12455540.11761690.11023780.10666640.10626710.099679230.10431540.10003890.094657960.09371040.09406620.09402770.087920120.097326650.08918290.076983230.086003450.08490230.082815790.079791940.084531370.091475450.10020950.086491590.087558760.070541160.065288960.069953210.06461980.067383270.065859230.064339130.063108160.063235720.06139820.060472890.059823330.058115350.057538820.054780890.053629830.051328450.052962970.050468240.050433850.049119550.045032470.050559540.045640090.044980730.045954520.046310920.045545560.045905150.045112360.043811690.044051540.041655960.042141530.041587530.039732190.037626620.039031820.037583620.037491250.037879710.034388750.034792680.035691390.035224050.035117730.032257180.030035710.035424870.036109380.034475140.03140980.032234960.033790040.030943860.031735230.034718880.034367530.03260480.032229270.030711790.030977870.031430790.031112220.032859070.032816970.030799150.03178070.0302050.029337090.029012130.028181790.028962480.029412310.029005330.028434940.027439630.026628110.027400320.029751070.028897470.028643460.02830070.028066440.028004350.026733990.028123360.027770990.026509560.026661280.025891520.025100720.024703480.026993090.026232870.025858070.026441790.025937850.025959540.024197630.024462860.024302940.023332840.024270740.024184340.023474080.024241080.023920670.023571830.024825920.02365280.023535560.022788330.021813140.022486190.022074390.021945210.021204060.020845110.022092160.021787380.021836730.022844120.021794050.021755680.021619320.020785470.021120220.020570010.020625150.020671320.021777570.02104620.026110330.02962220.026936310.028224810.023437970.02532050.027521980.025807640.026244380.02556110.026017380.025483310.025119820.019087270.021602220.025088330.020122070.021640090.024147160.01967830.021535420.020729170.01775590.018997990.01795630.018001990.020683780.01941870.017019840.017141860.016829010.017924040.020472030.018650270.016184540.016956230.016414550.016597540.016550290.020148480.018435860.019027420.021469070.019939580.019718680.019501890.019947530.019641310.019425970.019311360.019277670.018751590.018690350.018769220.018369880.018145850.017594650.017966230.017456090.017465660.01782650.017910170.017610540.01697550.017458540.01738580.016716890.016901420.016845990.016599860.016536330.016131170.016231840.014235030.014903710.014720890.012670110.013480650.015357060.014225670.012810890.013906980.011679640.012271440.011728150.012168810.011894150.011847360.011667930.011226090.011570760.011371730.01130610.011345440.011368950.011288570.011259610.012162340.012145670.011165440.011022970.010895130.011504510.01119760.011305320.011320760.01165780.011224610.011148170.011791840.010795060.010831840.011375270.010744030.01104210.011667910.011083580.011212480.010872740.01105290.010879530.010625420.010584420.010567020.011034540.010748150.010436860.010484430.010320860.010255980.010445870.010494590.010335640.010374730.010288450.010391640.011052440.010506640.010506260.010267840.010606040.010709880.010516220.0099409380.0099878730.010450850.010206560.010281320.01011230.012293540.010694320.011527460.011942020.011621270.011219730.01137220.011183690.0085341320.0098855250.0092134320.0093925930.011890310.010427520.011139630.011066390.011040360.011112540.0085159470.0097932690.011126040.010255120.010327180.010396210.010059240.0099460050.010536380.010537070.0098368530.010058370.010115390.011812630.011256630.011275750.010903820.01112510.011162450.010994740.010909460.010954070.010598720.010731960.010444230.010616530.011007120.010615230.010804380.010590430.010559220.011231350.01116790.011067810.011092120.011314820.010904570.010865420.010843080.010993470.011196980.010388110.010792120.010822390.01064330.010863130.010744760.010792130.0087770850.0081640610.0089414680.0095552760.010555280.010551130.010092830.010369410.010010360.0098474950.010359080.0096099730.0095373290.010244650.0098960780.0093490090.0082633840.0091827250.0086527240.0073661190.0079608690.0076848150.0078460310.0080534910.0079706950.0079168570.0083315380.008092460.0078751680.0076090520.0073911250.0072696210.0075672810.0074740970.0072524660.007226130.0069120640.0069395480.006835150.0067994740.0067993630.0067918320.0070440310.0077100250.0071188640.0066094020.0068414780.0074767930.007274350.0065974320.0068699220.0071611730.0072944570.0071736360.0071185320.0070999870.0072821220.0069262940.0069755330.006995937
## [1] "ROC del modelo con el fichero de validación: 0.967397783117961"

## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   no  yes
##        no  43.9  5.4
##        yes  6.1 44.6
##                             
##  Accuracy (average) : 0.8849
## 
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## | Chi-square contribution |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  1526 
## 
##  
##              | validacion$Revenue 
##       pred_Y |        no |       yes | Row Total | 
## -------------|-----------|-----------|-----------|
##           no |       690 |        71 |       761 | 
##              |   251.748 |   251.748 |           | 
##              |     0.907 |     0.093 |     0.499 | 
##              |     0.904 |     0.093 |           | 
##              |     0.452 |     0.047 |           | 
## -------------|-----------|-----------|-----------|
##          yes |        73 |       692 |       765 | 
##              |   250.432 |   250.432 |           | 
##              |     0.095 |     0.905 |     0.501 | 
##              |     0.096 |     0.907 |           | 
##              |     0.048 |     0.453 |           | 
## -------------|-----------|-----------|-----------|
## Column Total |       763 |       763 |      1526 | 
##              |     0.500 |     0.500 |           | 
## -------------|-----------|-----------|-----------|
## 
##  
## $t
##      y
## x      no yes
##   no  690  71
##   yes  73 692
## 
## $prop.row
##      y
## x             no        yes
##   no  0.90670171 0.09329829
##   yes 0.09542484 0.90457516
## 
## $prop.col
##      y
## x             no        yes
##   no  0.90432503 0.09305374
##   yes 0.09567497 0.90694626
## 
## $prop.tbl
##      y
## x             no        yes
##   no  0.45216252 0.04652687
##   yes 0.04783748 0.45347313
## 
## ROC curve variable importance
## 
##                         Importance
## PageValues               100.00000
## ProductRelated            34.08880
## BounceRates               31.97798
## Administrative_Duration   31.64729
## Administrative            31.15591
## VisitorType               27.98099
## Month                     18.02184
## Weekend                   16.09684
## Informational             15.25814
## Informational_Duration    14.73711
## TrafficType               11.87724
## OperatingSystems           9.95142
## Browser                    3.78952
## Region                     0.07905
## SpecialDay                 0.00000

plot(varImp(boostf))

modelos <- list(mlpf, knnf, svmf, rff,boostf)
Comparativa <- as.data.frame(Result (modelos))

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

## Setting levels: control = no, case = yes

## Setting direction: controls < cases

print(Comparativa)

##      Modelo               ROC              Sens              Spec
## 1       mlp 0.935302754270477 0.835260121810523 0.887313460867699
## 2       knn 0.906770842971203 0.877825655335247  0.75304660459875
## 3 svmRadial 0.928990782379916 0.899437095865418 0.803136486812803
## 4        rf 0.950040879863376 0.884706608355021 0.894852298033323
## 5  adaboost 0.957382957333718 0.878147084864908 0.891584341713397
##            Accuracy             Kappa    ROC Validación
## 1 0.861286426347466  0.72257285007849  0.94287998845696
## 2 0.815430994154398 0.630866610575404 0.903745304198609
## 3 0.851293712995841 0.702583069827159 0.935321186803145
## 4 0.889782596165575 0.779563967676112 0.958159056906156
## 5 0.884866817273514  0.76973306933932 0.967397783117961

modelos <- list(RF = rff, KNN = knnf, MLP = mlpf, SVM = svmf, boost = boostf)
resultados <- resamples(modelos)
resultados

## 
## Call:
## resamples.default(x = modelos)
## 
## Models: RF, KNN, MLP, SVM, boost 
## Number of resamples: 5 
## Performance metrics: Accuracy, Kappa, ROC, Sens, Spec 
## Time estimates for: everything, final model fit

summary(resultados)

## 
## Call:
## summary.resamples(object = resultados)
## 
## Models: RF, KNN, MLP, SVM, boost 
## Number of resamples: 5 
## 
## Accuracy 
##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## RF    0.8772504 0.8796069 0.8869779 0.8897826 0.8959869 0.9090909    0
## KNN   0.7937807 0.8083538 0.8181818 0.8154310 0.8239148 0.8329238    0
## MLP   0.8518822 0.8558559 0.8641571 0.8612864 0.8648649 0.8696721    0
## SVM   0.8337428 0.8435708 0.8518822 0.8512937 0.8542179 0.8730549    0
## boost 0.8821604 0.8827869 0.8845209 0.8848668 0.8861589 0.8887070    0
## 
## Kappa 
##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## RF    0.7545008 0.7592146 0.7739556 0.7795640 0.7919723 0.8181766    0
## KNN   0.5875614 0.6167434 0.6364073 0.6308666 0.6477987 0.6658223    0
## MLP   0.7037643 0.7116999 0.7283142 0.7225729 0.7297415 0.7393443    0
## SVM   0.6675082 0.6871142 0.7037643 0.7025831 0.7084058 0.7461229    0
## boost 0.7643208 0.7655738 0.7690363 0.7697331 0.7723204 0.7774141    0
## 
## ROC 
##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## RF    0.9440562 0.9480320 0.9480883 0.9500409 0.9494017 0.9606262    0
## KNN   0.8939371 0.8997693 0.9093746 0.9067708 0.9135320 0.9172413    0
## MLP   0.9237074 0.9302031 0.9386480 0.9353028 0.9404186 0.9435367    0
## SVM   0.9088031 0.9270666 0.9313515 0.9289908 0.9332692 0.9444635    0
## boost 0.9538274 0.9576812 0.9576969 0.9573830 0.9577393 0.9599701    0
## 
## Sens 
##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## RF    0.8674304 0.8772504 0.8868852 0.8847066 0.8918033 0.9001637    0
## KNN   0.8655738 0.8756137 0.8772504 0.8778257 0.8788871 0.8918033    0
## MLP   0.7921440 0.8265139 0.8311475 0.8352601 0.8379705 0.8885246    0
## SVM   0.8754098 0.8968903 0.9034370 0.8994371 0.9049180 0.9165303    0
## boost 0.8688525 0.8704918 0.8788871 0.8781471 0.8788871 0.8936170    0
## 
## Spec 
##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## RF    0.8819672 0.8870704 0.8870704 0.8948523 0.8918033 0.9263502    0
## KNN   0.7119476 0.7446809 0.7512275 0.7530466 0.7704918 0.7868852    0
## MLP   0.8508197 0.8805237 0.8918033 0.8873135 0.9018003 0.9116203    0
## SVM   0.7901639 0.7918033 0.7921440 0.8031365 0.8003273 0.8412439    0
## boost 0.8837971 0.8854337 0.8934426 0.8915843 0.8967213 0.8985270    0

dotplot(resultados)

diferencias <- diff(resultados)
summary(diferencias)

## 
## Call:
## summary.diff.resamples(object = diferencias)
## 
## p-value adjustment: bonferroni 
## Upper diagonal: estimates of the difference
## Lower diagonal: p-value for H0: difference = 0
## 
## Accuracy 
##       RF       KNN       MLP       SVM       boost    
## RF              0.074352  0.028496  0.038489  0.004916
## KNN   0.004590           -0.045855 -0.035863 -0.069436
## MLP   0.107663 0.029191             0.009993 -0.023580
## SVM   0.054905 0.151216  1.000000            -0.033573
## boost 1.000000 0.008645  0.034107  0.088539           
## 
## Kappa 
##       RF       KNN       MLP       SVM       boost    
## RF              0.148697  0.056991  0.076981  0.009831
## KNN   0.004584           -0.091706 -0.071716 -0.138866
## MLP   0.107701 0.029217             0.019990 -0.047160
## SVM   0.054911 0.151154  1.000000            -0.067150
## boost 1.000000 0.008631  0.034121  0.088527           
## 
## ROC 
##       RF       KNN       MLP       SVM       boost    
## RF              0.043270  0.014738  0.021050 -0.007342
## KNN   0.002206           -0.028532 -0.022220 -0.050612
## MLP   0.065467 0.002728             0.006312 -0.022080
## SVM   0.167147 0.207294  1.000000            -0.028392
## boost 0.749764 0.003469  0.026148  0.134856           
## 
## Sens 
##       RF      KNN        MLP        SVM        boost     
## RF             0.0068810  0.0494465 -0.0147305  0.0065595
## KNN   1.00000             0.0425655 -0.0216114 -0.0003214
## MLP   0.46205 0.75187               -0.0641770 -0.0428870
## SVM   0.02395 0.28658    0.25917                0.0212900
## boost 1.00000 1.00000    0.51279    1.00000              
## 
## Spec 
##       RF       KNN       MLP       SVM       boost    
## RF              0.141806  0.007539  0.091716  0.003268
## KNN   0.004423           -0.134267 -0.050090 -0.138538
## MLP   1.000000 0.017132             0.084177 -0.004271
## SVM   0.021072 0.350695  0.069050            -0.088448
## boost 1.000000 0.002776  1.000000  0.005143

bwplot(diferencias,layout=c(3,3))

names(diferencias)

## [1] "call"       "difs"       "confLevel"  "adjustment" "statistics"
## [6] "models"     "metric"

resultadose <- data.frame(
  modelo1=c(mlpf$modelInfo$label,
           knnf$modelInfo$label,
           svmf$modelInfo$label,
           boostf$modelInfo$label,
           rff$modelInfo$label),
          
  #AUCS de cada modelo
  ROC=round(c(mlpf_dataf$ROC,
               knnf_dataf$ROC,
               svmf_dataf$ROC,
              boostf_dataf$ROC,
              rff_dataf$ROC),digits = 3),
               

  #datos de precision de cada modelo, lo obtenemos de la matriz confusion
  Accuracy=round(c(mlpf_dataf$Accuracy,
                    knnf_dataf$Accuracy,
                    svmf_dataf$Accuracy,
                   boostf_dataf$Accuracy,
                    rff_dataf$Accuracy),digits=3),

    #datos del indice KAppa de cada modelo, lo obtenemos de la matriz confusion
  Kappa=round(c(mlpf_dataf$Kappa,
                    knnf_dataf$Kappa,
                    svmf_dataf$Kappa,
                    boostf_dataf$Kappa,
                    rff_dataf$Kappa),digits=3))

  
resultadose

##                                                     modelo1   ROC Accuracy
## 1                                    Multi-Layer Perceptron 0.935    0.861
## 2                                       k-Nearest Neighbors 0.907    0.815
## 3 Support Vector Machines with Radial Basis Function Kernel 0.929    0.851
## 4                             AdaBoost Classification Trees 0.957    0.885
## 5                                             Random Forest 0.950    0.890
##   Kappa
## 1 0.723
## 2 0.631
## 3 0.703
## 4 0.770
## 5 0.780

#Visualizamos los resultados
resultadose %>%
  mutate(
    modelo1 = modelo1,
    ROC = color_tile("white", "orange")(ROC),#rango de colosres de blanco a naranja, cuanto mas naranja mejor
    Accuracy = color_tile("white", "lightblue")(Accuracy),
    Kappa = color_tile("white", "lightgreen")(Kappa)
  ) %>%
  select(modelo1, everything()) %>%
  kable(escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  add_header_above(c(" ", "Entrenamiento" = 3))

resultados <- data.frame(
  modelo=c(mlpf$modelInfo$label,
           knnf$modelInfo$label,
           svmf$modelInfo$label,
           boostf$modelInfo$label,
           rff$modelInfo$label),
          
  #AUCS de cada modelo
  AUC=round(c(auc(roc_mlp),
               auc(roc_knn),
              auc(roc_svm),
              auc(roc_boost),
              auc(roc_rf)),digits = 3),
               

  #datos de precision de cada modelo, lo obtenemos de la matriz confusion
  Accuracy=round(c(confusion_mlp[["overall"]][["Accuracy"]],
                    confusion_knn[["overall"]][["Accuracy"]],
                    confusion_svm[["overall"]][["Accuracy"]],
                   confusion_boost[["overall"]][["Accuracy"]],
                    confusion_rf[["overall"]][["Accuracy"]]),digits=3),

    #datos del indice KAppa de cada modelo, lo obtenemos de la matriz confusion
  Kappa=round(c(confusion_mlp[["overall"]][["Kappa"]],
                    confusion_knn[["overall"]][["Kappa"]],
                    confusion_svm[["overall"]][["Kappa"]],
                   confusion_boost[["overall"]][["Kappa"]],
                    confusion_rf[["overall"]][["Kappa"]]),digits=3),

  
  AciertosClaseSI=round(c(confusion_mlp[["byClass"]][["Pos Pred Value"]],
                          confusion_knn[["byClass"]][["Pos Pred Value"]],
                           confusion_svm[["byClass"]][["Pos Pred Value"]],
                           confusion_boost[["byClass"]][["Pos Pred Value"]],
                          confusion_rf[["byClass"]][["Pos Pred Value"]]),digits=3),
  
  AciertosClaseNO=round(c(confusion_mlp[["byClass"]][["Neg Pred Value"]],
                          confusion_knn[["byClass"]][["Neg Pred Value"]],
                          confusion_svm[["byClass"]][["Neg Pred Value"]],
                          confusion_boost[["byClass"]][["Neg Pred Value"]],
                          confusion_rf[["byClass"]][["Neg Pred Value"]]),digits=3))


#Visualizamos los resultados
resultados %>%
  mutate(
    modelo = modelo,
    AUC = color_tile("white", "orange")(AUC),#rango de colosres de blanco a naranja, cuanto mas naranja mejor
    Accuracy = color_tile("white", "lightblue")(Accuracy),
    Kappa = color_tile("white", "lightgreen")(Kappa),
    AciertosClaseSI = color_tile("white", "hotpink")(AciertosClaseSI),
    AciertosClaseNO = color_tile("white", "hotpink")(AciertosClaseNO)
  ) %>%
  select(modelo, everything()) %>%
  kable(escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  add_header_above(c(" ", "Test" = 5))