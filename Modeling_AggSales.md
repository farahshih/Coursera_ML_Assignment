# Retail_Scanner_Data_Sales_Prediction


```r
library(knitr)
knit('./Func_AggSales.Rmd', tangle=TRUE)
source('./Func_AggSales.R')
```


```r
p<-"./frozenjuice/upcfrj.csv"
frj_upc<-readupc(p)
head(frj_upc)
```

```
##          upc                        des
## 1 1110000139 FLORIDA GOLD VALENCI 12 OZ
## 2 1110000390 FLORIDAGOLD OLD FASH 12 OZ
## 3 1110000391 FLORIDAGOLD PULP FRE 12 OZ
## 4 1110000550 ~FLORIDA GOLD ORANGE 64 OZ
## 5 1450000124 DEAN FOODS RED RASPB 10 OZ
## 6 2080006311 SPEAS PARENTS CHOICE 12 OZ
```


```r
frj<-read.csv("~/Documents/Codes/Pricing_Project/Data/csv-data/frozenjuice/wfrj.csv") 
head(frj)
```

```
##   STORE      UPC WEEK MOVE QTY PRICE SALE PROFIT OK
## 1     2 1.11e+09    1   14   1  1.79       43.18  1
## 2     2 1.11e+09    2   48   1  1.79       43.18  1
## 3     2 1.11e+09    3  156   1  1.29    B  21.16  1
## 4     2 1.11e+09    4    4   1  1.29    B  22.17  1
## 5     2 1.11e+09    5   51   1  1.79       43.91  1
## 6     2 1.11e+09    6  145   1  1.29    B  22.17  1
```


```r
frj<-clean(frj)  
frj<-process_data(frj,frj_upc)
head(frj)
```

```
##        upc store week move qty price sale profit ok
## 1 1.11e+09     2    1   14   1  1.79       43.18  1
## 2 1.11e+09     2    2   48   1  1.79       43.18  1
## 3 1.11e+09     2    3  156   1  1.29    B  21.16  1
## 4 1.11e+09     2    4    4   1  1.29    B  22.17  1
## 5 1.11e+09     2    5   51   1  1.79       43.91  1
## 6 1.11e+09     2    6  145   1  1.29    B  22.17  1
##                          des  sales unit_price prom
## 1 FLORIDA GOLD VALENCI 12 OZ  25.06       1.79    0
## 2 FLORIDA GOLD VALENCI 12 OZ  85.92       1.79    0
## 3 FLORIDA GOLD VALENCI 12 OZ 201.24       1.29    1
## 4 FLORIDA GOLD VALENCI 12 OZ   5.16       1.29    1
## 5 FLORIDA GOLD VALENCI 12 OZ  91.29       1.79    0
## 6 FLORIDA GOLD VALENCI 12 OZ 187.05       1.29    1
```

compute sku popularity (aggregate by sku)

```r
frj_agg<-agg(frj,frj_upc) 
nrow(frj_agg)==length(unique(frj$upc)) #TRUE! check if aggregation works correct
```

```
## [1] TRUE
```


```r
sum(frj_agg$pct[c(1:15)])
```

```
## [1] 0.4953958
```


```r
frj_agg<-prom_count(frj,frj_agg)
head(frj_agg)
```

```
##          upc   sales                        des        pct rank prom_freq
## 1 3828190029 7627822 HH ORANGE JUICE CONC 12 OZ 0.10723646    1 0.1930868
## 2 4850000145 4520098 TROP SB ORANGE JUICE 12 OZ 0.06354623    2 0.2700966
## 3 2500002519 3840911      MM ORANGE JUICE 12 OZ 0.05399781    3 0.2051971
## 4 4850000225 2414856 TROP SB HOME STYLE O 12 OZ 0.03394949    4 0.2773377
## 5 3828190021 2131603      DOM APPLE JUICE 12 OZ 0.02996735    5 0.1427718
## 6 1110000139 2034101 FLORIDA GOLD VALENCI 12 OZ 0.02859660    6 0.2290648
```

aggregate all stores sales and compute weighted price

```r
frj_ttl<-sku_ttl(frj)
head(frj_ttl)
```

```
## Source: local data frame [6 x 7]
## Groups: upc [1]
## 
##        upc  week ttlsales prom_n store_n ttlmv  w_price
##      (dbl) (int)    (dbl)  (dbl)   (int) (int)    (dbl)
## 1 1.11e+09     1  2458.80      0      70  1506 1.632669
## 2 1.11e+09     2  7805.20      0      69  4698 1.661388
## 3 1.11e+09     3 24744.78     71      71 19182 1.290000
## 4 1.11e+09     4  4333.11     73      73  3359 1.290000
## 5 1.11e+09     5  5868.65      0      73  3535 1.660156
## 6 1.11e+09     6 17325.99     68      68 13431 1.290000
```


### modeling (for one sku)

```r
i=2
tempfrj<-sub_sku(frj_ttl,frj_agg,i)
```


```r
tempfrj<-prom_freq(4, tempfrj)  #compute average promotion frequency
tempfrj<-subset(tempfrj, ttlsales<200000) #for i=2
head(tempfrj)
```

```
##        upc week ttlsales prom_n store_n ttlmv  w_price promfreq
## 1 4.85e+09    1  5378.94      0      71  3021 1.780516       NA
## 2 4.85e+09    2  4571.54      0      69  2576 1.774666       NA
## 3 4.85e+09    3  5302.45      0      71  2985 1.776365       NA
## 4 4.85e+09    4  5466.45      0      73  3060 1.786422     0.00
## 5 4.85e+09    5 23899.86     73      73 17929 1.333028     0.25
## 6 4.85e+09    6  6157.27      0      68  3393 1.814698     0.25
```


```r
regfrj<-find_reg_price(frj, frj_agg, i, 12) #find regular price 
tempfrj <-merge(tempfrj, regfrj, by="week") # combine all predictors
tempfrj<-reference_price(tempfrj, 0.6) # compute reference price, ref/w_price, w_price/regularp_rice
tempfrj <- last_prom(frj, frj_agg,tempfrj)
tempfrj <- promotion_factor(tempfrj) 
head(tempfrj,20)
```


