<!-- README.md is generated from README.Rmd. Please edit that file -->

#tableMatrix

`tableMatrix` package provides structures to store data. First one is tableList which only serves to wrap data.table and additional structures together, second is tableMatrix that combines useful features of data.table and matrix to gain faster access to data.

##Installation
From github:

```r
# install.packages("devtools")
devtools::install_github("InferenceTechnologies/tableMatrix")
```

##Motivation

###tableList
It is needed to wrap a data.table object and other structures together and preserve data.table behaviour.

####Example
Dataset and linear model stored in tableList.


```r
data(chickwts)
tl <- tableList(chickwts, lm(weight~feed, chickwts))

mean(tl[feed=="casein", weight])
#> [1] 323.5833

aid(tl)
#> 
#> Call:
#> lm(formula = weight ~ feed, data = chickwts)
#> 
#> Coefficients:
#>   (Intercept)  feedhorsebean    feedlinseed   feedmeatmeal    feedsoybean  
#>       323.583       -163.383       -104.833        -46.674        -77.155  
#> feedsunflower  
#>         5.333
```

###tableMatrix
Lets have dataset which consists of two parts - metadata columns (any types) and main data columns (only one type). If whole data were stored as data.table (data.frame), access to main part would be much slower (slow indexing in data.table). 
`tableMatrix` is result for this. It combines best of data.table (access via bracket to metadata part) and matrix. It stores dimensions of main part and effectively use this information while using multiple datasets which can have different dimensions of main data. It also can store additional structures.

####Example
Solving simple task - recreating pictures from data with `tableMatrix`. Used datasets (images8By8, images15By15) are bitmaps. 


```r
data(images8By8)
colnames(images8By8)
#>  [1] "direction" "dimX"      "dimY"      "pixel1"    "pixel2"   
#>  [6] "pixel3"    "pixel4"    "pixel5"    "pixel6"    "pixel7"   
#> [11] "pixel8"    "pixel9"    "pixel10"   "pixel11"   "pixel12"  
#> [16] "pixel13"   "pixel14"   "pixel15"   "pixel16"   "pixel17"  
#> [21] "pixel18"   "pixel19"   "pixel20"   "pixel21"   "pixel22"  
#> [26] "pixel23"   "pixel24"   "pixel25"   "pixel26"   "pixel27"  
#> [31] "pixel28"   "pixel29"   "pixel30"   "pixel31"   "pixel32"  
#> [36] "pixel33"   "pixel34"   "pixel35"   "pixel36"   "pixel37"  
#> [41] "pixel38"   "pixel39"   "pixel40"   "pixel41"   "pixel42"  
#> [46] "pixel43"   "pixel44"   "pixel45"   "pixel46"   "pixel47"  
#> [51] "pixel48"   "pixel49"   "pixel50"   "pixel51"   "pixel52"  
#> [56] "pixel53"   "pixel54"   "pixel55"   "pixel56"   "pixel57"  
#> [61] "pixel58"   "pixel59"   "pixel60"   "pixel61"   "pixel62"  
#> [66] "pixel63"   "pixel64"
dim(images8By8)
#> [1] 90 67
data(images15By15)
dim(images15By15)
#> [1]  90 228

tm <- tableMatrix(list(images15By15, images8By8),
list(1:3, 1:3), list(c(4:ncol(images15By15)),c(4:ncol(images8By8))), list(c(15,15), c(8,8)))

#metadata
tab(tm)
#>      tm.matN tm.matRow direction dimX dimY
#>   1:       1         1      down   15   15
#>   2:       1         2      down   15   15
#>   3:       1         3      down   15   15
#>   4:       1         4      down   15   15
#>   5:       1         5      down   15   15
#>  ---                                      
#> 176:       2        86      both    8    8
#> 177:       2        87      both    8    8
#> 178:       2        88      both    8    8
#> 179:       2        89      both    8    8
#> 180:       2        90      both    8    8

#dimensions of main data
matDim(tm)
#>    tm.matN dim1 dim2
#> 1:       1   15   15
#> 2:       2    8    8

#additional structures - now empty
aid(tm)
#> list()

length(mat(tm))
#> [1] 2

#subsetting via bracket passed to metadata part
tabSub <- tm[.(2)][direction=="both"]

length(mat(tabSub))
#> [1] 1

#recreating heat map
imageMean <- colMeans(mat(tabSub,1))

dim(imageMean) <- getRowDim(tabSub,1)

image(imageMean)
```

![plot of chunk unnamed-chunk-4](figures/README-unnamed-chunk-4-1.png)

