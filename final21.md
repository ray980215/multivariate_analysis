Final Exam, Multivariate Analysis, Spring 2021
================
309657009 邱泓儒
2021/6/10

``` r
library(mclust)
library(cluster)
library(MASS)
library(fBasics)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(nnet)
library(e1071)
```

``` r
setwd("C:/Users/ray98/Desktop/project/class/multivariate")
train <- read.csv("etctrain.csv", header=T)
test <- read.csv("etctest.csv", header=T)
head(train)
```

    ##    t1  t2  t3  t4  t5  t6  t7   t8   t9  t10  t11  t12  t13  t14  t15  t16  t17
    ## 1 315 188 124 107 133 204 479 1329 1442 1176 1015 1061 1117 1355 1686 1905 2295
    ## 2 445 310 213 164 165 251 383  873 1034 1110 1154 1292 1303 1514 1825 2285 2570
    ## 3 714 487 311 209 231 268 359  668  906 1137 1585 1958 1945 2205 2686 2749 2950
    ## 4 559 247 126  89 135 296 859 1846 1808 1291 1137 1133 1141 1407 1568 1791 2151
    ## 5 312 176 127 107 138 227 524 1352 1473 1150 1005  994 1076 1403 1591 1791 2168
    ## 6 281 212 149 113 144 209 508 1366 1462 1131 1011 1103 1079 1385 1633 1822 2257
    ##    t18  t19  t20  t21  t22  t23  t24 group
    ## 1 2624 1956 1577 1361 1168  844  603     4
    ## 2 2750 2428 1967 1962 1943 1562 1080     5
    ## 3 2979 2309 2052 2361 2520 1948  989     1
    ## 4 2235 1835 1288 1054  935  720  427     2
    ## 5 2277 1821 1325 1039  917  641  451     3
    ## 6 2325 1725 1281 1012  972  792  427     3

我們所要分析的資料中，每一觀察值(列)(observation(row))代表自某一天所量得的車流量值，共包含25個變數(variable)。變數t1、t2、、t24，分別代表每天0時-1時、1時-2時、、23時-24時，通過該ETC門架的車輛總數。變數group代表觀察值所屬的星期別：1為星期天，2為星期一，3為星期二、三或四，4為星期五，5為星期六。

# Question 1.multivariate mean inferences

**To examine the traffic flow differences among different week groups,
one can do the multivariate mean inferences on the etctrain dataset.**

## (a)

**Use the one-way MANOVA to examine the overall traffic flow differences
among differentweek groups. Also, perform the one-way ANOVA on each “t”
variable (24 variables in total) for thedifference in a specific time
point.**

``` r
# monava
fit <- manova(as.matrix(train[, 1:24]) ~ as.factor(train[, 25]))
summary(fit)
```

    ##                         Df Pillai approx F num Df den Df    Pr(>F)    
    ## as.factor(train[, 25])   4 2.7981   30.071     96   1240 < 2.2e-16 ***
    ## Residuals              330                                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# anova
pv <- c()
for(i in 1:24){
  fit <- aov(train[,i] ~ as.factor(train[,25]))
  pv <- c(pv, summary(fit)[[1]][1,5])
}
```

## (b)

**What do these tests tell you about traffic flow differences in five
week groups in the MANOV Aanalysis? Since we need to perform the test
for multiple time points simultaneously in the ANOVA analysis, what is
the cut-off for the p-value, below which can be considered to be
significant? How manytime points have significant differences?**

![H\_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0
"H_0") of MANOVA test is
![\\mathbf{\\mu\_1}=\\mathbf{\\mu\_2}=\\mathbf{\\mu\_3}=\\mathbf{\\mu\_4}=\\mathbf{\\mu\_5}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%7B%5Cmu_1%7D%3D%5Cmathbf%7B%5Cmu_2%7D%3D%5Cmathbf%7B%5Cmu_3%7D%3D%5Cmathbf%7B%5Cmu_4%7D%3D%5Cmathbf%7B%5Cmu_5%7D
"\\mathbf{\\mu_1}=\\mathbf{\\mu_2}=\\mathbf{\\mu_3}=\\mathbf{\\mu_4}=\\mathbf{\\mu_5}"),where
![\\mathbf{\\mu\_i}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%7B%5Cmu_i%7D
"\\mathbf{\\mu_i}") is the mean vector of group i

since p-value=2.2e-16, reject
![H\_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0
"H_0"), there is no evidence that
![\\mathbf{\\mu\_1}=\\mathbf{\\mu\_2}=\\mathbf{\\mu\_3}=\\mathbf{\\mu\_4}=\\mathbf{\\mu\_5}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%7B%5Cmu_1%7D%3D%5Cmathbf%7B%5Cmu_2%7D%3D%5Cmathbf%7B%5Cmu_3%7D%3D%5Cmathbf%7B%5Cmu_4%7D%3D%5Cmathbf%7B%5Cmu_5%7D
"\\mathbf{\\mu_1}=\\mathbf{\\mu_2}=\\mathbf{\\mu_3}=\\mathbf{\\mu_4}=\\mathbf{\\mu_5}")

![H\_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0
"H_0") of ANOVA test is
![\\mu\_1=\\mu\_2=\\mu\_3=\\mu\_4=\\mu\_5](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_1%3D%5Cmu_2%3D%5Cmu_3%3D%5Cmu_4%3D%5Cmu_5
"\\mu_1=\\mu_2=\\mu_3=\\mu_4=\\mu_5"),where
![\\mu\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_i
"\\mu_i") is the mean of group i

since there are 24 ANOVA tests,the adjusted cut-off for the p-value is
![0.05/24](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0.05%2F24
"0.05/24")

``` r
alpha <- 0.05 / 24
sum(pv < alpha)
```

    ## [1] 24

all 24 time points have significant differences in five groups

# Question 2 principal component analysis

**With the etctrain dataset, perform the principal component analysis
(PCA) on 24 “t” variables**

## (a)

**For PCA on the covariance matrix, how many principal components (PCs)
are needed? Whatdo the PC loadings tell you about the PCs you select?**

``` r
# PCA on the covariance matrix
princov <- princomp(train[, -25], cor=F)
summary(princov)
```

    ## Importance of components:
    ##                             Comp.1      Comp.2       Comp.3       Comp.4
    ## Standard deviation     1653.444382 434.7301835 424.15127405 293.95544591
    ## Proportion of Variance    0.808468   0.0558886   0.05320166   0.02555328
    ## Cumulative Proportion     0.808468   0.8643566   0.91755821   0.94311149
    ##                              Comp.5       Comp.6       Comp.7       Comp.8
    ## Standard deviation     219.60357395 1.745387e+02 1.473492e+02 1.345507e+02
    ## Proportion of Variance   0.01426141 9.008804e-03 6.420652e-03 5.353723e-03
    ## Cumulative Proportion    0.95737290 9.663817e-01 9.728024e-01 9.781561e-01
    ##                              Comp.9      Comp.10      Comp.11      Comp.12
    ## Standard deviation     122.86909932 1.079361e+02 92.393331164 89.164892798
    ## Proportion of Variance   0.00446446 3.445222e-03  0.002524439  0.002351102
    ## Cumulative Proportion    0.98262054 9.860658e-01  0.988590200  0.990941302
    ##                             Comp.13      Comp.14      Comp.15      Comp.16
    ## Standard deviation     84.525534185 74.382062570 64.853165805 59.424154218
    ## Proportion of Variance  0.002112805  0.001636138  0.001243787  0.001044262
    ## Cumulative Proportion   0.993054107  0.994690245  0.995934032  0.996978295
    ##                             Comp.17      Comp.18      Comp.19      Comp.20
    ## Standard deviation     5.394499e+01 4.749183e+01 4.171564e+01 3.678681e+01
    ## Proportion of Variance 8.605692e-04 6.669933e-04 5.146138e-04 4.001915e-04
    ## Cumulative Proportion  9.978389e-01 9.985059e-01 9.990205e-01 9.994207e-01
    ##                             Comp.21      Comp.22      Comp.23      Comp.24
    ## Standard deviation     2.938415e+01 2.264823e+01 1.812727e+01 1.594028e+01
    ## Proportion of Variance 2.553348e-04 1.516883e-04 9.717365e-05 7.514077e-05
    ## Cumulative Proportion  9.996760e-01 9.998277e-01 9.999249e-01 1.000000e+00

取一個pc即可以解釋超過80%的變異，再多取也不會改善非常多。沒有特別大的loading，表示變數(time points)間重要性差不多

## (b)

**For PCA on the correlation matrix, how many principal components (PCs)
are needed? Whatdo the PC loadings tell you about the PCs you select?**

``` r
# PCA on the correlation matrix
princor <- princomp(train[, -25], cor=T)
summary(princor)
```

    ## Importance of components:
    ##                           Comp.1    Comp.2     Comp.3     Comp.4     Comp.5
    ## Standard deviation     4.0397858 1.8353744 1.15362927 0.78761637 0.64355998
    ## Proportion of Variance 0.6799946 0.1403583 0.05545252 0.02584748 0.01725706
    ## Cumulative Proportion  0.6799946 0.8203529 0.87580538 0.90165286 0.91890992
    ##                           Comp.6     Comp.7      Comp.8      Comp.9     Comp.10
    ## Standard deviation     0.5981069 0.56693136 0.443582760 0.400457978 0.391121219
    ## Proportion of Variance 0.0149055 0.01339213 0.008198569 0.006681941 0.006373992
    ## Cumulative Proportion  0.9338154 0.94720755 0.955406121 0.962088063 0.968462055
    ##                            Comp.11     Comp.12     Comp.13    Comp.14
    ## Standard deviation     0.359973744 0.318525575 0.306777671 0.28292231
    ## Proportion of Variance 0.005399212 0.004227439 0.003921356 0.00333521
    ## Cumulative Proportion  0.973861267 0.978088706 0.982010062 0.98534527
    ##                            Comp.15     Comp.16     Comp.17     Comp.18
    ## Standard deviation     0.256605366 0.238555946 0.220335147 0.199432537
    ## Proportion of Variance 0.002743596 0.002371206 0.002022816 0.001657222
    ## Cumulative Proportion  0.988088868 0.990460074 0.992482890 0.994140112
    ##                            Comp.19     Comp.20     Comp.21      Comp.22
    ## Standard deviation     0.185606305 0.174852868 0.171038747 0.1318305684
    ## Proportion of Variance 0.001435404 0.001273897 0.001218927 0.0007241374
    ## Cumulative Proportion  0.995575516 0.996849413 0.998068340 0.9987924779
    ##                             Comp.23     Comp.24
    ## Standard deviation     0.1293834427 0.110636591
    ## Proportion of Variance 0.0006975031 0.000510019
    ## Cumulative Proportion  0.9994899810 1.000000000

取兩個pc即可以解釋超過80%的變異，再多取也不會改善非常多。沒有特別大的loading，表示變數(time points)間重要性差不多

# Question 3 clustering

## (a)

**Which approach has the best performance in clustering the days from
the same week
grouptogether?(那一個方法，最能將原本屬於同一個group的日子，經分群法後也分到同一個cluster?)**

``` r
da <- dist(train[,-25], method="euclidean")

# agglomerative hierarchical clustering with averagelinkage
fita <- hclust(da, method="average")
cuta <- cutree(fita, k=5)
table(cuta, train[,25])
```

    ##     
    ## cuta   1   2   3   4   5
    ##    1   0  37 147  45   1
    ##    2   0   0   2   1  44
    ##    3  45   0   0   0   1
    ##    4   0  11   0   0   0
    ##    5   1   0   0   0   0

正確分配的個數為147+45+44+11+0=247

正確率為247/335=0.7373134

``` r
# k-means
set.seed(66)
fitk <- kmeans(train[,-25], centers = 5)
table(fitk$cluster, train[,25])
```

    ##    
    ##      1  2  3  4  5
    ##   1 44  0  0  0  1
    ##   2  0 11 19 15  3
    ##   3  0 30 48 26  0
    ##   4  1  6 82  5  0
    ##   5  1  1  0  0 42

正確分配的個數為82+44+42+30+15=213(k-means法每次分群結果不同，但大約在這個值)

正確率為213/335=0.6358209

``` r
# model-based
fitmb <- Mclust(train[,-25], G = 5)
table(fitmb$classification, train[,25])
```

    ##    
    ##      1  2  3  4  5
    ##   1  0  0 81 38  0
    ##   2  0  7 10  7 44
    ##   3 46  0  0  0  2
    ##   4  0 41  2  0  0
    ##   5  0  0 56  1  0

正確分配的個數為81+46+44+41+1=213

正確率為213/335=0.6358209

正確率最佳為agglomerative hierarchical clustering with average linkage

## (b)

**To assess cluster fit, you are asked to create the silhouette plot for
each of the clustering approaches. Which approach has the best cluster
fit based on the average silhouette width?**

``` r
sia <- silhouette(cuta, da)
plot(sia)
```

![](final21_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
sik <- silhouette(fitk$cluster, da)
plot(sik)
```

![](final21_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
simb <- silhouette(fitmb$classification, da)
plot(simb)
```

![](final21_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

agglomerative hierarchical clustering with average linkage is the
best，since it has the largest average silhouette width

# Question 4 classification

**We want to predict observations’ week groups using the 24 “t”
variables. You are asked to evaluatethe performance of various
prediction methods.Use the etctrain dataset to train your model and the
etctest dataset to test it. The prediction methods you will evaluate are
the linear discriminant analysis, classificationand regression tree,
neural networks, and support vector machine. Which prediction method
performs best?**

``` r
# linear discriminant analysis
xname <- colnames(train)[-25]
fmla <- as.formula(paste("as.factor(group) ~ ", paste(xname, collapse= "+")))
fitlda <- lda(fmla, prior=rep(0.2, 5), data = train, na.action="na.omit")
testlda <- predict(fitlda, test[,-25])$class
table(test[,25], testlda)
```

    ##    testlda
    ##      1  2  3  4  5
    ##   1 12  0  0  0  0
    ##   2  0 20  0  0  0
    ##   3  1  1 31 11  1
    ##   4  0  0  3  8  0
    ##   5  0  0  0  0 12

``` r
accuracy <- tr(table(test[,25], testlda)) / 100
cat("misclassification rate = ", 1-accuracy, "\n")
```

    ## misclassification rate =  0.17

``` r
# classification and regression tree
fittreec <- rpart(fmla, data=train, method="class", cp=10^(-5))
printcp(fittreec)
```

    ## 
    ## Classification tree:
    ## rpart(formula = fmla, data = train, method = "class", cp = 10^(-5))
    ## 
    ## Variables actually used in tree construction:
    ## [1] t1  t13 t14 t24 t3  t8  t9 
    ## 
    ## Root node error: 186/335 = 0.55522
    ## 
    ## n= 335 
    ## 
    ##         CP nsplit rel error  xerror     xstd
    ## 1 0.228495      0  1.000000 1.00000 0.048901
    ## 2 0.225806      2  0.543011 0.68817 0.047814
    ## 3 0.155914      3  0.317204 0.35484 0.039139
    ## 4 0.026882      4  0.161290 0.22043 0.032250
    ## 5 0.016129      5  0.134409 0.18280 0.029716
    ## 6 0.013441      6  0.118280 0.16129 0.028098
    ## 7 0.000010      8  0.091398 0.16667 0.028516

``` r
cbestcp <- fittreec$cptable[which.min(fittreec$cptable[,"xerror"]),"CP"]
fittreecbest <- prune(fittreec, cp=cbestcp)
traintreec <- predict(fittreecbest, test[,-25])
getmax <- function(row){
  a <- sample(colnames(traintreec)[which(row == max(row))],1)
  return(as.numeric(a))
}
traintreec_Class <- apply(traintreec, 1, getmax)

table(test[,25], traintreec_Class)
```

    ##    traintreec_Class
    ##      1  2  3  4  5
    ##   1 11  0  0  0  1
    ##   2  0 20  0  0  0
    ##   3  0  1 36  6  2
    ##   4  0  0  2  8  1
    ##   5  0  0  0  1 11

``` r
accuracy <- tr(table(test[,25], traintreec_Class)) / 100
cat("misclassification rate = ", 1-accuracy, "\n")
```

    ## misclassification rate =  0.14

``` r
# neural networks
scaled1 <- as.matrix(scale(train[,-25]))
trains <- cbind(scaled1, train[,25])
colnames(trains) <- colnames(train)
scaled2 <- scale(test[,-25], center=attr(scaled1,"scaled:center"), 
                 scale=attr(scaled1,"scaled:scale"))
tests <- cbind(scaled2, test[,25])
colnames(tests) <- colnames(test)

a <- class.ind(trains[,25])
colnames(a) <- paste("a", colnames(a), sep="")
yname <- colnames(a)
xname <- colnames(trains)[-25]
fmlann <- as.formula(paste(paste(yname, collapse="+"), " ~ ", paste(xname, collapse="+")))
fitnn1 <- neuralnet(fmlann, data=cbind(a, trains[,-25]), 
                    hidden=c(12),
                    err.fct="ce", act.fct="logistic",
                    linear.output=FALSE)
fitnn2 <- neuralnet(fmlann, data=cbind(a, trains[,-25]), 
                    hidden=c(64, 16),
                    err.fct="ce", act.fct="logistic",
                    linear.output=FALSE)
fitnn3 <- neuralnet(fmlann, data=cbind(a, trains[,-25]), 
                    hidden=c(64, 64, 16), 
                    err.fct="ce", act.fct="logistic",
                    linear.output=FALSE)
fitnn4 <- neuralnet(fmlann, data=cbind(a, trains[,-25]), 
                    hidden=c(64, 64, 16, 16),
                    err.fct="ce", act.fct="logistic",
                    linear.output=FALSE)
prednn <- function(fitnn)
{ 
  btrain <- tests[,-25]
  prednn <- compute(fitnn, btrain)
  resind <- apply(prednn$net.result, 1, which.max)
  confusion <- table(resind, tests[,25])
  cat("Confusion table")
  print(confusion)
  accuracy <- sum(diag(confusion))/length(resind)
  cat("misclassification rate = ", 1-accuracy, "\n")
}
prednn(fitnn1)
```

    ## Confusion table      
    ## resind  1  2  3  4  5
    ##      1 11  0  1  0  0
    ##      2  0 20  1  0  0
    ##      3  0  0 35  2  1
    ##      4  0  0  6  9  0
    ##      5  1  0  2  0 11
    ## misclassification rate =  0.14

``` r
prednn(fitnn2)
```

    ## Confusion table      
    ## resind  1  2  3  4  5
    ##      1 12  0  0  0  0
    ##      2  0 16  1  0  0
    ##      3  0  4 34  2  1
    ##      4  0  0 10  9  0
    ##      5  0  0  0  0 11
    ## misclassification rate =  0.18

``` r
prednn(fitnn3)
```

    ## Confusion table      
    ## resind  1  2  3  4  5
    ##      1  7  0  0  0  0
    ##      2  0 17  2  1  0
    ##      3  1  3 34  1  1
    ##      4  0  0  9  9  0
    ##      5  4  0  0  0 11
    ## misclassification rate =  0.22

``` r
prednn(fitnn4)
```

    ## Confusion table      
    ## resind  1  2  3  4  5
    ##      1 12  0  1  0  0
    ##      2  0 19  6  3  0
    ##      3  0  1 34  1  1
    ##      4  0  0  4  7  0
    ##      5  0  0  0  0 11
    ## misclassification rate =  0.17

``` r
# support vector machine
fitsvml <- svm(fmla, kernel="linear", cross=10, data=trains)
fitsvmp <- svm(fmla, kernel="polynomial", cross=10, data=trains)
fitsvmr <- svm(fmla, kernel="radial", cross=10, data=trains)
fitsvms <- svm(fmla, kernel="sigmoid", cross=10, data=trains)

predsvm <- function(fitsvm)
{ 
  trainsvm <- predict(fitsvm, tests[,-25])
  confusion <- table(tests[,25], trainsvm)
  cat("Confusion table")
  print(confusion)
  accuracy <- tr(table(tests[,25], trainsvm)) / 100
  cat("misclassification rate = ", 1-accuracy, "\n")
}

predsvm(fitsvml)
```

    ## Confusion table   trainsvm
    ##      1  2  3  4  5
    ##   1 10  0  0  0  2
    ##   2  0 20  0  0  0
    ##   3  0  1 41  3  0
    ##   4  0  0  3  8  0
    ##   5  0  0  1  0 11
    ## misclassification rate =  0.1

``` r
predsvm(fitsvmp)
```

    ## Confusion table   trainsvm
    ##      1  2  3  4  5
    ##   1 10  0  1  0  1
    ##   2  0 18  2  0  0
    ##   3  0  1 43  1  0
    ##   4  0  0 11  0  0
    ##   5  0  0  3  0  9
    ## misclassification rate =  0.2

``` r
predsvm(fitsvmr)
```

    ## Confusion table   trainsvm
    ##      1  2  3  4  5
    ##   1 12  0  0  0  0
    ##   2  0 20  0  0  0
    ##   3  0  1 44  0  0
    ##   4  0  0  8  3  0
    ##   5  0  0  1  0 11
    ## misclassification rate =  0.1

``` r
predsvm(fitsvms)
```

    ## Confusion table   trainsvm
    ##      1  2  3  4  5
    ##   1  8  0  0  0  4
    ##   2  0 20  0  0  0
    ##   3  0  1 43  1  0
    ##   4  0  0 11  0  0
    ##   5 10  0  1  0  1
    ## misclassification rate =  0.28

若以錯分率為標準，support vector machine with linear/radial kernel 為最佳預測方法
