
# PSIndependenceTest: Independence Tests for Two-Way, Three-Way and Four-Way Contingency Tables

**author: Piotr Sulewski, Pomeranian University** <!-- badges: start -->
<!-- badges: end -->

The goal of the package is to put into practice the modular and
logarithmic minimum tests for independence in two-way, three-way and
four-way contingency tables. Statistic value, cv value and p-value are
calculated. This package also includes three table generation functions
and six data sets. To read more about the package please see (and cite
:)) papers:

Sulewski, P. (2016). Moc testów niezależności w tablicy dwudzielczej
większej niż 2×2. Przegląd statystyczny 63(2), 190-210.

Sulewski, P. (2019). The LMS for Testing Independence in Two-way
Contingency Tables. Biometrical Letters 56(1), 17-43.

Sulewski, P. (2018). Power Analysis Of Independence Testing for the
Three-Way Con-tingency Tables of Small Sizes. Journal of Applied
Statistics 45(13), 2481-2498.

Sulewski, P. (2021). Logarithmic Minimum Test for Independence in Three
Way Con-tingency Table of Small Sizes. Journal of Statistical
Computation and Simulation 91(13), 2780-2799.

## Installation

You can install the released version of **PSIndependenceTest** from CRAN
with:

``` r
install.packages("PSIndependenceTest")
```

You can install the development version of **PSIndependenceTest** from
[GitHub](https://github.com/) with:

``` r
library("remotes")
remotes::install_github("PiotrSule/PSIndependenceTest")
```

**This package includes four data sets**

The first one, **table1**, consist of 40 observations presented as
two-way contingency table 2 x 2. See details: Sulewski, P. (2017). A new
test for independence in 2x2 contingency tables. Acta Universitatis
Lodziensis. Folia Oeconomica 4(330), 55–75.

The second one, **table2**, consist of 25 observations described the
effect of a treatment for rheumatoid arthritis vs. a placebo presented
as two-way contingency table 2 x 3. See details: Sulewski, P. (2019).
The LMS for Testing Independence in Two-way Contingency Tables.
Biometrical Letters 56 (1), 17-43.

The third one, **table3**, consist of 695 observations described the
frequency of watching videos at home or at friends’ homes for young
people between 7 and 15 years of age, cross-classified according to age
and sex. Data are presented as three-way contingency table 3 x 3 x 2.
See details: Sulewski, P. (2021). Logarithmic Minimum Test for
Independence in Three Way Contingency Table of Small Sizes. Journal of
Statistical Computation and Simulation 91(13), 2780-2799

The fourth one, **table4**, consist of 100 observations obtained using
the Monte Carlo method when Ho is true, i.e. all probabilities pijt
equal 1/24. Data is presented as three-way contingency table 2 x 3 x 4.

The fifth one, **table5**,provides information on the fate of passengers
on the fatal maiden voyage of the ocean liner ‘Titanic’, summarized
according to economic status (class), sex, age and survival. Data is
presented as four-way contingency table 4 x 2 x 2 x 2. The sample size
equals 2201.

The sixth one, **table6**, consist of 100 observations obtained using
the Monte Carlo method when Ho is true, i.e. all probabilities pijtu
equal 1/16. Data is presented as four-way contingency table 2 x 2 x 2 x
2.

``` r
library(PSIndependenceTest)
dim(table1)
#> [1] 2 2
length(table2)
#> [1] 6
```

### Functions

**GenTab2**

This function generating the two-way contingency table with the Monte
Carlo method

``` r
GenTab2(matrix(1/8, nrow = 2, ncol = 4), 50)
#>      [,1] [,2] [,3] [,4]
#> [1,]    5    4    5    5
#> [2,]    6    6    8   11
GenTab2(matrix(1/12, nrow = 4, ncol = 3), 60)
#>      [,1] [,2] [,3]
#> [1,]    6    8    7
#> [2,]    1    2    5
#> [3,]    4    9    3
#> [4,]    3    5    7
```

**Mod2.stat**

This function returns the statistic value of the modular independence
test in the two-way contingency table.

``` r
Mod2.stat(table1)
#> [1] 1.273572
Mod2.stat(GenTab2(matrix(1/9, nrow = 3, ncol = 3), 90))
#> [1] 1.149439
```

**Mod2.cv**

This function returns the critical value of the modular independence
test in the two-way contingency table.

``` r
Mod2.cv(2, 2, 40, 0.05, B = 1e3) 
#> [1] 1.265177
Mod2.cv(2, 3, 60, 0.1) 
#> [1] 1.542129
```

**Mod2.pvalue**

This function returns the p-value of the modular independence test in
the two-way contingency table.

``` r
Mod2.pvalue(Mod2.stat(table1), 2, 2, 40, B = 1e3)
#> [1] 0.04695305
Mod2.pvalue(Mod2.stat(table2), 2, 3, 60)
#> [1] 0.6931307
```

**Mod2.test**

This function returns the test statistic and p-value of the logarithmic
minimum independence test in the two-way contingency table.

``` r
Mod2.test(table1, B = 1e3)
#> 
#>  Modular test for independence in two-way contingency table
#> 
#> data:  table1
#> D = 1.2736, p-value = 0.0488
Mod2.test(table2)
#> 
#>  Modular test for independence in two-way contingency table
#> 
#> data:  table2
#> D = 0.61103, p-value = 0.7001
```

**Lms2.stat**

This function returns the statistic value of the logarithmic minimum
independence test in the two-way contingency table.

``` r
Lms2.stat(table1)
#> [1] 1.660665
Lms2.stat(GenTab2(matrix(1/9, nrow = 3, ncol = 3), 90))
#> [1] 1.759932
```

**Lms2.cv**

This function returns the critical value of the logarithmic minimum
independence test in the two-way contingency table.

``` r
Lms2.cv(2, 2, 40, 0.05, B = 1e3) 
#> [1] 1.34987
Lms2.cv(2, 3, 60, 0.1)  
#> [1] 1.605235
```

**Lms2.pvalue**

This function returns the p-value of the logarithmic minimum
independence test in the two-way contingency table.

``` r
Lms2.pvalue(Lms2.stat(table1), 2, 2, 40, B = 1e3)
#> [1] 0.008991009
Lms2.pvalue(Lms2.stat(table2), 2, 3, 60)
#> [1] 0.7009299
```

**Lms2.test**

This function returns the test statistic and p-value of the logarithmic
minimum independence test in the two-way contingency table.

``` r
Lms2.test(table1, B = 1e3)
#> 
#>  Modular test for independence in two-way contingency table
#> 
#> data:  table1
#> D = 1.6607, p-value = 0.0176
Lms2.test(table2)
#> 
#>  Modular test for independence in two-way contingency table
#> 
#> data:  table2
#> D = 0.61437, p-value = 0.7059
```

**GenTab3**

This function generating the three-way contingency table with the Monte
Carlo method

``` r
GenTab3(array(1/12, dim=c(2,2,3)), 60)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    5    5
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    8    5
#> [2,]    6    5
#> 
#> , , 3
#> 
#>      [,1] [,2]
#> [1,]    5    4
#> [2,]    0    5
GenTab3(array(1/18, dim=c(2,3,3)), 80)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    5    8    0
#> [2,]    4    5    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    3    3    6
#> [2,]    5    2    4
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   10    3    6
#> [2,]    5    3    2
```

**Mod3.stat**

This function returns the statistic value of the modular independence
test in the three-way contingency table.

``` r
Mod3.stat(table3)
#> [1] 2.641208
Mod3.stat(GenTab3(array(1/12, dim=c(2,2,3)), 120))
#> [1] 1.890008
```

**Mod3.cv**

This function returns the critical value of the modular independence
test in the three-way contingency table.

``` r
Mod3.cv(2, 2, 2, 80, 0.05, B = 1e3) 
#> [1] 2.380025
Mod3.cv(2, 2, 2, 80, 0.1) 
#> [1] 2.181818
```

**Mod3.pvalue**

This function returns the p-value of the modular independence test in
the three-way contingency table.

``` r
Mod3.pvalue(Mod3.stat(table4), 2, 2, 2, 80, B = 1e3)
#> [1] 0.02397602
Mod3.pvalue(Mod3.stat(table4), 2, 2, 2, 80)
#> [1] 0.02409759
```

**Mod3.test**

This function returns the test statistic and p-value of the modular
independence test in the three-way contingency table.

``` r
Mod3.test(table4, B = 1e2)
#> 
#>  Modular test for independence in three-way contingency table
#> 
#> data:  table4
#> D = 2.5886, p-value = 0.0265
Mod3.test(table4, B = 1e3)
#> 
#>  Modular test for independence in three-way contingency table
#> 
#> data:  table4
#> D = 2.5886, p-value = 0.0283
```

**Lms3.stat**

This function returns the statistic value of the logarithmic minimum
independence test in the three-way contingency table.

``` r
Lms3.stat(table3)
#> [1] 2.712789
Lms3.stat(GenTab3(array(1/12, dim=c(2,2,3)), 120))
#> [1] 3.960989
```

**Lms3.cv**

This function returns the critical value of the logarithmic minimum
independence test in the three-way contingency table.

``` r
Lms3.cv(2, 2, 2, 80, 0.05, B = 1e2) 
#> [1] 2.775388
Lms3.cv(2, 2, 2, 80, 0.1, B = 1e3) 
#> [1] 2.313046
```

**Lms3.pvalue**

This function returns the p-value of the logarithmic minimum
independence test in the three-way contingency table.

``` r
Lms3.pvalue(Lms3.stat(table4), 2, 2, 2, 80, B = 1e3)
#> [1] 0.04095904
Lms3.pvalue(Lms3.stat(table4), 2, 2, 2, 80)
#> [1] 0.04019598
```

**Lms3.test**

This function returns the test statistic and p-value of the logarithmic
minimum independence test in the three-way contingency table.

``` r
Lms3.test(table4, B = 1e2)
#> 
#>  Logarithmic minimum test for independence in three-way contingency
#>  table
#> 
#> data:  table4
#> D = 2.6637, p-value = 0.04
Lms3.test(table4, B = 1e3)
#> 
#>  Logarithmic minimum test for independence in three-way contingency
#>  table
#> 
#> data:  table4
#> D = 2.6637, p-value = 0.0371
```

**GenTab4**

This function generating the four-way contingency table with the Monte
Carlo method.

``` r
GenTab4(array(1/16, dim=c(2,2,2,2)), 100)
#> , , 1, 1
#> 
#>      [,1] [,2]
#> [1,]    7    7
#> [2,]    9    9
#> 
#> , , 2, 1
#> 
#>      [,1] [,2]
#> [1,]   10    8
#> [2,]    5    4
#> 
#> , , 1, 2
#> 
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    5    5
#> 
#> , , 2, 2
#> 
#>      [,1] [,2]
#> [1,]    1   11
#> [2,]    5   10
GenTab4(array(1/36, dim=c(2,3,2,3)), 150)
#> , , 1, 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    2    3    4
#> [2,]    5    4    4
#> 
#> , , 2, 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    5    7    6
#> [2,]    9    3    6
#> 
#> , , 1, 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    4    5
#> [2,]    2    5    2
#> 
#> , , 2, 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    6    4
#> [2,]    4    7    1
#> 
#> , , 1, 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    6    1    2
#> [2,]    5    1    4
#> 
#> , , 2, 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    5    3
#> [2,]    3    4    3
```

**Mod4.stat**

This function returns the statistic value of the modular independence
test in the four-way contingency table.

``` r
Mod4.stat(table5)
#> [1] 46.70092
Mod4.stat(table6)
#> [1] 3.7301
```

**Mod4.cv**

This function returns the critical value of the modular independence
test in the four-way contingency table.

``` r
Mod4.cv(2, 2, 2, 2, 160, 0.05, B = 1e2)
#> [1] 4.395262
Mod4.cv(2, 2, 2, 2, 160, 0.1, B = 1e3)
#> [1] 4.425875
```

**Mod4.pvalue**

This function returns the p-value of the modular independence test in
the four-way contingency table.

``` r
Mod4.pvalue(Mod4.stat(table6), 2, 2, 2, 2, 160, B = 1e2)
#> [1] 0.3168317
Mod4.pvalue(Mod4.stat(table6), 2, 2, 2, 2, 160, B = 1e3)
#> [1] 0.3426573
```

**Mod4.test**

This function returns the test statistic and p-value of the modular
independence test in the -way contingency table.

``` r
Mod4.test(table6, B = 1e2)
#> 
#>  Modular test for independence in four-way contingency table
#> 
#> data:  table6
#> D = 3.7301, p-value = 0.3101
Mod4.test(table6, B = 1e3)
#> 
#>  Modular test for independence in four-way contingency table
#> 
#> data:  table6
#> D = 3.7301, p-value = 0.3111
```

**Lms4.stat**

This function returns the statistic value of the logarithmic minimum
independence test in the four-way contingency table.

``` r
Lms4.stat(table5)
#> [1] 116.0267
Lms4.stat(table6)
#> [1] 4.097161
```

**Lms4.cv**

This function returns the critical value of the logarithmic minimum
independence test in the four-way contingency table.

``` r
Lms4.cv(2, 2, 2, 2, 160, 0.05, B = 1e2)
#> [1] 5.321681
Lms4.cv(2, 2, 2, 2, 160, 0.1, B = 1e3)
#> [1] 4.764324
```

**Lms4.pvalue**

This function returns the p-value of the logarithmic minimum
independence test in the four-way contingency table.

``` r
Lms4.pvalue(Lms4.stat(table6), 2, 2, 2, 2, 160, B = 1e2)
#> [1] 0.2475248
Lms4.pvalue(Lms4.stat(table6), 2, 2, 2, 2, 160, B = 1e3)
#> [1] 0.2597403
```

**Lms4.test**

This function returns the test statistic and p-value of the logarithmic
minimum independence test in the four-way contingency table.

``` r
Lms4.test(table6, B = 1e2)
#> 
#>  Logarithmic minimum test for independence in four-way contingency table
#> 
#> data:  table6
#> D = 4.0972, p-value = 0.248
Lms4.test(table6, B = 1e3)
#> 
#>  Logarithmic minimum test for independence in four-way contingency table
#> 
#> data:  table6
#> D = 4.0972, p-value = 0.2513
```
