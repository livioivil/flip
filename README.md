
# Welcome to the dev-version of the 

#[library flip on CRAN](http://cran.r-project.org/web/packages/flip/index.html)


* * *

## Set up

To **install** this github version type (in R):

    #if devtools is not installed yet: 
    # install.packages("devtools") 
    library(devtools)
    install_github("flip", "livioivil")


* * *

## Some examples

```
library(flip)
```
An univarite analysis
```
set.seed(1)
y=rnorm(10)+.5
res=flip(y)
summary(res)
plot(res) # same ad hist(res)
```

![alt text](https://github.com/livioivil/flip/tree/master/figures/hist.png)

A multivarite analysis
```
set.seed(1)
df=data.frame(y1=rnorm(10)+.5,y2=rnorm(10))
res=flip(~.,data=df)
summary(res)
plot(res) 
```

![alt text](https://github.com/livioivil/flip/tree/master/figures/scatter.png)

```
# which is different from
##set the following if you get an error (mostly using Rstudio)
#par(mar=c(1,1,1,1))
hist(res)
```

![alt text](https://github.com/livioivil/flip/tree/master/figures/hists.png)

* * *

## References

For the general framework of univariate and multivariate permutation tests see: 

*Pesarin, F. (2001) Multivariate Permutation Tests with Applications in Biostatistics. Wiley, New York.*


For analysis of mixed-models see:

*L. Finos and D. Basso (2014) Permutation Tests for Between-Unit Fixed Effectsin Multivariate Generalized Linear Mixed Models. Statistics and Computing. Volume 24, Issue 6, pp 941-952. DOI: 10.1007/s11222-013-9412-6*

*D. Basso, L. Finos (2011) Exact Multivariate Permutation Tests for Fixed Effects in Mixed-Models. Communications in Statistics - Theory and Methods. DOI 10.1080/03610926.2011.627103*


For Rotation tests see: 

*Langsrud, O. (2005) Rotation tests, Statistics and Computing, 15, 1, 53-60*

*A. Solari, L. Finos, J.J. Goeman (2014) Rotation-based multiple testing in the multivariate linear model. Biometrics. Accepted*
