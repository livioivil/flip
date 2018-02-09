#' The library is devoted to permutation-based inferential methods.
#' 
#' It implements many univariate and multivariate permutation (and rotation)
#' tests.
#' 
#' The tests comprised are: the one and two samples, ANOVA, linear models, Chi
#' Squared test, rank tests (i.e. Wilcoxon, Mann-Whitney, Kruskal-Wallis),
#' Kolmogorov-Smirnov and Anderson-Darling.
#' 
#' Test on Linear Models are performed also in presence of covariates (i.e.
#' nuisance parameters).
#' 
#' The permutation and the rotation method to get the null distribution of the
#' test statistic(s) are available.
#' 
#' It also implements methods for multiplicity control such as Westfall-Young
#' min-p procedure and Closed Testing (Marcus, 1976).
#' 
#' \tabular{ll}{ Package: \tab flip\cr Type: \tab Package\cr Version: \tab
#' 1.1\cr Date: \tab 2012-02-05\cr License: \tab GPL <=2\cr LazyLoad: \tab
#' yes\cr Depends: \tab methods, e1071, someMTP, cherry\cr }
#' 
#' @name flip-package
#' @docType package
#' @author Livio Finos, with contributions by Florian Klinglmueller, Dario
#' Basso, Aldo Solari, Lucia Benetazzo, Jelle Goeman and Marco Rinaldo. Special
#' thanks are due to Ivan Marin-Franch and Fredrik Nilsson for the debugging
#' and the good questions.
#' 
#' Maintainer: livio finos <livio@@stat.unipd.it>
#' @references For the general framework of univariate and multivariate
#' permutation tests see: Pesarin, F. (2001) Multivariate Permutation Tests
#' with Applications in Biostatistics. Wiley, New York.
#' 
#' For analysis of mixed-models see: L. Finos and D. Basso (2014) Permutation
#' Tests for Between-Unit Fixed Effects in Multivariate Generalized Linear
#' Mixed Models. Statistics and Computing. Vo- lume 24, Issue 6, pp 941-952.
#' DOI: 10.1007/s11222-013-9412-6 J. J. Goeman and
#' 
#' D. Basso, L. Finos (2011) Exact Multivariate Permutation Tests for Fixed
#' Effec- ts in Mixed-Models. Communications in Statistics - Theory and
#' Methods. DOI 10.1080/03610926.2011.627103
#' 
#' For Rotation tests see: Langsrud, O. (2005) Rotation tests, Statistics and
#' Computing, 15, 1, 53-60
#' 
#' A. Solari, L. Finos, J.J. Goeman (2014) Rotation-based multiple testing in
#' the multivariate linear model. Biometrics. Accepted
#' 
#' The structure of \code{flip} is widely borrowed from library
#' \code{globaltest} by J. Goeman and J. Oosting in bioconductor.org.
#' @keywords package
#' @importFrom Rcpp evalCpp
#' @useDynLib flip
#' @examples
#' 
#' 
#' Y=data.frame(matrix(rnorm(50),10,5))
#' names(Y)=LETTERS[1:5]
#' Y[,1:2]=Y[,1:2]
#' x=rep(0:1,5)
#' data=data.frame(x=x, Z=rnorm(10))
#' res = flip(Y+matrix(x*2,10,5),~x,~Z,data=data)
#' res
#' 
#' plot(res)
#' 
#' p2=npc(res,"fisher",subsets=list(c1=c("A","B"),c2=names(Y)))
#' p2
#' 
NULL



#' @useDynLib your-package-name
#' @importFrom Rcpp sourceCpp
NULL


#' Seeds data
#' 
#' Famous seeds growing data from Pesarin, F. (2001) Multivariate Permutation
#' Tests with Applications in Biostatistics. Wiley, New York.
#' 
#' 
#' @format the data.frame contains the three columns: grs, x, y
#' @keywords htest
#' @name seeds
#' 
NULL



