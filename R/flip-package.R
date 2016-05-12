
#' Class "flip.object" (and related functions) for storing the result of the
#' function flip and flipMix
#' 
#' The class flip.object is the output of a call to \code{\link{flip}},
#' \code{\link{flipMix}}, \code{\link{npc}} and \code{\link{flip.adjust}}.  It
#' stores the information needed for various diagnostic plots.  Specific
#' functions to deal with these objects are also documented here.
#' 
#' 
#' @name flip.object-class
#' @aliases draw p.value plot size result getFlip cFlip flip.object-class
#' [,flip.object-method [,flip.object,ANY,ANY,ANY-method [[,flip.object-method
#' hist,flip.object-method length,flip.object-method names,flip.object-method
#' names<-,flip.object-method p.value,flip.object-method
#' plot,flip.object-method result,flip.object-method show,flip.object-method
#' size,flip.object-method sort,flip.object-method summary,flip.object-method
#' p.adjust,flip.object-method numericOrmatrixOrNULL-class arrayOrNULL-class
#' data.frameOrNULL-class numericOrmatrixOrNULL arrayOrNULL data.frameOrNULL
#' @docType class
#' @param obj Any \code{flip-object}
#' @param element Character string of either slot names of \code{obj} (i.e.
#' \code{"res","call","permP","permT"},
#' \code{"permSpace","permY","tail","data","call.env"}), elements of slot
#' \code{obj@data} (e.g. \code{"Y","X","Z","Strata"}) or columns of
#' \code{obj@res} (usually \code{"Test","Stat",}\code{ "tail","p-value"}).
#' Specific uses: \code{"Adjust:"} returns all columns of adjusted p-values in
#' \code{obj@res}. Any among \code{"nperms","perms","B"} return the number of
#' permutation used.
#' @param ... \code{flip-object}s to be concatenated.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("flip.object", ...)}.
#' @author livio finos
#' @keywords classes
#' @examples
#' 
#' showClass("flip.object")
#' 
#' y=matrix(rnorm(50),10,5)
#' colnames(y)=c("X1","X2","Y1","Y2","Y3")
#' res=flip(y)
#' 
#' ## Sort by p-values
#' sort(res)
#' 
#' ## Selecting tests
#' res[1:2]
#' #same as
#' res["X"]
#' #different from (it selects tests having "1" or "2" in the name)
#' res[c("1","2")]
#' 
#' ## Concatenates two flip-objects
#' cFlip(res[1:2],res[5])
#' 
#' 
#' #plotting results
#' plot(flip(y))
#' 
#' #Get any slot of the flip-object. eg the permutation space:
#' head(getFlip(res,"permT"))
#' #Get any element of the list obj@data. eg Y:
#' getFlip(res,"Y")
#' #Get any columns of the results table: obj@res. eg Statistic (choose among colnames(obj@res) ):
#' getFlip(res,"Stat")
#' 
NULL





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





#' These functions handle the orbit of permutation/rotation tests (i.e.
#' permutation/rotation space).
#' 
#' \code{make.permSpace} computes the \code{perms} x n matrix of ids used for
#' test of dependence. \code{make.signSpace} computes the \code{perms} x n
#' vector of +1 and -1 used for symmetry test.
#' 
#' \code{rom} computes a Random Orthogonal Matrix of size \code{n}X\code{n}
#' (C-compiled function, very fast)
#' 
#' \code{romFast} computes a Random Orthogonal Matrix of size \code{n}X\code{n}
#' using the \code{qr.Q} decomposition. \code{romFast} is faster than
#' \code{rom} but can be inaccurate (i.e. providing inaccurate type I error
#' control when used in testing), specially for very small \code{n} (i.e.
#' sample size).
#' 
#' \code{allpermutations} computes all permutations of a vector \code{Y}. Is is
#' based on the function \code{permutations} of the \code{library(e1071)}.
#' 
#' \code{t2p} computes the (possibily multivariate) space of p-values from the
#' space of test statistic.
#' 
#' \code{rom} implements the algorithm of Stewart (1980). The function is
#' compiled in C++.
#' 
#' @name permutationSpace
#' @aliases permutationSpace make.permSpace make.signSpace allpermutations
#' npermutations t2p rom romFast
#' @param IDs vector of IDs to be permuted. If \code{IDs} is a scalar, it is
#' replaced with \code{1:IDs}.
#' @param return.permIDs logical. If \code{TRUE}, the matrix of permuted IDs is
#' stored and returned. Only used with \code{testType="permutaiton"}
#' @param N number of elements of the sample. It is also the dimention of the
#' random orthogonal matrix in \code{rom}.
#' @param Y a vector of data. It can also be a vector 1:N referring to the IDs
#' of observations.
#' @param perms number of random permutations. If it is a list, it has two
#' elements \code{number} (the number of random permutation requested) and
#' \code{seed} (the seed to be set when start generating. it is useful for
#' reproducibility) If perms > number of all possible flips, then compute the
#' complete space.
#' @param T the (possibly multivariate) permutation space as returned, for
#' example by \code{flip}
#' @param obs.only logical. If \code{TRUE} only the p-value for observed test
#' statistic is returned, otherwise the whole space is computed. Defaults:
#' \code{TRUE} if \code{T} is a \code{flip-object}, \code{FALSE} otherwise.
#' @param tail Tail of the distribution being significant for H1. See also
#' argument \code{tail} in \code{\link{flip}}. Defaults: \code{1} if \code{T}
#' is NOT a \code{flip-object}, it is taken from \code{T} otherwise.
#' @param testType See argument \code{testType} in \code{\link{flip}}
#' @param Strata See argument \code{testType} in \code{\link{flip}}
#' @param X A vector of length \code{N} with a different value for each group.
#' Only used together with \code{testType="combination"}.
#' @param ... other parameters
#' @author Livio Finos, Aldo Solari, Marco Rinaldo and Lucia Benetazzo
#' @seealso \code{\link{flip}}
#' @references Pesarin (2001) Multivariate Permutation Tests with Applications
#' in Biostatistics. Wiley, New York.
#' 
#' Stewart, G. W. (1980). The efficient generation of random orthogonal
#' matrices with an application to condition estimators. SIAM Journal on
#' Numerical Analysis 17, 403-409.
#' @keywords manip
#' @examples
#' 
#' #10 random elements of the orbit of a one-sample test
#' make.signSpace(5, 10)
#' 
#' #All elements of the orbit of a one-sample test (the size of the space is 2^5 < 1000)
#' make.signSpace(5, 1000)
#' 
#' \dontrun{
#' #A random rotation matrix of size 3
#' (r=rom(3))
#' #verify that it is orthogonal:
#' r%*%t(r)
#' }
#' 
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
NULL



