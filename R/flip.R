#######################
flip.statTest <-
    c("t", "F", "ANOVA",
	"Wilcoxon","Kruskal-Wallis", "kruskal", "rank", "Mann-Whitney",
	"chisq","chisq.separated", "Fisher",
	#"KS", "kolmogorow", "Kolmogorow-Smirnov", "ad",
  "McNemar", "Sign","sum","coeff","cor","cor.Spearman","cor.rank","NA")

.get.statTest <- function(statTest){ 
	if(is(statTest,"function")) return(statTest) else
	
	statTest <- match.arg(tolower(statTest[1]),tolower(flip.statTest))
	statTest= flip.statTest[which(statTest==tolower(flip.statTest))]
	#synonyms
	if(statTest=="ANOVA") 
		statTest="F" else
	if(statTest=="kruskal") 
		statTest="Kruskal-Wallis" else
	if(statTest=="Mann-Whitney")
		statTest="Wilcoxon" else
		  if(statTest=="cor.rank")
		    statTest="cor.Spearman" #else
# 	if(statTest%in%c("KS", "kolmogorow"))
# 		statTest="Kolmogorow-Smirnov"
# 		
	statTest
}

#########################




#' The main function for univariate and multivariate testing under a
#' permutation (and rotation) framework + some utilities.
#' 
#' \code{flip} is the main function for permutation (or rotation) test.
#' 
#' It allows for multivariate one sample, C>=2 samples and any regression
#' tests. Also the use of covariates (to be fitted in the model but) not under
#' test is allowed.
#' 
#' 
#' \code{statTest="t"} is the t statistic derived from the correlation among
#' each Xs and each Ys (i.e. a linear model for each couples of Xs and Ys).
#' This is different from the fit of a multiple (multivariate) linear models,
#' since the correlation does not consider the other covariates).  The test
#' \code{t} is valid only under the assumption that each variable in X is
#' independent of each variable in Y. To get adequate test while adjusting for
#' covariates, use \code{Z} (see example below) The test statistic \code{"sum"}
#' is the sum of values (or frequencies) of the given sample centered on the
#' expected (i.e. computed on the overall sample). \code{"coeff"} is the
#' statistic based on the estimated coefficient of an \code{lm}. It produces a
#' test for every possible combination of (columns of) \code{X} and \code{Y}
#' (p-values can be combined using \code{npc}). \code{"cor"} is the correlation
#' (i.e. not partial correlation) between each column of \code{X} and each of
#' \code{Y}. \code{"cor.Spearman"} (or \code{"cor.rank"}) is the analogous for
#' Spearman's rank correlation coefficient.
#' 
#' \code{"ANOVA"} is synonyms of \code{"F"}. Only valid for dependence tests
#' (i.e. non constant \code{X}).  \code{"Mann-Whitney"} is synonyms of
#' \code{"Wilcoxon"}.  \code{"rank"} choose among \code{"Wilcoxon"} and
#' \code{"Kruskal-Wallis"} depending if the samples are two or more
#' (respectively).
#' 
#' The \code{"Wilcoxon"} statistic is based on the 'sum of ranks of second
#' sample minus n1*(n+1)/2' instead of 'sum of ranks of smallest sample minus
#' nSmallest*(n+1)/2'. Therefore the statistic is centered on 0 and allow for
#' two sided alternatives. Despite the p-value are ok, it requires the \code{X}
#' to be a two-levels factor in order to compute the right test statistic. When
#' the \code{X} is not a two-levels factor, it measures the codeviance among
#' \code{X} and ranks of \code{Y}.
#' 
#' For paired samples (see also the argument \code{Strata} and the example
#' below) the Signed Rank test is performed. To perform the Sign Test use
#' option \code{Sign} (i.e. same as Signed Rank but without using magnitude of
#' ranks).
#' 
#' The \code{"Fisher"} test is allowed only with dichotomous \code{Y}s. The
#' reported statistic is the bottom-right cell of the 2 by 2 frequencies table.
#' The \code{"chisq.separated"} test perform cell-wise chi squared (see also
#' Finos and Salmaso (2004) Communications in Statistics - Theory and methods).
#' 
#' The \code{"McNemar"} test is based on the signs of the differences, hence it
#' can be used also with ordinal or continuous responses. Only valid for
#' symmetry tests (i.e. \code{X} is constant or \code{NULL}).  The reported
#' statistic for \code{"McNemar"} test is the signed squared root of the
#' McNemar statistic. Hence it allows for tailed alternatives.
#' 
#' For ordered \code{X}, a stochastic ordering test can be performed using
#' \code{"t","Wilcoxon","sum"} and then combining the separated test using
#' \code{npc}.
#' 
#' When \code{statTest} is a \code{function}, the first argument must be
#' \code{Y}. This same function is ran to observed data \code{Y} and to a
#' number of permuted rows of \code{Y}. The returned value must be a vector of
#' test statistics. Please note that argument \code{tail} must be defined
#' accordingly. The default way the rows of \code{Y} are rearranged is through
#' permutation (without strata). More complex permutation strategies can be
#' defined through proper definition of argument \code{perm} (see also
#' \code{\link{permutationSpace}}).
#' 
#' For \code{testType="rotation"}: As long as the number of orthogonalized
#' residuals (i.e. the number of observations minus the number of columns in
#' \code{Z}) is lower than 50, the function \code{rom} is used. The the number
#' is larger, the faster version \code{romFast} is used instead. Although the
#' latter is less accurate, for such a big sample size, it is not expected to
#' affect the control of the type I error.
#' 
#' @aliases flip flip.statTest orthoZ
#' @param Y The response vector of the regression model. May be supplied as a
#' vector or as a \code{\link[stats:formula]{formula}} object. In the latter
#' case, the right hand side of \code{Y} is passed on to \code{alternative} if
#' that argument is missing, or otherwise to \code{null}.
#' @param X The part of the design matrix corresponding to the alternative
#' hypothesis. The covariates of the null model do not have to be supplied
#' again here. May be given as a half \code{\link[stats:formula]{formula}}
#' object (e.g. \code{~a+b}). In that case the intercept is always suppressed.
#' @param Z The part of the design matrix corresponding to the null hypothesis.
#' May be given as a design matrix or as a half
#' \code{\link[stats:formula]{formula}} object (e.g. \code{~a+b}). The default
#' for \code{Z} is \code{~1}, i.e. only an intercept. This intercept may be
#' suppressed, if desired, with \code{Z = ~0}.
#' @param data Only used when \code{Y}, \code{X}, or \code{Z} is given in
#' formula form. An optional data frame, list or environment containing the
#' variables used in the formula. If the variables in a formula are not found
#' in \code{data}, the variables are taken from environment(formula), typically
#' the environment from which \code{gt} is called.
#' @param tail Vector of values -1, 0 or 1 indicating the tail to be used in
#' the test for each column of \code{Y}. \code{tail=1} (-1) means that greater
#' (smaller) values bring more evidence to the alternative hypothesis.
#' \code{tail=0} indicates a two sided alternative. If the length of
#' \code{tail} is smaller than number of columns of \code{Y}, the values are
#' recycled.
#' @param perms The number of permutations to use. The default is \code{perms =
#' 1000}. Alternatively it can be a matrix (i.e. the permutation space) or a
#' list with elements \code{number} and \code{seed}.
#' @param Strata A vector, which unique values identifies strata. This option
#' is used only with \code{testType="permutation"}; parameter \code{Z} is not
#' considered in this case.  Also note that when only two levels with one
#' observation per each level are present in each stratum, the problem becomes
#' a paired two-samples problem and hence simplified to a one-sample test.
#' @param statTest Choose a test statistic from \code{flip.statTest}. See also
#' Details section.
#' @param flipReturn list of objects indicating what will be included in the
#' output.
#' 
#' e.g. \code{list(permP=TRUE,permT=TRUE,data=TRUE)}.
#' @param testType by default \code{testType="permutation"}. The use of option
#' \code{"combination"} is more efficient when \code{X} is indicator of groups
#' (i.e. C>1 samples testing). When the total number of possible combinations
#' exceeds 10 thousand, \code{"permutation"} is performed. As an alternative,
#' if you choose \code{"rotation"}, resampling is performed through random
#' linear combinations (i.e. a rotation test is performed). This option is
#' useful when only few permutations are available, that is, minimum reachable
#' significance is hight. See also the \code{details} section for the algorithm
#' used. The old syntax \code{rotationTest=TRUE} is maintained for
#' compatibility but is deprecated, use \code{testType="rotation"} instead.
#' @param returnGamma logical. Should be the eigenvectors (with corresponding
#' non-null eigenvalues) of the anti-projection matrix of \code{Z} (i.e. I-
#' Z(Z'Z)^-1 Z') returned?
#' @param \dots Further parameters. The followings are still valid but
#' deprecated:
#' 
#' \code{permT.return = TRUE, permP.return = FALSE},
#' 
#' \code{permSpace.return = FALSE, permY.return = FALSE}. Use \code{flipReturn}
#' instead.
#' 
#' \code{dummyfy} a named list of logical values (eg.
#' \code{list(X=TRUE,Y=TRUE)})
#' 
#' \code{rotationTest= TRUE}. Deprecated, use \code{testType='rotation'}
#' instead.
#' @return An object of class \code{flip.object}.  Several operations and plots
#' can be made from this object. See also \code{\link{flip.object-class}}.
#' @author livio finos (livioATstatDOTunipdDOTit)
#' @seealso The permutation spaces on which the test is based:
#' \code{\link{permutationSpace}} function and useful functions associated with
#' that object.
#' 
#' Multiplicity correction: \code{\link{flip.adjust}} and Global test:
#' \code{\link{npc}}.
#' @references For the general framework of univariate and multivariate
#' permutation tests see: Pesarin, F. (2001) Multivariate Permutation Tests
#' with Applications in Biostatistics. Wiley, New York.
#' 
#' For Rotation tests see: Langsrud, O. (2005) Rotation tests, Statistics and
#' Computing, 15, 1, 53-60
#' 
#' A. Solari, L. Finos, J.J. Goeman (2014) Rotation-based multiple testing in
#' the multivariate linear model. Biometrics. Accepted
#' @keywords htest
#' @examples
#' 
#' Y=matrix(rnorm(50),10,5)
#' colnames(Y)=LETTERS[1:5]
#' Y[,1:2]=Y[,1:2] +2
#' res = flip(Y)
#' res
#' plot(res)
#' 
#' X=rep(0:1,5)
#' Y=Y+matrix(X*2,10,5)
#' 
#' data=data.frame(Y,X=X, Z=rnorm(10))
#' #testing dependence among Y's and X
#' (res = flip(Y,~X,data=data))
#' #same as:
#' #res = flip(A+B+C+D+E~X,data=data)
#' 
#' 
#' #testing dependence among Y's and X, also using covariates
#' res = flip(Y,~X,~Z,data=data)
#' res
#' #Note that 
#' #flip(Y,X=~X,Z=~1,data=data)
#' #is different from
#' #flip(Y,~X,data=data)
#' #since the former is based on orthogonalized residuals of Y and X by Z.
#' 
#' \dontrun{
#' #Rotation tests:
#' rot=flip(Y,X,Z=~1,testType="rotation") 
#' # note the use Z=~1.
#' }
#' 
#' #Using rank tests:
#' res = flip(Y,~X,data=data,statTest="Wilcoxon")
#' res
#' 
#' #testing symmetry of Y around 0
#' Y[,1:2]=Y[,1:2] +2
#' res = flip(Y)
#' res
#' plot(res)
#' 
#' 
#' #use of strata (in this case equal to paired samples)
#' data$S=rep(1:5,rep(2,5))
#' #paired t
#' flip(A+B+C+D+E~X,data=data,statTest="t",Strata=~S)
#' #signed Rank test
#' flip(A+B+C+D+E~X,data=data,statTest="Wilcox",Strata=~S)
#' 
#' # tests for categorical data
#' data=data.frame(X=rep(0:2,10))
#' data=data.frame(X=factor(data$X),Y=factor(rbinom(30,2,.2+.2*data$X)))
#' flip(~Y,~X,data=data,statTest="chisq")
#' # separated chisq (Finos and Salmaso, 2004. Nonparametric multi-focus analysis 
#' # for categorical variables. CommStat - T.M.)
#' (res.sep=flip(~Y,~X,data=data,statTest="chisq.separated"))
#' npc(res.sep,"sumT2") #note that combined test statistic is the same as chisq
#' 
#' \dontrun{
#' # User-defined test statistic:
#' my.fun <- function(Y){
#'   summary(lm(Y~X))$coeff[1,"Pr(>|t|)"]
#' }
#' X<- matrix(rep(0:2,10))
#' Y <- matrix(rnorm(30))
#' flip(Y=Y,X=X,statTest=my.fun)
#' }
#' 
#' @export flip
flip <- function(Y, X=NULL, Z=NULL, data=NULL, tail = 0, perms = 1000, statTest=NULL, 
                 Strata=NULL, flipReturn, testType=NULL, ...) {

  if(is.null(statTest) ) if(is.null(list(...)$separatedX)   || list(...)$separatedX)   { statTest="t" } else statTest="F"
    statTest <- .get.statTest(statTest)
	
  if(is.null(testType)){
	if(is.null(list(...)$rotationTest) || (!list(...)$rotationTest) ) {testType="permutation"} else { testType="rotation"} 
  } 
  testType=match.arg(testType,c("permutation","rotation","symmetry","combination"))

  if(missing(flipReturn)||is.null(flipReturn)) 
    flipReturn=list(permT=TRUE,permP=FALSE,permSpace=FALSE,test=TRUE,permID=TRUE)
                    

  # store the call
  call <- match.call()
  
  if(!is.function(statTest)){
    # get matrices from inputs
    data <- .getXY(Y,X,Z,data,rotationTest=(testType=="rotation"),dummyfy=list(...)$dummyfy,statTest=statTest,Strata=Strata)
    rm(X,Y,Z,Strata)
    
    symmetryTest= is.null(data$X) || (length(unique(data$X))==1)
  
    #check if the problem can be set as one sample problem
    if(!symmetryTest) if(!is.function(statTest))
  	if(statTest%in% c("t","sum","cor","cor.Spearman","cor.rank","ranks","Wilcoxon","McNemar","Sign"))
  	  if(  !is.null(data$Strata) ){#is.null(data$Z)|| ncol(data$Z)==0)  &
  			keep=setdiff(1:ncol(data$X),.getIntercept(data$X))
  			if( (length(unique(data$X[,keep]))==2) && 
  				(ncol(data$X[,keep,drop=FALSE])==1) )
  					if(all(table(data$X[,keep],unlist(data$Strata))==1)){
  						attrsYassign=attributes(data$Y)$assign
  						attrsYfactors=attributes(data$Y)$factors
              
  						data$X=data$X[,keep,drop=FALSE]
  						levs=unique(data$X)
  						data$Y=t(sapply(unique(as.character(unlist(data$Strata))), function(ids){
                data$Y[(data$Strata==ids)&(data$X==levs[2]),]-
                data$Y[(data$Strata==ids)&(data$X==levs[1]),]}))
              
  						attributes(data$Y)$assign=attrsYassign
  						attributes(data$Y)$factors=attrsYfactors
  						data$X=NULL
  						data$Strata=NULL
  						data$Z=NULL
  						symmetryTest=TRUE
  					}	
  		}
    
    # if symmetry.nptest
    if(symmetryTest){
    		test= .symmetry.nptest(data, perms=perms, statTest=statTest,  tail = tail,testType=testType,...)
    ##dependence.nptest
    } else 
  	if ( !(any(is.na(data$Y))|| ifelse(is.null(data$X),TRUE,any(is.na(data$X)))) || statTest=="NA"){
      # standard solutions, not missing data
  		test= .dependence.nptest(data, perms=perms,statTest=statTest,  
                               tail = tail,testType=testType,
                               return.permIDs = flipReturn$permID, ...)
  	} else {	stop("Warning: NA values are not allowed unless you use statTest=\"NA\", nothing done.")	}
    #browser()
  } else{
    test= .custom.nptest(Y=Y, X=X, Z=Z, data=data, tail = tail, perms = perms, statTest=statTest, Strata=Strata, flipReturn=flipReturn, testType=testType, ...)
  }

  res <- test$test()
	#build the flip-object
  res$call=call
	res=.getOut(res=res,data=data, call=call, flipReturn=flipReturn,call.env=test)
  return(res)
}

#################

.custom.nptest <- function(Y,X=NULL, Z=NULL, data=NULL, tail = 0, 
          perms = 1000, statTest=NULL, Strata=NULL, flipReturn, 
                           testType=NULL, ...) {
  test<- function() {
    N=nrow(Y)
    if(is.null(N)) N=length(N)
    perms <- make.permSpace(N,perms,return.permIDs=FALSE,Strata=Strata)
    perms$rotFunct=NULL
    digitsK=trunc(log10(perms$B))+1
    
    
    obs=statTest(Y)
    permT=matrix(,perms$B,length(obs))
    colnames(permT)=names(obs)
    permT[1,]=obs
    for(i in 2:(perms$B))
      {permT[i,]=statTest(Y[sample(perms$n),,drop=FALSE] )
              if (i%%10==0) {
                cat(rep("\b", 2*digitsK+3), i, " / ", perms$B, sep="")
                flush.console()
              } 
    }
    flush.console()
    cat("\n")
    if(is.null(colnames(permT))){
      if(ncol(permT)==ncol(Y)) 
          colnames(permT)=.getTNames(Y,,permT=permT,checkUnique=TRUE) else
            if(ncol(permT)==ncol(Y)*ncol(X)) colnames(permT)=.getTNames(Y,X,permT=permT,checkUnique=TRUE) else
               colnames(permT)=paste("my.test",sep="",1:ncol(permT))
    }
    rownames(permT)=.getTRowNames(permT)		  
    res=list(permT=permT,perms=perms,tail=tail,extraInfoPre=list(Test="Custom"))
  }
  
  environment(test) <- sys.frame(sys.nframe())
  out <- sys.frame(sys.nframe())
  return(out)
}
