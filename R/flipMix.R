#' The main function for testing mixed models under a permutation (and
#' rotation) framework
#' 
#' It allows to test fixed effect in mixed models. You can test within-unit
#' effects, between-unit and interactions of the two. The response can be uni-
#' or multi-variate. See also examples below.
#' 
#' 
#' @aliases flipMix flipMixWithin obs2coeffWithin
#' @param modelWithin When it is a \code{\link[stats:formula]{formula}} object,
#' a (possibly multivariate) multiple linear model is fitted.  Responses are on
#' the left, while the right part contains ONLY within-unit variables.  In this
#' case \code{data} must be supplied.  Alternatively, it can be a \code{glm}, a
#' \code{lm} or \code{vgam} (\code{library(VGAM)}) (i.e. \code{vglm}) object.
#' The \code{modelWithin} have to be performed using only variables
#' within-unit, without using \code{units} indicator (in this case the argument
#' \code{data} is not used).  It can be also a list of models. It can be null
#' if data is provided in the right format (see below).
#' @param X The part of the design matrix corresponding to the between-unit
#' effect that are not null under the alternative hypothesis. If it is a matrix
#' or a data.frame it must have a number of rows equal to the number of units
#' or equal to the total number of observations (in the latest case all
#' elements of the same units must have the same values since they are
#' between-unit effects). The non-null between-unit covariates of null model
#' are defined in \code{Z} (see argument below) and do not have to be supplied
#' again here. See also the function \code{\link{flip}}
#' 
#' NOTE: When called from \code{flipMixWithin}, \code{W} is used only if
#' \code{statTest="TBTWest"}.
#' @param Z The part of the design matrix corresponding to the non-null
#' between-unit covariates of the model under the null hypothesis. May be given
#' as a design matrix or as a half \code{\link[stats:formula]{formula}} object
#' (e.g. \code{~a+b}). See also the function \code{\link{flip}}.  If it is a
#' matrix or a data.frame it must have a number of rows equal to the number of
#' units or equal to the total number of observations (in the latest case all
#' elements of the same units must have the same values since they are
#' between-unit effects).
#' @param units Vector of units IDs. May be given as a vector or as a half
#' \code{\link[stats:formula]{formula}} object (e.g. \code{~subj}).
#' @param perms The number of permutations to use. The default is \code{perms =
#' 1000}. Alternatively it can be a matrix (i.e. the permutation space) or a
#' list with elements \code{number} and \code{seed}. See also the function
#' \code{\link{flip}}.
#' @param data Same as in the function \code{\link{flip}}. If can also be the
#' results of \code{obs2coeffWithin}.
#' @param tail Same as in the function \code{\link{flip}}.
#' @param statTest For function \code{flipMix} choose among \code{"t"} and
#' \code{"F"} (very similar to \code{statTest} in function \code{flip}.\ For
#' function \code{flipMixWithin} choose among \code{"Tnaive"} (i.e. no estimate
#' of the variance), \code{"TH0est"} (Default, i.e. estimate of the variance
#' under H0), \code{"TH1est"} (i.e. estimate of the variance under H1 for each
#' permutation. Slower but some time more powerful) and \code{"TBTWest"} (i.e.
#' estimate of the variance using ILS algorithm at each permutation; it allows
#' for \code{Z} different from a constant term. This is the same algorithm used
#' for \code{flipMix}. MUCH slower but some time even more powerful).
#' 
#' Both functions allow for vector arguments.
#' @param flipReturn Same as in the function \code{\link{flip}}.
#' @param testType See also the function \code{\link{flip}}. Note that this
#' option used only with function \code{flipMix}.
#' @param Su Usually \code{NULL}. It is the covariance matrix of the random
#' effects. If not supplied, it is estimated by iterative least square
#' algorithm.
#' @param equal.se Logical. If \code{TRUE} it force the unit to have the same
#' variance of errors (like it is usually assumed in the lmer methods).
#' @param se Usually \code{NULL}. It is a matrix of unit-specific standard
#' errors. If not supplied it is estimated by the algorithm.
#' @param replaceNA.coeffWithin deafult is \code{NA} i.e. no replacement. You
#' can provide a specific value (or a vector of values). You can also choose
#' among "coeffMeans" and "unitMeans" (i.e. mean along columns or along rows of
#' Y).
#' @param replaceNA.coeffWithin.se deafult is \code{Inf}. Use the same options
#' of \code{replaceNA.coeffWithin} (but means are over the variances and then
#' rooted).
#' @param \dots Further parameters.  \code{test.coeffWithin} Vector of names or
#' IDs of within-unit variables that have to be tested (and reported). Note
#' that variables not in the list are used in the model (i.e. the they play the
#' role of nuisance parameters). \code{fastSumCombination},\code{onlyMANOVA}
#' and \code{linComb} are used in \code{flipMix} to deal with combination of
#' variables/coefficents.
#' 
#' See also the function \code{\link{flip}} for other parameters.
#' @return \code{flipMix} and \code{flipMixWithin} return an object of class
#' \code{flip.object}.  Several operations and plots can be made from this
#' object. See also \code{\link{flip.object-class}}.
#' 
#' Note that function \code{flipMix} with \code{statTest="t"} or \code{"F"}
#' provides tests for each effect between (and interaction) and also provides
#' the overall test \code{PC1} and \code{sum} (i.e. all effects ar null, same
#' as \code{npc} does).
#' 
#' Use \code{\link{npc}} with any
#' \code{comb.funct=c("data.sum","data.linComb","data.pc","data.trace")} to
#' combine results.
#' 
#' \code{obs2coeffWithin} return a list of objects that can be used as argument
#' of \code{data} in the function \code{flipMix} and \code{flipMixWithin}.
#' @author Livio Finos and Dario Basso
#' @seealso \code{\link{flip}}, \code{\link{npc}}
#' @references L. Finos and D. Basso (2013) Permutation Tests for Between-Unit
#' Fixed Effects in Multivariate Generalized Linear Mixed Models. Statistics
#' and Computing.
#' 
#' D. Basso, L. Finos (2011) Exact Multivariate Permutation Tests for Fixed
#' Effects in Mixed-Models. Communications in Statistics - Theory and Methods.
#' @keywords htest
#' @examples
#' 
#' N=10
#' toyData= data.frame(subj=rep(1:N,rep(4,N)), Within=rep(1:2,N*2),
#'           XBetween= rep(1:2,rep(N/2*4,2)),ZBetween= rep(rnorm(N/2),rep(8,N/2)))
#' toyData= cbind(Y1=rnorm(n=N*4,mean=toyData$subj+toyData$ZBetween+toyData$XBetween),
#'                Y2=rnorm(n=N*4,mean=toyData$subj+toyData$ZBetween+toyData$Within*2),toyData)
#' (toyData)
#' 
#' #####################
#' ###Testing Between-unit effects
#' (res=flipMix(modelWithin=as.matrix(toyData[,c("Y1","Y2")])~Within,data=toyData, 
#'       X=~XBetween,Z=~ZBetween,units=~subj,perms=1000,testType="permutation",statTest="t"))
#' #same as:
#' modelWithin <- lm(as.matrix(toyData[,c("Y1","Y2")])~Within,data=toyData)
#' (flipMix(modelWithin=modelWithin,data=toyData, X=~XBetween,Z=~ZBetween,units= ~subj, 
#'         perms=1000,testType="permutation",statTest="t"))
#' 
#' ### Note that this is different from:
#' modelWithin <- list(Y1=lm(Y1~Within,data=toyData),Y2=lm(Y2~Within,data=toyData))
#' (flipMix(modelWithin=modelWithin,data=toyData, X=~XBetween,Z=~ZBetween,units= ~subj,
#'         perms=1000,testType="permutation",statTest="t"))
#' 
#' ### combining results
#' (npc(res,"data.pc"))
#' (npc(res,"data.trace"))
#' ################################
#' ###Testing Within-unit effects
#' ## The resulting test is approximated. The estimate of the variance within units 
#' ## takes in account the presence of effects between units.
#' (flipMix(modelWithin=as.matrix(toyData[,c("Y1","Y2")])~Within,data=toyData, 
#'         units= ~subj, perms=1000,testType="permutation",statTest="t"))
#' 
#' ###The resulting tests are exact. If effects between are presents, 
#' ## statTest="Tnaive" or "TBTWest" are more suitable:
#' (res=flipMixWithin(modelWithin=as.matrix(toyData[,c("Y1","Y2")])~Within,data=toyData, 
#'         units= ~subj, perms=1000,statTest=c("TH1est")))
#' npc(res)
#' 
#' @export flipMix
flipMix <- function(modelWithin,X=NULL,Z=NULL,units, perms=1000, data=NULL, tail=0,
                    statTest=NULL,flipReturn, testType="permutation", 
                    Su=NULL, equal.se=FALSE,se=NA,replaceNA.coeffWithin="coeffMeans",
                    replaceNA.coeffWithin.se=replaceNA.coeffWithin, ...) {

  otherParams= list(...)
  if(is.null(otherParams$alsoMANOVA)) otherParams$alsoMANOVA=FALSE
  
  if(missing(flipReturn)||is.null(flipReturn)) 
  flipReturn=list(permT=TRUE,permP=FALSE,data=TRUE,call.env=TRUE)
  
  if(is.null(statTest) ) if(is.null(otherParams$separatedX)   || otherParams$separatedX)  
    { statTest="t" } else statTest="F"
  
  if(!(testType%in%c("permutation","rotation","simulation","symmetry"))) {
    if(is.null(otherParams$rotationTest) || (!otherParams$rotationTest) ) 
	{testType="permutation" } else { testType="rotation"}
  }
  
  if(is.null(statTest)) statTest="t"
  
  # store the call
  call <- match.call()
  if(!(is.list(data) && (!is.data.frame(data)))) {
     data<-obs2coeffWithin(modelWithin,X=X,Z=Z,units=units, data=data,equal.se=equal.se,se=se,
                        replaceNA.coeffWithin=replaceNA.coeffWithin,replaceNA.coeffWithin.se=replaceNA.coeffWithin.se,...)
 
  }
  rm(Z,X,modelWithin)
  #########
	N = nrow(data$coeffWithin)
	p = ncol(data$coeffWithin)
	############################## Estimate of random effects

	if(is.null(data$covs)) {data$covs=array(,c(N,p,p)); for( id in 1:N) data$covs[id,,]=diag(data$se[id,]^2)}
	if(is.null(Su)){
#     if(testType=='symmetry') #dalle simulazioni il guadagno in tempo pare essere minimo mentre il guadagno in potenza pare non irrilevante
#       {data$Su=sapply(1:ncol(data$coeffWithin), function(i)
#                                  .estimateSuMultiILS(Y=data$coeffWithin[,i,drop=FALSE],
#                                                      Z=as.matrix(cbind(data$X,data$Z)), 
#                                                      S=data$covs[,i,i,drop=FALSE])[1])
#        if(length(data$Su)>1) data$Su=diag(data$Su) else data$Su=matrix(data$Su)
#        }      else 
        data$Su=.estimateSuMultiILS(Y=data$coeffWithin,Z=as.matrix(cbind(data$X,data$Z)), S=data$covs)
	 } else {
		data$Su=Su; rm(Su) 
		}
	names(data)[names(data)=="coeffWithin"]="Y"
		if(length(unique(unlist(data$X)))>1){ # if X is not a constant perform dependence.nptest
		  res=.between.nptest(data, perms=perms, statTest=statTest[1], tail = tail, testType=testType,otherParams)
		  out=res$test()
		  #una pezza estetica:
		  if(ncol(data$X)==1)
		    colnames(out$permT)=paste(colnames(out$permT), "_|_",colnames(data$X),sep="")
		  res=.getOut(res=out,data=data, call=call, flipReturn=flipReturn,call.env=res)
      if(length(statTest)>1){
        for(i in 2:length(statTest)) {
          ressub=.between.nptest(data, perms=perms, statTest=statTest[i], tail = tail, testType=testType,otherParams)
          out=ressub$test()
          #una pezza estetica:
          if(ncol(data$X)==1)
            colnames(out$permT)=paste(colnames(out$permT), "_|_",colnames(data$X),sep="")
          out=.getOut(res=out,data=data, call=call, flipReturn=flipReturn,call.env=ressub)
          res=cFlip(res,out)          
        }
      }
		
      } else{ #otherwise perform a symmetry test
		  #estTypeWithin=c("none","H0","H1")
		  #type=H0 return a vector of estimates, 
      data$W=data$Y
		  data$W[,]=NA
		  for(j in 1:nrow(data$covs)){
		    data$W[j,]=1/sqrt(diag(data$Su) + diag(data$covs[j,,]))
		  }
      res=.symmetry.nptest(data, perms=perms, statTest=statTest[1],  tail = tail,testType=testType,...)
		  out=res$test()
      out$extraInfoPre=cbind(est.Su=diag(data$Su),out$extraInfoPre)
		  res=.getOut(res=out,data=data, call=call, flipReturn=flipReturn,call.env=res)
      
      if(length(statTest)>1){
			for (i in 2:length(statTest)){
				data$W=matrix(,dim(data$Y))
        dimnames(data$W)=dimnames(data$Y)
		    for(j in 1:nrow(data$covs)){  
          data$W[j,]=1/sqrt(diag(data$Su) + diag(data$covs[j,,]))
		    }
			  ressub=.symmetry.nptest(data, perms=perms, statTest=statTest[i],  tail = tail,testType="t",...)
				out=ressub$test()
				out=.getOut(res=out,data=data, call=call, flipReturn=flipReturn,call.env=res)
        res=cFlip(res,out)
			}
		}
	}
return(res)
}
