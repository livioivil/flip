############################
flip.npc.methods <-
    c("Fisher", "Liptak", "Tippett", "MahalanobisT", "MahalanobisP", "minP", "maxT", "maxTstd", "sumT", "Direct", "sumTstd", "sumT2", "Simes","kfwer", "data.sum","data.linComb","data.pc","data.trace")
############################


#' Functions for multiplicity corrections
#'
#' \code{npc} provides overall tests (i.e. weak FWER control), while
#' \code{flip.adjust} provides adjusted p-values (i.e. strong FWER control).
#'
#' \code{npc} combines the p-values using the combining functions (and the
#' method) described in Pesarin (2001). It makes use of the join space of the
#' permutations. This is usually derived from a call of \code{flip} function or
#' \code{flipMixWithin}.
#'
#' Very shortly: \code{"Fisher"} =-sum log(p-values) \code{"Liptak"} =sum
#' qnorm(p-values)  \code{"MahalanobisT"} = Mahalanobis distance of centered
#' matrix \code{permTP} (or \code{permTP@permT }) \code{"MahalanobisP"} = same
#' as above, but using scores defined by qnorm(p-values) (tails are forced to
#' be one-sided)  \code{"minP"} = \code{"Tippett"} = min(p-values) \
#' \code{"maxT"} = max(test statistics) \code{"maxTstd"} = max(standardized
#' test statistics)  \code{"sumT"} = sum (test statistics) \code{"sumTstd"}
#' = sum (standardized test statistics) \code{"sumT2"} = \code{sum (test
#' statistics)^2}. The followings have to be used carefully and only with
#' objects from function \code{flipMix}: \code{"data.sum" } = sum of all columns of
#' Y, \code{"data.linComb" } = sum of all columns of Y (includes a vector or
#' matrix \code{linComb} among the arguments), \code{"data.pc"} = extracts the
#' first Principal component from the covariance matrix (you may also include a
#' vector \code{whichPCs} indicating which PCs you want to consider)\
#' \code{"data.trace"} = Extends the Pillai Trace, use parametric bootstrap to
#' asses the significance.\code{"kfwer"} = can be only used with
#' \code{flip.adjust} (not in \code{npc}). It requires an extra parameter
#' \code{k} (\code{k=11} by default).
#'
#' \code{flip.adjust} adjusts the p-value for multiplicity (FamilyWise Error
#' Rate -FWER- and kFWER).  When \code{method} is equal to \code{"maxT"},
#' \code{"maxTstd"} (i.e. max T on \code{scale(permTP)}) or "minP" (i.e.
#' Tippett) it performs the step-down method of Westfall and Young (1993).  For
#' any other element of \code{flip.npc.methods} (i.e. "Fisher", "Liptak",
#' "sumT" (i.e. direct) or "sumT2" (sum of T^2)) a call to \code{npc} together
#' with a closed testing procedure is used (it make use of
#' \code{\link[cherry:closed]{cherry:closed}}).  When \code{method} is any
#' among \code{p.adjust.methods} the function \code{stats:p.adjust} or -if
#' weights are provided- \code{someMTP:p.adjust.w} is used.  To perform control
#' of the kFWER use \code{flip.adjust} with \code{method="kfwer"} and extra
#' parameter \code{k}.
#'
#' @aliases npc flip.adjust flip.npc.methods
#' @param permTP A permutation space (B times m matrix) or an
#' \code{flip.object} as produced by \code{\link{flip}}. Alternatively it can
#' be a \code{\link{flip.object-class}} resulting, for example from a call of
#' function \code{\link{flip}}.
#' @param comb.funct A combining function \code{flip.npc.methods} (all but
#' "kfwer"): "Fisher", "Liptak", "MahalanobisT" , "MahalanobisP" (i.e. related
#' to Hotelling T2), "minP" (i.e. Tippet), "maxT", "sumT" (i.e. direct) ,
#' "sumT2" (sum of T^2).  \code{"Fisher"} combining function is the default.
#' See also the section \code{Details}.
#' @param weights Optional argument that can be used to give certain variables
#' greater weight in the combined test.  Can be a vector or a list of vectors.
#' In the latter case, a separate test will be performed for each weight
#' vector.  If both \code{subsets} and \code{weights} are specified as a list,
#' they must have the same length. In that case, \code{weights} vectors may
#' have either the same length as the number of covariates in
#' \code{alternative}, or the same length as the corresponding subset vector.
#' Weights can be negative; the sign has no effect unless \code{directional} is
#' \code{TRUE}. It works for \code{npc} and \code{flip.adjust} with
#' \code{method}= "maxT", "maxTstd" or "minP"
#' @param subsets Optional argument that can be used to test one or more
#' subsets of variables.  Can be a vector of column names or indices of a
#' \code{\link{flip.object-class}} (\code{names(flipObject)}), or a list of
#' such vectors.  In the latter case, a separate test will be performed for
#' each subset. Only for
#' \code{comb.funct \%in\% c("data.sum","data.linComb","data.pc","data.trace")} the
#' names refers to the columns of \code{Y} data
#' \code{colnames(flipObject@data$Y) }. 
#' @param stdSpace Ask if the permutation distribution of the test statistic
#' should be standardized or not. The default is \code{FALSE}. The option is
#' applied only if \code{comb.funct} or \code{method} is equal to \code{"maxT"}
#' or \code{"sumT"}, it becomes useful when test statistics are of different
#' nature (e.g. chisquare and t-test).
#' @param ... further arguments. Among them, \code{tail} can be used to set the
#' tail of the alternative for the \code{permTP} (see also \code{\link{flip}}).
#' The arguments \code{statTest}, \code{fastSumCombination} and \code{linComb}
#' are used in objects \code{flipMix} and \code{comb.funct=
#' "data.sum","data.linComb","data.pc"} or \code{"data.trace"}.
#' @return The function returns an object of class
#' \code{\link{flip.object-class}} (and the use of
#' \code{getFlip(obj,"Adjust")}.
#' @author livio finos, Florian Klinglmueller and Aldo Solari.
#' @references Pesarin (2001) Multivariate Permutation Tests with Applications
#' in Biostatistics. Wiley, New York.
#'
#' P. H. Westfall and S. S. Young (1993). Resampling-based multiple testing:
#' Examples and methods for p-value adjustment. John Wiley & Sons.
#' @keywords htest
#' @examples
#'
#'
#' Y=data.frame(matrix(rnorm(50),10,5))
#' names(Y)=LETTERS[1:5]
#' Y[,1:2]=Y[,1:2]+1.5
#' res = flip(Y,perms=10000)
#'
#' ########npc
#' p2=npc(res) # same as p2=npc(res,"Fisher")
#' summary(p2)
#' p2=npc(res,"minP")
#' summary(p2)
#' p2=npc(res,"Fisher",subsets=list(c1=c("A","B"),c2=names(Y)))
#' summary(p2)
#' p2=npc(res,"Fisher",subsets=list(c1=c("A","B"),c2=names(Y)),weights=1:5)
#' summary(p2)
#'
#'
#' res=flip.adjust(res, method="maxT")
#' 
#' #res=flip.adjust(res,"BH")
#' ##same as
#' #p.adjust(res,"BH")
#'
#' ## now try
#' getFlip(res,"Adjust")
#'
#' @export npc
#' @export flip.npc.methods
npc <- function(permTP, comb.funct = c(flip.npc.methods, p.adjust.methods) ,subsets=NULL,weights=NULL, stdSpace=FALSE, ...){
#	on.exit(browser())
	### just in analogy with gt(). to be implemented as flip-options
	trace=FALSE
	if(is.null(list(...)$flipReturn))
    flipReturn=list(permT=TRUE,call.env=TRUE) else
      flipReturn=list(...)$flipReturn


	#### arrange comb.funct
	comb.funct <- match.arg(tolower(comb.funct[1]),tolower(flip.npc.methods))
	comb.funct <- flip.npc.methods[which(tolower(flip.npc.methods)==comb.funct)]
	if(comb.funct == "Tippett") comb.funct ="minP"
	if(comb.funct == "Direct") comb.funct ="sumT"

	if(is(permTP,"flip.object")){
    if(!is.null(list(...)$tail)) {  permTP@tail=tail  }
		nperms = permTP@call$B

    ##########flipMix type of combinations
    if(comb.funct %in% c("data.sum","data.pc","data.linComb","data.trace")) {
      if(comb.funct %in% c("data.trace")) test <- .trace.between.nptest
      if(comb.funct %in% c("data.sum","data.linComb","data.pc")) {
        if(!is.null(list(...)$statTest)&&(list(...)$statTest == "F"))
        test <-.F.between.nptest else
          test <-.t.between.nptest
      }
      environment(test) <- permTP@call.env
      environment(test)$otherParams=list(...)
      environment(test)$otherParams$subsets=subsets
      if(!is.null(environment(test)$otherParams$perms))   environment(test)$perms=environment(test)$otherParams$perms
      if(comb.funct %in% c("data.sum","data.linComb","data.pc")) environment(test)$otherParams$onlyMANOVA=TRUE
      if(comb.funct %in% c("data.sum")) environment(test)$otherParams$linComb=1
      res=test()

      out=.getOut(type="npc",res=list(permT=res$permT,extraInfoPre=list(comb.funct=comb.funct,nVar=res$extraInfoPre$nVar)),data=NULL,tail=list(...)$tail, call=match.call(),
                flipReturn=flipReturn,call.env=environment(test))
      return(out)

    } else
      if(comb.funct %in% c("Fisher", "Liptak", "minP","Simes")) {
			if(!is.null(permTP@permP)){
				permTP=permTP@permP
			} else {
				  if(!is.null(permTP@permT)) {
            permTP=t2p(.fixPermT(permTP@permT),obs.only=FALSE,tail=permTP@tail)} else
				    {print("Joint distribution of p-values not provided. Nothing done."); return()}
			}
		} else
      if(comb.funct %in% c("maxT", "maxTstd", "sumT","sumTstd",
                           "sumT2","MahalanobisT","MahalanobisP")) {
			if(is.null(permTP@permT)) {print("Joint distribution of p-values not provided. Nothing done."); return()}
			if(comb.funct %in% c("MahalanobisT","MahalanobisP")) permTP =permTP@permT else
         permTP=.setTail(.fixPermT(permTP@permT),tail=.fitTail(permTP@permT,permTP@tail) )
        if(comb.funct %in% c("maxTstd","sumTstd"))
           permTP=scale(permTP)
            permTP
      }
	} else if(!is.null(list(...)$tail)) {
	  permTP=.setTail(.fixPermT(permTP),tail=tail)
	}


	#if(!exists("nperm"))  nperms = list(number=nrow(permTP),seed=NA)
	if(!is.matrix(permTP)) permTP=as.matrix(permTP)
	if(stdSpace & (comb.funct %in% c("maxT", "sumT", "sumT2"))) {permTP = .t2stdt(permTP,FALSE)}
	if(comb.funct=="Fisher"){permTP = -log(permTP)} else
	if(comb.funct=="Liptak"){permTP = -qnorm(permTP)} else
	  # if(comb.funct%in%c("minP","Simes")){permTP = -permTP} else
	    if(comb.funct=="sumT2"){permTP = permTP^2}


	############
	temp=.getSubsetWeights(weights,subsets,colnames(permTP))
	weights=temp$weights
	subsets=temp$subsets
	many.weights=temp$many.weights
	many.subsets=temp$many.subsets
	one.weight=temp$one.weight

	rm(temp)

	# prepare progress info
   ###  if (missing(trace)) trace <- gt.options()$trace && (many.weights || many.subsets)
  if (trace && (many.subsets || many.weights)) {
    if (many.subsets)
      K <- length(subsets)
    else
      K <- length(weights)
    digitsK <- trunc(log10(K))+1
  }


  # weight
    if (one.weight) {
      if (length(weights) != ncol(permTP))
        stop("length of \"weights\" does not match column count of \"permTP\"")
      all.weights <- weights
    } else { #browser()
      if(comb.funct%in%c("sumT","sumTstd"))
		all.weights <- rep(1/sqrt(ncol(permTP)), ncol(permTP)) else
		all.weights <- rep(1, ncol(permTP))
	}
	names(all.weights) = colnames(permTP)

	if(comb.funct %in% c("Fisher", "Liptak", "sumT", "sumT2", "sumTstd"))
		  test= function(subset=NULL,weights=NULL){
		  permT = matrix(if(is.null(subset)) permTP%*%all.weights else permTP[,subset,drop=FALSE]%*%all.weights[subset]) ;
      permT} else
      if(comb.funct %in% c("maxT", "maxTstd"))
        test= function(subset=NULL,weights=NULL){ #browser()
          permT = matrix(apply(
            if(is.null(subset)) { 
              if(one.weight) 
                t(all.weights*t(permTP)) 
              else 
                permTP 
              } else {
                if(is.null(weights))
                  (permTP[,subset,drop=FALSE]) 
                else
                  t(weights[subset]*t(permTP[,subset,drop=FALSE]))
            }, 1, max))  ;
          permT
        } else
      if(comb.funct %in% c("minP"))
        test= function(subset=NULL,weights=NULL){ #browser()
          permT = -matrix(apply(if(is.null(subset)) { if(one.weight) t(t(permTP)/all.weights) else permTP } else
            t(t(permTP[,subset,drop=FALSE])/weights[subset]) , 1, min))  ;
          permT
        } else
          if(comb.funct %in% c("Simes")){

        test= function(subset=NULL,weights=NULL){ #browser()
          permT = if(is.null(subset)) { if(one.weight) t(t(permTP)/all.weights) else permTP } else
            t(t(permTP[,subset,drop=FALSE])/weights[subset])
          w=1:ncol(permT)
          Simes <- function(x) -min(sort(x)/w)
          permT=  matrix(plyr::aaply( permT , 1, Simes))  ;
          permT
        } }else
          if(comb.funct %in% c("MahalanobisT","MahalanobisP"))
        test= function(subset=NULL,weights=NULL){
         if(is.null(subset))
           if(one.weight) pseudoT<-permTP%*%diag(all.weights) else
             pseudoT<-permTP  else
             if(one.weight) pseudoT<-permTP[,subset,drop=FALSE]%*%diag(all.weights[subset])  else
               pseudoT<-permTP[,subset,drop=FALSE]
         if(ncol(pseudoT)==1)
           return(scale(pseudoT)^2)

         if(comb.funct %in% "MahalanobisP")
           pseudoT=qnorm( t2p(pseudoT,tail=1,obs.only=FALSE)*.99999999999)  else
             pseudoT=scale(pseudoT,scale=FALSE)

         ei=svd(pseudoT,nv=0)
         ei$d[ei$d<=0]=NA
         keep= is.finite(ei$d)
					  permT=matrix(rowSums(ei$u[,keep]^2)*nrow(ei$u))
         permT
					}



	# Do the test
  if ((!many.subsets) && (!many.weights)) {           # single weighting; single subset
    permT <- test()
	nVar=ncol(permTP)
  } else {
    L <- if (many.subsets) length(subsets) else length(weights)
    permT <- plyr::laply(1:L, 
                         function (i) { 
      if (trace && L>1) {
        cat(rep("\b", 2*digitsK+3), i, " / ", K, sep="")
        flush.console()
      }
      if (!many.weights) {                                           # single weighting; many subsets
        uit <- test(subset=subsets[[i]])
      } else if (!many.subsets) {                                    # many weightings; single subset
        uit <- test(weights=weights[[i]])
      } else {                                                      # many weightings; many subsets
        uit <- test(subset=subsets[[i]], weights=weights[[i]])
      }
	  uit
    })
    # browser()
    
    if(is.vector(permT)) 
      permT=as.matrix(permT) else
        permT=t(permT)
    if (many.subsets && !is.null(names(subsets))){
      colnames(permT) <- names(subsets)
	}
    else if (many.weights && !is.null(names(weights))){
      colnames(permT) <- names(weights)
	}
  nVar={if(is.null(subsets)) 1 else sapply(subsets,length)}*ifelse(is.null(weights), 1, sapply(weights,length))
  }
  if (trace && (many.subsets || many.weights) && L>1) cat("\n")



	  #build the flip-object
	out=.getOut(type="npc",res=list(permT=permT,extraInfoPre=list(comb.funct=comb.funct,nVar=nVar)),data=list(),tail=list(...)$tail, call=match.call(), flipReturn=flipReturn)
	return(out)
}
