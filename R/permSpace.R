
#' These functions handle the orbit of permutation/rotation tests (i.e.
#' permutation/rotation space).
#'
#' \code{make.permSpace} computes the \code{perms} x n matrix of ids used for
#' test of dependence. \code{make.signSpace} computes the \code{perms} x n
#' vector of +1 and -1 used for symmetry test.
#'
#' \code{rom} computes a Random Orthogonal Matrix of size \code{n}X\code{n}
#' (C-compiled function, very fast). implements the algorithm of Stewart (1980). The function is
#' compiled in C++. NOTE: this option is not available in the newest versions. This is now equivalent to \code{romFast}
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
#'
#' @name permutationSpace
#' @aliases permutationSpace make.permSpace make.signSpace allpermutations
#' npermutations t2p rom romFast
#' @export make.permSpace
#' @export make.signSpace
#' @export allpermutations
#' @export npermutations
#' @export t2p
#' @export rom
#' @export romFast
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


#utils::globalVariables(c("testType", "statTest","i","stratum","permSpace"))
#out <- sys.frame(sys.nframe())

####### deals with NA in permSpace
.fixPermT <- function(permT){
  if(any(is.na(permT))){
    permT[,apply(permT,2,function(x) all(is.na(x)))]=0
    permT=permT[apply(permT,1,function(x)!any(is.na(x))),,drop=TRUE]
  }
  permT
}
######## match the setting for permutation
.PermSpaceMatchInput <- function(perms) {
if (!is.list(perms)) {
		#the whole matrix of random permutaitons is provided
		if (is.matrix(perms)) return(list(permID = perms, B=nrow(perms) , n= ncol(perms), seed=NA))
		#only the number of random permutaitons is provided
		if (is.numeric(perms)) return(list(permID = NULL, B=perms ,seed=NA))

      } else return(perms)  #hopefully there are all elements, migliorare la funzione qui
}



############################
# Calculates signs flips of a vector of N elements.
# perms is the number of flips; if perms > number of all possible flips, then compute the complete space
############################
make.signSpace <- function(N,perms) {
    perms=.PermSpaceMatchInput(perms)
	perms$n=N
	if(is.null(perms$permID)){
		if (2^(N-1) <= perms$B) {
		    # all permutations if possible and if no stratas
			#random <- FALSE
		 # require(e1071)
			if(N>1){
				perms$permID <-cbind(0,e1071::bincombinations(N-1))
				perms$permID= perms$permID[-1,]
				perms$permID [which(perms$permID ==1)] <- -1
				perms$permID [which(perms$permID ==0)] <- 1
			} else {
				print("The function needs N>1")
				return()
				}
				perms$seed=NA
				perms$B=(2^(N))
				if(is.null(perms$rotFunct)) perms$rotFunct <- function(i) (permSpace$permID[i,]*data$Y)
		} else {
			#otherwise random permutations
			#if (is.na(perms$seed)) perms$seed <- round(runif(1)*1000)
			if ((!is.null(perms$seed))&&(!is.na(perms$seed))) set.seed(perms$seed)
			perms$permID <- matrix(1 - 2 * rbinom(N * ((perms$B-1)%/%2),1, 0.5), ((perms$B-1)%/%2), N)
			if(is.null(perms$rotFunct)) perms$rotFunct <- function(i) (permSpace$permID[i,]*data$Y)
		}
	} else if(is.null(perms$rotFunct)) perms$rotFunct <- function(i) (permSpace$permID[i,]*data$Y)
	perms$type="symmetry"
	perms
}

############################
# Calculates permutations space of a vector Y.
# perms is the number of permutations; if perms > number of all permutations, then compute the complete space
############################
make.permSpace <- function(IDs,perms,return.permIDs=FALSE,testType="permutation",Strata=NULL,X=NULL,...) {
  perms=.PermSpaceMatchInput(perms)
  if(tolower(testType)=="combination") {
    perms=.make.CombSpace(X,perms)
  } else if(tolower(testType)=="rotation") {
    perms=.make.RotSpace(IDs,perms)
  } else if(tolower(testType)=="simulation") {
	  perms=.make.SimSpace(IDs,perms)
	} else if(tolower(testType)=="symmetry") {
	  perms=make.signSpace(length(IDs),perms)
	} else 	{ ## then standard permutations
		perms=.make.PermSpace(IDs,perms,return.permIDs=return.permIDs,Strata=Strata)
  }
  if((!is.null(perms$seed))&&(!is.na(perms$seed))) set.seed(perms$seed)
  environment(perms$rotFunct) <- sys.frame(sys.parent())
  perms
}

##########################
##make random permutations of indices
##########################
.make.PermSpace <- function(IDs,perms,return.permIDs=FALSE,Strata=NULL,forceRandom=FALSE){
  if(is.null(return.permIDs)) return.permIDs=FALSE
     if(length(IDs)==1) IDs
	if(is.null(Strata)){
				perms$n=length(IDs)
				allperms=npermutations(IDs)
				# all permutations if possible
				if ( ( allperms <= perms$B) && (!forceRandom)) {
					#random <- FALSE
					perms$permID <- allpermutations(IDs)[-1,]
					perms$seed=NA
					perms$B=(allperms)
					perms$rotFunct <- function(i) (data$Y[perms$permID[i,],,drop=FALSE])
				} else {
					# otherwise random permutations
					# if (is.na(perms$seed))
						# perms$seed <- round(runif(1)*1000)
					# set.seed(perms$seed)
					if ((!is.null(perms$seed))&&(!is.na(perms$seed)))	set.seed(perms$seed)

					if(!return.permIDs) {
						perms$rotFunct <- function(i) (data$Y[sample(perms$n),,drop=FALSE])
					} else {
						perms$permID <- t(replicate((perms$B-1), IDs[sample(perms$n)]))
						perms$rotFunct <- function(i) (data$Y[perms$permID[i,],,drop=FALSE])
						}
				}
				perms$type="permutation"
		perms
	} else #Strata are present
	{
		strataLabel= as.vector(unique(as.matrix(Strata)))
		space=.make.PermSpace(IDs=IDs[Strata==strataLabel[1]],perms=perms,return.permIDs=TRUE,Strata=NULL,forceRandom=TRUE)
		for(i in 2:length(strataLabel))
		space$permID =cbind(space$permID,.make.PermSpace(IDs=IDs[Strata==strataLabel[i]],perms=perms,return.permIDs=TRUE,Strata=NULL,forceRandom=TRUE)$permID)
    space$n=length(IDs)
	space
	}
}


############################
# rotation space of a vector Y.
# perms is the number of permutations; seed the seed for random number generation, rotFunct the function to generate the random rotations
############################
rom <- romFast  <- function(N) {
  R <- matrix(rnorm(N^2),ncol=N)
  R <- qr.Q(qr(R, LAPACK = FALSE))
  flipsign <- which(rbinom(N,1,.5)==1)
  R[,flipsign] <- -R[,flipsign]
  R
  }



.make.RotSpace <- function(Y,perms) {

	##then it is a list anyway now
	perms <- perms[intersect(names(perms),c("B","seed","n","rotFunct"))]
	if(is.null(perms$n))  perms$n <- length(Y)
	if(is.null(perms$B))  perms$B <- 1000
	#if(is.null(perms$seed) || is.na(perms$seed) )  perms$seed <- round(runif(1)*1000)
	if(is.null(perms$rotFunct))
	  if(perms$n<50){
      perms$rotFunct  <- function(i) { #argument is not used now
        return(rom(perms$n)%*%data$Y)
      }
	  } else {
	    perms$rotFunct  <- function(i) { #argument is not used now
	      return(romFast(perms$n)%*%data$Y)
	    }
	  }
	perms$type="rotation"
	return(perms)
}


######################################
.make.SimSpace <- function(covs,perms) {

	##then it is a list anyway now
	perms <- perms[intersect(names(perms),c("B","seed","n","rotFunct"))]
	if(is.null(perms$n))  perms$n <- nrow(covs)
	if(is.null(perms$B))  perms$B <- 1000
	#if(is.null(perms$seed) || is.na(perms$seed) )  perms$seed <- round(runif(1)*1000)
	perms$p=ncol(covs)
  if(perms$p==1){
    perms$rots=array(1,c(nrow(covs),1))
  } else {
  	ei=eigen(covs[1,,]);
  	ei$values[ei$values<0]=0;
  	temp=diag(sqrt(ei$values))%*%t(ei$vectors)
    perms$rots=matrix(,nrow(covs),length(temp))
    perms$rots[1,]=temp
    rm(temp,ei)
  	for(i in 2:nrow(covs)) {
      ei=eigen(covs[i,,]);
      ei$values[ei$values<0]=0;
      perms$rots[i,]=diag(sqrt(ei$values))%*%t(ei$vectors)
  	}
  }
	#now covs is a list of length n
	rm(covs)
	if(is.null(perms$rotFunct))
		perms$rotFunct  <- function(i) { #argument is not used yet
			R <- rnorm(nrow(perms$rots))*perms$rots
			R <- array(R,c(perms$p,perms$n,perms$p))
			R <- apply(R,c(2,3),sum)
		}
	perms$type="simulation"
	return(perms)
}

############################
# Iterative function calculates all permutations of a vector
# values: vector of all unique values
# multiplicity: multiplicity of each value
############################
# .allpermutations <- function(values, multiplicity) {
#
#   if (length(values)==1) {
#     out <- values
#   } else {
#     n <- sum(multiplicity)
#     out <- matrix(0 , n, .npermutations(multiplicity))
#     where <- 0
#     for (i in 1:length(values)) {
#       if (multiplicity[i] == 1) {
#         newmult <- multiplicity[-i]
#         newvals <- values[-i]
#       } else {
#         newmult <- multiplicity
#         newmult[i] <- newmult[i] - 1
#         newvals <- values
#       }
#       range <- where + seq_len(.npermutations(newmult))
#       where <- range[length(range)]
#       out[1,range] <- values[i]
#       out[2:n, range] <- .allpermutations(newvals, newmult)
#     }
#   }
#   out
# }

############################
# Iterative function counts all permutations of a vector
# values: vector of all unique values
# multiplicity: multiplicity of each value
############################
.npermutations <- function(multiplicity) {
  round(exp(lfactorial(sum(multiplicity)) - sum(lfactorial(multiplicity))))
}

############################
# Counts all permutations of a vector y
# user-friendly version of .npermutations()
############################
npermutations <- function(Y) {
  .npermutations(table(Y))
}

############################
# Calculates all permutations of a vector y using permutations() of library(e1071)
############################
allpermutations <- function(Y) {
  matrix(Y[e1071::permutations(length(Y))],ncol=length(Y))
}


##########
#compute p-value space P from statistic space T (the percentile of the statistic T column-wise)
t2p<-function(T, obs.only=NULL, tail = NULL){
    if(is.null(tail)){
      if(is(T, "flip.object")) tail=T@tail else tail=1
    }
    if(is.null(obs.only)){
      if(is(T, "flip.object")) obs.only=FALSE else obs.only=TRUE
    }
    if(is(T, "flip.object")) {
      T=T@permT
    }
    if(is.vector(T)) T=as.matrix(T)

	if(!missing(tail))	T = .setTail(T,tail)

	if(!is.matrix(T)) {T<-as.matrix(T)}
	if(obs.only) {
		P=matrix(apply(T,2,function(permy)mean(permy>=permy[1],na.rm=TRUE)),1,ncol(T))
		rownames(P)="p-value"
	} 	else{
		#oth<-seq(1:length(dim(T)))[-1]
    T <- .fixPermT(T)
		B<-nrow(T)
		P=apply(-T,2,rank,ties.method ="max",na.last="keep")/B
		P=as.matrix(P)
		rownames(P)=c("p-obs",paste("p-*",1:(B-1),sep=""))
	}
	colnames(P)=colnames(T)
	return(P)
}


############### standardize permT space. used in maxTstd and
.t2stdt <- function(permT,obs.only=TRUE){ return(t(t((permT[1:(nrow(permT)^(!obs.only)),]))/apply(permT,2,sd,na.rm=TRUE)))}
