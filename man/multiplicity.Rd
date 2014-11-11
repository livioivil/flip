\name{npc}
\alias{npc}
\alias{flip.adjust}
\alias{flip.npc.methods}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for multiplicity corrections
}
\description{
\code{npc} provides overall tests (i.e. weak FWER control), while \code{flip.adjust} provides adjusted p-values (i.e. strong FWER control).
}
\usage{
npc(permTP, comb.funct = c(flip.npc.methods, p.adjust.methods), 
          subsets=NULL,weights=NULL, stdSpace=FALSE, ...)
flip.adjust(permTP, method = flip.npc.methods, maxalpha = 1, 
          weights = NULL, stdSpace=FALSE, ...)

flip.npc.methods 
#    c("Fisher", "Liptak", "Tippett", "MahalanobisT", "MahalanobisP", 
#      "minP", "maxT", "maxTstd", "sumT", "Direct", "sumTstd", "sumT2","kwer",
#      "data.sum","data.linComb","data.pc","data.trace") 
# use methods in the last row only for outputs of function flipMix()

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{permTP}{
  A permutation space (B times m matrix) or an \code{flip.object} as produced by \code{\link{flip}}. Alternatively it can be a \code{\link{flip.object-class}} resulting, for example from a call of function \code{\link{flip}}.
}
  \item{comb.funct}{ A combining function \code{flip.npc.methods} (all but "kfwer"): "Fisher", "Liptak", "MahalanobisT" , "MahalanobisP" (i.e. related to Hotelling T2), "minP" (i.e. Tippet), "maxT", "sumT" (i.e. direct) , "sumT2" (sum of T^2).
  \code{"Fisher"} combining function is the default.
  See also the section \code{Details}.
}
  \item{method}{ A method among \code{\link{flip.npc.methods}} or \code{\link{p.adjust.methods}}. By default \code{"maxT"} is used. 
  See also the section \code{Details}.
}
  \item{maxalpha}{ Adjusted p-values greater than \code{maxalpha} are forced to 1. It saves computational time when there are many hypotheses under test.
}
  \item{weights}{
Optional argument that can be used to give certain variables greater weight in the combined test. 
Can be a vector or a list of vectors. In the latter case, a separate test will be performed for each weight vector. 
If both \code{subsets} and \code{weights} are specified as a list, they must have the same length. In that case, \code{weights} vectors may have either the same length as the number of covariates in \code{alternative}, or the same length as the corresponding subset vector. Weights can be negative; the sign has no effect unless \code{directional} is \code{TRUE}.
It works for \code{npc} and \code{flip.adjust} with \code{method}= "maxT", "maxTstd" or "minP"
}
  \item{subsets}{Optional argument that can be used to test one or more subsets of variables. 
  Can be a vector of column names or indices of a \code{\link{flip.object-class}} (\code{names(flipObject)}), or a list of such vectors. 
  In the latter case, a separate test will be performed for each subset.
Only for \code{comb.funct%in%c("data.sum","data.linComb","data.pc","data.trace")} the names refers to the columns of \code{Y} data (\code{colnames(flipObject@data$Y)}).
}
}
 
 \item{stdSpace}{Ask if the permutation distribution of the test statistic should be standardized or not. The default is \code{FALSE}. The option is applied only 
 if \code{comb.funct} or \code{method} is equal to \code{"maxT"} or \code{"sumT"}, it becomes useful when test statistics are of different nature (e.g. chisquare and t-test).}
  
  \item{...}{further arguments. Among them, \code{tail} can be used to set the tail of the alternative for the \code{permTP} (see also \code{\link{flip}}).
The arguments \code{statTest}, \code{fastSumCombination} and \code{linComb} are used in objects \code{flipMix} and \code{comb.funct= "data.sum","data.linComb","data.pc"} or \code{"data.trace"}.
 }
  
}

\details{
\code{npc} combines the p-values using the combining functions (and the method) described in Pesarin (2001). It makes use of the join space of the permutations. This is usually derived from a call of \code{flip} function or \code{flipMixWithin}.

Very shortly:\\
\code{"Fisher"} =-sum log(p-values) \\
\code{"Liptak"} =sum qnorm(p-values) \\
\code{"MahalanobisT"} = Mahalanobis distance of centered matrix \code{permTP} (or \code{permTP@permT})  \\
\code{"MahalanobisP"} = same as above, but using scores defined by  qnorm(p-values) (tails are forced to be one-sided) \\
\code{"minP"} = \code{"Tippett"} = min(p-values) \\
\code{"maxT"} = max(test statistics) \\
\code{"maxTstd"} = max(standardized test statistics) \\
\code{"sumT"} = sum (test statistics) \\ 
\code{"sumTstd"} = sum (standardized test statistics) \\ 
\code{"sumT2"} = sum (test statistics)^2\\
The followings have to be used carefully and only with objects from function flipMix()\\
\code{"data.sum"} = sum of all columns of Y\\
\code{"data.linComb"} = sum of all columns of Y (include a vector or matrix \code{linComb} among the arguments)\\
\code{"data.pc"} = extract the first Principal component from the covariance matrix (you may also include a vector \code{whichPCs} indicating which PCs you want to consider)\\
\code{"data.trace"} = Extends the Pillai Trace, use parametric bootstrap to asses the significance.\\
\code{"kfwer"} = can be only used with \code{flip.adjust} (not in \code{npc}). It requires an extra parameter \code{k} (\code{k=11} by default).



\code{flip.adjust} adjusts the p-value for multiplicity (FamilyWise Error Rate -FWER- and kFWER).  
When \code{method} is equal to \code{"maxT"}, \code{"maxTstd"} (i.e. max T on \code{scale(permTP)}) or "minP" (i.e. Tippett) it performs the step-down method of Westfall and Young (1993).
 For any other element of \code{flip.npc.methods} (i.e. "Fisher", "Liptak", "sumT" (i.e. direct) or "sumT2" (sum of T^2)) a call to \code{npc} together with a closed testing procedure is used (it make use of  \code{\link[cherry:closed]{cherry:closed}}).
 When \code{method} is any among \code{p.adjust.methods} the function \code{stats:p.adjust} or -if weights are provided- \code{someMTP:p.adjust.w} is used.
 To perform control of the kFWER use \code{flip.adjust} with \code{method="kfwer"} and extra parameter \code{k}.
}


\value{
 The function returns an object of class \code{\link{flip.object-class}} (and the use of \code{getFlip(obj,"Adjust")}.
}


\author{
livio finos  (livioATstatDOTunipdDOTit) and Aldo Solari.
}

\references{
Pesarin (2001) Multivariate Permutation Tests with Applications in Biostatistics. Wiley, New York.

P. H. Westfall and S. S. Young (1993). Resampling-based multiple testing: Examples and methods for p-value adjustment. John Wiley & Sons.
}

\examples{

Y=data.frame(matrix(rnorm(50),10,5))
names(Y)=LETTERS[1:5]
Y[,1:2]=Y[,1:2]+1.5
res = flip(Y,perms=10000)

########npc
p2=npc(res) # same as p2=npc(res,"Fisher")
summary(p2)
p2=npc(res,"minP")
summary(p2)
p2=npc(res,"Fisher",subsets=list(c1=c("A","B"),c2=names(Y)))
summary(p2)
p2=npc(res,"Fisher",subsets=list(c1=c("A","B"),c2=names(Y)),weights=1:5)
summary(p2)



res=flip.adjust(res,"maxT")

#res=flip.adjust(res,"BH")
##same as 
#p.adjust(res,"BH")

## now try
#getFlip(res,"Adjust")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
