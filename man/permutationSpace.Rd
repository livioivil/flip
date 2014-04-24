\name{permutationSpace}
\alias{permutationSpace}
\alias{make.permSpace}
\alias{make.signSpace}
\alias{allpermutations}
\alias{npermutations}
\alias{t2p}
\alias{rom}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
These functions handle the orbit of permutation/rotation tests (i.e. permutation/rotation space).
}
\description{
\code{make.permSpace} computes the \code{perms} x n matrix of ids used for test of dependence. \code{make.signSpace} computes the \code{perms} x n vector of +1 and -1 used for symmetry test.

\code{rom} computes a Random Orthogonal Matrix of size \code{n}X\code{n}.

\code{t2p} computes the (possibily multivariate) space of p-values from the space of test statistic.

}
\usage{
make.permSpace(IDs,perms,return.permIDs=FALSE,testType="permutation",Strata=NULL)
make.signSpace(N, perms)
allpermutations(Y)
npermutations(Y)
rom(N)
t2p(T, obs.only = TRUE, tail)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{IDs}{ vector of IDs to be permuted. If \code{IDs} is a scalar, it is replaced with \code{1:IDs}.}
\item{return.permIDs}{ logical. If \code{TRUE}, the matrix of permuted IDs is stored and returned. Only used with \code{testType="permutaiton"}}
\item{N}{ number of elements of the sample. It is also the dimention of the random orthogonal matrix in \code{rom}.}
\item{Y}{ a vector of data. It can also be a vector 1:N referring to the IDs of observations.  }
  \item{perms}{ number of random permutations. If it is a list, it has two elements \code{number} (the number of random permutation requested) and \code{seed} (the seed to be set when start generating. it is useful for reproducibility)
  If perms > number of all possible flips, then compute the complete space.
}
\item{T}{ the (possibly multivariate) permutation space as returned, for example by \code{flip}}
\item{obs.only}{ logical. If \code{TRUE} only the p-value for observed test statistic is returned, otherwise the whole space is computed}
\item{tail}{Tail of the distribution being significant for H1. See also argument \code{tail} in \code{\link{flip}} }
\item{testType}{ See argument \code{testType} in \code{\link{flip}}}
\item{Strata}{ See argument \code{testType} in \code{\link{flip}}}

}

\details{
\code{rom} implements the algorithm of Stewart (1980). The function is compiled in C++.
}
\author{
Livio Finos, Aldo Solari and Marco Rinaldo
}

\references{
Pesarin (2001) Multivariate Permutation Tests with Applications in Biostatistics. Wiley, New York.

Stewart, G. W. (1980). The efficient generation of random orthogonal matrices with an
application to condition estimators. SIAM Journal on Numerical Analysis 17, 403-409.

}

\seealso{
\code{\link{flip}}
}
\examples{
#10 random elements of the orbit of a one-sample test
make.signSpace(5, 10)

#All elements of the orbit of a one-sample test (the size of the space is 2^5 < 1000)
make.signSpace(5, 1000)

\dontrun{
#A random rotation matrix of size 3
(r=rom(3))
#verify that it is orthogonal:
r%*%t(r)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
