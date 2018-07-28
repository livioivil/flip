#==========================================================
# CLASS DEFINITION *** CLASS DEFINITION *** CLASS DEFINITION
#==========================================================
# detach(unload=T)
# setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrmatrixOrNULL", c("numeric","matrix", "NULL"))
setClassUnion("arrayOrNULL", c("array", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
setClassUnion("numericOrmatrixOrcharacterOrNULL", c("numeric","matrix", "NULL","character"))
setClassUnion("envOrNULL", c("environment", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))


# Can extend and create with new("flip.object", ...)
#' Class flip.object
#'
#' The class flip.object is the output of a call to \code{\link{flip}}, \code{\link{flipMix}}, \code{\link{npc}}, \code{\link{flip.adjust}} etc. 
#' 
#' @name flip.object-class
#' @rdname flip.object-class
#' @exportClass flip.object
setClass("flip.object",
  representation(
    res = "data.frameOrNULL",
    call = "call",
	permP="arrayOrNULL",
	permT="arrayOrNULL",
	permSpace="listOrNULL",
	permY="arrayOrNULL",
    #functions = "environment",#"list",
    #subsets = "listOrNULL",
    #structure = "listOrNULL",
    #weights = "listOrNULL",
    tail = "numericOrmatrixOrcharacterOrNULL",
    #Z = "matrixOrNULL",
    #directional = "logical",
    data = "listOrNULL",
    call.env = "envOrNULL"
    #model = "character"
  )
  # ,prototype = list(
  #   res = NULL,
  #   permP=NULL,
  #   permT=NULL,
  #   permSpace=NULL,
  #   permY=NULL,
  #   data=NULL,
  #   call.env=NULL
  # )
)

setMethod("initialize", "flip.object",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object
            })

#==========================================================
# Function "show" prints a "flip.object" object
#==========================================================
#' @export
#' @title show
#' @param object a flip-object
#' @param ... additional arguments to be passed
#' @return NULL
#' @describeIn summary prints same as \code{summary}
#' 
setMethod("show", "flip.object", function(object)
{
  summary(object)
})


# @export
# @title summary
# @description prints information about the flip-object
# @aliases summary result show 
# setGeneric("summary")
#' Prints information about the flip-object.
# @description Prints information about the flip-object.
#' @name flip.object-class
#' @rdname flip.object-class
#' @param object a flip-object
#' @param star.signif If \code{TRUE} (default), it puts stars on the significant tests
#' @param only.p.leq Shows only tests with a p-value lower than \code{only.p.leq}. The default \code{NULL} is equivalent to set \code{only.p.leq=1}.
#' @param ... additional arguments to be passed
#' @return NULL
#' @aliases summary,flip.object-method
setMethod("summary", "flip.object", function(object,star.signif=TRUE,only.p.leq=NULL,...)
{
  nperms= as.list(object@call$perms)
  #cat(" \"flip.object\" object of package flip\n")
  cat(" Call:\n ")
  cat(deparse(object@call), "\n")
  # cat(ifelse(is.null(nperms$seed),"all",""), nperms$B, ifelse(is.finite(nperms$seed),"random",""), "permutations.\n",
	# ifelse(is.finite(nperms$seed),paste("(seed: ",is.finite(nperms$seed)," )",sep=""),""))
  cat(object@permSpace$B-1, "permutations.",sep=" ")
  cat("\n")
  if(!is.null(only.p.leq))  object@res=object@res[object@res[,ncol(object@res)]<=only.p.leq,,drop=FALSE]

  if(!is.null(object@res)&&(nrow(object@res)>0)){

    if(is.null(star.signif)) star.signif=TRUE
    if(is.logical(star.signif)) {
      if(star.signif) {
        #takes the last column among raw and Ajusted p-value
        column=c("p-value",names(object@res)[grep("Adjust",names(object@res))])
        column=column[length(column)]
      }
    } else { column=star.signif; star.signif=TRUE}
    if(star.signif){object@res$"sig."=sapply((object@res[,column]<=.05)+
                                    (object@res[,column]<=.01)+
                                    (object@res[,column]<=.001),function(x) paste(rep("*",x),collapse=""))
    }
  }
  result(object,...)
})


#==========================================================
# Functions to extract relevant information from
# a flip.object object
#==========================================================
# @export
# @title result
# @param object a flip-object
# @param ... additional arguments to be passed
# @return NULL
# @description prints information about the flip-object  
#' Prints information about the flip-object.
#' @name flip.object-class
#' @rdname flip.object-class
#' @exportMethod result
setGeneric("result", function(object,...) standardGeneric("result"))


# @export
#' @rdname flip.object-class
#' @aliases result,flip.object-method
#' @param object a flip-object
#' @param ... additional arguments to be passed
#' @return NULL
setMethod("result", "flip.object",
  function(object,...) {
    if(!is.null(object@res)&&(nrow(object@res)>0)){
      for(i in c("p-value",names(object@res)[grep("Adjust",names(object@res))])){
        lower=(object@res[,i]<.0001)
        object@res[,i]=formatC(object@res[,i],digits=4,drop0trailing=FALSE,format="f")
        object@res[lower,i]="<.0001"
      }
  }
      print(object@res, digits = 4)
})

# #==========================================================
# setGeneric("subsets", function(object, ...) standardGeneric("subsets"))
# setMethod("subsets", "flip.object", function(object, ...) {
  # object@subsets
# })



#==========================================================
# @export
# @title p.value
# Returns a vector of p-values from a flip-object.
# @name flip.object-class
# @rdname flip.object-class
# @exportMethod p.value
#' Returns a vector of p-values from a flip-object.
#' @name flip.object-class
#' @rdname flip.object-class
#' @exportMethod p.value
#' @param object a flip-object
#' @param ... additional arguments to be passed
#' @return vector of p-values

setGeneric("p.value", function(object, ...) standardGeneric("p.value"))


# @export
#' @rdname flip.object-class
#' @aliases p.value,flip.object-method
setMethod("p.value", "flip.object",
  function(object) {
    x=object@res[,"p-value"]
	names(x)=names(object)
	x

  }
)


#==========================================================
#it concatenates flip objects
#warning("More than one test statistic is imputed, only the first perms space will be stored.")

#' cFlip
#'
#' @param ... arguments
#' @export
#' @return res
cFlip <- function(...) {
  res=list(...)[[1]]
    if(length(list(...))>1){
		nperms=sapply(list(...),function(xx) nrow(xx@permT))
		if(length(unique(nperms))>1) {
      warning("The flip-objects have different number of permutations, the minimum number will be retained for each test.")
      nperms=min(nperms)
		} else nperms=nperms[1]

    for(i in 2:length(list(...)))  res@permT=cbind(res@permT[1:nperms,,drop=FALSE],list(...)[[i]]@permT[1:nperms,,drop=FALSE])

		res@tail = as.vector(unlist(sapply(1:length(list(...)), function(i)  rep(if(is.null(list(...)[[i]]@tail)) 0 else list(...)[[i]]@tail,length.out=ncol(list(...)[[i]]@permT))
                      )))
		# migliore questo output, ammettere la presenza di altri elementi in extraInfoPre
		resNames=unique(unlist(sapply(list(...),function(xx) colnames(xx@res))))
		resNames=c(setdiff(resNames,c("Stat","p-value")),c("Stat","p-value"))
		res@res[,setdiff(resNames,colnames(res@res))]=NA
		res@res=res@res[,resNames]
		for(i in 2:length(list(...)))  {
      res@res[nrow(res@res)+(1:nrow(list(...)[[i]]@res)),colnames(list(...)[[i]]@res)]=list(...)[[i]]@res
		}
	}
	res
}

#==========================================================
# @export
# @rdname flip.object-class
# @description size of permutation space: number of permutations X number of variables
# @param object a flip-object
# @param ... additional arguments to be passed
# @return NULL
#' 
#' Size of permutation space: number of permutations X number of variables.
#' @name flip.object-class
#' @rdname flip.object-class
#' @exportMethod plot
#' @param object a flip-object
#' @param ... additional arguments to be passed
#' @return NULL
#' 
setGeneric("size", function(object, ...) standardGeneric("size"))

# @export
#' @rdname flip.object-class
#' @aliases size,flip.object-method
# @export
setMethod("size", "flip.object",
  function(object) {
    dim(object@permT)
  }
)
# ==========================================================
# setGeneric("dim", function(object, ...) standardGeneric("dim"))
# setMethod("dim", "flip.object",
  # function(object) {
    # c(object@nperms$B , dim(object@res)[1])
  # }
# )


#==========================================================
# The subsetting methods for "flip.object"
#==========================================================
#' Operators acting on vectors, matrices, arrays and lists to extract or replace parts.
#' @name flip.object-class
#' @rdname flip.object-class
# @exportMethod "["
#' @param object a flip-object
#' @param ... additional arguments to be passed
#' @return NULL
#' @aliases [,flip.object-method
# 
setMethod("[", "flip.object",
            function(x, i, j,...,drop)
{
  iii=i
	if(is.character(i) && !all(i %in% names(x))){
		search=which(!(i %in% names(x)))
		extended= lapply(i, function(ii) names(x)[if(ii %in% names(x)) ii else grep(ii, names(x))] )
		i=unlist(extended)
	}


  if (all(i %in% names(x)) ||
          all(i %in% 1:length(x)) ||
          all(i %in% -1:-length(x)) ||
          (is.logical(i) && (length(i)== length(x)))) {
    x@res <- x@res[i, ,drop=FALSE]
    if (!is.null(x@permP)) #if(i <= ncol(x@permP))
		x@permP <- x@permP[,i,drop=FALSE]
    if (!is.null(x@permT)) #if(i <= ncol(x@permT))
		x@permT <- x@permT[,i,drop=FALSE]
    if (!is.null(x@tail)) # if((i <= length(as.vector(x@tail))) || (length(as.vector(x@tail))==1))
								x@tail <- x@tail[min(length(as.vector(x@tail)),1)]
     if("data"%in%slotNames(x)) if (!is.null(x@data)) {
       #le colonne di Y non sono le stesse delle colonne di permT (a meno che non ci sia un'unica colonna X)
       search=which(!(iii %in% colnames(x@data$Y)))
       extended= lapply(iii, function(ii) colnames(x@data$Y)[if(ii %in% colnames(x@data$Y)) ii else grep(ii, colnames(x@data$Y))] )
       i=unlist(extended)
      if(!is.null(x@data$Y)) x@data$Y <- x@data$Y[,i,drop=FALSE]
      if(!is.null(x@data$se)) x@data$se <- x@data$se[,i,drop=FALSE]
      if(!is.null(x@data$df.mod)) x@data$df.mod <- x@data$df.mod[,i,drop=FALSE]
      if(!is.null(x@data$df.res)) if(ncol(x@data$df.res)>1) x@data$df.res <- x@data$df.res[,i,drop=FALSE]
      if(!is.null(x@data$covs)) x@data$covs <- x@data$covs[,i,i,drop=FALSE]
      if(!is.null(x@data$Su)) x@data$Su <- x@data$Su[i,i,drop=FALSE]
      if(!is.null(x@data$dispersion)) if(ncol(x@data$dispersion)>1) x@data$dispersion <- x@data$dispersion[,i,drop=FALSE]
      if(!is.null(x@data$coeffWithin)) x@data$coeffWithin <- x@data$coeffWithin[,i,drop=FALSE]
    }
    #if (!is.null(x@weights)) x@weights <- x@weights[i]
    x
  } else {
    stop("invalid index set", call. = FALSE)
  }
})

#' @export
#' @rdname flip.object-class
setMethod("[[", "flip.object",
            function(x, i, j,...,exact)
{
   x[i]
})

#==========================================================
# The length method for "flip.object"
#==========================================================
#' @rdname flip.object-class
#' @aliases length,flip.object-method
#' 
setMethod("length", "flip.object",
            function(x)
{
  nrow(x@res)
})



#==========================================================
# The names and alias methods for "flip.object"
# (applies to pathwaynames)
#==========================================================

#' @rdname flip.object-class
#' @aliases names,flip.object-method
setMethod("names", "flip.object",
            function(x)
{
  rownames(x@res)
})


#' @rdname flip.object-class
#' @aliases names<-,flip.object-method
setMethod("names<-", "flip.object",
            function(x, value)
{
  rownames(x@res) <- value
  if (!is.null(x@permP)) colnames(x@permP) <- value
  if (!is.null(x@permT)) colnames(x@permT) <- value
  x
})



#==========================================================
# A sort method for "flip.object"
#==========================================================
# @export
# @rdname flip.object-class
# setGeneric("sort")

# @export
#' @rdname flip.object-class
#' @aliases sort,flip.object-method
#' 
setMethod("sort", "flip.object",
  function(x, decreasing = FALSE ) {
      ix <- order(p.value(x), decreasing=decreasing)
    x[ix]
  }
)



#==========================================================
# Multiple testing correction for "flip.object" object
#==========================================================

# @export
# @rdname flip.object-class
# setGeneric("p.adjust", function(p, method = p.adjust.methods, n = length(p)) standardGeneric("p.adjust"))

# @export
#' @rdname flip.object-class
#' @aliases p.adjust,flip.object-method
#' 
setMethod("p.adjust", matchSignature(signature(p = "flip.object"), p.adjust),
  function(p, method = p.adjust.methods, n = length(p)) {
    method <- method[1]
    method <- p.adjust.methods[grep(method, p.adjust.methods, ignore.case=T)]
    if(length(method)==(0))   # this is just to get a good error message
      method <- match.arg(method)
	if (missing(n))
      p@res <- cbind(p@res, p.adjust(p.value(p), method=method))
    else
      p@res <- cbind(p@res, p.adjust(p.value(p), method=method, n=n))

	colnames(p@res)[length(colnames(p@res))]=paste("Adjust:",method,sep="")

    p
  }
)



#==========================================================
# Histogram method to visualize permutations
#==========================================================
# Method hist.
# Shows the histogram of the distribution of the test statistics (computed under the null hypothesis).
# 
# @export
# @docType methods
# 
# @aliases hist
# @param x a flip.object
# @param ... additional arguments to be passed
# @return a \code{hist} object
# @rdname flip.object-methods
# @docType methods
#'
#' Method hist.
# @name flip.object-class
# @rdname flip.object-class
# @exportMethod hist  

setGeneric("hist", function(x,...) standardGeneric("hist"))

# @name hist
#' @aliases hist,flip.object-method
#' @rdname flip.object-methods
setMethod("hist", "flip.object", function(x, ...)  {

  flip.hist <- function(x, breaks=100, main=NULL, xlab = "Test Statistics", ...) {

     if (length(x) > 1){

       f<-function(k=length(x),yxratio=1.25){
       n.cl=ceiling(sqrt(k*yxratio) )
       n.rw=ceiling(k/n.cl)
       if((n.cl-1)*n.rw>=k) n.cl=n.cl-1
       c(n.cl,n.rw)}
       mfrow.now=par("mfrow")
       par(mfrow=f(k=length(x),yxratio=1.25))
        res=sapply(1:length(x),function(i)flip.hist(x[i]))
       par(mfrow=mfrow.now)

       return(invisible(res))
#      stop("length(object) > 1. Please reduce to a single test result")
  }
    # if (is.null(x@weights))
      # weights <- rep(1, size(x))
    # else
      # weights <- x@weights[[1]]
    # if (is.null(x@subsets))
      # subset <- seq_len(size(x))
    # else
      # subset <- x@subsets[[1]]

    #recalculate <- x@functions$permutations(subset, weights)
    if(is.null(main))  main=names(x)[1]
    Q <- x@permT[1,]
    nperm <- length(x@permT-1)
    hst <- hist(x@permT,plot=FALSE, breaks = breaks)

      cols=rep("#00A08A",length(hst$breaks)-1)
      cols.brd=cols
      pts=.setTail(cbind(c(x@permT[1,],hst$mids)),x@tail)
      cols[which(pts[-1]>=pts[1] )]="#F98400"
#       cols.brd[which(pts[-1]>=pts[1] )]="#FF0000"

     plot(hst,          xlim = c(1.1 * min(0, x@permT), 1.1 * max(x@permT)),
      main = main, xlab = xlab,col=cols,
      , border= cols.brd#"#F2AD00"
      , ...)#"#00A08A"
     if(is.null(list(...)$freq) & is.null(list(...)$probability))
       h <- max(hst$counts) else {
         if(c(1-list(...)$freq,list(...)$probability)>0 )
           h <- max(hst$density) else
             h <- max(hst$counts)
       }
redUnipd="#FF0000"
    lines(  c(Q, Q), c(h/2, 0) , lwd=2,col=redUnipd)
    points( Q,0 , lwd=2,col=redUnipd,pch=21,bg=redUnipd)
    text( Q, h/2, 'Observed\ntest\nstatistic' , pos=3,col=redUnipd)

    # No output
    invisible(list(statistic = Q, histogram = hst))
  }

  flip.hist(x,...)
})


# #==========================================================
# # Graph plot for flip-oject
# #==========================================================


# @export
#' Method plot.
#' @name flip.object-class
#' @rdname flip.object-class
#' @exportMethod plot
#' 
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

# @export
#' @rdname flip.object-class
#' @aliases plot,flip.object-method
setMethod("plot", "flip.object",
 function(x, y, ...) {
#setMethod("plot", "flip.object", function(x, y, ...) {
  if(!exists("main")) main=NULL
  if(!exists("xlab")) xlab = NULL
  if(!exists("ylab")) ylab=NULL

  plot.flip <- function(x, y=NULL, main, xlab, ylab, which.PCs=1:2,col="#F98400"#"orange
                        ,bg="#F2AD00" #"darkgrey"
                        ,pch=21,asp=1,...){
    #draw <- function(x, main, xlab, ylab,...){
    if (length(x)==1 ){
      hist(x, ...)
    } else if (length(x)==2 ){
      plot(x@permT, xlim=range(x@permT[,1]), ylim=range(x@permT[,2]),
           xlab=colnames(x@permT)[1],
           ylab=colnames(x@permT)[2],
           main= "Permutation Space" ,
           bg=bg, col=col,pch=pch,asp=asp,...)

      points(x@permT[1,1],x@permT[1,2],col="#F2AD00"#"darkgrey"
             ,bg="#00A08A"#"blue"
             ,cex=2,lwd=2,pch=21)
      text(x@permT[1,1],x@permT[1,2],labels="ObsStat",col="gray30")
    } else {
      keep=apply(x@permT,2,function(x)length(unique(x))>1)
      x@permT=x@permT[,keep]
      pc=prcomp(x@permT,scale. =FALSE,center=FALSE)
#       reduce the data
      pc$x=pc$x[,which.PCs]
      pc$sdev=pc$sdev[which.PCs]
      pc$rotation=pc$rotation[,which.PCs]

      #makes obs to be always on top-right quadrant:
      pc$rotation[,1]=pc$rotation[,1]*sign(pc$x[1,1])
      pc$rotation[,2]=pc$rotation[,2]*sign(pc$x[1,2])
      pc$x[,1]=pc$x[,1]*sign(pc$x[1,1])
      pc$x[,2]=pc$x[,2]*sign(pc$x[1,2])

      pc$x=pc$x[,1:2]/pc$sdev[1:2]
      datapc=pc$rotation[,1:2]*sqrt(nrow(pc$rotation))*1.3

      plot(pc$x, xlim=range(c(pc$x[,1],datapc[,1])), ylim=range(c(pc$x[,2],datapc[,2])),
           xlab=paste("PC1 (",round(pc$ sdev [1]^2 /sum(pc$ sdev ^2) *100,2)," %)",sep=""),
           ylab=paste("PC2 (",round(pc$ sdev [2]^2 /sum(pc$ sdev ^2) *100,2)," %)",sep=""),
           main= "PCA of Permutation Space" ,
           bg=bg, col=col,pch=pch,asp=asp)

      points(pc$x[1,1],pc$x[1,2],col="#F2AD00"#"darkgrey"
             ,bg="#00A08A"#"blue"
             ,cex=2,lwd=2,pch=21)

      arrows(0,0,datapc[,1],datapc[,2],col=ifelse( p.value(x)<.05,"#FF0000"#"red"
                                                   ,"#00A08A"#"blue"
                                                   ),lwd=2,angle=15,length=.1)
      text(datapc[,1],datapc[,2],labels=rownames(datapc),col="gray30")
      text(pc$x[1,1],pc$x[1,2],labels="ObsStat",col="gray30")

      # lam <- pc$sdev[1:2] #* sqrt(dim(pc$x)[1])
      # #plot(pc$x[, 1:2]/lam)
      # pc$x[,1:2]=pc$x[,1:2] / lam
      # pc$rotation[,1:2]=pc$rotation[,1:2]*lam
      # plot(pc$x[,1],pc$x[,2],lwd=1,pty="o",xlim=range(pc$x[,1])*1.2,ylim=range(pc$x[,2])*1.2,
      # xlab=paste("PC1 (",round(pc$ sdev [1]^2 /sum(pc$ sdev ^2) *100,2)," %)",sep=""),
      # ylab=paste("PC2 (",round(pc$ sdev [2]^2 /sum(pc$ sdev ^2) *100,2)," %)",sep=""),col="gray",pch=21,bg="gray")
      # points(pc$x[1,1],pc$x[1,2],col="red",lwd=3,pch=21,bg="red")
      # text(pc$x[1,1]*1.1,pc$x[1,2]*1.1,col="red","Obs")
      # arrows( 0, 0, 2*pc$rotation[,1], 2*pc$rotation[,2], lwd=1,col="gray")
      # text(2.1*pc$rotation[,1], 2.1*pc$rotation[,2], rownames(pc$rotation), cex=1.5,col="black")
      # title("PCA of Permutation Space")
    }
  }
  plot.flip(x,y=NULL, main=main, xlab=xlab, ylab=ylab,...)
})



# #==========================================================
# # get elements of flip-object
# #==========================================================

#' getFlip
#'
#' @param obj object?
#' @param element element?
#'
#' @return res?
getFlip <- function(obj,element){
  if(element%in%slotNames(obj))
    return(slot(obj,element))

  if(element%in%names(obj@res))
    return(obj@res[element])

  if(tolower(element)%in%c(tolower("Adjust:"),tolower("Adjust")))
    return(obj@res[grep("Adjust",colnames(obj@res))])

  if(!is.null(obj@data)){
    if(element%in%names(obj@data))
      return(obj@data[element])

    if(substr(element,1,5)=="data$")
      return(obj@data[substr(element,6,nchar(element))])

  } else if(!is.null(obj@call.env$data)){

    if(element%in%names(obj@data))
      return(obj@data[element])

    if(substr(element,1,5)=="data$")
      return(obj@data[substr(element,6,nchar(element))])

  }

  if(substr(element,1,4)=="res$")
    return(obj@res[substr(element,5,nchar(element))])

  if(element%in%c("nperms","perms","B"))
    return(obj@permSpace$B)
}
