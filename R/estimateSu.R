.estimateUni <- function(Y,Z,S,testedWithin,tol=0.001,max.int=10){
  N=length(S)
  Su=rep(NA,ncol(data$Y))
  names(Su) <- colnames(data$Y)
  
  I = diag(N) 
  
  corStat= ((ncol(data$X)==1))
  if(corStat) data$X=scale(data$X)
  
  for(j in which(!is.na(apply(data$Y,2,mean)))){
    
    ## estimating the random effects variances
    V = diag(S[,j])
    
    #######stime di Su uguali tra clusters
    Sus = sapply(unique(Z), function(k)  var(data$Y[Z==k,j]) -mean(S[Z==k,j]))  # stima comune a tutti i gruppi
    Su[j] = sum(Sus * nINclusters)/sum(nINclusters)
    if(Su[j]<0){Su[j]=0}
    diag(V) = diag(V) + Su[j]
    V = diag(1/sqrt(diag(V)))
    
    Z=V%*%data$Z
    #E = eigen(I-H0) :
    E = eigen(I-Z%*%solve(t(Z)%*%Z)%*%t(Z))
    if(any(!is.real(E$vectors))) { #make the matrix symmetric if (due to some weakness of R) the eivenvectors are not all real
      temp=I-Z%*%solve(t(Z)%*%Z)%*%t(Z)
      temp=(temp+t(temp))/2
      E = eigen(temp)
    }
    
    E$vectors=array(as.real(E$vectors),dim(E$vectors))
    #print(dim(E$vectors))
    #print(length(E$values))
    L0 = E$vectors[,(E$values > 1e-1)]  ###TODO: vale solo perche' tutti gli autovalori sono solo 1 o 0
    
    r0 = t(L0)%*%V%*%data$Y[,j] 
    X =  t(L0)%*%V%*%data$X 
    
    
    if(corStat){
      r0 = scale(r0)
      X =  scale(X) 
      permT[,j] = (array(r0[permSpace$permID],dim(permSpace$permID)))%*%X		 
    } else {
      P = X%*%solve(t(X)%*%X)%*%t(X)
      IminusP= diag(N-q)-P
      
      for(b in 1:dim(permSpace$permID)[1]){
        permT[b,j] = t(r0[permSpace$permID[b,]])%*%P%*%r0[permSpace$permID[b,]]/(t(r0[permSpace$permID[b,]])%*%IminusP%*%r0[permSpace$permID[b,]])
      }
      # browser()
      # permT[,j] = t(array(r0[permSpace$permID],dim(permSpace$permID)))%*%P%*%t(array(r0[permSpace$permID],dim(permSpace$permID)))/(array(r0[permSpace$permID],dim(permSpace$permID))%*%IminusP%*%t(array(r0[permSpace$permID],dim(permSpace$permID))))
    }
  }
  #permT=round(permT*(N-p-q)/p,10)  #########TODO occhio qui, con numeri molto piccoli possono verificarsi problemi
  
  # set tail if given
  if(!missing(tail) && corStat) { permT = .setTail(permT,tail) }
}


###############################################33
.estimateSuMultiILS = function(Y,Z,S,tol=0.001,max.int=10){


	tr =function(x){return(sum(diag(as.matrix(x))))}
	## si: subject's standard errors
	n = nrow(Y) ; p = ncol(Y)
	Su = array(0,dim=c(p,p))
	TR = array(0,dim=c(max.int,1))
	bb=2 ; ID = 0
	si = array(0,dim=c(n,1))

	while(bb <= max.int & ID == 0){
		for(i in 1:n){#print(tr(S[i,,]))
                        si[i] = tr(S[i,,])+tr(Su)
		}
    
    #print(si)
		V = diag(as.vector(1/sqrt(si))) ; # W =  diag(as.vector(sqrt(si)))
		ys = V%*%Y ; Zs = V%*%Z
		Hs = diag(n)-Zs%*%solve(t(Zs)%*%Zs)%*%t(Zs)
    #force the symmetry (errors on approx)
    Hs = (Hs+t(Hs))/2
		Es=eigen(Hs)
		Ls = Es$vectors%*%diag(Es$values)
		Rs = t(Ls)%*%ys  				#; Rr = W%*%Rs
		G = t(Ls)%*%V
		w = diag(t(G)%*%G)
		SS = array(0,dim=c(p,p))

		for(i in 1:n){SS = SS + w[i]*S[i,,]}
		#Rs = W%*%Rs
		SSu = (t(Rs)%*%Rs-SS)/sum(w) 
		
		########## simmetrizza per non avere PARTI IMMAGINARIE dovute a scarsa approssimazione
		E = eigen((SSu+t(SSu))/2)
		E$values[E$values<0]=0
		if(length(E$values)==1) E$values=matrix(E$values)
		
		A = diag(E$values) 
		TR[bb] = sum(A)
		Su = E$vectors%*%A%*%t(E$vectors)  ; 
		if(abs(TR[bb]-TR[bb-1])<tol){ID = 1}
		bb = bb+1
	}
  colnames(Su)=rownames(Su)=colnames(Y)
  attr(Su,"n.iter")=bb-1
	attr(Su,"TR")=TR
	return(Su)
}