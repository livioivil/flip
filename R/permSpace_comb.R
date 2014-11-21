.make.CombSpace <- function(X,perms) {
  
  ##then it is a list anyway now
  perms <- perms[intersect(names(perms),c("B","seed","n","rotFunct"))]
  if(is.null(perms$n))  perms$n <- length(X)
  if(is.null(perms$B))  perms$B <- 1000
  
  
  grpFr<-table(X)
  coeffmult<-factorial(perms$n)/prod(factorial(grpFr))
  if (is.na(coeffmult>1E5)||coeffmult>1E5){
    return(.make.PermSpace(X,perms))
  }
    
  if(is.null(perms$rotFunct)){
    perms$coeffmult=coeffmult
    perms$grpFr<-grpFr
    #n ? il vettore contenente le numerosit? di ogni gruppo
  
    #perms$n ? il numero totale di osservazioni
    perms$J<-length(grpFr)
    #J ? il numero di gruppi
    vec<-1:perms$n
    #vec ? il vettore degli indici delle osservazioni
    
    perms$coeffmult<-factorial(perms$n)/prod(factorial(grpFr[1:perms$J]))
    
    M<-list(NA)
    for(j in 2:perms$J) {
      #ogni iterazione corrisponde a un gruppo
      M<-c(M, list(combn(vec[1:sum(grpFr[1:j])],grpFr[j])))
      #ad ogni iterazione attacco ad M tutti i modi di combinare gli elementi del vettore di osservazioni 
      #fino al gruppo j a gruppi di dimensione pari alla numerosit? del gruppo in questione
    }
    perms$M=M
    rm(M)
    

    #coeffmult ? il numero delle combinazioni necessarie e sufficienti
    #se B>coeffmult si prende B=coeffmult e in quel caso li esploro tutti da 1 a coeffmult
    
    if(perms$B<perms$coeffmult) {
      RJv<-rep(0,perms$B)
      for(i in 1:perms$B) {
        RJv[i]<-ceiling(runif(1,0,perms$coeffmult))
      }
    } else {
      RJv<-2:perms$coeffmult
      perms$B=perms$coeffmult
    }
    perms$RJv=RJv
    rm(RJv)
    
    
   perms$rComb <- function(RJ) {
      ii<-rep(0,perms$J)
      RJ=perms$coeffmult-RJ
      for(j in perms$J:2) {
        #ciclo for con decremento in cui ogni iterazione corrisponde ad un gruppo
        tmp<-(factorial(sum(perms$grpFr[1:(j-1)]))/prod(factorial(grpFr[1:(j-1)])))
        ii[j]<-RJ%/%tmp+1
        RJ<-RJ%%tmp
      }
      ids<-list(1:perms$n)
      rec<-list(NA)
      for(j in 2:perms$J) {
        ids[[j]]<-ids[[j-1]][-perms$M[[perms$J+2-j]][,ii[perms$J+2-j]]]
        rec[[j]]<-ids[[j-1]][perms$M[[perms$J+2-j]][,ii[perms$J+2-j]]]
      }
      yperm<-c(ids[[perms$J]],rec[perms$J:2])
      yperm=unlist(yperm)
      return(yperm)
    }
    
    
    perms$rnks=rank(X, na.last = TRUE, ties.method="first")
    perms$ox=order(X)
    
    perms$rotFunct <-function(i){
      return(data$Y[perms$ox[perms$rComb(perms$RJv[i])][perms$rnks],,drop=FALSE])
    }
    }
  perms$type="combination"
  return(perms)
}	  
