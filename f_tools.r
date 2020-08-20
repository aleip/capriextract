getDefaults <- function(f){
  for (i in formalArgs(f)){
    
    print(sys.nframe())
    print(sys.call(-1))
    print(parent.frame())
    print(i)
    print(formals(f)[[i]])
    assign(i, formals(f)[[i]], envir = parent.frame())
  }
}
checkf <- function(f){
  getDefaults(f)
}

convertarguments2values<-function(f=NULL, ...){
  
  # Function that helps debugging - converts all arguments into values
  #          in the global environment so that the function can
  #          be checked directly line by line
  
  if(! is.null(f)){
    # Get first defaults - then overwrite with given indices
    for (i in formalArgs(f)){
      assign(i, formals(f)[[i]], envir = parent.frame())
    }
  }
  
  l<-as.list(match.call())
  save(l, file="l.rdata")
  for (i in 2:length(l)){
    #cat("\n", i, "Assigning ", l[[i]], " to ", paste(names(l)[i],collapse="") , "...")
    cat("\nstart",i,":", names(l)[i])
    n<-names(l)[i]
    if(! exists("n")){
      cat("\nNo argument name given for ", l[[i]],". Tentatively set to ",l[[i]])
      n<-l[[i]]
    }
    g<-eval(l[[i]])
    #cat(". ", n, " is character", g)
    assign(n, g, envir=parent.frame())
  }
  #print(as.list(match.call()))
  
  save(l, f, file="l.rdata")
  return(l)
}


# getDefaults <- function(f=NULL){
#   
#   # Get all arguments requested and their default values
#   va <- NULL
#   if(! is.null(f)) {
#     a <- formals(f)
#     
#     va <- data.table(
#       args = formalArgs(f), 
#       values = as.character(formals(f))
#       )
#   }
#   
#   return(va)
# }

