getLoopingArticles <- function(search, linkLimit){
  
  links <- page_backlinks("de","wikipedia", page=search, limit=linkLimit, namespaces=0)
  
  A <- c()
  B <- c()
  
  Count <- 1
  
  #Für jede verlinkung führe aus:
  for(link in links$query$backlinks){
    
    #Anzeigen vom Fortschritt
    print(paste((Count/length(links$query$backlinks))*100,"%","-",link$title, sep = " "))
    Count <- Count+1
    
    #nehme alle verlinkungen
    ref <- page_backlinks("de","wikipedia", page=link$title, limit=linkLimit, namespaces = 0)
    
    relate <- FALSE
    
    #hat diese verlinkung eine verlinkung zurück?
    for(refLink in ref$query$backlinks){
      if(refLink$title == search){
        A <- c(A, search)
        B <- c(B, link$title)
        relate <- TRUE
      }
    }
    
    #wenn es eine verlinkung zurück gibt
    if(relate){
      #nimm dir wieder alle verlinkungen
      for(refLink in ref$query$backlinks){
        ref2 <- page_backlinks("de","wikipedia", page=refLink$title, limit=linkLimit, namespaces = 0)
        
        #und suche nach zweiter verlinkungen
        for(refLink2 in ref2$query$backlinks){
          if(refLink2$title == link$title){
            A <- c(A, link$title)
            B <- c(B, refLink$title)
          }
        }
      }
    }
  }
  
  nodes <- data.frame(id=unique(B), label=unique(B))
  data <- data.frame(from=A, to=B)
  
  return(c(nodes, data))
}


getRecursiveLoop <- function(page, linklimit, layers, blockedPages=c()){
  if(layers > 0){
    
    links <- page_backlinks("de","wikipedia", page=page, limit=linkLimit, namespaces=0)
    
    blockedPages <- c(blockedPages, page)
    
    A <- c()
    B <- c()
    
    Count <- 1
    
    for(link in links$query$backlinks){
      print(paste((Count/length(links$query$backlinks))*100,"%","Layer:",layers,"Parent:",page,"-",link$title, sep = " "))
      Count <- Count+1
      
      relate <- FALSE
      ref <- page_backlinks("de","wikipedia", page=link$title, limit=linkLimit, namespaces = 0)
      for(refLink in ref$query$backlinks){
        if(refLink$title == page){
          A <- c(A, page)
          B <- c(B, link$title)
          relate <- TRUE
        }
      }
      
      if(relate){
        if((link$title %in% blockedPages) == FALSE){
          loop <- getRecursiveLoop(page=link$title, linklimit=linkLimit, layers=layers-1, blockedPages)
          if(length(loop) == 4){
            A <- c(A, as.vector(unlist(loop['from'], use.names=FALSE)))
            B <- c(B, as.vector(unlist(loop['to'], use.names=FALSE)))
          }
        } 
      }
    }
    
    nodes <- data.frame(id=unique(B), label=unique(B))
    data <- data.frame(from=A, to=B)
    
    return(c(nodes, data))
  } else {
    nodes <- data.frame(id=c(), label=c())
    data <- data.frame(from=c(), to=c())
    return(c(nodes, data))
  }
}