
#' info$moves - The vector into which moves should be placed (adjacent water hole numbers to move to, or
#'              0 for search)
#' info$mem - A field for story information that you wish to access in later turns
#' readings - a vector giving salinity, phosphate and nitrogen reading from croc's sensors for this turn
#' positions - a vector giving position of tourist 1, tourist 2 and you. If a tourist has been eaten this turn, the
#'            position is multiplied by -1, If they were eaten in a previous turn it is NA.
#' edges - a matrix giving he edge paths between water holes.
#' dists - a list of three matrices giving the mean
#'         and standard deviation of readings for salinity, phosphate and nitrogen respectively
#'         at each waterhole.
#'
#' return the info vector with updated moves (any desired change to mem)
hmmWC=function(info,readings,positions,edges,dists){
  d=max(edges)
  if (info$mem$status==0) {
    # If first turn, set up transition matrix and state vector in mem
    info$mem$T=makeTransitionMatrix(edges)
    info$mem$state=rep(1,d)
    info$mem$state[positions]=0
    info$mem$state=info$mem$state/sum(info$mem$state)
    info$mem$nextStep=makeNextStepMatrix(edges)
    info$mem$status=2
  }
  else if (info$mem$status==1) {
    # nextStep and T can be reused
    info$mem$state=rep(1,d)
    info$mem$state[positions]=0
    info$mem$state=info$mem$state/sum(info$mem$state)
    info$mem$status=2
  }
  else {
    # Multiply transition probabilities into state probabilities
    info$mem$state=info$mem$state%*%info$mem$T
  }

  probs=rep(0,d)
  seen=F
  if (!is.na(positions[1]) && positions[1]<0) {
    probs[-positions[1]]=1
    seen=T
  }
  # If they are in the same place & eaten we only need to do it once
  else if (!is.na(positions[2]) && positions[2]<0){
    probs[-positions[2]]=1
    seen=T
  }
  if (!seen) {
    probs=sapply(1:d,function(w){
      dnorm(readings[1],dists[[1]][w,1],dists[[1]][w,2])*
        dnorm(readings[2],dists[[2]][w,1],dists[[1]][w,2])*
        dnorm(readings[3],dists[[3]][w,1],dists[[1]][w,2])
    })
    if (!is.na(positions[1]))
      probs[positions[1]]=0
    if (!is.na(positions[2]))
      probs[positions[2]]=0

    probs=probs/sum(probs)
  }
  info$mem$state=info$mem$state*probs
  info$mem$state=info$mem$state/sum(info$mem$state)
  mostProbable=which.max(info$mem$state)
  moveOne=info$mem$nextStep[positions[3],mostProbable]
  if (moveOne==0) {
    # If we search, we are at most probable
    # If the game continues, croc is not there!
    info$mem$state[mostProbable]=0
    mostProbable=which.max(info$mem$state)
  }
  moveOneLoc=moveOne
  if (moveOne==0)
    moveOneLoc=positions[3]
  moveTwo=info$mem$nextStep[moveOneLoc,mostProbable]
  if (moveTwo==0) {
    # If we search, we are at most probable
    # If the game continues, croc is not there!
    info$mem$state[mostProbable]=0
  }

  info$moves=c(moveOne,moveTwo)
  return (info)
}

makeTransitionMatrix=function(edges) {
  d=max(edges)
  out=matrix(rep(0,d*d),ncol=d)
  for (i in 1:nrow(edges)) {
    out[edges[i,1],edges[i,2]]=out[edges[i,2],edges[i,1]]=1
  }
  for (i in 1:d) {
    out[i,i]=1
    out[i,]=out[i,]/sum(out[i,])
  }
  return (out)
}

makeNextStepMatrix=function(edges){
  d=max(edges)
  out=matrix(rep(NA,d*d),ncol=d)
  for (i in 1:d) {
    out[i,i]=0
  }
  for (i in 1:(d-1)) {
    for (j in (i+1):d) {
      out[i,j]=bfs(i,j,edges)
      out[j,i]=bfs(j,i,edges)
    }
  }
  return (out)
}

bfsNode=function(id,from){
  if (is.na(from$first))
    first=id
  else
    first=from$first
  list(id=id,first=first)
}
makeStart=function(id) {
  list(id=id,first=NA)
}
getNeighbors=function(id,edges){
  c(edges[which(edges[,1]==id),2],edges[which(edges[,2]==id),1])
}
bfs=function(i,j,edges){
  frontier=list(makeStart(i))
  visited=c()

  repeat {
    nextNode=frontier[[1]]
    if (nextNode$id==j)
      return (nextNode$first)
    visited=c(visited,nextNode$id)
    frontier=frontier[-1]

    neighbor_ids=getNeighbors(nextNode$id,edges)
    newNodes=rep(T,length(neighbor_ids))
    for (n in 1:length(neighbor_ids)) {
      if (neighbor_ids[n] %in% visited)
        newNodes[n]=F
    }
    if (any(newNodes)){
      neighbor_ids=neighbor_ids[newNodes]
      insertable=lapply(neighbor_ids,function(n){
        bfsNode(n,nextNode)
      })
      frontier=append(frontier,insertable)
    }
  }
}
