ashape <- matrix(c(1,1,0,
                   1,0,0,
                   1,0,0), 3, 3)

rotate.cc <- function(ashape){
  out <- apply(ashape, 1, rev)
  return(out)
}

score.shape <- function(ashape){
  return(sum(ashape)) #fix
}

home.shape.vert <- function(ashape, padrow=13){
  i <- 1
  so_far_only_zilch <- TRUE
  toremove <- c()
  while(so_far_only_zilch){
    if (rowSums(ashape)[i] == 0){
      toremove <- c(toremove, i)
    }else{
      so_far_only_zilch <- FALSE
    }
    i <- i + 1
    if (i >= padrow){
      print('no non-zeros found?')
      so_far_only_zilch <- FALSE
    }
  }
  if (length(toremove) > 0){ 
    ashape <- ashape[- toremove, ]
  }
  padding <- padrow - nrow(ashape)
  ashape <- rbind(ashape, matrix(rep(0, padding * ncol(ashape)), padding, ncol(ashape)))
  return(ashape)
}

home.shape.horiz <- function(ashape, padrow=13){
  tshape <- t(ashape)
  tshape <- home.shape.vert(tshape)
  ashape <- t(tshape)
  return(ashape)
}

rotate.all <- function(ashape){
  out <- list()
  i <- 1
  for (i in 1:4){
    out[[i]] <- ashape
    ashape <- rotate.cc(ashape)
  }
  ashape <- t(ashape)
  for (i in 5:8){
    out[[i]] <- ashape
    ashape <- rotate.cc(ashape)
  }
  return(out)
}
