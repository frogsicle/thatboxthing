# dummy shape
ashape <- matrix(c(1,1,0,
                   1,0,0,
                   1,0,0), 3, 3)


### everything needed to get scorable, cannonical shapes
# total scoring
score.shape <- function(ashape){
  s.vert <- score.vert(ashape)
  s.horiz <- score.horiz(ashape)
  return(c(s.vert, s.horiz)) 
}

# directional scoring
score.vert <- function(ashape){
  score <- sum(rowSums(ashape) * nrow(ashape):1)
  return(score)
}

score.horiz <- function(ashape){
  return(score.vert(t(ashape)))
}

## home
# to top and fill
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

# to left and fill
home.shape.horiz <- function(ashape, padrow=13){
  tshape <- t(ashape)
  tshape <- home.shape.vert(tshape)
  ashape <- t(tshape)
  return(ashape)
}

# to top left
home.shape <- function(x, pad=13) {
  x <- home.shape.vert(x, pad)
  x <- home.shape.horiz(x, pad)
  return(x)
}

## rotate
# rotate once
rotate.cc <- function(ashape){
  out <- apply(ashape, 1, rev)
  return(out)
}


# all 8 rotations
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
## combining 
# rotate and home
get.comparables <- function(ashape){
  rotations <- rotate.all(ashape)
  homies <- lapply(rotations, home.shape)
  return(homies)
}

score.all <- function(homies){
  out <- sapply(homies, score.shape)
  rownames(out) <- c('vert','horiz')
  return(out)
}

get.cannonical <- function(ashape){
  homies <- get.comparables(ashape)
  scores <- score.all(homies)
  # vertical before horizontal scoring
  rankedscores <- order(scores[1,], scores[2,], decreasing=T)
  return(homies[[rankedscores[1]]])
}
