isFirstDown <- FALSE

getDown <- function(down){
  down <- as.integer(down)
  if(isFirstDown){
    return (1)
  }else{
    if(down > 3){
      return(1)
    }else{
      return(down + 1)
    }
    
  }
}

getDownAndDistance <- function(down,dist, gain, fieldPos){
  gain <- as.integer(gain)
  dist <- as.integer(dist)
  down <- as.integer(down)
  fieldPos <- as.integer(fieldPos)
  
  if (gain >= dist){
    newDown = 1
    newDist = 10
    
  }else{
    if(down > 3){
      newDown = 1
    }else{
      newDown <- down + 1
    }
    newDist <- dist - gain
    
  }
  newFieldPos = fieldPos - gain
  return(list(down = newDown, dist = newDist, fieldPos =newFieldPos ))
}

getCurrentPlay <- function(df,team){
  df2 <- df[df$offTeam == team,]
  curPlay <- nrow(df2) + 1
  
  
  return(curPlay)
}

getCurrentDrive <- function(df,team){
  curDrive = 1
  if (nrow(df) >0){
    if (df$offTeam[1] != team){
      if (team %in% df$offTeam){
        df2 <- df[df$offTeam == team,]
        curDrive = as.numeric(df2$drive[1])+1
      }else{
        curDrive = 1
      }
      
    }else{
      df2 <- df[df$offTeam == team,]
      curDrive = as.numeric(df2$drive[1])
    }
  }
  return(curDrive)
}