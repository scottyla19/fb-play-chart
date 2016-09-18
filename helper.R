getCurrentPlay <- function(df,team){
  df2 <- df[df$offTeam == team,]
  curPlay <- nrow(df2) + 1
  
  
  return(curPlay)
}

getCurrentDrive <- function(df,team){
  curDrive = 1
  if (nrow(df) >0){
    if (df$offTeam[1] != team){
      print("drive change")
      if (team %in% df$offTeam){
        df2 <- df[df$offTeam == team,]
        curDrive = as.numeric(df2$drive[1])+1
      }else{
        curDrive = 1
      }
      
    }else{
      print("same drive")
      df2 <- df[df$offTeam == team,]
      curDrive = as.numeric(df2$drive[1])
    }
  }
  return(curDrive)
}