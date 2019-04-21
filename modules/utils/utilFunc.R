is.df.empty = function(df){
  if(!any(dim(df) == 0)){
    return (df)  
  } else {
    return (data.frame("Empty" =  " No rows in table"))
  }
}