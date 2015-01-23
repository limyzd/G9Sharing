g9share <- function (startdate, enddate) {
    
  days <- NULL
  diffnow <- NULL
  remain <- NULL
  begin <- NULL
  names <- c("Stacey","Zephy","D Lim","D Yeang", "David","Grace","Jia Xuan","Joice","Kai Ling", "Maria", "Sean")
  beginspace <- NULL
  diff<- NULL
  lengthrep <- NULL
  names2 <- NULL
  df <- NULL
  
  ##setting up dates sequence
  days <- seq(as.Date(startdate), as.Date(enddate) , by="1 day") 
    
  ##setting up the starting names
  diffnow <- (difftime(strptime(startdate, format = "%Y-%m-%d"),strptime("2015-01-23",format = "%Y-%m-%d"),units="days"))
  remain <- as.numeric(diffnow, units="days") %% 11  
  begin <- names[c((remain+1):11)]
  
  ##setting up spaces at start
  beginspace <- rep("",length.out=(as.POSIXlt(startdate)$wday))
  
  ##completing sequence of names
  diff <- (difftime(strptime(enddate, format = "%Y-%m-%d"),strptime(startdate,format = "%Y-%m-%d"),units="days")) + 1
  lengthrep <- as.numeric(diff, units="days") - length(begin)
  
  if (lengthrep<1){
    names2 <- begin
  } else {
    names2 <- c(begin, rep_len(names, length.out=(lengthrep)))
  }
  
  ## formatting
  df<- matrix(c(beginspace,paste(days,names2)),ncol=7,byrow=TRUE)
  colnames(df) <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
  
  print(df)
}