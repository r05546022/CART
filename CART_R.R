
data = read.csv("C:\\Users\\Tappy\\Desktop\\DATA3.csv");
attach(data);
data[,1] = as.factor(data[,1] )
len = length(data[,1])
wid = length(data[1,])


impu = function(DATA){
  
  DATA = data
  impurity = NULL
  cutpoint = NULL
  sol = 0;
  for(colnumber in 2:wid){
        
    impurity[colnumber-1] = 0
    cutpoint[colnumber-1] = min(DATA[,colnumber])
    i_t = 1-sum((table(DATA[,1])/sum(table(DATA[,1])))^2)
    for(i in seq(min(DATA[,colnumber])+0.001,max(DATA[,colnumber]),by = 0.001)){
      
      leftnode = subset(DATA[,c(1,colnumber)],DATA[,colnumber] >= i )
      rightnode = subset(DATA[,c(1,colnumber)],DATA[,colnumber] < i )
      
      if (length(leftnode[,1])==0){
        i_tleft = 0
      }else{
        i_tleft = 1-sum((table(leftnode[,1])/sum(table(leftnode[,1])))^2)
      }
      
      if (length(rightnode[,1])==0){
        i_tright = 0
      }else{
        i_tright = 1-sum((table(rightnode[,1])/sum(table(rightnode[,1])))^2)
      }
      
      P_l = length(leftnode[,1])/( length(leftnode[,1])+length(rightnode[,1]) )
      P_r = 1-P_l
      E_tc = P_l*i_tleft + P_r*i_tright
      
      if((i_t - E_tc) >impurity[colnumber-1] ){
        impurity[colnumber-1] = (i_t - E_tc)
        cutpoint[colnumber-1] = i
      } 
    } # i
  } # colnumber
  sol = c(which.min(impurity),cutpoint[which.min(impurity)])
  rightnode = subset(DATA,DATA[,which.min(impurity)+1] < cutpoint[which.min(impurity)] )
  leftnode = subset(DATA,DATA[,which.min(impurity)+1] >= cutpoint[which.min(impurity)] )
  List <- list("sol" = sol, "rightnode" = rightnode,"leftnode" = leftnode )
  return(List)
}
impu(data)$sol
D1.r = impu(data)$rightnode
D1.l = impu(data)$leftnode




