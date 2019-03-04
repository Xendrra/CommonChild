CommonChild <- function(in1,in2){
  in1=utf8ToInt(in1)
  in2=utf8ToInt(in2)
  common<-function(in1,in2){ 
    h=1
    i = 1
    output=character()
    for (j in 1:length(in1)){
      for (k in h:length(in2)){
        if (in1[j]==in2[k]){
          output[i]=in1[j]
          i=i+1
          h=k+1
          break
        }
      }
    }
    output = intToUtf8(output)
    return(output)
  }
  output1=common(in1,in2)
  output2=common(in2,in1)
  if (length(output2)>length(output1)){
    print(output2)
  }else {print(output1)
  }
}





