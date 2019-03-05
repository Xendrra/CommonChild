CommonChild <- function(in1,in2){
  #funkce CommonChild nalezne nejdelsiho spolecneho potomka dvou materskych retezcu
  #vstupy...in1 a in2, textove retezce
  #vystup...textovy retezec slozeny ze spolecnych znaku
  #
  #prevod rezecu na vektor hodnot dle ascii
  in1=utf8ToInt(in1)  
  in2=utf8ToInt(in2)
  #funkce pro nalezeni spolecneho znaku
  common<-function(in1,in2){ 
    h=1
    i = 1
    stop=FALSE
    output=character()
    for (j in 1:(length(in1))){             #pro vsechny hodnoty prvniho vektoru od prvniho po posledni
      for (k in h:(length(in2))){           #pro vsechny hodnoty druheho vektoru, od prvniho nebo od nasledujiciho za shodnym po posledni
        if (in1[j]==in2[k]){                #pokud jsou hodnoty na danych indexech shodne, hodnota se prepise do vystupniho vektoru
          output[i]=in1[j]                      
          i=i+1
          h=k+1                             #ke startovnimu indexu pro vnoreny for se pripocte index posledniho shodneho prvku z druheho vektoru
          if (h>length(in2)){
            stop=TRUE                       #pokud startovni index prekroci delku druheho vektoru, zacne platit podminka pro break obou cyklu
          }
          break
        }
      }
      if(stop){break}
    }
    output = intToUtf8(output)              #prevod vektoru vystupnich hodnot na retezec
    return(output)
  }
  output1=common(in1,in2)                    #volani funkce pro nalezeni spolecnych znaku v poradi in1,in2
  output2=common(in2,in1)                    #volani stejne funkce pro opacne poradi vektoru
  if (nchar(output2)>nchar(output1)){        #podminka pro vyber vysledneho retezce - vraci delsi retezec
    print(output2)
  }else {print(output1)
  }
}
