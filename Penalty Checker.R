#setwd("C:/Users/kep/Documents/GitHub/Marriage-Pen")
setwd("C:/Users/Kyle/Documents/GitHub/Marriage-Pen")

rm(list=ls()) 

#This spreadsheet has all the income data and will show all the calculations, step-by-step.

income<-read.csv("income.csv", header = TRUE, fill = TRUE, sep = ",")

statetax<-read.csv("statetaxdata.csv", header = TRUE, fill = TRUE, sep = ",")

fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#data<-read.csv("data.csv", header = TRUE, fill = TRUE, sep = ",")

data<-read.csv("newdata.csv", header = TRUE, fill = TRUE, sep = ",")

#Load Functions. One file is the basic tax functions, the burden functions are the 
#calculations for the actual penalty/bonus

source("Functions.R")

taxburden1<-function(income1,children,married,hoh,stateincometax){
  
  income<-income1
  children<-children
  
  if(children > 0){
    
    hoh<-1
    
  } else {
    
    hoh<-0
    
  }
  
  taxableincome<-FedTaxableIncome(income,children, married, hoh, stateincometax)
  incometax<-FedIncomeTax(taxableincome,married,hoh)
  ctc<-FedCTC(income,children,married)
  eitc<-FedEITC(income,children,married)
  medicare<-MedSurtax(income,married)
  amt<-AMT(income,married)
  totaltax<-( max(incometax-(ctc+eitc),amt)+medicare)
  
  
  
  return(max(totaltax,amt))
  
}

taxburden2<-function(income2,children,married,hoh,stateincometax){
  
  income<-income2
  
  children<-children
  
  if(children > 0){
    
    hoh<-1
    
  } else {
    
    hoh<-0
    
  }
  
  taxableincome<-FedTaxableIncome(income,children, married, hoh, stateincometax)
  incometax<-FedIncomeTax(taxableincome,married,hoh)
  ctc<-FedCTC(income,children,married)
  eitc<-FedEITC(income,children,married)
  medicare<-MedSurtax(income,married)
  amt<-AMT(income,married)
  totaltax<-( max(incometax-(ctc+eitc),amt) +medicare)
  
  
  return(totaltax)
  
}

marriedtaxburden<-function(income,children,married,hoh,stateincometax){
  
  income<-income
  married<-1
  children<-children
  
  taxableincome<-FedTaxableIncome(income,children, married, hoh, stateincometax)
  incometax<-FedIncomeTax(taxableincome,married,hoh)
  ctc<-FedCTC(income,children,married)
  eitc<-FedEITC(income,children,married)
  medicare<-MedSurtax(income,married)
  amt<-AMT(income,married)
  totaltax<-( max(incometax-(ctc+eitc),amt) +medicare)
  
  return(totaltax)
  
}

#No Children

children<-0
hoh<-0
output_nochildren<-data$income

x<-1

while(x <= length(data$multiplier)){

  multiplier<-data$multiplier[x]
  
  i<-1
  
  penalty<-NULL
  
  while(i <= length(data$income)){
    
    income<-data$income[i]
    stateincometax<-0
    married<-0
    
    income1<-income*multiplier
    income2<-income*(1-multiplier)

    person1<-taxburden1(income1,children,married,hoh,stateincometax)
    
    person2<-taxburden2(income2,children,married,hoh,stateincometax)
    
    singleburden<-(person1+person2)
    
    couple<-marriedtaxburden(income,children,married,hoh,stateincometax)
    
    penalty[i]<-couple-(singleburden)
    
    i<-i+1
    
  }
  
  
  output_nochildren<-cbind(output_nochildren,penalty)
  
  x<-x+1
  
}

#One Child

  children<-1
  hoh<-0
  output_onechild<-data$income
  
  x<-1
  
  while(x <= length(data$multiplier)){
  
    multiplier<-data$multiplier[x]
    
    i<-1
    
    penalty<-NULL
  
    while(i <= length(data$income)){
      
      income<-data$income[i]
      stateincometax<-0
      married<-0
      
      income1<-income*multiplier
      income2<-income*(1-multiplier)       
    
    #Person 1 test
    
      children<-1
      
      person1a<-taxburden1(income1,children,married,hoh,stateincometax)
      
      children<-0
      
      person1b<-taxburden1(income1,children,married,hoh,stateincometax)
    
    #person 2 test
    
      children<-0
          
      person2a<-taxburden2(income2,children,married,hoh,stateincometax)
      
      children<-1
      
      person2b<-taxburden2(income2,children,married,hoh,stateincometax)
    
    #optimal single burden
    
      singleburden<-min((person1a+person2a),(person1b+person2b))
    
    #couple taxburden
    
      couple<-marriedtaxburden(income,children,married,hoh,stateincometax)
      
      penalty[i]<-couple-(singleburden)
      
      i<-i+1
    
    }
    
  output_onechild<-cbind(output_onechild,penalty)
  
  x<-x+1
  
  }

  
#2 Children

  children<-2
  hoh<-0
  output_twochildren<-data$income

  x<-1
  
  while(x <= length(data$multiplier)){
    
    multiplier<-data$multiplier[x]
    
    i<-1
    
    penalty<-NULL
  
    while(i <= length(data$income)){
      
      income<-data$income[i]
      stateincometax<-0
      married<-0
      
      income1<-income*multiplier
      income2<-income*(1-multiplier)               
      
      #Person1 2 children
      
        children<-2
      
        person1a<-taxburden1(income1,children,married,hoh,stateincometax)
        
        children<-0
        
        person2a<-taxburden2(income2,children,married,hoh,stateincometax)
      
      #Person1 1 Child
      
        children<-1
      
        person1b<-taxburden1(income1,children,married,hoh,stateincometax)
      
        children<-1
        
        person2b<-taxburden2(income2,children,married,hoh,stateincometax)
      
      #Person2 2 Children
      
        children<-0
      
        person1c<-taxburden1(income1,children,married,hoh,stateincometax)
      
        children<-2
      
        person2c<-taxburden2(income2,children,married,hoh,stateincometax)
      
      #Optimal single burden
      
        singleburden<-min((person1a+person2a),(person1b+person2b),(person1c+person2c)) 
  
      #couple burden
      
        couple<-marriedtaxburden(income,children,married,hoh,stateincometax)
    
      #penalty or bonus
      
      penalty[i]<-couple-(singleburden)
    
    i<-i+1
  
    } 
  
    output_twochildren<-cbind(output_twochildren,penalty)
  
    x<-x+1

  }

#Manipulate Tables

write.table(output_nochildren,sep=",",file="nochildren_marriagepenalty.txt")
write.table(output_onechild,sep=",",file="onechild_marriagepenalty.txt")
write.table(output_twochildren,sep=",",file="twochildren_marriagepenalty.txt")
