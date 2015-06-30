setwd("C:/Users/kep/Documents/GitHub/Marriage-Pen")
#setwd("C:/Users/Kyle/Documents/GitHub/Marriage-Pen")

rm(list=ls()) 

#This spreadsheet has all the income data and will show all the calculations, step-by-step.

fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#data<-read.csv("data.csv", header = TRUE, fill = TRUE, sep = ",") #This is for 100 by 100 grid
 
data<-read.csv("newdata400.csv", header = TRUE, fill = TRUE, sep = ",") #This is for the 400 by 400 grid

#Load Functions. One file is the basic tax functions, the burden functions are the 
#calculations for the actual penalty/bonus

source("avgfunctions.R")

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

toutput_nochildren<-t(output_nochildren)
toutput_onechild<-t(output_onechild)
toutput_twochildren<-t(output_twochildren)

#percent

poutput_nochildren<-
poutput_onechild<-t(output_onechild)
poutput_twochildren<-t(output_twochildren)


#non Transposed Tables

write.table(output_nochildren,sep=",",file="nochildren_marriagepenalty.txt")
write.table(output_onechild,sep=",",file="onechild_marriagepenalty.txt")
write.table(output_twochildren,sep=",",file="twochildren_marriagepenalty.txt")

#Transposed Tables

write.table(toutput_nochildren,sep=",",file="tnochildren_marriagepenalty.txt")
write.table(toutput_onechild,sep=",",file="tonechild_marriagepenalty.txt")
write.table(toutput_twochildren,sep=",",file="ttwochildren_marriagepenalty.txt")

#Examples

  #Unequal Bonus
  
      children<-0
      married<-0
      hoh<-0
      stateincometax<-0

    #Person 1    
  
      income<-50000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      totaltax<-(incometax-EITC-CTC)+payrolltax
  
      Unequalbonus<-matrix(
        c(income,taxableincome,incometax,EITC,CTC,payrolltax,totaltax), 
        nrow = 7, ncol=1, byrow=TRUE, 
        dimnames = list(c("Income","Taxable Income","Income Tax","EITC","CTC","Payroll Tax","Total Tax"),c("Person1"))
        )
      
    #Person 2
  
      income<-25000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      totaltax<-(incometax-EITC-CTC)+payrolltax
  
      person2<-c(income,taxableincome,incometax,EITC,CTC,payrolltax,totaltax)
      Unequalbonus<-cbind(Unequalbonus,person2)
    
      unmarriedcouple<-rowSums(Unequalbonus)
      Unequalbonus<-cbind(Unequalbonus,unmarriedcouple)


    #Married
  
      income<-75000
      married<-1
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-Unequalbonus[6,1]+Unequalbonus[6,2]
      totaltax<-(incometax-EITC-CTC)+payrolltax
  
      couple<-c(income,taxableincome,incometax,EITC,CTC,payrolltax,totaltax)
      Unequalbonus<-cbind(Unequalbonus,couple)

  #Low Income Penalty

    married<-0
    stateincometax<-0

    #Person 1

      children<-1
      hoh<-1

      income<-15000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      totaltax<-(incometax-EITC-CTC)+payrolltax

      Lowincomepenalty<-matrix(
        c(income,taxableincome,incometax,EITC,CTC,payrolltax,totaltax), 
        nrow = 7, ncol=1, byrow=TRUE, 
        dimnames = list(c("Income","Taxable Income","Income Tax","EITC","CTC","Payroll Tax","Total Tax"),c("Person1"))
      )

    #Person 2

      children<-0
      hoh<-0
      
      income<-15000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      totaltax<-(incometax-EITC-CTC)+payrolltax
      
      person2<-c(income,taxableincome,incometax,EITC,CTC,payrolltax,totaltax)
      Lowincomepenalty<-cbind(Lowincomepenalty,person2)

      unmarriedcouple<-rowSums(Lowincomepenalty)
      Lowincomepenalty<-cbind(Lowincomepenalty,unmarriedcouple)


    #Married
    
      income<-30000
      married<-1
      children<-1
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-Lowincomepenalty[6,1]+Lowincomepenalty[6,2]
      totaltax<-(incometax-EITC-CTC)+payrolltax
      
      couple<-c(income,taxableincome,incometax,EITC,CTC,payrolltax,totaltax)
      Lowincomepenalty<-cbind(Lowincomepenalty,couple)

  #High Income Penalty

    married<-0
    stateincometax<-0

    #Person 1
    
      children<-0
      hoh<-0
      
      income<-150000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      medicaretax<-MedSurtax(income,married)
      amt<-AMT(income,married)
      totaltax<-max(amt,(incometax-EITC-CTC))+payrolltax+medicaretax
      
      Highincomepenalty<-matrix(
        c(income,taxableincome,incometax,EITC,CTC,payrolltax,medicaretax,amt,totaltax), 
        nrow = 9, ncol=1, byrow=TRUE, 
        dimnames = list(c("Income","Taxable Income","Income Tax","EITC","CTC","Payroll Tax","Medicare Surtax","AMT","Total Tax"),c("Person1"))
      )

    #Person 2
    
      children<-0
      hoh<-0
      
      income<-150000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      medicaretax<-MedSurtax(income,married)
      amt<-AMT(income,married)
      totaltax<-max(amt,(incometax-EITC-CTC))+payrolltax+medicaretax
      
      person2<-c(income,taxableincome,incometax,EITC,CTC,payrolltax,medicaretax,amt,totaltax)
      Highincomepenalty<-cbind(Highincomepenalty,person2)
      
      unmarriedcouple<-rowSums(Highincomepenalty)
      Highincomepenalty<-cbind(Highincomepenalty,unmarriedcouple)

    #Married Couple

      children<-0
      married<-1
      
      income<-300000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      medicaretax<-MedSurtax(income,married)
      amt<-AMT(income,married)
      totaltax<-max(amt,(incometax-EITC-CTC))+payrolltax+medicaretax

      couple<-c(income,taxableincome,incometax,EITC,CTC,payrolltax,medicaretax,amt,totaltax)
      Highincomepenalty<-cbind(Highincomepenalty,couple)

  #Marginal rate example


      children<-0
      hoh<-0
      married<-1
      income<-50000
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      medicaretax<-MedSurtax(income,married)
      amt<-AMT(income,married)
      totaltax1<-max(amt,(incometax-EITC-CTC))+payrolltax+medicaretax
      
      income<-50001
      taxableincome<-FedTaxableIncome(income,children,married,hoh,stateincometax)
      incometax<-FedIncomeTax(taxableincome,married,hoh)
      EITC<-FedEITC(income,children,married)
      CTC<-FedCTC(income,children,married)
      payrolltax<-FedEmployeePayroll(income,married)
      medicaretax<-MedSurtax(income,married)
      amt<-AMT(income,married)
      totaltax2<-max(amt,(incometax-EITC-CTC))+payrolltax+medicaretax
    
      marginalrate1<-totaltax2-totaltax1

