#This is a follow up to the marriage penalty calculator. This looks at the marginal rate penalty faced by
#second earners in a household.

setwd("C:/Users/kep/Documents/GitHub/Marriage-Pen")
#setwd("C:/Users/Kyle/Documents/GitHub/Marriage-Pen")

rm(list=ls()) 

#This spreadsheet has all the income data and will show all the calculations, step-by-step.

income<-read.csv("income.csv", header = TRUE, fill = TRUE, sep = ",")

statetax<-read.csv("statetaxdata.csv", header = TRUE, fill = TRUE, sep = ",")

fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

data<-read.csv("data.csv", header = TRUE, fill = TRUE, sep = ",")

#data<-read.csv("newdata400.csv", header = TRUE, fill = TRUE, sep = ",")

#Load Functions. One file is the basic tax functions, the burden functions are the 
#calculations for the actual penalty/bonus

source("marginalfunctions.R")

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

#####No Children; set up those parameters:

children<-0
hoh<-0

#Setup the output files for each earner

output_primary_nochildren<-data$income
output_secondary_nochildren<-data$income

x<-1

while(x <= length(data$multiplier)){

  multiplier<-data$multiplier[x]
  
  i<-1
  
  penaltyprimary<-NULL
  penaltysecondary<-NULL
  
  while(i <= length(data$income)){
    
    income<-data$income[i]
    stateincometax<-0
    married<-0
    
    income1<-income*multiplier
    income2<-income*(1-multiplier)

    person1<-taxburden1(income1+1,children,married,hoh,stateincometax)-taxburden1(income1,children,married,hoh,stateincometax) #primary earner's marginal rate
    
    person2<-taxburden2(income2+1,children,married,hoh,stateincometax)-taxburden2(income2,children,married,hoh,stateincometax) #secondary earner's marginal rate
      
    couple<-marriedtaxburden(income+1,children,married,hoh,stateincometax)-marriedtaxburden(income,children,married,hoh,stateincometax) #the couple's marginal rate
    
    penaltyprimary[i]<-couple-(person1)
    penaltysecondary[i]<-couple-(person2)
    
    i<-i+1
    
  }
  
  
  output_primary_nochildren<-cbind(output_primary_nochildren,penaltyprimary)
  output_secondary_nochildren<-cbind(output_secondary_nochildren,penaltysecondary)
  
  x<-x+1
  
}

toutput_secondary_nochildren<-t(output_secondary_nochildren)
write.table(toutput_secondary_nochildren,sep=",",file="testmarginalrates.txt")

  #Heat Map (I broke this badly. Needs to be fixed!!!)

  if (!require("gplots")) {
    install.packages("gplots", dependencies = TRUE)
    library(gplots)
  }
  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer", dependencies = TRUE)
    library(RColorBrewer)
  }

    #trim dataset for heatmap

      map_data<-toutput_secondary_nochildren[2:nrow(toutput_secondary_nochildren),]
  
    #Color of the heatmap

      my_palette <- colorRampPalette(c("red", "white", "blue"))(n = 299)

    #Defined Color Breaks

    # (optional) defines the color breaks manually for a "skewed" color transition
      #col_breaks = c(seq(-1,0,length=100),  # for red
       #              seq(0,0.8,length=100),              # for yellow
        #             seq(0.8,1,length=100))              # for green

    #creates the image

      # creates a 5 x 5 inch image
      #png("../images/heatmaps_in_r.png",    # create PNG for the heat map        
        #  width = 5*300,        # 5 x 300 pixels
        #  height = 5*300,
        #  res = 300,            # 300 pixels per inch
        #  pointsize = 8)        # smaller font size

   # heatmap.2(toutput_secondary__nochildren,
    #          cellnote = toutput_secondary__nochildren,  # same data set for cell labels
    #          main = "Second Earner Penalty", # heat map title
    #          notecol="black",      # change font color of cell labels to black
    #          density.info="none",  # turns off density plot inside color legend
    #          trace="none",         # turns off trace lines inside the heat map
    #          margins =c(12,9),     # widens margins around plot
    #          col=my_palette,       # use on color palette defined earlier 
    #          #breaks=col_breaks,    # enable color transition at specified limits
    #          dendrogram="row",     # only draw a row dendrogram
    #          Colv="NA")            # turn off column clustering

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

poutput_nochildren<-t(output_nochildrenchild)
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

