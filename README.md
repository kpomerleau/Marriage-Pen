##Marriage Penalty and Marriage Bonus Calculator

This Repo Contains the data and code for the paper: "Understanding the Marriage Penalty and Marriage Bonus"
Link : http://taxfoundation.org/article/understanding-marriage-penalty-and-marriage-bonus

This calculator's data was featured in both the New York Times and Fivethirtyeight piece on the marriage penalty
* NYT link: http://www.nytimes.com/interactive/2015/04/16/upshot/marriage-penalty-couples-income.html
* Fivethirtyeight link: http://fivethirtyeight.com/interactives/marriage-penalty/

**Files:**

* **data.csv:** A data set with two variables: income from 10,000 to 1,000,000 scaled by 100^1/100 and ratios from 1 to .5
* fedtax.csv: A data set with income tax parameters. These parameters are read by the tax calculator functions
* newdata.csv: A data set with two variables: income from 10,000 to 1,000,000 scaled by 200^1/200 and ratios from 1 to .5
* newdata400.csv: A data set with two variables: income from 10,000 to 1,000,000 scaled by 400^1/400 and ratios from 1 to .5
* avgfunctions.R: An R script that defines all the functions for the tax calculator
* marginalfunctions.R: An alternative script that properly measures marginal tax rates (it smooths IRS rounding)
* Marriage Marginal.R: A test script that looks at second-earner penalties for married couples (STILL TESTINGS)
* Penalty Checker.R: The main R script that calculates all output data
* There are also misc. output files in the repo produced by the Penalty Checker.R script

**File Details**

**data/newdata/newdata400.csv:**

Variable | Defintion
---|---------
`income` | `Income in dollars from 10,000 to 1 million. Scaling depends on number of observations (see above)`
`multiplier` | `The percent of total family income the primary earner brings home`

**avgfunctions.R**
*The same functions exist in marginalfunctions.R, they are only slightly altered

Function and Inputs | Defintion
---|---------
`AMT(income,married)` | `calculates AMT based on income and marrital status`
`FedCTC(income,children,married` | `calculates Child Tax Credit based on income, number of children and marrital status. Automatically accounts for refundability`
`FedEITC(income,children,married)` | `calculates Earned Income Tax Credit based on income, children, and marrital status`
`FedEmployeePayrollTax(income,married)` | `calculates employee payroll tax. implicitly assumes income is split 50-50 between earners.`
`FedEmployerPayrollTax(income,married)` | `calculates employee payroll tax. implicitly assumes income is split 50-50 between earners.`
`FedIncomeTax(taxableincome,married,hoh)` | `calculates income tax burden given taxable income, and filing status. hoh CANNOT = 1 if married = 1`
`FedTaxableIncome(income,children,married,hoh,stateincometax)` | `calculates federal taxable income given income, children, filing status. It will also switch to the state and local income tax deduction once it is greater than the standard deduction`
`FedUI(income,married,stateui)` | `calculates the federal unemployment insurance payroll tax based on income and marrital status and state ui. Implict 50-50 split for married couples`
`MedSurtax(income,married)` | `calculates the 0.9 percent medicare surtax based on income and marrital status`
`StateEITC(eitc,stateparam)` | `NOT USED: calculates state Earned income tax credit based on fed EITC and state law`
`StateIncomeTax(statetaxableincome,married,hoh,stateparam)` | `NOT USED: calculates state income tax based on state taxable income and filing status`
`StateParameters(state)` | `NOT USED: for a given state ID number, returns state income tax code parameters`
`StatePersonalCredit(income,statetaxableincome,married,hoh,stateparam)` | `NOT USED: Calculates a state "personal credit" given income and state parameters. Usually in replace of a state standard deduction or personal exemption`
`StateUI(income,married,stateparam)` | `NOT USED: State UI given income and state UI parameters`

**fedtax.csv**

* Contains federal income tax parameters (not all used for marriage penalty)


