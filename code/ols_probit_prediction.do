/*******************************************************************************
Program:       Empirical IO 2
Author:         Hoang TRAN
Date:             27/04/2018
Description:  OLS - Probit Regression

*******************************************************************************/

clear all 		
capture log close 
set type double
set more off, permanently
set excelxlsxlargefile on

*********************************************
*****		    			OLS Regression 						*****
*********************************************

**** Import data-set ****
cd "E:\TSE\Empirical IO\Project 2"
import excel wide.xlsx, firstrow clear

**** Create new variables ****
gen airpresence = (airpresenceaa + airpresenceal + airpresencedl ///
								+ airpresencelcc + airpresenceua + airpresencewn)/6
gen lairpresence = ln(airpresence)
gen route = (routeaa + routeal + routedl + routelcc + routeua + routewn)/6

gen lairpresence1 = lairpresence*1.5
gen lpopulation1 = lpopulation*1.5
gen ldistance1 = ldistance*1.5

**** Regression ****
reg firm_nb lpopulation ldistance lairpresence route tourism

**** Prediction ****
predict phat, xb
gen phat1 = _b[lpopulation]*lpopulation1 + _b[ldistance]*ldistance  /// 
					 + _b[lairpresence]*lairpresence  +  _b[route]*route + _b[tourism]*tourism ///
					 + _b[_cons]

gen phat2 = _b[lpopulation]*lpopulation + _b[ldistance]*ldistance1  /// 
					 + _b[lairpresence]*lairpresence  +  _b[route]*route + _b[tourism]*tourism ///
					 + _b[_cons]

gen phat3 = _b[lpopulation]*lpopulation + _b[ldistance]*ldistance  /// 
					 + _b[lairpresence]*lairpresence1  +  _b[route]*route + _b[tourism]*tourism ///
					 + _b[_cons]

tabstat(phat)
tabstat(phat1)
tabstat(phat2)
tabstat(phat3)


*********************************************
*****		    			Probit Regression 					*****
*********************************************
clear all 
import excel long.xlsx, firstrow clear

**** Counter-factual ****
gen lairpresence1 = lairpresence*1.5
gen lpopulation1 = lpopulation*1.5
gen ldistance1 = ldistance*1.5

**** Regression ****
probit airline lpopulation ldistance lairpresence 

**** Predicted probabilities ****
predict phat 
gen phat1 = normal(_b[lpopulation]*lpopulation1 + _b[ldistance]*ldistance  /// 
								+ _b[lairpresence]*lairpresence  +  _b[_cons])
								
gen phat2 = normal(_b[lpopulation]*lpopulation + _b[ldistance]*ldistance1  /// 
								+ _b[lairpresence]*lairpresence  +  _b[_cons])
								
gen phat3 = normal(_b[lpopulation]*lpopulation + _b[ldistance]*ldistance  /// 
								+ _b[lairpresence]*lairpresence1  +  _b[_cons])
								

**** Predicted operating status ****
gen airline0 = 0
replace airline0 = 1 if phat >= .5

gen airline1 = 0
replace airline1 = 1 if phat1 >= .5

gen airline2 = 0
replace airline2 = 1 if phat2 >= .5

gen airline3 = 0
replace airline3 = 1 if phat3 >= .5

**** Predicted number of operating firm ****
sort market

by market: egen firm_nb0 = total(airline0)
by market: egen firm_nb1 = total(airline1)
by market: egen firm_nb2 = total(airline2)
by market: egen firm_nb3 = total(airline3)

tabstat(firm_nb0)
tabstat(firm_nb1)
tabstat(firm_nb2)
tabstat(firm_nb3)

* Calculate error table
gen dummy = "F"
replace dummy = "T" if airline == airline0

sort firm
tabulate firm dummy, row

* Calculate RMSE
gen err = (firm_nb - firm_nb0)^2
egen sum_err = sum(err)
count
display sqrt(sum_err/16452)
