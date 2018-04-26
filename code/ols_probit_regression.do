use "C:\Users\wxh1000\Desktop\market_wide.dta"

gen airpresence = (airpresenceaa + airpresenceal + airpresencedl ///
				   + airpresencelcc + airpresenceua + airpresencewn)/6
gen lairpresence = ln(airpresence)
gen route = (routeaa + routeal + routedl + routelcc + routeua + routewn)/6

reg firm_nb lpopulation ldistance tourism
est sto ols1
reg firm_nb lpopulation ldistance tourism lairpresence 
est sto ols2
reg firm_nb lpopulation ldistance tourism route
est sto ols3
reg firm_nb lpopulation ldistance lairpresence
est sto ols4
reg firm_nb lpopulation ldistance tourism lairpresence route
est sto ols5

esttab ols* using test221.tex, r2 se nogap replace

use "C:\Users\wxh1000\Desktop\market_long.dta"

probit airline lpopulation ldistance tourism lairpresence 
est sto p1
probit airline lpopulation ldistance tourism route
est sto p2
probit airline lpopulation ldistance lairpresence 
est sto p3

esttab p* using test212.tex, se replace


 
 
 
 
 
 

 

