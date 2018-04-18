clear
use "C:\Users\wxh1000\Desktop\market_long.dta"
count if airline == 1
keep if airline == 1
keep market firm airport1 airport2
sort firm
gen firm2 = firm
drop market
sort firm airport1
duplicates drop firm airport1, force
save "C:\Users\wxh1000\Desktop\firm_operation_city.dta"
gen operate1 = 1
save, replace

clear
use "C:\Users\wxh1000\Desktop\market_long.dta"
merge m:1 firm airport1 using "C:\Users\wxh1000\Desktop\firm_operation_city.dta"
sort v1 firm
list market firm airport1 operate1
list market firm airport1 operate1 if _merge == 2
drop  if _merge == 2
save, replace

use "C:\Users\wxh1000\Desktop\firm_operation_city.dta"
rename airport1 airport2
rename operate1 operate2
save, replace

use "C:\Users\wxh1000\Desktop\market_long.dta"
drop _merge
merge m:1 firm airport2 using "C:\Users\wxh1000\Desktop\firm_operation_city.dta"
drop if _merge==2
sort v1 firm
