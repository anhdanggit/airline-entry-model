drop v6
drop if operating_carrier == "99"
save air4
clear

use "C:\Users\wxh1000\Desktop\air.dta", clear
bysort origin dest operating_carrier: egen pass = sum(passengers)
bysort origin dest operating_carrier: egen distance = mean(market_distance)
duplicates drop origin dest operating_carrier, force
drop passengers market_distance
gen airpresence = pass*distance

keep if operating_carrier == "AA" | ///
		operating_carrier == "DL" | ///	 
		operating_carrier == "UA" | /// 
		operating_carrier == "WN" | ///
		operating_carrier == "AL" | ///
		operating_carrier == "B6"| ///
		operating_carrier == "CP"| ///
		operating_carrier == "DH"| ///
		operating_carrier == "F9"| ///
		operating_carrier == "FL"| ///
		operating_carrier == "G4"| ///
		operating_carrier == "JR"| ///
		operating_carrier == "NJ"| ///
		operating_carrier == "NK"| ///
		operating_carrier == "SY"| ///
		operating_carrier == "TZ"

replace operating_carrier = "LCC" if ///
		operating_carrier == "B6"| ///
		operating_carrier == "CP"| ///
		operating_carrier == "DH"| ///
		operating_carrier == "F9"| ///
		operating_carrier == "FL"| ///
		operating_carrier == "G4"| ///
		operating_carrier == "JR"| ///
		operating_carrier == "NJ"| ///
		operating_carrier == "NK"| ///
		operating_carrier == "SY"| ///
		operating_carrier == "TZ"
		

keep if operating_carrier == "AA" | ///
		operating_carrier == "DL" | ///	
		operating_carrier == "LCC" | /// 
		operating_carrier == "UA" | /// 
		operating_carrier == "WN" | ///
		operating_carrier == "AL" 

drop airpresence
bysort origin dest operating_carrier: egen passengers = sum(pass)
bysort origin dest operating_carrier: egen dist = mean(distance)
duplicates drop origin dest operating_carrier, force
drop pass distance

save, replace

// Copy the same dataset as air2.dta
use air2, clear
rename origin d
rename dest origin
rename d dest
rename passengers passengers2
rename dist dist2
merge 1:1 origin dest operating_carrier using "C:\Users\wxh1000\Desktop\air.dta"

replace passengers = 0 if passengers == .	
replace passengers2 = 0 if passengers2 == .
replace dist = 0 if dist == .
replace dist2 = 0 if dist2 == .	
drop _merge

gen passenger = passengers + passengers2
egen distance = rowmax(dist dist2)

drop passengers passengers2 dist dist2
gen airpresence = passenger*distance

save "airpresence_firm", replace

drop passenger distance airpresence
******* Calculate the route
// Since in the dataset the record occurs only once 
//       for the airlines between two cities
// Thus I reverse the dest and origin, copy the whole dataset
//       and append to the previous dataset so as to represent
//       a round trip. Then the num of route is just to count
//       the num of observations.

bysort operating_carrier origin: egen route = count(dest)

rename origin airport
drop dest
duplicates drop airport operating_carrier, force
save "route_firm", replace

********* Merge
use "C:\Users\wxh1000\Desktop\airpresence_firm.dta"
rename origin airport1
rename dest airport2
merge m:1 airport1 operating_carrier using "C:\Users\wxh1000\Desktop\route_firm.dta"
drop _merge
rename route route1
merge m:1 airport2 operating_carrier using "C:\Users\wxh1000\Desktop\route_firm.dta"
drop _merge
gen route = (route1 + route2)/2
drop route1 route2

sort airport1 airport2
order airport1 airport2
save airpresence_route_firm, replace

gen firm = "aa"
replace firm = "al" if operating_carrier == "AL"
replace firm = "dl" if operating_carrier == "DL"
replace firm = "lcc" if operating_carrier == "LCC"
replace firm = "ua" if operating_carrier == "UA"
replace firm = "wn" if operating_carrier == "WN"

drop operating_carrier
save airpresence_route_firm, replace

use "C:\Users\wxh1000\Desktop\market_long.dta"
drop passengers airpresence route 
drop city_pair2
merge 1:1 airport1 airport2 firm using airpresence_route_firm
drop if _merge == 2
rename distance dist
gen lairpresence = ln(airpresence+1)
save, replace

reshape wide airline city_pair operate1 operate2 passenger dist airpresence route lairpresence, i(v1 market) j(firm) string
order v1 market airlineaa airlineal airlinedl airlinelcc airlineua airlinewn
save "C:\Users\wxh1000\Desktop\market_wide.dta"
