clear
import delimited "C:\Users\joshb\OneDrive\Documents\GitHub\Ag_Survey_2022\Survey_Data_Clean.csv", case(preserve) numericcols(2 3 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 51 52 53 56 57 58 59 60 61)

drop Q55_4_TEXT Consent v1 ShopType1 ShopType2 ShopType3 ShopType4 ShopType5 ShopType6

order TreatmentGroup CTPreference LocationLatitude LocationLongitude LocationLongitude UserLanguage Province Age Sex CommunityType FarmingHH HHSize Dependents Income Education PrimaryShopper STDiscountRetail STFarmersMarket STSupermarket STSpecialty STSupercenter STWholesaleClub LabelENV LabelLocal LabelFT LabelCOOL LabelCost LabelPractice Spending

// Treatment Group Questions

rename TreatmentGroup TG
rename CTPreference Food_Carbon_Tax

// Household Variables

rename LocationLatitude Latitude
rename LocationLongitude Longitude
rename CommunityType Community
rename HHSize HH_Size
rename Spending Weekly_Food_Exp
rename UserLanguage Survey_Language

// Grocery Shopping 

rename STDiscountRetail Discount_Retail
rename STFarmersMarket Farmers_Market
rename STSupermarket Supermarket
rename STSpecialty Specialty
rename STSupercenter Supercenter
rename STWholesaleClub Wholesale

// Food Labeling

rename LabelENV Label_Env
rename LabelLocal Label_Local
rename LabelFT Label_FT
rename LabelCOOL Label_COOL
rename LabelCost Label_Cost
rename LabelPractice Label_Practice

// Climate Change Policy Questions

rename CCMeetCommitments CC_Commitments
rename CCTooAmbitious CC_Ambitious
rename CCCTEffective CC_CT_Effective
rename CCDoMore CC_More
rename  CC_More CC_Do_More
rename  CCOverstated CC_Overstated
rename  CCBenifit CC_Benifit

// Ag Policy Questions

rename  AGFoodSecurity AG_Food_Security
rename  AGFoodProduction AG_Production_Restriction
rename AGFreeTrade Ag_Free_Trade
rename Ag_Free_Trade Ag_Trade
rename AGSmallFarm AG_Small_Farm
rename AGNoSupport AG_No_Support
rename AGSetPrice Ag_Price_Setting

// Ag Environment Policy Questions 

rename CCAGGHGAg CAG_AG_EM
rename CCAGGHGBeef CAG_Beef_EM
rename CCAGProductionlimits CAG_Limits
rename CCAGCTExempt CAG_CT_Exempt
rename CCAGRequireBMP CAG_BMP
rename CCAGCFL CAG_CFL

rename GovInvlovementAg Level_Gov_Ag

// Generic Policy 
rename PolicyIssuesCC PI_Climate
rename PolicyIssuesSpending PI_Spending
rename PolicyIssuesInternational PI_External
rename PolicyIssuesAg PI_Ag

rename Voting Party_Preference

// ----------------------------------------------------------------------------- //

// Variable Grouping and Reclassification

* TG Treatment Group
g Treatment = 0 if TG == "Control"
replace Treatment = 1 if TG != "Control"

* Treatment Value
g TG_Value = 0 if TG == "Control"
replace TG_Value = 1 if TG == "60% CF",
replace TG_Value = 2 if TG == "80% CF"| TG == "100% CF" 
replace TG_Value = 3 if TG ==  "120% CF"
replace TG_Value = 4 if TG == "140% CF"

* Food_Carbon_Tax --> Simplify to Disagree Neutral Agree 

*replace Food_Carbon_Tax = 1 if Food_Carbon_Tax == 1 | Food_Carbon_Tax==2
*replace Food_Carbon_Tax = 2 if Food_Carbon_Tax == 3
*replace Food_Carbon_Tax = 3 if Food_Carbon_Tax == 4 | Food_Carbon_Tax == 5

* Latitude --> Lat Value (N)
* Longitude --> Long Value (W) 

* What is your province or territory of residence? 
* Province --> regroup to Geographical Regions

g Region = 1 if Province == 2 // BC
replace Region = 2 if Province == 1 | Province == 3 |Province == 12 // Prarie
replace Region = 3 if Province == 9 // Ontario
replace Region = 4 if Province == 11 // Quebec
replace Region = 5 if Province == 4| Province == 5 |Province == 7| Province == 10 // Atlantic
replace Region = 6 if Province == 6| Province == 8| Province == 13 // Territories
	
* Age -> 1. 18-34, 2. 35-54, 3. 55+

* Sex -> Male, Female, Other to identifies as male

replace Sex = 0 if Sex == 2|Sex ==3 

* What type of community do you live in? 

* Community --> 1.Urban 2.Suburban 3.Rural 4.Other

g Community_City = 1 if Community == 1 | Community == 2
replace Community_City = 0 if Community_City ==. 

* Including yourself, how many people are in your household? 
* HH Size = 1, 2, 3, 4, 5, 6, 7 or More

* Dependents (Members of household under the age of 18) 

replace Dependents = 0 if Dependents == 4 | Dependents ==5
replace Dependents = 1 if Dependents == 3 

* Are you or a member of your household involved in farming? (FarmingHH) -. Yes no don't know to Yes binary ** 

replace FarmingHH = 0 if FarmingHH == 4 | FarmingHH ==5
replace FarmingHH = 1 if FarmingHH == 3 

* Income --> Values will be used to identify possible low income households
* 1. <10,000 
* 2. 10,000 --> 19,999
* 3. 20,000 --> 29,999
* 4. 30,000 --> 39,999
* 5. 40,000 --> 49,999
* 6. 50,000 --> 59,999
* 7. 60,000 --> 69,999
* 8. 70,000 --> 79,999
* 9. 80,000 --> 89,999
* 10. 90,000 --> 99,999
* 11. 100,000 --> 124,999
* 12. 125,000 --> 149,999
* 13. 150,000 --> 199,000
* 14. 200,000 --> 250,000
* 15. > 250,000
* 16. Prefer not to say

* What is the highest educational level you have completed?
* 1 = o	Less than high school  
* 2 = o	High school graduate  
* 3 = o	Vocational/Trade/Technical school  
* 4 = o	Some university/college  
* 5 = o	Bachelor's Degree  
* 6 = o	Advanced degree  
* 8 = o	Prefer not to say  (8) // I dont know why it skipped 7

* Change to Binary variable for completing University

g University = 1 if Education == 5 | Education == 6
replace University = 0 if University ==.

* PrimaryShopper --> Who is primarily responsible for grocery shopping in your house?
* 1. Me
* 2. Shared
* 3. Someone else

replace PrimaryShopper = 1 if PrimaryShopper == 2
replace PrimaryShopper = 0 if PrimaryShopper != 1

* Low Cost Shopper // Store type relitive to Supermarket < 0 == 1 

g Discount_Retail_R = 1 if Discount_Retail-Supermarket < 0
replace Discount_Retail_R = 0 if Discount_Retail_R ==. 

g Wholesale_R = 1 if Wholesale - Supermarket < 0
replace Wholesale_R = 0 if Wholesale_R ==. 

g Supercenter_R = 1 if Supercenter - Supermarket < 0
replace Supercenter_R = 0 if Supercenter_R ==. 


* Low_Cost_Shopping --> Values Range O (Primarily at supermarket) - 3 (at all three low cost alternatives)

g Low_Cost_Shopping = Discount_Retail_R+Wholesale_R+Supercenter_R
replace Low_Cost_Shopping = 0 if Low_Cost_Shopping ==. 

* Labeling --> Not important (1) to Extremely Important (5) --> Convert to relitive value compared with price, < 0 (less important than price), 0 (as important as price), > 0 (More important than price)

*Environmental sustainability certification 
g Label_Env_R = Label_Env - Label_Cost

replace Label_Env_R = 2 if Label_Env_R >=2 & Label_Env_R != . 
replace Label_Env_R = 0 if Label_Env_R ==. 

* Locally produced certification 
g Label_Local_R = Label_Local - Label_Cost

replace Label_Local_R = 2 if Label_Local_R >=2 & Label_Local_R != . 
replace Label_Local_R = 0 if Label_Local_R ==.

* Fair-trade certification 
g Label_FT_R = Label_FT - Label_Cost

replace Label_FT_R = 2 if Label_FT_R >=2 & Label_FT_R != . 
replace Label_FT_R = 0 if Label_FT_R ==.

* Country of origin labelling 
g Label_COOL_R = Label_COOL - Label_Cost

replace Label_COOL_R = 2 if Label_COOL_R >=2 & Label_COOL_R != . 
replace Label_COOL_R = 0 if Label_COOL_R ==.

* Farming practices certification (Organic, Biodynamic, Certified Humane, Non-GMO)
g Label_Practice_R = Label_Practice - Label_Cost

replace Label_Practice_R = 2 if Label_Practice_R >=2 & Label_Practice_R != . 
replace Label_Practice_R = 0 if Label_Practice_R ==.

* Food Expenditures: During a typical week what is your estimated household grocery bill?

* 1. < 50
* 2. 50 - 100
* 3. 100 - 150
* 4. 150 - 200
* 5. 200 - 250
* 6. > 250
* 7. Don't know

* Survey_Language --> Ignore for now

// Climate Change Policy Questions

* Canada will meet climate commitments with their current plan

replace CC_Commitments = 3 if CC_Commitments == . 

*replace CC_Commitments = 0 if CC_Commitments <= 2 // Disagree
*replace CC_Commitments = 1 if CC_Commitments == 3 // Neutral
*replace CC_Commitments = 2 if CC_Commitments == 4 | CC_Commitments == 5 // Agree

* Canada's climate commitments are too ambitious // Reverse Order

replace CC_Ambitious = (-1*CC_Ambitious)+6
replace CC_Ambitious = 3 if CC_Ambitious ==.
*replace CC_Ambitious = 0 if CC_Ambitious <= 2
*replace CC_Ambitious = 1 if CC_Ambitious == 3 
*replace CC_Ambitious = 2 if CC_Ambitious == 4 | CC_Ambitious == 5

* Carbon taxes are effective at reducing greenhouse gas emissions 

replace CC_CT_Effective = 3 if CC_CT_Effective ==.
*replace  CC_CT_Effective = 0 if CC_CT_Effective <= 2
*replace  CC_CT_Effective = 1 if CC_CT_Effective == 3 
*replace  CC_CT_Effective = 2 if CC_CT_Effective == 4 | CC_CT_Effective == 5

* Canada should do more to meet its international commitments under the Paris Climate Agreement 

replace CC_CT_Effective = 3 if CC_CT_Effective ==. 
*replace  CC_Do_More = 0 if CC_Do_More <= 2
*replace  CC_Do_More = 1 if CC_Do_More == 3 
*replace  CC_Do_More = 2 if CC_Do_More == 4 | CC_Do_More == 5

* The effects of climate change are overstated // Reverse Order
replace CC_Overstated = (-1*CC_Overstated)+6

replace CC_Overstated = 3 if CC_CT_Effective ==. 
*replace CC_Overstated = 0 if CC_Overstated <= 2
*replace CC_Overstated = 1 if CC_Overstated == 3 
*replace CC_Overstated = 2 if CC_Overstated == 4 | CC_Overstated == 5

* Canada's climate commitments  benefit  the economy. 

replace CC_Benifit = 3 if CC_Benifit ==. 
*replace CC_Benifit = 0 if CC_Benifit <= 2
*replace CC_Benifit = 1 if CC_Benifit == 3 
*replace CC_Benifit = 2 if CC_Benifit == 4 | CC_Benifit == 5


* Agriculture Policy Questions --> Strongly Disagree (1) to Strongly Agree (5) --> down to 3 

replace AG_Food_Security = 3 if AG_Food_Security ==. 
replace AG_Production_Restriction =3  if AG_Production_Restriction ==.
replace AG_Small_Farm = 3 if AG_Small_Farm ==. 
replace AG_No_Support = 3 if AG_No_Support ==. 

* The federal government has a responsibility to ensure food security within Canada

*replace AG_Food_Security = 0 if AG_Food_Security <= 2 // Disagree
*replace AG_Food_Security = 1 if AG_Food_Security == 3 // Neutral
*replace AG_Food_Security = 2 if AG_Food_Security == 4 | AG_Food_Security == 5 // Agree

* The amount of agricultural land used for non-food production (such as crops grown for ethanol) should be limited

*replace AG_Production_Restriction = 0 if AG_Production_Restriction <= 2
*replace AG_Production_Restriction = 1 if AG_Production_Restriction == 3 
*replace AG_Production_Restriction = 2 if AG_Production_Restriction == 4 | AG_Production_Restriction == 5

* The federal government should prioritize support for small scale family farms

*replace AG_Small_Farm = 0 if AG_Small_Farm <= 2
*replace AG_Small_Farm = 1 if AG_Small_Farm == 3 
*replace AG_Small_Farm = 2 if AG_Small_Farm == 4 | AG_Small_Farm == 5

* The federal government should not provide financial support to producers/farmers

*replace AG_No_Support = 0 if AG_No_Support <= 2
*replace AG_No_Support = 1 if AG_No_Support == 3 
*replace AG_No_Support = 2 if AG_No_Support == 4 | AG_No_Support == 5



// Agri-Environmental Policy Questions -->  Strongly Disagree (1) to Strongly Agree (5) --> down to 3 

* Agriculture is a major contributor of greenhouse gas emissions in Canada

replace CAG_AG_EM = 3 if CAG_AG_EM ==. 
replace CAG_Limits = 3 if CAG_Limits ==.
replace CAG_CT_Exempt = 3 if CAG_CT_Exempt ==.
replace CAG_BMP = 3 if CAG_BMP ==.
replace CAG_CFL = 3 if CAG_CFL ==.


*replace CAG_AG_EM = 0 if CAG_AG_EM <= 2 // Disagree
*replace CAG_AG_EM = 1 if CAG_AG_EM == 3 // Neutral
*replace CAG_AG_EM = 2 if CAG_AG_EM == 4 | CAG_AG_EM == 5 // Agree


* The federal government should implement production controls (i.e., by limiting total output) for agricultural products with large carbon footprints

*replace CAG_Limits = 0 if CAG_Limits <= 2
*replace CAG_Limits = 1 if CAG_Limits == 3 
*replace CAG_Limits = 2 if CAG_Limits == 4 | CAG_Limits == 5

* Emissions from agricultural production should be exempt from the federal carbon tax

*replace CAG_CT_Exempt = 0 if CAG_CT_Exempt <= 2
*replace CAG_CT_Exempt = 1 if CAG_CT_Exempt == 3 
*replace CAG_CT_Exempt = 2 if CAG_CT_Exempt == 4 | CAG_CT_Exempt == 5

* Farmers should be required to adopt measures that reduce greenhouse gas emissions

*replace CAG_BMP = 0 if CAG_BMP <= 2
*replace CAG_BMP = 1 if CAG_BMP == 3 
*replace CAG_BMP = 2 if CAG_BMP == 4 | CAG_BMP == 5

* Canada should introduce mandatory labelling of food products to show their carbon footprint

*replace CAG_CFL = 0 if CAG_CFL <= 2
*replace CAG_CFL = 1 if CAG_CFL == 3 
*replace CAG_CFL = 2 if CAG_CFL == 4 | CAG_CFL == 5

* How should the federal government encourage a reduction in agriculture-based greenhouse gas emissions?  Please select the option that you most agree with.

	* 1. The federal government should not incentivize or regulate  greenhouse gas emissions
	* 2. Farmers/Producers should be provided financial incentives to voluntarily reduce greenhouse gas emissions
	* 4. Farmers/Producers should be required to adopt practices that lower greenhouse gas emissions, and the federal government should help pay for it
	* 5. Farmers/Producers should be required to adopt practices that lower greenhouse gas emissions, and the federal government should not help pay for it

replace Level_Gov_Ag = 3 if Level_Gov_Ag == 4
replace Level_Gov_Ag = 4 if Level_Gov_Ag == 5 

// Ag and Climate Policy Relitive to fiscal Policy: Negitive Value = less important, 0 = as important, Positive = More Important

* Environment & Climate Change 
replace PI_Climate = PI_Climate- PI_Spending

* Agriculture & Food policy 
replace PI_Ag = PI_Ag - PI_Spending

* If a federal election were to be held tomorrow, which party would you vote for?

* 1. Liberal Party of Canada  
* 2. Conservative Party of Canada  
* 3. Bloc Quebecois  
* 4. New Democratic Party of Canada  
* 5. People's Party of Canada  
* 6. Green Party of Canada  
* 7. Other
* 8. Prefer not to say  

* Binary Right Voter

g Right_Vote = 1 if Party_Preference == 2 | Party_Preference == 5
replace Right_Vote = 0 if Right_Vote ==. 

* Binary Green Voter 

g Green_Vote = 1 if Party_Preference == 6
replace Green_Vote = 0 if Green_Vote ==. 

// ----------------------------------------------------------------------------- //

// EFA Factor Analysis Example

polychoric PI_Ag PI_Climate CAG_AG_EM CAG_Limits CAG_CT_Exempt CAG_BMP CAG_CFL AG_Food_Security AG_Production_Restriction AG_Small_Farm AG_No_Support CC_Commitments CC_Ambitious CC_CT_Effective CC_Overstated CC_Benifit Label_Env_R Label_Local_R Label_FT_R Label_COOL_R Label_Practice_R 

global N = r(sum_w)
matrix r = r(R)
factormat r, n($N)
screeplot, yline(1)
factormat r, n($N) mineigen(1) blanks(0.3)
estat kmo
estat common
rotate, promax blanks(.3)

predict Factor1 Factor2 Factor3

rename Factor1 Environment
rename Factor2 Food
rename Factor3 Ag_Inter



// ----------------------------------------------------------------------------- //

// Model Output

ologit Food_Carbon_Tax i.Treatment i.Age i.Sex i.University Income HH_Size#i.Dependents i. Weekly_Food_Exp b3.Region i.Community_City i.PrimaryShopper i.Low_Cost_Shopping i.Right_Vote i.Green_Vote Environment Food Ag_Inter, vce(robust)

// ---------------------------------------------------------------------------- // 

* Adjusted Household income
 
g Income_Value = 10000 if Income == 1
replace Income_Value = 15000 if Income == 2
replace Income_Value = 25000 if Income == 3
replace Income_Value = 35000 if Income == 4
replace Income_Value = 45000 if Income == 5
replace Income_Value = 55000 if Income == 6
replace Income_Value = 65000 if Income == 7
replace Income_Value = 75000 if Income == 8
replace Income_Value = 85000 if Income == 9
replace Income_Value = 95000 if Income == 10
replace Income_Value = 112500 if Income == 11
replace Income_Value = 137500 if Income == 12
replace Income_Value = 175000 if Income == 13
replace Income_Value = 225000 if Income == 14
replace Income_Value = 250000 if Income == 15

g Adj_Income = Income_Value/(HH_Size^.5)


g BC_Income = 1 if HH_Size == 1 & Adj_Income <= (0.80*41600)
replace BC_Income = 1 if HH_Size >1 & Adj_Income <= (0.80*(105000/(3^0.5)))
replace BC_Income = 0 if BC_Income!=1 & HH_Size !=.  & Adj_Income !=.




ologit Food_Carbon_Tax i.Treatment i.Age i.Sex i.University Adj_Income i.Dependents i. Weekly_Food_Exp b3.Region i.Community_City i.PrimaryShopper i.Low_Cost_Shopping i.Right_Vote i.Green_Vote Environment Food Ag_Inter, vce(robust)

* Adj food expenditure


g Food_Ex = 50 if Weekly_Food_Exp ==1
replace Food_Ex = 100 if Weekly_Food_Exp ==2
replace Food_Ex = 150 if Weekly_Food_Exp == 3
replace Food_Ex = 200 if Weekly_Food_Exp == 4
replace Food_Ex = 250 if Weekly_Food_Exp == 5
replace Food_Ex = 300 if Weekly_Food_Exp == 6

g Adj_Food_Ex = Food_Ex/(HH_Size^.5)




* Binary outcomes
ologit Food_Carbon_Tax i.Treatment i.Sex i.University i.BC_Income i.Dependents Adj_Food_Ex b3.Region i.Community_City i.PrimaryShopper Low_Cost_Shopping i.Right_Vote Environment Food Ag_Inter, vce(robust)

* Cost Values
ologit Food_Carbon_Tax i.TG_Value i.Sex i.University i.BC_Income i.Dependents Adj_Food_Ex b3.Region i.Community_City i.PrimaryShopper Low_Cost_Shopping i.Right_Vote Environment Food Ag_Inter, vce(robust)

* restricted to cost treatment
ologit Food_Carbon_Tax b1.TG_Value i.Sex i.University i.BC_Income i.Dependents Adj_Food_Ex b3.Region i.Community_City i.PrimaryShopper Low_Cost_Shopping i.Right_Vote Environment Food Ag_Inter if TG_Value>0, vce(robust)



