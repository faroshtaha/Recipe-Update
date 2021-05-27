# Recipe-Update
Recipe master data maintenance is carried out by the standard SAP t-codes C202 / C203. These t-codes allow maintenance of all recipe master related data. Access to these t-codes is tightly controlled and is carried out by the GBS DMO. 

OMP needs planning rate data to be to maintained separately from the current planning rates which are set for financial reasons. We cannot give Planners access to C202 or other recipe change transactions as this opens up access to affect costing data.

Instead, new recipe master maintenance transaction is required which will allow the Planner to only change their specific OMP planning speed data in the recipe data structure.

The main ABAP program, Z_MAINTAIN_RATES, is currently equipped to update only Std Value 1 (PLPO-VGW01). This program is using two custom FMs ZRECIPE_UPD (for updating the recipes) and ZRECIPE_READ (for reading the recipes into corresponding data structures which will be used for populating the ALV output). The FM ZRECIPE_UPD is currently only capable of updating the Std Rates (VGW01 to VGW06), but in future this FM can be enhanced to update other PLPO fields as well.

