**Paper authors: Anusha Shankar\, Joseph R Canepa, Catherine H Graham, Susan M Wethington, Donald R Powers**

Code by: Anusha Shankar, github/nushiamme; contact: or nushiamme<at>gmail<dot>com for questions about code/datasets

#### Code organisation

The code is organized by data type into 3 scripts, for
1. Temperature and thermoregulatory cost calculations,
2. The energy budget model, and
3. Floral resources (abundance and calorie calculations)

Figures not listed here were conceptual figures made in powerpoint or in prism.

-   **Temp\_EnergyBudget.R** - Needs input files *"BBLH_temperatures_compiled.csv"*, *"Melted_Te_thermo.csv"*, *"Melted_Ta_thermo.csv"*; contains code for producing and compiling thermpregulatory costs for thermoregulation component of the energy budget model.

-   **Energy\_budget\_BBLH\_Aug2018.R** - Needs input files *"EnergyBudget_model_values.csv"*, *"DLW_summary"*, *"Costas1986_VO2_DRPowers.csv"*, *"Validation_Enrichment_dose_A.csv"*, *"Validation_enrichment_eqb_B.csv"*, and *"Validation_CO2produc_dose_C.csv"*; Contains code to analyse energy budget models and doubly labelled water data 
    -   *Table S3*: Best MCMCglmm models selected for all three model categories tested
    -   *Supplementary Table S2*: Stepwise model DIC values for parameter combinations in the nighttime energy expenditure MCMCglmm models
    -   *Supplementary Table S3*: Comparing MCMCglmm stepwise model results for the rewarming models
    -   *Supplementary Figure S4*: MCMCglmm model for the probability of entry into torpor as a function of individual capture mass
    -   *Supplementary Figure S10*: MCMCglmm model for the nighttime energy expenditure as a function of torpor duration and minimum chamber temperature
    -   *Supplementary Figure S12*: MCMCglmm model for rewarming energy expenditure (kJ) as a function of mass and chamber temperature
    
-   **Energy\_budget\_resources.R** - Needs input files *"FloralCensusData2013.csv"*, *"Costas1986_VO2_DRPowers.csv"*, *"Validation_Enrichment_dose_A.csv"*, *"Validation_enrichment_eqb_B.csv"*, and *"Validation_CO2produc_dose_C.csv"*; Contains code to analyse energy budget models and doubly labelled water data 
    -   *Table S3*: Best MCMCglmm models selected for all three model categories tested


#### Packages you will need for all the scripts:

    + reshape2 or reshape
    + ggplot2
    + dplyr
    + grid
    
#### Additional packages you will need for the temperature-thermoregulation script:

    + data.table
    + gridExtra
    + scales

#### Additional packages you will need for the resources script:
    
    + plyr
    + ggthemes
    
