**Paper authors: A Shankar, CH Graham, JR Canepa, SM Wethington, DR Powers**

Code by: Anusha Shankar, github/nushiamme; contact: nushiamme<at>gmail<dot>com for questions about code/datasets

#### Code organisation

The code is organized by data type into 3 scripts, for
1. Temperature and thermoregulatory cost calculations,
2. The energy budget model, and
3. Floral resources (floral abundance)

Figures not listed here were conceptual figures made in powerpoint or in prism.

-   **Temp\_EnergyBudget.R** - Needs input files *"BBLH_temperatures_compiled.csv"*, *"Melted_Te_thermo.csv"*, *"Melted_Ta_thermo.csv"*; contains code for producing and compiling thermoregulatory costs for thermoregulation component of the energy budget model.
    -   *Figure 1d*: Distribution of temperatures at Harshaw and Sonoita, facetted by day and night.

-   **Energy\_budget\_BBLH\_Aug2018.R** - Needs input files *"EnergyBudget_model_values.csv"*, *"DLW_summary"*, *"Costas1986_VO2_DRPowers.csv"*, *"Validation_Enrichment_dose_A.csv"*, *"Validation_enrichment_eqb_B.csv"*, and *"Validation_CO2produc_dose_C.csv"*; Contains code to analyse energy budget models and doubly labelled water data 
    -   *Figure 2*: Energy budget models compared against doubly labelled water (DLW) measurements of daily energy expenditure. Figure 2a has DLW measurements, with individuals recaptured multiple times as colored points. Figure 2b has DLW values compared directly with model values.
    -   *Figure 3*: Stacked bar graph showing the different modeled components of the daily energy budget 
    -   *Supplementary Figure S1*: Validation of the modified DLW method
    -   *Supplementary Figure S3*: Scholander-Irving curve for Costa's hummingbirds
    
-   **Energy\_budget\_resources.R** - Needs input files *"FloralCensusData2013.csv"* (contact A Shankar for access), *"Costas1986_VO2_DRPowers.csv"*, *"Validation_Enrichment_dose_A.csv"*, *"Validation_enrichment_eqb_B.csv"*, and *"Validation_CO2produc_dose_C.csv"*; Contains code to analyse energy budget models and doubly labelled water data 
    -   *Table S3*: 

#### Packages you will need for all three scripts:

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
    
