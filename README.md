
# Data files supporting the paper "High fire frequency in California chaparral reduces post-fire shrub regeneration and native plant diversity"

We surveyed fifty-four 250-m2 plots to assess changes in plant community composition and postfire regeneration of chaparral shrubs across a wide range of fire frequencies, including plots that have burned up to six times in the past 30 years

    1) 'LNU_plot_description.csv' 
      
Dataset of all potential covariates used in the models. 

Fields are as follows: 
- Site: Unique site name. *QuailRidge* for Quail Ridge UC Natural Reserve (38°30’ N, 122°08’ W), *ColdCanyon* for Cold Canyon UC Natural Reserve (38°30’ N, 122°06’ W), *Bobcat* for Bobcat Ranch Audubon Reserve (38°31’ N, 122°04’ W), and *CacheCreek* for Berryessa Snow Mountain National Monument (38°54’ N, 122°18’ W) and Valley Vista Regional Park (38°54’ N, 122°16’ W) 
- PlotID: A unique plot ID for each distinct transect. 
- num_burn: The number of prior fires, according to the California Fire Return Interval Departure (FRID) database, that occured after 1990. Since these fire perimeters generally ignore unburned patches within fires that are less than hundreds of acres in size, we used Google Earth historical imagery to examine the landscape for unburned patches after each fire.
- preFireName: Name of fire prior to the LNU lightning complex
- preFireYear: Year of fire prior to the LNU lightning complex
- crrnFRI: The current fire return interval calculated by dividing the number of years in the fire record by the number of fires occuring between 1908 and the current year in a polygon. 
- mdnRFRI: (median Percent FRID) A measure of the extent to which contemporary fires (since 1908) are burning at frequencies similar to the frequencies that occurred prior to Euro-american settlement, with the median reference FRI as a basis for comparison.
- mnPFRID: (mean Percent FRID) A measure of the extent to which contemporary fires (since 1908) are burning frequencies similar to the frequencies that occurred prior to Euro-american settlement, with the mean reference FRI as a basis for comparison.
- mCC_FRI: A condition class categorization of the data in the mnPFRID field.
- PFR: "Presettlement Fire Regime." This represents the dominant vegetation type linked to its probable historical fire regime (Van de Water and Safford 2011)
- ppt: Point estimate of annual average precipitation using 4 km resolution from the PRISM dataset (PRISM Climate Group 2022).
- tmean: Point estimate of annual average temperature using 4 km resolution from the PRISM dataset (PRISM Climate Group 2022).
- elevation: Elevation of plot in meters
- slope: Percent slope of plot
- aspect: Aspect of plot in degrees
- hli: Heat load index, which uses aspect and slope to account for the amount of solar radiation received (McCune and Keon 2002)
- Distance_km: Distance from coast, measured in km
- TSLF: 'Time Since Last Fire.' This designates the time between the LNU lightning complex and the most recent fire. 
- cool_warm_slope: Broad categorization of aspect, with cool slopes representing North and East aspects and warm slopes representing South and West aspects. 

      2) LNU_severity.csv

- plotID: A unique plot ID for each distinct transect. 
- mean_diam_mm: The stem diameter 1 centimeter from the terminus of four stems from a chamise individual. One individual was chosen in or adjacent to each quadrat, and five additional individuals were measured at the entire 250m2 transect scale. 

      3) Subplot_species.csv
      
- PlotID: A unique plot ID for each distinct transect. 
- year: Year of survey. 
- spp: Species code, associated with SpeciesList.csv 
- seedling_resprout: Either *resprout* which designates a shrub that resprouted from burl, or *seedling* which designates a shrub which germinated from a seed. 
- cover_count: *Cover* designates a percent cover quantified in each quadrat. *Count* designates a count of each seedling or resprout in each quadrat.  
- Q1: Quadrat 1 of 5
- Q2: Quadrat 2 of 5
- Q3: Quadrat 3 of 5
- Q4: Quadrat 4 of 5
- Q5: Quadrat 5 of 5

      4) shrub_seedling_data.csv
      
- PlotID: A unique plot ID for each distinct transect. 
- spp: Species code, associated with SpeciesList.csv 
- year: Year of survey. 
- sum.count.5m2: Summation of seedlings in each quadrat, across the entire transect
- presence: Presence/absence of a seedling in each quadrat

      5) shrub_height.csv
      
- PlotID: A unique plot ID for each distinct transect. 
- spp: Species code, associated with SpeciesList.csv 
- status: *D* for dead individual or *L* for live individual
- prefire_ht_m: Height (meters) before LNU lightning complex, measured by skeleton.
- postfire_ht_m: Postfire height (meters) of live vegetation after the LNU lightning complex. 
- diam_largest_stem: Diameter of largest stem (cm)
- year: Year of survey
      
      6) SpeciesList.csv
      
- spp: Species code, associated with all other files
- Lifeform: Lifeform according to the University of California Jepson Herbarium eFlora (https://ucjeps.berkeley.edu/eflora/) - TR (tree), SH (shrub), FB (forb), GR (graminoid), FERN (fern). 
- scientificNameAuthorship: Abbreviated name of person who named plant
- Species: Species
- Genus: Genus
- Native_nonnative: Origin of plant (native or nonnative), according to the University of California Jepson Herbarium eFlora (https://ucjeps.berkeley.edu/eflora/)
- fac.obl: Fire regeneration strategy according to USDA Forest Service Fire Effects Information System (https://www.feis-crs.org/feis/) - obligate seeder (OS), facultative seeder (FS), or obligate resprouter (OR). 
- subspecies: Subspecies of plant if applicable

