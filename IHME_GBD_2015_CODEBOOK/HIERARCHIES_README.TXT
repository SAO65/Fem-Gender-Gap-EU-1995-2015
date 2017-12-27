The cause hierarchy file and the REI (risk factor, etiology, and impairment) hierarchy file contain GBD 2015 reporting hierarchies. The location hierarchies file contains the GBD 2015 reporting hierarchy and a number of other hierarchies, which will allow GBD 2015 results users to aggregate results by location in various ways (by GBD regions, World Bank regions, OECD countries, European Union countries, etc.).

To view each hierarchy in its nested order (e.g. for locations, a country would appear on the list beneath the region it belongs to), sort by the sort_order column. For the location_hierarchies file, sort first by location_set_version_id, and then by sort_order.


Definitions:

Locations Hierarchies

location_set_version:
The unique version of specific grouping of locations created for an organizational or analytic purpose, e.g., GBD 2015 reporting.

subnational_location:
This is a binary variable to indicate whether a location is subnational region. 0 = No; 1 = Yes.

location_code:
A letter or letter-number code for the location. IHME uses ISO 3166-1 alpha-3 codes for countries and custom codes for GBD regions and superregions. Most custom regions (e.g., Four World Regions, Commonwealth, UN Regional Commissions, etc.) haven't yet been assigned location codes.

path_to_top_parent:
This is the comma separated list of identifiers that display how far the child is from the top level (parent). For example, there is in the United Kingdom a subnational division called England with a location_id of 4749. The path of England is five layers down in the GBD 2015 reporting hierarchy for locations: World, Superregion, Region, Country, Subnational location. Therefore, the path is represented as: 1,64,73,95,4749. These location identifiers represent: Global, High-income, Western Europe


Cause Hierarchy

acause: Human and machine-readable name for a cause and, in theory, immune from hierarchical changes. Sometimes used in lieu of cause_id, although best practice now is to use cause_id as identifier for a specific cause.

path_to_top_parent: See the definition in Location Hierarchies.

yll_only, yld_only: These variables indicate whether a cause is used only in Years of Life Lost (YLLs - Mortality and Causes of Death) or Years Lived with Disability (YLDs - nonfatal health outcomes). NULL = the cause is used in all measures; 1 = the cause is used only in the specific measure -- YLL or YLD.


REI Hierarchy

rei_type: Indicates whether the rei is a risk, etiology, or impairment.

path_to_top_parent: See the definition in Location Hierarchies.




 

