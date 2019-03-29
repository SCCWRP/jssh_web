# README

Materials for JSSH analysis website https://sccwrp.shinyapps.io/jssh_web/

## Data

Data from the compiled geodatabase were extracted for exploratory analysis:

* `allfctprs` Pairwise evaluations of year plus habitat variables on S1/S2 density, by watershed and habitat type. Sites averaged within year by watershed, used in `varimp.Rmd`

* `alltops` top five models of salmonid density in relation to year and habitat variables, separate for each watershed and habitat type, station id is a random variable

* `fishdat` spatial data of steelhead and coho salmon surveys, steelhead density is recorded for two size classes (shorter than 75mm, 75mm or longer)

* `fishmtch` lookup table for matching locations where flow was esimated to fish sampling sites

* `habitat` data frame of habitat survey data

* `rchdat` data frame of 1/2 mile stream reach habitat data, includes wood in pools

* `reach` spatial data for stream reaches used in 1/2 mile habitat surveys

* `segment` spatial data for stream segments with fish data

* `stream` spatial data for complete hydrography for the four watersheds with fish and habitat data

Additional data were created for the website analyses:

* `trndhab_prep.RData` preprocessed habitat data for trend analyses at individual sites, used in `saltrends.Rmd`

* `trndst_prep.RData` preprocessed salmonid data for trend analyses at individual sites, used in `haban.Rmd`
