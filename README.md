# Readout_Datalogger
<details>
	
<summary> Version used: </summary> 

R (4.3.0, 2023-04-21)

Packages needed: 
- Pacman: (0.5.1)
- ggplot2: (2.3.4.2)
- lubridate: (1.9.2)
- ggnewscale: (0.4.9)
- dplyr: (1.1.2)
- glue: (1.6.2)
</details>

## Paper reference

**Using barcoding to reveal ecological patterns of nivicolous myxomycetes in the German Alps: 
How do they deal with varying snow conditions?**

Maho Inoue<sup>a,*</sup>, Jan Woyzichovski<sup>a</sup>, Ángela López-Villalba<sup>a</sup>, Oleg Shchepin<sup>a,b</sup>, Anja Klahr<sup>a</sup>, Yuri K. Novozhilov<sup>b</sup>, Martin Schnittler<sup>a</sup>

<details>
<summary>Further details: </summary>

<sup>*</sup>
	Corresponding author

<sup>a</sup>
	Institute of Botany and Landscape Ecology, University Greifswald, Greifswald, Germany
 
<sup>b</sup>
	V.L. Komarov Botanical Institute of the Russian Academy of Sciences, St. Petersburg, Russia

*Received 6 March 2024, Revised 1 July 2024, Accepted 9 July 2024, Available online 16 July 2024, Version of Record 16 July 2024.*

published in: Fungal Ecology, Volume 71, 2024, 101374, ISSN 1754-5048
</details>

DOI: [https://doi.org/10.1016/j.funeco.2024.101374](https://doi.org/10.1016/j.funeco.2024.101374)

### Abstract

A transect in the German limestone Alps was monitored over ten years for nivicolous myxomycetes to test if species display stable altitudinal belts for fruiting. The data set comprised 1368 barcoded specimens assigned to 112 ribotypes forming 51 ribogroups. Ribogroups were largely consistent with 35 identified morphospecies, although in eleven cases a morphospecies included several ribogroups. Fructification abundance correlated with duration of the snow cover inferred from data loggers placed at ground height. Morphospecies, ribogroups, and ribotypes showed a peak of fructification abundance at different elevations in different years. Species composition, not abundances, showed a high overlap with soil metabarcoding data. Thirteen ribogroups detected in the metabarcoding data set were never found as fructifications. This survey demonstrates that nivicolous myxomycetes are opportunists, which are likely to persist as trophic or resting stages independent from snow cover, but fruit only in altitudes and years with snow cover stable over several months.

# Purpose:

These two scripts were used in the above-mentioned article to read out the data logger’s raw files and to make a standardized preliminary analysis of the environmental data to predict the winter season and the snow conditions of each day based on the temperature readings and its daily fluctuation. 
The snow conditions are categorized in:
-	$${\color{green}Green}$$: days with mean temperature between −0.5 °C `lower_temp = -.5` and 2 °C `upper_temp = 2`and daily fluctuations below 3 °C `fluct_temp = 3`, indicating the presence of a closed snow cover which fosters growth of microbial communities and myxamoebal activity (Schmidt and Lipson, 2004; Schnittler et al., 2015).
-	$${\color{yellow}Yellow}$$: days with a mean temperature above 2 °C `upper_temp = 2` or daily fluctuations exceeding 3 °C `fluct_temp = 3`, indicating exposure to wind and sun radiation without snow cover.
-	$${\color{red}Red}$$: days with mean temperature below −0.5 °C  `lower_temp = -.5`, which is potentially harmful for myxamoebae due to soil frost (Shchepin et al., 2014), but they can survive these days in encysted state, if temperatures do not decrease too fast.
  
The snow season ($${\color{#A1CAF1}grey-blue}$$ bar) is defined by a starting condition (first period of four or more consecutive “green” days, `onset_days = 5`) and an ending condition (the last “green” or “red” day of this winter before August  `cut_month = 8`.

All the resulting statistics are limited within the snow season. Information outside of the snow seasons are excluded from the output tables.
The TOMST TMS-4 data loggers provide three temperatures and one moisture read-outs (Hobo data logger provide only one temperature and one relative humidity read-out.). The temperature sensors are vertically separated by around 10 cm. By the placement instruction from the company the middle one is at surface level. Because of these numerous sensors the resulting output tables contain all 4 read-outs (or only 2 by HOBO loggers) as well as all calculations for each sensor separated in different columns and labeled accordingly to their respective sensors.

Because different data loggers separated their data differently, the here presented scripts are separated depending on which data logger type was used by our group: 
-	1st generation loggers:
	-	HOBO U23 Pro v2 Temperature / Relative Humidity Data Logger (U23-001A) 
	&rarr;	`Snowcover_hobo.R`
-	2nd generation loggers:  
	-	TOMST TMS-4 Extreme 
	&rarr; 	`Snowcover_tomst_v2.R`


## Main function:

```R
tomst_snow(directory, ID_directory = NULL, upper_temp = 2, lower_temp = -.5, 
	fluct_temp = 3, fluct_para = "p2p", onset_days = 5, fahrenheit = FALSE,
	skip = 0, cut_month = 8, header = TRUE, sep_data = ";", sep_ID = ",", cover = NULL,
	Nr_label = NULL, plot = FALSE)
```

_…read out data loggers, formating date system and temperature units, creating two data frames:_
- _output1: a reduced data frame giving an overview of the snow period and snow conditions (in absolut and relativ amounts) per unique data logger and winter_
- _output2: detailed data frame with all environmental data points per day and further calculations (i.e. fluctuations)_
  
_establish snow period and daily snow conditions per winter, plot result in one graph `plot = T`._

<details>
<summary>Function parameter description:</summary>

> **directory:** (_string of characters_) path to .csv-file

> **ID_directory:** (_string of characters_) path to supplementary .csv-file. The default is NULL; it will not be used if set to NULL.
> 	A separate .csv file from the .csv file containing the environment data can be provided to extract the cover label and/or an alternative
> 	label name. The unique ID (serial number on the data logger) has to be provided in this supplementary file to assign the correct cover and/or label names.
> 	Header structure of .csv file: ID, Cover, Logger_Nr. (order is irrelevant)

> **upper_temp:** (_float_) Defines the highest temperature limit to determine suitable conditions. The default is 2 °C.

> **lower_temp:** (_float_) Defines the lowest temperature limit to determine suitable conditions. The default is -.5 °C.

> **fluct_temp:** (_float_) Defines the absolute temperature deviation limit to determine suitable conditions. The default is 3 °C.

> **fluct_para:** (_string of characters_) determining which deviation formula should be used:
> - standard deviation (“sd”),
> - variance (“var”),
> - the maximum difference from the mean (“p2x”),
> - the difference between maximum and minimum reached value (“p2p”),
> - the sum of difference to mean (“sd2x”),
> - the sum of difference to zero degree celsius (“sd2z”),
> - or all of the above (“all”).

> **onset_days:** (_integer_) defines the starting condition when the snow season starts (first period of n or more consecutive “green” days). Shorter periods that get > interupted by other categories will be then ignored. The default is 5 days.

> **fahrenheit:** (_boolean_) an internal function that converts given data from Fahrenheit to Celsius. The default is FALSE.

> **skip:** (_integer_) defines how many top rows in the .csv file should be ignored.  The default is 0.

> **cut_month:** (_integer_) separator to determine each year's winter season. The default is 8, with the first day in August the next winter starts.

> **header:** (_boolean_) Whether the first row is used for setting the column titles. The default is TRUE.

> **sep_data:** (_string of characters_) separation character to be used during reading in the .csv file. The default is ”;”.

> **sep_ID:** (_string of characters_) separation character to be used during reading in the supplementary .csv file. The default is “,”.

> **cover:** (_string of characters_) information on whether the logger was placed covered by trees or shrubs (close) or openly without any obstructions above it (open). The default is NULL; it will not be used if set to NULL. 

> **Nr_label:** (_string of characters_) The default is NULL; it will not be used if set to NULL. An alternative form to assign the data to a unique name.

> **plot:** (_boolean_) Whether at the end of caculations a plot over all winters within the main file should be plotted. The default is FALSE.


</details>

<details>
<summary>Output description:</summary>
	
#### **Output1:**
Per temperature sensor 38 columns will be generated presenting overview information.

|ID       |Winter | Nr. of suitable fragments 03| Nr. of no snow fragments 03| Nr. of unsuitable fragments 03| lcd_s_03| td_s_03| td_ns_03| td_ud_03| d_ss_03| rel. s d_03| rel. ns d_03| rel. u d_03| Min. temperature_03| Nr. of suitable fragments 02| Nr. of no snow fragments 02| Nr. of unsuitable fragments 02| lcd_s_02| td_s_02| td_ns_02| td_ud_02| d_ss_02| rel. s d_02| rel. ns d_02| rel. u d_02| Min. temperature_02| Nr. of suitable fragments 01| Nr. of no snow fragments 01| Nr. of unsuitable fragments 01| lcd_s_01| td_s_01| td_ns_01| td_ud_01| d_ss_01| rel. s d_01| rel. ns d_01| rel. u d_01| Min. temperature_01|
|:--------|:------|----------------------------:|---------------------------:|------------------------------:|--------:|-------:|--------:|--------:|-------:|-----------:|------------:|-----------:|-------------------:|----------------------------:|---------------------------:|------------------------------:|--------:|-------:|--------:|--------:|-------:|-----------:|------------:|-----------:|-------------------:|----------------------------:|---------------------------:|------------------------------:|--------:|-------:|--------:|--------:|-------:|-----------:|------------:|-----------:|-------------------:|
|95222856 |22/23  |                            4|                           3|                              0|      124|     158|        5|        0|     163|    96.93252|     3.067485|           0|               0.625|                            8|                           4|                              4|       99|     163|       13|        7|     183|    89.07104|     7.103825|    3.825137|             -1.5000|                           10|                           6|                              9|       66|      91|       18|       73|     182|    50.00000|      9.89011|    40.10989|             -6.5000|
|95222856 |23/24  |                            1|                           0|                              0|      130|     130|        0|        0|     130|   100.00000|     0.000000|           0|               0.750|                            1|                           0|                              0|      154|     154|        0|        0|     154|   100.00000|     0.000000|    0.000000|              0.8125|                            3|                           3|                              3|      160|     162|       28|       17|     207|    78.26087|     13.52657|     8.21256|             -4.3750|
|95222856 |24/25  |                            1|                           0|                              0|      115|     115|        0|        0|     115|   100.00000|     0.000000|           0|               0.625|                            1|                           0|                              0|      134|     134|        0|        0|     134|   100.00000|     0.000000|    0.000000|              0.2500|                           12|                          10|                             10|       92|     118|       93|       29|     240|    49.16667|     38.75000|    12.08333|             -8.1875|


#### **Output2:**
Per temperature sensor 37 columns will be generated presenting average information per day.


|Day        | Avg_temperature_03| Avg_temperature_02| Avg_temperature_01| Avg_moisture|ID       |Winter | Winter_id| Fluct_temperature_03| Fluct_temperature_02| Fluct_temperature_01|no_snow_03a |no_snow_02a |no_snow_01a |no_snow_03b |no_snow_02b |no_snow_01b |suitable_condition_03 |suitable_condition_02 |suitable_condition_01 |unsuitable_condition_03 |unsuitable_condition_02 |unsuitable_condition_01 |condition_03 |condition_02 |condition_01 | Max_temperature_03| Max_temperature_02| Max_temperature_01| Max_moisture| Min_temperature_03| Min_temperature_02| Min_temperature_01| Min_moisture|snow_period_03 |snow_period_02 |snow_period_01 |
|:----------|------------------:|------------------:|------------------:|------------:|:--------|:------|---------:|--------------------:|--------------------:|--------------------:|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:---------------------|:---------------------|:---------------------|:-----------------------|:-----------------------|:-----------------------|:------------|:------------|:------------|------------------:|------------------:|------------------:|------------:|------------------:|------------------:|------------------:|------------:|:--------------|:--------------|:--------------|
|2022-09-01 |           24.17763|           24.33026|           24.18816|     390.9684|95222856 |22/23  |         1|               1.1250|               1.1250|               1.1875|TRUE        |TRUE        |TRUE        |FALSE       |FALSE       |FALSE       |FALSE                 |FALSE                 |FALSE                 |FALSE                   |FALSE                   |FALSE                   |no_snow      |no_snow      |no_snow      |            24.7500|            24.8750|            24.8125|          394|            23.6250|            23.7500|            23.6250|          388|FALSE          |FALSE          |FALSE          |
|2022-09-02 |           23.91276|           24.06380|           23.92253|     390.0312|95222856 |22/23  |         1|               1.0000|               1.0000|               1.0625|TRUE        |TRUE        |TRUE        |FALSE       |FALSE       |FALSE       |FALSE                 |FALSE                 |FALSE                 |FALSE                   |FALSE                   |FALSE                   |no_snow      |no_snow      |no_snow      |            24.3750|            24.5000|            24.4375|          392|            23.3750|            23.5000|            23.3750|          387|FALSE          |FALSE          |FALSE          |
|2022-09-03 |           23.93620|           24.08854|           23.94857|     390.3229|95222856 |22/23  |         1|               0.9375|               0.8750|               0.9375|TRUE        |TRUE        |TRUE        |FALSE       |FALSE       |FALSE       |FALSE                 |FALSE                 |FALSE                 |FALSE                   |FALSE                   |FALSE                   |no_snow      |no_snow      |no_snow      |            24.3750|            24.5000|            24.3750|          393|            23.4375|            23.6250|            23.4375|          388|FALSE          |FALSE          |FALSE          |
|2022-09-04 |           23.95117|           24.11263|           23.98177|     390.7188|95222856 |22/23  |         1|               1.0625|               1.0625|               1.1250|TRUE        |TRUE        |TRUE        |FALSE       |FALSE       |FALSE       |FALSE                 |FALSE                 |FALSE                 |FALSE                   |FALSE                   |FALSE                   |no_snow      |no_snow      |no_snow      |            24.4375|            24.6250|            24.5000|          393|            23.3750|            23.5625|            23.3750|          388|FALSE          |FALSE          |FALSE          |
|2022-09-05 |           24.17904|           24.34310|           24.26562|     379.5104|95222856 |22/23  |         1|               1.0625|               1.0625|               1.2500|TRUE        |TRUE        |TRUE        |FALSE       |FALSE       |FALSE       |FALSE                 |FALSE                 |FALSE                 |FALSE                   |FALSE                   |FALSE                   |no_snow      |no_snow      |no_snow      |            24.8125|            24.9375|            25.0000|          392|            23.7500|            23.8750|            23.7500|          369|FALSE          |FALSE          |FALSE          |

</details>

<details>
	
<summary>Internal function structure:</summary>

These are the internal functions that the main function uses to analyze the raw files and generate the two output data frames. The internal function are used in order  of appearance.

```R
temp_prep(directory, ID_directory = NULL, sep_ID = ",", cover = NULL, Nr_label = NULL)
```

_…creating a template data frame filled with the ID number and, if possible, cover type and alternative unique name._

> **directory:** (_string of characters_) path to .csv-file 

> **ID_directory:** (_string of characters_) path to supplementary .csv-file. The default is NULL; it will not be used if set to NULL.
> 	A separate .csv file from the .csv file containing the environment data can be provided to extract the cover label and/or an alternative
> 	label name. The unique ID (serial number on the data logger) has to be provided in this supplementary file to assign the correct cover and/or label names.
> 	Header structure of .csv file: ID, Cover, Logger_Nr. (order is irrelevant)

> **sep_ID:** (_string of characters_) separation character to be used during reading in the supplementary .csv file. The default is “,”.

> **cover:** (_string of characters_) information on whether the logger was placed covered by trees or shrubs (close) or openly without any obstructions above it (open). The default is NULL; it will not be used if set to NULL. 

> **Nr_label:** (_string of characters_) The default is NULL; it will not be used if set to NULL. An alternative form to assign the data to a unique name.

```R
read_in_tomst(directory, sep_data = ";", header = TRUE, skip = 0, fahrenheit = FALSE)
```

_…read the TOMST .csv file, select the relevant columns by position, determine and unify the date system over the data frame, convert temperature units to Celsius or Fahrenheit, 
	directory: (string of characters) path to .csv-file_
 
> **sep_data:** (_string of characters_) separation character to be used during reading in the .csv file. The default is ”;”.

> **header:** (_boolean_) Whether the first row is used for setting the column titles. The default
is TRUE.

> **skip:** (_integer_) defines how many top rows in the .csv file should be ignored.  The default
is 0.

> **fahrenheit:** (_boolean_) an internal function that converts given data from Fahrenheit to Celsius. The
default is FALSE.

```R
avg_dataframe(data, temp, cover = NULL, Nr_label = NULL, cut_month = 8)
```

_…creating the data frame “daily average” by aggregating the TOMST data per day, introducing the column “Winter” and “Winter_id”: 
The "Winter" column is assigned a unique identifier for each winter season. A winter is defined as the period between the appearance of the month specified by the parameter "cut_month" in one year and the subsequent appearance of the same month in the following year. The column “Winter_id” represents the same information as the “Winter”-column but as an integer value, starting with 1._

> **data:** (_data frame_) data frame coming from the function read_in_tomst()

> **temp:** (_data frame_) data frame coming from the function temp-prep()

> **cover:** see function temp-prep()

> **Nr_label:** see function temp-prep()

> **cut_month:** (_integer_) separator to determine each year's winter season. The
default is 8, with the first day in August the next winter starts.

```R
avg_fluct(data, daily_averages, fluct_para = "p2p")
```

_…calculating for the temperature columns the daily deviation based on different formulas or approaches._
	
> **data:** (_data frame_) from avg_dataframe()
	
> **daily_averages:** (_data frame_) from avg_dataframe()

> **fluct_para:** (_string of characters_) determining which deviation formula should be used:
> - standard deviation (“sd”),
> - variance (“var”),
> - the maximum difference from the mean (“p2x”),
> - the difference between maximum and minimum reached value (“p2p”),
> - the sum of difference to mean (“sd2x”),
> - the sum of difference to zero degree celsius (“sd2z”),
> - or all of the above (“all”).
> 
> Those deviations or fluctuations are later used to determine the daily snow condition. The default is “p2p”.

```R
avg_conditions(daily_averages, upper_temp = 2, lower_temp = -.5, fluct_temp = 3)
```

_…determining the daily snow condition for each temperature column based on the fluctuations and function parameters. Three different snow conditions can be classified:_
- _no snow: the average temperature is above “upper temp,” or average temperature is below “upper temp,” above “lower_temp,” but fluctuation temperature is above “fluct_temp” under those conditions, a protective snow cover cannot exist on the surface_
- _unsuitable: the average temperature is below “lower_temp”  under those conditions, the soil freezes, and no myxomycete can survive_  
- _suitable: the average temperature is between “upper_temp” and “lower_temp,” and the fluctuation temperature is smaller than “fluct_temp” under those conditions, a protective snow cover exists and myxomycetes can survive or even multiply_

**daily_averages:** (_data frame_) from avg_fluct()

> **upper_temp:** (_float_) Defines the highest temperature limit to determine suitable conditions. The default is 2 °C.

> **lower_temp:** (_float_) Defines the lowest temperature limit to determine suitable conditions. The default is -.5 °C.

> **fluct_temp:** (_float_) Defines the absolute temperature deviation limit to determine suitable conditions. The default is 3 °C.

```R
avg_minmax(data, daily_averages)
```

_…calculating the maximum and minimum temperature per day per temperature column_
	
> **data:** (_data frame_) from avg_dataframe()

> **daily_averages:** (_data frame_) from avg_conditions()

```R
snow_seq(data, temp, daily_averages, onset_days = 5)
```

_…determines the snow period by the first days of suitable days longer than “onset_days” each winter. Calculate the absolute and relative number of condition fragments and the length of those fragments._
	
> **data:** (_data frame_) from avg_dataframe()

> **temp:** (_data frame_) from avg_dataframe() 

> **daily_averages:** (_data frame_) from avg_minmax()

> **onset_days:** (_integer_) defines the starting condition when the snow season starts (first period of n or more consecutive “green” days). Shorter periods that get > interupted by other categories will be then ignored. The default is 5 days.

```R
tomst_snow_plot(overview, daily_averages)
```

_…plotting all data for all existing winters in the data frame._

> **overview:** (_data frame_) from tomst_snow()

> **daily_averages:** (_data frame_) from tomst_snow()

</details>

### Additional functions

```R
tomst_snow_plot_sep(overview, daily_averages, selection = NULL)
```

_…plotting all data for each winter with the optional selection of only specific winters according to the column “Winter_id” and the parameter `selection`_

> **overview:** (_data frame_) from tomst_snow()

> **daily_averages:** (_data frame_) from tomst_snow()

> **selection:** (_list of integers_) The default is NULL; all winters will be plotted.

## How to use

Run a file:
```R
# define location to raw data:
tomst_rawdata<-"path/to/file.csv"

# use the main function to generate the two analyzed data frames, with default settings (except "plot"):
result<-tomst_snow(tomst_rawdata, plot = T)

# access the two data frames:
result$output1
result$output2
```

Run an entire folder:
```R
# define folder location of raw data:
files <- list.files("path/to/folder/", pattern = "data", full.names = T)

# Loop through all files in the folder and append them in two data frames:
for (i in files){
  temp_result<-tomst_snow(i, plot = F)
  if (i == files[1]){
    result_output_1<-temp_result$output1
    result_output_2<-temp_result$output2
  }else{
    result_output_1<-rbind(result_output_1, temp_result$output1)
    result_output_2<-rbind(result_output_2, temp_result$output2)
  }
}

```
Alternative approach for multiple files:

```R
# define folder location of raw data:
files <- list.files("path/to/folder/", pattern = "data", full.names = TRUE)

# Apply the `tomst_snow` function to the first three files
temp_results <- lapply(files[1:3], tomst_snow, plot = FALSE)

# Extract and combine the outputs
res_output_1 <- do.call(rbind, lapply(temp_results, `[[`, "output1"))
res_output_2 <- do.call(rbind, lapply(temp_results, `[[`, "output2"))
```

Plot all winters together:

```R
tomst_snow_plot(overview = res$output1, daily_averages = res$output2)
```
<details>
<summary>
Show plot
</summary>
	
![Examplary plot of a TOMST datalogger with multiple winters.](/images/Datalogger_tomst_all.png)
</details>

Plot selected winters separated:

```R
tomst_snow_plot_sep(overview = result$output1, daily_averages = result$output2, selection = c(1,2,3))
```

<details>
<summary>
Show plots
</summary>

![Examplary plot of a TOMST datalogger with the 1st winter.](/images/Datalogger_tomst_1.png)
![Examplary plot of a TOMST datalogger with the 2nd winter.](/images/Datalogger_tomst_2.png)
![Examplary plot of a TOMST datalogger with the 3rd winter.](/images/Datalogger_tomst_3.png)
</details>
