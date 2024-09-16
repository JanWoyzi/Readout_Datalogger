# Readout_Dattalogger

R (4.3.0, 2023-04-21)
Packages needed: 
Pacman: (0.5.1)
ggplot2: (2.3.4.2)
lubridate: (1.9.2)
ggnewscale: (0.4.9)
dplyr: (1.1.2)
glue: (1.6.2)

Main function structure:
tomst_snow(directory, ID_directory = NULL, upper_temp = 2, lower_temp = -.5, 
fluct_temp = 3, fluct_para = "p2p", onset_days = 5, fahrenheit = FALSE, skip = 0, cut_month = 8, header = TRUE, sep_data = ";", sep_ID = ",", cover = NULL, 
Nr_label = NULL, plot = FALSE)

temp-prep(directory, ID_directory = NULL, sep_ID = ",", cover = NULL, Nr_label = NULL)
…creating a template data frame filled with the ID number and, if possible, cover type and alternative unique name.
	directory: (string of characters) path to .csv-file 
ID_directory: ID_directory: (string of characters) path to supplementary .csv-file. The default is NULL; it will not be used if set to NULL. A separate .csv file from the .csv file containing the environment data can be provided to extract the cover label and/or an alternative label name. The unique ID (serial number on the data logger) has to be provided in this file to assign the correct cover and/or label names.
Header structure of .csv file: ID, Cover, Logger_Nr. (order is irrelevant)
sep_ID: (string of characters) separation character to be used during reading in the supplementary .csv file. The default is “,”.
cover: (string of characters) information on whether the logger was placed covered by trees or shrubs (close) or openly without any obstructions above it (open). The default is NULL; it will not be used if set to NULL. 
Nr_label: (string of characters) The default is NULL; it will not be used if set to NULL. An alternative form to assign the data to a unique name.

read_in_tomst(directory, sep_data = ";", header = TRUE, skip = 0, fahrenheit = FALSE)
…read the TOMST .csv file, select the relevant columns by position, determine and unify the date system over the data frame, convert temperature units to Celsius or Fahrenheit, 
	directory: (string of characters) path to .csv-file
sep_data: (string of characters) separation character to be used during reading in the .csv file. The default is ”;”.
header: (boolean) Whether the first row is used for setting the column titles. The default
is TRUE.  
skip: (integer) defines how many top rows in the .csv file should be ignored.  The default
is 0.
fahrenheit: an internal function that converts given data from Fahrenheit to Celsius. The
default is FALSE.

avg_dataframe(data, temp, cover = NULL, Nr_label = NULL, cut_month = 8)
…creating the data frame “daily average” by aggregating the TOMST data per day, introducing the column “Winter” and “Winter_id”: 
The "Winter" column is assigned a unique identifier for each winter season. A winter is defined as the period between the appearance of the month specified by the parameter "cut_month" in one year and the subsequent appearance of the same month in the following year.
	The column “Winter_id” represents the same information as the “Winter”-column but as an integer value, starting with 1.
	data: (data frame) data frame coming from the function read_in_tomst()
	temp: (data frame) data frame coming from the function temp-prep()
	cover: see function temp-prep()
	Nr_label: see function temp-prep()
cut_month: (integer) separator to determine each year's winter season. The
default is 8, with the first day in August the next winter starts.

avg_fluct(data, daily_averages, fluct_para = "p2p")
…calculating for the temperature columns the daily deviation based on different formulas or approaches.
	data: (data frame) from avg_dataframe()
	daily_averages: (data frame) from avg_dataframe()
fluct_para: (string of characters) determining which deviation formula should be used: standard deviation (“sd”), variance (“var”), the maximum difference from the mean (“p2x”), the difference between maximum and minimum reached value (“p2p”), the sum of difference to mean (“sd2x”), the sum of difference to zero degree celsius (“sd2z”), or all of the above (“all”). Those deviations or fluctuations are later used to determine the daily snow condition. The default is “p2p”.

avg_conditions(daily_averages, upper_temp = 2, lower_temp = -.5, fluct_temp = 3)
…determining the daily snow condition for each temperature column based on the fluctuations and function parameters. Three different snow conditions can be classified: 
- no snow: the average temperature is above “upper temp,” or average temperature is below “upper temp,” above “lower_temp,” but fluctuation temperature is above “fluct_temp” under those conditions, a protective snow cover cannot exist on the surface
	- unsuitable: the average temperature is below “lower_temp”  under those conditions, the soil freezes, and no myxomycete can survive  
	- suitable: the average temperature is between “upper_temp” and “lower_temp,” and the fluctuation temperature is smaller than “fluct_temp”  under those conditions, a protective snow cover exists and myxomycetes can survive or even multiply 
	daily_averages: (data frame) from avg_fluct()
upper_temp: (float) Defines the highest temperature limit to determine suitable conditions. The default is 2 °C.
lower_temp: (float) Defines the lowest temperature limit to determine suitable conditions. The default is -.5 °C.
fluct_temp: (float) Defines the absolute temperature deviation limit to determine suitable conditions. The default is 3 °C.

avg_minmax(data, daily_averages)
…calculating the maximum and minimum temperature per day per temperature column
	data: (data frame) from avg_dataframe()
	daily_averages: (data frame) from avg_conditions()

snow_seq(data, temp, daily_averages, onset_days = 5)
…determines the snow period by the first days of suitable days longer than “onset_days” each winter. Calculate the absolute and relative number of condition fragments and the length of those fragments.
	data: (data frame) from avg_dataframe()
	temp: (data frame) from avg_dataframe() 
	daily_averages: (data frame) from avg_minmax()
	onset_days: (integer) The default is 5 days.

tomst_snow_plot(overview, daily_averages)
…plotting all data for all existing winters in the data frame.
	overview: (data frame) from snow_seq()
	daily_averages: (data frame) from snow_seq()

tomst_snow_plot_sep(overview, daily_averages, selection = NULL)
…plotting all data for each winter with the optional selection of only specific winters according to the column “Winter_id” and the parameter “selection.”
	overview: (data frame)
	daily_averages: (data frame)
	selection: (list of integers) The default is NULL; all winters will be plotted.

