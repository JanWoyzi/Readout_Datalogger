
# package requirements: ggplot2, lubridate, ggnewscale, dplyr
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, 
               lubridate, 
               ggnewscale, 
               dplyr,
               glue)

library(ggplot2)

# internal functions: ----
# functions for calculation fluctuation per day:

sum_of_differences_to_zero <- function(x) {
  sum(abs(x - 0))
}

sum_of_differences_to_mean <- function(x) {
  sum(abs(x - mean(x, na.rm = TRUE)))
}

max_difference_from_mean <- function(x) {
  max(abs(x - mean(x, na.rm = TRUE)))
}

difference_between_max_and_min_differences <- function(x) {
  max_diff <- max(x - mean(x, na.rm = TRUE))
  min_diff <- min(x - mean(x, na.rm = TRUE))
  max_diff - min_diff
}
# function to convert units:
convert_Fahrenheit2Celsius <- function(x) {
  (x - 32) * (5/9)
}

# function to get only the first part of a strsplit operation:
get_first_part <- function(x, sep_element) {
  split_res <- strsplit(x, sep_element)[[1]]
  return(split_res[1])
}

# main functions:
temp_prep <- function(directory, ID_directory = NULL, sep_ID = ",",
                      cover = NULL, Nr_label = NULL){
  # template data frame: ----
  temp <- data.frame(matrix(ncol = 1, nrow = 1))
  # ID ----
  colnames(temp) <- c("ID")
  t1<-tail(strsplit(directory, "/")[[1]], n=1)
  t2<-strsplit(t1, ".csv")[[1]]
  id <- strsplit(t2, "_")[[1]][2]
  temp$ID <- id
  
  # Logger label.: ----
  # Cover: ---- # what if no cover is wished to be used #TODO
  if (!is.null(ID_directory)) { # if a ID file is given
    ID_df <- read.csv2(ID_directory, sep = sep_ID, header = T, skip = 0)
    temp$`Logger nr.` <- ID_df[ID_df$ID == id,]$Logger_Nr.
    if (any(colnames(ID_df) == "Cover")) {
      temp$Cover <- ID_df[ID_df$ID == id,]$Cover
    }
  } else { # if no id file is given
    if (!is.null(Nr_label)) { # if label is given in function parameter
      temp$`Logger nr.` <- Nr_label
    }
    if (!is.null(cover)){
      temp$Cover <- cover
    }
  }
  return(output1 = temp)
}

read_in_tomst <- function(directory, sep_data = ";", header = TRUE, skip = 0,
                          fahrenheit = FALSE){
  # data prep:----
  data <- read.csv2(directory, sep = sep_data, header = header, skip = skip)[ , c(1,2,4:7)] # read only the columns in, containing relevant environmental information
  colnames(data) <- c("Index", "Time", "Temperature_03", "Temperature_02", "Temperature_01", "Moisture") # rename the column names !!! ID of HOBO unity gets lost!!!
  #* date format decision tree: ----
  
  clock<-nchar(strsplit(data$Time, " ")[[1]][2]) # number of characters of time can tell if seconds is used or not
  # find the first element that is not a digit--> result should be the index of the date separator:
  index_date <- regexpr("\\D", strsplit(data$Time[1], " ")[[1]][1])
  # use the index of the date separator to extract the char of the date separator:
  sep_date <- substr(strsplit(data$Time[1], " ")[[1]][1], index_date, index_date)
  # first split date from clock time with " ", then use date separator to split into three parts
  date_list <- purrr::transpose(strsplit(unlist(lapply(data$Time[1:3000], get_first_part, sep_element = " ")), sep_date))
  date_df<-data.frame("length" = c(length(unique(purrr::list_simplify(date_list[[1]]))), # splitting the 3 positions up but keep them together in a dataframe
                                   length(unique(purrr::list_simplify(date_list[[2]]))),
                                   length(unique(purrr::list_simplify(date_list[[3]])))),
                      "position" = c(1,2,3))
  date_df<-date_df[order(date_df$length), ] # reorder according to number of changes during chosen time period
  rownames(date_df) <- NULL # reindex
  date_df$datum[1] <- "y" # lowest amount of changes (max. 2)
  date_df$datum[2] <- "m" #  should be between, max. amount of changes = 12
  date_df$datum[3] <- "d" # most amount of changes (max. 31)
  date_format<-paste(date_df[order(date_df$position), 3], collapse = "") # put the datum info together according to their original position
  if (clock == 8) { # in combination to clock and date_format, the correct datetime function can be called
    datum_format<-paste(date_format, "hms", sep = "_")
  } else {
    datum_format<-paste(date_format, "hm", sep = "_")
  }
  switch (datum_format,
          "ymd_hms" = data$Time <- lubridate::ymd_hms(data$Time),
          "ydm_hms" = data$Time <- lubridate::ydm_hms(data$Time),
          "mdy_hms" = data$Time <- lubridate::mdy_hms(data$Time),
          "dmy_hms" = data$Time <- lubridate::dmy_hms(data$Time),
          "ymd_hm" = data$Time <- lubridate::ymd_hm(data$Time),
          "ydm_hm" = data$Time <- lubridate::ydm_hm(data$Time),
          "mdy_hm" = data$Time <- lubridate::mdy_hm(data$Time),
          "dmy_hm" = data$Time <- lubridate::dmy_hm(data$Time))
  
  data <- dplyr::mutate_at(data, c("Temperature_03", "Temperature_02", "Temperature_01", "Moisture"), as.numeric)
  if (fahrenheit == TRUE){ # check if conversion to °C is necessary, see above
    dplyr::mutate_at(data, c("Temperature_03", "Temperature_02", "Temperature_01"), convert_Fahrenheit2Celsius)
  }
  return(data)
}

avg_dataframe <- function(data, temp, cover = NULL, Nr_label = NULL, 
                          cut_month = 8) { 
  # creating data frame: daily average ----
  data$Day <- format(data$Time, "%Y-%m-%d") # for the daily average
  daily_averages <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01, data$Moisture), by = list(data$Day), FUN = mean, na.rm = TRUE)
  colnames(daily_averages) <- c("Day", "Avg_temperature_03", "Avg_temperature_02", "Avg_temperature_01", "Avg_moisture")
  daily_averages$Day <- as.POSIXct(daily_averages$Day, format = "%Y-%m-%d")
  daily_averages$ID <- temp$ID[1]
  if (!is.null(cover)) {
    daily_averages$Cover <- cover
  }
  if (!is.null(Nr_label)) {
    daily_averages$`Logger nr.` <- Nr_label
    daily_averages$Nr_cover <- paste(Nr_label, cover, sep = " ") # lazy solution for later plotting
  }
  
  daily_averages$Winter<-ifelse(month(daily_averages$Day) > cut_month - 1, 
                                paste(substr(year(daily_averages$Day), 3, 4), substr(year(daily_averages$Day) + 1, 3, 4), sep = "/"), 
                                paste(substr(year(daily_averages$Day)-1, 3, 4), substr(year(daily_averages$Day), 3, 4), sep = "/"))
  daily_averages$Winter_id <- as.integer(factor(daily_averages$Winter))
  
  # Winter ----
  data$Day <- as.POSIXct(data$Day, format = "%Y-%m-%d")
  data <- merge(x = data, y = daily_averages[ , c("Day", "Winter")], by = "Day", all.x=TRUE)
  
  winter_events <- unique(daily_averages$Winter)
  temp<-cbind(temp, winter_events)
  colnames(temp)[length(colnames(temp))] <- "Winter" #rename the last col
  return(list(data = data, output1 = temp, output2 = daily_averages))
}

avg_fluct <- function(data, daily_averages, fluct_temp = 3, fluct_para = "p2p"){
  # > Parameters----
  # ** Fluctuation:----
  if (fluct_para[1] == "all"){
    fluct_para <- c("sd", "var", "p2x", "p2p", "sd2x", "sd2z")
  }
  for (i in seq(fluct_para)){
    switch(fluct_para[i],
           "sd" = { daily_sd <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01), by = list(data$Day), FUN = sd, na.rm = TRUE)
           colnames(daily_sd) <- c("Day", "Fluct_temperature_03", "Fluct_temperature_02", "Fluct_temperature_01") # SD temperature
           daily_sd$Day<- as.POSIXct(daily_sd$Day, format = "%Y-%m-%d")
           daily_averages<-merge(daily_averages, daily_sd, by = "Day")},
           "var" = {daily_var <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01), by = list(data$Day), FUN = var, na.rm = TRUE)
           colnames(daily_var) <- c("Day", "Fluct_temperature_03", "Fluct_temperature_02", "Fluct_temperature_01") # Variance temperature
           daily_var$Day<- as.POSIXct(daily_var$Day, format = "%Y-%m-%d")
           daily_averages<-merge(daily_averages, daily_var, by = "Day")},
           "p2x" = {daily_max_diff_mean <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01), by = list(data$Day), FUN = max_difference_from_mean)
           colnames(daily_max_diff_mean) <- c("Day", "Fluct_temperature_03", "Fluct_temperature_02", "Fluct_temperature_01") # Max difference to avg °C
           daily_max_diff_mean$Day<- as.POSIXct(daily_max_diff_mean$Day, format = "%Y-%m-%d")
           daily_averages<-merge(daily_averages, daily_max_diff_mean, by = "Day")},
           "p2p" = {daily_p2p <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01), by = list(data$Day), FUN = difference_between_max_and_min_differences)
           colnames(daily_p2p) <- c("Day", "Fluct_temperature_03", "Fluct_temperature_02", "Fluct_temperature_01") # (max)peak to (min)peak
           daily_p2p$Day<- as.POSIXct(daily_p2p$Day, format = "%Y-%m-%d")
           daily_averages<-merge(daily_averages, daily_p2p, by = "Day")},
           "sd2x" = {daily_diff_mean <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01), by = list(data$Day), FUN = sum_of_differences_to_mean)
           colnames(daily_diff_mean) <- c("Day", "Fluct_temperature_03", "Fluct_temperature_02", "Fluct_temperature_01") # Sum of difference to avg °C
           daily_diff_mean$Day<- as.POSIXct(daily_diff_mean$Day, format = "%Y-%m-%d")
           daily_averages<-merge(daily_averages, daily_diff_mean, by = "Day")},
           "sd2z" = {daily_diff_0 <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01), by = list(data$Day), FUN = sum_of_differences_to_zero)
           colnames(daily_diff_0) <- c("Day", "Fluct_temperature_03", "Fluct_temperature_02", "Fluct_temperature_01") # Sum of difference to 0°C
           daily_diff_0$Day<- as.POSIXct(daily_diff_0$Day, format = "%Y-%m-%d")
           daily_averages<-merge(daily_averages, daily_diff_0, by = "Day")})
  }
  return(daily_averages)
}

avg_conditions <- function(daily_averages, upper_temp = 2, 
                           lower_temp = -.5, fluct_temp = 3){
  # ** Condition decision tree:----
  daily_averages<-dplyr::mutate(daily_averages, 
                                no_snow_03a = Avg_temperature_03 > upper_temp,
                                no_snow_02a = Avg_temperature_02 > upper_temp,
                                no_snow_01a = Avg_temperature_01 > upper_temp,
                                no_snow_03b = Avg_temperature_03 < upper_temp & # "no_snow_02" will be declared later to "no_snow"
                                  Avg_temperature_03 > lower_temp &
                                  Fluct_temperature_03 > fluct_temp,
                                no_snow_02b = Avg_temperature_02 < upper_temp & 
                                  Avg_temperature_02 > lower_temp &
                                  Fluct_temperature_02 > fluct_temp,
                                no_snow_01b = Avg_temperature_01 < upper_temp & 
                                  Avg_temperature_01 > lower_temp &
                                  Fluct_temperature_01 > fluct_temp,
                                suitable_condition_03 = Avg_temperature_03 <= upper_temp & 
                                  Avg_temperature_03 >= lower_temp &
                                  Fluct_temperature_03 <= fluct_temp,
                                suitable_condition_02 = Avg_temperature_02 <= upper_temp & 
                                  Avg_temperature_02 >= lower_temp &
                                  Fluct_temperature_02 <= fluct_temp,
                                suitable_condition_01 = Avg_temperature_01 <= upper_temp & 
                                  Avg_temperature_01 >= lower_temp &
                                  Fluct_temperature_01 <= fluct_temp,
                                unsuitable_condition_03 = Avg_temperature_03 < lower_temp,
                                unsuitable_condition_02 = Avg_temperature_02 < lower_temp,
                                unsuitable_condition_01 = Avg_temperature_01 < lower_temp,
                                
                                condition_03 = dplyr::case_when(
                                  no_snow_03a ~ "no_snow",
                                  no_snow_03b ~ "no_snow", # no_snow_0xb cases are turned to no_snow
                                  suitable_condition_03 ~ "suitable_condition",
                                  unsuitable_condition_03 ~ "unsuitable_condition",
                                  TRUE ~ "none"),
                                
                                condition_02 = dplyr::case_when(
                                  no_snow_02a ~ "no_snow",
                                  no_snow_02b ~ "no_snow", 
                                  suitable_condition_02 ~ "suitable_condition",
                                  unsuitable_condition_02 ~ "unsuitable_condition",
                                  TRUE ~ "none"),
                                
                                condition_01 = dplyr::case_when(
                                  no_snow_01a ~ "no_snow",
                                  no_snow_01b ~ "no_snow", 
                                  suitable_condition_01 ~ "suitable_condition",
                                  unsuitable_condition_01 ~ "unsuitable_condition",
                                  TRUE ~ "none"))
  return(daily_averages)
}

avg_minmax <- function(data, daily_averages){
  # Max temp per day: ----
  daily_max <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01, data$Moisture), by = list(data$Day), FUN = max, na.rm = TRUE)
  colnames(daily_max) <- c("Day", "Max_temperature_03", "Max_temperature_02", "Max_temperature_01", "Max_moisture")
  daily_max$Day<- as.POSIXct(daily_max$Day, format = "%Y-%m-%d")
  daily_averages<-merge(daily_averages, daily_max, by = "Day")
  
  # Min temp per day: ----
  daily_min <- aggregate(list(data$Temperature_03, data$Temperature_02, data$Temperature_01, data$Moisture), by = list(data$Day), FUN = min, na.rm = TRUE)
  colnames(daily_min) <- c("Day", "Min_temperature_03", "Min_temperature_02", "Min_temperature_01", "Min_moisture")
  daily_min$Day<- as.POSIXct(daily_min$Day, format = "%Y-%m-%d")
  daily_averages<-merge(daily_averages, daily_min, by = "Day")
  return(daily_averages)
}

snow_seq <- function(data, temp, daily_averages, onset_days = 5){
  loop_list <- c("03", "02", "01")
  for (w in unique(daily_averages$Winter)){
    for (i in loop_list) {
      # Create a sequence of days with snow cover
      snow_cover_sequence_varname <- paste0("snow_cover_sequence_", i) # sub(".*_", "", i)
      daily_averages_varname <- paste0("condition_", i)
      colum_index <- which(colnames(daily_averages) == daily_averages_varname)
      assign(snow_cover_sequence_varname, rle(daily_averages[daily_averages$Winter == w, colum_index]))
      
      # Break point if not all sequences fulfill all requirements!
      if (all(get(snow_cover_sequence_varname)$values == "no_snow") | 
          all(get(snow_cover_sequence_varname)$lengths[which(get(snow_cover_sequence_varname)$values != "no_snow")] < onset_days)) {
        # all temp columns:
        temp_colname <- paste0("Nr. of suitable fragments ", i)
        temp[temp$Winter == w, temp_colname] <- 0 # fragments covered snow days within the snow cover period
        temp_colname <- paste0("Nr. of no snow fragments ", i)
        temp[temp$Winter == w, temp_colname] <- 0 # fragments not covered snow days within the snow cover period
        temp_colname <- paste0("Nr. of unsuitable fragments ", i)
        temp[temp$Winter == w, temp_colname] <- 0 # fragments not unsuitable days within the snow cover period
        temp_colname <- paste0("lcd_s_", i)
        temp[temp$Winter == w, temp_colname] <- 0 # Longest continues days under snow
        temp_colname <- paste0("td_s_", i)
        temp[temp$Winter == w, temp_colname] <- 0 # Total days of snow
        temp_colname <- paste0("td_ns_", i)
        temp[temp$Winter == w, temp_colname] <- 0 # Total days of no snow
        temp_colname <- paste0("td_ud_", i)
        temp[temp$Winter == w, temp_colname] <- 0 # Total days of unsuitable days
        temp_colname <- paste0("d_ss_", i)
        temp[temp$Winter == w, temp_colname] <- 0 # Days of snow seasons
        temp_colname <- paste0("rel. s d_", i)
        temp[temp$Winter == w, temp_colname] <- NA # Rel. amount of snow days
        temp_colname <- paste0("rel. ns d_", i)
        temp[temp$Winter == w, temp_colname] <- NA # Rel. amount of no snow days
        temp_colname <- paste0("rel. u d_", i)
        temp[temp$Winter == w, temp_colname] <- NA # Rel. amount of unsuitable days
        temp_colname <- paste0("Min. temperature_", i)
        data_colname <- paste0("Temperature_", i)
        temp[temp$Winter == w, temp_colname] <- min(data[data$Winter == w, data_colname], na.rm = T)
        
        # daily_average:
        daily_colname <- paste0("snow_period_", i)
        daily_averages[daily_averages$Winter == w, daily_colname] <- FALSE
        
      } else {
        # the first days of snow cover that sustain more than onset_days days
        index_first_snow_varname <- paste0("index_first_snow_", i)
        assign(index_first_snow_varname, which(get(snow_cover_sequence_varname)$values != "no_snow" & 
                                                 get(snow_cover_sequence_varname)$lengths > onset_days)[1])
        
        # the last snow cover within the winter period decided by suitable_condition and unsuitable_condition
        index_last_snow_varname <- paste0("index_last_snow_", i)
        assign(index_last_snow_varname, tail(which(get(snow_cover_sequence_varname)$values != "no_snow"), 1))
        
        # exclude days/conditions that are not IN the snow season, defined by first and last day of snow cover (see above)
        filtered_snow_cover_sequence_varname <- paste0("filtered_snow_cover_sequence_", i)
        assign(filtered_snow_cover_sequence_varname, list(
          lengths = get(snow_cover_sequence_varname)[[1]][get(index_first_snow_varname):get(index_last_snow_varname)],
          values = get(snow_cover_sequence_varname)[[2]][get(index_first_snow_varname):get(index_last_snow_varname)]
        ))
        
        # Identify continuous snow cover periods
        times_snow_cover_varname <- paste0("times_snow_cover_", i)
        assign(times_snow_cover_varname, with(get(filtered_snow_cover_sequence_varname), {
          data.frame(
            SnowCover = values,
            StartDay = cumsum(lengths) - lengths + 1,
            EndDay = cumsum(lengths)
          )
        }))
        
        # Separate snow conditions from snow cover periods
        continuous_snow_cover_varname <- paste0("continuous_snow_cover_", i)
        assign(continuous_snow_cover_varname, get(times_snow_cover_varname)[get(times_snow_cover_varname)$SnowCover == "suitable_condition", ])
        
        fragmented_snow_cover_varname <- paste0("fragmented_snow_cover_", i)
        assign(fragmented_snow_cover_varname, get(times_snow_cover_varname)[get(times_snow_cover_varname)$SnowCover == "no_snow", ]) 
        
        unsuitable_snow_cover_varname <- paste0("unsuitable_snow_cover_", i)
        assign(unsuitable_snow_cover_varname, get(times_snow_cover_varname)[get(times_snow_cover_varname)$SnowCover == "unsuitable_condition", ]) # unsuitable days (< -0.5°C) are not disjunctive to !suitable days
        
        # Insert fragments count of each condition to the second table (temp)
        temp_colname <- paste0("Nr. of suitable fragments ", i)
        temp[temp$Winter == w, temp_colname] <- nrow(get(continuous_snow_cover_varname)) # fragments covered snow days within the snow cover period
        temp_colname <- paste0("Nr. of no snow fragments ", i)
        temp[temp$Winter == w, temp_colname] <- nrow(get(fragmented_snow_cover_varname)) # fragments not covered snow days within the snow cover period
        temp_colname <- paste0("Nr. of unsuitable fragments ", i)
        temp[temp$Winter == w, temp_colname] <- nrow(get(unsuitable_snow_cover_varname)) # fragments not unsuitable days within the snow cover period
        
        # longest period under snow ----
        #continuous_snow_cover_03 <- dplyr::mutate(continuous_snow_cover_03, lenght = (EndDay - StartDay)+1)
        assign(continuous_snow_cover_varname, dplyr::mutate(get(continuous_snow_cover_varname), lenght = (EndDay - StartDay)+1))
        temp_colname <- paste0("lcd_s_", i)
        temp[temp$Winter == w, temp_colname] <- max(get(continuous_snow_cover_varname)$lenght) # Longest continues days under snow
        
        # total length of snow conditions ----
        temp_colname <- paste0("td_s_", i)
        temp[temp$Winter == w, temp_colname] <-  sum(get(continuous_snow_cover_varname)$lenght) # Total days of snow
        
        assign(fragmented_snow_cover_varname, dplyr::mutate(get(fragmented_snow_cover_varname), lenght = (EndDay - StartDay)+1))
        temp_colname <- paste0("td_ns_", i)
        temp[temp$Winter == w, temp_colname] <- sum(get(fragmented_snow_cover_varname)$lenght) # Total days of no snow
        
        assign(unsuitable_snow_cover_varname, dplyr::mutate(get(unsuitable_snow_cover_varname), lenght = (EndDay - StartDay)+1))
        temp_colname <- paste0("td_ud_", i)
        temp[temp$Winter == w, temp_colname] <- sum(get(unsuitable_snow_cover_varname)$lenght) # Total days of unsuitable days
        
        # length of snow cover season ----,
        temp_colname <- paste0("d_ss_", i)
        temp[temp$Winter == w, temp_colname] <- max(get(times_snow_cover_varname)$EndDay) # Days of snow seasons
        
        # snow period begin and end day: 
        first_day_sp_varname<- paste0("first_day_sp_", i)
        daily_averages_subset <- daily_averages[daily_averages$Winter == w, 1]
        assign(first_day_sp_varname, daily_averages_subset[sum(get(snow_cover_sequence_varname)$lengths[1:get(index_first_snow_varname)-1])+1]) # index of offset for snow period
        
        last_day_sp_varname<- paste0("last_day_sp_", i)
        assign(last_day_sp_varname, daily_averages_subset[sum(get(snow_cover_sequence_varname)$lengths[1:get(index_last_snow_varname)])]) # index of last day for snow period
        
        daily_colname <- paste0("snow_period_", i)
        daily_averages[daily_averages$Winter == w, daily_colname] <- ifelse(daily_averages[daily_averages$Winter == w, ]$Day >= get(first_day_sp_varname) & 
                                                                              daily_averages[daily_averages$Winter == w, ]$Day <= get(last_day_sp_varname),
                                                                            TRUE, FALSE)
        
        # relative amounts ----
        temp_colname <- paste0("rel. s d_", i)
        temp[temp$Winter == w, temp_colname] <- (sum(get(continuous_snow_cover_varname)$lenght)/max(get(times_snow_cover_varname)$EndDay))*100 # Rel. amount of snow days
        
        temp_colname <- paste0("rel. ns d_", i)
        temp[temp$Winter == w, temp_colname] <- (sum(get(fragmented_snow_cover_varname)$lenght)/max(get(times_snow_cover_varname)$EndDay))*100 # Rel. amount of no snow days
        
        temp_colname <- paste0("rel. u d_", i)
        temp[temp$Winter == w, temp_colname] <- (sum(get(unsuitable_snow_cover_varname)$lenght)/max(get(times_snow_cover_varname)$EndDay))*100 # Rel. amount of unsuitable days
        
        # Minimum temperature for each winter: ----
        temp_colname <- paste0("Min. temperature_", i)
        data_colname <- paste0("Temperature_", i)
        temp[temp$Winter == w, temp_colname] <- min(data[data$Winter == w, data_colname], na.rm = T)
      }
    }
  }
  return(list(output1 = temp, output2 = daily_averages))
}

# plot ----
tomst_snow_plot <- function(overview, daily_averages, coeff_2nd_axis = 100){
  colors_snow= c("TRUE" = "#87aeed", "FALSE" = "gray") # color scheme for indicator system: snow period
  colors_label_snow = c("TRUE" = "snow period", "FALSE" = "no snow period") # labels for indicator system: snow period
  colors_cond = c("no_snow" = "gold", "suitable_condition" = "green", "unsuitable_condition" = "red", "no_snow02" = "blue") # color scheme for indicator system: snow conditions
  colors_label_cond = c("no_snow" = "no snow", "suitable_condition" = "suitable condition", "unsuitable_condition" = "unsuitable condition", "no_snow02" = "no snow 02") # labels for indicator system: snow conditions
  cut_month_all <- aggregate(Day ~ Winter, daily_averages, min) # which dates the years are cut for geom_vline()
  min_temp <- min(overview[, which(grepl("Min. temperature_", colnames(overview)))])-1 # places the indicator system under the coldest temperature value
  dist_02 <- 2 # vertical placement in the plot to the first (T03) indicator segment in °C! 
  dist_01 <- 4 # vertical placement in the plot to the first (T03) indicator segment in °C! 
  
  titel_label <- c("Logger nr.", "Cover", "ID") # order is important
  titel_index <- na.omit(match(titel_label, names(overview)))
  title <- paste(sapply(titel_index, function(i) overview[1, i]), collapse = " ")
  from_year <- year(daily_averages$Day[1])
  to_year <- year(max(daily_averages$Day))
  title <- glue("{title} - {from_year} to {to_year}")
  
  g<-ggplot(daily_averages, aes(x = Day)) +
    geom_ribbon(aes(ymin = Min_moisture/coeff_2nd_axis, ymax = Max_moisture/coeff_2nd_axis), fill = "grey50") + # min/max ribbon around moisture graph
    geom_ribbon(aes(ymin = Min_temperature_03, ymax = Max_temperature_03), fill = "darkblue", alpha = 0.2) + # min/max ribbon around temp graph 03
    geom_ribbon(aes(ymin = Min_temperature_02, ymax = Max_temperature_02), fill = "blue", alpha = 0.2) + # min/max ribbon around temp graph 02
    geom_ribbon(aes(ymin = Min_temperature_01, ymax = Max_temperature_01), fill = "lightblue", alpha = 0.6) + # min/max ribbon around temp graph 01
    geom_vline(xintercept = cut_month_all$Day, linetype="dashed", color = "darkred", linewidth=1) + # Cut line of year
    geom_segment(aes(x = Day, xend = Day, y = min_temp-1, yend = min_temp-.5, color = snow_period_01), linewidth = 1.2) + # snow period indicator for temp 03
    geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_02-1, yend = min_temp-dist_02-.5, color = snow_period_02), linewidth = 1.2) + # snow period indicator for temp 02
    geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_01-1, yend = min_temp-dist_01-.5, color = snow_period_03), linewidth = 1.2) + # snow period indicator for temp 01
    scale_color_manual(values = colors_snow, labels = colors_label_snow, name = "Snow period:") + # color scheme for snow period indicator for temp 03
    ggnewscale::new_scale_color() + # adding new color scale to the plot
    geom_segment(aes(x = Day, xend = Day, y = min_temp-2, yend = min_temp-1, color = condition_01), linewidth = 1.2) + # snow condition indicator for temp 03
    geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_02-2, yend = min_temp-dist_02-1, color = condition_02), linewidth = 1.2) + # snow condition indicator for temp 02
    geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_01-2, yend = min_temp-dist_01-1, color = condition_03), linewidth = 1.2) + # snow condition indicator for temp 01
    scale_color_manual(values = colors_cond, labels = colors_label_cond, name = "Snow condition:") + # color scheme for snow condition indicator for temp 03
    geom_line(aes(y = Avg_temperature_03),linewidth = 0.7, alpha = 0.7, color = "darkblue", linetype="solid") + # graph of temp 03
    geom_line(aes(y = Avg_temperature_02),linewidth = 0.7, alpha = 0.7, color = "blue", linetype="solid") + # graph of temp 02
    geom_line(aes(y = Avg_temperature_01),linewidth = 0.7, alpha = 0.7, color = "lightblue", linetype="solid") + # graph of temp 01
    #geom_line(aes(color = condition_03), linewidth = 0.7, alpha = 0.7) + # snow conditions on the temp line
    geom_line(aes(x = Day, y = Avg_moisture/coeff_2nd_axis), linewidth = 0.7, alpha = 0.7, color = "black") + # Moisture line
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y/%m", guide = guide_axis(angle=45)) + # X-axis label
    scale_y_continuous("Temperature in °C", sec.axis = sec_axis(~.*coeff_2nd_axis, name = "Moisture in ?")) + # Y-axis titles
    ggtitle(title) + # Titel
    xlab("Time") + # X-axis titel
    #ylim(c(-15,40)) + # y limitations
    annotate("text", x = as.POSIXct(daily_averages$Day[1], format = "%Y-%m-%d")-days(14), y = c(min_temp-1.25, min_temp-dist_02-1.25, min_temp-dist_01-1.25), 
             label = c("T01:", "T02:", "T03:") , color="black", 
             size=4 , angle=0) +
    theme(legend.position = "bottom",
          legend.box = "horizontal", 
          axis.line.y.right = element_line(color = "black"), 
          axis.ticks.y.right = element_line(color = "black"),
          axis.text.y.right = element_text(color = "black"), 
          axis.title.y.right = element_text(color = "black")) # overall theme for the plot
  print(g)
}

tomst_snow <- function(directory, ID_directory = NULL, upper_temp = 2, 
                       lower_temp = -.5, fluct_temp = 3, 
                       fluct_para = "p2p", onset_days = 5, fahrenheit = FALSE, 
                       skip = 0, cut_month = 8, header = TRUE, sep_data = ";",
                       sep_ID = ",", cover = NULL, Nr_label = NULL, coeff_2nd_axis = 100, plot = FALSE) {
  out1 <- temp_prep(directory, ID_directory = ID_directory, sep_ID = sep_ID, cover = cover, Nr_label = Nr_label)
  data<-read_in_tomst(directory, sep_data = sep_data, header = header, skip = skip, fahrenheit = fahrenheit)
  res<-avg_dataframe(data = data, temp = out1, cover = cover, Nr_label = Nr_label, cut_month =  cut_month)
  out2 <- avg_fluct(res$data, res$output2, fluct_temp = fluct_temp, fluct_para = fluct_para)
  out2 <- avg_conditions(out2, upper_temp = upper_temp, lower_temp = lower_temp, fluct_temp = fluct_temp)
  out2 <- avg_minmax(res$data, out2)
  res2 <- snow_seq(data = res$data, temp = res$output1, daily_averages = out2, onset_days = onset_days)
  if (plot == TRUE){
    tomst_snow_plot(overview = res2$output1, daily_averages = res2$output2, coeff_2nd_axis = coeff_2nd_axis)
  }
  return(list(output1 = res2$output1, output2 = res2$output2))
}

# plot function to plot single plots for each winter season:

tomst_snow_plot_sep <- function(overview, daily_averages, selection = NULL, coeff_2nd_axis = 100){
  
  if (is.null(selection)){
    print("all winters will be plotted")
    winter_count = max(daily_averages$Winter_id)
  } else {
    print(selection)
    winter_count = selection
  }
  for (x in winter_count) {
    overview_sel <- overview[x,]
    daily_averages_sel <- daily_averages[daily_averages$Winter_id == x,]
    colors_snow= c("TRUE" = "#87aeed", "FALSE" = "gray") # color scheme for indicator system: snow period
    colors_label_snow = c("TRUE" = "snow period", "FALSE" = "no snow period") # labels for indicator system: snow period
    colors_cond = c("no_snow" = "gold", "suitable_condition" = "green", "unsuitable_condition" = "red", "no_snow02" = "blue") # color scheme for indicator system: snow conditions
    colors_label_cond = c("no_snow" = "no snow", "suitable_condition" = "suitable condition", "unsuitable_condition" = "unsuitable condition", "no_snow02" = "no snow 02") # labels for indicator system: snow conditions
    cut_month_all <- aggregate(Day ~ Winter, daily_averages_sel, min) # which dates the years are cut for geom_vline()
    min_temp <- min(overview_sel[, which(grepl("Min. temperature_", colnames(overview_sel)))])-1 # places the indicator system under the coldest temperature value
    dist_02 <- 2 # vertical placement in the plot to the first (T03) indicator segment in °C! 
    dist_01 <- 4 # vertical placement in the plot to the first (T03) indicator segment in °C! 
    
    titel_label <- c("Logger nr.", "Cover", "ID") # order is important
    titel_index <- na.omit(match(titel_label, names(overview_sel)))
    title <- paste(sapply(titel_index, function(i) overview_sel[1, i]), collapse = " ")
    from_year <- year(daily_averages_sel$Day[1])
    to_year <- year(max(daily_averages_sel$Day))
    title <- glue("{title} - {from_year} to {to_year}")
    
    g<-ggplot(daily_averages_sel, aes(x = Day)) +
      geom_ribbon(aes(ymin = Min_moisture/coeff_2nd_axis, ymax = Max_moisture/coeff_2nd_axis), fill = "grey50", alpha = 0.7) + # min/max ribbon around moisture graph
      geom_ribbon(aes(ymin = Min_temperature_03, ymax = Max_temperature_03), fill = "darkblue", alpha = 0.2) + # min/max ribbon around temp graph 03
      geom_ribbon(aes(ymin = Min_temperature_02, ymax = Max_temperature_02), fill = "blue", alpha = 0.2) + # min/max ribbon around temp graph 02
      geom_ribbon(aes(ymin = Min_temperature_01, ymax = Max_temperature_01), fill = "lightblue", alpha = 0.6) + # min/max ribbon around temp graph 01
      geom_vline(xintercept = cut_month_all$Day, linetype="dashed", color = "darkred", linewidth=1) + # Cut line of year
      geom_segment(aes(x = Day, xend = Day, y = min_temp-1, yend = min_temp-.5, color = snow_period_01), linewidth = 1.2) + # snow period indicator for temp 03
      geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_02-1, yend = min_temp-dist_02-.5, color = snow_period_02), linewidth = 1.2) + # snow period indicator for temp 02
      geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_01-1, yend = min_temp-dist_01-.5, color = snow_period_03), linewidth = 1.2) + # snow period indicator for temp 01
      scale_color_manual(values = colors_snow, labels = colors_label_snow, name = "Snow period:") + # color scheme for snow period indicator for temp 03
      ggnewscale::new_scale_color() + # adding new color scale to the plot
      geom_segment(aes(x = Day, xend = Day, y = min_temp-2, yend = min_temp-1, color = condition_01), linewidth = 1.2) + # snow condition indicator for temp 03
      geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_02-2, yend = min_temp-dist_02-1, color = condition_02), linewidth = 1.2) + # snow condition indicator for temp 02
      geom_segment(aes(x = Day, xend = Day, y = min_temp-dist_01-2, yend = min_temp-dist_01-1, color = condition_03), linewidth = 1.2) + # snow condition indicator for temp 01
      scale_color_manual(values = colors_cond, labels = colors_label_cond, name = "Snow condition:") + # color scheme for snow condition indicator for temp 03
      geom_line(aes(y = Avg_temperature_03),linewidth = 0.7, alpha = 0.7, color = "darkblue", linetype="solid") + # graph of temp 03
      geom_line(aes(y = Avg_temperature_02),linewidth = 0.7, alpha = 0.7, color = "blue", linetype="solid") + # graph of temp 02
      geom_line(aes(y = Avg_temperature_01),linewidth = 0.7, alpha = 0.7, color = "lightblue", linetype="solid") + # graph of temp 01
      #geom_line(aes(color = condition_03), linewidth = 0.7, alpha = 0.7) + # snow conditions on the temp line
      geom_line(aes(x = Day, y = Avg_moisture/coeff_2nd_axis), linewidth = 0.7, alpha = 0.7, color = "black") + # Moisture line
      scale_x_datetime(date_breaks = "1 month", date_labels = "%Y/%m", guide = guide_axis(angle=45)) + # X-axis label
      scale_y_continuous("Temperature in °C", sec.axis = sec_axis(~.*coeff_2nd_axis, name = "Moisture in ?")) + # Y-axis titles
      ggtitle(title) + # Titel
      xlab("Time") + # X-axis titel
      #ylim(c(-15,40)) + # y limitations
      annotate("text", x = as.POSIXct(daily_averages_sel$Day[1]- as.difftime(25, units="days"), format = "%Y-%m-%d"), y = c(min_temp-1.25, min_temp-dist_02-1.25, min_temp-dist_01-1.25), 
               label = c("T01:", "T02:", "T03:") , color="black", 
               size=4 , angle=0) +
      theme(legend.position = "bottom",
            legend.box = "horizontal", 
            axis.line.y.right = element_line(color = "black"), 
            axis.ticks.y.right = element_line(color = "black"),
            axis.text.y.right = element_text(color = "black"), 
            axis.title.y.right = element_text(color = "black")) # overall theme for the plot
    print(g)
  }
}
