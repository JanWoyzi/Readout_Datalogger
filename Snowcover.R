# package requirements: ggplot2, lubridate, ggnewscale

library(ggplot2)

hobo_snow <- function(directory, upper_temp = 2, lower_temp = -.5, fluct_temp = 3, 
                      fluct_para = "p2p", onset_days = 5, plot = FALSE){
  '
  The function takes several parameters and processes HOBO logger data in a 
  specific format. It calculates various statistics related to snow cover 
  conditions, such as the duration of snow cover, the number of snow days, the 
  longest continuous snow period, and more. It can also create a plot to 
  visualize the temperature and snow cover data.
  
  Parameters:
        directory:
            (string of character)
            Path to file
        upper_temp:
            (numeric, default = 2)
            Select all temperatue values that are lower than this value. For 
            determining days with potential snow cover condition.
        lower_temp:
            (numeric, default = -.5)
            Select all temperature values that are above this value
        fluct_temp:
            (numeric, default = 3)
            limits temperature variance to this value. Everthing above will be 
            excluded from the condition for snow cover.
        onset_days:
            (numeric, default = 5)
            Defines the amount of continues snow days that is used to set the 
            starting point for the snow period.
        plot:
            (logical, default = FALSE)
            Activates (T) or deactivates (F) the creation of a plot.
  
  Read in single .csv-files in the HOBO formating style. Output is a list with 
  two dataframes:
    output1: sinlge-row-dataframe with following categories:
  
        Logger nr.
            Internal number used for this particular logger, 1 digit number
        ID
            Identifier number placed on the device itself ("hardcoded"), 
            factory internal 8 digit number
        Cover
            Information if the logger was placed covered by trees or shrubs 
            (close)or openly without any obstructions above it (open).
        Time
            The year of collecting the device.
            >>>> For winter calculation, it is the transition into this year!<<<<
        Days of snow season
            Time period of snow. Snow days are defined by temperature under 
            "upper_temp", above "lower_temp" and variance within this day is 
            lower than 3. Onset and offset are snow days. Onset is defined by 
            the number of contiues snow day (onset_days).
            Offset is the last day of snow.
        Total days of snow
            Total sum of all days that got defined as snow days within the snow 
            season.
        Total days of no snow
            Total sum of all days that are not defined as snow days within the snow 
            season.
        Longest continues days under snow
            Longest number of days that are continuesly defined as snow days and are not
            fragmented by no snow days.
        Nr. of snow fragments
            Within the snow period days of snow and no snow days can appear, 
            creating fragments within the snow period. Here only the number of 
            snow fragments is presented. 
        Nr. of no snow fragments
            Only the number of fragments of no snow days are counted within the 
            snow period. 
        Rel. amount of snow days
            Relative representation of snow days within the snow period.
        Rel. amount of no snow days
            Relative representation of no snow days within the snow period.
  
    output2: 
        the read-out of the HOBO device average to "day" increments.
  
  '
  # internal functions: ----
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
  # template data frame: ----
  temp <- data.frame(matrix(ncol = 1, nrow = 1))
  colnames(temp) <- c("Logger nr.")
  # ID ----
  ID_data <- read.csv2(directory, sep = ";", header = F)[1,1] # read only the first 4 columns in, containing relavant environmental information
  ID_label <- stringr::str_extract(ID_data, "(\\d+)")
  temp$ID <- ID_label
  # Logger Nr.: ----
  t1<-tail(strsplit(directory, "/")[[1]], n=1)
  t2<-strsplit(t1, ".csv")[[1]]
  nr <- strsplit(t2, "_")[[1]][2]
  temp$`Logger nr.` <- nr
  # Cover: ----
  if (strsplit(t2, "_")[[1]][3] == "op") {
    cover <- "open"
    temp$Cover <- cover
  } else {
    cover <- "close"
    temp$Cover <- cover
  }
  # data prep:----
  data <- read.csv2(directory, sep = ",", header = T, skip = 1)[ , 1:4] # read only the first 4 columns in, containing relavant environmental information
  data <- data[1:(nrow(data) - 4),] # delete last four rows, they contain no sensor data # lazy solution! sometimes only the last 3 rows sometimes no rows
  #* check for Fahrenheit: ----
  fahrenheit <- 0 # converting steps needs to be done later, but determining that a conversation is need has to be done now!
  if (grepl("F",colnames(data)[3])){ # in the Temperature column is somewhere the Letter F or C used to indicate Fahrenheit or Celsius
    fahrenheit <- 1
  }
  colnames(data) <- c("Index", "Time", "Temperature", "Rel. Humidity") # rename the column names !!! ID of HOBO unity gets lost!!!
  #* date format decision: ----
  # if (grepl("(\\d{2}:\\d{2}:\\d{2} \\D{2}$)", data$Time[1])) { # to extract only the date part without time, but different formats exists
  #   sample_C<- purrr::list_simplify(strsplit(data$Time, "( \\d{2}:\\d{2}:\\d{2} \\D{2}$)"))[1:3000] # in a sample of 1300 ticks, the amount of changes in each position (Year, Month, day) is used to determine what time scale which position is 
  # } else {
  #   sample_C<- purrr::list_simplify(strsplit(data$Time, "( \\d{2}:\\d{2}$)"))[1:3000]
  # }
  
  # take a sample of 3000 elements, but remove the hour:minute:seconnd pm/am information
  sample_C<-purrr::list_simplify(strsplit(data$Time, "( \\d{2}:\\d{2}:\\d{2} \\D{2}$)"))[1:3000]
  sample_C<-purrr::list_simplify(strsplit(sample_C, "( \\d{2}:\\d{2}$)"))
  
  # separate the date elements from each other (year, month, days) in unknown order
  if (grepl("\\.", sample_C[1])){ # some formats uses dots instead of "/"
    sample_C<-purrr::transpose(strsplit(sample_C, "\\."))
  } else {
    sample_C<-purrr::transpose(strsplit(sample_C, "/"))
  }
  # check if one of the three list contains any! 4-digit elements, if yes define it with a small number to designated it late as year
  if (any(grepl("\\d{4}", unique(sample_C)))){
    p_index<-which(grepl("\\d{4}", unique(sample_C))) # find position of 4-digit str
    p<-c(1, 2, 3) # to keep the order of elements
    p<-p[-p_index] # remove the postion where 4-digit were found
    date_df<-data.frame("position" = c(1,2,3)) # creat dataframe for defining dataum format
    # create a length column to count how many changes (unique elements) happend for those 3000 entries
    date_df$length[date_df$position == p_index] <- 1 # on the position with the 4-digit str place a low value, the change or number of unique elemnts for years should be lower than for month or days
    for (i in p){ # go through the remaining entries
      # fill in the count of unique elements
      date_df$length[date_df$position == i] <-length(unique(purrr::list_simplify(sample_C[[i]])))
    }
  } else {
    date_df<-data.frame("length" = c(length(unique(purrr::list_simplify(sample_C[[1]]))), # splitting the 3 positions up but keep them together in a dataframe
                                     length(unique(purrr::list_simplify(sample_C[[2]]))),
                                     length(unique(purrr::list_simplify(sample_C[[3]])))),
                        "position" = c(1,2,3))
  }
  
  date_df<-date_df[order(date_df$length), ] # reorder according to number of changes during chosen time period
  rownames(date_df) <- NULL # reindex
  date_df$datum[1] <- "y" # lowest amount of changes (max. 2)
  date_df$datum[2] <- "m" #  should be between, max. amount of changes = 12
  date_df$datum[3] <- "d" # most amount of changes (max. 31)
  date_format<-paste(date_df[order(date_df$position), 3], collapse = "") # put the datum info together according to their original position
  data <- dplyr::rowwise(data)
  data <- dplyr::mutate(data, Clock = nchar(strsplit(Time, " ")[[1]][2]))
  clcok_chg_index<-which(diff(data$Clock) !=0)
  test_vect<-c(0,clcok_chg_index, nrow(data))
  data_tmp<-data.frame()
  for (i in seq(length(test_vect)-1)) {
    test_df01<-data[(test_vect[i]+1):(test_vect[i+1]),]
    if (test_df01$Clock[1] == 5){
      switch(date_format, 
             "ymd" = test_df01$Time <- lubridate::ymd_hm(test_df01$Time),
             "ydm" = test_df01$Time <- lubridate::ydm_hm(test_df01$Time),
             "mdy" = test_df01$Time <- lubridate::mdy_hm(test_df01$Time),
             "dmy" = test_df01$Time <- lubridate::dmy_hm(test_df01$Time))
    }else {
      switch (date_format,
              "ymd" = test_df01$Time <- lubridate::ymd_hms(test_df01$Time),
              "ydm" = test_df01$Time <- lubridate::ydm_hms(test_df01$Time),
              "mdy" = test_df01$Time <- lubridate::mdy_hms(test_df01$Time),
              "dmy" = test_df01$Time <- lubridate::dmy_hms(test_df01$Time))
    }
    data_tmp<-rbind(data_tmp, test_df01)
  }
  data_tmp<-data_tmp[order(data_tmp$Index),]
  data<-data_tmp
  data$Temperature<- as.numeric(data$Temperature)
  if (fahrenheit == 1){ # check if conversion to °C is necessary, see above
    data<-dplyr::mutate(data, Temperature = (Temperature - 32) * (5/9))
  }
  data$`Rel. Humidity`<- as.numeric(data$`Rel. Humidity`)
  # Time ----
  temp$Time <- format(tail(data$Time, n=1), "%Y") # last entry is used to determine the year
  # daily average ----
  data$day <- format(data$Time, "%Y-%m-%d") # for the daily average
  daily_averages <- aggregate(list(data$Temperature, data$`Rel. Humidity`), by = list(data$day), FUN = mean, na.rm = TRUE)
  colnames(daily_averages) <- c("Day", "Avg_temperature", "Avg_humidity")
  daily_averages$Day <- as.POSIXct(daily_averages$Day, format = "%Y-%m-%d")
  daily_averages$ID <- ID_label
  daily_averages$`Logger nr.` <- nr
  daily_averages$Cover <- cover
  daily_averages$Nr_cover <- paste(nr, cover, sep = " ") # lazy solution for later plotting
  daily_averages$Time <- format(tail(data$Time, n=1), "%Y")
  # > Parameters----
  
  if (fluct_para[1] == "all"){
    fluct_para <- c("sd", "var", "p2x", "p2p", "sd2x", "sd2z")
  }
  if (length(fluct_temp)!=length(fluct_para)){
    stop("The amount of parameters are not equal to the amount of values given. ")
  } else {
    for (i in seq(fluct_para)){
      switch(fluct_para[i],
             "sd" = { daily_sd <- aggregate(list(data$Temperature), by = list(data$day), FUN = sd, na.rm = TRUE)
             colnames(daily_sd) <- c("Day", "Fluct_temperature") # SD temperature
             daily_sd$Day<- as.POSIXct(daily_sd$Day, format = "%Y-%m-%d")
             daily_averages<-merge(daily_averages, daily_sd, by = "Day")},
             "var" = {daily_var <- aggregate(list(data$Temperature), by = list(data$day), FUN = var, na.rm = TRUE)
             colnames(daily_var) <- c("Day", "Fluct_temperature") # Variance temperature
             daily_var$Day<- as.POSIXct(daily_var$Day, format = "%Y-%m-%d")
             daily_averages<-merge(daily_averages, daily_var, by = "Day")},
             "p2x" = {daily_max_diff_mean <- aggregate(list(data$Temperature), by = list(data$day), FUN = max_difference_from_mean)
             colnames(daily_max_diff_mean) <- c("Day", "Fluct_temperature") # Max difference to avg °C
             daily_max_diff_mean$Day<- as.POSIXct(daily_max_diff_mean$Day, format = "%Y-%m-%d")
             daily_averages<-merge(daily_averages, daily_max_diff_mean, by = "Day")},
             "p2p" = {daily_p2p <- aggregate(list(data$Temperature), by = list(data$day), FUN = difference_between_max_and_min_differences)
             colnames(daily_p2p) <- c("Day", "Fluct_temperature") # (max)peak to (min)peak
             daily_p2p$Day<- as.POSIXct(daily_p2p$Day, format = "%Y-%m-%d")
             daily_averages<-merge(daily_averages, daily_p2p, by = "Day")},
             "sd2x" = {daily_diff_mean <- aggregate(list(data$Temperature), by = list(data$day), FUN = sum_of_differences_to_mean)
             colnames(daily_diff_mean) <- c("Day", "Fluct_temperature") # Sum of difference to avg °C
             daily_diff_mean$Day<- as.POSIXct(daily_diff_mean$Day, format = "%Y-%m-%d")
             daily_averages<-merge(daily_averages, daily_diff_mean, by = "Day")},
             "sd2z" = {daily_diff_0 <- aggregate(list(data$Temperature), by = list(data$day), FUN = sum_of_differences_to_zero)
             colnames(daily_diff_0) <- c("Day", "Fluct_temperature") # Sum of difference to 0°C
             daily_diff_0$Day<- as.POSIXct(daily_diff_0$Day, format = "%Y-%m-%d")
             daily_averages<-merge(daily_averages, daily_diff_0, by = "Day")})
    }
  }
  
  daily_averages<-dplyr::mutate(daily_averages, 
                                no_snow = Avg_temperature > upper_temp,
                                suitable_condition = Avg_temperature < upper_temp & 
                                  Avg_temperature > lower_temp &
                                  Fluct_temperature < fluct_temp, 
                                unsuitable_condition = Avg_temperature < lower_temp,
                                no_snow_02 = Avg_temperature < upper_temp & # "no_snow_02" will be declared later to "no_snow"
                                  Avg_temperature > lower_temp &
                                  Fluct_temperature > fluct_temp,
                                condition = dplyr::case_when(
                                  no_snow ~ "no_snow",
                                  suitable_condition ~ "suitable_condition",
                                  unsuitable_condition ~ "unsuitable_condition",
                                  no_snow_02 ~ "no_snow", # no_snow_02 cases are turned to no_snow
                                  TRUE ~ "none")
  )
  # Max and min temp per day: ----
  daily_max <- aggregate(list(data$Temperature), by = list(data$day), FUN = max, na.rm = TRUE)
  daily_min <- aggregate(list(data$Temperature), by = list(data$day), FUN = min, na.rm = TRUE)
  colnames(daily_max) <- c("Day", "Max_temperature")
  colnames(daily_min) <- c("Day", "Min_temperature")
  daily_max$Day<- as.POSIXct(daily_max$Day, format = "%Y-%m-%d")
  daily_min$Day<- as.POSIXct(daily_min$Day, format = "%Y-%m-%d")
  daily_averages<-merge(daily_averages, daily_max, by = "Day")
  daily_averages<-merge(daily_averages, daily_min, by = "Day")
  # nr. of fragments ----
  snow_cover_sequence <- rle(daily_averages$condition) # Create a sequence of days with snow cover
  
  index_first_snow <- which(snow_cover_sequence$values == "suitable_condition" & snow_cover_sequence$lengths >= onset_days)[1] # the first snow cover that sustain more or equal than onset_days days
  index_last_snow <- tail(which(snow_cover_sequence$values == "suitable_condition"), 1)
  filtered_snow_cover_sequence <- list(
    lengths = snow_cover_sequence[[1]][index_first_snow:index_last_snow],
    values = snow_cover_sequence[[2]][index_first_snow:index_last_snow]
  )
  # Identify continuous snow cover periods
  times_snow_cover <- with(filtered_snow_cover_sequence, {
    data.frame(
      SnowCover = values,
      StartDay = cumsum(lengths) - lengths + 1,
      EndDay = cumsum(lengths)
    )
  })
  
  # Filter only continuous snow cover periods
  continuous_snow_cover <- times_snow_cover[times_snow_cover$SnowCover == "suitable_condition", ]
  fragmented_snow_cover <- times_snow_cover[!times_snow_cover$SnowCover == "suitable_condition", ] 
  unsuitable_snow_cover <- times_snow_cover[times_snow_cover$SnowCover == "unsuitable_condition", ] # unsuitable days (< -0.5°C) are not disjunctive to !suitable days
  
  temp$`Nr. of snow fragments` <- nrow(continuous_snow_cover) # fragments covered snow days within the snow cover period
  temp$`Nr. of no snow fragments` <- nrow(fragmented_snow_cover) # fragments not covered snow days within the snow cover period
  temp$`Nr. of unsuitable fragments` <- nrow(unsuitable_snow_cover) # fragments not unsuitable days within the snow cover period
  
  # longest period under snow ----
  continuous_snow_cover <- dplyr::mutate(continuous_snow_cover, lenght = (EndDay - StartDay)+1)
  temp$`Longest continues days under snow` <- max(continuous_snow_cover$lenght)
  
  # total length under snow and not ----
  temp$`Total days of snow` <-  sum(continuous_snow_cover$lenght)
  
  fragmented_snow_cover <- dplyr::mutate(fragmented_snow_cover, lenght = (EndDay - StartDay)+1)
  temp$`Total days of no snow` <- sum(fragmented_snow_cover$lenght)
  
  unsuitable_snow_cover <- dplyr::mutate(unsuitable_snow_cover, lenght = (EndDay - StartDay)+1)
  temp$`Total days of unsuitable days` <- sum(unsuitable_snow_cover$lenght)
  
  # length of snow cover season ----
  temp$`Days of snow season` <- max(times_snow_cover$EndDay)
  
  # snow period begin and end day: 
  first_day_sp<-daily_averages[sum(snow_cover_sequence$lengths[1:index_first_snow-1])+1,1] # index of offset for snow period
  last_day_sp<-daily_averages[sum(snow_cover_sequence$lengths[1:index_last_snow]),1] # index of last day for snow period
  
  daily_averages<-dplyr::mutate(daily_averages, snow_period = Day <= last_day_sp & Day >= first_day_sp) # extra column for entire snow period, mainly for plot reasons
  
  # relative amounts ----
  temp$`Rel. amount of snow days` <- (sum(continuous_snow_cover$lenght)/max(times_snow_cover$EndDay))*100
  temp$`Rel. amount of no snow days` <- (sum(fragmented_snow_cover$lenght)/max(times_snow_cover$EndDay))*100
  temp$`Rel. amount of unsuitable days` <- (sum(unsuitable_snow_cover$lenght)/max(times_snow_cover$EndDay))*100
  
  # Minimum Temperature over entire dataset:
  temp$`Min. temperature` <- min(data$Temperature, na.rm = T)
  
  # plot ----
  if (plot == TRUE){
    #colors = c("FALSE" = "NA", "TRUE" = "red")
    colors_snow= c("TRUE" = "#87aeed", "FALSE" = "NA")
    colors_label_snow = c("TRUE" = "snow period", "FALSE" = "no snow period")
    colors_cond = c("no_snow" = "gold", "suitable_condition" = "green", "unsuitable_condition" = "red", "no_snow02" = "blue")
    colors_label_cond = c("no_snow" = "no snow", "suitable_condition" = "suitable condition", "unsuitable_condition" = "unsuitable condition", "no_snow02" = "no snow 02")
    
    g<-ggplot(daily_averages, aes(x = Day, y = Avg_temperature)) +
      #geom_segment(aes(x = Day, xend = Day, y = -1, yend = -1.2, color = above_98rH), size = 1.2) +
      #geom_segment(aes(x = Day, xend = Day, y = Avg_temperature, yend = Avg_temperature+0.2, color = condition), linewidth = 1.2) +
      geom_ribbon(aes(ymin = Min_temperature, ymax = Max_temperature), fill = "grey70") +
      geom_segment(aes(x = Day, xend = Day, y = -15.5, yend = -13.5, color = snow_period), linewidth = 1.2) +
      scale_color_manual(values = colors_snow, labels = colors_label_snow, name = "Snow period:") +
      ggnewscale::new_scale_color() +
      geom_segment(aes(x = Day, xend = Day, y = -15, yend = -14, color = condition), linewidth = 1.2) +
      scale_color_manual(values = colors_cond, labels = colors_label_cond, name = "Snow condition:") +
      geom_line(linewidth = 0.7, alpha = 0.7) +
      #geom_line(aes(color = condition), linewidth = 0.7, alpha = 0.7) +
      geom_line(aes(x = Day, y = Avg_humidity), linewidth = 0.7, alpha = 0.7, color = "blue") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%Y/%m", guide = guide_axis(angle=45)) +
      ggtitle(paste(nr, cover, ID_label, sep = " ")) +
      xlab("Time") +
      ylab("Temperature in °C") + 
      #ylim(c(-15,40)) +
      #scale_color_manual(values = colors, labels = colors_lab, name = "Snow condition:") +
      theme(legend.position = "bottom")
    print(g)
  }
  return(list(output1 = temp, output2 = daily_averages))
}

