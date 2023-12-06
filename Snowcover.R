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

# function call: ----

list_2016<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2016/", pattern = "\\d{8}_\\d_\\D{2}.csv", full.names = T)
list_2017<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2017/", pattern = "\\d{8}_\\d_\\D{2}.csv", full.names = T)
list_2018<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2018/", pattern = "\\d{7}_\\d_\\D{2}.csv", full.names = T)
list_2019<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2019/", pattern = "\\d{7}_\\d_\\D{2}.csv", full.names = T)
list_2020<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2020/", pattern = "\\d{7}_\\d_\\D{2}.csv", full.names = T)
list_2021<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2021/", pattern = "\\d{7}_\\d_\\D{2}.csv", full.names = T)
list_2022<-list.files("J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2022/", pattern = "\\d{7}_\\d_\\D{2}.csv", full.names = T)


list_all <- c(list_2016, list_2017, list_2018, list_2019, list_2020, list_2021, list_2022)

output_01<-data.frame()
output_02<-data.frame()
for (list in list_all) {
  for (logger in list) {
    temp <- hobo_snow(logger, upper_temp = 2, fluct_temp = 3, plot = F)
    output_01<-rbind(output_01, temp$output1)
    output_02<-rbind(output_02, temp$output2)
  }
}

t<-output_01[order(output_01$`Logger nr.`),-2]
output_02[output_02$no_snow02 == T,]
test<-output_02[output_02$ID == 10394753 & output_02$Nr_cover == "5 close",]


file_test<-"J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2017/10394753_5_cl.csv"
test<-hobo_snow(file_test, plot = T)


test_out1<-test$output1
test_out2<-test$output2

output_01

# Table: ----

write.table(output_01[order(output_01$`Logger nr.`),-2], "J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Figures_raw/Datalogger_HOBO/Snow_cover_HOBOb.csv", 
           row.names=FALSE,
           sep = ",")

# Figures: ----

file_part <- "J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Figures_raw/Datalogger_HOBO/Winter_"
#colors = c("FALSE" = "NA", "TRUE" = "red")
colors_snow= c("TRUE" = "#87aeed", "FALSE" = "NA")
colors_label_snow = c("TRUE" = "snow period", "FALSE" = "no snow period")
colors_cond = c("no_snow" = "gold", "suitable_condition" = "green", "unsuitable_condition" = "red", "no_snow02" = "blue")
colors_label_cond = c("no_snow" = "no snow", "suitable_condition" = "suitable condition", "unsuitable_condition" = "unsuitable condition", "no_snow02" = "no snow 02")
years <- c(2016, 2017, 2018, 2019, 2020, 2021)
years <- c(2022)

save <- T
for (year in years) {
  p<-ggplot2::ggplot(output_02[output_02$Time == year,], aes(x = Day, y = Avg_temperature)) +
    geom_ribbon(aes(ymin = Min_temperature, ymax = Max_temperature), fill = "grey70") +
    geom_segment(aes(x = Day, xend = Day, y = -15, yend = -13, color = snow_period), linewidth = 1.2) +
    scale_color_manual(values = colors_snow, labels = colors_label_snow, name = "Snow period:") +
    ggnewscale::new_scale_color() +
    geom_segment(aes(x = Day, xend = Day, y = -15, yend = -14, color = condition), linewidth = 1.2) +
    scale_color_manual(values = colors_cond, labels = colors_label_cond, name = "Snow condition:") +
    geom_line(linewidth = 0.7, alpha = 0.7) +
  #ggeom_line(aes(x = Day, y = Avg_humidity), linewidth = 0.7, alpha = 0.7, color = "blue") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b", guide = guide_axis(angle=45), 
                   limits = c(as.POSIXct(paste(year-1, "-10-01", sep = ""), tz = 'UTC'), 
                              as.POSIXct(paste(year, "-7-01", sep = ""), tz = 'UTC'))) +
    ggtitle(paste("Winter",year-1, "to", year, sep = " " )) +
    ylim(c(-15,40)) +
    facet_wrap(~Nr_cover , scales="free" ) +
    xlab("Time") +
    ylab("Temperature in °C") + 
    theme_light() +
    theme(legend.position = "bottom",
        legend.direction = "horizontal")
  if (save == T) {
    ggsave(
      filename = paste(file_part, year-1, "-", year, ".tiff", sep = ""), width = 12, 
      height = 8.5, dpi = 150, device = "tiff", 
      p, bg = "white")
  } else {
    print(p)
  }
}

# test area ----

ggplot(output_01)+
  geom_point(aes(x=`Logger nr.` , y=`Longest continues days under snow`, color = Cover))

ggplot(output_01)+
  geom_bar(aes(x=`Logger nr.`, y = `Longest continues days under snow`, fill = Time, color = Cover), stat = "identity", position=position_dodge()) +
  scale_fill_brewer(palette="Blues")

# Pre-build: ----


directory = "J:/Work/Garmisch-Paper/R-Proj/GAP_paper2023/Datalogger/HOBO/2017/10394754_5_op.csv"
upper_temp = 2
lower_temp = -.5 
fluct_temp = 3
fluct_para = "p2p"
onset_days = 5
plot = TRUE


# template data frame: ----
temp <- data.frame(matrix(ncol = 1, nrow = 1))
colnames(temp) <- c("Logger nr.") # 1 digit number
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
data <- data[1:(nrow(data) - 3),] # delete last three rows, they contain no sensor data
#* check for Fahrenheit: ----
fahrenheit <- 0 # converting steps needs to be done later, but determining that a conversation is need has to be done now!
if (grepl("F",colnames(data)[3])){ # in the Temperature column is somewhere the Letter F or C used to indicate Fahrenheit or Celsius
  fahrenheit <- 1
}
colnames(data) <- c("Index", "Time", "Temperature", "Rel. Humidity") # rename the column names !!! ID of HOBO unity gets lost!!!
#* date format decision tree: ----
clock<-nchar(strsplit(data$Time, " ")[[1]][2]) # number of characters of time can tell if seconds is used or not
if (grepl("(\\d{2}:\\d{2}:\\d{2} \\D{2}$)", data$Time[1])) { # to extract only the date part without time, but different formats exists
  sample_C<- purrr::list_simplify(strsplit(data$Time, "( \\d{2}:\\d{2}:\\d{2} \\D{2}$)"))[1:3000] # in a sample of 1300 ticks, the amount of changes in each position (Year, Month, day) is used to determine what time scale which position is 
} else {
  sample_C<- purrr::list_simplify(strsplit(data$Time, "( \\d{2}:\\d{2}$)"))[1:3000]
}
if (grepl("\\.", sample_C[1])){ # some formats uses dots instead of "/"
  sample_C<-purrr::transpose(strsplit(sample_C, "\\."))
} else {
  sample_C<-purrr::transpose(strsplit(sample_C, "/"))
}
date_df<-data.frame("length" = c(length(unique(purrr::list_simplify(sample_C[[1]]))), # splitting the 3 positions up but keep them together in a dataframe
                                 length(unique(purrr::list_simplify(sample_C[[2]]))),
                                 length(unique(purrr::list_simplify(sample_C[[3]])))),
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
        "dmy_hm" = data$Time <- lubridate::dmy_hm(data$Time)
)
data$Temperature<- as.numeric(data$Temperature)
if (fahrenheit == 1){ # check if concersation to °C is necessary, see above
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
#daily_averages<-dplyr::mutate(daily_averages, 
#                              upper_value = Avg_temperature < upper_temp,
#                              lower_value = Avg_temperature > lower_temp,
#                              above_98rH = Avg_humidity > 98)
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

# daily_averages<-dplyr::mutate(daily_averages, 
#                               no_snow = upper_value == T & 
#                                 fluct_temperature < fluct_temp,
#                               suitable_condition = upper_value == T & 
#                                 fluct_temperature < fluct_temp & 
#                                 Avg_temperature > lower_temp, 
#                               unsuitable_condition = )

daily_averages<-dplyr::mutate(daily_averages, 
                              no_snow = Avg_temperature > upper_temp,
                              suitable_condition = Avg_temperature < upper_temp & 
                                Avg_temperature > lower_temp &
                                Fluct_temperature < fluct_temp, 
                              unsuitable_condition = Avg_temperature < lower_temp,
                              no_snow_02 = Avg_temperature < upper_temp & 
                                Avg_temperature > lower_temp &
                                Fluct_temperature > fluct_temp,
                              condition = dplyr::case_when(
                                no_snow ~ "no_snow",
                                suitable_condition ~ "suitable_condition",
                                unsuitable_condition ~ "unsuitable_condition",
                                no_snow_02 ~ "no_snow",
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

index_first_snow <- which(snow_cover_sequence$values == "suitable_condition" & snow_cover_sequence$lengths > 5)[1] # the first snow cover that sustain more than onset_days days
index_last_snow <- tail(which(snow_cover_sequence$values == "suitable_condition"), 1)
filtered_snow_cover_sequence <- list(
  lengths = snow_cover_sequence[[1]][index_first_snow:index_last_snow],
  values = snow_cover_sequence[[2]][index_first_snow:index_last_snow]
)
# snow period begin and end day: ----
first_day_sp<-daily_averages[sum(snow_cover_sequence$lengths[1:index_first_snow-1])+1,1] # index of offset for snow period
last_day_sp<-daily_averages[sum(snow_cover_sequence$lengths[1:index_last_snow]),1] # index of last day for snow period

daily_averages<-dplyr::mutate(daily_averages, snow_period = Day <= last_day_sp & Day >= first_day_sp) # extra column for entire snow period, mainly for plot reasons

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

# relative amounts ----
temp$`Rel. amount of snow days` <- (sum(continuous_snow_cover$lenght)/max(times_snow_cover$EndDay))*100
temp$`Rel. amount of no snow days` <- (sum(fragmented_snow_cover$lenght)/max(times_snow_cover$EndDay))*100
temp$`Rel. amount of unsuitable days` <- (sum(unsuitable_snow_cover$lenght)/max(times_snow_cover$EndDay))*100

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
    #geom_segment(aes(x = first_day_sp, xend = first_day_sp, y = -5.5, yend = -4, color = "grey20"), linewidth = 1.2) +
    #geom_segment(aes(x = last_day_sp, xend = last_day_sp, y = -5.5, yend = -4, color = "grey20"), linewidth = 1.2) +
    geom_ribbon(aes(ymin = Min_temperature, ymax = Max_temperature), fill = "grey70") +
    #geom_segment(aes(x = first_day_sp, xend = last_day_sp, y = -4.75, yend = -4.75, color = "grey20"), linewidth = 2.5) +
    geom_segment(aes(x = Day, xend = Day, y = -5.25, yend = -4.25, color = snow_period), linewidth = 1.2) +
    scale_color_manual(values = colors_snow, labels = colors_label_snow, name = "Snow period:") +
    ggnewscale::new_scale_color() +
    geom_segment(aes(x = Day, xend = Day, y = -5, yend = -4.5, color = condition), linewidth = 1.2) +
    scale_color_manual(values = colors_cond, labels = colors_label_cond, name = "Snow condition:") +
    geom_line(linewidth = 0.7, alpha = 0.7) +
    #geom_line(aes(color = condition), linewidth = 0.7, alpha = 0.7) +
    geom_line(aes(x = Day, y = Avg_humidity), linewidth = 0.7, alpha = 0.7, color = "blue") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y/%m", guide = guide_axis(angle=45)) +
    ggtitle(paste(nr, cover, ID_label, sep = " ")) +
    xlab("Time") +
    ylab("Temperature in °C") + 
    theme(legend.position = "bottom")
  print(g)
}

p<-ggplot(data, aes(x = Time, y = Temperature )) +
  geom_line(linewidth = 0.7, alpha = 0.7) +
  #geom_line(aes(x = Time, y = `Rel. Humidity`), linewidth = 0.7, alpha = 0.7, color = "blue") +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M", guide = guide_axis(angle=45), 
                   limits = c(as.POSIXct("2017-01-01 00:00:00", tz = 'UTZ'), 
                              as.POSIXct("2017-01-02 00:00:00", tz = 'UTZ'))) +
  #ylim(c(0,5)) +
  xlab("Time") +
  ylab("Temperature in °C")
print(p)

daily_averages[daily_averages$Day == "2017-01-01",] # unsuitable days
daily_averages[daily_averages$Day == "2017-06-17",] # no snow
daily_averages[daily_averages$Day == "2017-04-17",] # snow

ggplot(data, aes(x=Temperature, color = day)) + # snow
  geom_histogram(binwidth = 0.5) +
  xlim(c(-5,30))+
  theme(legend.position = "none")

ggplot(data[data$day == "2017-01-01",], aes(x=Temperature)) + # unsuitable days
  geom_histogram(binwidth = 0.5) +
  xlim(c(-5,30))

ggplot(data[data$day == "2017-06-17",], aes(x=Temperature)) + # no snow
  geom_histogram(binwidth = 0.5) +
  xlim(c(-5,30))

ggplot(data[data$day == "2017-04-17",], aes(x=Temperature)) + # snow
  geom_histogram(binwidth = 0.5) +
  xlim(c(-5,30))

ggplot(daily_averages, aes(x=Difference_to_avg, y=Avg_temperature, color = condition)) + # standard
  geom_point()

ggplot(daily_averages, aes(x= Difference_to_0, y=Avg_temperature, color = condition)) + # interesting
  geom_point()

ggplot(daily_averages, aes(x= fluct_temperature, y=Avg_temperature, color = condition)) + # 
  geom_point()

ggplot(daily_averages, aes(x= Difference_to_avg, y=Difference_to_0, color = condition)) +
  geom_point()

ggplot(daily_averages, aes(x=Difference_to_avg, y=fluct_temperature, color = condition)) + # nope
  geom_point()

ggplot(daily_averages, aes(x=Peak_to_peak, y=Avg_temperature, color = condition)) + # 
  geom_point()

ggplot(daily_averages, aes(x=Difference_to_0, y=Peak_to_peak, color = condition)) + # 
  geom_point()

ggplot(daily_averages, aes(x=Day, y=fluct_temperature, color = condition)) + # 
  geom_point()

ggplot(daily_averages, aes(x=Day, y=Difference_to_0, color = condition)) + # 
  geom_point()

ggplot(daily_averages, aes(x=Day, y=Peak_to_peak, color = condition)) + # nope
  geom_point()
# third analysis: ----
sum(filtered_snow_cover_sequence$lengths)/3

floor(23/3)
ceiling(23/3)


grouped_by_day <- split(daily_averages, daily_averages$Day)
days_under_snow <- sapply(grouped_by_day, function(day_data) {
  if (any(day_data$snow_cover == T)) {
    return(1)
  } else {
    return(0)
  }
})

days_under_snow <- sum(days_under_snow)

grouped_by_day <- split(data, data$day)
days_upper_temp <- sapply(grouped_by_day, function(day_data) {
  if (any(day_data$Temperature < 0)) {
    return(1)
  } else {
    return(0)
  }
})

days_below_zero <- sum(days_below_zero)


grouped_by_hour <- split(data, data$hour)
hours_below_zero <- sapply(grouped_by_hour, function(hour_data) {
  if (any(hour_data$Temperature < 0)) {
    return(1)
  } else {
    return(0)
  }
})
hours_below_zero <- sum(hours_below_zero)


colors = c("FALSE" = "NA", "TRUE" = "red")


ggplot(data, aes(x = Time, y = Temperature)) +
  geom_segment(aes(x = Time, xend = Time, y = 0, yend = 0.2, color = below_zero), size = 1.2) +
  geom_segment(aes(x = Time, xend = Time, y = 3, yend = 2.8, color = upper_temp), size = 1.2) +
  geom_line(size = 0.7, alpha = 0.7) +
  geom_line(aes(x = Time, y = `Rel. Humidity`), size = 0.7, alpha = 0.7, color = "blue") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y/%m") +
  xlab("Time") +
  ylab("Temperature in °C") + 
  scale_color_manual(values = colors, name = "Temperature") +
  theme(legend.position = "bottom")
