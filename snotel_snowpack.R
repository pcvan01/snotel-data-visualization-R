#----------------------------------------------------------------------
# This file creates visualizations of SNOTEL site data
# PCV 7/2022
#----------------------------------------------------------------------
# Load Packages
library('snotelr') #load SNOTEL data
library('tidyverse') #data organization and manipulation
library('reshape2') #for melt() command to reshape data
library('scales') #to help with ggplot date axis
#
# Select SNOTEL Site - 
# example site ID 842 Vail Mountain, 10,300 ft
# example site ID 590 Lone Mountain (Big Sky) 8,800 ft
#
site_id <- c(590)
#
# Load, clean, and organize monthly data
# Provide a variety of date formats for possible future manipulation and plotting
raw_snotel_data <- as.data.frame(snotel_download(site_id, internal = TRUE)) %>%
                   mutate(snow_water_equivalent = snow_water_equivalent*0.0393701) #convert mm to inches 
cal_year <- as.numeric(substr(raw_snotel_data$date,1,4))
month_day <- paste(substr(raw_snotel_data$date,6,7),substr(raw_snotel_data$date,9,10),sep="-")
month <- as.numeric(substr(raw_snotel_data$date,6,7))
day <-  as.numeric(substr(raw_snotel_data$date,9,10))
water_year <- month
plot_date <- month #this will help with plotting going forward
snotel_data <- add_column(raw_snotel_data, water_year, .before = "date")
snotel_data <- add_column(snotel_data, cal_year, .before = "date")
snotel_data <- add_column(snotel_data, plot_date, .before = "date")
snotel_data <- add_column(snotel_data, month_day, .before = "date")
snotel_data <- add_column(snotel_data, month, .before = "date")
snotel_data <- add_column(snotel_data, day, .before = "date")
snotel_data <- snotel_data %>%
                 mutate(water_year = replace(water_year, water_year < 10, 0)) %>%
                 mutate(water_year = replace(water_year, water_year >= 10, 1)) %>%
                 mutate(water_year = water_year + cal_year) %>%
                 mutate(plot_date = replace(plot_date, plot_date < 10, 1)) %>%
                 mutate(plot_date = replace(plot_date, plot_date >= 10, 0)) %>%
                 mutate(plot_date = plot_date + 1969) %>%
                 mutate(plot_date = as.Date(paste(plot_date,month_day,sep="-"),format = "%Y-%m-%d"))
snotel_data <- snotel_data %>%
                 drop_na(snow_water_equivalent) %>% #remove NA values in SWE
                 filter( month_day != "02-29") #remove leap years - this is ok for this application when considering SWE
julian <- as.Date(paste(rep("1970",length(snotel_data$month_day)),snotel_data$month_day,sep="-"),format = "%Y-%m-%d")
julian <- format(julian, "%j")
julian <- as.numeric(julian)
snotel_data <- add_column(snotel_data, julian, .before = "date")
#
#
# Organize annual (water year) data for peak SWE statisics and remove missing years
snotel_data_year <- snotel_data %>%
                    group_by(water_year) %>%
                    summarise(peak_snow_water_equivalent = max(snow_water_equivalent),swe_count = n())
current_year <- as.numeric(substr(Sys.Date(),1,4))
current_month <- as.numeric(substr(Sys.Date(),6,7))
if(current_month >= 10){current_year <- current_year + 1}
years_to_keep <- snotel_data_year %>%
                    filter(water_year != current_year,swe_count >= 265) %>%
                    select(water_year)
n_recent_years <- tail(years_to_keep,n=4)
#
# Remove years with missing data
snotel_data_year <- snotel_data_year %>%
                    filter(water_year %in% years_to_keep$water_year | water_year %in% current_year)
snotel_data <- snotel_data %>%
                    filter(water_year %in% years_to_keep$water_year | water_year %in% current_year)
# Generate annual (water year) statistics
q <- c(.25,.5,.75)
snotel_data_quantiles <- snotel_data %>%
                          select(plot_date, julian, month, day, snow_water_equivalent) %>%
                          group_by(plot_date) %>%
                          summarise(julian=mean(julian),month=mean(month),day=mean(day),percentile_SWE25 = quantile(snow_water_equivalent, probs = q[1]), percentile_SWE50 = quantile(snow_water_equivalent, probs = q[2]), percentile_SWE75 = quantile(snow_water_equivalent, probs = q[3]))
#
# Organize all data of interest - this tidy format can be helpful in gg plot
frame_for_plotting1 <- snotel_data %>%
                          select(plot_date,water_year,julian, month, day, snow_water_equivalent)
frame_for_plotting2 <- melt(snotel_data_quantiles, id.vars=c('plot_date','julian','month','day'))
frame_for_plotting2 <- frame_for_plotting2 %>%
                          relocate(variable, .before = julian)
names(frame_for_plotting2)[2] <- c('water_year')
names(frame_for_plotting2)[6] <- c('snow_water_equivalent')
final_plot_data <- rbind(frame_for_plotting1,frame_for_plotting2)
#
#
# For plot 1
current_year_label <- as.numeric(substr(Sys.Date(),1,4))
if(current_month >= 10){
  current_year_label = paste(current_year_label,"-",current_year_label+1," Season",sep='')
} else{
  current_year_label = paste(current_year_label-1,"-",current_year_label," Season",sep='')}
current_swe <- subset(final_plot_data, water_year == c(current_year))
plot1_data <- subset(final_plot_data, 
                     water_year == c(current_year) | 
                     water_year == c("percentile_SWE50") | 
                     water_year == c("percentile_SWE25") |
                     water_year %in% n_recent_years$water_year)
plot1_data25 <- subset(final_plot_data, 
                       water_year == c("percentile_SWE25"))
plot1_data75 <- subset(final_plot_data, 
                       water_year == c("percentile_SWE75"))
#
baseplot <- ggplot() +
             geom_line(data = plot1_data,
                       aes(x=plot_date,y=snow_water_equivalent,color=water_year, linetype = water_year, size = water_year)) +
              scale_colour_manual(breaks = c(current_year,
                                             'percentile_SWE50',
                                             'percentile_SWE25',
                                             n_recent_years$water_year[4],
                                             n_recent_years$water_year[3],
                                             n_recent_years$water_year[2],
                                             n_recent_years$water_year[1]),
                                  values = c('blue',
                                             'black',
                                             'gray',
                                             'firebrick1',
                                             'darkgoldenrod1',
                                             'deeppink1',
                                             'chartreuse1'),
                                  labels = c(current_year_label,
                                             'Median',
                                             '25th/75th Percentile',
                                             n_recent_years$water_year[4],
                                             n_recent_years$water_year[3],
                                             n_recent_years$water_year[2],
                                             n_recent_years$water_year[1])) + 
              scale_linetype_manual(breaks = c(current_year,
                                               'percentile_SWE50',
                                               'percentile_SWE25',
                                               n_recent_years$water_year[4],
                                               n_recent_years$water_year[3],
                                               n_recent_years$water_year[2],
                                               n_recent_years$water_year[1]),
                                    values = c(1,11,1,1,1,1,1),
                                    labels = c(current_year_label,
                                               'Median',
                                               '25th/75th Percentile',
                                               n_recent_years$water_year[4],
                                               n_recent_years$water_year[3],
                                               n_recent_years$water_year[2],
                                               n_recent_years$water_year[1])) + 
              scale_size_manual(breaks = c(current_year,
                                           'percentile_SWE50',
                                           'percentile_SWE25',
                                           n_recent_years$water_year[4],
                                           n_recent_years$water_year[3],
                                           n_recent_years$water_year[2],
                                           n_recent_years$water_year[1]),
                                values = c(1.2,0.7,0.7,0.7,0.7,0.7,0.7),
                                labels = c(current_year_label,
                                           'Median',
                                           '25th/75th Percentile',
                                           n_recent_years$water_year[4],
                                           n_recent_years$water_year[3],
                                           n_recent_years$water_year[2],
                                           n_recent_years$water_year[1])) + 
              geom_line(data = subset(final_plot_data, water_year == c("percentile_SWE75")),
                        aes(x=plot_date,y=snow_water_equivalent),color= c("gray"), linetype = c(1), size = c(0.7)) +
              ggtitle(toupper(paste("Depth of Snow as Snow Water Equivalent\n",raw_snotel_data$network[1],"site:",raw_snotel_data$site_name[1],sep=" "))) +
              ylab('inches') +
              xlab('') +
              scale_x_date(breaks = date_breaks("months"), labels = date_format("%b"),limits = c(as.Date("1969-10-01"), as.Date("1970-06-30")), expand = c(0, 0)) + #expect error message from removed values
              scale_y_continuous(breaks = scales::breaks_width(2), expand = c(0, 0)) +
              theme_classic()  +
              theme(plot.title = element_text(hjust = 0.5,size=10),
                    legend.title = element_blank()) + 
              geom_ribbon(aes(x=plot1_data25$plot_date,ymin=plot1_data25$snow_water_equivalent,ymax=plot1_data75$snow_water_equivalent),fill = c("gray"),alpha=0.3)
baseplot
