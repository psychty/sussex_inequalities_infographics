# Hexagonal plots

# Load some packages ####
# These are the packages used in this script
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'jsonlite', 'viridis', 'nomisr', 'ggstar', 'fingertipsR', 'readODS')

# This command installs only those packages (from our object packages) which are not already installed.
install.packages(setdiff(packages, rownames(installed.packages())))

# This loads the packages
easypackages::libraries(packages)

# Task - looks like we're making seven Hex plots (2 x building blocks, 1 x starting, 2 x living, 1 x ageing, 1 x dying well), with about 9 indicators in each, for each ICT (there are 16 but we'll only be able to make 13 without manually cutting the B&H data)

# Thats 91 plots (or 112 if we get B&H ICTs rather than the whole UA).

# We're already making 16 lifecourse infographics.

# Thankfully we can automate the hex plots in R, it just requires a bit of looping and filtering and getting the data in the right place.

# Steps ####
# Get data for all indicators for all areas (about 800 rows)
# We will loop through each of the ICT areas, and plots, getting the data we need for each one, and exporting an image. 

# ICT areas 
# This version has the four ICTs in B&H but we cannot get fingertips data for these
# ICT_areas <- c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove West', 'Brighton and Hove South', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')

# A version including Brighton and Hove UA
ICT_areas <- c('Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')

# Specify some colours - if we do this here, we can tweak the colours just once in the object which is referenced throughout the script (rather than finding and replacing every instance of #fadfdf.
hex_colours_high <- '#fadfdf'
hex_colours_low <- '#cde8c5'
hex_colours_similar <- '#fff2ae'
hex_colours_na <- '#e5f4fb'

# We need a set of styling for our hex figures we can store in a theme rather than using the same code over and over
theme_hex = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 16), # Here in bold
    plot.subtitle = element_text(colour = "#000000", size = 13),
    plot.caption = element_text(colour = "#000000", size = 13),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(colour = "#000000", size = 13, face = 'bold'),
    strip.background = element_blank(),
    legend.title = element_text(colour = "#000000", size = 10, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.text = element_text(colour = "#000000", size = 10),
    legend.position = 'top',
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    )
}

# Specify where our outputs should go
output_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Outputs'

# Specify where any data comes from 
data_directory <-  '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data'

# The hex figures are drawn as a scatter plot in ggplot with x and y coordinates
# We match the hex_id from the indicators list - this has the x and y positions of each icon/hex/blockbuster
# We need a grid that covers 7 across by 4 rows
hex_positions <- data.frame(x = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,1,2,3,4,5,6,7,1.5,2.5,3.5,4.5,5.5,6.5,7.5,1,2,3,4,5,6,7),
                            y = c(rep(1.5,7), rep(2, 7), rep(2.5,7), rep(3,7)),
                            Hex_ID = seq(1,28))

# If you can sync the sharepoint folder to a local onedrive folder, you can access it that way.
# This is a list of indicators for the hex/blockbuster figures, the Hex_ID is the position from bottom left to right upwards
# e.g.  7,8,9
#       4,5,6
#       1,2,3

# on the biggest grid...

#  22,23,24,25,26,27,28
#  15,16,17,18,19,20,21
#  8,9,10,11,12,13,14
#  1,2,3,4,5,6,7

# It also has some of the label text we need for the figure
hex_indicators <- read_csv('C:/Users/rtbp8900/West Sussex County Council/Sussex-wide PH Intelligence - Integrated Care Team ICT profiles/hex_indicators.csv') %>% 
  left_join(hex_positions, by = 'Hex_ID')

# Data extract ####

# Building blocks 1 ####
hex_indicators %>% 
  filter(Figure_ID == 'Building blocks 1')

Working_age_out_of_work_ohid <- fingertips_data(IndicatorID = 93097,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Year = Timeperiod, Numerator = Count, Denominator, Rate = Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_england = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Working_age_out_of_work') # we add this ID so we can join the data with the hex indicators

Fuel_poverty_ohid <- fingertips_data(IndicatorID = 93759,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Year = Timeperiod, Numerator = Count, Denominator, Rate = Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_england = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Fuel_poverty') # we add this ID so we can join the data with the hex indicators

# Housing_affordability_ratio

# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoresidencebasedearningslowerquartileandmedian/current/ratioofhousepricetoresidencebasedearnings.xlsx

# Deprivation 




# Building blocks 2



# Ageing well #

# Indicator 1 - 3
# These are from fingertips

Dementia_diagnosis_rate_ohid <- fingertips_data(IndicatorID = 92949,
                              AreaTypeID = 'All',
                              categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Year = Timeperiod, Numerator = Count, Denominator, Rate = Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_england = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Dementia_diagnosis_rate') # we add this ID so we can join the data with the hex indicators

Falls_admissions_rate_persons_ohid <- fingertips_data(IndicatorID = 22401,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>% 
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Year = Timeperiod, Numerator = Count, Denominator, Rate = Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_england = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% 
  mutate(Indicator_short_name = 'Falls_admissions_rate_persons')

Pensionable_age_winter_fuel_payments_ohid <- fingertips_data(IndicatorID = 93945,
                                                      AreaTypeID = 'All',
                                                      categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>% 
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Year = Timeperiod, Numerator = Count, Denominator, Rate = Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_england = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% 
  mutate(Indicator_short_name = 'Pensionable_age_winter_fuel_payments')

# This is only at UTLA- as such we need to rebuild it from the sources of numerators and denominators

# Numerator - this checks if the file is in the data directory and if it is not, then it will download the file and save to the data directory
if(file.exists(paste0(data_directory, '/Winter_fuel_payments_202122.ods')) != TRUE){
  download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1114952/tables-winter-fuel-payments-2021-to-2022-experimental.ods',
                paste0(data_directory, '/Winter_fuel_payments_202122.ods'),
                mode = 'wb')
}

wfp_numerator <- read_ods(paste0(data_directory, '/Winter_fuel_payments_202122.ods'),
         sheet = '2_Local_Authority',
         skip = 6) %>% 
  filter(`Local Authority` %in% ICT_areas)


# Indicator 4 - 6, 9

# Indicator  7 and 8

ascof_df <- read_csv('https://files.digital.nhs.uk/A4/766F29/meas-from-asc-of-eng-2022-csv-v3.csv')
unique(ascof_df$`Measure Group Description`)

i = 1

ICT_x <- ICT_areas[i]

df <- data.frame(x = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,1,2,3,4,5,6,7,1.5,2.5,3.5,4.5,5.5,6.5,7.5,1,2,3,4,5,6,7),
           y = c(rep(1.5,7), rep(2, 7), rep(2.5,7), rep(3,7)),
           Hex_ID = seq(1,28),
           value = seq(1,28, 1),
           indicator_line_1 = c('Per 1,000\nhouseholds\nexperiencing\nhomelessness\n', 'Older people\nliving in\npoverty\n\n','Killed or seriously injured\non roads per\nbillion vehicle miles\n\n', 'Air pollution:\nfine particulate matter\nconcentrations of total\nPM2.5\n', 'Violent offences\nper 1,000 population\n\n\n', 'Working age\nin employment\n\n\n','Children live\nin poverty\n\n\n','Good level of\ndevelopment at\nage 4-5\n\n', 'Pupil absence\n\n\n\n','Per 1,000\nhouseholds\nexperiencing\nhomelessness\n', 'Older people\nliving in\npoverty\n\n','Killed or seriously injured\non roads per\nbillion vehicle miles\n\n', 'Air pollution:\nfine particulate matter\nconcentrations of total\nPM2.5\n', 'Violent offences\nper 1,000 population\n\n\n', 'Working age\nin employment\n\n\n','Children live\nin poverty\n\n\n','Good level of\ndevelopment at\nage 4-5\n\n', 'Pupil absence\n\n\n\n','Per 1,000\nhouseholds\nexperiencing\nhomelessness\n', 'Older people\nliving in\npoverty\n\n','Killed or seriously injured\non roads per\nbillion vehicle miles\n\n', 'Air pollution:\nfine particulate matter\nconcentrations of total\nPM2.5\n', 'Violent offences\nper 1,000 population\n\n\n', 'Working age\nin employment\n\n\n','Children live\nin poverty\n\n\n','Good level of\ndevelopment at\nage 4-5\n\n', 'Pupil absence\n\n\n\n','text'),
           compared_england = c('high', 'low', 'low','high', 'same','low','high', 'not_applicable', 'high','high', 'low', 'low','high', 'same','low','high', 'not_applicable', 'high','high', 'low', 'low','high', 'same','low','high', 'not_applicable', 'high','low'),
           ICT = ICT_x)


plot_1 <- ggplot(df,
       aes(x = x,
           y = y)) +
  geom_star(starshape = 6,
            size = 70,
            aes(fill = compared_england),
            colour = NA) +
  scale_x_continuous(limits = c(0.5,7.5)) +
  scale_y_continuous(limits = c(1,3.5)) +
  scale_fill_manual(values = c('high' = hex_colours_high, 'low' = hex_colours_low, 'same' = hex_colours_similar, 'not_applicable' = hex_colours_na)) +
  geom_text(aes(x - .4,
                y + .1,
                label = value),
            hjust = 0,
            vjust = 0,
            fontface = 'bold',
            size = 7) +
  labs(title = 'Ageing Well',
       subtitle = ICT_x) +
  geom_text(aes(x - .4,
                y - .2,
                label = indicator_line_1),
            hjust = 0,
            vjust = 0) +
  # coord_equal()+
  theme_hex() +
  theme(legend.position = 'none')

# each figure will have its own set height and width

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(output_directory, '/test_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
    width = 22,
    height = 12)  
print(plot_1)
dev.off()


# a three by three grid can use 9 x 9.3 unit 
svg(filename = paste0(output_directory, '/test_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
    width = 9,
    height = 9.3)  
print(plot_1)
dev.off()
