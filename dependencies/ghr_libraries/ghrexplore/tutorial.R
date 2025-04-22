# load data ----
# Simulate the loading of IDE Package
devtools::load_all()
devtools::document()

# Load data
# dengue_MS is spatiotemporal data
data(dengue_MS)
# dengue_SP is timeseries data 
data(dengue_SP)
# dengue map
data("map_MS")

# Load map
ggplot() +
  geom_sf(data = map_MS)

# Create Adjacences Matrix
nb <- spdep::poly2nb(map_MS)
g <- spdep::nb2mat(nb, style = "B")
g

### make testing data sets ----
data.test <- dengue_MS %>% 
  filter(micro_code != "50001") %>%
  mutate(tmin = case_when(
    date == "2000-01-01" & micro_code == "50002" ~ NA, 
    T ~ tmin
  ))

data.test.1 <- dengue_MS %>% 
  mutate(tmin = case_when(
    date == "2000-01-01" & micro_code == "50002" ~ NA, 
    T ~ tmin
  ))

# Check palettes 
?GHR_colors
GHR_palettes()

# 0. helper functions ----

## 0.1 check_na ----
# Function to check for NAs in a specified column and return a warning if there are any

check_na("tmin",data.test.1)


# 1. plot_timeseries ----

## 1.1 plot_timeseries covariates ----

# Plot TS of temperature: all plots in the same graph
devtools::load_all()

dengue_MS %>%
  plot_timeseries(var = "tmin",
                  type = "cov",
                  time = "date",          
                  area = "micro_code",   
                  title = "Yearly Minimun Temperature") 

# Plot TS of temperature: log scale
dengue_MS %>%
  plot_timeseries(var = "tmin",           # Variable to be plotted 
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title = "Yearly Minimun Temperature",
                  log = TRUE) 

# Plot TS of temperature: plots in separate graphs
devtools::load_all()

dengue_MS %>%
  plot_timeseries(var = "tmin",
                  time = "date",
                  area = "micro_code",
                  panel = TRUE,           # The multiple plots 
                  var_label= "Minimum Temperature",
                  palette = "violetred")

# Plot TS of temperature: highlight single area
dengue_MS %>%
  plot_timeseries(var = "tmin",
                  time = "date",
                  area = "micro_code",
                  highlight = 50001,      # The Spatial unit to be highlighted
                  title="Monthly Minimun Temperature")

# Plot TS of PDSI: aggregate over space, all  plots in same graph
dengue_MS %>%
  plot_timeseries(var = "tmin",
                  time = "date",
                  area = "micro_code",
                  aggregate_space = "meso_code",  # The target for aggregation
                  aggregate_space_fun = "mean",   # Function for aggregation
                  aggregate_time = "year",
                  palette = "Colorblind")

# Plot TS of temperature: aggregate over space, plots in separate graph
dengue_MS %>%
  plot_timeseries(var = "pdsi",
                  time = "date",
                  area = "micro_code",
                  aggregate_space = "meso_code",
                  aggregate_space_fun = "mean",
                  panel = TRUE,
                  palette = "grey",
                  title = "Yearly Minimum Temperature")

# Plot TS of temperature: aggregate over space, plots in same graph
dengue_MS %>%
  plot_timeseries(var = "pdsi",
                  time = "date",
                  area = "micro_code",
                  aggregate_space = "meso_code",
                  aggregate_space_fun = "mean",
                  palette = "Blues")

# Plot TS of temperature: aggregate over space and time and highlight
dengue_MS %>%
  plot_timeseries(var = "pdsi",
                  time = "date",
                  area = "micro_code",
                  aggregate_space = "meso_code",
                  aggregate_space_fun = "mean",
                  aggregate_time = "year",
                  highlight = 5001)


## 1.2 plot_timeseries cases ----

# Plot TS of cases
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "counts",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Cases",
                  palette = "royalblue") 

# Plot TS of cases (log)
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "counts",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Cases",
                  log=TRUE) 

# Plot TS of cases: allowing different scale of the y_axis
dengue_MS %>%
  plot_timeseries(var = "dengue_cases",  
                  type = "counts",
                  time = "date",          
                  area = "micro_code",    
                  title= "Monthly Cases",
                  palette= "tomato",
                  free_y_scale = TRUE) 

# Plot TS of incidence
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",  
                  aggregate_time = "month", 
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Incidence")

# Plot TS of incidence: with panels by area 
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  panel = TRUE,           # The multiple plots 
                  palette = "#800080",
                  pt=1)

# Plot TS of incidence: with panels by area and different scales in the y axis
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Incidence",
                  palette = "Colorblind",
                  panel = FALSE, 
                  free_y_scale = TRUE)

# Plot TS of incidence: highlighting one area
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Incidence",
                  highlight = "50001")

# Plot TS of incidence: person-time = 1000
dengue_MS %>% 
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Incidence",
                  pt = 1000)

# Plot TS of cases: aggregate over space and time, all  plots in same graph
dengue_MS %>%
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "counts",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Counts",
                  aggregate_space = "meso_code",  # The target for aggregation
                  aggregate_time = "year")

# Plot TS of incidence: aggregate over space and time, all plots in same graph
dengue_MS %>%
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  title= "Monthly Incidence",
                  aggregate_space = "meso_code",  # The target for aggregation
                  aggregate_time = "year")

# Plot TS of incidence: aggregate over space, plots in separate graph
dengue_MS %>%
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  aggregate_space = "meso_code",
                  panel = TRUE,
                  title= "Monthly Incidence",
                  palette = "Blues")

# Plot TS of incidence: aggregate over space and time and highlight
dengue_MS %>%
  plot_timeseries(var = "dengue_cases", # Variable defining the cases 
                  type = "inc",
                  pop = "population",
                  time = "date",          # Variable defining the date "dd-mm-yyyy"
                  area = "micro_code",    # Variable defining the spatial unit
                  aggregate_space = "meso_code",
                  aggregate_time = "year",
                  highlight = 5001,
                  title= "Yearly Incidence")

# 2. plot_heatmap ----
devtools::load_all()

## 2.1 plot_heatmap covariates ----
dengue_MS %>%
  plot_heatmap(var = "tmin",           # Variable to be plotted 
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",    # Variable defining the spatial unit
               var_label = "Min Temp",  
               title= "Minimum Temperature",
               palette = "viridis")    # Palette from GHRpalette

# Median centering
dengue_MS %>%
  plot_heatmap(var = "tmin",           # Variable to be plotted 
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",    # Variable defining the spatial unit
               var_label = "Min Temp",  
               title= "Minimum Temperature",
               palette = "viridis",
               centering = "median") 

#Custom centering
dengue_MS %>%
  plot_heatmap(var = "tmin",           # Variable to be plotted 
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",    # Variable defining the spatial unit
               var_label = "Min Temp",  
               title= "Minimum Temperature",
               palette = "viridis",
               centering = 11) 

## plot_heatmap covariates: log TRUE 
dengue_MS %>%
  plot_heatmap(var = "tmin",           # Variable to be plotted 
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",    # Variable defining the spatial unit
               # palette = "BlYlRd", 
               var_label = "Min Temp",  
               title= "Minimum Temperature",
               palette = "cividis",
               log = TRUE)    

# Plot Heatmap of temperature: aggregate across space
dengue_MS %>%
  plot_heatmap(var = "tmin",
                   time = "date",
                   area = "micro_code",
                   aggregate_space = "meso_code",   # Aggregation target
                   aggregate_space_fun = "mean",    # Function for aggregation
                   palette = "Purp")

# Plot Heatmap of temperature: aggregate across space
dengue_MS %>%
  plot_heatmap(var = "tmin",
               time = "date",
               area = "micro_code",
               aggregate_space = "state_code",   
               aggregate_space_fun = "mean")

# Plot Heatmap of PDSI
dengue_MS %>%
  plot_heatmap(var = "pdsi",                      # New Variable 
               time = "date",
               area = "micro_code",
               palette = "Reds")             # Change palette 


## 2.2 plot_heatmap cases ----
devtools::load_all()

# Plot Heatmap of case counts (no log)
dengue_MS %>%
  plot_heatmap(var = "dengue_cases", # Variable defining the cases 
               type = "counts",
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",  
               palette = "BlYlRd", 
               title = "Dengue counts", 
               var_label = "Dengue \ncounts")    # Palette from GHRpalette

# Plot Heatmap of case counts median centering
dengue_MS %>%
  plot_heatmap(var = "dengue_cases", # Variable defining the cases 
               type = "counts",
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",  
               palette = "BlYlRd", 
               title = "Dengue counts", 
               var_label = "Dengue \ncounts",
               centering = "median")  

# Plot Heatmap of case counts custom centering
dengue_MS %>%
  plot_heatmap(var = "dengue_cases", # Variable defining the cases 
               type = "counts",
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",  
               palette = "BlYlRd", 
               title = "Dengue counts", 
               var_label = "Dengue \ncounts",
               centering = 5000)  

# Plot Heatmap of case counts (log)
dengue_MS %>%
  plot_heatmap(var = "dengue_cases", # Variable defining the cases 
               type = "counts",
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",  
               palette = "BlYlRd", 
               log = TRUE,
               title = "Dengue counts", 
               var_label = "Dengue \ncounts (log)")    # Palette from GHRpalette



# Plot Heatmap of incidence 
dengue_MS %>%
  plot_heatmap(var = "dengue_cases", # Variable defining the cases 
               type = "inc",
               pop = "population",
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",    # Variable defining the spatial unit
               palette = "BlYlRd")    # Palette from GHRpalette

# Plot Heatmap of incidence: aggregate across space
dengue_MS %>%
  plot_heatmap(var = "dengue_cases", # Variable defining the cases 
               type = "inc",
               pop = "population",
               time = "date",          # Variable defining the date "dd-mm-yyyy"
               area = "micro_code",    # Variable defining the spatial unit
               aggregate_space = "meso_code",   # Aggregation target
               palette = "Purp")


# 3. plot_seasonality ----
devtools::load_all()

## 3.1 plot_seasonality  covariates ----
dengue_MS %>%
  plot_seasonality(var = "tmin",                    # Variable to be plotted
                   time = "date",                   # Variable defining the date "dd-mm-yyyy"
                   area = "micro_code",
                   var_label = "Minimum Temperature", 
                   palette= "Reds")             # Variable defining the area

## plot_seasonality  covariates log TRUE
dengue_MS %>%
  plot_seasonality(var = "tmin",                    # Variable to be plotted
                   time = "date",                   # Variable defining the date "dd-mm-yyyy"
                   area = "micro_code",
                   var_label = "Minimum Temperature", 
                   palette= "Reds",
                   log = TRUE)    


# Plot Sesonality of temperature: aggregated across space 
dengue_MS %>%
  plot_seasonality(var = "tmin",
                       time = "date",
                       area = "micro_code",
                       aggregate_space = "meso_code")  # Target of the aggregation

## 3.2 plot_seasonality  cases ----

# Plot Seasonality of case counts from single timeseries
dengue_SP %>%
  plot_seasonality(var = "cases",
                   type = "counts",
                   time = "date",    # Variable defining the date "dd-mm-yyyy"
                   var_label = "Monthly Dengue Cases", 
                   xlab= "Month", 
                   ylab= "Number of cases")

# Plot Seasonality of case counts from single timeseries (log)
dengue_SP %>%
  plot_seasonality(var = "cases",
                   type = "counts",
                   time = "date",    # Variable defining the date "dd-mm-yyyy"
                   log = TRUE,
                   var_label = "Monthly Dengue Cases", 
                   xlab= "Month", 
                   ylab= "Number of cases")

# Plot Seasonality of case counts:  with different y axis scales per area
dengue_MS %>%
  plot_seasonality(var = "dengue_cases",
                   type = "counts",
                   time = "date",    # Variable defining the date "dd-mm-yyyy"
                   area = "micro_code", # Variable defining the spatial unit
                   title= "Monthly Dengue Cases", 
                   xlab= "Month", 
                   ylab= "Number of cases",
                   free_y_scale=TRUE)

# Plot Seasonality of incidence
dengue_MS %>%
  plot_seasonality(var = "dengue_cases",
                   type = "inc",
                   time = "date",    # Variable defining the date "dd-mm-yyyy"
                   pop = "population",
                   area = "micro_code", # Variable defining the spatial unit
                   pt = 1000, # specifying desired person time
                   title= "Monthly Dengue Incidence")

# Plot Seasonality of incidence: aggregated across space
dengue_MS %>%
  plot_seasonality(var = "dengue_cases",
                   type = "inc",
                   time = "date",    # Variable defining the date "dd-mm-yyyy"
                   pop = "population",
                   area = "micro_code", # Variable defining the spatial unit
                   aggregate_space  = "meso_code",
                   pt = 100000, # specifying desired person time
                   title= "Monthly Dengue Incidence")

# 4. plot_map ----
devtools::load_all()

## 4.1 plot_map covariates ----

# Plot covariate, average
plot_map(data = data.test, 
         var = "tmin",         # Variable to be plotted 
         time = "date",        # Variable defining the date "dd-mm-yyyy"
         area = "micro_code",  # Variable defining area in the dataframe
         map = map_MS,         # the sf object for the map 
         map_area = "code",    # Varibale defining the area in the sf object
         aggregate_time_fun = "mean",         
         palette ="viridis",
         by_year = FALSE,
         var_label= "Min Temp")

# Same but log=TRUE
plot_map(data = data.test, 
         var = "tmin",         # Variable to be plotted 
         time = "date",        # Variable defining the date "dd-mm-yyyy"
         area = "micro_code",  # Variable defining area in the dataframe
         map = map_MS,         # the sf object for the map 
         map_area = "code",    # Varibale defining the area in the sf object
         aggregate_time_fun = "mean",         
         palette ="viridis",
         var_label= "Min Temp", 
         by_year = FALSE,
         log=TRUE)

# Plot covariate, yearly, median
dengue_MS %>% 
  plot_map(var = "tmin",          
           time = "date",        
           area = "micro_code",  
           map = map_MS,          
           map_area = "code",    
           aggregate_time_fun = "median",         
           by_year = TRUE, 
           palette ="Greens",
           var_label = "Min Temp")      # One Map for each year

# Plot covariate, yearly, median, bin
dengue_MS %>% 
  plot_map(var = "tmin",          
           time = "date",        
           area = "micro_code",  
           map = map_MS,          
           map_area = "code",    
           aggregate_time_fun = "median",         
           by_year = TRUE, 
           palette ="Greens",
           var_label = "Min Temp",
           bin = seq(0, 1, 0.1))  

## 4.2 plot_map factor ----

# Plot covariate single map
plot_map(data = data.test, 
         var = "biome_name",         # Variable to be plotted 
         time = "date",        # Variable defining the date "dd-mm-yyyy"
         area = "micro_code",  # Variable defining area in the dataframe
         map = map_MS,         # the sf object for the map 
         map_area = "code",    # Varibale defining the area in the sf object
         palette ="viridis",
         by_year = FALSE,
         var_label= "Biome")

# Plot covariate yearly maps
plot_map(data = dengue_MS, 
         var = "meso_name",         # Variable to be plotted 
         time = "date",        # Variable defining the date "dd-mm-yyyy"
         area = "micro_code",  # Variable defining area in the dataframe
         map = map_MS,         # the sf object for the map 
         map_area = "code",    # Varibale defining the area in the sf object
         by_year = TRUE,
         var_label= "Meso")


## 4.3 plot_map cases ----
devtools::load_all()

# Temporal average cases
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "counts",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           by_year = FALSE, 
           palette = "viridis")

# Temporal average cases centering median
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "counts",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           by_year = FALSE, 
           palette = "viridis",
           centering = "median")

# Temporal average cases custom
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "counts",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           by_year = FALSE, 
           palette = "viridis",
           centering = 50000)

dengue_MS %>% 
  mutate(dengue_cases1000 = dengue_cases/1000) |> 
  plot_map(var = "dengue_cases1000",         # Variable to be plotted 
           type = "counts",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           by_year = FALSE, 
           palette = "viridis",
           bin = seq(0,1, 1/3))

# Temporal average cases - log
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "counts",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           centering = NULL,
           by_year = FALSE, 
           palette = "Purp",
           log = TRUE)

# Yearly mean cases
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "counts",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",
           palette = "Purp")

# Temporal average incidence pt 1000
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "inc",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           pt = 1000,
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           centering = NULL)

# Temporal average incidence pt 10
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "inc",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           pt = 10,
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           centering = NULL)

# Spatial average
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "inc",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           by_year = FALSE)

# Yearly median + titles + pt + bins
dengue_MS %>% 
  plot_map(var = "dengue_cases",         # Variable to be plotted 
           type = "inc",
           time = "date",        # Variable defining the date "dd-mm-yyyy"
           area = "micro_code",  # Variable defining area in the dataframe
           pop = "population",
           map = map_MS,         # the sf object for the map 
           map_area = "code",    # Variable defining the area in the sf object
           aggregate_time_fun = "median",
           by_year = TRUE,
           title = "Custom",
           pt = 1000,
           bin = seq(0,1,0.2))

# 5. plot_correlation ----
devtools::load_all()

# Plot
dengue_MS %>% 
  plot_correlation(var = c("dengue_cases","pop_density", 
                           "tmax", "tmin", "pdsi", "urban",
                           "water_network", "water_shortage"),  
                   method = "pearson",
                   var_label = c("dengue cases","pop. density", 
                                 "max temp", "min temp", "drought index", "urbanization",
                                 "water network", "water shortage"),
                   title = "Correlation matrix",
                   palette = "magma") 

# Correlation matrix
dengue_MS %>% 
  plot_correlation(var = c("dengue_cases","pop_density", 
                           "tmax", "tmin", "pdsi", "urban",
                           "water_network", "water_shortage"),  
                   method = "pearson",
                   var_label = c("dengue cases","pop. density", 
                                 "max temp", "min temp", "drought index", "urbanization",
                                 "water network", "water shortage"),
                   print=TRUE) 

# 6. plot_bivariate ----
devtools::load_all()

## 6.1 plot_bivariate covariates ----

# Scatter - No grouping
dengue_MS %>%
  plot_bivariate(var = c("pop_density", "tmin"), palette = "#d04a2d")

# Scatter - Grouping
dengue_MS %>%
  plot_bivariate(var = c("pop_density", "tmin"), area = "micro_code")

# Scatter - Panels
dengue_MS %>%
  plot_bivariate(var = c("pop_density", "tmin"), 
                 area = "micro_code", panel = TRUE, free_x_scale = TRUE, palette = "chocolate")

# Box - No grouping
dengue_MS %>%
  mutate(biome_code = as.factor(biome_code)) |> 
  plot_bivariate(var = c("pop_density", "biome_code"), palette = "#d04a2d")

# Box - Grouping
dengue_MS %>%
  mutate(biome_code = as.factor(biome_code)) |> 
  plot_bivariate(var = c("biome_code", "tmin"), area = "micro_code",
                 palette = "Reds")

# Box - Panels
dengue_MS %>%
  mutate(biome_code = as.character(biome_code)) |> 
  plot_bivariate(var = c("biome_code", "tmin"), 
                 area = "micro_code", panel = TRUE, free_x_scale = TRUE, palette = "chocolate")


## 6.1 plot_bivariate cases ----
dengue_MS %>% 
  plot_bivariate(var = c("pop_density","dengue_cases"), 
                 var_label = c("Pop. density", "DENV cases"))


# 7. plot_multiple ----
devtools::load_all()

## 7.1 plot_multiple timeseries with multiple palettes ----
plots <- plot_multiple(
  plot_function = plot_timeseries,
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
  type = c("counts", "inc", "cov", "cov", "cov"),
  pop = "population",
  pt=100,
  var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp", "PDSI"),
  palette = c("blue", "red", "darkgreen", "purple", "orange"),
  time = "date",
  area = "micro_code",
  panel = TRUE)

print(plots[[1]])  
print(plots[[2]])  
print(plots[[3]])  
print(plots[[4]])  
print(plots[[5]])  

## log
plots <- plot_multiple(
  plot_function = plot_timeseries,
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "tmax", "tmin"),
  type = c("counts", "inc", "cov", "cov"),
  pop = "population",
  pt=100,
  var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp"),
  palette = c("Blues", "Reds", "BrBG", "viridis"),
  time = "date",
  area = "micro_code",
  log = TRUE,
  panel = TRUE)

print(plots[[1]])  
print(plots[[2]])  
print(plots[[3]])  
print(plots[[4]])  

## 7.2 plot_multiple heatmap ----
plots <- plot_multiple(
  plot_function = plot_heatmap,
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
  type = c("counts", "inc", "cov", "cov", "cov"),
  pop = "population",
  pt=1000,
  var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp", "PDSI"),
  palette = c("Blues", "Reds", "BrBG", "viridis", "Purp"),
  time = "date",
  area = "micro_code")

print(plots[[1]])  
print(plots[[2]])  
print(plots[[3]])  
print(plots[[4]])  

##  7.3 plot_multiple seasonality ----
plots <- plot_multiple(
  plot_function = plot_seasonality,
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
  type = c("counts", "inc", "cov", "cov", "cov"),
  pop = "population",
  pt = 1000,
  var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp", "PDSI"),
  palette =  c("Blues", "Reds", "BrBG", "viridis", "Purp"),
  time = "date",
  area = "micro_code")

print(plots[[1]])  
print(plots[[2]])  
print(plots[[3]])  
print(plots[[4]])  
print(plots[[5]])          

##  7.4 plot_multiple map ----
devtools::load_all()

plots <- plot_multiple(
  plot_function = plot_map, 
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
  type = c("inc", "counts", "cov", "cov", "cov"),
  pop = "population",
  pt = 100,
  var_label = c("Dengue Cases", "Dengue Counts", "Max Temp", "Min Temp", "PDSI"),
  palette = c("Reds", "Blues", "viridis", "cividis", "RdYlBu"),
  map = map_MS,         # the sf object for the map 
  map_area = "code",    # Variable defining the area in the sf object
  time = "date",
  area = "micro_code")

# Access individual plots
print(plots[[1]])  
print(plots[[2]])  
print(plots[[3]])  
print(plots[[4]])  
print(plots[[5]])  

# 8. plot_combine ----
devtools::load_all()
devtools::document()

plot_combine(plot_list = plots, 
             ncol = 1,
             align = "v",
             combine_legend = TRUE,
             combine_xaxis = TRUE,
             rel_widths_l = c(7,1),
)

 # 9. plot_compare ----
 
 ## 9.1 plot_compare timeseries ----
devtools::load_all()

plot_compare(
  plot_function = plot_timeseries,
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "pdsi"),
  type = c("inc", "counts", "cov"),
  pop = "population",
  time = "date",
  area = "micro_code",
  pt = 100,
  var_label = c("Dengue inc", "Dengue counts", "PDSI"), 
  palette = "Qualitative",
  combine_legend=TRUE, 
  ncol_legend = 1,
  ncol=1, 
  align="h")

# log
plot_compare(
  plot_function = plot_timeseries,
  data = dengue_MS,
  var = c("dengue_cases", "dengue_cases", "pdsi"),
  type = c("inc", "counts", "cov"),
  pop = "population",
  time = "date",
  area = "micro_code",
  pt = 100,
  var_label = c("Dengue inc", "Dengue counts", "PDSI"), 
  palette = "Qualitative",
  combine_legend=TRUE, 
  ncol_legend = 1,
  ncol=1, 
  align="h")


 ## 9.2 plot_compare seasonality ----

devtools::load_all()

 plot_compare(
   plot_function = plot_seasonality,
   data = dengue_MS,
   var = c("dengue_cases", "dengue_cases", "pdsi"),
   type = c("counts", "inc", "cov"),
   pop = "population",
   time = "date",
   area = "micro_code",
   pt = 100,
   var_label = c("Dengue Cases", "Dengue inc", "Min Temp"), 
   ncol_legend = 1,
   combine_legend=TRUE)
 
 ## 9.3 plot_compare heatmap ----
 devtools::load_all()
 plot_compare(
   plot_function = plot_heatmap,
   data = dengue_MS,
   var = c("dengue_cases", "pdsi"),
   type = c("inc", "cov"),   
   pop = "population",
   time = "date",
   area = "micro_code",
   var_label = c("Dengue Cases", "Min Temp"), 
   palette = c("Reds", "Blues"),
   ncol_legend = 1,
   combine_xaxis =TRUE)
 
 ## 9.4 plot_compare map ----
 devtools::load_all()
 plot_compare(
   plot_function = plot_map,
   data = dengue_MS,
   var = c("dengue_cases", "tmax"),
   type = c("inc", "cov"),  
   pop = "population",
   time = "date",
   area = "micro_code",
   var_label= c("Dengue Incidence", "Max Temperature"), 
   palette = c("Reds", "Blues"),
   map = map_MS,         # the sf object for the map 
   map_area = "code", 
   by_year = FALSE,
   ncol_legend = 1,
   combine_xaxis =TRUE)
