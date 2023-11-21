






```{r}
# Function to calculate the individual-specific average for the two previous years
individual_avg <- function(year, id, data) {
      # Filter for the individual and for the two preceding years
      prev_years <- sort(setdiff((year - 3):(year - 1), c(2016)), decreasing = TRUE)
      individual_data <- data[data$nomem_encr == id & data$year %in% prev_years, ]
      
      # If both years are NA, return NA
      if (all(is.na(individual_data$rosenberg))) {
            return(NA)
      }
      
      # Calculate the mean, excluding NA values
      mean(individual_data$rosenberg, na.rm = TRUE)
}

# Apply the function to fill NA values for specific years
for (year in c(2010, 2012, 2015, 2018)) {
      # Identify rows for the specific year with NA in Rosenberg
      rows_to_update <- which(self_esteem$year == year & is.na(self_esteem$rosenberg))
      
      # Calculate and fill in the average for each individual
      for (row in rows_to_update) {
            id <- self_esteem$nomem_encr[row]
            self_esteem$rosenberg[row] <- individual_avg(year, id, self_esteem)
      }
}

# View the updated dataframe
View(self_esteem)

#check na's 
aggregate(is.na(rosenberg) ~ year, data = self_esteem, FUN = sum)
```


##1.3. Inpute average for missing 2016 survey
For 2016 we input 2015, which was calculated above as the individuals specific average over 2014 and 2013.
We only do this for people are already participating in the survey prior to 2016. 


```{r}
# Identify individuals with observations between 2008 and 2015
individuals <- unique(self_esteem$nomem_encr[self_esteem$year %in% 2008:2015])

# Create a new dataframe for 2016 observations
self_esteem_2016 <- data.frame()

# Loop through each individual and impute the 2015 value for 2016
for (id in individuals) {
      # Find the 2015 Rosenberg score for the individual
      rosenberg_2015 <- self_esteem$rosenberg[self_esteem$nomem_encr == id & self_esteem$year == 2015]
      
      # If there is a 2015 score, create a new observation for 2016
      if (length(rosenberg_2015) > 0 && !is.na(rosenberg_2015)) {
            self_esteem_2016 <- rbind(self_esteem_2016, data.frame(nomem_encr = id, year = 2016, rosenberg = rosenberg_2015))
      }
}

# Combine the new 2016 observations with the original dataframe
self_esteem <- rbind(self_esteem, self_esteem_2016)

# View the updated dataframe
View(self_esteem)

```


##1.4. Quick explanatory analysis and adding dummy 
```{r}
#average Rosenberg per year
aggregate(rosenberg ~ year, data = self_esteem, FUN = mean, na.rm = TRUE)

#average Rosenberg per year
aggregate(rosenberg ~ year, data = self_esteem, FUN = median, na.rm = TRUE)

#check na's 
aggregate(is.na(rosenberg) ~ year, data = self_esteem, FUN = sum)

#mean standard deviation within individual 
individual_variation <- aggregate(rosenberg ~ nomem_encr, data = self_esteem, FUN = sd, na.rm = TRUE)

mean(individual_variation$rosenberg, na.rm = TRUE)

#histogram
hist(self_esteem$rosenberg, breaks = 49)

#dummy, coded 0 as below or equal to 35 (low SE) or 1 as higher than 35
#or 0 if below or equal to 40 etc.
self_esteem$dum_35 <- ifelse(self_esteem$rosenberg <= 35, 0, 1)
self_esteem$dum_40 <- ifelse(self_esteem$rosenberg <= 40, 0, 1)
self_esteem$dum_45 <- ifelse(self_esteem$rosenberg <= 45, 0, 1)


table(self_esteem$dum_35)
proportions(table(self_esteem$dum_35))

table(self_esteem$dum_40)
proportions(table(self_esteem$dum_40))

table(self_esteem$dum_45)
proportions(table(self_esteem$dum_45))

```


#2.3. Lay-offs, NA Corrected
The above variables creates many na's
so laid_off_na_corrected corrects if a person has a different job status (e.g. housewife, retirement etc)

??? Is this correct to do -> should be considered

If both are NA, lay-off = 0 
If t=0 is na, and t = 1 is 0 then lay-off = 1
If t=0 is na, and t = 1 is 1 then lay-off = 0
If t=0 is 1, and t=1 is NA then lay-off = 0
If t=0 is 0, and t=1 is NA then lay-off = 0

```{r}
# Sort the employment dataframe by id and year
employment <- employment[order(employment$id, employment$year), ]

# Initialize the laid_off_na_corrected column with 0
employment$laid_off_na_corrected <- 0

# Loop through the dataframe to set laid_off_na_corrected
for (i in 1:(nrow(employment) - 1)) {
    if (employment$id[i] == employment$id[i + 1]) {
        current_status <- employment$emp_status[i]
        next_status <- employment$emp_status[i + 1]

        # Check for NA in current and next status
        if (is.na(current_status) || is.na(next_status)) {
            if (is.na(current_status) && !is.na(next_status) && next_status == 0) {
                employment$laid_off_na_corrected[i] <- 1
            } else {
                employment$laid_off_na_corrected[i] <- 0
            }
        } else {
            # Apply conditions for non-NA values
            employment$laid_off_na_corrected[i] <- ifelse(current_status == 1 && next_status == 0, 1, 0)
        }
    }
}


```
