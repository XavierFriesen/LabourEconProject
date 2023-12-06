prop.table(table(df_no_na_fem$dum_40_inp))
prop.table(table(df_no_na_male$dum_40_inp))

#not laid off
# Load necessary library
library(dplyr)

# Assuming df_no_na is your dataframe
# Group by individual and summarize
df_grouped <- df_no_na %>%
      group_by(id) %>%
      summarise(laid_off = max(laid_off),  # 1 if laid off at least once
                dum_40_inp = min(dum_40_inp))  # Assuming dum_40_inp doesn't change per individual

# Now create the table
result_table <- table(df_grouped$laid_off, df_grouped$dum_40_inp)

# View the table
result_table

remove(df_grouped, result_table)


library(dplyr)

# Assuming df_no_na is your dataframe
# Group by individual and then process each group
df_grouped <- df_no_na %>%
      group_by(id) %>%
      arrange(time, .by_group = TRUE) %>%  # Assuming 'time' is the variable indicating the order of events
      filter(laid_off == 1) %>%  # Keep only rows where laid_off is 1
      slice(1) %>%  # Select the first such row for each individual
      ungroup() %>%
      mutate(laid_off = 1)  # Since these are all cases where laid_off is 1

# Now create the table with the original data, but with updated dum_40_inp
# We'll join the original dataset with the grouped dataset to get the updated dum_40_inp
df_updated <- df_no_na %>%
      left_join(df_grouped[, c("id", "dum_40_inp")], by = "id", suffix = c("", "_first_laid_off")) %>%
      mutate(dum_40_inp = ifelse(is.na(dum_40_inp_first_laid_off), dum_40_inp, dum_40_inp_first_laid_off))

result_table <- table(df_updated$laid_off, df_updated$dum_40_inp)

# View the table
result_table

remove(df_updated, df_grouped, result_table)
library(dplyr)

# Assuming df_no_na is your dataframe
# First, identify the first occurrence of being laid off for each individual
first_laid_off <- df_no_na %>%
  group_by(id) %>%
  arrange(time, .by_group = TRUE) %>%  # Replace 'time' with your actual time variable
  filter(laid_off == 1) %>%
  slice(1) %>%
  select(id, dum_40_inp) %>%
  ungroup()

# Now, summarize the overall laid_off status for each individual
individual_summary <- df_no_na %>%
  group_by(id) %>%
  summarize(laid_off_ever = max(laid_off)) %>%
  left_join(first_laid_off, by = "id")

# Create the table
result_table <- table(individual_summary$laid_off_ever, individual_summary$dum_40_inp)

# View the table
result_table
