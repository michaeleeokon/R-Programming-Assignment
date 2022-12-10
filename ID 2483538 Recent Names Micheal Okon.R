#loading packages
library(dplyr)
library(babynames)
library(ggplot2)

Use suppressPackageStartupMessages() to eliminate package
startup messages

babynames <- babynames %>% 
  select(year, sex, name, number = n)
  
  head(babynames)
  tail(babynames)
  
  glimpse(babynames)
  
  Rows: 1,924,665
Columns: 4
$ year   [3m[38;5;246m<dbl>[39m[23m 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1…
$ sex    [3m[38;5;246m<chr>[39m[23m "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", …
$ name   [3m[38;5;246m<chr>[39m[23m "Mary", "Anna", "Emma", "Elizabeth", "Minnie", "Margare…
$ number [3m[38;5;246m<int>[39m[23m 7065, 2604, 2003, 1939, 1746, 1578, 1472, 1414, 1320, 1…


babynames %>%
  # Filter for the year 1990
  filter(year == 1990) %>%
  # Sort the number column in descending order 
  arrange(desc(number))
  
  # Find the most common name in each year
babynames %>%
  group_by(year) %>%
  top_n(1, number)
  
  # Filter for the names Steven, Thomas, and Matthew 
selected_names <- babynames %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"), sex == "M")

# Plot the names using a different color for each name
ggplot(selected_names, aes(x = year, y = number, color = name)) +
  geom_line()
  
  # Find the year each name is most common 
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total)%>%
  group_by(name)%>%
  top_n(1, fraction)
  
  babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  # Ungroup the table 
  ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum 
  mutate(fraction_max = number / name_max)
  
  names_normalized <- babynames %>%
                     group_by(name) %>%
                     mutate(name_total = sum(number),
                            name_max = max(number)) %>%
                     ungroup() %>%
                     mutate(fraction_max = number / name_max)
					 
					 # Filter for the names Steven, Thomas, and Matthew
names_filtered <- names_normalized %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"), sex == "M")

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) +
  geom_line()
  
  babynames_fraction <- babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total)
  
  babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio between each year 
  mutate(ratio = fraction / lag(fraction))
  
  babynames_ratios_filtered <- babynames_fraction %>%
                     arrange(name, year) %>%
                     group_by(name) %>%
                     mutate(ratio = fraction / lag(fraction)) %>%
                     filter(fraction >= 0.00001)
					 
					 babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  top_n(1, ratio) %>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)