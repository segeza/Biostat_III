
# Installing packages
pacman::p_load(
  dplyr,
  data.table,
  gapminder,
  dplyr,
  readr,
  tidyr,
  ggplot2
  )

# Data loading and screening
data <- gapminder   #Life expectnacy dataset across the globe
str(data)
glimpse(data)

# Subsetting Rows

# 01. Filter
gapminder_africa <- filter(data, continent == "Africa")

gapminder_2000 <- filter(data, year >= 2000)

gapminder_2002 <-  filter(data, year == 2002)

gapminder_65 <- filter(data, lifeExp > 65) 

gaminder_africa_2000 <- filter(data, continent == "Africa", year >2000)

gapminder_europe_2000 <- filter(data, continent == "Europe" | year < 2000)

# 02. Slice
gapminder_1 <- slice(data, 1)  # First row
gapminder_last <- slice(data, n())
gapminder_5 <- slice(data, 5) 
gapminder_first5 <- slice_head(data, n=5) 
gapminder_last5 <- slice_tail(data, n=5)

# For slicing smallest observations for Life Exp
gamminder_life <- slice_min(data, order_by = lifeExp, n=5)



# Subsetting Columns

diamond <- diamonds # New data set for this ecxercise

# 01. Select

diamonds_subset <- select (diamond, carat, cut, clearly)

diamonds_no_xyz <- select(diamond, -c(x,y,z))

# Using column numbers
diamonds_1_6 <- select(diamond, -(1:6))


diamonds_start_c <- diamond %>% 
  select(starts_with("c"))

diamonds_ends_e <- diamond %>% 
  select(ends_with("e"))

diamond_second_last <- diamond %>% 
  select(last_col()-1)

diamonds_with_e <- diamond %>% 
  select(contains("e"))

# Remove diamonds_ends_e from the environment
rm(diamonds_ends_e)

# piping

gapminder_x <- data %>% 
  select(continent, country, lifeExp) %>% 
  filter(lifeExp > 35)

gapminder_botswana <- data %>% 
  filter(gdpPercap >= 800 & country == "Botswana") %>% 
  select(country)

# A European observation of a country with the highest Life expectancy in 2007
gapminder_euro <- data %>% 
  filter( continent == "Europe" & year == 2007) %>% 
  slice_max( order_by = lifeExp, n=1)



# Sorting

diamonds_carat_sort <- arrange(diamond, carat)
#OR
diamonds_carat_sort_1 <- arrange(diamond, desc(carat))

diamonds_carat_cuts <- arrange(diamond, cut, desc(carat))

View(diamonds_carat_cuts)


# Mutate

gapminder <- mutate(gapminder, gdp = gdpPercap * pop)

gapminder_01 <-  gapminder %>% 
  mutate(gdp = gdpPercap * pop,
         gdp_million = gdp/1000000)

View(gapminder_01)


# Summaries
diamond %>% 
  summarise(mean_carat = mean(carat, na.rm = T),
            mean_depth = mean(depth, na.rm = T),
            mean_price = mean(price, na.rm = T)
            )


# Grouping

avg_life_exp <- data %>% 
  summarise(mean_lifeexp = mean(lifeExp, na.rm = T), 
            .by = country)

avg_life_exp_years <- data %>% 
  summarise(mean_lifeexp = mean(lifeExp, na.rm = T), 
            .by = c (country, year))


# Ask Eke ???????????
first_obs_country <- data %>% 
  group_by(country) %>% 
  arrange(gdpPercap) %>% 
  slice(1:3)
View(first_obs_country)

# Excercise

# Qn 01
new_diamond <- mutate(diamond, my_depth = 300 * z / (x + y))

# Qn 02
new_gpm <- filter(data, continent == "Europe", 
                  lifeExp < 50, 
                  year < 1990)

# qn 03
diamond_price_depth <- mutate(diamond, 
                              price_depth_ratio = price/depth)


View(new_gpm)


# Merging datasets

# 01. Mergninga data sets thata have the same number of columns

gapminder_africa <- filter(data, continent == "Africa")
gapminder_europe <- filter(data, continent == "Europe")

combined_gapminder <- bind_rows(gapminder_africa, gapminder_europe)

# 02. data sets with same number of rows (want to join columns)

gapmider_first_3 <- select(data, country, continent, year)
gapminder_next_3 <- select(data, lifeExp, pop)
combined_02 <- bind_cols(gapminder_next_3, gapmider_first_3)


# Joining using a unique identifier

df_primary <- tibble(
  ID = 1:5,
  sex = c("m", "m", NA, "f", "f"),
  age = c(19, 22, NA, 19, 18)
)

df_secondary <- tibble( ID = c(2, 3, 4, 4, 5, 5, 6, 6, 7), 
                        score = c(10, 18, 21, 23, 9, 11, 11, 12, 3)
)

# Left Join
left_join_result <- df_primary %>%
  left_join(df_secondary, by = "ID")

View(left_join_result)

# OR
right_join_result <- df_primary %>%
  left_join(df_secondary, by = "ID")

View(right_join_result)


# Inner Join (retains only shared IDs)
inner_join_result <- df_primary %>%
  inner_join(df_secondary, by = "ID")

# Full Join (Keeps everything)
full_join_result <- df_primary %>%
  full_join(df_secondary, by = "ID")

# Anti Join (Keeps only what's missiong in the second dataset)
anti_join_result <- df_primary %>%
  anti_join(df_secondary, by = "ID")

# In cases where you have more than one identifier
df_1 <- tribble(
  ~ID, ~year, ~items,
  "A", 2015,3,
  "A", 2016,7,
  "A", 2017,6,
  "B", 2015,4,
  "B", 2016,8,
  "B", 2017,7,
  "C", 2015,4,
  "C", 2016,6,
  "C", 2017,6)

df_2 <- tribble(
  ~PID, ~year, ~prices,
  "A", 2015,9,
  "A", 2016,8,
  "A", 2017,12,
  "B", 2015,13,
  "B", 2016,14,
  "B", 2017,6,
  "C", 2015,15,
  "C", 2016,15,
  "C", 2017,13)

df_join <- left_join(df_1, df_2, 
                     by = c("ID" = "PID", "year", "prices"))
# Alternatively (Look at the "ID")
df_join <- left_join(df_2, df_1, 
                     by = c("PID" = "ID", "year", "prices"))


# Reshaping data using tidyr (using gapminder data)

gaminder_wide <- gapminder %>%
  select(country, year, pop, gdpPercap) %>%  # Only select relevant columns
  pivot_wider(names_from = year, values_from = c(gdpPercap, pop))

View(gaminder_wide)

gapminder_wide3 <- gapminder %>%
  select(country, year, gdpPercap) %>%  
  pivot_wider(names_from = year, values_from = c(gdpPercap), names_prefix = "gdp_")

View(gapminder_wide3)


# Converting back to longer format
gapminder_long_gdp <- gapminder_wide3 %>%
  pivot_longer(cols = matches("gdp_"), # Select all columns starting with 'gdp_'. Can be done manually
               names_to = "year",           # Create a new 'year' column
               values_to = "gdpPercap",   # Create a new 'gdpPercap' column with the values
               names_prefix = "gdp_") 

View(gapminder_long_gdp)

