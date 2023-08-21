library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

#-Filtering-#

# Look for an exact match

penguins_biscoe <- penguins %>%
  filter(island == "Biscoe")
head(penguins_biscoe)

penguins_2007 <- penguins %>%
  filter(year == 2007)
head(penguins_2007)

# and
penguins_ad_torg <- penguins %>%
  filter(species == "Adelie" & island == "Torgersen")
# can also use filter(species == "Adelie", island == "Torgersen")
head(penguins_ad_torg)

penguins %>%
  filter(species == "Gentoo" & year == 2008)

# or
gentoo_adelie <- penguins %>%
  filter(species == "Gentoo" | species == "Adelie")

penguins %>%
  filter(island == "Dream" | year == 2009)

# fiddler crabs data

ggplot(pie_crab, aes(x = water_temp, size)) +
  geom_point()


# keep obs for NIB, ZI, DB, JC

crab_sites <- pie_crab %>%
  filter(site %in% c("NIB", "ZI", "DB", "JC")) #can also create this vector outside of filter and just put vector name here
unique(crab_sites$site)

sites <- c("PIE", "ZI", "NIB", "BB", "CC")
pie_crab %>%
  filter(site %in% sites)


# all EXCEPT, exclusion statements

pie_crab %>%
  filter(site != "ZI")

pie_crab %>%
  filter(! site %in% c("BB", "CC", "PIE")) # put ! in front of site to exclude this vector


# Combinations of filtering

crab_size_site <- pie_crab %>%
  filter(site %in% c("NIB", "CC", "ZI"),
         size > 13)
crab_size_site


#-Selecting-#

# individual column by name
crab_subset <- pie_crab %>%
  select(size, site, latitude)
names(crab_subset)

#ranges
crab_subset_range <- pie_crab %>%
  select(site:air_temp)
names(crab_subset_range)

crab_sub_2 <- pie_crab %>%
  select(date:size, water_temp, name)
names(crab_sub_2)

#reorder
reorderd_crabs <- pie_crab %>%
  select(name, water_temp, size, date)
reorderd_crabs

#-Mutate-#

crab_resize <- pie_crab %>%
  mutate(size_cm = size / 10) %>%
  rename(size_mm = size)
crab_resize


pie_crab %>%
  mutate(mean_size = mean(size)) # adds the mean and populates the column with the value output

crabs_teddy <- pie_crab %>%
  mutate(name = "Teddy") # overwrites the name column and values with our new string
crabs_teddy

# group_by plus summarise

group_mean <- pie_crab %>%
  group_by(name) %>%
  summarise(mean_size = mean(size),
            sd_size = sd(size))
group_mean


# group_by then mutate

group_mutate <- pie_crab %>%
  group_by(site) %>%
  mutate(mean_size = mean(size))
unique(group_mutate$mean_size)


# new column based on data

case_crab <- pie_crab %>%
  mutate(bin = case_when(size > 20 ~ "Giant",
                         size <= 20 ~ "Small"))
case_crab

binned_sites <- pie_crab %>%
  mutate(region = case_when(site %in% c("ZI", "CC", "PIE") ~ "Low",
                            site %in% c("ZZ", "NIB") ~ "Medium",
                            TRUE ~ "High")) #TRUE means anything else (the else of case_when())
binned_sites

