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





