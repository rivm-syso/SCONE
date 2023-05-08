################################################################################
#
# Copyright 2022 Rijksinstituut voor Volksgezondheid en Milieu (RIVM).
#
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU Affero General Public License as published by the Free 
# Software Foundation, either version 3 of the License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR 
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with 
# this program.  If not, see <https://www.gnu.org/licenses/>.”
#
################################################################################
#
# Load data
# 
# Load csv files in socialcontactdata.org format 
# (produced in data pipeline ./scripts/data)
# Combine to participants and contacts
#
################################################################################

local <- "./data/2021_SCONE_NL_"
public <- "https://zenodo.org/record/7649375/files/2021_SCONE_NL_"

participants <- read_in(file = "participant_common.csv", local = local, public = public) %>% 
  full_join(read_in(file = "hh_common.csv", local = local, public = public)) %>% 
  full_join(read_in(file = "sday.csv", local = local, public = public)) %>% 
  full_join(read_in(file = "participant_extra.csv", local = local, public = public))

contacts <- read_in(file = "contact_common.csv", local = local, public = public)

invited <- read_in(file = "invited.csv", local = local, public = public)

population <- read_csv("./data/populationNL_2021.csv")

population_immigrated <- tibble(file = list.files("./data", full.names = TRUE)) %>% 
  filter(grepl(file, pattern = "migratie")) %>% 
  pull(file) %>% 
  read_csv2(skip = 5) %>% 
  slice(-n())

population_incare <- tibble(file = list.files("./data", full.names = TRUE)) %>% 
  filter(grepl(file, pattern = "institutionele")) %>% 
  pull(file) %>% 
  read_csv2(skip = 4) %>%
  slice(-n())

rm(local)
rm(public)
