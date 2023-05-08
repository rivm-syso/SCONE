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
# Save data
# 
# Construct tibbles in socialcontactdata.org format
# Save as csv files
# To be published on Zenodo
#
################################################################################

prefix <- "./data/2021_SCONE_NL_"


##################### participant_common #######################################

participants %>% 
  select(part_id, participant_id, hh_id, part_age, part_gender) %>% 
  write_csv(paste0(prefix, "participant_common.csv"))


##################### participant_extra ########################################

participants %>%
  select(part_id, 
         participant_id, 
         country_of_birth, 
         education, 
         no_contacts, 
         starts_with("vaccination"),
         starts_with("GFI"),
         frailty) %>% 
  write_csv(paste0(prefix, "participant_extra.csv"))


##################### hh_common ################################################

participants %>% 
  mutate(country = "NL") %>% 
  select(hh_id, country, hh_size) %>% 
  write_csv(paste0(prefix, "hh_common.csv"))


##################### sday #####################################################

participants %>% 
  select(part_id, round, survey_date, survey_day, dayofweek) %>% 
  mutate(year = 2021) %>% 
  write_csv(paste0(prefix, "sday.csv"))


##################### contact_common ###########################################

contacts %>% 
  select(part_id,
         participant_id,
         cnt_id,
         contact_id,
         cnt_age_exact,
         cnt_age_est_min,
         cnt_age_est_max,
         cnt_gender,
         hh_member,
         ends_with("_contact"),
         starts_with("cont_")) %>% 
  write_csv(paste0(prefix, "contact_common.csv"))


##################### invited (not for Zenodo) #################################

invited %>% 
  write_csv(paste0(prefix, "invited.csv"))


rm(prefix)



