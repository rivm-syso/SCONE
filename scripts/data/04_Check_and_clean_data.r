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
# Check and clean data
# 
# Correct household members: set hh_member to FALSE when
#   participants lives alone or in a nursing home
#   contact age is not known exactly
# Make vaccination status of participants consistent
#   first check consistency vaccination_none, vaccination_NA, vaccination_prefernottosay 
#   then: prefernottosay -> vaccination status to NA
#         not filled out -> vaccination status and prefernottosay to NA
# Check inconsistencies in household size and reported household members
#   but leave as is
# Check if participant checks box that (s)he did not have any contacts on a day,
#   that part_id does not occur in contacts
#
################################################################################

################## Exclude participants without age or gender ##################

# filter out participants without age or gender (participant_id 2030)
# (should never have been entered in Access database)
tmp_id <- participants %>%
  filter(is.na(part_age) | is.na(part_gender)) %>%
  select(part_id)

participants <- participants %>% anti_join(tmp_id)
contacts <- contacts %>% anti_join(tmp_id)


################## Correction household members ################################

# determine part_id of participants living alone or in nursing home
tmp_id <- participants %>% 
  filter(hh_size == "1" | hh_size == "Not applicable") %>% 
  pull(part_id)

# 144 contacts are reported as household member of participants living alone
contacts %>% filter(part_id %in% tmp_id & hh_member) %>% nrow
# -> set hh_member to FALSE
contacts <- contacts %>% 
  mutate(hh_member = if_else(part_id %in% tmp_id, FALSE, hh_member))

# 61 contacts are reported as household member but without exact age
contacts %>% filter(is.na(cnt_age_exact) & hh_member) %>% nrow
# -> set hh_member to FALSE
contacts <- contacts %>% 
  mutate(hh_member = if_else(is.na(cnt_age_exact), FALSE, hh_member))


################## Make vaccination status consistent ##########################

# check vaccination_none, vaccination_prefernottosay, and vaccination_NA can 
# only be TRUE if rest is FALSE (i.e. the only TRUE in row of vaccination variables) 
participants %>% 
  filter(!duplicated(participant_id)) %>% 
  filter(vaccination_none | vaccination_prefernottosay | vaccination_NA) %>% 
  select(starts_with("vaccination")) %>% 
  rowSums()

participants <- participants %>% 
  # set vaccination status to NA if participant does not prefer to say (1 participant)
  # set vaccination status and prefernottosay to NA if question is not filled out (6 participants)
  # variables vaccination_NA and vaccination_none not needed anymore
  mutate(across(.cols = paste0("vaccination_", c("COVID19", "influenza", "IPD")), ~ if_else(vaccination_prefernottosay, NA, .)),
         across(.cols = paste0("vaccination_", c("COVID19", "influenza", "IPD", "prefernottosay")), ~ if_else(vaccination_NA, NA, .))) %>% 
  select(-vaccination_NA, -vaccination_none)


################## Inconsistencies (leave as is) ###############################

# check if participant reported household member every participation day
# 16 household members are not reported every day the participant participated
contacts %>% 
  filter(hh_member) %>% 
  group_by(participant_id, contact_id) %>% 
  summarise(n_cont = n()) %>% 
  left_join(participants %>% 
              group_by(participant_id) %>% 
              count(name = "n_part")
            ) %>% 
  group_by(n_cont, n_part) %>% 
  count %>% 
  filter(n_cont != n_part)

# check if number of reported household members == household size - 1
# 99 participants report household members that don't agree with household size
# (mainly participant in 2-person household not mentioning partner)
contacts %>% 
  filter(hh_member) %>% 
  group_by(participant_id) %>% 
  summarise(n_hh_member = length(unique(contact_id))) %>% 
  full_join(participants %>% 
              group_by(participant_id) %>% 
              summarise(hh_size = first(hh_size))) %>% 
  replace_na(replace = list(n_hh_member = 0)) %>% 
  group_by(n_hh_member, hh_size) %>% 
  count %>% 
  ungroup %>% 
  filter(hh_size != "Not applicable") %>% 
  filter(n_hh_member != as.integer(hh_size) - 1)


################## Checks ######################################################

# check part_id does not occur in contacts when no_contacts = TRUE
participants %>% 
  filter(no_contacts) %>% 
  select(part_id) %>% 
  semi_join(contacts)


# check not more invited than target and all invited accounted for
invited %>% filter(n > target | is.na(age_group)) %>% nrow


rm(tmp_id)
