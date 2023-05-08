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
# Load raw data
# 
# Raw data are entered in Access databases:
# - dataSCONE_round1.accdb
# - dataSCONE_round2.accdb
#
# Each database exports a participants and a contacts table as Excel file
# 
# Of invited persons birthday and sex are used to calculate number of invited per
# age group and sex
#
# Weekday numbers and weekdays in English and Dutch are needed to reorganize data
# (see 03_Reorganize_data.r)
#
# NOTE: directory ./data_raw/ is not publicly accessible
#
################################################################################


participants_round1 <- read_excel("./data_raw/dataSCONE_participants_round1.xlsx")
contacts_round1 <- read_excel("./data_raw/dataSCONE_contacts_round1.xlsx")

participants_round2 <- read_excel("./data_raw/dataSCONE_participants_round2.xlsx")
contacts_round2 <- read_excel("./data_raw/dataSCONE_contacts_round2.xlsx")

invited_round1 <- read_excel("./data_raw/Invited_round1.xlsx")
invited_round2 <- read_excel("./data_raw/Invited_round2.xlsx")
invited_target <- read_excel("./data_raw/Invited_target.xlsx")

# Combine the two rounds, and add unique participant_id to contacts

participants <- bind_rows(
  "round 1" = participants_round1,
  "round 2" = participants_round2,
  .id = "round"
) %>% 
  mutate(participant_id = as.integer(Participantcode))

contacts <- bind_rows(
  "round 1" = contacts_round1,
  "round 2" = contacts_round2,
  .id = "round"
) %>% 
  left_join(participants %>% select(round, `ID-nrpatient`, participant_id))

invited <- bind_rows(
  "round 1" = invited_round1,
  "round 2" = invited_round2,
  .id = "round"
)

# weekday names in English and Dutch
weekdays <- tibble(dayofweek = 1:7,
                   survey_day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                   weekdag = c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag"))


rm(participants_round1)
rm(participants_round2)
rm(contacts_round1)
rm(contacts_round2)
rm(invited_round1)
rm(invited_round2)

