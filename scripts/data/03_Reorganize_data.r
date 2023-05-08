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
# Reorganize data
# 
# Change participants to long format with one participation day per record
# Translate variable names to English
# Unknown part_age are recorded as 0 -> recode to NA (will be excluded later)
# Unknown cnt_age_exact are recorded as -1 -> recode to NA
# GFI score and frailty boolean are calculated per participant
#
# Create id's:
#   participant id = round + participant number (1 + 3 digits)
#   part_id = participant_id + weekday number (4 + 1 digits)
#   contact_id = participant_id + contact number (4 + 3 digits)
#   cont_id = contact_id + weekday number (7 + 1 digits)
# Note: participant_id and contact_id are always unique persons
#
# Resulting participants and contacts only have participant_id and part_id in common
#
# For each invited person the age at time of invitation is calculated
# Invited persons grouped by age group, sex and round for response rate calculation
#
################################################################################



###################### Participants ############################################

# general information per participant in English
participants <- participants %>% 
  mutate(
    part_age = if_else(Leeftijd == 0, NA_real_, Leeftijd),
    part_gender = case_when(Geslacht == "Man" ~ "M",
                            Geslacht == "Vrouw" ~ "F",
                            TRUE ~ NA_character_),
    round = gsub(round, pattern = "round ", replacement = "") %>% as.integer,
    country_of_birth = case_when(
      Geboorteland == "Ander land" ~ "Other",
      Geboorteland == "Nederland" ~ "NL",
      TRUE ~ NA_character_),
    education = case_when(
      grepl(Opleiding, pattern = "1") ~ "Primary or no education",
      grepl(Opleiding, pattern = "2") ~ "Secondary education (3 or 4 years)",
      grepl(Opleiding, pattern = "3") ~ "Secondary education (5 or 6 years)",
      grepl(Opleiding, pattern = "4") ~ "Higher education",
      grepl(Opleiding, pattern = "Zeg") ~ "Prefer not to say",
      TRUE ~ NA_character_),
    hh_size = case_when(
      Huishoudgrootte == "Leeg" ~ NA_character_,
      Huishoudgrootte == "Niet van toepassing" ~ "Not applicable",
      TRUE ~ Huishoudgrootte),
    across(.cols = starts_with("GFI"),
           ~ case_when(. == "ja" ~ "Yes",
                       . == "nee" ~ "No",
                       . == "soms" ~ "Sometimes",
                       . == "leeg" ~ NA_character_,
                       TRUE ~ .))
  ) %>% 
  rename(vaccination_COVID19 = Vaccinatie_COVID19,
         vaccination_influenza = Vaccinatie_influenza,
         vaccination_IPD = Vaccinatie_pneumokokken,
         vaccination_none = Vaccinatie_geen,
         vaccination_prefernottosay = Vaccinatie_zegiklieverniet,
         vaccination_NA = Vaccinatie_leeg,
         GFI_groceries = GFI_boodschappen,
         GFI_walking = GFI_lopen,
         GFI_dressing = GFI_aankleden, 
         GFI_grade = GFI_rapportcijfer, 
         GFI_seeing = GFI_zien,
         GFI_hearing = GFI_horen,
         GFI_weightloss = GFI_afgevallen,
         GFI_medicine = GFI_medicijnen,
         GFI_memory = GFI_geheugen, 
         GFI_emptiness = GFI_leegte,
         GFI_missing = GFI_gemis,
         GFI_abandoned = GFI_indesteek, 
         GFI_sad = GFI_somber, 
         GFI_anxious = GFI_angstig)


# Calculate GFI score and define frailty
participants <- participants %>% 
  left_join(
    participants %>%
      #filter(!duplicated(participant_id)) %>% 
      select(participant_id, starts_with("GFI")) %>%
      pivot_longer(cols = starts_with("GFI"), names_to = "GFI", names_prefix = "GFI_") %>%
      mutate(GFI_score = case_when(
        GFI %in% c("groceries", "walking", "dressing", "toilet") & value == "Yes" ~ 0,
        GFI %in% c("groceries", "walking", "dressing", "toilet") & value == "No" ~ 1,
        GFI == "grade" & value %in% as.character(7:10) ~ 0,
        GFI == "grade" & value %in% as.character(0:6) ~ 1,
        GFI %in% c("seeing", "hearing", "weightloss", "medicine") & value == "No" ~ 0,
        GFI %in% c("seeing", "hearing", "weightloss", "medicine") & value == "Yes" ~ 1,
        GFI == "memory" & value %in% c("No", "Sometimes") ~ 0,
        GFI == "memory" & value == "Yes" ~ 1,
        GFI %in% c("emptiness", "missing", "abandoned", "sad", "anxious") & value == "No" ~ 0,
        GFI %in% c("emptiness", "missing", "abandoned", "sad", "anxious") & value %in% c("Yes", "Sometimes") ~ 1
      )) %>% 
      group_by(participant_id) %>% 
      # max 2 missing values per participant (excluding 4 participants)
      filter(sum(is.na(GFI_score)) <= 2) %>% 
      summarise(GFI_score = sum(GFI_score, na.rm = TRUE)) %>% 
      mutate(frailty = if_else(GFI_score >= 4, TRUE, FALSE)) 
  )


# participant data in long format (one record for each filled out day)
# create part_id by concatenating participant_id and weekday number
participants <- participants %>% 
  pivot_longer(cols = starts_with("Invuldatum"), 
               names_to = "weekdag", 
               names_prefix = "Invuldatum_", 
               values_to = "date") %>% 
  mutate(survey_date = as.Date(date)) %>% 
  full_join(
    participants %>% 
      pivot_longer(cols = starts_with("Geencontacten"), 
                   names_to = "weekdag", 
                   names_prefix = "Geencontacten_",
                   values_to = "no_contacts") %>% 
      select(participant_id, weekdag, no_contacts)
  ) %>% 
  full_join(
    participants %>% 
      pivot_longer(cols = starts_with("Nietsingevuld"), 
                   names_to = "weekdag", 
                   names_prefix = "Nietsingevuld_",
                   values_to = "not_filled_out") %>% 
      select(participant_id, weekdag, not_filled_out)
  ) %>% 
  filter(!not_filled_out) %>% 
  full_join(weekdays %>% mutate(weekdag = tolower(weekdag))) %>% 
  mutate(part_id = as.integer(paste0(participant_id, dayofweek)),
         hh_id = paste0("HH", part_id)) %>% 
  select(part_id, 
         participant_id,
         round, 
         survey_day,
         survey_date,
         dayofweek,
         part_age, 
         part_gender, 
         hh_id,
         hh_size, 
         country_of_birth, 
         education,
         no_contacts,
         starts_with("vacc"), 
         starts_with("GFI"),
         frailty)
  

###################### Contacts ################################################

# rename variables in English
# variable naming convention: 
#   cnt_ refers to person
#   _contact refers to contact characteristic
#   cont_ refers to contact location
contacts <- contacts %>% 
  rename(cnt_name = Naam,
         cnt_age_exact = Leeftijd_precies,
         cnt_age_est_min = Leeftijd_van,
         cnt_age_est_max = Leeftijd_tot,
         hh_member = Huisgenoot,
         phys_contact = Fysiek,
         prot_contact = Bescherming,
         dist_contact = Afstand,
         over15min_contact = `Lange duur`,
         cont_home = Locatie_thuis,
         cont_otherhome = Locatie_anderhuis,
         cont_work = Locatie_werk,
         cont_transport = Locatie_transport,
         cont_leisure = Locatie_vrijetijd,
         cont_shop = Locatie_winkel,
         cont_outside = Locatie_buiten,
         description_cont_otherplace = Locatie_anders_namelijk) %>% 
  mutate(cont_otherplace = if_else(!is.na(description_cont_otherplace), "x", NA_character_),
         cnt_age_exact = if_else(cnt_age_exact < 0, NA_real_, cnt_age_exact),
         cnt_gender = case_when(Geslacht == "man" ~ "M",
                                Geslacht == "vrouw" ~ "F",
                                TRUE ~ NA_character_),
         across(c("hh_member", contains("contact")),
                ~ case_when(. == "ja" ~ TRUE,
                            . == "nee" ~ FALSE,
                            . == "leeg" ~ NA)),
         across(.cols = starts_with("cont_"),
                ~ case_when(. == "x" ~ TRUE,
                            TRUE ~ FALSE))) %>% 
  select(-description_cont_otherplace)


# create part_id by concatenating participant_id and weekday number
# create contact_id by concatenating participant_id and unique contact persons (using cnt_name to identify)
# create cnt_id by concatenating contact_id and weekday number
contacts <- contacts %>% 
  full_join(weekdays %>% rename(Invoerdag = weekdag)) %>% 
  mutate(part_id = as.integer(paste0(participant_id, dayofweek))) %>% 
  arrange(part_id, dayofweek) %>% 
  group_by(participant_id) %>% 
  group_split() %>% 
  map_df(~.x %>% 
           group_by(cnt_name) %>% 
           mutate(tmp_id = cur_group_id(), 
                  tmp_id2 = cur_group_id():(cur_group_id() -1 + n())) %>% 
           ungroup() %>% 
           mutate(tmp_id = if_else(is.na(cnt_name), tmp_id2, tmp_id),
                  contact_id = paste0(participant_id, formatC(tmp_id, width=3, flag="0")),
                  cnt_id = paste0(contact_id, dayofweek))
  ) %>% 
  select(participant_id, 
         part_id, 
         contact_id, 
         cnt_id, 
         starts_with("cnt_"), 
         hh_member, 
         ends_with("_contact"), 
         starts_with("cont_")) %>% 
  select(-cnt_name)

###################### Invited #################################################

invited <- invited %>%
  mutate(sex = factor(Geslachtsaanduiding) %>% fct_recode(M = "Man", F = "Vrouw"),
         birth_date = str_replace_all(Geboortedatum, pattern = "00-00-", replacement = "01-01-"),
         round = gsub(round, pattern = "round ", replacement = "") %>% as.integer,
         invitation_date = case_when(round == 1 ~ as.Date("2021-03-30"),
                                     round == 2 ~ as.Date("2021-10-01"),
                                     TRUE ~ ymd(NA)),

         age = floor(as.integer(invitation_date - as.Date(birth_date, "%d-%m-%Y"))/365.25),
         age_group = cut(age,
                         breaks = c(seq(70, 90, 5), Inf),
                         include_lowest = TRUE,
                         right = FALSE,
                         labels = c("70-74", "75-79", "80-84", "85-89", "90+"))) %>% 
  group_by(round, age_group, sex) %>% 
  count %>% 
  full_join(invited_target)


