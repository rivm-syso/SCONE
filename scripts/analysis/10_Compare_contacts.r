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
# Compare number of contacts with
# - Pienter 2 (2006/2007) and Pienter 3 (2016/2017)
# - Pico's (4 and 6)
# - CoMix (which waves)
#
################################################################################

# round 1: 12 April - 15 May 2021
# round 2: 4 Oct - 8 Nov 2021

# average number of contacts per day (including household members)

SCONE1_comparison <- contacts %>% 
  full_join(participants) %>% 
  group_by(part_id, part_age, round) %>% 
  summarise(ncont = sum(!is.na(cnt_id))/length(unique(part_id))) %>% 
  filter(round == 1) %>% 
  ungroup %>% 
  select(ncont) %>% 
  basic_bootstrap()

SCONE2_comparison <- contacts %>% 
  full_join(participants) %>% 
  group_by(part_id, part_age, round) %>% 
  summarise(ncont = sum(!is.na(cnt_id))/length(unique(part_id))) %>% 
  filter(round == 2) %>% 
  ungroup %>% 
  select(ncont) %>% 
  basic_bootstrap()

# Pienter 2 (2006/2007)

P2_part <- read_csv("/rivm/s/backerj/PienterContact/socialcontactdata.org/Pienter2/2006_Pienter2_NL_participant_common.csv")
P2_cont <- read_csv("/rivm/s/backerj/PienterContact/socialcontactdata.org/Pienter2/2006_Pienter2_NL_contact_common.csv")


P2_comparison <- P2_cont %>% 
  full_join(P2_part) %>% 
  group_by(part_id, part_age) %>% 
  summarise(ncont = sum(!is.na(cont_id))) %>% 
  ungroup() %>% 
  filter(part_age >= 70) %>% 
  select(ncont) %>% 
  basic_bootstrap()


# Pienter 3 (2016/2017)

P3_part <- read_csv("/rivm/s/backerj/PienterContact/socialcontactdata.org/Pienter3/2016_Pienter3_NL_participant_common.csv")
P3_cont <- read_csv("/rivm/s/backerj/PienterContact/socialcontactdata.org/Pienter3/2016_Pienter3_NL_contact_common.csv")

P3_comparison <- P3_cont %>% 
  full_join(P3_part) %>% 
  group_by(part_id, part_age) %>% 
  summarise(ncont = sum(!is.na(cont_id))) %>% 
  ungroup() %>% 
  filter(part_age >= 70) %>% 
  select(ncont) %>% 
  basic_bootstrap()


# Pico 4 (Feb 2021)

pico4 <- readRDS("/rivm/s/backerj/nCoV/ContactPico/data/Contacts_PICO4.rds")

pico4_comparison <- pico4 %>%
  group_by(part_id, part_age) %>% 
  summarise(ncont = sum(!is.na(cnt_age))) %>% 
  ungroup() %>% 
  filter(part_age >= 70) %>% 
  select(ncont) %>% 
  basic_bootstrap()


# Pico 6 (Nov 2021)

pico6 <- readRDS("/rivm/s/backerj/nCoV/ContactPico/data/Contacts_PICO6.rds")

pico6_comparison <- pico6 %>%
  group_by(part_id, part_age) %>% 
  summarise(ncont = sum(!is.na(cnt_age))) %>% 
  ungroup() %>% 
  filter(part_age >= 70) %>% 
  select(ncont) %>% 
  basic_bootstrap()


# CoMix

CoMix_part <- read_csv("/rivm/s/backerj/nCoV/ContactEpiPose/CoMixNL/data/CoMix_nl_participant_common.csv")
CoMix_part_extra <- read_csv("/rivm/s/backerj/nCoV/ContactEpiPose/CoMixNL/data/CoMix_nl_participant_extra.csv")
CoMix_cont <- read_csv("/rivm/s/backerj/nCoV/ContactEpiPose/CoMixNL/data/CoMix_nl_contact_common.csv")


# 3 waves during round 1
CoMix1_comparison <- CoMix_cont %>% 
  full_join(CoMix_part) %>% 
  full_join(CoMix_part_extra) %>% 
  filter(date >= ymd("2021-04-10") & date < ymd("2021-05-25")) %>% 
  group_by(part_id, part_age) %>% 
  summarise(ncont = sum(!is.na(cont_id))) %>% 
  ungroup() %>% 
  filter(part_age >= 70) %>%
  select(ncont) %>% 
  basic_bootstrap()

# last 2 waves in September before round 2
CoMix2_comparison <- CoMix_cont %>% 
  full_join(CoMix_part) %>% 
  full_join(CoMix_part_extra) %>% 
  filter(date >= ymd("2021-09-01")) %>% 
  group_by(part_id, part_age) %>% 
  summarise(ncont = sum(!is.na(cont_id))) %>% 
  ungroup() %>% 
  filter(part_age >= 70) %>% 
  select(ncont) %>% 
  basic_bootstrap()


comparison_data <- bind_rows("SCONE1"= SCONE1_comparison,
                             "SCONE2"= SCONE2_comparison,
                             "P2" = P2_comparison,
                             "P3" = P3_comparison,
                             "pico4" = pico4_comparison,
                             "pico6" = pico6_comparison,
                             "CoMix1" = CoMix1_comparison,
                             "CoMix2" = CoMix2_comparison,
                             .id = "survey") %>% 
  pivot_wider(names_from = probs, values_from = ncont_bs, names_prefix = "ncont_bs_") %>% 
  mutate(ncont_full = paste0(sprintf("%0.1f", ncont_bs_0.5), " (", sprintf("%0.1f", ncont_bs_0.025), " - ", sprintf("%0.1f", ncont_bs_0.975), ")"))

saveRDS(comparison_data, file = "./results/contacts_comparison.rds")


