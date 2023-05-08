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
# Prepare data for analysis
# - define age groups of participants, contacts, and population
# - factor participant variables to order and label them properly
#
################################################################################


participants <- participants %>% 
  mutate(part_age_group = cut(part_age, 
                              breaks = c(seq(70, 90, 5), Inf),
                              labels = c("70-74", "75-79", "80-84", "85-89", "90+"),
                              right = FALSE,
                              include_lowest = TRUE),
         part_gender = factor(part_gender, levels = c("F", "M"), labels = c("Female", "Male")),
         education = factor(education, 
                            levels = c("Primary or no education", "Secondary education (3 or 4 years)", "Secondary education (5 or 6 years)", "Higher education"),
                            labels = c("Primary or no education", "Secondary education\n(3 or 4 years)", "Secondary education\n(5 or 6 years)", "Higher education")),
         round = factor(round)
         )

contacts <- contacts %>% 
  mutate(cnt_age = coalesce(cnt_age_exact, 0.5*(cnt_age_est_min + cnt_age_est_max)),
         cnt_age_group = cut(cnt_age, 
                             breaks = c(0, 18, seq(30, 60, 10), seq(70, 90, 5), Inf),
                             labels = c("0-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                             right = FALSE,
                             include_lowest = TRUE)) 

invited <- invited %>% 
  mutate(sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
         round = factor(round))


population <- population %>% 
  mutate(age_group = cut(age, 
                         breaks = c(0, 18, seq(30, 60, 10), seq(70, 90, 5), Inf),
                         labels = c("0-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                         right = FALSE,
                         include_lowest = TRUE)) %>% 
  group_by(age, age_group, gender) %>% 
  summarise(n = sum(population)) %>% 
  ungroup

population_immigrated <- population_immigrated %>% 
  transmute(gender = if_else(Geslacht == "Mannen", "Male", "Female"),
            age = str_extract(Leeftijd, pattern = "[0-9]{2}"),
            n = aantal) %>% 
  mutate(age_group = cut(as.integer(age), breaks = c(seq(70, 90, 5), Inf), 
                         labels = c("70-74", "75-79", "80-84", "85-89", "90+"),
                         include_lowest = TRUE, 
                         right = FALSE)) %>% 
  group_by(gender, age_group) %>% 
  summarise(n = sum(n))


population_incare <- population_incare %>% 
  mutate(aantal = aantal...3 + aantal...4) %>% 
  transmute(gender = if_else(Geslacht == "Mannen", "Male", "Female"),
            age = as.integer(str_extract(Leeftijd, pattern = "[0-9]{2}")),
            n = aantal) 

color_palette <- c("#984464","#CF597E", "#008080", "#63A99E", "#366296", "#4684B4")

