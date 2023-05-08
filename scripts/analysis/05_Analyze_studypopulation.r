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
# Analyze study population
# - frailty fraction by age group and gender
#
################################################################################

# Overview table

tmp_part <- participants %>% 
  filter(!duplicated(participant_id)) %>% 
  mutate(round = if_else(round == 1, "1 (April 2021)", "2 (October 2021)"),
         country_of_birth = if_else(country_of_birth == "NL", "Netherlands", "Other country"),
         GFI_score = case_when(GFI_score <= 1 ~ "0-1 (non-frail)",
                               GFI_score <= 3 ~ "2-3 (non-frail)",
                               GFI_score <= 5 ~ "4-5 (frail)",
                               GFI_score <= 7 ~ "6-7 (frail)",
                               GFI_score <= 15 ~ "8+ (frail)"),
         frailty = if_else(frailty, "frail", "non-frail"),
         hh_size = case_when(hh_size == "Not applicable" ~ "Not applicable",
                             hh_size == 1 ~ "1",
                             hh_size == 2 ~ "2",
                             hh_size > 2 ~ "3+",
                             TRUE ~ NA_character_))


part_table <- bind_rows("Survey period" = tmp_part %>% rename(var = round) %>% group_by(var) %>% count,
                        "Participant age group" = tmp_part %>% rename(var = part_age_group) %>% group_by(var) %>% count,
          "Participant gender" = tmp_part %>% rename(var = part_gender) %>% group_by(var) %>% count,
          "Country of birth" = tmp_part %>% rename(var = country_of_birth) %>% group_by(var) %>% count,
          "Frailty (GFI)" = tmp_part %>% rename(var = GFI_score) %>% group_by(var) %>% count,
          "Household size" = tmp_part %>% rename(var = hh_size) %>% group_by(var) %>% count,
          "Education level" = tmp_part %>% rename(var = education) %>% group_by(var) %>% count,
          .id = "name") %>% 
  group_by(name) %>% 
  mutate(perc = 100*n/sum(n),
         var = if_else(is.na(var), "(Missing)", var),
         rank = 1:n()) %>% 
  mutate(variable = if_else(rank == 1, name, ""),
         "n (%)" = paste0(n, " (", sprintf("%0.1f", perc), ")"))


saveRDS(part_table, "./results/part_table.rds")

# number of immigrated participants: 34
# expected number of immigrated participants: 53

tmp_part %>% filter(country_of_birth == "Other country") %>% count

population %>% 
  filter(age >= 70) %>% 
  group_by(gender, age_group) %>% 
  summarise(n = sum(n)) %>% 
  ungroup %>% 
  full_join(population_immigrated %>% rename(n_imm = n)) %>% 
  full_join(participants %>% 
              filter(!duplicated(participant_id)) %>%
              group_by(part_gender, part_age_group) %>% 
              count %>% 
              rename(n_part = n),
            by = c("age_group" = "part_age_group",
                   "gender" = "part_gender")) %>% 
  mutate(expected = n_part*n_imm/n) %>% 
  summarise(expected = sum(expected))

# lower response not caused by stochastic effects
pbinom(q = 34, size = 730, prob = 53/730)

# number of participants in care: 10
# expected number of participants in care: 60

tmp_part %>% filter(hh_size == "Not applicable") %>% count

population %>% 
  filter(age >= 70) %>% 
  mutate(age = if_else(age >= 95, 95, age)) %>% 
  group_by(gender, age) %>% 
  summarise(n = sum(n)) %>% 
  full_join(population_incare %>% rename(n_incare = n)) %>% 
  full_join(participants %>% 
            filter(!duplicated(participant_id)) %>%
            mutate(part_age = if_else(part_age >= 95, 95, part_age)) %>% 
            group_by(part_gender, part_age) %>% 
            count %>% 
            rename(n_part = n),
          by = c("age" = "part_age",
                 "gender" = "part_gender")) %>% 
  ungroup %>% 
  mutate(expected = n_part*n_incare/n) %>%
  summarise(expected = sum(expected))


# frailty between rounds not markedly different -> omit round in frailty plot
participants %>% 
  filter(!duplicated(participant_id), !is.na(frailty)) %>% 
  group_by(part_age_group, part_gender, round) %>% 
  summarise(frail = sum(frailty),
            nonfrail = sum(!frailty),
            fracfrail = sum(frailty)/n()) %>% 
  pivot_wider(names_from = round, values_from = contains("frail")) %>% 
  mutate(p_value = fisher.test(rbind(c(frail_1, nonfrail_1), c(frail_2, nonfrail_2)))$p.value)
  #mutate(p_value = chisq.test(rbind(c(frail_1, nonfrail_1), c(frail_2, nonfrail_2)), simulate.p.value = TRUE)$p.value)
  

pA <- participants %>% 
  filter(!duplicated(participant_id), !is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  group_by(part_age_group, part_gender) %>% 
  summarise(n = n(),
            x = sum(frailty),
            p = x/n) %>%
  mutate(p_lower = qqbinom(q = 0.025, x, n),
         p_upper = qqbinom(q = 0.975, x, n)) %>% 
  ggplot(aes(x = part_age_group, y = p, fill = part_gender)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = p_lower, ymax = p_upper),
                position = position_dodge(width = 0.9),
                width = 0.3,
                col = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     limits = c(0, 0.9),
                     breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = color_palette[c(2, 6)]) +
  labs(x = "Participant age group", 
       y = "Fraction frail",
       fill = NULL) +
  theme_light() +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))

ggsave(pA, filename = paste0("./figures/Participants_frail_", today(), ".png"), height = 5, width = 7, dpi = 300)

# # M.A.L.M. van Assen et al. / Archives of Gerontology and Geriatrics 67 (2016) 120–129
# Measuring frailty in Dutch community-dwelling older people: Reference values of the Tilburg Frailty Indicator (TFI)
# TFI <- expand_grid(part_age = c(seq(70, 86, 2), 90),
#                    part_gender = c("Male", "Female")) %>% 
#   mutate(frail =       c(10.8, 13.9, 11.0, 15.6, 11.4, 17.9, 14.5, 20.2, 16.6, 22.1, 17.9, 24.2, 19.8, 28.4, 23.4, 31.6, 28.1, 34.8, 31.2, 43.7),
#          frail_lower = c( 9.6, 12.6,  9.8, 14.2, 10.1, 16.4, 13.0, 18.5, 14.8, 20.3, 15.9, 22.1, 17.3, 26.0, 20.4, 28.7, 24.3, 31.4, 27.8, 41.0),
#          frail_upper = c(11.9, 15.2, 12.3, 17.0, 12.7, 19.5, 16.1, 21.9, 18.4, 24.0, 20.0, 26.2, 22.2, 30.8, 26.4, 34.4, 31.9, 38.1, 34.6, 46.4))

# participants %>% 
#   filter(!duplicated(participant_id), !is.na(frailty)) %>% 
#   group_by(part_age, part_gender) %>% 
#   summarise(frac_frail = sum(frailty)/n(),
#             `frac_non-frail` = sum(!frailty)/n()) %>% 
#   pivot_longer(cols = contains("frail"), names_to = "frailty", values_to = "frac", names_prefix = "frac_") %>% 
#   ggplot(aes(x = part_age, y = frac, fill = frailty %>% fct_rev)) +
#   geom_bar(stat = "identity") +
#   geom_ribbon(data = TFI,
#             aes(x = part_age, ymin = 0.01*frail_lower, ymax = 0.01*frail_upper),
#             inherit.aes = FALSE,
#             alpha = 0.3) +
#   geom_line(data = TFI,
#             aes(x = part_age, y = 0.01*frail),
#             inherit.aes = FALSE) +
#   theme_light() +
#   facet_wrap(facets = vars(part_gender),
#              ncol = 1)
# 
# 
# 
# participants %>% 
#   filter(!duplicated(participant_id), !is.na(frailty)) %>% 
#   group_by(part_age_group, part_gender) %>% 
#   summarise(frac_frail = sum(frailty)/n(),
#             `frac_non-frail` = sum(!frailty)/n()) %>% 
#   pivot_longer(cols = contains("frail"), names_to = "frailty", values_to = "frac", names_prefix = "frac_") %>% 
#   mutate(part_age = case_when(part_age_group == "70-74" ~ 72.5,
#                               part_age_group == "75-79" ~ 77.5,
#                               part_age_group == "80-84" ~ 82.5,
#                               part_age_group == "85-89" ~ 87.5,
#                               part_age_group == "90+" ~ 92.5)) %>% 
#   ggplot(aes(x = part_age, y = frac, fill = frailty %>% fct_rev)) +
#   geom_bar(stat = "identity") +
#   geom_ribbon(data = TFI,
#               aes(x = part_age, ymin = 0.01*frail_lower, ymax = 0.01*frail_upper),
#               inherit.aes = FALSE,
#               alpha = 0.3) +
#   geom_line(data = TFI,
#             aes(x = part_age, y = 0.01*frail),
#             inherit.aes = FALSE) +
#   labs(x = "participant age group", 
#        y = "fraction",
#        fill = NULL) +
#   theme_light() +
#   facet_wrap(facets = vars(part_gender),
#              ncol = 1)


# Fraction living alone

pB <- participants %>% 
  mutate(hh_size = case_when(hh_size == "Not applicable" ~ NA_character_,
                             hh_size == 1 ~ "1",
                             hh_size > 1 ~ "2+",
                             TRUE ~ NA_character_)) %>% 
  filter(!duplicated(participant_id), !is.na(hh_size)) %>% 
  group_by(part_age_group, part_gender) %>% 
  summarise(n = n(),
            x = sum(hh_size == "1")) %>%
  mutate(p_lower = qqbinom(q = 0.025, x, n),
         p_upper = qqbinom(q = 0.975, x, n),
         p = x/n) %>% 
  ggplot(aes(x = part_age_group, y = p, fill = part_gender)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = p_lower, ymax = p_upper),
                position = position_dodge(width = 0.9),
                width = 0.3,
                col = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     limits = c(0, 0.9),
                     breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = color_palette[c(2, 6)]) +
  labs(x = "Participant age group", 
       y = "Fraction living alone",
       fill = NULL) +
  theme_light() +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))


ggsave(pB, filename = paste0("./figures/Participants_householdsize_", today(), ".png"), height = 5, width = 7, dpi = 300)

# Education level by participant age and gender

pC <- participants %>%
  filter(!duplicated(participant_id), !is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  filter(!is.na(education) & education != "Prefer not to say") %>% 
  ggplot(aes(x = part_age_group, fill = education %>% fct_rev)) +
  geom_bar(position = "fill") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = color_palette[c(1,2,4,3)]) +
  labs(x = "Participant age group", 
       y = "Fraction",
       fill = NULL) +
  theme_minimal() +
  theme(#strip.background = element_rect(fill = "white"),
    #strip.text = element_text(colour = "black"),
    #legend.justification = c(1, 0), 
    legend.position = "right") +
  guides(fill = guide_legend(reverse = FALSE)) +
  facet_wrap(facets = vars(part_gender))

ggsave(pC, filename = paste0("./figures/Participants_education_", today(), ".png"), height = 5, width = 8, dpi = 300)


# Vaccination coverage by participant age and round

pD <- participants %>% 
  filter(!duplicated(participant_id), !is.na(vaccination_COVID19), !is.na(vaccination_influenza), !is.na(vaccination_IPD)) %>%
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  group_by(part_age_group, round) %>% 
  summarise(vaccination_COVID19 = sum(vaccination_COVID19),
            vaccination_influenza = sum(vaccination_influenza),
            vaccination_IPD = sum(vaccination_IPD),
            n = n()) %>% 
  pivot_longer(starts_with("vaccination"), names_to = "vaccination", names_prefix = "vaccination_", values_to = "x") %>% 
  mutate(p_lower = qqbinom(q = 0.025, x, n),
         p_upper = qqbinom(q = 0.975, x, n),
         p = x/n,
         vaccination = factor(vaccination, levels = c("COVID19", "influenza", "IPD"), labels = c("COVID-19", "Influenza", "IPD"))) %>% 
  ggplot(aes(x = part_age_group, y = 100*p, group = interaction(round, vaccination), fill = vaccination, col = vaccination, alpha = round)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = 100*p_lower, ymax = 100*p_upper),
                position = position_dodge(width = 0.8),
                width = 0.3,
                col = 1,
                alpha = 1) +
  scale_fill_manual(values = color_palette[c(1,3,5)]) +
  scale_color_manual(values = color_palette[c(1,3,5)]) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(x = "Participant age group",
       y = "Vaccination coverage (%)",
       alpha = "Survey period") +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1,1)) +
  guides(fill = "none",
         color = "none") +
  facet_wrap(facets = vars(vaccination),
             nrow = 1)

ggsave(pD, filename = paste0("./figures/Participants_vaccination_", today(), ".png"), height = 5, width = 8, dpi = 300)





plot_grid(plot_grid(pA, pB, nrow = 1, labels = c("A", "B")),
          pC + theme(),
          pD,
          nrow = 3,
          labels = c("", "C", "D"))

ggsave(filename = paste0("./figures/Participants.png"), height = 10.5, width = 7, dpi = 300, bg = "white")
ggsave(filename = paste0("./figures/Fig1_Participants.pdf"), height = 10.5, width = 7, dpi = 300, bg = "white")
