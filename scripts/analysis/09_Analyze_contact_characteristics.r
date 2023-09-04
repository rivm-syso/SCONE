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
# Analyze contact characteristics (compare frail/non-frail and round 1/2)
# - risk type of contact: physical, duration, protection, distance
# - repeated contacts
# - location
#
################################################################################

###### Risk type of community contacts #########################################

contacts %>% 
  filter(!hh_member) %>% 
  full_join(participants) %>% 
  # only real contacts
  filter(!is.na(cnt_id)) %>% 
  filter(!is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  mutate(risk_phys = as.integer(phys_contact),
         risk_dist = as.integer(!dist_contact),
         risk_time = as.integer(over15min_contact),
         risk_prot = as.integer(!prot_contact)) %>% 
  pivot_longer(cols = starts_with("risk"), names_to = "type", names_prefix = "risk_", values_to = "risk_score") %>% 
  group_by(participant_id, frailty, round, type) %>% 
  summarise(risk_score = mean(risk_score, na.rm = TRUE)) %>% 
  mutate(type = factor(type, 
                       levels = c("prot", "dist", "time", "phys", "total"), 
                       labels = c("without protection", "closer than 1.5 m", "lasting over 15 min", "with physical contact", "total high risk")),
         frailty = factor(frailty, levels = c(TRUE, FALSE))) %>% 
  ggplot(aes(x = interaction(frailty, round), y = risk_score, fill = round, alpha = frailty)) +
  geom_boxplot(outlier.shape = NA,
               width = 0.6,
               coef = NULL) + # extend whiskers to min and max value
  geom_signif(comparisons = list(c("TRUE.2", "FALSE.2"),
                                 c("TRUE.2", "TRUE.1"),
                                 c("TRUE.1", "FALSE.1"),
                                 c("FALSE.2", "FALSE.1")),
              col = 1,
              alpha = 1,
              y_position = c(1.01, 1.07, 1.01, 1.14),
              #extend_line = -0.01,
              vjust = 0.5,
              tip_length = 0.01,
              angle = 0.001,
              textsize = 3,
              map_signif_level = c("   ***" = 0.001, "  **" = 0.01, "  *" = 0.05, "  ns" = 1)
  ) +
  scale_x_discrete(breaks = c("TRUE.1", "FALSE.1", "TRUE.2", "FALSE.2"),
                   labels = c("Frail", "Non-frail", "Frail", "Non-frail")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06)),
                     breaks = seq(0, 1, 0.25)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = color_palette[c(1,3)]) +
  coord_flip() +
  labs(x = "Participants",
       y = "Fraction of community contacts per participant",
       fill = "Survey period") +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = 1),
        legend.position = "bottom",
        axis.text.x = element_text(size = 8)) +
  guides(alpha = guide_none()) +
  facet_wrap(facets = vars(type),
             ncol = 1)


ggsave(filename = paste0("./figures/Contacts_type.png"), height = 7, width = 7, dpi = 300)
ggsave(filename = paste0("./figures/Fig4_Contacts_type_R2.pdf"), height = 7, width = 7, dpi = 300)


###### Contact locations #######################################################

# per participant as fraction of all community contacts per week
# count multiple locations as 1/multiple

contacts %>% 
  filter(!hh_member) %>% 
  mutate(n_loc = rowSums(across(starts_with("cont_")))) %>% 
  filter(n_loc > 0) %>% 
  select(participant_id, n_loc, starts_with("cont_")) %>% 
  group_by(participant_id) %>% 
  mutate(n_cont = n()) %>% 
  pivot_longer(cols = starts_with("cont_"), names_to = "location", names_prefix = "cont_") %>% 
  mutate(value = as.integer(value)/n_loc) %>% 
  group_by(participant_id, location) %>% 
  summarise(frac = sum(value)/mean(n_cont)) %>% 
  left_join(participants %>% filter(!duplicated(participant_id)) %>% select(participant_id, round, frailty, hh_size)) %>% 
  filter(!is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  group_by(location, round, frailty) %>%
  summarise(frac = mean(frac)) %>% 
  mutate(frailty = factor(frailty, levels = c(TRUE, FALSE), labels = c("Frail", "Non-frail")),
         #round = paste("Survey period ", round),
         location = factor(location,
                           levels = c("home", "otherhome", "outside", "leisure", "shop", "work", "transport", "otherplace"),
                           labels = c("home", "home of contact", "outside", "leisure", "shop", "(volunteering) work", "transport", "other"))) %>% 
  #filter(round == "round 1") %>% 
  ggplot(aes(x = round, y = frac, fill = location %>% fct_rev)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("#786289", "#CD9FAA", color_palette)) +
  labs(x = "Survey period",
       y = "Fraction of community contacts per participant",
       fill = "Location") +
  theme_minimal() +
  facet_wrap(facets = vars(frailty))

ggsave(filename = paste0("./figures/Contacts_location.png"), bg = "white", height = 5, width = 7, dpi = 300)
ggsave(filename = paste0("./figures/Fig5_Contacts_location.pdf"), bg = "white", height = 5, width = 7, dpi = 300)



###### Repeated community contacts #############################################


# comparing frailty status and restriction level
pA <- contacts %>% 
  filter(!hh_member) %>% 
  group_by(participant_id) %>% 
  summarise(n_cont = n(),
            n_unique = length(unique(contact_id)),
            frac = (n_cont - n_unique)/n_cont) %>% 
  left_join(participants %>% filter(!duplicated(participant_id)) %>% select(participant_id, round, frailty, hh_size)) %>% 
  filter(!is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  mutate(frailty = factor(frailty, levels = c(TRUE, FALSE))) %>% 
  ggplot(aes(x = interaction(frailty, round), y = frac, fill = round, alpha = frailty)) +
  geom_boxplot(outlier.shape = NA,
               width = 0.6,
               coef = NULL) + # extend whiskers to min and max value
  geom_signif(comparisons = list(c("TRUE.2", "FALSE.2"),
                                 c("TRUE.2", "TRUE.1"),
                                 c("TRUE.1", "FALSE.1"),
                                 c("FALSE.2", "FALSE.1")),
              col = 1,
              alpha = 1,
              y_position = c(1.01, 1.07, 1.01, 1.14) - 0.15,
              #extend_line = -0.01,
              vjust = 0.5,
              tip_length = 0.01,
              angle = 0.001,
              textsize = 3,
              map_signif_level = c("   ***" = 0.001, "  **" = 0.01, "  *" = 0.05, "  ns" = 1)
  ) +
  scale_x_discrete(breaks = c("TRUE.1", "FALSE.1", "TRUE.2", "FALSE.2"),
                   labels = c("Frail", "Non-frail", "Frail", "Non-frail")) +
  scale_y_continuous(limits = c(0, 1), 
                     expand = expansion(mult = c(0, 0)),
                     breaks = seq(0, 0.99, 0.25)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = color_palette[c(1,3)]) +
  coord_flip() +
  labs(x = "Participants",
       y = "Fraction of repeated community contacts per participant",
       fill = "Survey period") +
  theme_light() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8)) +
  guides(alpha = guide_none())


# comparing living situation and restriction level
pB <- contacts %>% 
  filter(!hh_member) %>% 
  group_by(participant_id) %>% 
  summarise(n_cont = n(),
            n_unique = length(unique(contact_id)),
            frac = (n_cont - n_unique)/n_cont) %>% 
  left_join(participants %>% filter(!duplicated(participant_id)) %>% select(participant_id, round, hh_size)) %>% 
  mutate(hh_size = case_when(hh_size == "Not applicable" ~ NA,
                             is.na(hh_size) ~ NA,
                             hh_size == 1 ~ TRUE,
                             hh_size > 1 ~ FALSE,
                             TRUE ~ NA),
         hh_size = factor(hh_size, levels = c("TRUE", "FALSE"))) %>% 
  filter(!is.na(hh_size)) %>% 
  ggplot(aes(x = interaction(hh_size, round), y = frac, fill = round, alpha = hh_size)) +
  geom_boxplot(outlier.shape = NA,
               width = 0.6,
               coef = NULL) + # extend whiskers to min and max value
  geom_signif(comparisons = list(c("TRUE.2", "FALSE.2"),
                                 c("TRUE.2", "TRUE.1"),
                                 c("TRUE.1", "FALSE.1"),
                                 c("FALSE.2", "FALSE.1")),
              col = 1,
              alpha = 1,
              y_position = c(1.01, 1.07, 1.01, 1.14) - 0.15,
              #extend_line = -0.01,
              vjust = 0.5,
              tip_length = 0.01,
              angle = 0.001,
              textsize = 3,
              map_signif_level = c("   ***" = 0.001, "  **" = 0.01, "  *" = 0.05, "  ns" = 1)
  ) +
  scale_x_discrete(breaks = c("TRUE.1", "FALSE.1", "TRUE.2", "FALSE.2"),
                   labels = c("Living alone", "Living together", "Living alone", "Living together")) +
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0)),
                     breaks = seq(0, 0.99, 0.25)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = color_palette[c(1,3)]) +
  coord_flip() +
  labs(x = "Participants",
       y = "Fraction of repeated community contacts per participant",
       fill = "Survey period   ") +
  theme_light() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8)) +
  guides(alpha = guide_none())


plot_grid(pA + labs(y = NULL) + guides(fill = guide_none()), 
          pB ,
          align = "v",
          nrow = 2,
          labels = c("A", "B"),
          rel_heights = c(4, 5))



ggsave(filename = paste0("./figures/Contacts_repeated.png"), height = 5, width = 7, dpi = 300)


