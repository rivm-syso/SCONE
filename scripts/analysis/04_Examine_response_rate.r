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
# You should have included a copy of the GNU General Public License along with 
# this program.  If not, see <https://www.gnu.org/licenses/>.”
#
################################################################################
#
# Overview tables
#
################################################################################

tmp <- participants %>% 
  filter(!duplicated(participant_id)) %>% 
  group_by(part_age_group, part_gender, round) %>% 
  count(name = "included") %>% 
  full_join(invited %>% rename(part_age_group = age_group,
                               part_gender = sex,
                               invited = n)) %>% 
  ungroup

tmp %>% 
  #group_by(round, part_age_group) %>% 
  #group_by(round, part_gender) %>% 
  #group_by(part_age_group, part_gender) %>% 
  #group_by(part_gender) %>% 
  #group_by(part_age_group) %>%
  #group_by(round) %>% 
  summarise(target = sum(target),
            invited = sum(invited),
            included = sum(included)) %>% 
  mutate(perc_response = round(100*included/invited))


response_tbl <- tmp %>% 
  #mutate(round = as.character(round)) %>% 
  bind_rows(., tibble(.) %>% 
              group_by(round, part_age_group) %>% 
              summarise(part_gender = "Total",
                        target = sum(target),
                        invited = sum(invited),
                        included = sum(included))
  ) %>%  
  bind_rows(., tibble(.) %>% 
              group_by(round, part_gender) %>% 
              summarise(part_age_group = "total",
                        target = sum(target),
                        invited = sum(invited),
                        included = sum(included))
  ) %>% 
  bind_rows(., tibble(.) %>% 
              group_by(part_gender) %>% 
              summarise(round = "total",
                        target = sum((part_age_group != "total")*target),
                        invited = sum((part_age_group != "total")*invited),
                        included = sum((part_age_group != "total")*included),
                        part_age_group = "total")
  ) %>% 
  mutate(response = round(100*included/invited)) %>%
  arrange(round, part_age_group) %>%
  select(round, part_age_group, part_gender, invited, included, response) %>%
  ungroup %>% 
  pivot_wider(names_from = part_gender, 
              values_from = c("invited", "included", "response"),
              names_glue = "{part_gender}_{.value}", 
              names_vary = "slowest") 


saveRDS(response_tbl, "./results/response_table.rds")



tmp2 <- tmp %>% 
  filter(round == 1) %>% 
  mutate(design_response = 100*included/invited,
         round = factor(2, levels = c(1,2))) %>% 
  select(part_age_group, part_gender, round, design_response) %>% 
  right_join(tmp) %>% 
  mutate(design_response = if_else(round == 1, 100*50/target, design_response),
         response = 100*included/invited,
         design_included = round(0.01*design_response*target)) %>% 
  rename(observed_included = included,
         observed_response = response) %>% 
  pivot_longer(cols = grep(names(.), pattern = "observed|design"),
               names_to = c("type", ".value"),
               names_sep = "_")  %>% 
  pivot_longer(cols = c("response", "included"),
               names_to = "what")

ggplot(data = tmp2 %>% 
         filter(round == 2) %>% 
         mutate(what = factor(what, levels = c("response", "included"), labels = c("response rate (%)", "number included"))),
       aes(x = part_age_group,
           y = value,
           fill = part_gender)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_manual(values = color_palette[c(2, 6)]) +
  labs(x = "Participant age group",
       y = NULL,
       fill = "Participant\ngender") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        strip.placement = "outside") +
  facet_grid(cols = vars(type),
             rows = vars(what),
             scale = "free_y",
             switch = "y")

ggsave(filename = paste0("./figures/Sampling_scheme_2.png"), height = 5, width = 8, dpi = 300, bg = "white")


  