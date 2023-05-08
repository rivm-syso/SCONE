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
# Analyze contacts
# - number of contacts: compare frail/non-frail and round 1/2
# - number of contacts of average person
# - contact matrices
#
################################################################################

# week sum of number of contacts with non-household members
# with imputed missing data
fit_data <- contacts %>% 
  filter(!hh_member) %>% 
  full_join(participants) %>% 
  filter(!is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  group_by(part_id, participant_id, frailty, part_age, part_gender, round) %>% 
  summarise(n_cont = sum(!is.na(cnt_id))) %>% 
  ungroup %>% 
  bind_rows(missing_data) %>% 
  arrange(participant_id) %>% 
  fill(frailty, part_age, part_gender, round) %>% 
  group_by(participant_id, frailty, part_age, part_gender, round) %>% 
  filter(n() == 7) %>% # only participants with full week of data (partly imputed)
  summarise(n_cont = sum(n_cont)) %>% 
  ungroup


# plot of fit data by round and frailty
ggplot(data = fit_data %>% 
         filter(n_cont < 100) %>%
         mutate(
           n_cont_group = cut(n_cont, 
                              breaks = seq(0, 200, 5),
                              labels = paste0(seq(0, 199, 5), "-",seq(4, 200, 5)),
                              right = FALSE, 
                              include_lowest = TRUE),
           frailty = factor(frailty, levels = c(TRUE, FALSE), labels = c("frail", "non-frail")),
           round = factor(paste("Survey period", round))),
       aes(x = n_cont_group, fill = frailty)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 75),
                     expand = c(0, 0)) +
  labs(x = "number of community contacts per week",
       y = "number of participants",
       fill = NULL) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 270),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        legend.justification = c(1, 1), 
        legend.position = c(0.98, 0.98)) +
  facet_wrap(facets = vars(round),
             ncol = 1,
             labeller = label_context)

ggsave(filename = paste0("./figures/Contacts_barplot_", today(), ".png"), height = 5, width = 7, dpi = 300)


# Fit number of contacts

fit.nb <- gam(n_cont ~ frailty*part_age*part_gender*round,
              family = nb(),
              data = fit_data)

summary(fit.nb)
qq.gam(fit.nb)

ggplot(data = fit_data %>% 
         mutate(pred.fit = predict.gam(fit.nb, newdata = .),
                pred.se = predict.gam(fit.nb, newdata = ., se.fit = TRUE)$se.fit) %>% 
         mutate(pred = exp(pred.fit),
                pred_lower = exp(pred.fit - 1.96*pred.se),
                pred_upper = exp(pred.fit + 1.96*pred.se)) %>% 
         mutate(n_cont = if_else(n_cont == 0, 0.5, as.numeric(n_cont)),
                frailty = factor(frailty, levels = c(TRUE, FALSE), labels = c("Frail", "Non-frail"))),
       aes(x = part_age, col = round, fill = round)) +
  geom_point(aes(y = n_cont),
             pch = 21, alpha = 0.7) +
  #geom_smooth(aes(y = n_cont)) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper),
              alpha = 0.3,
              col = NA) +
  geom_line(aes(y = pred)) +
  scale_y_continuous(trans = "log",
                     breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100, 200),
                     labels = c(0, 1, 2, 5, 10, 20, 50, 100, 200),
                     minor_breaks = NULL,
                     limits = c(-1, 200)) +
  scale_color_manual(values = color_palette[c(2, 4)]) +
  scale_fill_manual(values = color_palette[c(2, 4)]) +
  #scale_size_area(max_size = 2) +
  labs(x = "Participant age",
       y = "Number of community contacts per week",
       fill = "Survey period",
       colour = "Survey period") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        #legend.justification = c(1, 0), 
        legend.position = "bottom") +
  facet_grid(rows = vars(part_gender),
             cols = vars(frailty))

ggsave(filename = paste0("./figures/Contacts_fit.png"), height = 5, width = 7, dpi = 300)
ggsave(filename = paste0("./figures/Fig2_Contacts_fit.pdf"), height = 5, width = 7, dpi = 300)




# Compare rounds and frailty


# frail/nonfrail in round 1 (p-value 0.0848)
anova(gam(n_cont ~ part_age*part_gender,
          family = nb(),
          data = fit_data %>% filter(round == 1)), 
      gam(n_cont ~ part_age*part_gender*frailty,
          family = nb(),
          data = fit_data %>% filter(round == 1)), 
      test = "Chisq")

# frail/nonfrail in round 2 (p-value 0.02335)
anova(gam(n_cont ~ part_age*part_gender,
          family = nb(),
          data = fit_data %>% filter(round == 2)), 
      gam(n_cont ~ part_age*part_gender*frailty,
          family = nb(),
          data = fit_data %>% filter(round == 2)), 
      test = "Chisq")


# round 1/2 for frail (p-value 0.005007)
anova(gam(n_cont ~ part_age*part_gender,
          family = nb(),
          data = fit_data %>% filter(frailty)), 
      gam(n_cont ~ part_age*part_gender*round,
          family = nb(),
          data = fit_data %>% filter(frailty)), 
      test = "Chisq")

# round 1/2 for nonfrail (p-value 1.452e-10)
anova(gam(n_cont ~ part_age*part_gender,
          family = nb(),
          data = fit_data %>% filter(!frailty)), 
      gam(n_cont ~ part_age*part_gender*round,
          family = nb(),
          data = fit_data %>% filter(!frailty)), 
      test = "Chisq")


# No effect of household size on community contacts

fit_data_expanded <- contacts %>% 
  filter(!hh_member) %>% 
  full_join(participants) %>% 
  filter(!is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  mutate(hh_size = case_when(hh_size == "Not applicable" ~ NA_character_,
                             is.na(hh_size) ~ NA_character_,
                             hh_size == 1 ~ "1",
                             hh_size > 1 ~ "2+",
                             TRUE ~ NA_character_)) %>% 
  group_by(part_id, participant_id, frailty, part_age, part_gender, round, hh_size, vaccination_COVID19) %>% 
  summarise(n_cont = sum(!is.na(cnt_id))) %>% 
  ungroup %>% 
  bind_rows(missing_data) %>% 
  arrange(participant_id) %>% 
  fill(frailty, part_age, part_gender, round, hh_size) %>% 
  group_by(participant_id, frailty, part_age, part_gender, round, hh_size, vaccination_COVID19) %>% 
  filter(n() >= 5) %>% 
  summarise(n_cont = sum(n_cont)) %>% 
  ungroup

anova(gam(n_cont ~ part_age*part_gender*frailty*round,
          family = nb(),
          data = fit_data_expanded), 
      gam(n_cont ~ part_age*part_gender*frailty*round*hh_size,
          family = nb(),
          data = fit_data_expanded), 
      test = "Chisq")

# also no effect of COVID-19 vaccination, but unrealistic model estimates
# because most unvaccinated are 70-74 in round 1

anova(gam(n_cont ~ part_age*part_gender*frailty*round*hh_size,
          family = nb(),
          data = fit_data_expanded %>% filter(!is.na(vaccination_COVID19))), 
      gam(n_cont ~ part_age*part_gender*frailty*round*hh_size*vaccination_COVID19,
          family = nb(),
          data = fit_data_expanded), 
      test = "Chisq")

summary(gam(n_cont ~ part_age*part_gender*frailty*round*hh_size*vaccination_COVID19,
            family = nb(),
            data = fit_data_expanded))


# check dispersion coefficient for each stratum

summary(gam(n_cont ~ part_age*part_gender,
              family = nb(),
              data = fit_data %>% filter(frailty, round == 2)))


fit_data %>% 
  filter(n_cont < 100) %>%
  ggplot(aes(x = n_cont, col = interaction(round, frailty))) +
  geom_density() +
  theme_minimal()

################## Average frail and non-frail person of 70+ #############################

# frailty distribution over age and gender of study population
fit.frail.simple <- glm(frailty ~ part_age * part_gender,
                        data = participants %>% 
                          filter(!duplicated(participant_id)) %>% 
                          filter(hh_size != "Not applicable" | is.na(hh_size)),
                        family = binomial())

summary(fit.frail.simple)

# combine with 2021 NL population
average_frail_population <- population %>%
  filter(age >= 70) %>% 
  rename(part_age = age,
         part_age_group = age_group,
         part_gender = gender) %>%
  mutate(prop_frailty = predict(fit.frail.simple, newdata = ., type = "response")) %>%
  mutate(n_TRUE = round(n*prop_frailty),
         n_FALSE = round(n*(1-prop_frailty))) %>%
  dplyr::select(part_age, part_age_group, part_gender, n_TRUE, n_FALSE) %>%
  pivot_longer(starts_with("n_"), names_to = "frailty", values_to = "n", names_prefix = "n_" ) %>%
  mutate(frailty = as.logical(frailty)) %>% 
  group_by(part_gender, frailty) %>% 
  summarise(part_age = sum(n*part_age)/sum(n),
            n = sum(n)) %>% 
  group_by(frailty) %>% 
  mutate(fraction = n/sum(n)) %>% 
  ungroup

# average non-frail person is 49% female of 77.5 and 51% male of 76.3
# average frail person is 68% female of 79.7 and 32% male of 79.9

# predicted number of contacts for average frail/non-frail person in rounds 1/2
pred.data <- bind_rows("1" = average_frail_population,
                       "2" = average_frail_population,
                       .id = "round") %>%
  mutate(pred.fit = predict.gam(fit.nb, newdata = .),
         pred.se = predict.gam(fit.nb, newdata = ., se.fit = TRUE)$se.fit) %>% 
  group_by(round, frailty) %>% 
  summarise(pred.fit = weighted.mean(pred.fit, w = n),
            pred.se = weighted.mean(pred.se, w = n)) %>%
  mutate(pred = exp(pred.fit),
         pred_lower = exp(pred.fit - 1.96*pred.se),
         pred_upper = exp(pred.fit + 1.96*pred.se),
         pred_full = paste0(sprintf("%0.0f", pred), " (", sprintf("%0.0f", pred_lower), " - ", sprintf("%0.0f", pred_upper), ")")) %>% 
  ungroup

# add predicted number of contacts for average person in rounds 1/2
pred.data <- bind_rows(pred.data,
                       bind_rows("1" = average_frail_population,
                                 "2" = average_frail_population,
                                 .id = "round") %>%
                         mutate(pred.fit = predict.gam(fit.nb, newdata = .),
                                pred.se = predict.gam(fit.nb, newdata = ., se.fit = TRUE)$se.fit) %>% 
                         group_by(round) %>% 
                         summarise(pred.fit = weighted.mean(pred.fit, w = n),
                                   pred.se = weighted.mean(pred.se, w = n)) %>%
                         mutate(pred = exp(pred.fit),
                                pred_lower = exp(pred.fit - 1.96*pred.se),
                                pred_upper = exp(pred.fit + 1.96*pred.se),
                                pred_full = paste0(sprintf("%0.0f", pred), " (", sprintf("%0.0f", pred_lower), " - ", sprintf("%0.0f", pred_upper), ")")) %>% 
                         ungroup)

saveRDS(pred.data, file = "./results/predicted_contacts_averageperson.rds")

# check oldest non-frail in round 1 have lowest number of contacts
expand.grid(round = c(1,2),
            frailty = c(TRUE, FALSE),
            part_gender = c("Male", "Female"),
            part_age = c(70, 90)) %>% 
  mutate(pred.fit = predict.gam(fit.nb, newdata = .),
         pred.se = predict.gam(fit.nb, newdata = ., se.fit = TRUE)$se.fit) %>% 
  mutate(pred = exp(pred.fit),
         pred_lower = exp(pred.fit - 1.96*pred.se),
         pred_upper = exp(pred.fit + 1.96*pred.se)) %>% 
  arrange(pred)


# contact difference by bootstrapping (many bootstraps to get a stable median)

pred.data %>% 
  filter(!is.na(frailty)) %>% 
  group_by(frailty) %>% 
  summarise(probs = c(0.025, 0.5, 0.975),
            diff = quantile(exp(rnorm(100000, mean = pred.fit[1], sd = pred.se[1])) - exp(rnorm(100000, mean = pred.fit[2], sd = pred.se[2])), probs = probs))


pred.data %>% 
  filter(!is.na(frailty)) %>% 
  group_by(round) %>% 
  summarise(probs = c(0.025, 0.5, 0.975),
            diff = quantile(exp(rnorm(100000, mean = pred.fit[1], sd = pred.se[1])) - exp(rnorm(100000, mean = pred.fit[2], sd = pred.se[2])), probs = probs))


# contact reduction by bootstrapping (many bootstraps to get a stable median)

pred.data %>% 
  filter(!is.na(frailty)) %>% 
  group_by(round) %>% 
  summarise(probs = c(0.025, 0.5, 0.975),
            reduction = quantile(1-exp(rnorm(100000, mean = pred.fit[2], sd = pred.se[2]))/exp(rnorm(100000, mean = pred.fit[1], sd = pred.se[1])), probs = probs))

pred.data %>% 
  filter(!is.na(frailty)) %>% 
  group_by(frailty) %>% 
  summarise(probs = c(0.025, 0.5, 0.975),
            reduction = quantile(1-exp(rnorm(100000, mean = pred.fit[1], sd = pred.se[1]))/exp(rnorm(100000, mean = pred.fit[2], sd = pred.se[2])), probs = probs))



######################## Contact matrices #########################


# analyse contacts by part_age_group and cnt_age_group (and frailty and round)
contact_data <- contacts %>% 
  right_join(participants %>% 
              filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
              select(-frailty) %>% 
              # sample unknown frailty from frailty distribution of same part_age_group and round
              full_join(participants %>% 
                          filter(!duplicated(participant_id)) %>% 
                          filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
                          group_by(part_age_group, round) %>% 
                          mutate(frailty = if_else(is.na(frailty), sample(frailty[!is.na(frailty)], size = n(), replace = TRUE), frailty),
                                 frailty = factor(frailty)) %>% 
                          select(participant_id, frailty))) %>%
  # sample unknown cnt_age from cnt_age_group distribution of same part_age_group, frailty and round
  group_by(part_age_group, round, frailty, .drop = FALSE) %>% 
  mutate(cnt_age_group = if_else(is.na(cnt_age_group), sample(cnt_age_group[!is.na(cnt_age_group)], size = n(), replace = TRUE), cnt_age_group),
         n_part = length(unique(participant_id))) %>% 
  group_by(part_age_group, cnt_age_group, round, frailty, n_part, .drop = FALSE) %>% 
  summarise(n_cont = sum(!is.na(cnt_id))) %>% 
  # no contacts in one part_age_group and cnt_age_group combination, retained by .drop = FALSE but needs n_part
  group_by(part_age_group, round, frailty, .drop = FALSE) %>% 
  fill(n_part) %>% 
  # reset frailty to boolean (set to factor to use .drop = FALSE above)
  mutate(frailty = as.logical(frailty),
         m = n_cont/n_part) %>% 
  ungroup %>% 
  # join population distribution to normalize cnt_age_group
  full_join(population %>% 
              rename(cnt_age_group = age_group) %>% 
              group_by(cnt_age_group) %>% 
              summarise(n = sum(n)) %>% 
              mutate(n = n/mean(n))) %>% 
  mutate(c_obs = m/n)


# check 720 participants
contact_data %>% 
  group_by(part_age_group, round, frailty) %>% 
  filter(!duplicated(part_age_group)) %>% 
  pull(n_part) %>% sum

# check 16505 contacts
contact_data %>% pull(n_cont) %>% sum


# smooth symmetric matrix by averaging 
contact_data <- contact_data %>%
  left_join(contact_data %>% 
              transmute(round = round,
                        frailty = frailty,
                        tmp = part_age_group,
                        part_age_group = cnt_age_group,
                        cnt_age_group = tmp,
                        c_obs2 = c_obs) %>% 
              select(-tmp)) %>% 
  mutate(c_smt = if_else(is.na(c_obs2), c_obs, (c_obs + c_obs2)/2))



log10max <- log10(max(contact_data$c_smt, na.rm = TRUE))
log10min <- log10(min(contact_data$c_smt, na.rm = TRUE))

ggplot(
    data = contact_data %>% 
      mutate(frailty = factor(frailty, levels = c(TRUE, FALSE), labels = c("Frail participant", "Non-frail participant")),
             round = factor(round, labels = c("Survey period 1", "Survey period 2"))),
    mapping = aes(x = part_age_group, y = cnt_age_group, fill = c_smt)) +
  geom_tile() +
  scale_fill_viridis_c(
    limits = c(0.99, 1.01)*10^c(log10min, log10max),
    breaks = c(0, outer(c(1, 2, 5), 10^(floor(log10min):ceiling(log10max)))),
    # na.value = "#450256",
    na.value = "white",
    trans = "log",
    direction = 1) +
  coord_equal() +
  labs(
    x = "Participant age group",
    y = "Contact age group",
    fill = "Contact rate") +
  theme_minimal() +
  theme(
    #legend.position = "none",
    panel.grid = element_blank(),
    legend.key.height = unit(0.09, "npc"),
    axis.text.x = element_text(
      angle = -90,
      hjust = 0,
      vjust = 0.5)) +
    facet_grid(rows = vars(frailty),
               cols = vars(round))

ggsave(filename = paste0("./figures/Contacts_matrices.png"), bg = "white", height = 7, width = 5, dpi = 300)
ggsave(filename = paste0("./figures/Fig3_Contacts_matrices.pdf"), bg = "white", height = 7, width = 5, dpi = 300)


############# Check contacts of nursing home and migrant participants ##########


tmp_data <- contacts %>% 
  filter(!hh_member) %>% 
  full_join(participants) %>% 
  filter(!is.na(frailty)) %>% 
  mutate(cob = if_else(country_of_birth == "Netherlands", "NL", "other"),
         hh_size = case_when(hh_size == "Not applicable" ~ "Not applicable",
                             is.na(hh_size) ~ NA_character_,
                             hh_size == 1 ~ "1",
                             hh_size > 1 ~ "2+",
                             TRUE ~ NA_character_)) %>%
  group_by(participant_id, frailty, part_gender, part_age, part_age_group, round, hh_size, cob) %>% 
  # only participants that participated at least 5 days
  # their total number of contacts corrected to mimic 7 days participation
  filter(length(unique(survey_day)) >= 5) %>% 
  summarise(n_day = length(unique(survey_day)),
            n_cont = sum(!is.na(cnt_id))*7/n_day) %>% 
  ungroup


tmp_data %>% 
  filter(hh_size == "Not applicable") %>% 
  mutate(pred.fit = predict.gam(fit.nb, newdata = .),
         pred.se = predict.gam(fit.nb, newdata = ., se.fit = TRUE)$se.fit) %>% 
  mutate(pred = exp(pred.fit),
         pred_lower = exp(pred.fit - 1.96*pred.se),
         pred_upper = exp(pred.fit + 1.96*pred.se)) %>% 
  ungroup %>% 
  summarise(n_cont = mean(n_cont),
            pred = mean(pred),
            pred_lower = mean(pred_lower),
            pred_upper = mean(pred_upper)) 

tmp_data %>% 
  filter(cob == "other") %>% 
  mutate(pred.fit = predict.gam(fit.nb, newdata = .),
         pred.se = predict.gam(fit.nb, newdata = ., se.fit = TRUE)$se.fit) %>% 
  mutate(pred = exp(pred.fit),
         pred_lower = exp(pred.fit - 1.96*pred.se),
         pred_upper = exp(pred.fit + 1.96*pred.se)) %>% 
  ungroup %>% 
  summarise(n_cont = mean(n_cont),
            pred = mean(pred),
            pred_lower = mean(pred_lower),
            pred_upper = mean(pred_upper)) 


