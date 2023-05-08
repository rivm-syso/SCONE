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
# Analyze fatigue effect
# - plot participant day against day of week
# - fit gam to disentangle day of week and fatigue effect
# - estimate number of contacts for 5- and 6-day participants
#
################################################################################

# all participants participating 5 or more days
fit_fatigue_data <- contacts %>% 
  filter(!hh_member) %>% 
  full_join(participants) %>% 
  filter(!is.na(frailty)) %>% 
  filter(hh_size != "Not applicable" | is.na(hh_size)) %>% 
  group_by(part_id, participant_id, survey_date, survey_day, round) %>% 
  summarise(n_cont = sum(!is.na(cnt_id))) %>% 
  group_by(participant_id) %>% 
  filter(n() >= 5) %>% 
  mutate(survey_day = factor(survey_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         participant_day = rank(survey_date)) %>% 
  #filter(part_round != as.integer(survey_day)) %>% 
  ungroup

# plot participation day against day of week: majority of participants start on Monday


fit_fatigue_data %>% filter(participant_day == 1) %>% group_by(survey_day) %>% count %>% ungroup %>% mutate(frac = n/sum(n))

fit_fatigue_data %>% 
  group_by(participant_day, survey_day) %>% 
  count %>% 
  ggplot(aes(x = participant_day, y = survey_day, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n, col = n < 500)) +
  coord_equal() +
  scale_x_continuous(breaks = 1:7) +
  scale_fill_viridis(trans = "log",
                     breaks = c(10, 20, 50, 100, 200, 500)) +
  scale_color_manual(values = c("black", "white")) +
  labs(x = "Participation day", 
       y = "Day of week",
       fill = "Number of\nresponses") +
  theme_minimal() +
  guides(col = guide_none())


ggsave(filename = paste0("./figures/Participant_day_of_week.png"), bg = "white", height = 5, width = 7, dpi = 300)



# fit data to disentangle fatigue and day of week effect
# all other participant characteristics included in random effect per participant 
# (also checked whether fatigue effect differs per round (using participant_day*round): no)

fit.fatigue <- gam(n_cont ~ survey_day + participant_day + s(participant_id, bs = "re"),
                   family = nb(),
                   data = fit_fatigue_data)

summary(fit.fatigue)
qq.gam(fit.fatigue)

# make forest plot of results

res <- confint(fit.fatigue, level = 0.95) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(OR = exp(Estimate),
         OR_lower = exp(`2.5%`),
         OR_upper = exp(`97.5%`),
         term = gsub(x = term, pattern = "survey_day", replacement = ""),
         term = if_else(term == "participant_day", "Participation day\n(per day)", term)) %>% 
  add_row(term = "Monday (ref)", OR = 1, OR_lower = 1, OR_upper = 1, .before = 1) %>% 
  mutate(term = factor(term,
                       levels = c("Participation day\n(per day)", "Monday (ref)", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>% fct_rev)


ggplot(data = res,
       aes(x = term, y = OR, ymin = OR_lower, ymax = OR_upper)) +
  geom_pointrange() + 
  geom_hline(yintercept = 1, lty = 2) + 
  geom_text(aes(x = as.integer(term),
                y = 1.2,
                label = paste0(sprintf("%0.2f", round(OR, digits = 2)), " (",
                               sprintf("%0.2f", round(OR_lower, digits = 2)), " - ",
                               sprintf("%0.2f", round(OR_upper, digits = 2)), ")")),
            size = 3.5,
            hjust = "right",
            nudge_x = 0.2) + ## decimal places
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(x = NULL,
       y = "RR (95% CI)") +
  theme_light() 

ggsave(filename = paste0("./figures/Fatigue_forestplot.png"), height = 5, width = 7, dpi = 300)

# How large is fatigue effect over the course of a week?

observed <- fit_fatigue_data %>% 
  group_by(participant_id) %>% 
  summarise(n_cont = sum(n_cont)) %>% 
  summarise(mean(n_cont))

corrected <- fit_fatigue_data %>% 
  mutate(participant_day = 1) %>% 
  mutate(n_cont = exp(predict.gam(fit.fatigue, newdata = .))) %>% 
  group_by(participant_id) %>% 
  summarise(n_cont = sum(n_cont)) %>% 
  summarise(mean(n_cont))

# 13% additional contacts if fatigue effect is corrected for
corrected/observed
7/sum(outer(0.96, 0:6, "^"))


# determine number of contacts for participants who participated 5 or 6 days for the missing days
# taking fatigue and day of week effect into account (assuming start on Monday)

missing_data <- fit_fatigue_data %>% 
  full_join(expand.grid(participant_id = unique(fit_fatigue_data$participant_id),
                        survey_day = factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))) %>% 
  filter(is.na(part_id)) %>% 
  mutate(participant_day = as.integer(survey_day)) %>% 
  mutate(n_cont = exp(predict.gam(fit.fatigue, newdata = .)))

