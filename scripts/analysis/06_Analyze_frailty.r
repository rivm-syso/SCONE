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
# Analysis of frailty in study population
# - glm to study covariates of frailty
# - part_gender*part_age most important: will be used in analyzing contacts
#
################################################################################

# participant data to fit frailty
fitdata_part <- participants %>% 
  filter(!duplicated(participant_id)) %>% 
  mutate(hh_size = case_when(hh_size == "Not applicable" ~ NA_character_,
                             is.na(hh_size) ~ NA_character_,
                             hh_size == 1 ~ "1",
                             hh_size > 1 ~ "2+",
                             TRUE ~ NA_character_)) 


# fit frailty
fit.frail <- glm(frailty ~ part_age*part_gender + country_of_birth + education + hh_size + round,
                 data = fitdata_part,
                 family = binomial())

# same model with part_gender Male as first level to isolate interaction term
fit.frail.M <- glm(frailty ~ part_age*part_gender + country_of_birth + education + hh_size + round,
                 data = fitdata_part %>% mutate(part_gender = factor(part_gender, levels = c("Male", "Female"))),
                 family = binomial())

summary(fit.frail)
summary(fit.frail.M)

# mean effect of part_gender
fit.ems <- confint(emmeans(fit.frail.M, pairwise ~ part_gender, by = "part_age"))$contrasts


# construct forest plot

res <- bind_rows(
  full_join(as_tibble(summary(fit.frail)$coefficients, rownames = "var"),
            as_tibble(confint(fit.frail, level = 0.95), rownames = "var")) %>% 
    filter(!(var %in% c("part_genderMale", "part_age:part_genderMale", "(Intercept)"))),
  full_join(as_tibble(summary(fit.frail.M)$coefficients, rownames = "var"),
            as_tibble(confint(fit.frail.M, level = 0.95), rownames = "var")) %>% 
    filter(var == "part_age") %>% 
    mutate(var = "part_ageM"),
  tibble(var = paste("Male (vs female) at age", sprintf("%0.1f", round(fit.ems$part_age, digits = 2))),
         Estimate = fit.ems$estimate,
         `2.5 %` = fit.ems$asymp.LCL,
         `97.5 %` = fit.ems$asymp.UCL)
) %>% 
  mutate(OR = exp(Estimate),
         OR_lower = exp(`2.5 %`),
         OR_upper = exp(`97.5 %`))


res <- res %>% 
  mutate(var = factor(var,
                      levels = c("part_age", 
                                 "part_ageM", 
                                 "Male (vs female) at age 81.7", 
                                 "hh_size2+", 
                                 "country_of_birthOther", 
                                 "educationSecondary education (3 or 4 years)",
                                 "educationSecondary education (5 or 6 years)",
                                 "educationHigher education",
                                 "round2"),
                      labels = c("Age of female participant (per year)",
                                 "Age of male participant (per year)",
                                 "Male (vs female) at age 81.7", 
                                 "Household size 2+ (vs 1)", 
                                 "Other country of birth (vs Netherlands)", 
                                 "Secondary education (3 or 4 years)\n(vs Primary or no education)",
                                 "Secondary education (5 or 6 years)\n(vs Primary or no education)",
                                 "Higher education\n(vs Primary or no education)",
                                 "Survey period 2 (vs 1)")) %>% 
          fct_rev())

ggplot(data = res,
       aes(x = var, y = OR, ymin = OR_lower, ymax = OR_upper)) +
  geom_pointrange() + 
  geom_hline(yintercept = 1, lty = 2) + 
  geom_text(aes(x = as.integer(var), 
                y = 2.5, 
                label = paste0(sprintf("%0.2f", round(OR, digits = 2)), " (",
                              sprintf("%0.2f", round(OR_lower, digits = 2)), " - ",
                              sprintf("%0.2f", round(OR_upper, digits = 2)), ")")), 
            size = 3.5,
            hjust = "right",
            nudge_x = 0.2) + ## decimal places
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(x = NULL,
       y = "OR (95% CI)",
       subtitle = "Multiple logistic regression model for frailty") +
  theme_light() 

ggsave(filename = paste0("./figures/Frailty_forestplot.png"), height = 5, width = 7, dpi = 300)


# package emmeans to average out interactions
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

tmp <- emtrends(fit.frail, pairwise ~ part_gender, var = "part_age")
as_tibble(tmp$emtrends)
emmip(fit.frail, part_gender ~ part_age, cov.reduce = range)


tmp <- emmeans(fit.frail, pairwise ~ part_gender | part_age)
as_tibble(tmp$contrasts)

emmeans(fit.frail, pairwise ~ part_gender, by = "part_age")

# at specific age
emmeans(fit.frail, pairwise ~ part_gender | part_age, at = list(part_age = 90))

# all ages
emmeans(fit.frail, pairwise ~ part_gender | part_age, cov.reduce = FALSE)




