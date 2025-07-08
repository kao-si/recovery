
# Lack of Autonomy and Spontaneous Recovery of Hedonic Response

# Load packages
library(tidyverse)
library(patchwork)
library(modelsummary)
library(lme4)
library(lmerTest)

# Study 1 ####

# Load data
s1 <- haven::read_sav("Data_Study1_ForcedChoice.sav") %>%
filter(failed_ins == 0)

# Summary statistics of gender and age
table(s1$sex)

psych::describe(s1$age)

# Correlation between the two recovery items
cor.test(s1$recover1, s1$recover2)

# Variable construction
s1 <- s1 %>%
mutate(
    # treatment condition
    treat = case_when(
        item_chosen == item_eaten ~ "Free Choice",
        item_chosen != item_eaten ~ "Forced Choice"
    ),
    # spontaneous recovery
    recover = rowMeans(select(., recover1, recover2), na.rm = TRUE)
)

# Factor variables
s1$item_chosen <- factor(s1$item_chosen, levels = c(1, 2),
labels = c("Alphabet Cookies", "Animal Cookies"))

s1$item_eaten <- factor(s1$item_eaten, levels = c(1, 2),
labels = c("Alphabet Cookies", "Animal Cookies"))

s1$treat <- factor(s1$treat, levels = c("Free Choice", "Forced Choice"))

# Treatment distribution
table(s1$treat)

## Figure 1 ====

# Construct data frame for plotting
s1_mean <- s1 %>%
pivot_longer(
    cols = c(enjoy1:enjoy8, recover),
    names_to = "time",
    values_to = "rating"
) %>%
group_by(treat, time) %>%
summarize(
    mean = mean(rating, na.rm = TRUE),
    se = plotrix::std.error(rating, na.rm = TRUE)
) %>%
ungroup()

# Figure 1A: Trajectory of enjoyment ratings
fig1_a <- s1_mean %>%
filter(time != "recover") %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
scale_y_continuous(limits = c(2.5, 5.7),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Sequence",
    y = "Average Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
)

# Figure 1B: Spontaneous recovery
fig1_b <- s1_mean %>%
filter(time %in% c("enjoy8", "recover")) %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = c("T8", "Recovery")) +
scale_y_continuous(limits = c(2.5, 5.7),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Sequence",
    y = "Average Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
)

# Combine Figures 1A and 1B
fig1 <- fig1_a + fig1_b +
plot_annotation(
    tag_levels = "A"
) &
theme(
    plot.title = element_text(size = 14)
)

# Export figure
ggsave(plot = fig1, filename = "_figures/fig1.png",
width = 18, height = 9)

## Table 1 ====

# Model 1, baseline model
s1_lm1 <- lm(recover ~ treat + enjoy8, s1)

summary(s1_lm1)

# Model 2, controlling for item_chosen
s1_lm2 <- lm(recover ~ treat + enjoy8 + item_chosen, s1)

# Model 3, controlling for item_eaten
s1_lm3 <- lm(recover ~ treat + enjoy8 + item_eaten, s1)

# Table 1
modelsummary(
    list(s1_lm1, s1_lm2, s1_lm3),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "^R2$|AIC|BIC|Log.Lik.|F|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "treatForced Choice" = "Forced Choice",
        "enjoy8" = "Previous Enjoyment",
        "item_chosenAnimal Cookies" = "Item Chosen",
        "item_eatenAnimal Cookies" = "Item Eaten"
    ),
    title = "Table 1. Results of Study 1",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

## Other Analyses ====

# T1 & T8 enjoyment ratings
psych::describeBy(list(T1 = s1$enjoy1, T8 = s1$enjoy8), group = s1$treat)

t.test(enjoy1 ~ treat, s1, var.equal = TRUE)

t.test(enjoy8 ~ treat, s1, var.equal = TRUE)

# Investigate if there is a difference in trajectory of enjoyment ratings
# between the two conditions

# Pivot longer
s1_long <- s1 %>%
    pivot_longer(
        cols = c(enjoy1:enjoy8),
        names_to = "time",
        values_to = "rating"
    ) %>%
    mutate(time = as.numeric(str_extract(time, "\\d+")))

# Mixed model

# All ratings
s1_lmx1 <- lmer(rating ~ time * treat + (1 | id), data = s1_long)

summary(s1_lmx1)

# Ratings after T4
s1_lmx2 <- lmer(rating ~ time * treat + (1 | id),
data = filter(s1_long, time > 4))

summary(s1_lmx2)

# Table A1
modelsummary(
    list(
        "(1)\nAll Ratings" = s1_lmx1,
        "(2)\nRatings after T4" = s1_lmx2
    ),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "AIC|BIC|Log.Lik.|F|RMSE|ICC",
    coef_map = c(
        "(Intercept)" = "Constant",
        "time" = "Rating Sequence",
        "treatForced Choice" = "Forced Choice",
        "time:treatForced Choice" = "Interaction"
    ),
    title = "Table A1. Study 1",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

# Study 2 ####

# Load data
s2 <- haven::read_sav("Data_Study2_SubChoice.sav") %>% filter(failed_ins == 0)

# Summary statistics of gender and age
table(s2$male)

psych::describe(s2$age)

# Correlation between the two recovery items
cor.test(s2$recover1, s2$recover2)

# Correlation between the two items of perceived lack of autonomy
cor.test(s2$autnm1, s2$autnm2)

# Variable construction
s2 <- s2 %>%
mutate(
    # spontaneous recovery
    recover = rowMeans(select(., recover1, recover2), na.rm = TRUE),
    # perceived lack of autonomy
    lack_autnm = rowMeans(select(., autnm1, autnm2), na.rm = TRUE),
    # time elapsed between two sessions
    time_elap = difftime(time2, time1, units = "hours")
)

# Factor treatment variable
s2$treat  <- factor(s2$treat, levels = c(0, 1),
labels = c("Control", "Substituted Choice"))

## Figure 2 ====

# Construct data frame for plotting
s2_mean <- s2 %>%
pivot_longer(
    cols = c(enjoy1:enjoy8, recover),
    names_to = "time",
    values_to = "rating"
) %>%
group_by(treat, time) %>%
summarize(
    mean = mean(rating, na.rm = TRUE),
    se = plotrix::std.error(rating, na.rm = TRUE)
) %>%
ungroup()

# Figure 2A: Trajectory of enjoyment ratings
fig2_a <- s2_mean %>%
filter(time != "recover") %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
scale_y_continuous(limits = c(2.6, 4.3),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Sequence",
    y = "Average Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
)

# Figure 2B: Spontaneous recovery
fig2_b <- s2_mean %>%
filter(time %in% c("enjoy8", "recover")) %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = c("T8", "Recovery")) +
scale_y_continuous(limits = c(2.6, 4.3),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Sequence",
    y = "Average Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
)

# Combine Figures 2A and 2B
fig2 <- fig2_a + fig2_b +
plot_annotation(
    tag_levels = "A"
) &
theme(
    plot.title = element_text(size = 14)
)

# Export figure
ggsave(plot = fig2, filename = "_figures/fig2.png",
width = 18, height = 9)

## Table 2 ====

# Model 1, baseline model
s2_lm1 <- lm(recover ~ treat + enjoy8, s2)

summary(s2_lm1)

# Model 2, controlling for time elapsed between two sessions
s2_lm2 <- lm(recover ~ treat + enjoy8 + time_elap, s2)

# Table 2
modelsummary(
    list(s2_lm1, s2_lm2),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "^R2$|AIC|BIC|Log.Lik.|F|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "treatSubstituted Choice" = "Substituted Choice",
        "enjoy8" = "Previous Enjoyment",
        "time_elap" = "Time Elapsed"
    ),
    title = "Table 2. Results of Study 2",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

## Other Analyses ====

# T1 & T8 enjoyment ratings, perceived lack of autonomy, and
# time elapsed between two sessions
psych::describeBy(list(
    T1 = s2$enjoy1,
    T8 = s2$enjoy8,
    TE = as.numeric(s2$time_elap),
    LA = s2$lack_autnm
),
group = s2$treat)

psych::describe(as.numeric(s2$time_elap))

t.test(enjoy1 ~ treat, s2, var.equal = TRUE)

t.test(enjoy8 ~ treat, s2, var.equal = TRUE)

t.test(time_elap ~ treat, s2, var.equal = TRUE)

t.test(lack_autnm ~ treat, s2, var.equal = TRUE)

# Investigate if there is a difference in trajectory of enjoyment ratings
# between the two conditions

# Pivot longer
s2_long <- s2 %>%
    pivot_longer(
        cols = c(enjoy1:enjoy8),
        names_to = "time",
        values_to = "rating"
    ) %>%
    mutate(time = as.numeric(str_extract(time, "\\d+")))

# Mixed model

# All ratings
s2_lmx1 <- lmer(rating ~ time * treat + (1 | id), data = s2_long)

summary(s2_lmx1)

# Ratings after T4
s2_lmx2 <- lmer(rating ~ time * treat + (1 | id),
data = filter(s2_long, time > 4))

summary(s2_lmx2)

# Table A2
modelsummary(
    list(
        "(1)\nAll Ratings" = s2_lmx1,
        "(2)\nRatings after T4" = s2_lmx2
    ),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "AIC|BIC|Log.Lik.|F|RMSE|ICC",
    coef_map = c(
        "(Intercept)" = "Constant",
        "time" = "Rating Sequence",
        "treatSubstituted Choice" = "Substituted Choice",
        "time:treatSubstituted Choice" = "Interaction"
    ),
    title = "Table A2. Study 2",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

# Study 3 ####

# Load data
s3 <- haven::read_sav("Data_Study3_Preference.sav")

# Summary statistics of gender and age
table(s3$male)

psych::describe(s3$age)

# Factor variables
s3$eval_order <- factor(s3$eval_order, levels = c(0, 1),
labels = c("Alphabet Cookies First", "Animal Cookies First"))

s3$item_eaten <- factor(s3$item_eaten, levels = c(0, 1),
labels = c("Alphabet Cookies", "Animal Cookies"))

# Reliability of the liking items
s3_alp <- s3 %>% filter(eval_order == "Alphabet Cookies First")

s3_ani <- s3 %>% filter(eval_order == "Animal Cookies First")

alp_like <- data.frame(
    liking_1 = c(s3_alp$liking1_1, s3_ani$liking2_1),
    liking_2 = c(s3_alp$liking1_2, s3_ani$liking2_2),
    liking_3 = c(s3_alp$liking1_3, s3_ani$liking2_3)
)

ani_like <- data.frame(
    liking_1 = c(s3_ani$liking1_1, s3_alp$liking2_1),
    liking_2 = c(s3_ani$liking1_2, s3_alp$liking2_2),
    liking_3 = c(s3_ani$liking1_3, s3_alp$liking2_3)
)

# Alpha of the liking items for Alphabet cookies
psych::alpha(alp_like[, 1:3])

# Alpha of the liking items for Animal cookies
psych::alpha(ani_like[, 1:3])

# Correlation between the two recovery items
cor.test(s3$recover1, s3$recover2)

# Correlation between the two items of perceived lack of autonomy
cor.test(s3$autnm1, s3$autnm2)

# Variable construction
s3 <- s3 %>%
mutate(
    # spontaneous recovery
    recover = rowMeans(select(., recover1, recover2), na.rm = TRUE),
    # perceived lack of autonomy
    lack_autnm = rowMeans(select(., autnm1, autnm2), na.rm = TRUE),
    # difference in liking between first and second evaluated item
    like_diff = rowMeans(select(., liking1_1, liking1_2, liking1_3) -
    select(., liking2_1, liking2_2, liking2_3), na.rm = TRUE),
    # difference in liking between eaten and uneaten item
    relpref_eaten = case_when(
        (item_eaten == "Alphabet Cookies" &
        eval_order == "Alphabet Cookies First") |
        (item_eaten == "Animal Cookies" &
        eval_order == "Animal Cookies First") ~ like_diff,
        TRUE ~ -like_diff
    )
)

# Summary statistics of lack_autnm
psych::describe(s3$lack_autnm)

# Correlation between relpref_eaten and lack_autnm
cor.test(s3$relpref_eaten, s3$lack_autnm)

## Table 3 ====

# Model 1, baseline model
s3_lm1 <- lm(recover ~ relpref_eaten + enjoy8, s3)

summary(s3_lm1)

# Model 2, controlling for item_eaten
s3_lm2 <- lm(recover ~ relpref_eaten + enjoy8 + item_eaten, s3)

summary(s3_lm2)

# Table 3
modelsummary(
    list(s3_lm1, s3_lm2),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "^R2$|AIC|BIC|Log.Lik.|F|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "relpref_eaten" = "Relative Preference",
        "enjoy8" = "Previous Enjoyment",
        "item_eatenAnimal Cookies" = "Item Eaten"
    ),
    title = "Table 3. Results of Study 3",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

# Study 4 ####

# Load data
s4 <- haven::read_sav("Data_Study4_BehMeasure.sav") %>%
filter(!is.na(play_count))

# Summary statistics of gender and age
table(s4$male)

psych::describe(s4$age)

# Factor treatment variable
s4$treat  <- factor(s4$treat, levels = c(0, 1),
labels = c("Control", "Substituted Choice"))

# Construct variable measuring time elapsed between two sessions
s4$time_elap  <- difftime(s4$time2, s4$time1, units = "hours")

## Figure 3 ====

# Construct data frame for plotting
s4_mean <- s4 %>%
pivot_longer(
    cols = c(enjoy1:enjoy8),
    names_to = "time",
    values_to = "rating"
) %>%
group_by(treat, time) %>%
summarize(
    mean = mean(rating, na.rm = TRUE),
    se = plotrix::std.error(rating, na.rm = TRUE)
) %>%
ungroup()

# Figure 3A: Trajectory of enjoyment ratings
fig3_a <- s4_mean %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Sequence",
    y = "Average Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
)

# Figure 3B: Spontaneous recovery
fig3_b <- s4 %>%
ggplot(aes(treat, play_count)) +
geom_violin() +
stat_summary(geom = "point", aes(shape = treat), fun = "mean",
size = 5, show.legend = FALSE) +
labs(
    x = "Treatment Condition",
    y = "Play Count"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
)

# Combine Figures 3A and 3B
fig3 <- fig3_a + fig3_b +
plot_annotation(
    tag_levels = "A"
) &
theme(
    plot.title = element_text(size = 14)
)

# Export figure
ggsave(plot = fig3, filename = "_figures/fig3.png",
width = 18, height = 9)

## Table 4 ====

# Model 1, poisson regression
s4_pos1 <- glm(play_count ~ treat + enjoy8, family = poisson(), s4)

summary(s4_pos1)

# Model 2, poisson regression controlling for time elapsed between two sessions
s4_pos2 <- glm(play_count ~ treat + enjoy8 + time_elap, family = poisson(), s4)

# Model 3, OLS regression
s4_lm1 <- lm(play_count ~ treat + enjoy8, s4)

# Model 4, OLS regression controlling for time elapsed between two sessions
s4_lm2 <- lm(play_count ~ treat + enjoy8 + time_elap, s4)

# Table 4
modelsummary(
    list(
        "(1)\nPoisson" = s4_pos1,
        "(2)\nPoisson" = s4_pos2,
        "(3)\nOLS" = s4_lm1,
        "(4)\nOLS" = s4_lm2
    ),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "^R2$|AIC|BIC|Log.Lik.|F|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "treatSubstituted Choice" = "Substituted Choice",
        "enjoy8" = "Previous Enjoyment",
        "time_elap" = "Time Elapsed"
    ),
    title = "Table 4. Results of Study 4",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

## Other Analyses ====

# T1 & T8 enjoyment ratings, and
# time elapsed between two sessions
psych::describeBy(list(
    T1 = s4$enjoy1,
    T8 = s4$enjoy8,
    TE = as.numeric(s4$time_elap)
),
group = s4$treat)

psych::describe(as.numeric(s4$time_elap))

t.test(enjoy1 ~ treat, s4, var.equal = TRUE)

t.test(enjoy8 ~ treat, s4, var.equal = TRUE)

t.test(time_elap ~ treat, s4, var.equal = TRUE)

# Investigate if there is a difference in trajectory of enjoyment ratings
# between the two conditions

# Pivot longer
s4_long <- s4 %>%
    pivot_longer(
        cols = c(enjoy1:enjoy8),
        names_to = "time",
        values_to = "rating"
    ) %>%
    mutate(time = as.numeric(str_extract(time, "\\d+")))

# Mixed model

# All ratings
s4_lmx1 <- lmer(rating ~ time * treat + (1 | ResponseId), data = s4_long)

summary(s4_lmx1)

# Ratings after T4
s4_lmx2 <- lmer(rating ~ time * treat + (1 | ResponseId),
data = filter(s4_long, time > 4))

summary(s4_lmx2)

# Table A3
modelsummary(
    list(
        "(1)\nAll Ratings" = s4_lmx1,
        "(2)\nRatings after T4" = s4_lmx2
    ),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "AIC|BIC|Log.Lik.|F|RMSE|ICC",
    coef_map = c(
        "(Intercept)" = "Constant",
        "time" = "Rating Sequence",
        "treatSubstituted Choice" = "Substituted Choice",
        "time:treatSubstituted Choice" = "Interaction"
    ),
    title = "Table A3. Study 4",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)
