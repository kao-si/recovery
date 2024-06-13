
# Autonomy and Recovery from Hedonic Adaptation

# Load packages
library(tidyverse)
library(patchwork)
library(modelsummary)

# Study 1 ####

## Method ====

# Load data
s1 <- haven::read_sav("Data_Main-Effect-Choice.sav") %>% filter(failed_ins == 0)

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
        item_chosen == item_eaten ~ 0,
        item_chosen != item_eaten ~ 1
    ),
    # recovery measure
    recover = rowMeans(select(., recover1, recover2), na.rm = TRUE)
)

# Factor variables
s1$treat <- factor(s1$treat, levels = c(0, 1),
labels = c("Free Choice", "Imposed Choice"))

s1$item_chosen <- factor(s1$item_chosen, levels = c(1, 2),
labels = c("Alphabet Cookies", "Animal Cookies"))

s1$item_eaten <- factor(s1$item_eaten, levels = c(1, 2),
labels = c("Alphabet Cookies", "Animal Cookies"))

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

# Figure 1A: Hedonic decline
fig1_a <- s1_mean %>%
filter(time != "recover") %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
scale_y_continuous(limits = c(2.5, 5.7),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Overtime",
    y = "Mean Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
)

# Figure 1B: Recovery
fig1_b <- s1_mean %>%
filter(time %in% c("enjoy8", "recover")) %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.03) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels =
c("T8", "Recovery Measure\n(About 10 minutes later)")) +
scale_y_continuous(limits = c(2.5, 5.7),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Overtime",
    y = "Mean Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
)

# Combine Figures 1A and 1B
fig1 <- fig1_a + fig1_b +
plot_annotation(
    tag_levels = "A",
    title = "Hedonic decline (A) and recovery (B), Study 1",
    caption = "Error bars represent ± 1 standard error."
) &
theme(
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 12)
)

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
        "treatImposed Choice" = "Imposed Choice",
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

# Study 2 ####

## Method ====

# Load data
s2 <- haven::read_sav("Data_Main-Effect.sav") %>% filter(failed_ins == 0)

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
    # recovery measure
    recover = rowMeans(select(., recover1, recover2), na.rm = TRUE),
    # perceived lack of autonomy of choice
    lack_autnm = rowMeans(select(., autnm1, autnm2), na.rm = TRUE),
    # time elapsed until recovery measure
    time_elap = difftime(time2, time1, units = "hours")
)

# Factor treatment variable
s2$treat  <- factor(s2$treat, levels = c(0, 1),
labels = c("Control", "Imposed Choice"))

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

# Figure 2A: Hedonic decline
fig2_a <- s2_mean %>%
filter(time != "recover") %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
scale_y_continuous(limits = c(2.6, 4.3),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Overtime",
    y = "Mean Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
)

# Figure 2B: Recovery
fig2_b <- s2_mean %>%
filter(time %in% c("enjoy8", "recover")) %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.03) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(
    labels = c("T8", "Recovery Measure\n(On average 3 hours and\n20 minutes later)")
) +
scale_y_continuous(limits = c(2.6, 4.3),
labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Overtime",
    y = "Mean Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
)

# Combine Figures 2A and 2B
fig2 <- fig2_a + fig2_b +
plot_annotation(
    tag_levels = "A",
    title = "Hedonic decline (A) and recovery (B), Study 2",
    caption = "Error bars represent ± 1 standard error."
) &
theme(
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 12)
)

## Table 2 ====

# Model 1, baseline model
s2_lm1 <- lm(recover ~ treat + enjoy8, s2)

summary(s2_lm1)

# Model 2, controlling for time elapsed until recovery measure
s2_lm2 <- lm(recover ~ treat + enjoy8 + time_elap, s2)

# Table 2
modelsummary(
    list(s2_lm1, s2_lm2),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "^R2$|AIC|BIC|Log.Lik.|F|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "treatImposed Choice" = "Imposed Choice",
        "enjoy8" = "Previous Enjoyment",
        "time_elap" = "Time Elapsed"
    ),
    title = "Table 2. Results of Study 2",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

## Other Analyses ====

# T1 & T8 enjoyment ratings, perceived lack of autonomy, and
# time elapsed until recovery measure
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

# Study 3 ####

## Method ====

# Load data
s3 <- haven::read_sav("Data_Preference.sav")

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
    # recovery measure
    recover = rowMeans(select(., recover1, recover2), na.rm = TRUE),
    # perceived lack of autonomy of choice
    lack_autnm = rowMeans(select(., autnm1, autnm2), na.rm = TRUE),
    # difference in liking between first and second evaluated item
    like_diff = rowMeans(select(., liking1_1, liking1_2, liking1_3) -
    select(., liking2_1, liking2_2, liking2_3), na.rm = TRUE)
) %>%
mutate(
    # difference in liking between eaten and uneaten item
    relpref_eaten = case_when(
        (item_eaten == "Alphabet Cookies" &
        eval_order == "Alphabet Cookies First") |
        (item_eaten == "Animal Cookies" &
        eval_order == "Animal Cookies First") ~ like_diff,
        TRUE ~ -like_diff
    )
)

# Correlation between relpref_eaten and lack_autnm
cor.test(s3$relpref_eaten, s3$lack_autnm)

## Table 3 ====

# Model 1, baseline model
s3_lm1 <- lm(recover ~ relpref_eaten + enjoy8, s3)

summary(s3_lm1)

# Model 2, controlling for item_eaten
s3_lm2 <- lm(recover ~ relpref_eaten + enjoy8 + item_eaten, s3)

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

## Method ====

# Load data
s4 <- haven::read_sav("Data_Behavior-Measure.sav") %>%
filter(!is.na(play_count))

# Summary statistics of gender and age
table(s4$male)

psych::describe(s4$age)

# Factor treatment variable
s4$treat  <- factor(s4$treat, levels = c(0, 1),
labels = c("Control", "Imposed Choice"))

# Construct variable measuring time elapsed until recovery measure
s4$time_elap  <- difftime(s4$StartDate_2nd, s4$StartDate_1st, units = "hours")

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

# Figure 3A: Hedonic decline
fig3_a <- s4_mean %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 5) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
labs(
    x = "Enjoyment Rating Overtime",
    y = "Mean Enjoyment Rating"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
)

# Figure 3B: Recovery
fig3_b <- s4 %>%
ggplot(aes(treat, play_count)) +
geom_violin() +
stat_summary(geom = "point", aes(shape = treat), fun = "mean",
size = 5, show.legend = FALSE) +
stat_summary(geom = "errorbar",
fun.data = function(x) {
    se_x <- plotrix::std.error(x, na.rm = TRUE)
    data.frame(
        ymin = mean(x) - se_x,
        ymax = mean(x) + se_x
    )
},
width = 0.03
) +
labs(
    x = "Treatment Condition",
    y = "Play Count"
) +
ggpubr::theme_pubr() +
theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
)

# Combine Figures 3A and 3B
fig3 <- fig3_a + fig3_b +
plot_annotation(
    tag_levels = "A",
    title =
    "Hedonic decline (A) and recovery (on average three and half hours later; B), Study 4",
    caption = "Error bars represent ± 1 standard error."
) &
theme(
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 12)
)

## Table 4 ====

# Model 1, poisson regression
s4_pos1 <- glm(play_count ~ treat + enjoy8, family = poisson(), s4)

summary(s4_pos1)

# Model 2, poisson regression controlling for time elapsed until recovery measure
s4_pos2 <- glm(play_count ~ treat + enjoy8 + time_elap, family = poisson(), s4)

# Model 3, OLS regression
s4_lm1 <- lm(play_count ~ treat + enjoy8, s4)

# Model 4, OLS regression controlling for time elapsed until recovery measure
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
        "treatImposed Choice" = "Imposed Choice",
        "enjoy8" = "Previous Enjoyment",
        "time_elap" = "Time Elapsed"
    ),
    title = "Table 4. Results of Study 4",
    notes = "Numbers in parentheses represent standard errors.",
    output = "markdown"
)

## Other Analyses ====

# T1 & T8 enjoyment ratings, and
# time elapsed until recovery measure
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