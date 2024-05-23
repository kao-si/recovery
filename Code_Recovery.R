
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

# Exploratory variables
s1 <- s1 %>%
mutate(
    # initial desire for item chosen
    init_des_chosen = case_when(
        item_chosen == "Alphabet Cookies" ~
        rowMeans(select(., desire_alp1, desire_alp2), na.rm = TRUE),
        TRUE ~ rowMeans(select(., desire_ani1, desire_ani2), na.rm = TRUE)
    ),
    # initial desire for item not chosen
    init_des_unchosen = case_when(
        item_chosen == "Alphabet Cookies" ~
        rowMeans(select(., desire_ani1, desire_ani2), na.rm = TRUE),
        TRUE ~ rowMeans(select(., desire_alp1, desire_alp2), na.rm = TRUE)
    )
) %>%
# difference in inital desire between item chosen versus item not chosen
mutate(init_des_diff = init_des_chosen - init_des_unchosen)

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

# Figure 1A: Hedonic adaptation
fig1_a <- s1_mean %>%
filter(time != "recover") %>%
ggplot(aes(time, mean, group = treat)) +
geom_line(aes(linetype = treat), linewidth = 0.8, show.legend = FALSE) +
geom_point(aes(shape = treat), size = 3) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = paste0("T", 1:8)) +
ylim(c(2.5, 6)) +
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
geom_point(aes(shape = treat), size = 3) +
geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.03) +
scale_linetype_manual(values = c("dashed", "solid")) +
scale_x_discrete(labels = c("T8", "Recovery\n(About 10 Minutes Later)")) +
ylim(c(2.5, 6)) +
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
    title = "Hedonic adaptation and recovery (Study 1)",
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

# Model 4, controlling for difference in inital desire between item chosen
# versus item not chosen
s1_lm4 <- lm(recover ~ treat + enjoy8 + init_des_diff, s1)

# Table 1
modelsummary(
    list(s1_lm1, s1_lm2, s1_lm3, s1_lm4),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    gof_omit = "^R2$|AIC|BIC|Log.Lik.|F|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "treatImposed Choice" = "Imposed Choice",
        "enjoy8" = "Previous Enjoyment",
        "item_chosenAnimal Cookies" = "Item Chosen",
        "item_eatenAnimal Cookies" = "Item Eaten",
        "init_des_diff" = "Difference in Initial Desire"
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