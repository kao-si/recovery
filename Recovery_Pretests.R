
# Lack of Autonomy and Spontaneous Recovery from Hedonic Decline
# Study Pretests

# Load packages
library(tidyverse)
library(ez)

# Study 1 Pretest ####

# Load data
s1 <- haven::read_sav("Data_Study1_Pretest.sav")

# Paired t-test to compare the attractiveness ratings of Alphabet vs. Animal
# cookies
t.test(s1$attract_alpha, s1$attract_animal, paired = TRUE)

# Descriptive statistics
psych::describe(s1$attract_alpha)
psych::describe(s1$attract_animal)

# Create an object to compare choice proportions of Alphabet vs. Animal cookies
choice <- c(
    s1$choice1[s1$choice1 %in% c(3, 4)],
    s1$choice2[s1$choice2 %in% c(3, 4)]
) %>%
factor(labels = c("Alphabet Cookies", "Animal Cookies"))

table(choice)

# Compare the enjoyment ratings of Alphabet vs. Animal cookies

# Create data frames
s1_enjoy1 <- s1[s1$choice1 %in% c(3, 4), c("choice1", "enjoy1")] %>%
rename(
    choice = choice1,
    enjoy = enjoy1
)

s1_enjoy2 <- s1[s1$choice2 %in% c(3, 4), c("choice2", "enjoy2")] %>%
rename(
    choice = choice2,
    enjoy = enjoy2
)

s1_enjoy <- rbind(s1_enjoy1, s1_enjoy2)

# Use independent t-test
t.test(enjoy ~ choice, s1_enjoy, var.equal = TRUE)

# Descriptive statistics
psych::describeBy(s1_enjoy$enjoy, s1_enjoy$choice)

# Study 2 Pretest ####

# Load data
s2 <- haven::read_sav("Data_Study2_Pretest.sav")

# Summary statistics of gender and age
table(s2$gender)

psych::describe(s2$age)

# Correlations between the two attractiveness items
cor.test(s2$focal_attract1, s2$focal_attract2)
cor.test(s2$choice1_attract1, s2$choice1_attract2)
cor.test(s2$choice2_attract1, s2$choice2_attract2)
cor.test(s2$choice3_attract1, s2$choice3_attract2)

# Variable construction
s2 <- s2 %>%
  mutate(
    # Attractiveness rating of the focal product
    focal_attract = rowMeans(s2[, c("focal_attract1",
    "focal_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #1
    choice1_attract = rowMeans(s2[, c("choice1_attract1",
    "choice1_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #2
    choice2_attract = rowMeans(s2[, c("choice2_attract1",
    "choice2_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #3
    choice3_attract = rowMeans(s2[, c("choice3_attract1",
    "choice3_attract2")], na.rm = TRUE),
    # Create a numeric participant ID
    id = row_number()
  )

# Reshape data frame into long-form
s2_long <- s2 %>%
  pivot_longer(
    cols = c(focal_attract, choice1_attract, choice2_attract, choice3_attract),
    names_to = "brand",
    values_to = "rating"
  )

# Repeated-measures ANOVA to compare the attractiveness ratings of the four
# brands
ezANOVA(
  data = s2_long,
  dv = .(rating),
  wid = .(id),
  within = .(brand),
  detailed = TRUE,
  type = 3
)

# Study 3 Pretest ####

# Load data
s3 <- haven::read_sav("Data_Study3_Pretest.sav")

# Summary statistics of gender and age
table(s3$male)

psych::describe(s3$age)

# Paired t-test to compare the attractiveness ratings of Alphabet vs. Animal
# cookies
t.test(s3$liking_alpha, s3$liking_animal, paired = TRUE)

# Descriptive statistics
psych::describe(s3$liking_alpha)
psych::describe(s3$liking_animal)

# Study 4 Pretest ####

# Load data
s4 <- haven::read_sav("Data_Study4_Pretest.sav")

# Summary statistics of gender and age
table(s4$gender)

psych::describe(s4$age)

# Correlations between the two attractiveness items
cor.test(s4$focal_attract1, s4$focal_attract2)
cor.test(s4$choice1_attract1, s4$choice1_attract2)
cor.test(s4$choice2_attract1, s4$choice2_attract2)
cor.test(s4$choice3_attract1, s4$choice3_attract2)
cor.test(s4$choice4_attract1, s4$choice4_attract2)
cor.test(s4$choice5_attract1, s4$choice5_attract2)
cor.test(s4$choice6_attract1, s4$choice6_attract2)

# Variable construction
s4 <- s4 %>%
  mutate(
    # Attractiveness rating of the focal music clip
    focal_attract = rowMeans(s4[, c("focal_attract1",
    "focal_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #1
    choice1_attract = rowMeans(s4[, c("choice1_attract1",
    "choice1_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #2
    choice2_attract = rowMeans(s4[, c("choice2_attract1",
    "choice2_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #3
    choice3_attract = rowMeans(s4[, c("choice3_attract1",
    "choice3_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #4
    choice4_attract = rowMeans(s4[, c("choice4_attract1",
    "choice4_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #5
    choice5_attract = rowMeans(s4[, c("choice5_attract1",
    "choice5_attract2")], na.rm = TRUE),
    # Attractiveness rating of choice option #6
    choice6_attract = rowMeans(s4[, c("choice6_attract1",
    "choice6_attract2")], na.rm = TRUE)
  )

# Reshape data frame into long-form
s4_long <- s4 %>%
  pivot_longer(
    cols = c(focal_attract, choice1_attract, choice2_attract,
    choice3_attract, choice4_attract, choice5_attract, choice6_attract),
    names_to = "clip",
    values_to = "rating",
    values_drop_na = TRUE
  )

# One-way ANOVA to compare the attractiveness ratings of the seven
# music clips
s4_aov <- aov(rating ~ clip, data = s4_long)

summary(s4_aov)
