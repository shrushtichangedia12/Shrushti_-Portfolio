# ============================================================
# MBA STARTING SALARIES - ANALYSIS
# ============================================================

# LOAD LIBRARIES
library(readxl)
library(ggplot2)
library(dplyr)

# LOAD DATA
data <- read_excel("C:/Users/Shrushti/Downloads/W12513-XLS-ENG.xlsx")
View(data)


# ============================================================
# DATA PREPARATION
# ============================================================

# Convert invalid salary values to NA
data$salary[data$salary == 998 | data$salary == 999] <- NA

# Remove rows where salary is missing
data$salary[data$salary == 0] <- NA
data <- data[!is.na(data$salary), ]

# Convert sex and frstlang to factors so R treats them as categories
data$sex <- factor(data$sex, levels = c(1, 2), labels = c("Male", "Female"))
data$frstlang <- factor(data$frstlang, levels = c(1, 2), labels = c("English", "Other"))

View(data)

# Create MBA average
data$mba_avg <- (data$f_avg + data$s_avg) / 2

# Create GMAT groups
data$gmat_group <- cut(data$gmat_tot, breaks = 3,
                       labels = c("Low", "Medium", "High"))

# Check the data
str(data)


# ============================================================
# Q1: DOES SALARY VARY?
# ============================================================

# Basic statistics
mean(data$salary)
sd(data$salary)
median(data$salary)

# Histogram
ggplot(data, aes(x = salary)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of MBA Starting Salaries",
       x = "Starting Salary",
       y = "Count") +
  theme_minimal()


# ============================================================
# Q2: DOES GENDER / AGE AFFECT SALARY?
# ============================================================

# Average salary by gender
data %>%
  group_by(sex) %>%
  summarise(mean_salary = mean(salary),
            sd_salary = sd(salary))

# T-test: Does salary differ by gender?
t.test(salary ~ sex, data = data)

# Boxplot: Salary by Gender
ggplot(data, aes(x = sex, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = "Starting Salary by Gender",
       x = "Gender",
       y = "Starting Salary") +
  theme_minimal()

# Correlation: Age and salary
cor(data$age, data$salary)

# Correlation: Work experience and salary
cor(data$work_yrs, data$salary)

# Scatter plot: Work Experience vs Salary
ggplot(data, aes(x = work_yrs, y = salary)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Work Experience vs Starting Salary",
       x = "Years of Work Experience",
       y = "Starting Salary") +
  theme_minimal()


# ============================================================
# Q2b: DOES FIRST LANGUAGE AFFECT GMAT?
# (Daer is a non-English speaker with a low GMAT)
# ============================================================

# Average GMAT by language group
data %>%
  group_by(frstlang) %>%
  summarise(mean_gmat = mean(gmat_tot),
            sd_gmat = sd(gmat_tot))

# T-test: Does GMAT differ by first language?
t.test(gmat_tot ~ frstlang, data = data)

# Boxplot: GMAT by First Language
ggplot(data, aes(x = frstlang, y = gmat_tot, fill = frstlang)) +
  geom_boxplot() +
  labs(title = "GMAT Score by First Language",
       x = "First Language",
       y = "Total GMAT Score") +
  theme_minimal()


# ============================================================
# Q3: ARE STUDENTS SATISFIED?
# ============================================================

# Mean satisfaction score
mean(data$satis)

# T-test: Is satisfaction above midpoint of 4?
t.test(data$satis, mu = 4)

# Bar chart of satisfaction
ggplot(data, aes(x = factor(satis))) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Student Satisfaction with MBA Program",
       x = "Satisfaction Score (1 = Low, 7 = High)",
       y = "Number of Students") +
  theme_minimal()


# ============================================================
# Q4: DOES GMAT AFFECT MBA PERFORMANCE?
# ============================================================

# Correlation between GMAT and MBA average
cor(data$gmat_tot, data$mba_avg)

# Scatter plot: GMAT vs MBA Average
ggplot(data, aes(x = gmat_tot, y = mba_avg)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GMAT Score vs MBA Average",
       x = "Total GMAT Score",
       y = "MBA Average") +
  theme_minimal()

# ANOVA: Does MBA average differ across GMAT groups?
anova_model <- aov(mba_avg ~ gmat_group, data = data)
summary(anova_model)

# Boxplot: MBA Average by GMAT Group
ggplot(data, aes(x = gmat_group, y = mba_avg, fill = gmat_group)) +
  geom_boxplot() +
  labs(title = "MBA Average by GMAT Group",
       x = "GMAT Group",
       y = "MBA Average") +
  theme_minimal()

model <- lm(salary ~ age + sex + work_yrs + gmat_tot + mba_avg + frstlang, data = data)
summary(model)
