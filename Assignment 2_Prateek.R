library(readr)
library(ggplot2)
library(dplyr)
#Univariate

#Question 1: How does salary vary with experience level?
data <- read.csv("/Users/Prateekg/Downloads/Dataset.csv")
head(data)
ggplot(data, aes(x = experience_level, y = salary_in_usd)) +
  geom_boxplot() +
  labs(title = "Salary distribution across experience levels",
       x = "Experience Level",
       y = "Salary (USD)")
#Insight: Senior-level professionals tend to have higher salaries compared to entry-level and mid-level professionals.

#Question 2: What is the distribution of salaries across different job categories? 
ggplot(data, aes(x = salary_in_usd, fill = job_category)) +
  geom_histogram(binwidth = 5000, position = "dodge") +
  labs(title = "Salary distribution across job categories",
       x = "Salary (USD)",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set3") + theme_minimal()
#Insight: Salaries in the "Machine Learning" category generally appear higher compared to other job categories.

#Question 3
ggplot(data, aes(x = work_year, y = salary_in_usd)) +
  geom_line() +
  labs(title = "Salary trend over the years",
       x = "Year",
       y = "Salary (USD)")
#Insight: From 2022 to 2023 the salary of employees has gradually started increasing.

#Bivariate

#Question 4:How does salary vary with company size?
ggplot(data, aes(x = company_size, y = salary_in_usd, fill = company_size)) +
  geom_violin()
# Insight: Employees in larger companies tend to have higher salaries compared to those in smaller companies.

#Multivariate

#Question 5:How does salary vary with job category, experience level, and employment type?
ggplot(data, aes(x = job_category, y = salary_in_usd, color = experience_level)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~employment_type) +
  labs(title = "Salary distribution across job categories by experience level and employment type",
       x = "Job Category",
       y = "Salary (USD)") 
#Insight: Full time jobs have higher salaries and and the frequency of employees are also higher.

#Question 6: How does salary vary with employment type and work setting?
ggplot(data, aes(x = employment_type, y = salary_in_usd, fill = work_setting)) +
  geom_bar(stat = "identity") +
  labs(title = "Salary vs. Employment Type and Work Setting",
       x = "Employment Type", y = "Salary (USD)")
#Insight: This stacked barplot shows how salaries vary across different employment types and work settings.

#Question 7: How does the density of salaries vary with job experience and job category?
ggplot(data, aes(x = experience_level, y = job_category)) +
  geom_hex(aes(fill = stat(density)), bins = 20)
#Insight: Darker areas indicate higher density, suggesting regions where more data points are concentrated. 

