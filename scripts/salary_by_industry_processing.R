library(dplyr)
library(readr)
library(stringr)
library(knitr)
library(kableExtra)

df = read_csv("~/Masters/Data608/wk8/salary_by_industry_raw.csv")


categorize_naics = function(title) {
  if (grepl(pattern = "Hospital|Health|Care", title, ignore.case = TRUE)) {
    return("Healthcare and Social Assistance")
  } else if (grepl(pattern = "Manufacturing|Construction", title, ignore.case = TRUE)) {
    return("Manufacturing and Construction")
  } else if (grepl(pattern = "Retail|Wholesale", title, ignore.case = TRUE)) {
    return("Retail and Wholesale Trade")
  } else if (grepl(pattern = "Accounting|Engineering|Scientific", title, ignore.case = TRUE)) {
    return("Professional, Scientific, and Technical Services")
  } else if (grepl(pattern = "Food|Entertainment|Arts|Accommodation", title, ignore.case = TRUE)) {
    return("Accommodation, Food Services, and Entertainment")
  } else if (grepl(pattern = "Software|Data|Publishing|Telecommunication", title, ignore.case = TRUE)) {
    return("Information, Communication, and Technology")
  } else if (grepl(pattern = "Financial|Insurance|Real Estate", title, ignore.case = TRUE)) {
    return("Financial Services, Insurance, and Real Estate")
  } else {
    return("Other Industries")
  }
}


df$category = sapply(df$NAICS_TITLE, categorize_naics)


df = df %>% 
  mutate(A_MEAN_numeric = parse_number(A_MEAN)) %>%
  filter(!is.na(A_MEAN_numeric)) %>%
  select(-A_MEAN_numeric) 

df$A_MEAN = gsub("[^0-9.]", "", df$A_MEAN)
df$A_MEAN = as.numeric(df$A_MEAN)

write.csv(df, 'clean_industry.csv')



boxplot_stats2 = df %>%
  group_by(category) %>%
  summarize(
    Min = min(A_MEAN, na.rm = TRUE),
    Q1 = quantile(A_MEAN, 0.25, na.rm = TRUE),
    Median = median(A_MEAN, na.rm = TRUE),
    Mean = mean(A_MEAN, na.rm = TRUE),
    StDev=sd(A_MEAN, na.rm= TRUE),
    Q3 = quantile(A_MEAN, 0.75, na.rm = TRUE),
    Max = max(A_MEAN, na.rm = TRUE),
    .groups = 'drop'
  )%>%
  ungroup()

boxplot_stats2 = boxplot_stats2 %>%
  arrange(desc(Mean))


boxplot_table2 = kable(boxplot_stats2, format = "html", caption = "Boxplot Statistics by Category") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE, color = "blue")
save_kable(boxplot_table2, file = "boxplot_stats2.html")


df3=read.csv("clean_job.csv")
class(df3$A_MEAN)
df3$A_MEAN = gsub("[^0-9.]", "", df3$A_MEAN)
df3$A_MEAN = as.numeric(df3$A_MEAN)

df3 = df3 %>% filter(!is.na(A_MEAN))

boxplot_stats = df3 %>%
  group_by(OCC_TITLE) %>%
  summarize(
    Min = min(A_MEAN, na.rm = TRUE),
    Q1 = quantile(A_MEAN, 0.25, na.rm = TRUE),
    Median = median(A_MEAN, na.rm = TRUE),
    Mean = mean(A_MEAN, na.rm = TRUE),
    StDev=sd(A_MEAN, na.rm= TRUE),
    Q3 = quantile(A_MEAN, 0.75, na.rm = TRUE),
    Max = max(A_MEAN, na.rm = TRUE),
    IQR = IQR(A_MEAN, na.rm = TRUE),
    .groups = 'drop'
  )%>%
  arrange(desc(Mean))

boxplot_table = kable(boxplot_stats, "html", booktabs = TRUE, caption = "Summary Statistics for A_MEAN by OCC_TITLE") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE, color = "blue")

save_kable(boxplot_table, "boxplot_table.html")
