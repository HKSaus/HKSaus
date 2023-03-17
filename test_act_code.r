library(tidyverse)

setwd("C:/Users/hsuh01/Documents")

db <- read_csv("test_act.csv")


db_1 <- db %>% 
  group_by(Year,Industry) %>%
  mutate(Average_Industry_Tax = mean(TaxPaid)) %>%
  ungroup() %>%
  mutate(Flag = case_when(TaxPaid < Average_Industry_Tax ~ "Below Industry Average",
                                 TaxPaid >= Average_Industry_Tax ~ "At or Above Industry Average")) %>%
  group_by(Employer) %>%
  mutate(Yoy_pct_change = (TaxPaid - lag(TaxPaid)) / lag(TaxPaid) * 100) %>%
  ungroup()

db_1 %>% 
  ggplot(aes(x = Year, y = TaxPaid, fill = Industry)) + 
  geom_col(position = "fill") +
  ylab("Proportion of Tax Paid")



  




