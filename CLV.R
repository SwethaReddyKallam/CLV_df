source("C:/CLV/email.R")
source("C:/CLV/seller.R")
source("C:/CLV/medallia.R")
source("C:/CLV/origin.R")

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)

origin_length = Origin_Length %>% 
  #filter(!fan_channel %in% 'PRIMARY TICKET SALES') %>%
  select(customer_master_index,fiscal_year, property)

data = origin_length %>%
  group_by(customer_master_index) %>%
  summarise(
    first_interaction = min(fiscal_year),
    last_interaction = max(fiscal_year), 
    total_years = n_distinct(fiscal_year),
    #total_interaction = tally(fiscal_year),
    first_property = property[1],
    last_property = last(property)
  ) %>%
  arrange(customer_master_index)

d = origin_length %>%
  select(customer_master_index, fiscal_year)

property = left_join(d, Origin_Property, by = c("customer_master_index" = "customer_master_index", 
                                                "fiscal_year" = "fiscal_year")) %>%
  distinct(customer_master_index, fiscal_year, .keep_all = TRUE) %>%
  select(, -fiscal_year) %>%
  group_by(customer_master_index) %>% 
  summarize_all(sum)

property['most_interacted_property'] <- names(property)[2:(ncol(property)-1)][max.col(property[2:(ncol(property)-1)], "first")]


data1 = property %>% 
  rename(total_interactions = combo_prop)

channel = left_join(d, Origin_Channel, by = c("customer_master_index" = "customer_master_index", 
                                              "fiscal_year" = "fiscal_year")) %>%
  distinct(customer_master_index, fiscal_year, .keep_all = TRUE) %>%
  select(, -fiscal_year) %>%
  group_by(customer_master_index) %>% 
  summarize_all(sum)

channel['most_interacted_channel'] <- names(channel)[2:(ncol(channel)-1)][max.col(channel[2:(ncol(channel)-1)], "first")]

data2 = channel %>% 
  rename(total_channels = combo_channel) %>%
  select(customer_master_index, total_channels, most_interacted_channel)

data_origin = left_join(data, data1, by='customer_master_index') %>%
  left_join(., data2, by='customer_master_index') 



### EMAIL
data_email = Email_Data_Total %>%
  select(customer_master_index, open_rate_Total, click_sent_rate_Total, click_open_rate_Total) 

df = left_join(data_origin, data_email, by ='customer_master_index') 

### MEDALLIA 

medallia = MD_2 %>%
  select(customer_master_index, answered_survey, event_count, likelihood_to_recommend_Detractor, 
         likelihood_to_recommend_Promoter, entering_the_arena_theatre_Detractor, 
         entering_the_arena_theatre_Promoter) %>%
  rename(survey_answered_flag = answered_survey, survey_answered = event_count) 

#medallia['likelihood_to_recommend'] <- names(medallia)[(ncol(medallia)-3):(ncol(medallia)-2)][max.col(medallia[(ncol(medallia)-3):(ncol(medallia)-2)], "first")]
#medallia$likelihood_to_recommend[medallia$likelihood_to_recommend == "likelihood_to_recommend_Detractor"] <- "Detractor"
#medallia$likelihood_to_recommend[medallia$likelihood_to_recommend == "likelihood_to_recommend_Promoter"] <- "Promoter"

#medallia['entering_the_arena_theatre'] <- names(medallia)[(ncol(medallia)-1):ncol(medallia)][max.col(medallia[(ncol(medallia)-1):ncol(medallia)], "first")]
#medallia$entering_the_arena_theatre[medallia$entering_the_arena_theatre == "entering_the_arena_theatre_Detractor"] <- "Detractor"
#medallia$entering_the_arena_theatre[medallia$entering_the_arena_theatre == "entering_the_arena_theatre_Promoter"] <- "Promoter"

medallia = medallia %>%
  select(-likelihood_to_recommend_Detractor, -likelihood_to_recommend_Promoter, 
         -entering_the_arena_theatre_Detractor, -entering_the_arena_theatre_Promoter)

medallia1 = MD_YOY %>%
  group_by(customer_master_index) %>%
  summarise(
    weighted_score = last(weighted_score)
  ) 

Medallia2 = Medallia_Data %>% 
  mutate(likelihood_to_recommend = as.numeric(likelihood_to_recommend),
         entering_the_arena_theatre=as.numeric(entering_the_arena_theatre),
         helpfulness_of_ushers=as.numeric(helpfulness_of_ushers),
         cleanliness_of_arena_theatre=as.numeric(cleanliness_of_arena_theatre),
         cleanliness_of_restrooms=as.numeric(cleanliness_of_restrooms),
         quality_of_experience_at_concession_stands=
           as.numeric(quality_of_experience_at_concession_stands),
         quality_of_ingame_entertainment_experience=
           as.numeric(quality_of_ingame_entertainment_experience),
         quality_of_merchandise_shopping_experience=
           as.numeric(quality_of_merchandise_shopping_experience),
         overall_value=as.numeric(overall_value))

Medallia2$Avg_score = rowMeans(Medallia2[,c(4,6:12,14)], na.rm = TRUE)

Medallia3 = Medallia2 %>%
  group_by(customer_master_index) %>%
  summarise(
    survey_first_taken = str_sub(tm_event_date[1],1,4),
    survey_last_taken = str_sub(last(tm_event_date), 1,4), 
    most_surveys_taken_in  = names(table(event_business_unit))[which.max(table(event_business_unit))],
    comment_string_count = mean(sapply(strsplit(ltr_comment, " "), length), na.rm = TRUE),
    avarage_score = mean(Avg_score, na.rm = TRUE)) 

data_medallia = left_join(medallia, medallia1, by='customer_master_index') %>%
  left_join(., Medallia3, by='customer_master_index') 

df = left_join(df, data_medallia, by = 'customer_master_index')

### SELLER/BUYER

seller_buyer = rbind(Archtics_Resale_Buyer_Data,Archtics_Resale_Seller_Data)

s_b = seller_buyer %>%
  filter(customer_master_index != -1 ) %>%
  filter(ticket_exchange_activity_desc_new != "NA" & ticket_exchange_activity_desc_new != "OTHER") %>%
  select(-tm_event_date) %>%
  group_by(customer_master_index,ticket_exchange_activity_desc_new) %>%
  summarise(count=n()) %>%
  dcast(., customer_master_index ~ paste0("ticket_excahnge_activity_", ticket_exchange_activity_desc_new), value.var="count")


s_b1 = seller_buyer %>%
  filter(customer_master_index != -1 ) %>%
  select(customer_master_index, event_business_unit) %>%
  group_by(customer_master_index, event_business_unit) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))%>%
  slice(1)%>%ungroup()%>%
  select(customer_master_index, event_business_unit) %>%
  rename(most_activities_in = event_business_unit)

seller = left_join(s_b, s_b1, by = 'customer_master_index')

df = left_join(df, seller, by ='customer_master_index')
df = df %>% 
  mutate(most_interacted_property = as.numeric(most_interacted_property),
         survey_first_taken = as.numeric(survey_first_taken),
         survey_last_taken = as.numeric(survey_last_taken),
         survey_last_taken = as.numeric(survey_last_taken)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
         

dff = df[1:100,]
write.csv(dff, "C:/CLV/df_100.csv")

