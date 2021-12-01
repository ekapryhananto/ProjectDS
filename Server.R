library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

data <- read.csv(file = "D:/Kuliah/Semester 5/Prak DS/pprojet/covids.csv")
data <- data %>% arrange(desc(user_id)) %>% select(user_id, translated) %>% sample_n(100)
datatrain<- read.csv(file = "D:/Kuliah/Semester 5/Prak DS/pprojet/data_training.csv")

all_data <- data.frame(text = data$translated, sentiment = NA) %>% rbind(datatrain) %>% 
  mutate(id = row_number(), .before = "text")

data_predict_full <- all_data[1:100,]
datatrain<- all_data[101:250,]

temporarydata <- all_data
temporarydata$text <- gsub("#\\w+", " ", temporarydata$text)
temporarydata$text <- gsub("^\\s+|\\s+$", "", temporarydata$text)
temporarydata$text <- gsub("@\\w+", " ", temporarydata$text)
temporarydata$text <- gsub('\\d+', "", temporarydata$text)
temporarydata$text <- gsub("https://t.co/\\w+", " ", temporarydata$text)
temporarydata$text <- gsub('[^\x01-\x7F]', "", temporarydata$text)
temporarydata$text <- gsub('[\\.\\,]', " ", temporarydata$text)
temporarydata$text <- gsub('\\d+', "", temporarydata$text)
temporarydata$text <- gsub('[[:punct:]]', "", temporarydata$text)
temporarydata$text <- gsub('[[:cntrl:]]', " ", temporarydata$text)
temporarydata$text <- gsub("[ \t]{2,}", " ", temporarydata$text)
temporarydata$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", temporarydata$text)
temporarydata$text <- tolower(temporarydata$text)
temporarydata[temporarydata == ""] <- NA
temporarydata <- temporarydata %>% select(id, text) %>% unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% group_by(id) %>% summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()

clean_datatrain <- datatrain %>%
  left_join(temporarydata, by = "id") %>% select(id, text.y)
colnames(clean_datatrain)[2] <- "text"

clean_data_predict <- data_predict_full %>% left_join(temporarydata, by = "id") %>% select(id, text.y)

colnames(clean_data_predict)[2] <- "text"

result_predict <- data_predict_full

for (j in seq_len(nrow(clean_data_predict))) {
  cat(sprintf("\n (%d / %d)", j, nrow(clean_data_predict)))
  data_predict <- clean_data_predict[j,]
  tidy_data <- clean_datatrain%>% rbind(data_predict)
  
  tf_idf <- tidy_data %>%
    unnest_tokens(word, text) %>% count(id, word, sort = TRUE) %>% bind_tf_idf(word, id, n)
  
  bobot_predict <- tf_idf %>% filter(id == data_predict$id)
  
  bobot_training <- data.frame(id = integer(),
                               sum = numeric())
  
  for (i in seq_len(nrow(clean_datatrain))) {
    temp_data <- tf_idf %>% filter(id == clean_datatrain$id[i])
    
    join <- bobot_predict %>% inner_join(temp_data, by = "word") %>%mutate(kali = tf_idf.x * tf_idf.y)
    
    bobot_training <- bobot_training %>%rbind(data.frame(id = clean_datatrain$id[i], sum = sum(join$kali)))
  }
  
  kuadrat_bobot <- tf_idf
  kuadrat_bobot$tf_idf <- kuadrat_bobot$tf_idf^2
  
  vektor <- data.frame(id = integer(),sum = numeric(), sqrt = numeric())
  
  for (i in seq_len(nrow(tidy_data))) {
    temp_data <- kuadrat_bobot %>%
      filter(id == tidy_data$id[i])
    
    temp_sum <- sum(temp_data$tf_idf)
    temp_sqrt <- sqrt(temp_sum)
    
    vektor <- vektor %>% rbind(data.frame(id = tidy_data$id[i],sum = temp_sum, sqrt = temp_sqrt))
  }
  
  vektor_predict <- vektor %>% filter(id == data_predict$id)
  
  cosine <- data.frame(id = integer(),
                       cosine = numeric())
  for (i in seq_len(nrow(clean_datatrain))) {
    temp_id <- clean_datatrain$id[i]
    temp_bobot <- bobot_training %>% filter(id == temp_id)
    temp_vektor <- vektor %>% filter(id == temp_id)
    temp_cosine <- temp_bobot$sum / (vektor_predict$sqrt * temp_vektor$sqrt)
    
    cosine <- cosine %>%rbind(data.frame(id = temp_id,cosine = temp_cosine))
  }
  
  k = 5
  check <- cosine %>% left_join(datatrain, by = "id") %>% select(id, cosine, sentiment) %>%
    arrange(desc(cosine)) %>% head(k)
  
  sentiment_predict <- check %>% count(sentiment)
  sentiment_predict <- sentiment_predict$sentiment[which.max(sentiment_predict$n)]
  
  result_predict$sentiment[j] <- sentiment_predict
}

write.csv(data_predict_full, "D:/Kuliah/Semester 5/Prak DS/pprojet/data_predict.csv", row.names = FALSE)

write.csv(clean_data_predict, "D:/Kuliah/Semester 5/Prak DS/pprojet/data_predict_clean.csv", row.names = FALSE)

write.csv(result_predict, "D:/Kuliah/Semester 5/Prak DS/pprojet/data_predict_result.csv", row.names = FALSE)