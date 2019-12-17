library(tidyverse)
library(tidytext)
data("stop_words")


setwd("D:/hansard")
getwd()


##
#memory.limit(10000000)
## Load data
speeches <- readRDS("hansard-1979-2018-v261.rds")

## create extra stop words

political_words <- data_frame(word=c("hon","friend","eá","á","â","eâ","pageâ","lineâ","insertâ","prime","minister","gentleman","government","people"))
## Filter by party leaders
speeches_selectedvars <- speeches %>%
  filter(str_detect(proper_name,c("Corbyn|Ed Miliband|Blair|Boris Johnson|Theresa May|David Cameron|Gordon Brown|Nick Clegg|Vince Cable|Farron|Harriet Harman|John Major|William Hague|Iain Duncan Smith|Michael Howard|Jo Swinson"))) %>% 
  unnest_tokens(word,speech) %>% select(pp_id,word_count,speech_date,year,proper_name,party,government,word,ministry) 

speeches_unnested <- speeches %>%
                     filter(str_detect(proper_name,c("Corbyn|Ed Miliband|Blair|Boris Johnson|Theresa May|David Cameron|Gordon Brown|Nick Clegg|Vince Cable|Farron|Harriet Harman|John Major|William Hague|Iain Duncan Smith|Michael Howard|Jo Swinson"))) %>% 
                     unnest_tokens(word,speech) %>% select(pp_id,word_count,speech_date,year,proper_name,party,government,word,ministry) %>% 
                     anti_join(stop_words) %>%
                     anti_join(political_words) %>%
                     filter(!str_detect(word, "\\d")) %>%
                     count(proper_name,word,sort = TRUE)


speeches_filtered_totalwords <- speeches_unnested %>% 
  group_by(proper_name) %>% 
  summarize(total = sum(n))


speeches_filtered <- left_join(speeches_unnested, speeches_filtered_totalwords)
speeches_filtered <- left_join(speeches_filtered, speeches_selectedvars)


saveRDS(speeches_unnested, "leaderspeeches.rds")

##### filter to speakers only
speechesexpanded_selectedvars <- speeches %>%
  filter(str_detect(proper_name,c("John Bercow|Jacob Rees-Mogg"))) %>% 
  unnest_tokens(word,speech) %>% select(pp_id,word_count,speech_date,year,proper_name,party,government,word,ministry) 

speechesexpanded_unnested <- speeches %>%
  filter(str_detect(proper_name,c("John Bercow|Jacob Rees-Mogg"))) %>% 
  unnest_tokens(word,speech) %>% select(pp_id,word_count,speech_date,year,proper_name,party,government,word,ministry) %>% 
  anti_join(stop_words) %>%
  anti_join(political_words) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(proper_name,word,sort = TRUE)


speechesexpanded_filtered_totalwords <- speechesexpanded_unnested %>% 
  group_by(proper_name) %>% 
  summarize(total = sum(n))


speechesexpanded_filtered <- left_join(speechesexpanded_unnested, speechesexpanded_filtered_totalwords)
speechesexpanded_filtered <- left_join(speechesexpanded_filtered, speechesexpanded_selectedvars)



saveRDS(speechesexpanded_unnested, "speakerspeeches.rds")


########################### words by leader

  speeches %>%
    filter(str_detect(proper_name,c("Corbyn|Ed Miliband|Blair|Boris Johnson|Theresa May|David Cameron|Gordon Brown|Nick Clegg|Vince Cable|Farron|Harriet Harman|John Major|William Hague|Iain Duncan Smith|Michael Howard|Jo Swinson"))) %>% 
    count(proper_name,speech_date,sort=TRUE)  %>%
    group_by(proper_name) %>%
    mutate(sumn = sum(n)) %>%
    ungroup(n) %>%
    mutate(proper_name=fct_reorder(proper_name,sumn)) %>%
    ggplot(aes(proper_name, n))+
    geom_col() +
    ylab("Incidents of speech") +
    xlab(NULL) +
    coord_flip() 



########################### most common words

## top words by leader

speeches_unnested %>%
  group_by(proper_name) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, proper_name)) %>%
  ggplot(aes(word, n, fill = factor(proper_name))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ proper_name, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()



## top words across all leaders

speeches_unnested %>%
  filter(n > 1500) %>%
  group_by(word) %>%
  mutate(sumn = sum(n)) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word,sumn)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Number of words") +
  coord_flip()




################################ create dataframe for correlation analysis
frequency <- speeches_unnested %>% 
  group_by(proper_name) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(proper_name, proportion) %>% 
  gather(proper_name, proportion, `David Cameron`:`William Hague`)




# Correlation of words between Boris and other leaders
ggplot(frequency, aes(x = proportion, y = `Boris Johnson`, color = abs(`Boris Johnson` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75")  +
  facet_wrap(~proper_name, nrow = 3) +
  theme(legend.position="none") +
  labs(y = "Boris Johnson", x = NULL)

#' correlation between individual leaders
cor.test(data = frequency[frequency$proper_name == "Tony Blair",],
         ~ proportion + `Boris Johnson`)

#create dataframe for corr chart
forcorrelation <- speeches_unnested %>% 
  group_by(proper_name) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(proper_name, proportion) %>%
  select(-word)


## get correlations
library("PerformanceAnalytics")
chart.Correlation(forcorrelation, histogram=FALSE, pch=80)

## correlation matrix
res <- cor(forcorrelation,use="na.or.complete")
round(res, 2)


####
source("http://www.sthda.com/upload/rquery_cormat.r")
library(corrplot)

rquery.cormat(forcorrelation, type="upper")




########################################### add sentiments

speeches_filtered_sentiments <- speeches_filtered %>% 
                     inner_join(get_sentiments("afinn")) %>%
                     inner_join(get_sentiments("nrc")) %>%
                     rename(nrc=sentiment) %>%
                     inner_join(get_sentiments("bing")) %>%
                     rename(bing=sentiment)

#### sentiments over time
 speeches_filtered_sentiments %>%
                              count(proper_name,index=year,bing) %>%
                              spread(`bing`,n,fill=0) %>%
                              mutate(sentiment = positive - negative) %>%
                              ggplot(aes(index,sentiment,fill=proper_name)) +
                              geom_col(show.legend = FALSE) +
                              facet_wrap(~proper_name,ncol=2,scales="fixed")


 speeches_filtered_sentiments %>%
   count(proper_name,index=year,bing) %>%
   spread(`bing`,n,fill=0) %>%
   mutate(sentiment = ((positive/(positive+negative))-(negative/(positive+negative)))) %>%
   ggplot(aes(index,sentiment,fill=proper_name)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~proper_name,ncol=2,scales="fixed")+
   labs(title="Sentiment over time by political leader",subtitle = "Net percentage positivity")+
   scale_y_continuous(labels = scales::percent) +
   xlab("") +
   ylab("Net positive sentiment")
 
 
 ### most common positive and negative words
speeches_filtered_sentiments %>%
  count(word,sort=TRUE,bing) %>%
  ungroup() %>%
  group_by(bing) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=bing)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~bing,scales="free_y") +
  labs(y="Contribution to sentiment",
       x=NULL) +
  coord_flip()

### most common words by emotion
speeches_filtered_sentiments %>%
  count(word,sort=TRUE,nrc) %>%
  ungroup() %>%
  group_by(nrc) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=nrc)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~nrc,scales="free_y",ncol = 5) +
  labs(y="Contribution to sentiment",
       x=NULL) +
  coord_flip()


### word cloud sentiment

library(reshape2)
library(wordcloud)

dev.new(5,5)

speeches_filtered_sentiments %>%
  filter(proper_name=="Boris Johnson") %>%
  count(word,sort=TRUE,bing) %>%
  acast(word ~ bing, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),scale=c(4,.5),
                   max.words = 100)

#### share of emotions 

speeches_filtered_sentiments %>%
  count(nrc,sort=TRUE) %>%
  ungroup() %>%
  filter(!nrc=="positive") %>%
  filter(!nrc=="negative") %>%
  mutate(nrc = fct_reorder(nrc,n)) %>%
  ggplot(aes(nrc,n)) +
  geom_col(show.legend=FALSE) +
  coord_flip()

#### most common emotions 

speeches_filtered_sentiments %>%
  count(nrc,sort=TRUE) %>%
  ungroup() %>%
  filter(!nrc=="positive") %>%
  filter(!nrc=="negative") %>%
  mutate(nrc = fct_reorder(nrc,n)) %>%
  ggplot(aes(nrc,n)) +
  geom_col(show.legend=FALSE) +
  coord_flip()

#### most common emotions by politician

speeches_filtered_sentiments %>%
  count(proper_name,nrc,sort=TRUE) %>%
  ungroup() %>%
  filter(!nrc=="positive") %>%
  filter(!nrc=="negative") %>%
  mutate(nrc = reorder_within(nrc,n,proper_name)) %>%
  ggplot(aes(nrc,n,fill=factor(proper_name))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ proper_name, scales = "free") +
  coord_flip() +
  scale_x_reordered()


############################### calculate and plot tf-idf

speeches_filtered_tfidf <- speeches_unnested %>%
  bind_tf_idf(word,proper_name,n) %>%
  arrange(desc(tf_idf))


speeches_filtered_tfidf %>%
  filter(str_detect(proper_name,c("Vince Cable|Farron|Harriet Harman|John Major|William Hague|Iain Duncan Smith|Michael Howard|Jo Swinson"))) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(proper_name) %>%
  top_n(15) %>% 
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = proper_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~proper_name, ncol = 2, scales = "free") +
  coord_flip()


## calculate tf-idf for mogg and bercow

speechesexpanded_filtered_tfidf <- speechesexpanded_unnested %>%
  bind_tf_idf(word,proper_name,n) %>%
  arrange(desc(tf_idf))


speechesexpanded_filtered_tfidf %>%
  filter(proper_name=="John Bercow") %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(proper_name) %>%
  top_n(75) %>% 
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = proper_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~proper_name, ncol = 2, scales = "free") +
  coord_flip()


speechesexpanded_filtered_tfidf %>%
  filter(proper_name=="Jacob Rees-Mogg") %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(proper_name) %>%
  top_n(75) %>% 
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = proper_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~proper_name, ncol = 2, scales = "free") +
  coord_flip()



########################## LASSO Regression ############################################################


library(Matrix)

### for lasso
lasso_y <- speeches %>%
  filter(proper_name=="Tony Blair"|proper_name=="Jeremy Corbyn") %>%
  mutate(document = row_number())

# cast into a Matrix object
words_sparse <- lasso_y %>%
  unnest_tokens(word,speech,token="ngrams",n=1) %>%
  anti_join(stop_words) %>%
  anti_join(political_words) %>%
  filter(!str_detect(word, "\\d")) %>%
  group_by(word) %>%
  filter(n() > 35) %>%
  ungroup() %>%
  count(document, word) %>%
  cast_sparse(document,word, n)

dim(words_sparse)

words_sparse_rownames <- as.integer(rownames(words_sparse))

words_joined <- data_frame(document = words_sparse_rownames) %>%
  left_join(lasso_y %>%
              select(document,proper_name))


library(glmnet)

is_johnson <- words_joined$proper_name == "Jeremy Corbyn"
model <- cv.glmnet(words_sparse, is_johnson,
                   family = "binomial", keep = TRUE
)

plot(model)
plot(model$glmnet.fit)

library(broom)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

coefs %>%
  group_by(estimate > 0) %>%
  top_n(25, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Words which have the highest/lowest probability of being spoken by Corbyn rather than Blair"
  )