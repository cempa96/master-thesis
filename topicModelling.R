install.packages(c("tm", "topicmodels", "pdftools", "caret", "randomForest", "quanteda", "modelsummary", "logistf", "car"))
devtools::install_github("zsiders/EnsembleRandomForests")
library("tidyverse")
library("tm")
library("topicmodels")




# --------------------------------------------------------------------------------------------------------------
# ---------------- FUNCTIONS -----------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Function that does the following:
  # Create a corpus with the tm package using the argument pdfFolder which is the path to a folder with PDF documents.
  # For each document in the corpus, extract the dnr-number, i.e the case number.
  # Rename the corpus documents and the pdf-documents with the dnr-number. 
createCorpus <- function(pdfFolder) {

  corpus <- Corpus(DirSource(pdfFolder),
                   readerControl = list(reader = readPDF(), language = "swe"))
  
  dnrs <- as.character()
  fileList <- list.files(pdfFolder, full.names = T)
  
  for (i in 1:length(corpus)) {
    dnrs[i] <- str_extract(corpus[[i]]$content[1], "(?<=Dnr: )\\d{2}/\\d{5}")
    names(corpus)[i] <- paste0(gsub("/", "_", dnrs[i]))
    newFileName <- paste0(gsub("/", "_", dnrs[i]), ".PDF")
    newFilePath <- file.path(pdfFolder, newFileName)
    file.rename(fileList[i], newFilePath)
  }
  
  return(corpus)
}

# Function that takes a corpus, removes unwanted paragraphs, sentences, words and other characters
# and returns a clean version of the corpus that can then be used in the topic model.
cleanCorpus <- function(corpus) {
  words <- c("granskningsnämnden", "bedömning", "ärendet", "nämnden", "beslut", "saken",
             "anmälan", "föreläggande", "programföretagets", "yttrande", "dnr", "tel",
             "fax", "box", "stockholmgloben", "registratormrtvse", "registratormprtse",
             "besöksadress", "arenavägen", "wwwmrtvse", "wwwmprtse", "comfact", "signature",
             "referensnummer", "se", "kabyssgatan", "programmet", "anmälarens", "kommentar",
             "saklighet", "opartiskhet", "inslaget", "inslagen", "strider", "ärendena",
             "kravet", "kraven", "frias", "fälls", "inslag", "fria", "anmälaren", "anmälda",
             "svt", "uppgift", "redaktionen", "fått", "fick", "även", "reportern", "anser",
             "enligt", "fråga", "bakgrund", "aktuella", "granskat", "det", "följande",
             "annat", "bland", "också", "ska", "finns", "och", "men", "rapport", "anför",
             "visad", "medför", "konstaterar", "myndigheten", "kommit", "uppgiften",
             "handlade", "sändes", "nyhet", "sade", "rättelse", "vill", "därefter", "andra",
             "sätt", "får", "del", "många", "reporter", "flera", "medverkande", "uttalanden",
             "programföretaget", "heller", "gjord", "kritisk", "program", "därför", "programserien",
             "programledarna", "programledaren", "programledare", "nej", "lyssnarna", "genomslagskraft",
             "särskilda", "bestämmelsen", "televisionen", "mediet", "otillbörligt", "gynnande",
             "agenda", "ekot", "lunchekot", "morgonekot", "mer", "olika", "kommer", "programmen",
             "the", "aktuellt", "sammanfattningsvis", "sändningen", "osakligt", "anmälningarna",
             "anmälarna", "sändningstillstånd", "åtgärd", "pröva", "tvlagen", "anfört", "ställas",
             "innebär", "två", "alltså", "fann", "genom", "sitt", "emot")
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\n", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\\\n", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub('”', " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub('“', " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub('″', " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("’", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("‘", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub('–', " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("…", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("§", " ", x, perl = T)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("AKTUELL(A)? BESTÄMMELSE(R)?(.*?)(?=[A-Z]{5,})", " ", x, perl = T)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("Detta beslut har(.*)", " ", x, perl = T)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("Myndigheten för press.*?www\\.mprt\\.se", " ", x, perl = T)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("Myndigheten för radio.*?www\\.mrtv\\.se", " ", x, perl = T)))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("swedish"))
  corpus <- tm_map(corpus, removeWords, words)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument, language = "swedish")
  return(corpus)
}

# Function that takes a corpus and a category of misconduct, filters the corpus for documents that have been
# investigated for the given category and returns a filtered corpus.
filterCorpus <- function(corpus, category) {
  corpus <- tm_map(corpus, content_transformer(function(x) str_flatten(x)))
  sections <- tm_map(corpus, content_transformer(function(x) regmatches(x, gregexpr("BESLUT(.*)_____", x))))
  sections <- tm_filter(sections, function(x) any(str_detect(content(x), category)))
  indices <- meta(corpus, "id") %in% meta(sections, "id")
  filteredCorpus <- corpus[indices]
  return(filteredCorpus)
}



# --------------------------------------------------------------------------------------------------------------
# ---------------- CORPORA -------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# Create a corpus of all documents.
mainCorpus <- createCorpus("pdfs")

# Check whether all corpus documents were renamed correctly.
which(str_length(meta(mainCorpus, "id")) != 12)

# If not, change manually. For example:
#mainCorpus[[1976]]$content
#mainCorpus[[1976]]$meta
#meta(mainCorpus[[1976]], "id") <- "18_03521"


# Check whether all pdf files were renamed correctly.
which(str_length(list.files("pdfs")) != 12)


# Create two separate corpora with the documents that mention "objectivity" and "impartiality".
objCorpus <- filterCorpus(mainCorpus, "saklighet")
impCorpus <- filterCorpus(mainCorpus, "opartiskhet")




# Inspect the result of some randomly chosen documents.
inspect(objCorpus[[sample(length(objCorpus),1)]])
inspect(impCorpus[[sample(length(impCorpus),1)]])




# --------------------------------------------------------------------------------------------------------------
# ---------------- DATA FRAMES ---------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Load the metadata about the cases.
metadata <- read.csv("final_df.csv", sep = ";") %>% 
  distinct(.)

# Create a data frame with the objectivity cases with a variable that contains summary information about the cases.
objCases <- data.frame(case = str_replace(names(objCorpus), ".PDF", ""), 
                       summary = str_extract(content(objCorpus), "(?<=SAKEN).*(?=___)")) %>% 
  mutate(summary = str_squish(summary))

# Merge the two to get metadata for the corpus documents.
objDF <- inner_join(metadata, objCases, by = "case") %>% 
  mutate(decision = case_when(
    grepl("(?<!inte |inte att det |inte att inslaget )(\\n)*strider(\\n)* mot(\\n)* krave(n|t)(\\n)* på(\\n|\\s|^\\.)*saklighet", summary, perl = T) ~ "Conviction", 
    TRUE ~ "Acquittal"))

# Check for and remove duplicates.
#objDuplicates <- objDF %>% count(case) %>% filter(n > 1)
#objDF %>% filter(case %in% objDuplicates$case) %>% view()
objDF <- distinct(objDF)

rm(objCases)

# What proportion was convicted?
objDF %>%
  filter(decision == "Conviction") %>%
  summarize(propConviction = n() / nrow(objDF))

# Check for mislabeled cases.
objDF %>% 
  filter(decision == "Acquittal" & grepl("fälls", summary, ignore.case = T)) %>%
  select(case, decision, summary) %>%
  view()

# Change these manually.
objDF <- objDF %>%
  mutate(decision = case_when(
    case %in% c("14_01159", "13_02860", "16_00753", "15_01881", "13_02554", 
               "15_00797", "14_00829", "15_01528", "17_01058", "17_03923", 
               "14_01973", "13_00588", "14_01339", "14_00430", "21_03599",
               "13_00222") ~ "Conviction",
    TRUE ~ decision))

# Again check for mislabeled cases. 
objDF %>% 
  filter(decision == "Conviction" & grepl("frias", summary, ignore.case = T)) %>%
  select(case, decision, summary) %>%
  view()




# Same procedure for the other corpus
impCases <- data.frame(case = str_replace(names(impCorpus), ".PDF", ""), 
                       summary = str_extract(content(impCorpus), "(?<=SAKEN).*(?=___)")) %>% 
  mutate(summary = str_squish(summary))

impDF <- inner_join(metadata, impCases, by = "case") %>% 
  mutate(decision = case_when(
    grepl("(?<!inte |inte att det |inte att inslaget )(\\n)*strider(\\n)* mot(\\n)* krave(n|t)(\\n)* på(\\n|\\s|^\\.)*opartiskhet", summary, perl = T) ~ "Conviction", 
    TRUE ~ "Acquittal"))

#impDuplicates <- impDF %>% count(case) %>% filter(n > 1)
#impDF %>% filter(case %in% impDuplicates$case) %>% view()
impDF <- distinct(impDF)

rm(impCases)

impDF %>%
  filter(decision == "Conviction") %>%
  summarize(propConviction = n() / nrow(impDF))

impDF %>% 
  filter(decision == "Acquittal" & grepl("fälls", summary, ignore.case = T)) %>%
  select(case, decision, summary) %>%
  view()

impDF <- impDF %>%
  mutate(decision = case_when(
    case %in% c("13_00618", "16_01737", "17_02686", "14_01469", "17_01477", 
                "13_00487", "13_00636", "13_00994", "13_02040", "13_02554", 
                "13_02868", "14_00804", "14_00808", "14_00829", "14_01339", 
                "14_01973", "14_02147", "14_02894", "14_02898", "15_00461", 
                "15_01064", "15_01398", "15_01528", "15_02619", "15_02658", 
                "16_00361", "16_00456", "16_00867", "17_00208", "17_00395", 
                "17_01025", "17_01477", "17_01572", "17_02280", "17_02686", 
                "17_03737", "13_00289", "13_00222") ~ "Conviction",
    TRUE ~ decision))

impDF %>% 
  filter(decision == "Conviction" & grepl("frias", summary, ignore.case = T)) %>%
  select(case, decision, summary) %>%
  view()

impDF <- impDF %>%
  mutate(decision = case_when(
    case %in% c("16_01330", "16_01445", "13_01205", "16_01295") ~ "Acquittal",
    TRUE ~ decision))

rm(metadata)

#write_csv(objDF, "objDF.csv")
#write_csv(impDF, "impDF.csv")




# --------------------------------------------------------------------------------------------------------------
# ---------------- TOPIC MODELING ------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Clean the corpora.
objCorpus <- cleanCorpus(objCorpus)
impCorpus <- cleanCorpus(impCorpus)

# Create document-term-matrices.
objDTM <- DocumentTermMatrix(objCorpus)
impDTM <- DocumentTermMatrix(impCorpus)

summary(colSums(as.matrix(objDTM)))
summary(colSums(as.matrix(impDTM)))

# Create models with different number of topics, calculate perplexity for each and fit the final model
# with the optimal number of topics.
set.seed(1996)
nrTopics <- seq(50, 150, 10)
objPerp <- c()
objRowIDs <- 1:nrow(objDTM)
objTrainIDs <- sample(x = objRowIDs, size = 0.8*length(objRowIDs), replace = F)
objTestIDs <- objRowIDs[!objRowIDs %in% objTrainIDs]
objTrainDTM <- objDTM[objTrainIDs,]
objTestDTM <- objDTM[-objTrainIDs,]
rm(objRowIDs, objTrainIDs, objTestIDs)


for(k in 1:length(nrTopics)) {
  lda <- LDA(x = objTrainDTM,
                    k = nrTopics[k], 
                    method = "Gibbs")
  
  objPerp[k] <- perplexity(object = lda, newdata = objTestDTM)
  print(objPerp)
}

rm(objTestDTM, objTrainDTM, k, lda)

objLDA <- LDA(x = objDTM,
             k = nrTopics[which.min(objPerp)],
             method = "Gibbs")

objLDA_post <- posterior(objLDA)





impPerp <- c()
impRowIDs <- 1:nrow(impDTM)
impTrainIDs <- sample(x = impRowIDs, size = 0.8*length(impRowIDs), replace = F)
impTestIDs <- impRowIDs[!impRowIDs %in% impTrainIDs]
impTrainDTM <- impDTM[impTrainIDs,]
impTestDTM <- impDTM[-impTrainIDs,]
rm(impRowIDs, impTrainIDs, impTestIDs)


for(k in 1:length(nrTopics)) {
  lda <- LDA(x = impTrainDTM,
                    k = nrTopics[k], 
                    method = "Gibbs")
  
  impPerp[k] <- perplexity(object = lda, newdata = impTestDTM)
  print(impPerp)
}

rm(impTestDTM, impTrainDTM, k, lda)

impLDA <- LDA(x = impDTM,
             k = nrTopics[which.min(impPerp)],
             method = "Gibbs")

impLDA_post <- posterior(impLDA)

plot(nrTopics, objPerp, type = "l")
plot(nrTopics, impPerp, type = "l")




# --------------------------------------------------------------------------------------------------------------
# ---------------- INSPECT RESULTS & UPDATE DATA FRAMES --------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Get the topic distributions over terms.
objTopicDistOverTerms <- data.table::data.table(topic = 1:nrTopics[which.min(objPerp)], 
                                                objLDA_post$terms)

objTopicDistOverTerms <- data.table::melt.data.table(objTopicDistOverTerms,
                                                     id.vars = "topic")
# Get the top ten terms for each topic.
objTop10 <- objTopicDistOverTerms %>% 
  group_by(topic) %>% 
  slice_max(order_by = value, n = 10, with_ties = F)


objTop10 %>%
  filter(topic %in% c(3, 10, 24, 31, 43, 49, 53, 56, 57)) %>%
  mutate(
    topic_label = case_when(
      topic == 56 ~ "Topic: Covid",
      topic == 31 ~ "Topic: USA/Trump",
      topic == 3 ~ "Topic: GenderDysphoria/Trans",
      topic == 10 ~ "Topic: Sweden Democrats",
      topic == 24 ~ "Topic: Climate",
      topic == 57 ~ "Topic: Energy production",
      topic == 53 ~ "Topic: Islam",
      topic == 49 ~ "Topic: Russia/Ukraine",
      topic == 43 ~ "Topic: Israel/Palestine",
      TRUE ~ as.character(topic)),
    translation = case_when(
      variable == "barn" ~ "child/children",
      variable == "könsdysfori" ~ "gender dysphoria",
      variable == "behandl" ~ "treatment",
      variable == "unga" ~ "youth",
      variable == "granskn" ~ "review",
      variable == "uppdrag" ~ "mission",
      variable == "vården" ~ "healthcare",
      variable == "bup" ~ "child psychiatric care",
      variable == "sverigedemokraterna" ~ "sweden democrats",
      variable == "partiet" ~ "the party",
      variable == "politiska" ~ "political",
      variable == "partiledar" ~ "party leader",
      variable == "parti" ~ "party",
      variable == "politik" ~ "politics",
      variable == "partier" ~ "parties",
      variable == "valet" ~ "the election",
      variable == "politisk" ~ "political",
      variable == "världen" ~ "the world",
      variable == "klimatförändringar" ~ "climate change",
      variable == "klimatet" ~ "the climate",
      variable == "havet" ~ "the ocean",
      variable == "forskar" ~ "researcher",
      variable == "människan" ~ "humans",
      variable == "västlänken" ~ "the west link",
      variable == "senast" ~ "recently",
      variable == "klimat" ~ "climate",
      variable == "amerikanska" ~ "american",
      variable == "amerikansk" ~ "american",
      variable == "presidenten" ~ "the president",
      variable == "korrespondenten" ~ "correspondent",
      variable == "granskningsnämnden" ~ "broadcasting commission",
      variable == "presidentvalet" ~ "presidential election",
      variable == "skogen" ~ "forest",
      variable == "skog" ~ "forest",
      variable == "skogsstyrelsen" ~ "the swedish forest agency",
      variable == "konflikten" ~ "the conflict",
      variable == "israeliska" ~ "israeli",
      variable == "ryssland" ~ "russia",
      variable == "ukraina" ~ "ukraine",
      variable == "ryska" ~ "russian",
      variable == "rysk" ~ "russian",
      variable == "syrien" ~ "syria",
      variable == "kriget" ~ "the war",
      variable == "regimen" ~ "the regime",
      variable == "tidningen" ~ "the newspaper",
      variable == "medier" ~ "media",
      variable == "webbplatsen" ~ "the website",
      variable == "svenska" ~ "swedish",
      variable == "svensk" ~ "swedish",
      variable == "avpixlat" ~ "avpixlat",
      variable == "muslimska" ~ "muslim",
      variable == "svenskarna" ~ "the Swedes",
      variable == "procent" ~ "percent",
      variable == "visad" ~ "shown",
      variable == "dag" ~ "day",
      variable == "statistik" ~ "statistics",
      variable == "missvisand" ~ "misleading",
      variable == "vindkraft" ~ "wind power",
      variable == "vindkraftverk" ~ "wind turbine",
      variable == "kärnkraft" ~ "nuclear power",
      variable == "vindkraften" ~ "wind power",
      variable == "naturvårdsverket" ~ "environmental protection agency",
      variable == "energi" ~ "energy",
      variable == "antalet" ~ "the number",
      variable == "sverig" ~ "sweden",
      TRUE ~ as.character(variable))) %>% 
  ggplot(.,aes(y = factor(translation), x = value)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") + 
  facet_wrap(~topic_label, scales = "free", ncol = 3, nrow = 3) +
  labs(
    title = "Figure 6.1: Top ten terms for a selection of topics, objectivity corpus.",
    caption = "Note: Translations are approximate.",
    x = "Proportion of topic",
    y = "Term") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Times"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        plot.caption = element_text(hjust = 0, size = 10, family = "Times"))


# Get the document distributions over topics.
objDocDistOverTopics <- data.frame(case = gsub(".PDF", "", objLDA@documents),
                                  objLDA_post$topics,
                                  row.names = NULL) %>% 
  rename_with(., ~ paste0("Topic",1:objLDA@k), starts_with("X"))
              
# Inspect the case with the highest proportion of a selected topic.
#objDocDistOverTopics %>% 
  #arrange(desc(Topic6)) %>%
  #slice(1) %>% 
  #select(case)

#objCorpus[["18_01710.PDF"]][["content"]]

# Update data frame with the topic distributions. Also add variable "year" and re-code decision variable.
# NOTE: The LDA does not yield identical results for each run, even with a specific seed. The topics that
# are removed and renamed here will not be the same topics during a different run. 
objDFfinal <- inner_join(objDF, objDocDistOverTopics, by = "case") %>%
  mutate(year = str_extract(date, "\\d{4}"),
         decision = ifelse(decision == "Conviction", 1, 0)) %>%
  select(case, channel, program, year, date, title, summary, decision_date, decision, starts_with("topic")) %>%
  rename(top_Covid = Topic56,
         top_Climate = Topic24,
         top_GenderDys_Trans = Topic3,
         top_Energy = Topic57,
         top_USA_Trump = Topic31,
         top_SwedDems = Topic10,
         top_Islam = Topic53,
         top_Israel_Pal = Topic43,
         top_Rus_Ukr = Topic49) %>% 
  select(-c(Topic1, Topic33, Topic50, Topic54)) %>% 
  distinct(., case, .keep_all = T) %>% 
  arrange(case)




# Same procedure for the other data frame. Note however that the number of topics and therefore their content
# differs from the previous model.
impTopicDistOverTerms <- data.table::data.table(topic = 1:nrTopics[which.min(impPerp)],
                                                impLDA_post$terms)

impTopicDistOverTerms <- data.table::melt.data.table(impTopicDistOverTerms,
                                                     id.vars = "topic")

impTop10 <- impTopicDistOverTerms %>% 
  group_by(topic) %>% 
  slice_max(order_by = value, n = 10, with_ties = F)

impTop10 %>% 
  filter(topic %in% c(3, 7, 8, 50, 54, 67, 73, 76, 82)) %>%
  mutate(
    topic_label = case_when(
      topic == 54 ~ "Topic: NATO",
      topic == 67 ~ "Topic: USA/Trump",
      topic == 8 ~ "Topic: Refugees",
      topic == 73 ~ "Topic: Sweden Democrats",
      topic == 82 ~ "Topic: Climate",
      topic == 3 ~ "Topic: Energy production",
      topic == 76 ~ "Topic: Islam/Right-wing extremism",
      topic == 7 ~ "Topic: Russia/Ukraine",
      topic == 50 ~ "Topic: Israel/Palestine",
      TRUE ~ as.character(topic)),
    translation = case_when(
      variable == "vindkraft" ~ "wind power",
      variable == "vindkraftverk" ~ "wind turbine",
      variable == "vindkraften" ~ "the wind power",
      variable == "kärnkraft" ~ "nuclear power",
      variable == "energi" ~ "energy",
      variable == "svensk" ~ "swedish",
      variable == "kärnkraften" ~ "nuclear power",
      variable == "bygga" ~ "to build",
      variable == "ryssland" ~ "russia",
      variable == "ukraina" ~ "ukraine",
      variable == "ryska" ~ "russian",
      variable == "rysk" ~ "russian",
      variable == "kriget" ~ "the war",
      variable == "landet" ~ "the country",
      variable == "ukrainska" ~ "ukrainian",
      variable == "krim" ~ "crimea",
      variable == "asylsökand" ~ "asylum seeker",
      variable == "flyktingar" ~ "refugees",
      variable == "brott" ~ "crime",
      variable == "kriminologiprofessorn" ~ "the criminology professor",
      variable == "migrationsverket" ~ "the migration agency",
      variable == "veckan" ~ "the week",
      variable == "professorn" ~ "the professor",
      variable == "nyanlända" ~ "newcomers",
      variable == "uppehållstillstånd" ~ "residence permit",
      variable == "asyl" ~ "asylum",
      variable == "konflikten" ~ "the conflict",
      variable == "israeliska" ~ "israeli",
      variable == "palestinska" ~ "palestinian",
      variable == "dokumentären" ~ "the documentary",
      variable == "ensamkommand" ~ "unaccompanied",
      variable == "svenskt" ~ "swedish",
      variable == "medlemskap" ~ "membership",
      variable == "natomedlemskap" ~ "nato membership",
      variable == "psykologen" ~ "the psychologist",
      variable == "flyktingbarn" ~ "refugee children",
      variable == "psykolog" ~ "psychologist",
      variable == "nackdelar" ~ "disadvantages",
      variable == "amerikanska" ~ "american",
      variable == "presidenten" ~ "the president",
      variable == "granskningsnämnden" ~ "broadcasting commission",
      variable == "presidentvalet" ~ "presidential election",
      variable == "amerikansk" ~ "american",
      variable == "demokraterna" ~ "the democrats",
      variable == "sverigedemokraterna" ~ "sweden democrats",
      variable == "partiet" ~ "the party",
      variable == "partiledar" ~ "party leader",
      variable == "politiska" ~ "political",
      variable == "politik" ~ "politics",
      variable == "parti" ~ "party",
      variable == "partier" ~ "parties",
      variable == "valet" ~ "the election",
      variable == "moderaterna" ~ "the moderates",
      variable == "organisation" ~ "organization",
      variable == "högerextrema" ~ "far-right",
      variable == "åsikter" ~ "opinions",
      variable == "tidningen" ~ "the newspaper",
      variable == "nazistiska" ~ "nazi",
      variable == "nazist" ~ "nazi",
      variable == "världen" ~ "the world",
      variable == "klimatförändringar" ~ "climate change",
      variable == "havet" ~ "the ocean",
      variable == "klimatet" ~ "the climate",
      variable == "klimatförändringarna" ~ "climate changes",
      variable == "jorden" ~ "the earth",
      variable == "människan" ~ "humans",
      variable == "klimat" ~ "climate",
      variable == "palestina" ~ "palestine",
      variable == "svenskarna" ~ "the swedes",
      TRUE ~ as.character(variable))) %>% 
  ggplot(.,aes(y = factor(translation), x = value)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") + 
  facet_wrap(~topic_label, scales = "free", ncol = 3, nrow = 3) +
  labs(
    title = "Figure 6.2: Top ten terms for a selection of topics, impartiality corpus.",
    caption = "Note: Translations are approximate.",
    x = "Proportion of topic",
    y = "Term") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Times"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        plot.caption = element_text(hjust = 0, size = 10, family = "Times"))



impDocDistOverTopics <- data.frame(case = gsub(".PDF", "", impLDA@documents),
                                   impLDA_post$topics,
                                   row.names = NULL) %>% 
  rename_with(., ~ paste0("Topic",1:impLDA@k), starts_with("X"))


#impDocDistOverTopics %>% 
  #arrange(desc(Topic6)) %>%
  #slice(1) %>% 
  #select(case)

#impCorpus[["18_01710.PDF"]][["content"]]


impDFfinal <- inner_join(impDF, impDocDistOverTopics, by = "case") %>%
  mutate(year = str_extract(date, "\\d{4}"),
         decision = ifelse(decision == "Conviction", 1, 0)) %>% 
  select(case, channel, program, year, date, title, summary, decision_date, decision, starts_with("topic")) %>%
  rename(top_USA_Trump = Topic67,
         top_Climate = Topic82,
         top_Israel_Pal = Topic50,
         top_SwedDems = Topic73,
         top_Nato = Topic54,
         top_Rus_Ukr = Topic7,
         top_Energy = Topic3,
         top_Islam_Racism = Topic76,
         top_Refugees = Topic8) %>% 
  select(-c(Topic1, Topic22, Topic65, Topic78)) %>%
  distinct(., case, .keep_all = T) %>%
  arrange(case)




# --------------------------------------------------------------------------------------------------------------
# ---------------- RANDOM FOREST AND LOGIT ---------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
library(caret)
library(randomForest)

# Use a regular random forest to determine the most important topics.
objRF <-  objDFfinal %>% select(decision, starts_with("top")) %>%
  train(as.factor(decision) ~ .,
        data = .,
        method = "rf",
        trControl = trainControl(method = "cv", number = 5))

# Confusion matrix
objRF$finalModel$confusion




# An alternative that is better at predicting with imbalanced data due to an additional bagging step.
library("EnsembleRandomForests")

# Create model with same parameters as the previous.
objERF <- ens_random_forests(df = objDFfinal, 
                             var = "decision",
                             covariates = objDFfinal %>% select(starts_with("top")) %>% colnames(),
                             header = "case",
                             n.forests = 1,
                             ntree = 500,
                             mtry = 2,
                             save = F)

# Confusion matrix looks better. This is the model we'll use moving forward.
objERF$model[[1]]$mod$confusion
objERF$ens.perf$auc
objERF$ens.perf$tss
objERF$ens.perf$rmse

# Variable importance data frame with importance as relative to the most important variable.
objVarImpDF <- as.data.frame(objERF[["var.imp.raw"]]) %>%
  slice(-nrow(.)) %>%
  mutate(Importance = format(V1/max(V1), scientific = F)) %>%
  arrange(desc(Importance)) %>% 
  select(-V1) %>%
  rownames_to_column() %>% 
  rename(Topic = rowname) %>% 
  slice_head(., n = 9)


# Use the selected topics to fit logistic regression models.
library("logistf")
library("modelsummary")

# Standard logit
objLogit <- objDFfinal %>% 
  select(decision, top_GenderDys_Trans, top_SwedDems, top_Climate, top_USA_Trump, top_Israel_Pal, 
         top_Rus_Ukr, top_Islam, top_Covid, top_Energy) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

# Firth's bias-reducing logit that is better at handling imbalanced data.
objLogitFirth <- objDFfinal %>% 
  select(decision, top_GenderDys_Trans, top_SwedDems, top_Climate, top_USA_Trump, top_Israel_Pal, 
         top_Rus_Ukr, top_Islam, top_Covid, top_Energy) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

# Two extensions to the previous model. Dplyr workflow not possible here
# so the variables have to be specified as a formula.
objLogitFlic <- 
  flic(decision ~ top_GenderDys_Trans + top_SwedDems + top_Climate + top_USA_Trump + top_Israel_Pal + 
       top_Rus_Ukr + top_Islam + top_Covid + top_Energy, 
       data = objDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

objLogitFlac <- 
  flac(decision ~ top_GenderDys_Trans + top_SwedDems + top_Climate + top_USA_Trump + top_Israel_Pal + 
       top_Rus_Ukr + top_Islam + top_Covid + top_Energy, 
       data = objDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

# Store the models in a data frame along with their AIC measures for comparison.
objLogitDF <- modelsummary(list("Standard" = objLogit, "Firth" = objLogitFirth, "FLIC" = objLogitFlic, "FLAC" = objLogitFlac),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(objLogit)[2], 3)), 
    Firth = as.character(round(extractAIC(objLogitFirth)[2], 3)),
    FLIC = as.character(round(extractAIC(objLogitFlic)[2], 3)), 
    FLAC = as.character(round(extractAIC(objLogitFlac)[2], 3))))


# Same procedure here but using the most important variables from the ERF to fit logistic regression models
objLogit2 <- objDFfinal %>% 
  select(decision, objVarImpDF[1:9,1]) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

objLogitFirth2 <- objDFfinal %>% 
  select(decision, objVarImpDF[1:9,1]) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

objLogitFlic2 <- 
  flic(decision ~ top_Energy + top_SwedDems + Topic29 + Topic66 + Topic47 + 
       Topic44 + Topic37 + Topic23 + Topic55, 
       data = objDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

objLogitFlac2 <- 
  flac(decision ~ top_Energy + top_SwedDems + Topic29 + Topic66 + Topic47 + 
       Topic44 + Topic37 + Topic23 + Topic55, 
       data = objDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))


objLogitDF2 <- modelsummary(list("Standard" = objLogit2, "Firth" = objLogitFirth2, "FLIC" = objLogitFlic2, "FLAC" = objLogitFlac2),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(objLogit2)[2], 3)), 
    Firth = as.character(round(extractAIC(objLogitFirth2)[2], 3)),
    FLIC = as.character(round(extractAIC(objLogitFlic2)[2], 3)), 
    FLAC = as.character(round(extractAIC(objLogitFlac2)[2], 3))))

# Inspect the models. 
objLogitDF %>% view()
objLogitDF2 %>% view()




# --------------------------------------------------------------------------------------------------------------
# ---------------- TRIGRAMS ------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Reset the corpus.
objCorpus <- filterCorpus(mainCorpus, "saklighet")

library(quanteda)

# Create a quanteda corpus from the original corpus. Here we only want the sections of the documents
# that corresponds to the commission's ruling and their reasoning behind it.
objCorpusQ <- 
  str_extract(content(objCorpus), "(?<=GRANSKNINGSNÄMNDENS BEDÖMNING)(.*)(?=Detta beslut har)") %>% 
  str_remove(., "\\|(.*)") %>%
  str_replace_all(., "\\\\n", " ") %>%
  corpus(.)

# Create a document-feature-matrix (which is what it's called in quanteda). Remove unwanted
# words and symbols and then create trigrams.
objDFM <- 
  tokens(objCorpusQ,
         remove_numbers = T,
         remove_separators = T,
         remove_symbols = T) %>%
  tokens_remove("[\\.\\/]", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("[-–�]", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove('[\\,:"”§&…]', valuetype = "regex", padding = F, verbose = T) %>%
  tokens_remove("\\]|\\[", valuetype = "regex", padding = F, verbose = T) %>%
  tokens_remove("(?i)inslag(et|en)", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)krav(et|en)", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)strider", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)opartiskhet", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)saklighet", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("((?i)gransknings)?(?i)nämnden(s)?", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove(stopwords("swedish"), padding  = F) %>%
  tokens_tolower() %>% 
  tokens_ngrams(n = 3) %>%
  dfm()

# Rename documents in the DFM.
objDFM@docvars$docname_ <- str_replace(names(objCorpus), ".PDF", "")

# Extract the 500 most common trigrams based on document frequency.
objTopNgrams <- topfeatures(objDFM, n = 500, scheme = "docfreq")

# Make it into a data frame and add the other relevant variables.
objTopNgramsDF <- convert(objDFM[,names(objTopNgrams)], to = "data.frame") %>% 
  rename(case = doc_id) %>% 
  filter(case %in% objDFfinal$case) %>%
  mutate(decision = objDFfinal$decision) %>% 
  select(case, decision, everything())

# Filter for trigrams that seem meaningful..
objTopNgramsDF <- objTopNgramsDF %>% 
  select(case, decision, 41, 43, 49, 54, 89, 92, 110, 114, 120, 127, 138, 147, 161, 166,
                         174, 175, 178, 181, 185, 197, 200, 203, 217, 220, 224, 226, 236, 
                         252, 262, 266, 275, 282, 291, 297, 302, 306, 309, 319, 325, 331, 
                         333, 340, 344, 364, 365, 366, 369, 378, 387, 390, 401, 410, 430, 
                         441, 446, 452, 454, 455, 456, 458, 470, 475, 481, 485, 488, 491,
                         492, 493, 497, 94, 198, 268, 281, 341, 336, 371, 397)

objTopNgramsDF <- objTopNgramsDF %>%
  rowwise() %>% 
  mutate(Response_Criticism = sum(c_across(c(11, 12, 18, 24, 26, 30, 40, 41, 55, 62, 64, 71))),
         Framing_Presentation = sum(c_across(c(7, 10, 13, 17, 20, 21, 22, 23, 27, 34, 35, 36, 39, 42, 58, 70))),
         PSB_Mission = sum(c_across(c(15, 16, 19, 25, 45, 46, 47, 48, 49, 51, 56, 57, 60, 61, 67, 68, 69))),
         Factual_Reporting = sum(c_across(c(53, 65, 72, 74, 75, 78,79))),
         Reporter_Expression = sum(c_across(c(3, 9, 14, 28, 29, 31, 33, 37, 44, 50, 52, 59, 66))),
         Guest_Expression = sum(c_across(c(3, 4, 5, 6, 8, 9, 14, 29, 32, 38, 50, 54))))




objLogit3 <- objTopNgramsDF %>% 
  select(decision, 80:ncol(.)) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

objLogitFirth3 <- objTopNgramsDF %>% 
  select(decision, 80:ncol(.)) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

objLogitFlic3 <- 
  flic(decision ~ Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression,
       data = objTopNgramsDF, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

objLogitFlac3 <- 
  flac(decision ~ Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression, 
       data = objTopNgramsDF, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))


objLogitDF3 <- modelsummary(list("Standard" = objLogit3, "Firth" = objLogitFirth3, "FLIC" = objLogitFlic3, "FLAC" = objLogitFlac3),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(objLogit3)[2], 3)), 
    Firth = as.character(round(extractAIC(objLogitFirth3)[2], 3)),
    FLIC = as.character(round(extractAIC(objLogitFlic3)[2], 3)), 
    FLAC = as.character(round(extractAIC(objLogitFlac3)[2], 3))))




objLogit4 <- objTopNgramsDF %>% 
  inner_join(., objDFfinal, by = c("case", "decision")) %>% 
  select(decision, top_GenderDys_Trans, top_SwedDems, top_Climate, top_USA_Trump, top_Israel_Pal, 
         top_Rus_Ukr, top_Islam, top_Covid, top_Energy, Response_Criticism, Framing_Presentation, PSB_Mission, 
         Factual_Reporting, Reporter_Expression, Guest_Expression) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

objLogitFirth4 <- objTopNgramsDF %>% 
  inner_join(., objDFfinal, by = c("case", "decision")) %>% 
  select(decision, top_GenderDys_Trans, top_SwedDems, top_Climate, top_USA_Trump, top_Israel_Pal, 
         top_Rus_Ukr, top_Islam, top_Covid, top_Energy, Response_Criticism, Framing_Presentation, PSB_Mission, 
         Factual_Reporting, Reporter_Expression, Guest_Expression,
         Response_Criticism, Framing_Presentation, PSB_Mission, Factual_Reporting, Reporter_Expression, Guest_Expression) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

objLogitFlic4 <- 
  flic(decision ~ top_GenderDys_Trans + top_SwedDems + top_Climate + top_USA_Trump + top_Israel_Pal + 
       top_Rus_Ukr + top_Islam + top_Covid + top_Energy + Response_Criticism + Framing_Presentation + 
       PSB_Mission + Factual_Reporting + Reporter_Expression + Guest_Expression,
       data = inner_join(objTopNgramsDF, objDFfinal, by = c("case", "decision")), 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

objLogitFlac4 <- 
  flac(decision ~ top_GenderDys_Trans + top_SwedDems + top_Climate + top_USA_Trump + top_Israel_Pal + 
       top_Rus_Ukr + top_Islam + top_Covid + top_Energy + Response_Criticism + Framing_Presentation + 
       PSB_Mission + Factual_Reporting + Reporter_Expression + Guest_Expression, 
       data = inner_join(objTopNgramsDF, objDFfinal, by = c("case", "decision")), 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))


objLogitDF4 <- modelsummary(list("Standard" = objLogit4, "Firth" = objLogitFirth4, "FLIC" = objLogitFlic4, "FLAC" = objLogitFlac4),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(objLogit4)[2], 3)), 
    Firth = as.character(round(extractAIC(objLogitFirth4)[2], 3)),
    FLIC = as.character(round(extractAIC(objLogitFlic4)[2], 3)), 
    FLAC = as.character(round(extractAIC(objLogitFlac4)[2], 3))))



# Same procedure for the impartiality corpus.
impERF <- ens_random_forests(df = impDFfinal, 
                             var = "decision",
                             covariates = impDFfinal %>% select(starts_with("top")) %>% colnames(),
                             header = "case",
                             n.forests = 1,
                             ntree = 500,
                             mtry = 2,
                             save = F)

impERF$model[[1]]$mod$confusion
impERF$ens.perf$auc
impERF$ens.perf$tss
impERF$ens.perf$rmse

impVarImpDF <- as.data.frame(impERF[["var.imp.raw"]]) %>%
  slice(-nrow(.)) %>%
  mutate(Importance = format(V1/max(V1), scientific = F)) %>%
  arrange(desc(Importance)) %>% 
  select(-V1) %>%
  rownames_to_column() %>% 
  rename(Topic = rowname) %>% 
  slice_head(., n = 9)




impLogit <- impDFfinal %>% 
  select(decision, top_Energy, top_Rus_Ukr, top_Refugees, top_Israel_Pal, top_Nato, 
         top_USA_Trump, top_SwedDems, top_Islam_Racism, top_Climate) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFirth <- impDFfinal %>% 
  select(decision, top_Energy, top_Rus_Ukr, top_Refugees, top_Israel_Pal, top_Nato, 
         top_USA_Trump, top_SwedDems, top_Islam_Racism, top_Climate) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFlic <- 
  flic(decision ~ top_Energy + top_Rus_Ukr + top_Refugees + top_Israel_Pal + top_Nato + top_USA_Trump + 
       top_SwedDems + top_Islam_Racism + top_Climate, 
       data = impDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

impLogitFlac <- 
  flac(decision ~ top_Energy + top_Rus_Ukr + top_Refugees + top_Israel_Pal + top_Nato + top_USA_Trump + 
       top_SwedDems + top_Islam_Racism + top_Climate, 
       data = impDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

impLogitDF <- modelsummary(list("Standard" = impLogit, "Firth" = impLogitFirth, "FLIC" = impLogitFlic, "FLAC" = impLogitFlac),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(impLogit)[2], 3)), 
    Firth = as.character(round(extractAIC(impLogitFirth)[2], 3)),
    FLIC = as.character(round(extractAIC(impLogitFlic)[2], 3)), 
    FLAC = as.character(round(extractAIC(impLogitFlac)[2], 3))))


impLogit2 <- impDFfinal %>% 
  select(decision, impVarImpDF[1:9,1]) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFirth2 <- impDFfinal %>% 
  select(decision, impVarImpDF[1:9,1]) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFlic2 <- 
  flic(decision ~ Topic71 + Topic81 + Topic40 + Topic14 + Topic4 + top_Energy + Topic57 + top_Climate + top_Israel_Pal, 
       data = impDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

impLogitFlac2 <- 
  flac(decision ~ Topic71 + Topic81 + Topic40 + Topic14 + Topic4 + top_Energy + Topic57 + top_Climate + top_Israel_Pal, 
       data = impDFfinal, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))


impLogitDF2 <- modelsummary(list("Standard" = impLogit2, "Firth" = impLogitFirth2, "FLIC" = impLogitFlic2, "FLAC" = impLogitFlac2),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(impLogit2)[2], 3)), 
    Firth = as.character(round(extractAIC(impLogitFirth2)[2], 3)),
    FLIC = as.character(round(extractAIC(impLogitFlic2)[2], 3)), 
    FLAC = as.character(round(extractAIC(impLogitFlac2)[2], 3))))


impLogitDF %>% view()
impLogitDF2 %>% view()



detach("package:quanteda", unload = T)
impCorpus <- filterCorpus(mainCorpus, "opartiskhet")
library("quanteda")

impCorpusQ <- 
  str_extract(content(impCorpus), "(?<=GRANSKNINGSNÄMNDENS BEDÖMNING)(.*)(?=Detta beslut har)") %>% 
  str_remove(., "\\|(.*)") %>%
  str_replace_all(., "\\\\n", " ") %>%
  corpus(.)

impDFM <- 
  tokens(impCorpusQ,
         remove_numbers = T,
         remove_separators = T,
         remove_symbols = T) %>%
  tokens_remove("[\\.\\/]", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("[-–�]", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove('[\\,:"”§&…]', valuetype = "regex", padding = F, verbose = T) %>%
  tokens_remove("\\]|\\[", valuetype = "regex", padding = F, verbose = T) %>%
  tokens_remove("(?i)inslag(et|en)", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)krav(et|en)", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)strider", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)opartiskhet", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("(?i)saklighet", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove("((?i)gransknings)?(?i)nämnden(s)?", valuetype = "regex", padding = T, verbose = T) %>%
  tokens_remove(stopwords("swedish"), padding  = F) %>%
  tokens_tolower() %>% 
  tokens_ngrams(n = 3) %>%
  dfm()

impDFM@docvars$docname_ <- str_replace(names(impCorpus), ".PDF", "")

impTopNgrams <- topfeatures(impDFM, n = 500, scheme = "docfreq")

impTopNgramsDF <- convert(impDFM[,names(impTopNgrams)], to = "data.frame") %>% 
  rename(case = doc_id) %>% 
  filter(case %in% impDFfinal$case) %>%
  mutate(decision = impDFfinal$decision) %>% 
  select(case, decision, everything())

impTopNgramsDF <- impTopNgramsDF %>%
  rowwise() %>% 
  mutate(Response_Criticism = sum(c_across(c(
          "fick_möjlighet_bemöta", "möjlighet_bemöta_kritik", "möjlighet_bemöta_kritiken", "tillfälle_bemöta_kritiken",   
          "klart_utpekad_part", "ges_tillfälle_bemöta", "tillräcklig_möjlighet_bemöta", "bemöta_kommentera_kritiken",  
          "krävde_möjlighet_bemötande", "kritik_klart_utpekad", "bemöta_kritik_riktades", "borde_fått_bemötas",
          "part_krävde_bemötande", "fick_bemöta_kritik"))),
         Framing_Presentation = sum(c_across(c(
          "konstaterar_tydligt_framgick", "utformning_innehåll_övrigt", "betydelse_framställningen_medför",
          "tydligt_framgick_utgångspunkten", "programmen_ska_utformas", "kritisk_infallsvinkel_medför",
          "konstaterar_utgångspunkten_programmet", "genom_kritiskt_förhållningssätt", "programmet_kritisk_infallsvinkel",
          "utgångspunkten_programmet_skildra", "nyheter_ska_presenteras", "anser_tydligt_framgick",
          "programverksamheten_programmen_ska", "gav_missvisande_bild", "anser_utformats_sätt", "sägas_program_karaktär",
          "betydelse_framställningen_oavsett", "framställningen_oavsett_riktigheten", "konstaterar_klart_framgick",
          "programmets_utformning_innehåll", "förhållanden_skildrades_programmet", "granskande_inslag_slag",
          "uppgiften_betydelse_framställningen", "klart_framgick_utgångspunkten", "framgick_utgångspunkten_programmet"))),
         PSB_Mission = sum(c_across(c(
          "svt_s_uppdrag", "belysa_händelser_skeenden", "kommentera_belysa_händelser", "frihet_bestämma_frågor",
          "svt_frihet_bestämma", "programledarens_roll_ifrågasätta", "roll_ifrågasätta_polemisera",
          "ifrågasätta_polemisera_inringande", "frihet_bedöma_värdet", "stor_frihet_bestämma",
          "polemisera_inringande_personerna", "olika_nyheter_bestämma", "uppdrag_pröva_frågor",
          "råda_vidsträckt_yttrandefrihet", "utrymme_värderande_omdömen", "ur_olika_perspektiv"))),
         Factual_Reporting = sum(c_across(c(
          "enligt_utgör_faktafel", "svt_visat_grund", "visat_grund_påståendet", "visat_tillräcklig_grund" ))),
         Reporter_Expression = sum(c_across(c(
          "gav_uttryck_personliga", "uttryck_personliga_uppfattning", "uttalanden_medför_överträdelse",
          "programledarens_förhållningssätt_uttalanden", "uttryck_personliga_uppfattningar", "anser_programledarens_förhållningssätt",
          "finna_programledarens_förhållningssätt", "frågor_provokativ_karaktär", "heller_programledarens_förhållningssätt",
          "personligt_präglade_ställningstaganden", "programledares_frågor_provokativ", "programledarens_förhållningssätt_medför",
          "innebar_ställningstagande_strid", "kritiserade_uttalandet_gjordes", "uttalandet_gjordes_tillfälligt",
          "godtagit_programledares_frågor", "direktsänt_diskussionsprogram_programledarens", "kallat_tillfälligt_medverkande",
          "anser_programledarnas_förhållningssätt", "anser_programledarens_uttalanden", "innebar_programledarens_uttalande",
          "förhållningssätt_innebar_överträdelse", "programledarskapets_personliga_ton", "personliga_ton_får",
          "programledarens_förhållningssätt_innebar", "programledarnas_förhållningssätt_medför",
          "programledarnas_förhållningssätt_aktuella", "uttalanden_programledarens_förhållningssätt",
          "programvärd_gav_uttryck"))),
         Guest_Expression = sum(c_across(c(
          "gav_uttryck_personliga", "inbjuden_egenskap_expert", "medverkande_gav_uttryck", "medverkande_inbjuden_egenskap",
          "tillfällig_medverkande_inbjuden", "uttryck_personliga_uppfattning", "uttalanden_medför_överträdelse",
          "uttryck_personliga_uppfattningar", "tillfälligt_medverkande_inbjuden", "tydliggöra_intervjuades_ståndpunkt",
          "personligt_präglade_ställningstaganden", "tillfälligt_medverkande_företrädare", "tillfälligt_medverkande_gav",
          "innebar_ställningstagande_strid", "kritiserade_uttalandet_gjordes", "uttalandet_gjordes_tillfälligt",
          "inslag_tillfälligt_medverkande", "forskaren_tillfällig_medverkande", "fick_komma_tals", "tillfälligt_medverkande_uttalar",
          "medverkande_uttalar_kritiskt", "personliga_ton_får", "konstaterar_tillfälligt_medverkande"))))




impLogit3 <- impTopNgramsDF %>% 
  select(decision, 503:ncol(.)) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFirth3 <- impTopNgramsDF %>% 
  select(decision, 503:ncol(.)) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFlic3 <- 
  flic(decision ~ Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression,
       data = impTopNgramsDF, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

impLogitFlac3 <- 
  flac(decision ~ Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression, 
       data = impTopNgramsDF, 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))


impLogitDF3 <- modelsummary(list("Standard" = impLogit3, "Firth" = impLogitFirth3, "FLIC" = impLogitFlic3, "FLAC" = impLogitFlac3),
             stars = T,
             output = "default") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(impLogit3)[2], 3)), 
    Firth = as.character(round(extractAIC(impLogitFirth3)[2], 3)),
    FLIC = as.character(round(extractAIC(impLogitFlic3)[2], 3)), 
    FLAC = as.character(round(extractAIC(impLogitFlac3)[2], 3))))




impLogit4 <- impTopNgramsDF %>% 
  inner_join(., impDFfinal, by = c("case", "decision")) %>% 
  select(decision, top_Energy, top_Rus_Ukr, top_Refugees, top_Israel_Pal, top_Nato, top_USA_Trump, top_SwedDems, 
         top_Islam_Racism, top_Climate, Response_Criticism, Framing_Presentation, PSB_Mission, Factual_Reporting, 
         Reporter_Expression, Guest_Expression) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = F, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFirth4 <- impTopNgramsDF %>% 
  inner_join(., impDFfinal, by = c("case", "decision")) %>% 
  select(decision, top_Energy, top_Rus_Ukr, top_Refugees, top_Israel_Pal, top_Nato, top_USA_Trump, top_SwedDems, 
         top_Islam_Racism, top_Climate, Response_Criticism, Framing_Presentation, PSB_Mission, Factual_Reporting, 
         Reporter_Expression, Guest_Expression) %>% 
  logistf(as.factor(decision) ~ ., 
          data = ., 
          firth = T, 
          control = logistf.control(maxit = 150),
          plcontrol = logistpl.control(maxit = 250))

impLogitFlic4 <- 
  flic(decision ~ top_Energy + top_Rus_Ukr + top_Refugees + top_Israel_Pal + top_Nato + top_USA_Trump + 
       top_SwedDems + top_Islam_Racism + top_Climate + Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression,
       data = inner_join(impTopNgramsDF, impDFfinal, by = c("case", "decision")), 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))

impLogitFlac4 <- 
  flac(decision ~ top_Energy + top_Rus_Ukr + top_Refugees + top_Israel_Pal + top_Nato + top_USA_Trump + 
       top_SwedDems + top_Islam_Racism + top_Climate + Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression, 
       data = inner_join(impTopNgramsDF, impDFfinal, by = c("case", "decision")), 
       control = logistf.control(maxit = 150),
       plcontrol = logistpl.control(maxit = 250))


impLogitDF4 <- modelsummary(list("Standard" = impLogit4, "Firth" = impLogitFirth4, "FLIC" = impLogitFlic4, "FLAC" = impLogitFlac4),
             stars = T,
             output = "data.frame") %>% 
  select(-1) %>% 
  slice_head(n = nrow(.) - 2) %>% 
  rows_insert(., data.frame(
    term = "AIC",
    Standard = as.character(round(extractAIC(impLogit4)[2], 3)), 
    Firth = as.character(round(extractAIC(impLogitFirth4)[2], 3)),
    FLIC = as.character(round(extractAIC(impLogitFlic4)[2], 3)), 
    FLAC = as.character(round(extractAIC(impLogitFlac4)[2], 3))))



# Create tables with the final models
objLogitModels <- modelsummary(list("Model1" = objLogitFlac, "Model2" = objLogitFlac3, "Model3" = objLogitFlac4),
                               stars = T,
                               exponentiate = F,
                               estimate = "{estimate} ({std.error}){stars}",
                               statistic = NULL,
                               gof_map = "none",
                               output = "data.frame",
                               title = "Table x.y: Log odds of breach of impartiality, by decision of the Broadcasting Commission.") %>% 
  select(-c(part, statistic)) %>% 
  bind_rows(data.frame(
    term = c("RMSE", "AIC"),
    "Model1" = c(as.character(round(get_gof(objLogitFlac)[1], 3)), as.character(round(extractAIC(objLogitFlac)[2], 3))),
    "Model2" = c(as.character(round(get_gof(objLogitFlac3)[1], 3)), as.character(round(extractAIC(objLogitFlac3)[2], 3))),
    "Model3" = c(as.character(round(get_gof(objLogitFlac4)[1], 3)), as.character(round(extractAIC(objLogitFlac4)[2], 3)))))


impLogitModels <- modelsummary(list("Model1" = impLogitFlac, "Model2" = impLogitFlac3, "Model3" = impLogitFlac4),
                               stars = T,
                               exponentiate = F,
                               estimate = "{estimate} ({std.error}){stars}",
                               statistic = NULL,
                               gof_map = "none",
                               output = "data.frame",
                               title = "Table x.y: Log odds of breach of impartiality, by decision of the Broadcasting Commission.") %>% 
  select(-c(part, statistic)) %>% 
  bind_rows(data.frame(
    term = c("RMSE", "AIC"),
    "Model1" = c(as.character(round(get_gof(impLogitFlac)[1], 3)), as.character(round(extractAIC(impLogitFlac)[2], 3))),
    "Model2" = c(as.character(round(get_gof(impLogitFlac3)[1], 3)), as.character(round(extractAIC(impLogitFlac3)[2], 3))),
    "Model3" = c(as.character(round(get_gof(impLogitFlac4)[1], 3)), as.character(round(extractAIC(impLogitFlac4)[2], 3)))))



# VIF tests to check for multicollinearity among the predictors.
car::vif(lm(decision ~ top_GenderDys_Trans + top_SwedDems + top_Climate + top_USA_Trump + top_Israel_Pal + 
       top_Rus_Ukr + top_Islam + top_Covid + top_Energy + Response_Criticism + Framing_Presentation + 
       PSB_Mission + Factual_Reporting + Reporter_Expression + Guest_Expression, 
       data = inner_join(objTopNgramsDF, objDFfinal, by = c("case", "decision"))))

car::vif(lm(decision ~ top_Energy + top_Rus_Ukr + top_Refugees + top_Israel_Pal + top_Nato + top_USA_Trump + 
       top_SwedDems + top_Islam_Racism + top_Climate + Response_Criticism + Framing_Presentation + PSB_Mission + 
       Factual_Reporting + Reporter_Expression + Guest_Expression, 
       data = inner_join(impTopNgramsDF, impDFfinal, by = c("case", "decision"))))