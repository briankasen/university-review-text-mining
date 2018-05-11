library(tm)

#Create Corpus
# docs <- Corpus(DirSource("C:/Users/joel1/OneDrive/2017 2018 Spring A/MRKT 845 Advanced Marketing Analytics/Project 2 Text Mining/project_2_text_mining/School Reviews"))
docs <- Corpus(DirSource("reviews/"))

# Also create an unaltered copy of docs
docs_orig <- docs

# verify number of documents in the corpa
docs

#inspect a particular document. here, we picked one of the longest written review, one from purdue
writeLines(as.character(docs[[3]]))

# create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

# docs <- tm_map(docs, toSpace, "-")
# docs <- tm_map(docs, toSpace, ":")

# Remove punctuation marks
docs <- tm_map(docs, removePunctuation)

# Check the result of the transformation applied using the purdue review
writeLines(as.character(docs[[3]]))

# Remove non-standard punctuation
docs <- tm_map(docs, toSpace, '"')
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, '-')



#alternatively, view printout of more documents at a time, by lines, for quicker inspection
strwrap(docs[1:20])

#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))

#check result of the transformation applied
strwrap(docs[1:20])

#Strip digits (std transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers)

#check again the result of the transformation applied
strwrap(docs[1:20])

#remove stopwords using the standard list in tm
# but first create doc with the stop words still preserved
# placed the removal of general stopwords after removal of university names which used stopwords
docs_w_stop_words <- docs
# docs <- tm_map(docs, removeWords, stopwords("english"))

#check result of transformation again
strwrap(docs[1:20])

# remove additional words found in higher frequency but are not associative
docs <- tm_map(docs, removeWords, c("the", "like", "also", "said", "often", "go", "theres", "just", "say", "get", "say", "can", "see", "will", "one", "doand", "dont", "u", "youll", "im", "ive", "although", "via", "etc", "youre", "within", "either", "even", "let", "tell", "wholl", "though", "shall", "couldnt", "wouldnt", "shouldnt", "didnt", "doesnt", "us", "throughout", "however", "andor", "pu"))

#check result of transformation again
strwrap(docs[1:20])

# Build a Document Term Matrix
dtm <- DocumentTermMatrix(docs)

dtm

# inspect a small portion of the dtm
inspect(dtm[3:3,1:50])

# get frequency of words in the corpus
freq <- colSums(as.matrix(dtm))
# freq

#length should be total number of terms
length(freq)

# create sort order (descending)
ord <- order(freq,decreasing=TRUE)
# ord

#inspect most frequently occurring terms
freq[head(ord)]

#inspect least frequently occurring terms
freq[tail(ord)]

# define threshold variables so they can be used for the aggregate dtm and the institution dtms further down in the script
dtm_minimum_word_length = 3
dtm_maximum_word_length = 25
dtm_minimum_doc_frequency = 1
dtm_maximum_doc_frequency = 700

# Here we have told R to include only those words that within our doc frequency thresholds
# We have also enforced  lower and upper limit to length of the words included
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(dtm_minimum_word_length, dtm_maximum_word_length), bounds = list(global = c(dtm_minimum_doc_frequency,dtm_maximum_doc_frequency))))
dtmr

freqr <- colSums(as.matrix(dtmr))

#length should be total number of terms
length(freqr)

#create sort order (asc)
ordr <- order(freqr,decreasing=TRUE)

#inspect most frequently occurring terms
freqr[head(ordr)]

#inspect least frequently occurring terms
freqr[tail(ordr)]

# let's take get a list of terms that occur at least a  100 times in the entire corpus
findFreqTerms(dtmr,lowfreq=100)

# term correlations: correlation is a quantitative measure of the co-occurrence of words in multiple documents
correlation_threshold <- .05
findAssocs(dtmr,"northwesternuniversity",correlation_threshold)
findAssocs(dtmr,"universityofmichiganannarbor",correlation_threshold)
findAssocs(dtmr,"universityofwisconsinmadison",correlation_threshold)
findAssocs(dtmr,"universityofillinoisaturbanachampaign",correlation_threshold)
findAssocs(dtmr,"purdueuniversity",correlation_threshold)
findAssocs(dtmr,"pennsylvaniastateuniversity",correlation_threshold)
findAssocs(dtmr,"ohiostateuniversity",correlation_threshold)
findAssocs(dtmr,"universityofminnesotatwincities",correlation_threshold)
findAssocs(dtmr,"michiganstateuniversity",correlation_threshold)
findAssocs(dtmr,"universityofmarylandcollegepark",correlation_threshold)
findAssocs(dtmr,"indianauniversitybloomington",correlation_threshold)
findAssocs(dtmr,"universityofiowa",correlation_threshold)
findAssocs(dtmr,"rutgersuniversitynewbrunswick",correlation_threshold)
findAssocs(dtmr,"universityofnebraskalincoln",correlation_threshold)

#Now let us perform editing of misspelled words, words that should split up, and words that should be combined
# after running the correlation and frequency steps, where we found additional words that we felt should be edited

docs <- tm_map(docs, content_transformer(gsub), pattern = "on campus", replacement = "oncampus")
docs <- tm_map(docs, content_transformer(gsub), pattern = "off campus", replacement = "offcampus")
docs <- tm_map(docs, content_transformer(gsub), pattern = "professorprofess", replacement = "professor")
docs <- tm_map(docs, content_transformer(gsub), pattern = "professs", replacement = "professor")
docs <- tm_map(docs, content_transformer(gsub), pattern = "professorta", replacement = "professor")
docs <- tm_map(docs, content_transformer(gsub), pattern = "professors", replacement = "professor")
docs <- tm_map(docs, content_transformer(gsub), pattern = "students", replacement = "student")
docs <- tm_map(docs, content_transformer(gsub), pattern = "sciences", replacement = "science")
docs <- tm_map(docs, content_transformer(gsub), pattern = " campus beautiful ", replacement = " beautiful campus ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " job guaranteed ", replacement = " guaranteed job ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "succeed", replacement = "success")
docs <- tm_map(docs, content_transformer(gsub), pattern = " university ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " northwestern ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " evanston ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " michigan ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " ann arbor ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " wisconsin ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " madison ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " illinois ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " urbana ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " champaign ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " uiuc ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " purdue ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " purdues ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " penn ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " pennsylvania ", replacement = " ")

docs <- tm_map(docs, content_transformer(gsub), pattern = " penn state ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " ohio ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " ohio state ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " columbus ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " minnesota ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " twin cities ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " cities ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " maryland ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " college park ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " indiana ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " bloomington ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " iowa ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " iowa city ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " rutgers ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " new brunswick ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " nebraska ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " nebraskalincoln ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " lincoln ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " illini ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " urbanachampaign ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " spartan ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " badger ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " husker ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " buckeye ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "bucks", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " buckeyes", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " buckeye", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " red", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " minnesotatwin", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " paul", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " minneapolis", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " badgers", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " blue", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " lafayette", replacement = " ")

docs <- tm_map(docs, content_transformer(gsub), pattern = " hawkeyes ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " hoosier ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " hoosiers ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " rutgers", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " boilermaker", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " purdue", replacement = " ")

docs <- tm_map(docs, content_transformer(gsub), pattern = " bucks ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " boiler ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " maker ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " spartans", replacement = " ")

docs <- tm_map(docs, content_transformer(gsub), pattern = " boilermaker ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " hoosier ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " blue ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " white ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " gold ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " green ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " red ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " crimson ", replacement = " ")

docs <- tm_map(docs, content_transformer(gsub), pattern = " u ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " i ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " m ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " w ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " iu ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " unl ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " umich ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " msu", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " umn ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " psu ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " umd ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " uw ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " uwmadison ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " wisconsinmadison ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " osu ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " um ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " don t ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " tell ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " spending ", replacement = " spend ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " start ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " day ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " donÃ ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " come ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " knowing ", replacement = " know ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " itll  ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " applying ", replacement = " applying ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " put ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " puts ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " youÃ ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " lakes ", replacement = " lake ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " havent ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " saw ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " got ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " set ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " instead ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " either ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " thon ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " brought ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " maybe ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " brings ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " suggest ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " didnt ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " thus ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " therefore ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " gone ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " used ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " becomes ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " perhaps ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " wish ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " next ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " recent ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " due ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " without ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " end ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " honestly ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " isnt ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " five ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " youre ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " lot ", replacement = " lots ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " somehow ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " recently ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " around ", replacement = " ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " missionsplit ", replacement = " mission split ")
docs <- tm_map(docs, content_transformer(gsub), pattern = " itâ???Ts ", replacement = " ")

# once again, check the result of the transformation
strwrap(docs[1:20])

# Update DTM
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(dtm_minimum_word_length, dtm_maximum_word_length), bounds = list(global = c(dtm_minimum_doc_frequency,dtm_maximum_doc_frequency))))

# Count number of terms
dtmr

# get frequency of words in the corpus
freq <- colSums(as.matrix(dtmr))
# freq

#length should be total number of terms
length(freq)

# create sort order (descending)
ord <- order(freq,decreasing=TRUE)
# ord

#inspect most frequently occurring terms
freq[head(ord)]

#inspect least frequently occurring terms
freq[tail(ord)]

library(reshape2)
library(ggplot2)

# create plot of top ten most used words
dtm.matrix <- as.matrix(dtmr)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
toptwenty <- head(sort(wordcount, decreasing=TRUE), 15)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word, levels=dfplot$word[order(dfplot$value,decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)

library(ngram)

#additional general terms to filter out both for institution corpus and later the consolidated corpus
addtl_words_to_filter <- c("easy", "need", "diverse", "food", "never", "available", "take", "study", "huge", "absolutely", "definitely", "organizations", "community", "know", "new", "better", "everything", "staff", "friendly", "met", "meet", "sports", "resources", "opportunity", "beautiful", "academically", "known", "learn", "learning", "school", "schools", "activities", "around", "offer", "offers", "plenty", "anyone", "someone", "may", "much", "extremely", "however", "able", "way", "nice", "dont", "come", "enough", "makes", "year", "hard", "made", "make", "things", "want", "think", "different", "education", "overall", "lot", "lots", "help", "world", "friends", "every", "large", "involved", "club", "clubs", "home", "amazing", "time", "everyone", "time", "ive", "experience", "really", "something", "life", "best", "find", "feel", "major", "classes", "big", "good", "class", "first", "going", "well", "academic", "academics", "oncampus", "opportunities", "student", "campus", "school", "people", "great", "many", "college", "always", "state", "love", "place", "professor", "great")

n_count = 4
institutions_vector = c('northwesternuniversity','universityofmichiganannarbor','universityofwisconsinmadison','universityofillinoisaturbanachampaign','purdueuniversity','pennsylvaniastateuniversity','ohiostateuniversity','universityofminnesotatwincities','michiganstateuniversity','universityofmarylandcollegepark','indianauniversitybloomington','universityofiowa','rutgersuniversitynewbrunswick','universityofnebraskalincoln')
for( i in 1:length(institutions_vector)) {
  for( current_n in 2:n_count) {
    print(paste("--- Begin N-Gram Analysis (n=", n_count, ") for ", institutions_vector[i], " --- "))
    
    # subset corpus to create a smaller corpus of just nebraska reviews
    institution_corpus <-
      tm_filter(docs, function(x)
        any(grep(institutions_vector[i], x, fixed = TRUE)))
    # strip institution id out of corpus since we know what institution we are doing n-gram analysis on to prevent institution name from showing up in results
    institution_corpus <-
      tm_map(
        institution_corpus,
        content_transformer(gsub),
        pattern = institutions_vector[i],
        replacement = " "
      )
    
    # institution_corpus
    # view first row in reduced corpus
    print(paste("--- Begin N-Gram Analysis (n = ", current_n , ") for ",  institutions_vector[i], " --- "))
    # create ngram object
    institution_ng <-
      ngram(concatenate(lapply(institution_corpus, "[", 1)), n = current_n)
    

    # print top 5 phrasetable n-grams found in university reviews
    print(head(get.phrasetable(institution_ng), 5))
    
    print(paste("--- End N-Gram Analysis (n = ", current_n , ") for ", institutions_vector[i], " --- "))
    # End N-Gram Analyis
    
    institution_corpus <- tm_map(institution_corpus, removeWords, addtl_words_to_filter)
    
    # Here we have told R to include all those words 
    # We have also enforced  lower and upper limit to length of the words
    institution_dtmr <-
      DocumentTermMatrix(institution_corpus, control = list(wordLengths = c(dtm_minimum_word_length, dtm_maximum_word_length), bounds = list(global = c(dtm_minimum_doc_frequency, dtm_maximum_doc_frequency))))
    # institution_dtmr
    
    # get frequency of words in the corpus
    freq <- colSums(as.matrix(institution_dtmr))
    # freq
    
    #length should be total number of terms
    # length(freq)
    
    # create sort order (descending)
    institution_ord <- order(freq, decreasing = TRUE)
    institution_ord
    
    institution_freqr <- colSums(as.matrix(institution_dtmr))
    #length should be total number of terms
    length(institution_freqr)
    
    #inspect most frequently occurring terms
    if(current_n == 4){
    print(paste('--- Most Common Terms for ', institutions_vector[i], " --- "))
    print(freq[head(institution_ord)])
    }
    
    #WORDCLOUD
    
    library(wordcloud)
    #setting the same seed each time ensures consistent look across clouds
    set.seed(42)
    layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
    par(mar = rep(0, 4))
    plot.new()
    text(x = 0.5, y = 0.5, institutions_vector[i])

    # color wordcloud with 10 or more occurances within the instituion's dtm
    wordcloud(
      names(institution_freqr),
      institution_freqr,
      min.freq = 10,
      colors = brewer.pal(6, "Dark2"),
      main = "Title"
    )
    
    # color wordcloud with 5 or more occurances within the instituion's ngram
    layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
    par(mar = rep(0, 4))
    plot.new()
    text(x = 0.5, y = 0.5, institutions_vector[i])
    wordcloud(
      get.phrasetable(institution_ng)$ngram,
      get.phrasetable(institution_ng)$freq,
      min.freq = 5,
      max.words = 25,
      random.order = F,
      colors = brewer.pal(6, "Dark2"),
      main = "Title"
    )
    
  }
}

# remove additional high frequency, general terms
docs <- tm_map(docs, removeWords, addtl_words_to_filter)

# Update DTM using same length and document frequency thresholds
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(dtm_minimum_word_length, dtm_maximum_word_length), bounds = list(global = c(dtm_minimum_doc_frequency,dtm_maximum_doc_frequency))))

# Count number of terms
dtmr

# get frequency of words in the corpus
freq <- colSums(as.matrix(dtmr))
# freq

#length should be total number of terms
length(freq)

# create sort order (descending)
ord <- order(freq,decreasing=TRUE)
# ord

# create plot of top ten most used words
dtm.matrix <- as.matrix(dtmr)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
toptwenty <- head(sort(wordcount, decreasing=TRUE), 15)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word, levels=dfplot$word[order(dfplot$value,decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)

#inspect most frequently occurring terms
freq[head(ord)]
correlation_threshold <- .10
findAssocs(dtmr,"northwesternuniversity",correlation_threshold)
findAssocs(dtmr,"universityofmichiganannarbor",correlation_threshold)
findAssocs(dtmr,"universityofwisconsinmadison",correlation_threshold)
findAssocs(dtmr,"universityofillinoisaturbanachampaign",correlation_threshold)
findAssocs(dtmr,"purdueuniversity",correlation_threshold)
findAssocs(dtmr,"pennsylvaniastateuniversity",correlation_threshold)
findAssocs(dtmr,"ohiostateuniversity",correlation_threshold)
findAssocs(dtmr,"universityofminnesotatwincities",correlation_threshold)
findAssocs(dtmr,"michiganstateuniversity",correlation_threshold)
findAssocs(dtmr,"universityofmarylandcollegepark",correlation_threshold)
findAssocs(dtmr,"indianauniversitybloomington",correlation_threshold)
findAssocs(dtmr,"universityofiowa",correlation_threshold)
findAssocs(dtmr,"rutgersuniversitynewbrunswick",correlation_threshold)
findAssocs(dtmr,"universityofnebraskalincoln",correlation_threshold)

library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
par(mar = rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "All Big Ten University Reviews - Common Words")

# color wordcloud with 51 or more occurances of the most common terms across all schools
wordcloud(
  names(freqr),
  freqr,
  min.freq = 51,
  colors = brewer.pal(6, "Dark2"),
  main = "All Big Ten University Reviews - Common Words"
)

