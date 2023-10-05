## Introduction to NLP        

## - Creation of a corpus-object (tm package)
## - Pre-processing (tm package)
## - Stemming or lemmatization? (tm + textstem packages)
## - DTM (tm package)


# The focus will be on exploring the package and getting some texts into the corpus object format. 
# For this purpose, we analyze 30 documents concerning 30 original and individual
# speeches from Donald Trump during his electoral campaign (2016).

library(tm)        

# First of all, we need to create the corpus-object, 
# representing an abstract collection of text documents
docs <- Corpus(DirSource("Trump_30"))  # create the corpus

class(docs)

as.character(docs[[30]]) # in this way we can have a character representation of the document


# The pre-processing phase is very important before applying any NLP method: from 
# sentiment analysis to topic modelling, it is always necessary to "clean" the raw texts 
# before proceeding with further analysis. 
# For instance, there are some NLP techniques (such as Latent Dirichlet Allocation, LDA)
# which are very sensible to the pre-processing (--> the final results can be highly influenced by a wrong pre-processing).

# 1. Remove the punctuation marks
# 2. Convert the corpus to lower case
# 3. Remove all numbers since these do not usually contribute to the meaning of the text (optional)
# 4. Remove standard English stop-words
# 5. Remove non-standard English stop-words (optional)
# 6. Remove the white spaces obtained after the removal of the stopwords/numbers/punctuations


docs <- tm_map(docs, content_transformer(removePunctuation)) # over-write the documents applying, step by step, all the transformations
as.character(docs[[30]])

docs <- tm_map(docs, content_transformer(tolower))
as.character(docs[[30]])

docs <- tm_map(docs, content_transformer(removeNumbers))
as.character(docs[[30]])

# Let's have a look to the most common stopwords in English
stopwords("en")
docs <- tm_map(docs, content_transformer(removeWords), stopwords("english"))  

# Customize the stopwords (optional)
docs <- tm_map(docs, content_transformer(removeWords), c("applause", "lot",
                                    "done","do","gonna","let","got","just",
                                    "many","much","get","two","one","can",
                                    "said","say","didn","did","ll","going",
                                    "do","don","couldn","could","aye", "e",
                                    "yi","will" ,"doesn","t" ,"s", "m", "re",
                                    "trump", "as", "able", "about", "above", "according",
                                    "accordingly", "across", "actually", "after", "afterwards",
                                    "again", "against", "aint", "all", "allow", "allows", "almost",
                                    "alone", "along", "already", "also", "although",
                                    "always", "am", "among", "amongst", "an", "and",
                                    "another", "any", "anybody", "anyhow", "anyone",
                                    "anything", "anyway", "anyways", "anywhere", "apart", "appear",
                                    "appreciate", "appropriate", "are", "arent", "around", "as",
                                    "aside", "ask", "asking", "associated", "at", "available",
                                    "away", "awfully", "be", "became", "because", "become", "becomes",
                                    "becoming", "been", "before", "beforehand", "behind", "being", "believe",
                                    "below", "beside", "besides", "best", "better", "between", "beyond", "both",
                                    "brief", "but", "by", "cmon", "cs", "came", "can", "cant", "cannot", "cant",
                                    "cause", "causes", "certain", "certainly", "clearly", "co",
                                    "com", "come", "comes", "concerning", "consequently", "consider",
                                    "considering", "contain", "containing", "contains", "corresponding",
                                    "could", "couldnt", "course", "currently", "definitely", "described",
                                    "despite", "did", "didnt", "different", "do", "does", "doesnt", "doing",
                                    "dont", "done", "down", "downwards", "during", "each", "edu", "eg", "eight",
                                    "either", "else", "elsewhere", "enough", "entirely", "especially", "et",
                                    "etc", "even", "ever", "every", "everybody", "everyone", "everything",
                                    "everywhere", "ex", "exactly", "example", "except", "far", "few",
                                    "fifth", "first", "five", "followed", "following", "follows",
                                    "for", "former", "formerly", "forth", "four", "from", "further",
                                    "furthermore", "get", "gets", "getting", "given", "gives",
                                    "go", "goes", "going", "gone", "got", "gotten", "greetings",
                                    "had", "hadnt", "happens", "hardly", "has", "hasnt", "have",
                                    "havent", "having", "he", "hes", "hello", "help", "hence", "her", "hillary",
                                    "here", "heres", "hereafter", "hereby", "herein", "hereupon",
                                    "hers", "herself", "hi", "him", "himself", "his", "hither",
                                    "hopefully", "how", "howbeit", "however", "id", "ill", "im", "ive", "shes", "youre",
                                    "ie", "if", "ignored", "immediate", "in", "inasmuch",
                                    "inc", "indeed", "indicate", "indicated", "indicates",
                                    "inner", "insofar", "instead", "into", "inward", "is", "isnt",
                                    "it", "itd", "itll", "its", "itself", "just", " keep",
                                    "keeps", "kept", "know", "knows", "known", "last", "lately",
                                    "later", "latter", "latterly", "least", "less", "lest", "let",
                                    "lets", "like", "liked", "likely", "little", "look", "looking",
                                    "looks", "ltd", "mainly", "many", "may", "maybe", "me", "mean",
                                    "meanwhile", "merely", "might", "more", "moreover", "most",
                                    "mostly", "much", "must", "my", "myself", "name", "namely", "nd",
                                    "near", "nearly", "necessary", "need", "needs", "neither",
                                    "never", "nevertheless", "new", "next", "nine", "no", "nobody",
                                    "non", "none", "noone", "nor", "normally", "not", "nothing", "novel",
                                    "now", "nowhere", "obviously", "of", "off", "often", "oh", "ok",
                                    "okay", "old", "on", "once", "one", "ones", "only", "onto", "or",
                                    "other", "others", "otherwise", "ought", "our", "ours",
                                    "ourselves", "out", "outside", "over", "overall", "own",
                                    "particular", "particularly", "per", "perhaps", "placed",
                                    "please", "plus", "possible", "presumably", "probably", "provides",
                                    "que", "quite", "qv", "rather", "rd", "re", "really", "reasonably",
                                    "regarding", "regardless", "regards", "relatively", "respectively",
                                    "right", "said", "same", "saw", "say", "saying", "says", "second",
                                    "secondly", "see", "seeing", "seem",
                                    "seemed", "seeming", "seems", "seen",
                                    "self", "well", "worse", "theyre", "thats", "think", "way", "tell", "tells",
                                    "want", "make", "take", "thank"))

as.character(docs[[30]])

docs <- tm_map(docs, content_transformer(stripWhitespace))
writeLines(as.character(docs[[30]]))


# Best order to follow
# 1. convert to lower case
# 2. remove standard stopwords
# 3. remove punctuations 
# 4. remove numbers
# 5. remove customized words
# 6. strip white spaces

# Stemming --> Transform each word into the word’s root (hence, removing suffix and prefix).

# PROS: 
# - Easier than lemmatization.
# CONS:
# - It might cause a lot of ambiguity --> 'Axes' is both the plural form of 'axe' and 'axis'. 
# By chopping the "s", you won't be able to relate if "axes" refers to "axis" or "axe"...
# Another example is about the word 'better' that should be resolved to 'good', but stemming would fail to do that.
# - Stemming is hard for several languages like Hebrew and Arabic


# Stemming the corpus
docs_stemmed <- tm_map(docs, content_transformer(stemDocument)) # use the same syntax as before


docs_stemmed[[30]]$content # Another way to inspect the content of a document

# Lemmatization --> The process of grouping together the different inflected forms of a word. 
# Every word is converted to the corresponding lemma --> it relies on the use of a dictionary

# PROS: 
# - More accurate than stemming.
# CONS:
# - Because lemmatization involves deriving the meaning of a word from something like a dictionary, 
# it's very time consuming. So most lemmatization algorithms are slower compared to their stemming counterparts.

library("textstem")

# lemmatizing the corpus
docs_lemmatized <- tm_map(docs, content_transformer(lemmatize_words))
inspect(docs_lemmatized[[30]])  # another way (3rd) to inspect the content of a document

# You can also create a custom function that you can use to clean the corpus

clean_corpus <- function(CORPUS){
  CORPUS <- tm_map(CORPUS, content_transformer(removePunctuation))
  CORPUS <- tm_map(CORPUS, content_transformer(removeNumbers))
  CORPUS <- tm_map(CORPUS, content_transformer(tolower))
  CORPUS <- tm_map(CORPUS, content_transformer(removeWords), stopwords("en"))
  CORPUS <- tm_map(CORPUS, content_transformer(stripWhitespace))
  return(CORPUS)
}

docs_cleaned_by_my_function <- clean_corpus(Corpus(DirSource("Trump_30")))
# Note! I have not use the object "docs" because now it holds the cleaned documents. 
# I have used as input of the function a new object 'corpus' from the same documents
as.character(docs_cleaned_by_my_function[[30]])

# The next step is the creation of the document term matrix (DTM), a matrix that lists all the
# occurrences of the words in the corpus, by document.

dtm <- DocumentTermMatrix(docs)  # apply the function DocumentTermMatrix to the cleaned corpus 
dtm
# 166086 cells are 0;
# 27174 have non-zero values;
# The total number of cells in this matrix is given by 30*6442 = 193.260 --> also given by 166086+27174
# 86% of all cells are zero (which is 166086/(166086+27174))
# Inspecting the DTM...
str(dtm)
# i, j, v are some indexes (not useful for us)

# With the following line of code we can extract the names of the first 20 words from the DocumentTermMatrix object 
head(dtm$dimnames$Terms, 20)

# Sometimes, it is very common also to calculate the Term Document Matrix (TDM), which is simply 
# the transpose of the DTM
tdm <- TermDocumentMatrix(docs)
tdm
inspect(tdm[202:205, 1:5])

findFreqTerms(dtm, 200) # find the terms that occur at least 200 times --> function belonging to "tm"

findAssocs(dtm, "terrorist", 0.8) # find terms with at least 0.8 correlation with the term "terrorist"

findMostFreqTerms(dtm) # find the 6 most frequent terms for each document



# Now we want to identify the most frequent words in all the corpus...
freq_w <- colSums(as.matrix(dtm)) # We are obtaining the frequencies of each word considering *all* documents


ord <- order(freq_w, decreasing = TRUE) # sort in descending order
ord # Returns the list of the **indexes/positions** of the sorted words 
#list the most frequent words
freq_w[head(ord)]
#list the least frequent words
freq_w[tail(ord)]

# We can represent everything graphically using a barplot. 
# In this case I am using 'ggplo2' which is a famous package for graphical representations. 

wf=data.frame(term=names(freq_w),occurrences=freq_w)  # creating a data frame containing the words and the corresponding frequencies
library(ggplot2)
p <- ggplot(subset(wf, freq_w>200), aes(term, occurrences)) # We are subsetting the data frame by considering those words whose frequencies is higher than 200.
# Starting from these words we create the barplot.
p <- p + geom_bar(stat= "identity")  # Graphical parameters to draw the bars
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) # Putting the words on the X-axis
p

# Since the DTM tends to get very big already for normal sized data sets, in 'tm' there is 
# a method to remove sparse terms, i.e., terms occurring only in very few documents. 
# Normally, this reduces the matrix dramatically without losing significant relations 

new_dtm <- removeSparseTerms(dtm, sparse = 0.4) # function removeSparseTerms()
# We are removing those terms which have at least 40% of empty.
new_dtm

# Weight a document term matrix by TF-IDF: term frequency - inverse document frequency
dtm_tfidf <- weightTfIdf(dtm)
dtm_tfidf
inspect(dtm_tfidf[20:25, 1:5])
