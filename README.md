# Introduction-to-NLP

This R code provides an introduction to Natural Language Processing (NLP) techniques using the tm (text mining) package. Here's an explanation of what the code does:

Creating a Corpus Object: It starts by creating a corpus object named docs. A corpus is a collection of text documents that will be processed and analyzed.

Text Preprocessing:

Remove Punctuation: It removes punctuation marks from the text documents.
Convert to Lowercase: The text is converted to lowercase to ensure uniformity.
Remove Numbers: Numeric characters are removed from the text.
Remove Standard English Stopwords: Common English stopwords like "the," "and," "is," etc., are removed.
Remove Customized Stopwords: Additional customized stopwords are removed.
Strip Whitespace: Extra whitespace is removed from the text.
Stemming: Stemming is applied to the corpus using the stemDocument function. Stemming reduces words to their root form, but it may result in ambiguous words.

Lemmatization: Lemmatization is performed using the textstem package. Lemmatization reduces words to their base or dictionary form, which is often more accurate than stemming but can be slower.

Creating a Custom Cleaning Function: A custom function named clean_corpus is defined to perform the same preprocessing steps. It takes a corpus as input and returns a cleaned corpus.

Document-Term Matrix (DTM): The code creates a Document-Term Matrix (DTM) using the DocumentTermMatrix function from the cleaned corpus. The DTM represents the frequency of words in the corpus, where rows are documents, and columns are words. It also contains information about term frequency.

Exploring the DTM:

Extracting Word Names: The code extracts and displays the names of the first 20 words from the DTM.
Finding Frequent Terms: It identifies terms that occur at least 200 times in the corpus.
Finding Term Associations: The code finds terms with at least 0.8 correlation with the term "terrorist."
Finding Most Frequent Terms: It identifies the six most frequent terms for each document in the corpus.
Word Frequency Analysis: The code calculates and visualizes the most frequent and least frequent words in the entire corpus using a barplot.

Removing Sparse Terms: Sparse terms, which occur only in a few documents, are removed from the DTM using the removeSparseTerms function. This reduces the size of the matrix.

TF-IDF Weighting: Term Frequency-Inverse Document Frequency (TF-IDF) weighting is applied to the DTM using the weightTfIdf function. This weights the terms based on their importance in the corpus.
