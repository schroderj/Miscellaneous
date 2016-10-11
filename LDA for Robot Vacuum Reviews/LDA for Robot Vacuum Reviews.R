# Import and Clean Data ---------------------------------------------------
# Install packages for importing xlsx files, parsing text and creating a document term matrix, 
# and running the topic model.
install.packages("xlsx"); install.packages("tm"); install.packages("topicmodels")
library(xlsx); library(tm); library(topicmodels)

# Set the working directory.
setwd("~/Dropbox/LDA for Robot Vacuum Reviews")

# The read.xlsx command requires that the first row on each worksheet contains the variable names.
# HP_RobotVacuum_AmazonReviews_2, included with this R code, has that change implemented.

# Import the review data for each of the vacuum models, ensuring text is convereted as a string.
roomba_880 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Roomba_880",stringsAsFactors=FALSE)
roomba_650 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Roomba_650",stringsAsFactors=FALSE)
roomba_770 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Roomba_770",stringsAsFactors=FALSE)
roomba_870 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Roomba_870",stringsAsFactors=FALSE)
bissell_1605 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Bissell_1605",stringsAsFactors=FALSE)
bobsweep = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="bObsweep",stringsAsFactors=FALSE)
itouchless = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="ITouchless",stringsAsFactors=FALSE)
moneual = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Moneual",stringsAsFactors=FALSE)
neato_connect = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Neato_Connect",stringsAsFactors=FALSE)
neato_d75 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Neato_D75",stringsAsFactors=FALSE)
neato_d80 = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Neato_D80",stringsAsFactors=FALSE)
neato_xv = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Neato_XV",stringsAsFactors=FALSE)
p3_p4960v = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="P3_P4960V",stringsAsFactors=FALSE)
samsung_powerbot_20w = read.xlsx("HP_RobotVacuum_AmazonReviews_2.xlsx",sheetName="Samsung_Powerbot_20w",stringsAsFactors=FALSE)

# Each review includes a title, which is stored in the initial import as a separate string. The titles
# are as informative as the reviews, sometimes even more so, so we need to concatenate the title and
# review into a single string. Additionally, we need to remove emojis and other non-UTF-8 characters.
roomba_880 = paste(roomba_880$Title,roomba_880$Review); roomba_880 = iconv(roomba_880, "ASCII", "UTF-8", sub="")
roomba_650 = paste(roomba_650$Title,roomba_650$Review); roomba_650 = iconv(roomba_650, "ASCII", "UTF-8", sub="")
roomba_770 = paste(roomba_770$Title,roomba_770$Review); roomba_770 = iconv(roomba_770, "ASCII", "UTF-8", sub="")
roomba_870 = paste(roomba_870$Title,roomba_870$Review); roomba_870 = iconv(roomba_870, "ASCII", "UTF-8", sub="")
bissell_1605 = paste(bissell_1605$Title,bissell_1605$Review); bissell_1605 = iconv(bissell_1605, "ASCII", "UTF-8", sub="")
bobsweep = paste(bobsweep$Title,bobsweep$Review); bobsweep = iconv(bobsweep, "ASCII", "UTF-8", sub="")
itouchless = paste(itouchless$Title,itouchless$Review); itouchless = iconv(itouchless, "ASCII", "UTF-8", sub="")
moneual = paste(moneual$Title,moneual$Review); moneual = iconv(moneual, "ASCII", "UTF-8", sub="")
neato_connect = paste(neato_connect$Title,neato_connect$Review); neato_connect = iconv(neato_connect, "ASCII", "UTF-8", sub="")
neato_d75 = paste(neato_d75$Title,neato_d75$Review); neato_d75 = iconv(neato_d75, "ASCII", "UTF-8", sub="")
neato_d80 = paste(neato_d80$Title,neato_d80$Review); neato_d80 = iconv(neato_d80, "ASCII", "UTF-8", sub="")
neato_xv = paste(neato_xv$Title,neato_xv$Review); neato_xv = iconv(neato_xv, "ASCII", "UTF-8", sub="")
p3_p4960v = paste(p3_p4960v$Title,p3_p4960v$Review); p3_p4960v = iconv(p3_p4960v, "ASCII", "UTF-8", sub="")
samsung_powerbot_20w = paste(samsung_powerbot_20w$Title,samsung_powerbot_20w$Review); samsung_powerbot_20w = iconv(samsung_powerbot_20w, "ASCII", "UTF-8", sub="")


# Select a Corpus and Generate a Document Term Matrix ---------------------
# The corpus can consist of one or more sets of reviews. Here I have four corpora based on low,
# middle, and high-priced brands plus all reviews at once. The code is set to run one corpus at a time.

corpus = Corpus(VectorSource(p3_p4960v))                                     # Low-priced brand.
corpus = Corpus(VectorSource(c(neato_connect,neato_d75,neato_d80,neato_xv))) # Middle-priced brand.
corpus = Corpus(VectorSource(samsung_powerbot_20w))                          # High-priced brand.

# The entire set of reviews.
corpus = Corpus(VectorSource(c(roomba_880,roomba_650,roomba_770,bissell_1605,bobsweep,
                               itouchless,moneual,neato_connect,neato_d75,neato_d80,neato_xv,
                               p3_p4960v,samsung_powerbot_20w)))

# # Functions to check the corpus object.
# inspect(corpus[1:2])
# writeLines(as.character(corpus[[1]]))

# # Backup correction if you see errors related to characters not being sete in UTF-8.
# # CAUTION: The first line is for Windows, the second line is for Mac OS X.
# corpus = tm_map(corpus,content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
# corpus = tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub = "byte")))

# The output from the topic model will depend on the vocabulary that is included. A few functions,
# such as removing punctuation and setting everything to lowercase, is conventional. Other changes
# may be more impactful and should be used carefully.

corpus = tm_map(corpus,content_transformer(tolower))     # Make corpus lowercase.
corpus = tm_map(corpus,removePunctuation)                # Remove punctuation.
corpus = tm_map(corpus,stripWhitespace)                  # Strip extra whitespace.
corpus = tm_map(corpus,removeNumbers)                    # Remove numbers (including model numbers).
corpus = tm_map(corpus,removeWords,stopwords('english')) # Remove all words in the stopwords('english') set.
# corpus = tm_map(corpus,removeWords,stopwords('smart'))   # Remove all words in the stopwords('smart') set.
# corpus = tm_map(corpus,stemDocument)                     # Truncates words to get at common word roots.

# Generate the document term matrix.
dtm = DocumentTermMatrix(corpus)

# # Function to look at the document term matrix object.
# inspect(dtm[1:10,1:20])


# Run the Topic Model and Analyze Results ---------------------------------
# The topic model we're using is called latent Dirichlet allocation. The output will let us see what
# words occur often together across the entire corpus. The number of topics K will need to be specified
# by you as the analyst, so it's advisable to run the topic model for a number of different values of K
# and compare results.

lda_output = LDA(dtm,method="Gibbs",k=3) # K specifies the number of topics being estimated.
terms(lda_output,k=20)                   # K specifies the number of most likely terms printed for each topic.

# To quickly produce a number of topic solutions, I loop through a number of different solutions
# for a given corpus, save out the corresponding most likely terms for each topic, and export an
# Excel workbook with all of the solutions.
solution = list()
for (i in 2:10) {
  lda_output = LDA(dtm,method="Gibbs",k=i); print(i-1)
  solution[[i-1]] = terms(lda_output,k=15)
}
write.xlsx(solution[[1]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 1")
write.xlsx(solution[[2]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 2",append=TRUE)
write.xlsx(solution[[3]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 3",append=TRUE)
write.xlsx(solution[[4]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 4",append=TRUE)
write.xlsx(solution[[5]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 5",append=TRUE)
write.xlsx(solution[[6]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 6",append=TRUE)
write.xlsx(solution[[7]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 7",append=TRUE)
write.xlsx(solution[[8]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 8",append=TRUE)
write.xlsx(solution[[9]],"Category-Wide Topic Solutions.xlsx",sheetName="Solution 9",append=TRUE)
