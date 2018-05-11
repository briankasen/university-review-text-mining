# Big Ten University Student Review Text Mining using R


Purpose: For a data science text mining group project, We needed to collect a number of reviews from a set of companies within any particular industry. The selected industry was higher education since our group had an intrinsic interest in the higher education space for a number of years and our coursework was a part of the MBA program at the University of Nebraska - Lincoln, Big Ten University.

The analysis consisted of collected n reviews for each Big Ten University. We decided to build a web scraping script to expedite the review collection process. All reviews were collected from the website www.unigo.com (http://www.unigo.com).

Once we collected the reviews, we performed necessary cleanup to remove punctuation, special characters, and filler words. We looked at correlation of terms to university names to identify unique terms using a document term matrix approach. Terms such as "college", school abbreviations, school mascots, etc. were removed so we could more easily identify relevant terms that we could compare universities against each other. The analysis produced top terms in word clouds to visualize the results.

In addition to the dtm analysis, we sought to see how adjectives such as "great", "pretty", or "very" related to terms to identify unique attributes to universities. To do this we implemented n-gram analysis to look at 2, 3, and 4 term combinations. This enabled us to see that some schools were commonly described to have a "pretty campus" or "very expensive". The value of n-gram gave us deeper text mining capabilities. Word Clouds were used to help us identify common words or phrases used to describe each university.
