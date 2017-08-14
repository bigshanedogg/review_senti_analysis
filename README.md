README

This code is for sentiment analysis of movie reviews parsed from NAVER(Korean top portal site). Each review is labelled into "G"-good, "A"-average, "B"-bad according to its 0-10 score, and the final goal is judging whether specific review is favorable to movie or not automatically given only reviews without score. It is practically useful when applying in getting the tendency of unstructured text data such as News reviews or community's tendency to specific issue. (Additional goal is understanding the algorithm deeply by implementing Naive Bayesian classifier myself with R).

Actually, The accuracy is 74% (divided into "G","B") and 66.5% (divided into "G","A","B"), not that high, because the Korean dictionary supported in R doesn't include new word or slang. It's meaningful that It used only 73k reviews to train and simple algorithm (Naive Bayesian classifier).  
