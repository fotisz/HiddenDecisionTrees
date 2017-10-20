################################################################################################################################
# Author: Naveenkumar Ramaraju                                                                                                 #
# Hidden Decision Trees                                                                                                        #
# Based on article: http://www.datasciencecentral.com/profiles/blogs/state-of-the-art-machine-learning-automation-with-hdt     #
# Date: Feb-17-2017                                                                                                            #
# File version - 1                                                                                                             #
# R - version: 3.3.2                                                                                                           #
################################################################################################################################
ptm <- proc.time()
library(hash)


# this function is reusable function to update single and bigram words to the hash with pv and ID
update_word_data <- function(hash_to_update, word, pv, ID){
  if(has.key(word, hash_to_update))
  {
    word_data = hash_to_update[[word]]
    word_data$count = word_data$count + 1
    word_data$PV = word_data$PV + pv
    
    if (pv > word_data$pv_max)
    {
      word_data$pv_max = pv
    }
    
    if (pv < word_data$pv_min)
    {
      word_data$pv_min = pv
    }
    
    word_data$ids[[length(word_data$ids) + 1]] = ID
    hash_to_update[[word]] = word_data
  }
  else
  {
    hash_to_update[[word]] = list(count=1,PV=pv,pv_max = pv,pv_min = pv, ids = list(ID)) # count, max, min and ids in a list
  }
  return  # nothing to return - hash updated by reference with word as key
}

List = hash() # using hash to avoid r-bind and performance issues
articleTitle = list()
articlepv = list()
con = file("HDT-data3.txt",open="r")
lines = readLines(con) 
for (line_num in 2:length(lines)){ # excluding first line as it is header
  line = lines[line_num] 
  line = tolower(line)
  aux = strsplit(line,'\t') # Indexes will have: 1 - Title, 2 - URL, 3 - data and 4 - page views 
  url = aux[[1]][2]
  pv = log(1 + as.numeric(aux[[1]][4])) 
  
  if (isTRUE(grep("/blogs/", url) == 1)) 
  {
    type = "BLOG"
  }
  else
  {
    type = "OTHER"
  }
  
  #--- clean article titles, remove stop words
  title = aux[[1]][1]
  title = paste("",title,"", sep=" ") # adding space at the ends
  title = gsub('["]', ' ', title) # replacing special characters with a space to avoid clbbing of words
  title = gsub('[?]', ' ? ', title)
  title = gsub('[:]', ' ', title)
  title = gsub('[.]', ' ', title)
  title = gsub('[(]', ' ', title)
  title = gsub('[)]', ' ', title)
  title = gsub('[,]', ' ', title)
  title = gsub(' a ', ' ', title)
  title = gsub(' the ', ' ', title)
  title = gsub(' for ', ' ', title)
  title = gsub(' in ', ' ', title)
  title = gsub(' and ', ' ', title)
  title = gsub(' or ', ' ', title)
  title = gsub(' is ', ' ', title)
  title = gsub(' in ', ' ', title)
  title = gsub(' are ', ' ', title)
  title = gsub(' of ', ' ', title)
  #title = gsub('  ', ' ', title) # replacing double spaces with single space
  title = trimws(title)
  
  #break down article title into keyword tokens
  aux2 = strsplit(title,' ')
  for (k in 1:length(aux2[[1]]))
  {
    aux2[[1]][k] = gsub(' ', '', aux2[[1]][k])
  }
  aux2 = aux2[[1]][aux2[[1]] != '']
  
  for (k in 1:length(aux2)) 
  {
    word = paste(aux2[k], "\t", "N/A", "\t", type)
    update_word_data(List, word, pv, ID<-line_num-1)
    if ((length(aux2) - k) > 0)
    {
      word1 = aux2[k]
      word2 = aux2[k+1]
      word = paste(word1, "\t", word2, "\t", type)
      update_word_data(List, word, pv, ID<-line_num-1)
    }
  }
  articleTitle[[line_num-1]] = title
  articlepv[[line_num-1]] = pv
}

nArticles=length(lines) - 1 # -1 as first line is title
close(con)

avg_pv = mean(unlist(articlepv))
articleFlag = rep("BAD", nArticles)
nidx=0;
nidx_Good=0;
out_data <- data.frame(key_word = character(), displayed_in_articles = numeric(), avg_pv = numeric(),min_pv = numeric(), max_pv = numeric(), ids = character(), stringsAsFactors=FALSE)
reason_data <- data.frame(title = character(), pv = numeric(), idx = character(), n = numeric(), Avg = numeric(), pv_min = numeric(), pv_max = numeric(), stringsAsFactors=FALSE)
for (idx in keys(List))
{
  word_data = List[[idx]]
  n = word_data$count
  Avg = word_data$PV/n
  nidx =  nidx + 1
  if ( ((n > 3) & (n < 8) & (word_data$pv_min > 6.9) & (Avg > 7.6)) | 
       ((n >= 8) & (n < 16) & (word_data$pv_min > 6.7) & (Avg > 7.4)) |
       ((n >= 16) & (n < 200) & (word_data$pv_min > 6.1) & (Avg > 7.2)) ) 
  {
    nidx_Good = nidx_Good + 1
    idlist = word_data$ids
    out_data[nidx_Good,] <- c(idx, n, Avg, word_data$pv_min, word_data$pv_max, toString(idlist))
    for (ID in idlist)
    {
      ID = as.numeric(ID)
      title=articleTitle[[ID]]
      pv=articlepv[[ID]]
      reason_data[nrow(reason_data) + 1, ] = c(title, pv, idx, n, Avg, word_data$pv_min, word_data$pv_max)
      articleFlag[ID] = "GOOD"; 
    }
  }
}
write.table(out_data, "hdt-out2.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE) #writing output to files
write.table(reason_data, "hdt-reasons.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)

pv_threshold = 7.1 # Threshold to compute results
# computing results using vector condition check and inbuilt sum and mean functions
n1 = sum(articleFlag == "GOOD")
n2 = sum(articleFlag != "GOOD")
FalsePositive = sum(articlepv < pv_threshold & articleFlag == "GOOD")
FalseNegative = sum(articlepv > pv_threshold & articleFlag != "GOOD")
m1 = sum(articlepv > pv_threshold)
m2 = sum(articlepv > pv_threshold)

# Printing results
avg_pv1 = mean(unlist(articlepv[which(articleFlag == "GOOD")]))
avg_pv2 = mean(unlist(articlepv[which(articleFlag != "GOOD")]))
errorRate = (FalsePositive + FalseNegative)/nArticles
aggregationFactor = (nidx/nidx_Good)/(nArticles/n1)
print (paste("Average pv:", avg_pv))
print (paste("Number of articles marked as good: ", n1, " (real number is ", m1,")", sep = "") )
print (paste("Number of articles marked as bad: ", n2, " (real number is ", m2,")", sep = ""))
print (paste("Avg pv: articles marked as good:", avg_pv1))
print (paste("Avg pv: articles marked as bad:",avg_pv2))
print (paste("Number of false positive:",FalsePositive," (bad marked as good)"))
print (paste("Number of false negative:", FalseNegative, " (good marked as bad)"))
print (paste("Number of articles:", nArticles))
print (paste("Error Rate: ", errorRate))
print (paste("Number of feature values: ", nidx, " (marked as good: ", nidx_Good,")", sep = ""))
print (paste("Aggregation factor:", aggregationFactor))


proc.time() - ptm
