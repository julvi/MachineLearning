#function [FrqItemsets, AssocRules, Summary1, Summary2] = ...
#    runApriori(filename, minSup, minConf, maxRule)
runApriori <- function(filename, minSup, minConf, maxRule=4){
# RUNAPRIORI Mine for associations based on the Apriori algorithm
#
# The Apriori algorithm used in the script is provided by http://www.borgelt.net//apriori.html, for
# details of the algorithm see also http://www.borgelt.net//doc/apriori/apriori.html
#
# Usage
#   [FrqItemsets, AssocRules, Summary1, Summary2] = ...
#       apriori(filename, minSup, minConf, maxRule);
#
# Input
#   filename     A string defining the name of the file to mine for
#                associations, the file needs to be comma separated.
#   minSup       Minimum support in percentage of data points
#   minConf      Minimum confidence in percentage
#   maxRule      Define the largest number of items in an association
#                      rule (default = 4)
#
# Output: a list containing:
#   FrqItemsets  Cell array containing frequent itemsets
#   AssocRules   Cell array containing the found association rules
#
# Author: Laura Frølich, lff@imm.dtu.dk


pth = paste(file.path(getwd(), 'Tools', 'Apriori'), '/', sep='')

# Run Apriori Algorithm
#filename=file.path('Data', 'courses.txt')
#minConf=100
#minSup=80
#maxRule=4

print('Mining for frequent itemsets by the Apriori algorithm');
status1 <- system(paste(pth, 'apriori -f"," -s', minSup, ' -v"[Sup. %0S]" ', filename, ' apriori_temp1.txt', sep=''))

if(status1!=0){
    stop('An error occured while calling apriori');
  }

if(minConf>0){
    print('Mining for associations by the Apriori algorithm');
    status2 <- system(paste(pth, 'apriori -tr -f"," -n', maxRule, ' -c', minConf, ' -s', minSup, ' -v"[Conf. %0C,Sup. %0S]" ', filename, ' apriori_temp2.txt', sep=''))
    if(status2!=0){
        stop('An error occured while calling apriori');
      }
  }

# Extract estimated associated from stored file apriori_temp1.txt
print('Apriori analysis done, extracting results');
# Extract Frequent Itemsets
FrqItemsets <- readLines('apriori_temp1.txt')
cat(c('Frequent Item Sets:', FrqItemsets, '\n'), sep='\n')


# Extract Association rules
if(minConf>0){
AssocRules <- readLines('apriori_temp2.txt')
} else {
    AssocRules='Condidence set to zero'
  }
cat(c('Association Rules: ', AssocRules, '\n'), sep='\n')

# Delete temporary files
unlink('apriori_temp1.txt');
unlink('apriori_temp2.txt');

res <- list()
res$FreqItemSets <- FrqItemsets
res$AssocRules <- AssocRules
res
}
