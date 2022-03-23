# copy the OS Webster's 2nd International Dictionary to somewhere you can find it 
# cp /usr/share/dict/words /Documents

# load libraries
library(tidyverse)
library(cape)
library(circlize)
library(wesanderson)
library(ggplot2)
library(streamgraph)

# load words
words = read_delim('Documents/words.csv', delim = '\n', col_names = F)

# convert to lower case
words[,1] = sapply(1:nrow(words), function(i) tolower(words[i,1]))

# length of words
words$len = NA
words$len = sapply(1:nrow(words), function(i) nchar(words[i,1]))
max(words$len)
# 24 max length

# create dataframe with frequency of each letter at each position
dfWords = data.frame(matrix(ncol = 25, nrow = 26))
places = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24')
colnames(dfWords) = c('letter', paste0('place', places))
dfWords$letter = letters
dfWords[,c(2:25)] = 0

for (i in 1:nrow(words)){
	vect = strsplit(words[[1]][i], '')[[1]] # create vector of letters in the word
	vect = vect[vect %in% letters]  # remove non-letter elements
	for (j in 1:length(vect)){
		dfWords[dfWords$letter == vect[j],j+1] = dfWords[dfWords$letter == vect[j],j+1] + 1
	}
}

# reshape to long format
dfWordsLong = gather(dfWords, place, freq, place01:place24, factor_key = T)

# the streamgraph package expects the x-axis variable to be a date, so we'll treat the place of each letter as a pseudo-year
dfWordsLong$place = sapply(1:nrow(dfWordsLong), function(i) str_remove(dfWordsLong$place[i], 'place'))
dfWordsLong$place = as.numeric(dfWordsLong$place)
dfWordsLong$place = 2000 + dfWordsLong$place

# create a list of colors for each letter
pal = wes_palette("Zissou1", 27, type = 'continuous')
pal = rev(pal)

# create streamgraph
stream = dfWordsLong %>%
  group_by(place, letter) %>%
  tally(wt=freq) %>%
  streamgraph("letter", "n", "place") %>%
  sg_axis_x(26) %>%
  sg_fill_manual(pal) %>%
  sg_legend(show=TRUE, label="Letter: ") %>%
  sg_axis_x(100, "year", "%Y") %>%
  sg_axis_y(tick_count = 0)
  
# save as html widget
# saveWidget(stream, "streamgraphDropdown.html")

# modify the streamgraphDropdown.html directly for aesthetics (e.g., background, legend and tooltip placement)
