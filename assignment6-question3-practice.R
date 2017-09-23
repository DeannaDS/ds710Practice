#read in the encrypted counts
encB <-  read.csv("ds710fall2017assignment6-Brisbin/EncB.csv")
#read in the letter frequencies data
freq <- read.csv("ds710fall2017assignment6-Brisbin/Letter Frequencies.csv")

#sort the frequencies
freq.sortedEnglish <- freq[order(freq$English),]
#create a new vector of the encrypted letters
comparison <- encB[order(encA$frequency), ]

#update the names of the columns
names(comparison) <- c("enc_letter", "enc_freq")


#we want the Letter and English columns from the freq.SortedEnglish as vectors to add to the existing dataframe
engFreq <- as.vector(freq.sortedEnglish["English"])
colnames(engFreq) <- "english_freq"
engLetters <- as.vector(freq.sortedEnglish["Letter"])
colnames(engLetters) <- "english_letter"


#add these two vectors to our dataframe
comparison$english_letter <- engLetters
comparison$english_freq <- engFreq

comparison
