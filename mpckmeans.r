#install.packages("conclust")
#install.packages("sjmisc")
library(conclust)
library(sjmisc)
#setwd('C:/Users/---------------')
wine1 <- read.table("wine.csv",sep=",",header=FALSE)
wine = data.matrix(wine1)
head(wine)
#standardization
wine_sc=scale(wine[,2:14])

# single code for specific constraints
mustLink=matrix(c(58, 59, 75, 64), nrow = 2)
cantLink=matrix(c(59, 60, 41, 15), nrow = 2)

k = 3
pred = mpckm(wine_sc, k, mustLink, cantLink)

# vector that reports cluster index for each element
pred

# more complicated code for experiments automatization

# vectors intialization
time_vector <-  vector() # execution times
rand_i_vector <- vector() # rand index ranks
must_uniq_elements_vec <- vector() # unique elements amount for must-Link constraints
cant_uniq_elements_vec <- vector() # unique elements amount for cant-Link constraints
united_uniq_elements_vec <- vector() # unique elements amount in total

# variable initialization for the while operation. We give a large number, larger than the dataset rows
e=200
nrow_mustLink=e
nrow_cantLink=e

# loop for 1:1 ratio of must-Link / cannot-Link constraints
for(i in 1:150) {
    sum_na=1
    
    while (sum_na != 0 || nrow_mustLink!=i || nrow_cantLink!=i) {
    
    # generate random pairs of must - cannot constraints
    mustLink=matrix(sample.int(178, size = (i)*2, replace = TRUE), nrow = i, ncol = 2)
    cantLink=matrix(sample.int(178, size = i*2, replace = TRUE), nrow = i, ncol = 2)
    
    # check that no pair is generated more than one time
        
    # the "while" loop checks the amount of unique pairs. If they are fewer than i, that means that there are duplicates, and so, the while loops repeats until all pairs are unique
    must_uniq=unique(t(apply(mustLink, 1, sort )))
    cant_uniq=unique(t(apply(cantLink, 1, sort )))

    nrow_mustLink=nrow(must_uniq)
    nrow_cantLink=nrow(cant_uniq)    
        
           
    cantLink_rev <- subset(cantLink, select=c(2,1))

    require(data.table)
    must = setkey(data.table(mustLink))
    cant = setkey(data.table(cantLink))

    cant_rev = setkey(data.table(cantLink_rev))
    
    # we ask to delete every row (pair) that is common on must and cannot constraints tables. If there are no common pairs, the "while" loop stops
    na1 = na.omit( cant[must,which=TRUE] )
    na2 = na.omit( cant_rev[must,which=TRUE] )
    sum_na = sum(na1)+sum(na2)
    }
    
    # mpck-means for k=3 (because three are the classes of the dataset) and timing
    k = 3
    start_time <- Sys.time()
    ############################
    pred = mpckm(wine_sc, k, mustLink, cantLink)
    ############################
    end_time <- Sys.time()
    #print(pred)
    time=end_time - start_time
    #print(time)
    time_vector <- c(time_vector, time)
    
    # count the amount of elements
    must_uniq_elements=length(unique(as.vector(mustLink)))
    must_uniq_elements_vec<- c(must_uniq_elements_vec, must_uniq_elements)
    
    cant_uniq_elements=length(unique(as.vector(cantLink)))
    cant_uniq_elements_vec<- c(cant_uniq_elements_vec, cant_uniq_elements)
    
    constr_united<-rbind(mustLink,cantLink)
    united_uniq_elements=length(unique(as.vector(constr_united)))
    united_uniq_elements_vec<- c(united_uniq_elements_vec, united_uniq_elements)

    
    # Rand Index calculation
    predlist=pred # this is the vector that has the cluster index for each element after the mpck-means clustering
    truthlist=wine[,1] # this is the vector that has the true class index for each element
    
    # function that increments a variable amount by one
    `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
    
    a=0
    b=0
    
    # check every possible pair, regarding whether they are in the same group both after the clustering AND in the original dataset (a of the Rand Index formula) or in different group (b of the Rand Index formula)

    for (i in 1:177) {

        for (j in i:177) {
            if (predlist[i] == predlist[j+1] && truthlist[i] == truthlist[j+1]) {
                a%+=%1
                }
            if (predlist[i] != predlist[j+1] && truthlist[i] != truthlist[j+1]) {
                b%+=%1
                }
             }
          }
    #  a+b+c+d sum is the amount of every possible pair
    abcd=0
    for (i in 1:178) {
        abcd%+=%i
       }
    RI = (a+b)/abcd
    rand_i_vector <- c(rand_i_vector, RI)
    
    
    
    }