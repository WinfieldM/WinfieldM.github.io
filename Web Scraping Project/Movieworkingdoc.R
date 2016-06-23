`5000films` <- read.delim("C:/Users/User/Desktop/Movies/5000films.txt", header=FALSE, stringsAsFactors=FALSE)
View(`5000films`)
film <- `5000films`
View(film)

MovieBoxOffice1 <- read.delim("C:/Users/User/Desktop/Movies/MovieBoxOffice1.txt", header=FALSE, stringsAsFactors=FALSE)
View(MovieBoxOffice1)
film <- MovieBoxOffice1
names(film) <- c("Release Date", "Movie Title", "Production Budget", "Domestic Grosses", "Worldwide Grosses")

#Because these are strings, we need to remove the $ and convert to numerics.
film$`Worldwide Grosses` <- sapply(strsplit(film$`Worldwide Grosses`, split='$', fixed=TRUE), function(x) (x[2]))
film$`Worldwide Grosses` <- gsub(",","",film$`Worldwide Grosses`)
film$`Worldwide Grosses` = as.numeric(film$`Worldwide Grosses`)
film$`Worldwide Grosses`

View(film)

film$`Domestic Grosses` <- sapply(strsplit(film$`Domestic Grosses`, split='$', fixed=TRUE), function(x) (x[2]))
film$`Domestic Grosses` <- gsub(",","",film$`Domestic Grosses`)
film$`Domestic Grosses` = as.numeric(film$`Domestic Grosses`)
film$`Domestic Grosses`

View(film)

film$`Production Budget` <- sapply(strsplit(film$`Production Budget`, split='$', fixed=TRUE), function(x) (x[2]))
film$`Production Budget` <- gsub(",","",film$`Production Budget`)
film$`Production Budget` = as.numeric(film$`Production Budget`)
film$`Production Budget`

View(film)

film$`Release Date` = as.Date(film$`Release Date`, format = "%m/%d/%Y")
film$`Release Date`

View(film)


#Create Foreign Grosses column
film$'Foreign Grosses' = film[,5]-film[,4]
View(film)

format(film$`Release Date`,'%b')

library(dplyr)


View(film)
       

#Convert release dates into as.date, then into strategic timing factors with a function --> new column 
    #https://en.wikipedia.org/wiki/Dump_months
# I categorized all of the release dates into four buckets: Dump, Awards, Summer, and Aggressive.
# The Dump factor includes the second week of August until the second week of September, January, and February.
# Awards includes September 15th until December 31st.
#Summer includes May until the first week of August(to account for timing decisions related to films like Suicide Squad and Guardians of the Galaxy).
#Aggressive includes March and April, to account for timing decisions related to films like Captain America: The Winter Soldier and Batman v. Superman: Dawn of Justice.
#It is called Aggressive because these are 'summer' films released early to capture abnormal profits prior to the official start of the summer calendar.

film <- mutate(film, Month = format(film$`Release Date`,'%b'))
c <- filter(film, film$Month == 'Dec'|film$Month == 'Sep'|film$Month == 'Nov'|film$Month == 'Oct')
c <- mutate(c, Timing = 2)
d <- filter(film, film$Month == 'May'|film$Month == 'Jun'|film$Month == 'Jul')
d <- mutate(d, Timing = 3)
e <- filter(film, film$Month == 'Mar'|film$Month == 'Apr')
e<- mutate(e, Timing = 4)
f <- filter(film, film$Month == 'Aug'|film$Month == 'Jan'|film$Month == 'Feb')
f<- mutate(f, Timing = 1)
b <- rbind(c,d,e,f)
film <- b
film$Timing <- as.factor(film$Timing)


1 = Dump months
2 = Awards months
3 = Summer
4 = Aggressive

train = non-empty
test = empty

# into factors for SVM: -1 equals 1 billion or more:
  
y <- filter(film, film$`Worldwide Grosses` >= 1000000000)
y <- mutate(y, Billion = -1)
z <- filter(film, film$`Worldwide Grosses` >= 800000000 & film$`Worldwide Grosses` < 1000000000)
z <- mutate(z, Billion = 1)
v <- filter(film, film$`Worldwide Grosses` < 800000000 & film$`Worldwide Grosses` >= 531000000)
v <- mutate(v, Billion = 2)
j <- filter(film, film$`Worldwide Grosses` < 531000000)
j <- mutate(j, Billion = 3)
x <- rbind(y,z,v,j)
film <- x

View(film)

set.seed(0)
x1 = film$`Production Budget`
x2 = film$`Release Date`
y = as.factor(film$Billion)
nonlinear = data.frame(x1, x2, y)
plot(nonlinear$x1, nonlinear$x2, col = nonlinear$y)

library(e1071)

set.seed(0)
nonlinear.test = nonlinear[c(37, 159, 55, 82, 122, 149),]
nonlinear.train = nonlinear[c(1:36,38:54, 56:81, 83:121, 123:148, 150:158, 160:340),]

set.seed(0)
cv.svm.radial = tune(svm,
                     y ~ .,
                     data = nonlinear.train,
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-1, 1.5, length = 20)),
                                   gamma = 10^(seq(-2, 1, length = 20))))


library(rgl)
plot3d(cv.svm.radial$performances$cost,
       cv.svm.radial$performances$gamma,
       cv.svm.radial$performances$error,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

best.nonlinear.model = cv.svm.radial$best.model
summary(best.nonlinear.model)

ypred = predict(best.nonlinear.model, nonlinear.test)

svm.best.nonlinear = svm(y ~ .,
                         data = nonlinear,
                         kernel = "radial",
                         cost = best.nonlinear.model$cost,
                         gamma = best.nonlinear.model$gamma)
plot(svm.best.nonlinear, nonlinear)
summary(svm.best.nonlinear)
svm.best.nonlinear$index
ypred = predict(svm.best.nonlinear, nonlinear)
table("Predicted Values" = ypred, "True Values" = nonlinear[, "y"])


library(gbm)

f = film$`Foreign Grosses`
g = film$Timing
h = film$`Production Budget`
d = film$`Domestic Grosses`

set.seed(0)
boostf.film = gbm(f ~ g+h, data = nonlinear.train,
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4)

par(mfrow = c(1, 1))
summary(boostf.film)

boostd.film = gbm(d ~ g+h, data = nonlinear.train,
                 distribution = "gaussian",
                 n.trees = 10000,
                 interaction.depth = 4)

par(mfrow = c(1, 1))
summary(boostd.film)

par(mfrow = c(1, 2))
plot(boostf.film, i = "g")
plot(boostf.film, i = "h")
#Foreign grosses are higher in 2 and 3, but higher in 2 than in 3, and seem to increase as the budget does.

par(mfrow = c(1, 2))
plot(boostd.film, i = "g")
plot(boostd.film, i = "h")
#Domestic grosses are higher in 2 and 3, but higherin 3 than in 2, and seem to increase as the budget does, but by nowhere near as much as foreign grosses.


