##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 2                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##


# turn off significance stars in lm output
options(show.signif.stars=FALSE)

(x <- c(1, 2, 3, 4))
(names <- c("John", "Sandy", 'Mary'))
(v <- c(TRUE, FALSE))

cooperation <- c(49,64,37,52,68,54,61,79,64,29,27,58,52,41,30,40,39,44,34,44)

rep(5, 3)
rep(c(1, 2, 3), 2)
rep(1:3, 3:1)
(condition <- rep(c("public", "anonymous"), c(10, 10)))
(sex <- rep(rep(c("male", "female"), c(5, 5)), 2))

(Guyer <- data.frame(cooperation, condition, sex))

library(car)
Prestige
(Prestige <- read.table(file.choose(), header=TRUE))
Prestige <- read.table(
   "http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige.txt",
   header=TRUE)
summary(Prestige)

file <-
"http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige-bugged.txt"
Prestige <- read.table(file, header=TRUE)
(counts <- count.fields(file))
which(counts != 7)

file <-
"http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige-fixed.txt"
Prestige <- read.fwf(file,
    col.names=c("occupation", "education", "income", "women",
        "prestige", "census", "type"),
    row.names="occupation",
    widths=c(25, 5, 5, 5, 4, 4, 4))

# The following example works on Windows
library(RODBC)
channel <- odbcConnectExcel("D:/data/Datasets.xls") # use location on your file system
Prestige <- sqlQuery(channel, "select * from [Prestige$]")
odbcClose(channel)
head(Prestige) # first 6 rows

rownames(Prestige) <- Prestige$F1
Prestige$F1 <- NULL  # remove F1 from Prestige
head(Prestige)
remove(Prestige)  # clean up

library(car)
head(Prestige)
objects()
remove(channel, names, v, x)
detach(package:RODBC)

attach(Duncan)
search()
prestige
Duncan$prestige
attach(Prestige)
search()
prestige

Duncan$prestige
detach(Prestige)
search()
prestige
mean(prestige)
mean(prestige, trim=0.1)
mean <- function(x){
    warning("the mean function in the base package is shadowed")
    sum(x)/length(x)
}

mean <- mean(prestige)  # uses then overwrites our mean function
mean
mean(prestige, trim=0.1)
remove(mean)
detach(Duncan)
mean(Duncan$prestige)
(lm(prestige ~ income + education, data=Duncan))
with(Duncan, mean(prestige))
with(Duncan, lm(prestige ~ income + education))
head(Freedman)  # first 6 rows
dim(Freedman)   # number of rows and columns
head(Freedman$density, 20)  # first 20 values
median(Freedman$density)
median(Freedman$density, na.rm=TRUE)

with(Freedman, {
    plot(density, crime)
    # to exit from identify, right-click on Windows, esc on Mac OS X
    identify(density, crime, row.names(Freedman))
})


log(c(1, 10, NA, 100), base=10)

with(Freedman, plot(log(density, base=10), crime))


lm(crime ~ log(density, base=10), data=Freedman)
abline(lm(crime ~ log(density, base=10), data=Freedman), lty="dashed")

good <- with(Freedman, complete.cases(crime, density))
head(good, 20)  # first 20 values
with(Freedman,
    lines(lowess(log(density[good], base=10), crime[good], f=1.0)))

Freedman.good <- na.omit(Freedman)
head(Freedman.good)  # first 6 rows
dim(Freedman.good)   # number of rows and columns
NA == c(1, 2, NA, 4)
is.na(c(1, 2, NA, 4))
sum(is.na(Freedman))
objects()
remove(good, Freedman.good)
condition
is.character(condition)
remove(cooperation, condition, sex)
Guyer$condition
is.character(Guyer$condition)
is.factor(Guyer$condition)
summary(Guyer)
perc.coop <- 100*Guyer$cooperation/120
Guyer$perc.coop <- 100*Guyer$cooperation/120
head(Guyer)  # first 6 rows
Guyer$cooperation <- with(Guyer, log(perc.coop/(100 - perc.coop)))
head(Guyer)

Guyer$coop.4 <- cut(Guyer$perc.coop, 4)
summary(Guyer$coop.4)
Guyer$coop.groups <- with(Guyer, cut(perc.coop,
    quantile(perc.coop, c(0, 1/3, 2/3, 1)),
    include.lowest=TRUE,
    labels=c("low", "med", "high")))
summary(Guyer$coop.groups)
(Guyer$coop.2 <- recode(Guyer$perc.coop, "lo:50=1; 50:hi=2"))
set.seed(12345)  # for reproducibility
(sample.20 <- sort(sample(nrow(Womenlf), 20)))  # 20 random obs.
Womenlf[sample.20, ]  # 20 randomly selected rows
(seed <- sample(2^31 - 1, 1))
set.seed(seed)
# recode in two ways:
Womenlf$working <- recode(Womenlf$partic,
    ' c("parttime", "fulltime")="yes"; "not.work"="no" ')
Womenlf$working.alt <- recode(Womenlf$partic,
    ' c("parttime", "fulltime")="yes"; else="no" ')
Womenlf$working[sample.20]  # 20 sampled observations
with(Womenlf, all(working == working.alt))  # check
Womenlf$fulltime <- recode(Womenlf$partic,
    ' "fulltime"="yes"; "parttime"="no"; "not.work"=NA ')
Womenlf$fulltime[sample.20]  # 20 sampled observations
Womenlf$region.4 <- recode(Womenlf$region, ' c("Prairie", "BC")="West" ')
Womenlf$region.4[sample.20]  # 20 sampled observations
Womenlf$working.alt.2 <- factor(with(Womenlf,
    ifelse(partic %in% c("parttime", "fulltime"), "yes", "no")))
with(Womenlf, all.equal(working, working.alt.2))
Womenlf$fulltime.alt <- factor(with(Womenlf,
    ifelse(partic == "fulltime", "yes",
        ifelse(partic == "parttime", "no", NA))))
with(Womenlf, all.equal(fulltime, fulltime.alt))
with(Womenlf, all(fulltime == fulltime.alt))
remove(Womenlf)
(A <- matrix(1:12, nrow=3, ncol=4))
(B <- matrix(c("a", "b", "c"), 4, 3, byrow=TRUE)) # 4 rows, 3 columns
dim(A)
dim(B)
set.seed(54321) # for reproducibility
(v <- sample(10, 10))  # permutation of 1 to 10
length(v)
dim(v)
as.matrix(v)
(array.3 <- array(1:24, dim=c(4, 3, 2)))  # 4 rows, 3 columns, 2 layers
(list.1 <- list(mat.1=A, mat.2=B, vec=v))  # a 3-item list
v
v[2]
v[c(4, 2, 6)]
v[c(4, 2, 4)]
v[-c(2, 4, 6, 8, 10)]
names(v) <- letters[1:10]
v
v[c("f", "i", "g")]
v < 6
v[v < 6]  # all entries less than 6
(vv <- v)  # make copy of v
vv[c(1, 3, 5)] <- 1:3
vv
vv[c("b", "d", "f", "h", "j")] <- NA
vv
remove(vv)
A
A[2, 3]  # element in row 2, column 3
A[c(1, 2), 2]  # rows 1 and 2, column 2
A[c(1, 2), c(2, 3)] # rows 1 and 2, columns 2 and 3
A[c(1, 2), ]  # rows 1 and 2, all columns
A[ , 1]
A[ , 1, drop=FALSE]
A[ , -c(1, 3)]  # omit columns 1 and 3
A[-1, -2]     # omit row 1 and column 2
rownames(A) <- c("one", "two", "three")  # set row names
colnames(A) <- c("w", "x", "y", "z")     # set column names
A
A[c("one", "two"), c("x", "y")]
A[c(TRUE, FALSE, TRUE), ]
(AA <- A)  # make a copy of A
AA[1, ] <- 0  # set first row to zeroes
AA
remove(AA)
list.1
list.1[c(2, 3)]  # elements 2 and 3
list.1[2]  # returns a one-element list
list.1[[2]]  # returns a matrix
list.1["mat.1"]  # produces a one-element list
list.1[["mat.1"]]  #  extracts a single element
list.1$mat.1
list.1$mat.1 <- matrix(1, 2, 2)      # replace element
list.1$title <- "an arbitrary list"  # new element
list.1$mat.2 <- NULL                 # delete element
list.1
list.1["title"] <- list(NULL)
list.1
list.1$vec[3]
list.1[["vec"]][3:5]
list.1$foo
head(Guyer)  # first 6 rows
Guyer[ , 1]  # first column
Guyer[ , "cooperation"]  # equivalent
Guyer[c(1, 2), ]  # rows 1 and 2
Guyer[c("1", "2"), "cooperation"] # by row and column names
Guyer[-(6:20), ]  # drop rows 6 through 20
with(Guyer, Guyer[sex == "female" & condition == "public", ])
subset(Guyer, sex == "female" & condition == "public")
Guyer$cooperation
Guyer[["cooperation"]]
head(Guyer["cooperation"]) # first six rows
remove(A, B, v, array.3, list.1)

Hamlet <- c(
"To be, or not to be: that is the question:",
"Whether 'tis nobler in the mind to suffer",
"The slings and arrows of outrageous fortune,",
"Or to take arms against a sea of troubles,",
"And by opposing end them? To die: to sleep;",
"No more; and by a sleep to say we end",
"The heart-ache and the thousand natural shocks",
"That flesh is heir to, 'tis a consummation",
"Devoutly to be wish'd. To die, to sleep;",
"To sleep: perchance to dream: ay, there's the rub;",
"For in that sleep of death what dreams may come",
"When we have shuffled off this mortal coil,",
"Must give us pause: there's the respect",
"That makes calamity of so long life;",
"For who would bear the whips and scorns of time,",
"The oppressor's wrong, the proud man's contumely,",
"The pangs of despised love, the law's delay,",
"The insolence of office and the spurns",
"That patient merit of the unworthy takes,",
"When he himself might his quietus make",
"With a bare bodkin? who would fardels bear,",
"To grunt and sweat under a weary life,",
"But that the dread of something after death,",
"The undiscover'd country from whose bourn",
"No traveler returns, puzzles the will",
"And makes us rather bear those ills we have",
"Than fly to others that we know not of?",
"Thus conscience does make cowards of us all;",
"And thus the native hue of resolution",
"Is sicklied o'er with the pale cast of thought,",
"And enterprises of great pith and moment",
"With this regard their currents turn awry,",
"And lose the name of action. - Soft you now!",
"The fair Ophelia! Nymph, in thy orisons",
"Be all my sins remember'd.")


# alternative to the above:
file <-"http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Hamlet.txt"
Hamlet <- readLines(file)
# end alternative

head(Hamlet)       # first 6 lines
length(Hamlet)     # number of lines
nchar(Hamlet)      # number of characters per line
sum(nchar(Hamlet)) # number of characters in all

lines.1_6 <- paste(Hamlet[1:6], collapse=" ")

strwrap(lines.1_6)
substring(lines.1_6, 1, 42)

strsplit(lines.1_6, " ")
strsplit(lines.1_6, "[ ,;:.?!]+")[[1]]
strsplit(lines.1_6, "[.?!] *")[[1]]


characters <- strsplit(lines.1_6, "")[[1]]
length(characters)    # number of characters
head(characters, 20)  # first 20 characters
all.lines <- paste(Hamlet, collapse=" ")
words <- strsplit(all.lines, " ")[[1]]
length(words)    # number of words
head(words, 20)  # first 20 words
words <- sub("[,;:.?!]", "", words)
head(words, 20)
sub("me", "you", "It's all, 'me, me, me' with you!")
gsub("me", "you", "It's all, 'me, me, me' with you!")
head(words <- tolower(words), 20)  # first 20 words
word.counts <- sort(table(words), decreasing=TRUE)
word.counts[word.counts > 2]  # words used more than twice
head(sort(unique(words)), 20)  # first 20 unique words
length(unique(words))  # number of unique words
grep("-", words)
words[grep("-", words)]
grep("^-", words)
words <- words[- grep("^-", words)] # negative index to delete
head(sort(unique(words)), 20)
grep("!$", c("!10", "wow!"))
grep("^and$", c("android", "sand", "and", "random"))
grep("and", c("android", "sand", "and", "random"))
data <- c("-123.45", "three hundred", "7550", "three hundred 23", "Fred")
data[grep("^[0-9.-]*$", data)]
data[grep("^[^0-9.-]*$", data)]
words[grep("^(the|a|an)$", words)]
grep("\\$", c("$100.00", "100 dollars"))
remove(Hamlet, lines.1_6, characters, all.lines, word.counts, data)
set.seed(123456789) # for reproducibility
X <- rnorm(100000*100)
X <- matrix(X, 100000, 100)

# Windows:
memory.size()
memory.limit()

y <- 10 + as.vector(X %*% rep(1, 100) + rnorm(100000, sd=10))

system.time(m <- lm(y ~ X))
head(coef(m))  # first 6 coefficients
p <- as.vector(1/(1 + exp(-X %*% rep(0.25, 100))))
summary(p)
yy <- rbinom(100000, 1, prob=p)
table(yy)
system.time(m <- glm(yy ~ X, family=binomial))
head(coef(m))  # first 6 coefficients
D <- data.frame(X, y, yy)
dim(D)
object.size(D)
# the following assumes you are usnig Windows with a C drive with
# a directory temp; change names as needed
write.table(D, "C:/temp/largeData.txt")
system.time(DD <- read.table("C:/temp/largeData.txt", header=TRUE))
system.time(DD <- read.table("C:/temp/largeData.txt", header=TRUE,
    colClasses=c("character", rep("numeric", 102))))
save(DD, file="C:/temp/DD.Rdata")
remove(DD)
system.time(load("C:/temp/DD.Rdata"))
dim(DD)
remove(D, p, X, y, yy)

(x <- 1:5)
length(x)
class(x)
mode(x)
(y <- log(x))
length(y)
class(y)
mode(y)
(cv <- c("Abel", "Baker", "Charlie"))
length(cv)
class(cv)
mode(cv)
(lst <- list(x=x, y=y, cv=cv))
length(lst)
class(lst)
mode(lst)
(X <- cbind(x, y))
length(X)
class(X)
mode(X)
head(Duncan)
length(Duncan)
class(Duncan)
mode(Duncan)
Duncan$type
length(Duncan$type)
class(Duncan$type)
mode(Duncan$type)
length(lm)
class(lm)
mode(lm)

(mod <- lm(prestige ~ income + education, data=Duncan))
length(mod)
class(mod)
mode(mod)
X # a matrix
attributes(X)
attributes(Duncan) # a data frame
attributes(mod) # a linear-model object
str(Duncan) # a data frame
(num <- numeric(5))  # create numeric vector of 0s of length 5
(fac <- factor(c("a","b","c","c","b","a")))  # create factor
vector(mode="numeric", length=5)
vector(mode="list", length=2)
is.numeric(num)  # predicate for mode numeric
is.numeric(fac)
is.factor(fac)  # predicate for class factor

is(fac, "factor")
is(fac, "numeric")
(char <- as.character(fac))  # coerce to mode character
(num <- as.numeric(fac))  # coerce to mode numeric
(as.factor(num)) # coerce to class factor
as.numeric(char)
as(fac, "character")
as(fac, "numeric")
is.integer(2)
is.integer(2L)
is.integer(1:100)
is.integer(2L + 3L)
is.integer(2L*3L)
is.integer(4L/2L)
is.integer(sqrt(4L))
is.integer(2L + 3)
(sqrt(2))^2 == 2
(sqrt(2))^2 - 2
all.equal((sqrt(2))^2, 2)
all.equal(2, 4)
all.equal(2, "2")
(z <- complex(real=0, imaginary=1))
(w <- 2 + 2*z) # 2 is coerced to complex
w*z
w/z
1/0
-1/0
0/0
