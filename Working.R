library(dplyr)
table(apps$Installs)
unique(apps$Installs)

table(apps$Price)
as.numeric(apps$Price)

is.special <- function(x){
  if (is.numeric(x)) (is.infinite(x) | is.nan(x))
}


#checking for na and special values.

colSums(is.na(apps))

sum(is.nan(apps$Sentiment_Polarity))       
sum(is.na(apps$Sentiment_Polarity))  


nan <- which(is.nan(apps$Sentiment_Polarity))
na <- which(is.na(apps$Sentiment_Polarity))  

setdiff(na, nan)
x <- apps[setdiff(na, nan),]

y <- apps[intersect(na,nan),]

variables <- colnames(apps)

#Date conversion

a <- apps %>% mutate(update = mdy(apps$`Last Updated`))

a <- a %>% mutate(Date = day(a$update), 
                  Month = month(a$update), 
                  Year = year(a$update))

Boxplot(a$Sentiment_Polarity~a$Year)

#price 
str(apps$Price)
table(apps$Price)
a$Price <- apps$Price

a$Price <- substr(a$Price,2,nchar(a$Price)) %>% as.numeric()
table(a$Price)
sum(is.na(a$Price))

#imputing na in price with 0
a$Price[which(is.na(a$Price))] <- 0


#Factor of Type
class(
a$Type)

table(a$Type)
View(a[which(a$Type ==0),])
a <- a[-which(a$Type ==0),]

table(a$Type)
a$Type <- factor(a$Type, levels = c("Free", "Paid"))
class(a$Type)
levels(a$Type)

#Factor Sentiment

class(a$Sentiment)
table(a$Sentiment)

a1 <- a %>% 
  mutate(Sentiment = 
           factor(a$Sentiment, 
                  levels = c("Negative", "Neutral", "Positive"),
                  labels = c("Negative", "Neutral", "Positive"),
                  ordered = T))
str(a1$Sentiment)
a <- a1
View(a1$Sentiment)


#Size - changing numeric
table(a$Size)


#Content Ratings

names(table(a$`Content Rating`))

#order
c("Everyone", "Everyone 10+", "Teen", "Mature 17+", "Adults only 18+")

a$`Content Rating` <- factor(a$`Content Rating`, 
       levels = c("Everyone", "Everyone 10+", "Teen", "Mature 17+", "Adults only 18+"),
       labels = c("Everyone", "Everyone 10+", "Teen", "Mature 17+", "Adults only 18+"),
       ordered = TRUE
       )
str(a$`Content Rating`)

levels(a$`Content Rating`)

#Installs

apps$Installs <- apps$Installs %>% 
  factor(levels = c( "0","0+","1+","5+","10+","50+","100+","500+","1,000+",
                                                     "5,000+","10,000+",  "50,000+", "100,000+",  "500,000+",
                                                     "1,000,000+","5,000,000+" ,  "10,000,000+" ,  "50,000,000+", "100,000,000+",
                                                     "500,000,000+","1,000,000,000+") ,
         labels = c( "0","0+","1+","5+","10+","50+","100+","500+","1,000+",
                                                     "5,000+","10,000+",  "50,000+", "100,000+",  "500,000+",
                                                     "1,000,000+","5,000,000+" ,  "10,000,000+" ,  "50,000,000+",
                                                     "100,000,000+", "500,000,000+","1,000,000,000+"),
         ordered=T)



levels(a1$Installs)
apps$Installs

unique(apps$`Content Rating`)
apps$`Content Rating`<- apps$`Content Rating` %>% factor(levels = c("Everyone", "Everyone 10+" ,  "Teen"  ,
                                                                    "Mature 17+", "Adults only 18+"),
                                                         labels =   c("Everyone", "Everyone 10+" ,  "Teen"  ,
                                                                      "Mature 17+", "Adults only 18+"),ordered = T )

#Factor mutation
a1 <- apps %>% mutate(
  Installs = factor(apps$Installs, 
                    levels = c( "0","0+","1+","5+","10+","50+","100+","500+","1,000+",
                                "5,000+","10,000+",  "50,000+", "100,000+",  "500,000+",
                                "1,000,000+"," 5,000,000+" ,  "10,000,000+" ,  "50,000,000+", "100,000,000+",
                                "500,000,000+","1,000,000,000+") ,
                    labels = c( "0","0+","1+","5+","10+","50+","100+","500+","1,000+",
                                "5,000+","10,000+",  "50,000+", "100,000+",  "500,000+",
                                "1,000,000+"," 5,000,000+" ,  "10,000,000+" ,  "50,000,000+",
                                "100,000,000+", "500,000,000+","1,000,000,000+"),
                    ordered=T),
  Type = factor(apps$Type, 
                levels = c("Free", "Paid"),
                labels = c("Free", "Paid")),
  `Content Rating`= factor(apps$`Content Rating`,
                           levels = c("Everyone", "Everyone 10+",  "Teen", "Mature 17+", "Adults only 18+"),
                           labels = c("Everyone", "Everyone 10+",  "Teen", "Mature 17+", "Adults only 18+"),
                           ordered = T ),
  Sentiment = factor(apps$Sentiment, 
                      levels = c("Negative", "Neutral", "Positive"),
                      labels = c("Negative", "Neutral", "Positive"),
                      ordered = T)
  )


#checking for translated reviews where there are na but not na for semitment polarity

# Updateing factors as 
d$Category <-as.factor(d$Category)
class(d$Category)
l<-d %>% group_by(Category) %>% summarise(m=mean(size_num,na.rm = T))

g<-d %>% group_by(Category) %>% 
  mutate(size_new=ifelse(is.na(size_num), 
                         mean(size_num,na.rm = T),size_num))



#transforming Reviews

Boxplot(log10(apps$Reviews))



#checking outliers 
sapply(apps, is.numeric) %>% kable(col.names = "Numeric column")

boxplot(apps$Reviews)


Boxplot(apps$Rating)
boxplot(apps$Rating~apps$Type)

boxplot(apps$Rating~apps$Type+apps$Sentiment)


boxplot(apps$Sentiment_Polarity)
boxplot(apps$Sentiment_Polarity~apps$Sentiment)

boxplot(apps$Sentiment_Subjectivity)
boxplot(apps$Sentiment_Subjectivity~apps$Sentiment)
boxplot(apps$Sentiment_Subjectivity)

boxplot(hg$size_upd)
boxplot(hg$size_upd~hg$Type)

Price <- str_extract_all(apps$Price,"[:digit:]") 
table(Price)
summary(Price)



# Transform

log10(apps$Reviews)

apps <- apps %>% ungroup() %>%  mutate(Reviews_t=log10(apps$Reviews)
                                       )


#checking special values

is.special <- function(x){
  if (is.numeric(x)) (is.infinite(x) | is.nan(x))
}
sp <- sapply(apps, is.special) %>% data.frame()


n <- colSums(is.na(apps)) %>% as.data.frame()
names(n) <- "NA"

sp <- sapply(apps, is.infinite) %>% as.data.frame() %>% 
colSums()
sp

nan <- sapply(apps, is.nan) %>% as.data.frame() %>% 
colSums()

bind_cols(n, sp, nan)

x <- n %>% mutate(Special = sp, Nan = nan) 
row.names(x) <- colnames(apps)
x

#Checking the top 5 rated categories in the app store

ratings <- apps %>% group_by(Category) %>% summarise("Avg rating"=round(mean(Rating),2))
ratings<-ratings%>% arrange (desc(`Avg rating`))
head(ratings,5)

# highest number of reviews
reviews <- apps %>% group_by(Category) %>% summarise(reviews=round(sum(Reviews),2))
reviews<-reviews%>% arrange (desc(reviews))
head(reviews,5)

# Top rated apps
apprating<-apps %>% group_by(App)  %>% summarise(rating=mean(Rating))
apprating<- apprating %>% arrange(desc(rating))
head(apprating,5)
      
#Sentiment Polarity versus Last updated year
boxplot(apps$Sentiment_Polarity~apps$Year, 
        main="Sentiment spread based on last updated Year",
        xlab = "Year last updated",
        ylab = "Average Sentiment",
        col=(c("lightblue")))

#Sentiment Polarity versus Price
boxplot(Sentiment_Polarity~Price, data = apps,
        main="Sentiment spread based on Price",
        xlab = "Price of App",
        ylab = "Average Sentiment",
        col=(c("gold")))

