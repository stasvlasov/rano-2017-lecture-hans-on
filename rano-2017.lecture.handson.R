## Install and load required R packages
## --------------------------------------------------------------------------------
packages <- c("igraph"
            , "network"
            , "sna"
            , "stringr"
            , "magrittr"
            , "htmlwidgets"
            , "networkD3"
            , "pbapply"
            , "dplyr"
            , "stringi"
              ## , "stargazer"  # useless for netlm
            , "texreg")
packages.check <- lapply(packages, function(package) {
    print(paste("Setting up package:", package))
    if (!require(package, character.only = TRUE)) {
        install.packages(package
                       , repos = 'http://cloud.r-project.org'
                       , dependencies = TRUE)
        library(package, character.only = TRUE)}})



## Load data collected during RANO lecture into the memory
## --------------------------------------------------------------------------------
## First, specify where the data is located and what is the name of the file
data.path <- "./"  # note that on windows / is usually \
data.file <- "rano-2017-hands-on.data.csv"
data <- read.csv(file.path(data.path, data.file)
               , stringsAsFactors = FALSE
               , colClasses = "character")
## Specify columns of interest and how to rename it
data.select <- c("name" ,"Name"
               , "age" ,"Age"
               , "gen" ,"Gender"
               , "country" ,"Country"
               , "confer" ,"How.many.times..if.ever..you.participated.at.conference."
               , "cuisine" ,"What.is.your.favorite.cuisine."
               , "tvshow" ,"What.is.your.favorite.TV.show."
               , "music" ,"What.is.your.favorite.music.genre."
               , "concert.may" ,"Which.music.festival.or.concert.if.any.have.you.been.to.in.May."
               , "concert.april" ,"Which.music.festival.or.concert.if.any.have.you.been.to.in.April."
               , "concert.summer" ,"Which.music.festival.or.concert.if.any.are.you.going.to.this.summer."
               , "friends" ,"Name.those.course.participants.with.whom.you.are.friends.")
## Filter columns of interest
data <- data[data.select[c(F,T)]]
## Rename
names(data) <- data.select[c(T,F)]


## Clean up the data
## --------------------------------------------------------------------------------
#' Cleans strings for better matching
clear.string <- function(x) {
    gsub("\\s+", " ", x) %>%
        str_conv("UTF-8") %>%
        str_trim %>%
        toupper %>%
        chartr(enc2utf8("ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ")
             , "SOZsozYYuAAAAAAACEEEEIIIIDNOOOOOOUUUUYsaaaaaaaceeeeiiiionoooooouuuuyy", .) %>%
        stri_replace_all("", regex = c("\\U0001f614", "#0475", "^NONE$", "-"), vectorize_all = FALSE)
}

## Remove observations without names
data %<>% filter(name != "57")  # code 57 correspond to name ""
## Fill mean age when the value is 0
data$age %<>% as.numeric %>% ifelse(. == 0, NA, .) %>% ifelse(is.na(.), mean(., na.rm = TRUE), .) %>% round
## Clean some columns
data %<>% mutate(name = name %<>% clear.string
           , friends = friends %<>% clear.string
           , tvshow = tvshow %<>% clear.string
           , concert.april = concert.april %<>% clear.string
           , concert.may = concert.may %<>% clear.string
           , concert.summer = concert.summer %<>% clear.string)


## Split the values for questions where multiple answers allowed
## --------------------------------------------------------------------------------
split.sep <- function(x) lapply(str_split(x, "[,/]"), clear.string)
split.spc <- function(x) str_split(x, "\\s+") %>% unlist
data$friends.list <- data$friends %>% split.sep %>% lapply(split.spc)
data$tvshow.list <- data$tvshow %>% split.sep
data$concert.april.list <- data$concert.april %>% split.sep
data$concert.may.list <- data$concert.may %>% split.sep
data$concert.summer.list <- data$concert.summer %>% split.sep


## Anonymazer (alredy implemented on data)
## --------------------------------------------------------------------------------
## fnames <- c(data$name, data$friends.list %>% unlist) %>% factor
## mnames <- match(data$name, fnames)
## mfriends <- sapply(data$friends.list, function(friends) {
##     match(friends, fnames) %>% paste(collapse = ", ")
## })
## data$Name <- mnames
## data$Name.those.course.participants.with.whom.you.are.friends. <- mfriends
## write.csv(data, file = "rano-2017-hands-on.data.csv")


## Create adjacency matrixes
## --------------------------------------------------------------------------------
## Projects 2-mode network
make.affil.matrix <- function(v) sapply(v, function(i) {
    sapply(v, function(j) {
        sum(i %in% j)
        })
})
## Calculates matrix of differences
make.diff.matrix <- function(v) sapply(v, function(i) {
    sapply(v, function(j) {
        abs(as.numeric(i) - as.numeric(j))
        })
})
## Calculates simple adjacency matrix
make.adj.matrix <- function(v.list, v) sapply(v.list, function(f) 
    sapply(v %in% f, sum))


## Make all the matrixes
mat <- data[c("age"
       , "gen"
       , "country"
       , "confer"
       , "cuisine"
       , "tvshow.list"
       , "music"
       , "concert.may.list"  
       , "concert.april.list"
       , "concert.summer.list")] %>%
    lapply(make.affil.matrix)

mat$age.diff <- data$age %>% make.diff.matrix
mat$friendship <- data$friends.list %>% make.adj.matrix(data$name)
mat$concert.all <- mat$concert.may.list + mat$concert.april.list + mat$concert.summer.list

## Rename rows and cols
rename.matrix <- function(matrix, name.list) {
        rownames(matrix) <- name.list
        colnames(matrix) <- name.list
        return(matrix)
}
dename.matrix <- function(matrix) {
        rownames(matrix) <- NULL
        colnames(matrix) <- NULL
        return(matrix)
}

mat %<>% lapply(rename.matrix, data$name) 

mat %<>% lapply(dename.matrix) 


## Make graph objects and objects for visualizing with networkD3 package
## --------------------------------------------------------------------------------
igraph <- lapply(mat, function(matrix) {
    graph_from_adjacency_matrix(matrix
                              , mode = "plus"
                              , weighted = TRUE
                              , diag = FALSE)
})
netd3 <- lapply(igraph, igraph_to_networkD3)




## Visualize music similarities network
netd3$music$nodes$cuisine <- data$cuisine
forceNetwork(Links = netd3$music$links
           , Nodes = netd3$music$nodes,
           , Source = "source"
           , Target = "target"
           , Value = "value"
           , NodeID = "name"
           , Group = "cuisine"
           , zoom = TRUE
           , opacity = 0.9)

## Save visualization to file
## %>%
##     saveNetwork(file = "~/mega/teach/rano-2017/lecture/hads-on/org-export-html/img/music.network.html"
##               , selfcontained = TRUE)



## Visualize friendship network
## netd3$friendship$nodes$name <- data$name
netd3$friendship$nodes$gen <- data$gen
forceNetwork(Links = netd3$friendship$links
           , Nodes = netd3$friendship$nodes,
           , Source = "source"
           , Target = "target"
           , Value = "value"
           , NodeID = "name"
           , Group = "gen"
           , zoom = TRUE
           , opacity = 0.9)

## Save visualization to file
## %>%
## saveNetwork(file = "~/mega/teach/rano-2017/lecture/hads-on/org-export-html/img/friendship.html"
##               , selfcontained = TRUE)




## Visualize food similarities network
## netd3$cuisine$nodes$music <- data$music
## forceNetwork(Links = netd3$cuisine$links
##            , Nodes = netd3$cuisine$nodes,
##            , Source = "source"
##            , Target = "target"
##            , Value = "value"
##            , NodeID = "name"
##            , Group = "music"
##            , zoom = TRUE
##            , opacity = 0.9)




## QAP Analysis
##--------------------------------------
mod <- list()  # save regression results here
## Explain planned co-participation in summer concerts
mod$concert <- netlogit(mat$concert.summer
                      , mat[c("music"
                            , "concert.april.list"
                            , "concert.may.list")]
                      , nullhyp = 'qap')

## Explain frienship
mod$friendship.1 <- netlogit(mat$friendship
                           , mat[c("concert.all"
                                   ## , "tvshow.list"
                                   ## , "cuisine"
                                   ## , "gen"
                                   ## , "country"
                                   )]
                           , nullhyp = 'qap')

## Explain frienship
mod$friendship.2 <- netlogit(mat$friendship
                           , mat[c("concert.all"
                                 , "tvshow.list"
                                   ## , "cuisine"
                                   ## , "gen"
                                   ## , "country"
                                   )]
                           , nullhyp = 'qap')

## Explain frienship
mod$friendship.3 <- netlogit(mat$friendship
                           , mat[c("concert.all"
                                 , "tvshow.list"
                                 , "cuisine"
                                   ## , "gen"
                                   ## , "country"
                                   )]
                           , nullhyp = 'qap')

## Explain frienship
mod$friendship.4 <- netlogit(mat$friendship
                           , mat[c("concert.all"
                                 , "tvshow.list"
                                 , "cuisine"
                                 , "gen"
                                   ## , "country"
                                   )]
                           , nullhyp = 'qap')


## Explain frienship
mod$friendship.5 <- netlogit(mat$friendship
                           , mat[c("concert.all"
                                 , "tvshow.list"
                                 , "cuisine"
                                 , "gen"
                                 , "country"
                                   )]
                           , nullhyp = 'qap')


## Save results
save.image("rano-2017.lecture.hands-on.Rdata")




names(mod) <- c("Concerts"
      , "Model 1"
      , "Model 2"
      , "Model 3"
      , "Model 4"
      , "Model 5")


## Print tables
## --------------------------------------------------------------------------------

screenreg(mod[-1]
        , stars = c(0.01, 0.05, 0.1, 0.2)
        , custom.coef.names = c("(Intercept)"
                              , "Concert co-participation"
                              , "Favourite TV-show"
                              , "Cuisine Preferences"
                              , "Gender"
                              , "Country"))


htmlreg(mod[-1]
      , stars = c(0.01, 0.05, 0.1, 0.2)
      , symbol = "&#8224;"
        , custom.coef.names = c("(Intercept)"
                              , "Concert co-participation"
                              , "Favorite TV-show"
                              , "Cuisine Preferences"
                              , "Gender"
                              , "Country"))

htmlreg(mod[1]
      , stars = c(0.01, 0.05, 0.1, 0.2)
      , symbol = "&#8224;"
        , custom.coef.names = c("(Intercept)"
                              , "Gender"
                              , "Concerts in April"
                              , "Concerts in May"))


