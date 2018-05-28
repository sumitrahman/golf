library(tidyverse)
library(lubridate)
# database of golf scores
# last update: 26 May 2018

# Player database----
Players <- data.frame(playerID=1:3,
                      name=c("Sumit Rahman", "Faisal Sheikh", "Nipun Sheikh"),
                      birthday=c(make_date(year=1976, month=11, day=26),
                                 NA,
                                 make_date(year=1980, month=8, day=27)),
                      gender=c("male", "male", "female"))

add.player <- function(name=NA,birthday=NA,gender=NA){
    #if name is not a string, give name as NA and print warning
    #if birthday is not a date before today, give birthday as NA and print warning and syntax of make_date
    #if gender is not male/female give warning, but return as lower case
    
    Players <<- bind_rows(Players, list(playerID=max(Players$playerID)+1,name=name, birthday=birthday, gender=gender))
}

add.player(name="Ferhan Sheikh",gender="male")
add.player(name="Sabri Sheikh",birthday=make_date(2007, 11, 11),gender="male")

# Course database----
Courses <- data.frame(courseID=1,
                      name="Fairlop Waters Hurricane",
                      SSS=NA)

Courses$yards <- list(c(129,90,215,112,160,103,137,125,96))
Courses$par <- list(rep(3,9))
Courses$SI <- list(c(5,7,1,8,2,4,3,6,9))


add.course <- function(name, SSS=NA, yards=NA, par=NA, SI=NA){
        #if name is not a string, give name as NA and print warning
        #if birthday is not a date before today, give birthday as NA and print warning and syntax of make_date
        #if gender is not male/female give warning, but return as lower case
        
        Courses <<- bind_rows(Courses, list(courseID=max(Courses$courseID)+1,name=name, SSS=SSS, yards=list(yards), par=list(par), SI=list(SI)))
}

add.course(name="Adlington Graduate Blue",
           yards=c(127, 110, 249, 175, 171, 145, 224, 162, 156),
           par=rep(3,9),
           SI=c(8, 9, 1, 5, 4, 3, 2, 7, 6))

add.course(name="High Beech Yellow",
           yards=c(155, 215, 173, 169, 195, 149, 168, 152, 129),
           par=rep(3,9),
           SI=c(1, 4, 5, 6, 8, 7, 2, 3, 9))

add.course(name="Los Palos Yellow",
           yards=c(106, 92, 127, 155, 115, 100, 132, 101, 113),
           par=rep(3,9),
           SI=c(6, 8, 3, 1, 2, 5, 7, 9, 4))

add.course(name="Fairlop Waters 18 Yellow",
           SSS=69,
           yards=c(376, 137, 307, 340, 480, 402, 225, 491, 336,
                   290, 359, 126, 337, 489, 487, 340, 314, 182),
           par=c(4, 3, 4, 4, 5, 4, 3, 5, 4, 4, 4, 3, 4, 5, 5, 4, 4, 3),
           SI=c(7, 17, 15, 13, 6, 3, 1, 10, 8, 14, 4, 18, 11, 2, 9, 5, 16, 12))

add.course(name="Fairlop Waters Spitfire Yellow",
           SSS=68,
           yards=c(351, 131, 268, 390, 324, 409, 130, 500, 305,
                   145, 303, 454, 364, 513, 100, 265, 426, 423),
           par=c(4, 3, 4, 4, 4, 4, 3, 5, 4, 3, 4, 4, 4, 5, 3, 4, 4, 4),
           SI=c(6, 16, 12, 8, 14, 2, 18, 4, 10, 13, 11, 1, 9, 5, 15, 17, 3, 7))

add.course(name="Cerrado del Aguila Yellow",
           SSS=NA,
           yards=1.0936133*c(293, 502, 375, 133, 313, 166, 291, 124, 555),
           par=c(4, 5, 4, 3, 4, 3, 4, 3, 5),
           SI=c(8, 1, 2, 4, 6, 5, 7, 9, 3))

add.course(name="Epping Golf Course Yellow",
           SSS=63,
           yards=c(378, 180, 310, 273, 242, 333, 100, 282, 348,
                   175, 405, 136, 297, 345, 294, 284, 140, 359),
           par=c(4, 3, 4, 4, 4, 4, 3, 4, 4, 3, 5, 3, 4, 4, 4, 4, 3, 4),
           SI=c(9, 13, 5, 15, 1, 7, 17, 3, 11, 14, 8, 18, 10, 2, 16, 4, 12, 6))


# Round database----
Rounds <- data.frame(roundID=1,
                      playerID=1,
                      courseID=1,
                     datetime=make_datetime(2017,6,29,9),
                     handicap=FALSE)
Rounds$scores <- list(c(5, 4, 9, 3, 6, 7, 5, 4, 4))
Rounds$putts <- list(c(1, 2, 2, 2, 3, 3, 2, 2, 2))

add.round <- function(playerID, courseID, datetime=NA, handicap=FALSE, scores, putts=NA){
    #if name is not a string, give name as NA and print warning
    #if birthday is not a date before today, give birthday as NA and print warning and syntax of make_date
    #if gender is not male/female give warning, but return as lower case
    
    Rounds <<- bind_rows(Rounds, list(roundID=max(Rounds$roundID)+1,
                                      playerID=playerID,
                                      courseID=courseID,
                                      datetime=datetime,
                                      handicap=handicap,
                                      scores=list(scores),
                                      putts=list(putts)))
}



add.round(playerID=1, courseID=1, datetime=make_datetime(2017,6,29,10), handicap=FALSE,
          scores=c(6, 6, 5, 4, 4, 4, 5, 4, 11),
          putts=c(2, 4, 2, 2, 2, 3, 3, 2, 2))

add.round(playerID=1, courseID=1, datetime=make_datetime(2017,5,11), handicap=FALSE,
          scores=c(4, 2, 5, 5, 5, 5, 6, 4, 3))

add.round(playerID=1, courseID=1, datetime=make_datetime(2017,4,21), handicap=FALSE,
          scores=c(10, 3, 5, 6, 7, 5, 4, 3, 5))

add.round(playerID=2, courseID=1, datetime=make_datetime(2017,4,21), handicap=FALSE,
          scores=c(6, 4, 5, 4, 5, 6, 6, 4, 2))

add.round(playerID=1, courseID=1, datetime=make_datetime(2017,6,22,9), handicap=FALSE,
          scores=c(4, 5, 5, 3, 5, 5, 6, 4, 3),
          putts=c(2, 2, 2, 1, 2, 2, 2, 2, 2))

add.round(playerID=1, courseID=1, datetime=make_datetime(2017,6,22,10), handicap=FALSE,
          scores=c(6, 4, 6, 4, 4, 4, 5, 5, 4),
          putts=c(2, 2, 4, 2, 2, 3, 3, 2, 1))

add.round(playerID=1, courseID=2, datetime=make_datetime(2017,7,8), handicap=FALSE,
          scores=c(4, 5, 7, 5, 3, 5, 5, 4, 4),
          putts=c(2, 3, 2, 2, 2, 2, 3, 2, 3))

add.round(playerID=1, courseID=2, datetime=make_datetime(2017,7,23), handicap=FALSE,
          scores=c(4, 4, 9, 6, 7, 5, 3, 4, 22),
          putts=c(3, 3, 4, 2, 1, 2, 1, 2, 2))

add.round(playerID=1, courseID=3, datetime=make_datetime(2017,7,30), handicap=FALSE,
          scores=c(4, 5, 4, 6, 6, 5, 7, 5, 6),
          putts=c(1, 2, 1, 3, 3, 1, 2, 2, 2))

add.round(playerID=1, courseID=4, datetime=make_datetime(2017,8,24,14), handicap=FALSE,
          scores=c(4, 3, 5, 6, 3, 4, 6, 5, 5),
          putts=c(2, 2, 2, 3, 2, 2, 2, 3, 3))

add.round(playerID=1, courseID=4, datetime=make_datetime(2017,8,24,15), handicap=FALSE,
          scores=c(5, 7, 6, 5, 3, 4, 7, 3, 4),
          putts=c(2, 3, 3, 1, 2, 3, 4, 1, 3))

add.round(playerID=1, courseID=2, datetime=make_datetime(2017,9,3), handicap=FALSE,
          scores=c(4, 4, 5, 5, 4, 6, 5, 5, 4),
          putts=c(2, 2, 2, 3, 2, 2, 1, 2, 2))

add.round(playerID=1, courseID=3, datetime=make_datetime(2017,9,10), handicap=FALSE,
          scores=c(9, 6, 6, 5, 9, 6, 7, 6, 7),
          putts=c(3, 2, 2, 3, 4, 3, 4, 3, 4))

add.round(playerID=1, courseID=5, datetime=make_datetime(2017,9,22), handicap=TRUE,
          scores=c(10, 5, 9, 8, 9, 9, 5, 8, 6, 7, 7, 4, 9, 7, 8, 8, 7, 7),
          putts=c(2, 3, 3, 2, 2, 2, 2, 1, 3, 3, 2, 2, 3, 2, 2, 2, 2, 2))

add.round(playerID=1, courseID=2, datetime=make_datetime(2017,9,24), handicap=FALSE,
          scores=c(5, 5, 4, 6, 7, 7, 5, 4, 4),
          putts=c(2, 2, 2, 2, 2, 3, 2, 2, 3))

add.round(playerID=1, courseID=3, datetime=make_datetime(2017,10,1), handicap=FALSE,
          scores=c(7, 7, 5, 2, 7, 5, 5, 6, 6),
          putts=c(3, 2, 3, 0, 3, 2, 3, 2, 3))

add.round(playerID=1, courseID=5, datetime=make_datetime(2017,10,5), handicap=TRUE,
          scores=c(6, 3, 10, 8, 11, 7, 4, 9, 5, 7, 7, 3, 8, 9, 8, 9, 8, 6),
          putts=c(2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 3, 2, 2, 3, 2, 2, 2, 2))

add.round(playerID=1, courseID=5, datetime=make_datetime(2017,10,12), handicap=TRUE,
          scores=c(11, 5, 7, 11, 7, 11, 6, 8, 6, 6, 7, 3, 7, 9, 8, 9, 6, 5),
          putts=c(3, 2, 2, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2))

add.round(playerID=1, courseID=1, datetime=make_datetime(2017,5,17), handicap=FALSE,
          scores=c(4, 2, 5, 5, 5, 5, 6, 4, 3))

add.round(playerID=2, courseID=1, datetime=make_datetime(2017,7,27), handicap=FALSE,
          scores=c(4, 3, 6, 5, 6, 4, 5, 3, 4))

add.round(playerID=5, courseID=1, datetime=make_datetime(2017,7,27), handicap=FALSE,
          scores=c(5, 7, 8, 5, 8, 6, 4, 10, 5))

add.round(playerID=2, courseID=6, datetime=make_datetime(2017,8,1), handicap=FALSE,
          scores=c(8, 3, 5, 6, 6, 10, 6, 10, 7, 4, 6, 6, 5, 12, 4, 6, 12, NA))

add.round(playerID=4, courseID=6, datetime=make_datetime(2017,8,1), handicap=FALSE,
          scores=c(6, 5, 6, 5, 4, 5, 8, 7, 9, 2, 5, 7, 5, 8, 3, 7, 5, NA))

add.round(playerID=1, courseID=1, datetime=make_datetime(2017,10,15), handicap=FALSE,
          scores=c(3, 5, 6, 5, 5, 5, 4, 4, 4),
          putts=c(2, 2, 2, 3, 2, 2, 2, 2, 2))

add.round(playerID=2, courseID=1, datetime=make_datetime(2017,10,15), handicap=FALSE,
          scores=c(5, 5, 5, 6, 6, 5, 6, 9, 4),
          putts=c(2, 2, 2, 2, 2, 2, 3, 2, 2))

add.round(playerID=3, courseID=1, datetime=make_datetime(2017,10,15), handicap=FALSE,
          scores=c(9, 8, 5, 4, 7, 7, 5, 5, 5),
          putts=c(3, 3, 2, 1, 2, 2, 2, 2, 2))

add.round(playerID=1, courseID=3, datetime=make_datetime(2017,11,16), handicap=FALSE,
          scores=c(6, 6, 5, 8, 7, 4, 10, 8, 9),
          putts=c(1, 3, 1, 3, 3, 2, 2, 2, 2))

add.round(playerID=1, courseID=7, datetime=make_datetime(2018, 4, 18), handicap=FALSE,
          scores=c(7, 14, 6, 8, 6, 5, 6, 5, 8),
          putts=c(1, 2, 2, 3, 2, 2, 2, 3, 3))

add.round(playerID=1, courseID=8, datetime=make_datetime(2018, 5, 24), handicap=TRUE,
          scores=c(8, 6, 11, 6, 5, 6, 7, 7, 7, 5, 7, 8, 4, 6, 5, 6, 4, 7),
          putts=c(3, 3, 4, 2, 1, 3, 4, 2, 4, 2, 2, 2, 2, 1, 3, 3, 2, 3))

# functions----
#validate rounds
#check round in the database
#check there is a valid player
#check there is a valid course
#check date of round is after birthdate of player
#check length of course equals length of round

validate.round <- function(r){
    if(!(r %in% names(rounds))){
        return("Round not in database")
    }
    if(suppressWarnings(
        !any(lapply(X = players,
                    FUN=function(x){identical(rounds[[r]][["player"]],x)})))){
        print("Player not in database")
    }
    if(suppressWarnings(
        !any(lapply(X = courses,
                    FUN=function(x){identical(rounds[[r]][["course"]],x)})))){
        return("Course not in database")
    }
    if(length(rounds[[r]]$shots)!=length(rounds[[r]][["course"]]$par)){
        return("Number of holes played doesn't match length of course")
    }
    if(!is.POSIXct(rounds[[r]][["datetime"]])){
        return("No date")
    }else if (difftime(rounds[[r]][["datetime"]],rounds[[r]][["player"]]$DOB)>0){
        paste0(rounds[[r]][["player"]]$name, 
               " played at ",
               rounds[[r]][["course"]]$name,
               " on ",
               rounds[[r]][["datetime"]],
               ", scoring ",
               sum(rounds[[r]]$shots),
               " on a par ",
               sum(rounds[[r]][["course"]]$par),
               " course.")
    }
}


#validate course
# needs a names
# needs pars
# needs Si
validate.course <- function(c){
    if(!(c %in% names(courses))){
        return("Course not in database")
    }
    if(is.null(courses[[c]]$name)){
        return("There is no course name specified")
    }
    if(is.null(courses[[c]]$par)){
        return("There are no pars specified")
    }
    if(!is.numeric(courses[[c]][["par"]]) | length(courses[[c]][["par"]])==0){
        return("The pars have not been specified correctly")
    }
    if(is.null(courses[[c]]$SI)){
        return("There are no stroke indices specified")
    }
    if(!setequal(courses[[c]]$SI,1:length(courses[[c]]$par))){
        return("The stroke indices have not been specified correctly")
    }
    return("Course is specified correctly")
}

# validate player
# needs name
# needs DOB
validate.player <- function(p){
    if(!(p %in% names(players))){
        return("Player not in database")
    }
    if(is.null(players[[p]]$name)){
        return("There is no name specified")
    }
    if(is.null(players[[p]]$DOB)){
        return("There is no date of birth specified")
    }
    if(!is.POSIXct(players[[p]]$DOB)){
        return("Birthdate is not specifed correctly. Use something like 'DOB=make_datetime(year=1976, month=11, day=26)'")
    }
    if(is.null(players[[p]]$handicap)){
        return("There is no handicap specified.  Maximum of 28 for men and 36 for women")
    }
    if(!is.numeric(players[[p]]$handicap) | length(players[[p]]$handicap)!=1 | players[[p]]$handicap != players[[p]]$handicap){
        return("Handicap is not a whole number")
    }
    return("Player is specified correctly")
}

#Sumit's first handicap 2018 CONGU rules
# only using first three 18 hole Rounds
# 1. adjust scores to no more than double par - this is AGS
# 2. AGD = AGS - SSS
# 3. pick lowest AGD: LAGD
# 4. handicaop = LAGD*1.13/1.237 - truncated to integer, max allowed is 54
# 
# to adjust
# handicap cat    exact       playing buffer zone below BZ: multiply by strokes below BZ
# 1               to 5.4      5       0-1         -0.1
# 2               5.5-12.4    6-12    0-2         -0.2
# 3               12.5-20.4   13-20   0-3         -0.3
# 4               20.5-28.4   21-28   0-4         -0.4
# 5               28.5-36.4   29-36   0-5         -0.5
# 6               36.5-54.0   37-54   0-6         -0.6
# calc. nett score by reducing scores on individual scores to double bogeys (after accounting for SI and handicap)
# calc. nett differential = nett score - CSS/SSS
# 

AGD1 <- sum(pmin(c(rounds[["r0"]][["shots"]],rounds[["r1"]][["shots"]]),5))-54
AGD2 <- sum(pmin(c(rounds[["r2"]][["shots"]],rounds[["r3"]][["shots"]]),5))-54
AGD3 <- sum(pmin(c(rounds[["r4"]][["shots"]],rounds[["r5"]][["shots"]]),5))-54
LAGD <- min(AGD1, AGD2, AGD3)
print(paste("handicap is",floor(LAGD)))

calc.AGD <- function(round){
    course <- Rounds$courseID[Rounds$roundID==round]
    
    sum(pmin(2*(Courses %>% filter(courseID==course) %>% select(par))[[1]][[1]]
             ,(Rounds %>% filter(roundID==round) %>% select(scores))[[1]][[1]])) - Courses$SSS[Courses$courseID==course]
}

filter(Rounds, playerID==1, handicap==TRUE) %>% 
    arrange(datetime) %>% 
    mutate(score=map(scores,sum),
           putts=map(putts,sum)) %>% 
    select(score, putts)

abc <- filter(Rounds, playerID==1, handicap==TRUE) %>% 
    arrange(datetime) %>% 
    mutate(score=map(scores,sum)) %>% 
    head(3) %>% 
    select(roundID, datetime, score, courseID) %>% 
    left_join(select(Courses,courseID, SSS)) %>% 
    mutate(AGD = map(roundID,calc.AGD))
abc
print(paste("handicap is",floor(min(unlist(abc$AGD))*1.13/1.237)))



# pick round (15, 18, 19)


mutate(abc, AGD = map(roundID,calc.AGD))
