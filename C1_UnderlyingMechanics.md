Building a 'Unique' Passing Model - Part 1 - Create the Underlying Mechanics
================

Intro
-----

You are watching a football match, possession is being circulated around the middle in unspectacular fashion. Then all of a sudden, your central midfielder (known for his vision) gets a moment of time and hits a great pass. You know it's a great pass, it's something different, yeah it's not normal, it's..... unique. I am writing this before I set off on a coding journey to see if I can identify pass uniqueness (referred to PU from now on), the following is the documentation of the journey mistakes, tagents and hopefully successes included.

The Data & Setup
----------------

I will be using the recently released dataset from [Statsbomb](https://twitter.com/statsbomb), which is free to use and [easy to access](https://www.github.com). The dataset consists of 11 games at the time of writing, which is equivilent to over 25,000 events. Although this sounds a large dataset I conceed that it might not be suffiecent to gain reliable PU insight, let's hope Statsbomb release more games over the coming months.

##### Loading the Data

``` r
events <- readRDS("SB_events_DB.RDS") ## an events dataframe loaded from local storage - see my previous tutorial 
```

Enviornment
-----------

We will be using a variety of R packages to help us with the analysis. To use a package it first needs to be installed by running install.packages("NAME\_OF\_PACKAGE") and then loading it to your session via require(NAME\_OF\_PACKAGE).

``` r
require(dplyr); require(RANN); require(formattable); require(ggplot2)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: RANN

    ## Loading required package: formattable

    ## Loading required package: ggplot2

There are some packages that need to be installed direct from Github repositories, this is acheived via the devtools pacakge. In our case this will be mine own package [SBpitch](https://www.github.com), which creates a pitch ready to plot Statsbomb event data on top of.

``` r
require(devtools)
```

    ## Loading required package: devtools

``` r
# devtools::install_github("FCrSTATS/SBpitch") ## only run once, uncomment before running and then comment back afterwards. 
require(SBpitch)
```

    ## Loading required package: SBpitch

Data Preparation
----------------

First task is to filter out events that we don't want to include in our PU model. The criteria being:

1.  Only include passes
2.  Remove goal kicks, corners, throw-ins, free-kicks and kick-offs

We will utlise the power of out good friend the dplyr package, this also requires to remove some NAs in some columns that we will use.

``` r
## removing NAs from the pass.outcome.name and pass.type.name columns 
events$pass.outcome.name <- ifelse(is.na(events$pass.outcome.name), "Complete", as.character(events$pass.outcome.name))
events$pass.type.name <- ifelse(is.na(events$pass.type.name),"-",as.character(events$pass.type.name))

## Filter out by our criteria 
passes <- events %>% filter(type.name == "Pass")
passes <- passes %>% filter(pass.type.name != "Goal Kick" & pass.type.name != "Corner" & pass.type.name != "Throw-in" & pass.type.name != "Free Kick" & pass.type.name != "Kick Off")
```

The passes dataframe has 146 variables, I prefer working with more streamlined dataframes during analysis. I am therefore going to create som reference data of passes (passDB) and players (PlayersDB) in case I require extra information later on. I also want to create the dataframe of pass information that I will use for the analysis (passAnalysis)

``` r
## Create the reference data 
passDB <- passes[c(1,8,11,13,15,17,23,25,26:27,40,43,47,101:104,141:146)] ## only selecting certain columns of Pass information
PlayerDB <- unique(events[c(19,23,25)]) ## only selecting certain columns of player information

## Create the dataframe for analysis and calculate the speed of the pass
passAnalysis <- passDB[c(1,2,9:11,14:17)] 
```

Calculating Uniqueness
----------------------

I plan is to compare each pass against the rest of the passes to ascertain it's statistically difference, the varaibles I want to use in this analysis are:

1.  duration - the time the pass took from release to receive
2.  pass.length - distance from origin to destination
3.  pass.angle - the angle of the pass measured in radions
4.  pass.height.id - Statsbomb provide a 3 grade variable for pass height (1) ground pass (2) mid-height (3) high-ball.
5.  location.x - the vertical location of the pass origin
6.  location.y - the horizontal location of the pass origin
7.  location.x - the vertical location of the pass destination
8.  location.y - the horizontal location of the pass destination

These were the columns that I selected to create the passAnalysis dataframe.

To calculate the PU of each pass I will use the Nearest Neighbour (NN) calculation which will provide a 'distance' value for each pass comapared against, which can then be summed to create a total distance value. The higher the number the more different, or 'unique', the pass is compared to others in the dataset.

To make sure each variable above has an equal impact on the NN calculation we need to normalise the data \*\*\*\* ADD EXPLAINER LINK //. We create a simple normalise function and pass the whole passAnalysis data through this leaving us with 'normalised\_data' which is the data we will use for the analysis.

``` r
## Create the normalise function 
normalise <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

## Normalise the data  
normalised_data <- normalise(passAnalysis[2:9])
normalised_data$id <- passAnalysis$id # keeping the pass ID for event tracking 
```

Nearest-Neighbour Calculations
------------------------------

We need to take each pass and then complete a Nearest-Neighbour calculation in comparison to every other pass in the dataset. There are 'better' ways of coding this but I will use a trusted 'for' loop. Check the in-line comments below for an explaination of what is going on.

``` r
## Create an empty dataframe to store the results for each 'loop' 
UniqueCatcher <- data.frame(id = character(), Uniqueness = numeric(), stringsAsFactors = F)

## Create a 'for' loop - see each comment below 
for (p in 1:nrow(normalised_data)) { # p = a number between 1 and the number of rows in the normalised_data dataframe. The for loop will complete the code contained within in 'for' each of these p numbers. 

  selectedPass <- normalised_data[p,] # select just one pass to be compared - the 'selectedPass
  
  ## 
  Selected <- selectedPass[1:8] ## remove the 'id' value as we don't want to use it within the NN calculations 
  Base <- normalised_data[1:8] ## remove the 'id' value as we don't want to use it within the NN calculations 
  
  ## Run the nn2 analysis for the selected pass 
  nnResults <- nn2(Base, Selected, k=min(nrow(Base),nrow(Base)),treetype="bd", searchtype="standard",eps=2)
  
  ## sum all the distances to create a total value of Uniqueness 
  Total.Dist <- sum(nnResults$nn.dists[1,])
  
  ## create a temporary dataframe for the results of our selected pass 
  temp <- data.frame(id = selectedPass$id, Uniqueness = Total.Dist, stringsAsFactors = F)
  
  ## add the results for the selected pass to the overall results catcher via bind_rows 
  UniqueCatcher <- bind_rows(UniqueCatcher, temp)
  
  ## a way of keeping up with progress but is not essential  
  
  if(p/100%%1==0){cat("-")}else{}
}

#saveRDS(UniqueCatcher, "passDB.RDS")
```

Investigation of Initial Results
--------------------------------

Let's create some plots to understand results of the NN analysis. Firstly, a simple histogram of the Total Distance.

``` r
hist(UniqueCatcher$Uniqueness, breaks = 50, main = "Histogram of Uniqueness", xlab = "Uniqueness", col = "#FF9CA2")
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown.png)

The histogram shows a normal distribtion of uniqueness, yet there are a few outliers at the above 7500 Next let's look at the relationship between the variables and uniqueness

#### Pass Origin: location.x (Vertical) & Uniqueness

``` r
## create a findings dataframe 
findings <- merge(passDB, UniqueCatcher, by = "id")

# Relationship between location.y and Uniqueness 
ggplot(data = findings) + geom_point(aes(x= location.x, y = Uniqueness), alpha = 0.3, colour = "#E24F55") + theme_minimal()
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-1.png)

#### Pass Origin: location.y (Horizontal) & Uniqueness

``` r
# Relationship between location.y and distance 
ggplot(data = findings) + geom_point(aes(x= Uniqueness, y = location.y), alpha = 0.3, colour = "#E24F55") + theme_minimal()
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-2.png)

#### Pass Destination: location.x (Vertical) & Uniqueness

``` r
# Relationship between location.y and distance 
ggplot(data = findings) + geom_point(aes(x= location.x, y = Uniqueness), alpha = 0.3, colour = "#E24F55") + theme_minimal()
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-3.png)

#### Pass Destination: location.y (Horizontal) & Uniqueness

``` r
# Relationship between location.y and distance 
ggplot(data = findings) + geom_point(aes(x= Uniqueness, y = location.y), alpha = 0.3, colour = "#E24F55") + theme_minimal()
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-4.png)

#### Pass Angle & Uniqueness

``` r
# Relationship between pass.end_location.y and distance 
ggplot(data = findings) + geom_point(aes(x= findings$pass.angle, y =  findings$Uniqueness), alpha = 0.3, colour = "#E24F55") + theme_minimal() + xlab("Pass Angle") + ylab("Uniqueness")
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-5.png)

#### Pass Length & Uniqueness

``` r
# Relationship between pass.end_location.y and distance 
ggplot(data = findings) + geom_point(aes(x= findings$pass.length, y =  findings$Uniqueness), alpha = 0.3, colour = "#E24F55") + theme_minimal() + xlab("Pass Length") + ylab("Uniqueness")
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-6.png)

Reviewing the results by eye 'Uniqueness' seems to be mainly driven by the origin and desitnation position on the pitch with passes in central areas having a lower 'Uniqueness'. The other variables seem to impact 'Uniqueness' mainly at the extremes of their ranges or not at all.

#### Pass Origin and Uniqueness

``` r
## generate a pitch from the SBpitch package 
p <- create_Pitch(goaltype = "barcanumbers")

# x,y of event plotted with distance as shading = for pass location                 
p + geom_point(data = findings, aes(x=findings$location_2p.x, y = findings$location_2p.y, colour=findings$Uniqueness), alpha = 0.5, position = "jitter", size = 2) + scale_colour_gradient(low = "white", high = "#E24F55") + theme(legend.position="none")  
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-7.png)

#### Pass Destination and Distance

``` r
## generate a pitch from the SBpitch package 
p <- create_Pitch(goaltype = "barcanumbers")

# x,y of event plotted with distance as shading = for pass end location                 
p + geom_point(data = findings, aes(x=findings$pass_end.location_2p.x, y = findings$pass_end.location_2p.y, colour=findings$Uniqueness), alpha = 0.5, position = "jitter") + scale_colour_gradient(low = "white", high = "#E24F55") + theme(legend.position="none")  
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-8.png)

At the moment I am a little worried about the bias towards passes in or into the defensive third having a high 'Uniqueness' value, mathmatically it's right but it doesn't really fit with the aims of the analysis being to identify 'unique' passers in an attacking sense. I experimented with filtering out passes in or into the defensive third and rerunning the analysis but I still wasn't happy with it. Therefore I decided to keep the initial results and attempt to make the analysis more specific to its original aims later in the process.

The Eye Test
------------

Currently we are assuming that the NN calculations worked and similar passes are actually similar! Let's use the SBpitch package to quickly plot some of the passes and they closest matches to make sure our analysis passes the eye test.

#### Create the Data

``` r
## First I select a random pass from the dataset 
RanPass <- round(runif(1, 1, nrow(normalised_data)),0)

## I want to compare against a segment of the overall dataset, this figure will change as Statsbomb add more data so I calculate how much 0.1% of the dataset and also +1 for the pass itself. Finally I save it as a parameter called Pass_Select_Scope. 
Pass_Select_Scope <- round(nrow(normalised_data) * 0.001 ,0) + 1

# I then filter all passes out except the random pass that has been selected. Saving that to a dataframe on it's own called selectedPass
selectedPass <- normalised_data[RanPass,]
  
## I then create two dataframes ready for NN analysis 
Selected <- selectedPass[1:8] ## selected for the RanPass
Base <- normalised_data[,1:8] ## Base for everything else. 
  
## Run the nn2 analysis comparing all passes to the selected pass. 
nnResults <- nn2(Base, Selected, k=min(nrow(Base),nrow(Base)),treetype="bd", searchtype="standard",eps=2)
distance <- nnResults$nn.dists[1,] ## Saving the distance output of the NN
Pass <-  nnResults$nn.idx[1,] ## Saving the index of the NN for later analysis
NNid <- normalised_data[Pass,]$id
  
## Create a database of results 
NN_Review <- data.frame(Uniqeness = distance, id = NNid, stringsAsFactors = F)

## filter just to the closest 0.1% of the pass database 
review_passes <- NN_Review[1:Pass_Select_Scope,]

## Create the pre-plotting dataframes   
 pass_to_plot <- merge(review_passes, passDB[c(1,7,13:21)], by="id")
 pass_to_plot_original <- pass_to_plot[1,]
 pass_to_plot <- pass_to_plot[-1,]
```

#### Let's plot the passes

``` r
## first we use the SBpitch package to create a basic pitch plot, I like the barcanumbers goaltype and I reduced padding to 1 
p <- create_Pitch(goaltype = "barcanumbers", padding = 1)
p
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-9.png)

``` r
## Let's plot all of the passes that are closst matches 
p <- p + geom_segment(data = pass_to_plot, aes(x = location_2p.x, y = location_2p.y, xend = pass_end.location_2p.x, yend = pass_end.location_2p.y), size = 0.75, colour = "#ECAFB1") + geom_point(data = pass_to_plot, aes(x = location_2p.x, y = location_2p.y),colour = "#ECAFB1", size = 2) 
p 
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-10.png)

``` r
## Let's plot the random pass we have compared to all other passes 
p <- p + geom_segment(data = pass_to_plot_original, aes(x = location_2p.x, y = location_2p.y, xend = pass_end.location_2p.x, yend = pass_end.location_2p.y), size = 0.75, colour = "#D43B47") + geom_point(data = pass_to_plot_original, aes(x = location_2p.x, y = location_2p.y),colour = "#D43B47", size = 2) 
p
```

![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-11.png)

Other Examples
--------------
![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-12.png)
![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-13.png)
![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-14.png)
![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-15.png)
![](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/images/passmodel1/Unknown-16.png)


