Plotting Shots Using Statsbomb Data
================

In the previous tutorial we learnt how to create a plot map background for use with Statsbomb data. Let's use what we learn and then plot some shots for one particular game.

Before We Start
---------------

If you have not done so already check out:

1.  [Getting Started With StatsBomb Data](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/1.GettingStartedWithStatsBombData.md)
2.  [Creating a Statsbomb Ready Shot Map with ggplot2](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/3.CreateShotMaps.md)

The Setup
---------

We need to turn off warnings and load the ggplot2 and dplyr packages.

``` r
options(warn=-1)
require(ggplot2); require(dplyr)
```

    ## Loading required package: ggplot2

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Create The Base Shot Map
------------------------

Let's recreate the function that we created in our [previous tutorial]((https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/3.CreateShotMaps.md)).

``` r
create_StatsBomb_ShotMap <- function(grass_colour, line_colour, background_colour, goal_colour){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
    ymin <- 0 # minimum width
    ymax <- 80 # maximum width
    xmin <- 60 # minimum length
    xmax <- 120 # maximum length
    
    # Defining features along the length
    boxEdgeOff <- 102
    sixYardOff <- 114
    penSpotOff <- 108
    halfwayline <- 60
    
    # Defining features along the width
    boxEdgeLeft <- 18
    boxEdgeRight <- 62
    sixYardLeft <- 30 
    sixYardRight <- 50
    goalPostLeft <- 36
    goalPostRight <- 44
    CentreSpot <- 40   
    
    # other dimensions
    centreCirle_d <- 20   
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create leftD arc ####
  dArc <- circleFun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]
    
    ## initiate the plot, set some boundries to the plot
  p <- ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
  # add the theme 
  theme_blankPitch() +
  # add the base rectangle of the pitch 
  geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
  # add the 18 yard box offensive
  geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
  # add the six yard box offensive
  geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
  # add the arc circle 
  geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
     # add the goal offensive
  geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)
  
  return(p)

}

## plot the base shot map 
p <- create_StatsBomb_ShotMap("#ffffff", "#A9A9A9", "#ffffff", "#000000")
p
```

![](PlotingShots_files/figure-markdown_github/unnamed-chunk-2-1.png)

Now we have a base map let's plot the shots of one team, to do this we need to wrangle some data.

Creating the data we need
-------------------------

First we load in our saved events DB

``` r
events <- readRDS("SB_events_DB.RDS")
```

Now we are going to use the power of the dplyr package to trim the data down to just the shots of one team from one match.

``` r
dat <- events %>% filter(match_id == "7443" & team.name == "Portland Thorns FC" & type.name == "Shot")
## add a goal count to help plotting
dat$Goal <- ifelse(dat$shot.outcome.name == "Goal","1","0")
```

Plotting the Shots
------------------

#### Basic Version

``` r
p + geom_point(data = dat, aes(x=location.y, y=location.x), colour = "#DF5058")
```

![](PlotingShots_files/figure-markdown_github/unnamed-chunk-5-1.png)

#### Goal Outcomes Version

``` r
p + geom_point(data = dat, aes(x=location.y, y=location.x, colour = Goal)) +
  theme(legend.position="none") + 
  scale_colour_manual(values = c("#F1BEBE", "#DF5058"))
```

![](PlotingShots_files/figure-markdown_github/unnamed-chunk-6-1.png)

#### xG and Goal Outcomes Version

``` r
p + geom_point(data = dat, aes(x=location.y, y=location.x, size=shot.statsbomb_xg, colour = Goal)) + 
  theme(legend.position="none") + 
  scale_colour_manual(values = c("#F1BEBE", "#DF5058"))
```

![](PlotingShots_files/figure-markdown_github/unnamed-chunk-7-1.png)

#### Titles, Totals, xG and Goal Outcomes Version

``` r
p + geom_point(data = dat, aes(x=location.y, y=location.x, size=shot.statsbomb_xg, colour = Goal)) + 
  theme(legend.position="none") + 
  scale_colour_manual(values = c("#F1BEBE", "#DF5058")) + 
  geom_text(aes(x = 2, y=68,label = dat$team.name[1]), hjust=0, vjust=0.5, size = 5, colour = "#DF5058") +
  geom_text(aes(x = 2, y=66,label = paste0("Expected Goals (xG): ",round(sum(dat$shot.statsbomb_xg),2))), hjust=0, vjust=0.5, size = 3) + 
  geom_text(aes(x = 2, y=64,label = paste0("Actual Goals: ",round(sum(as.numeric(dat$Goal)),0))), hjust=0, vjust=0.5, size = 3) + 
    geom_text(aes(x = 2, y=62,label = paste0("xG Difference: ",round(sum(as.numeric(dat$Goal)),0)-round(sum(dat$shot.statsbomb_xg),2))), hjust=0, vjust=0.5, size = 3) 
```

![](PlotingShots_files/figure-markdown_github/unnamed-chunk-8-1.png)
