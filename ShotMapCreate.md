Create a Half-Pitch Perfect for Shot Maps
================

When using the Statsbomb data for the assemssment of goal scoring oppournitnues it would be handy to have a pitch plot for just half a pitch rather than the [full pitch we made previously](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/CreateAPitchForStatsBomb.md).

The Setup
---------

A few things to start with:

1.  Let's turn the warning messages off. This will allow the tutorial to be easier to follow on Github but is generally bad practice.
2.  Load ggplot2 which is the main graphics package that we will use create the shot map plot. If you haven't installed the package yet then do so by running install.packages('ggplot2') in the terminal.

``` r
options(warn=-1)
require(ggplot2)
```

    ## Loading required package: ggplot2

Getting Your Head Around ggplot2
--------------------------------

ggplot2 is an amazing package that allows you to build reproducable and complex graphics in R. The best way to think about ggplot2 is that with each line of code you aer building layers of graphics that sit on top of each other. The first line goes at the bottom and you add layers gradually. In my ggplot2 tutorials I like to show this layering approach so that you can start to explore and produce your own plots.

Set The Pitch Dimensions
------------------------

Thankfully Statsbomb have included a [guide](https://github.com/statsbomb/open-data/blob/master/doc/StatsBomb%20Event%20Data%20Specification%20v1.0.2.pdf) to help users of their data. Included within this is the pitch dimensions:

So let's start by defining all of the variables related to the dimensions of a Statsbomb pitch, this will allow us to easily update the dimensions if they ever change and to better understand the code we will use.

``` r
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
```

### Creating the Circles

Next is to calculate the data we need to plot the arc at the edge of the box. We reuse the circle function from our previous [radar tutorial](https://github.com/FCrSTATS/Visualisations/blob/master/2.BuildingARadar.md) to help.

The following code will just store the arc data into dataframes that we will use plate to plot the pitch.

``` r
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
```

Let's Get Plotting
------------------

Now we have all the data we will need to generate the pitch plot. So let's build the plot layer-by-layer, step-by-step to help us understand the code.

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax))
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-4-1.png)

This shows the very basic ggplot2 plot. A clean slate but an ugly one! Let's use a custom theme to make it look nicer.

``` r
## set some colours for our plot
grass_colour <- "#ffffff" # white
line_colour <- "#A9A9A9" # dark grey 
background_colour <- "#ffffff" # white
goal_colour <- "#000000" # black 

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
```

Now we have defined the styling lets plot it again using the theme

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() 
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-6-1.png)

Perfect, now we have a blank page styled with a white background as we define. Let's start building up the pitch features one by one:

#### Pitch Boarder

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() +
# add the base rectangle of the pitch 
geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour)
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-7-1.png)

#### 18 Yard Boxes

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() +
# add the base rectangle of the pitch 
geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the 18 yard box offensive
geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) 
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-8-1.png)

#### Goal Lines

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() +
# add the base rectangle of the pitch 
geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the 18 yard box offensive
geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the goal offensive
geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1) 
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-9-1.png)

#### 6 Yard Boxes

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() +
# add the base rectangle of the pitch 
geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the 18 yard box offensive
geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the goal offensive
geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1) +
# add the six yard box offensive
geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) 
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-10-1.png)

#### The Arc

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() +
# add the base rectangle of the pitch 
geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the 18 yard box offensive
geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the goal offensive
geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1) +
# add the six yard box offensive
geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the arc circle 
geom_path(data=dArc, aes(x=x,y=y), colour = line_colour)
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-11-1.png)

#### The Spots

``` r
## initiate the plot, set some boundries to the plot
ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
# add the theme 
theme_blankPitch() +
# add the base rectangle of the pitch 
geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the 18 yard box offensive
geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the goal offensive
geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1) +
# add the six yard box offensive
geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
# add the arc circle 
geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
# add penalty spot 
geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) 
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-12-1.png)

Changing it to a Function
-------------------------

This is a lot of code and would be much easier to manage and re-use if it was a function.

We need to decide which variables we may want to change in later use and then pass these to the function rather than defining them within the function. The obvious variables to change are the color scheme and the pitch dimensions.

We will use function() and then pass the variables like this:

function(grass\_colour, line\_colour, background\_colour, goal\_colour, BasicFeatures)

We then delete where we have defined these in the original code as we wouldn't want to define them twice.

The full function code below:

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
  # add penalty spot 
  geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
     # add the goal offensive
  geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)
  
  return(p)

}
```

### Plot Shot Maps Using the Function

Now we have defined the function it will be each to reuse to create all kinds of styles of shot maps with ease. We are now ready with a shot map background to plot events on top of in a reproducable and easy way.

The only thing that you need to do is make sure the x value for the event is mapped to the y when plotting and vice versa.

#### Black & White

``` r
p <- create_StatsBomb_ShotMap("#ffffff", "#A9A9A9", "#ffffff", "#000000")
p
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-14-1.png)

#### Green

``` r
p <- create_StatsBomb_ShotMap("#538032", "#ffffff", "#538032", "#000000")
p
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-15-1.png)

#### Pink

``` r
p <- create_StatsBomb_ShotMap("#775D6A", "#F4828C", "#775D6A", "#7E3C5D")
p
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-16-1.png)

#### Nighttime

``` r
p <- create_StatsBomb_ShotMap("#202020", "#797876", "#202020", "#131313")
p
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-17-1.png)

#### Blue

``` r
p <- create_StatsBomb_ShotMap("#224C56", "#B3CED9", "#224C56", "#15393D")
p
```

![](ShotMapCreate_files/figure-markdown_github/unnamed-chunk-18-1.png)
