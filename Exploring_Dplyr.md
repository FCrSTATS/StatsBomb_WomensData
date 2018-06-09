xG and dplyr - group\_by(), arrange() and top\_n()
================

The dplyr package in R is an extremely powerful tool that helps organise, group and filter data with intuitive ease. We will show its power using StatsBomb free women's football data. To get the data and be setup for this tutorial follow my previous [guide](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/1.GettingStartedWithStatsBombData.md).

First of all we will load the events database we saved as an RDS file from the initial Statsbomb Guide.

``` r
events <- readRDS("SB_events_DB.RDS")
head(events[1:5]) # just to test it loaded well 
```

    ##                                     id index period    timestamp minute
    ## 1 1055bdac-0320-4ca4-b0a1-38624245501a     1      1 00:00:00.000      0
    ## 2 78e63f61-3bf1-4a9b-9c4f-27b43e5ed71e     2      1 00:00:00.000      0
    ## 3 7fe92118-5965-4033-9b59-29b3947a3d8a     3      1 00:00:00.000      0
    ## 4 c007670e-f679-4f80-b901-b704130fee05     4      1 00:00:00.000      0
    ## 5 7925a1d3-fc1c-458a-9300-37fa25a2b137     5      1 00:00:00.100      0
    ## 6 dddef0cb-75ef-4e0e-8f45-e8b7dd9e2c7d     6      1 00:00:00.100      0

xG Tables - Player Level with Top 10 Tables
-------------------------------------------

Let's say we want to make a Top 10 overview of Expected Goals (xG) from all of the events we have in our database. Let's use the power of dplyr and piping (%&gt;%) to quickly create as display these in two line of code. Let's first load the packages.

``` r
require(dplyr) # load the dpylr package
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

``` r
require(formattable) # load the formattable package - make sure its installed if not
```

    ## Loading required package: formattable

Piping is a very powerful concept, and allows us to pass a temporary dataframe from left to right, whilst completing the calcuation defined left of the pipe. In the example below, we pass our dataframe 'events' into the pipe, and then group the data by player.name variable, pass it through the pipe, then sum all of the shot.statsbomb\_xg events per player, pass it through the pipe, arrange the data frame in descending order with the xG variable, pass it through the pipe and finally take just the top 10.

We then display the outcome using the formattable package as it has nice and minimal formatting.

``` r
Overview <- events %>% ## take the events dataframe
  group_by(player.name) %>% ## group by player.name
  summarise(xG = sum(shot.statsbomb_xg, na.rm=TRUE)) %>% ## create a xG total by summing shot.statsbomb_xg 
  arrange(desc(xG)) %>% ## sort the results by xG in desending order
  top_n(10) ## select the top 10 
```

    ## Selecting by xG

``` r
formattable(Overview) ## display using formattable package
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
player.name
</th>
<th style="text-align:right;">
xG
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
Jessica McDonald
</td>
<td style="text-align:right;">
3.1030445
</td>
</tr>
<tr>
<td style="text-align:right;">
Crystal Alyssia Dunn
</td>
<td style="text-align:right;">
1.8279918
</td>
</tr>
<tr>
<td style="text-align:right;">
Jodie Taylor
</td>
<td style="text-align:right;">
1.4601885
</td>
</tr>
<tr>
<td style="text-align:right;">
Lynn Williams
</td>
<td style="text-align:right;">
1.3110152
</td>
</tr>
<tr>
<td style="text-align:right;">
Ana-Maria Crnogorcevic
</td>
<td style="text-align:right;">
1.1316334
</td>
</tr>
<tr>
<td style="text-align:right;">
Samantha Mewis
</td>
<td style="text-align:right;">
0.9054807
</td>
</tr>
<tr>
<td style="text-align:right;">
Débora Cristiane de Oliveira
</td>
<td style="text-align:right;">
0.8887439
</td>
</tr>
<tr>
<td style="text-align:right;">
Ashley Hatch
</td>
<td style="text-align:right;">
0.8830589
</td>
</tr>
<tr>
<td style="text-align:right;">
Francesca Kirby
</td>
<td style="text-align:right;">
0.7904245
</td>
</tr>
<tr>
<td style="text-align:right;">
Alanna Kennedy
</td>
<td style="text-align:right;">
0.7675679
</td>
</tr>
</tbody>
</table>
There are a few things that I want to tidy up, first is that the xG totals could be rounded to 2 decimal places and the column headings could be changed to 'Player' and xG to 'xG Total'.

``` r
require(formattable) # load the formattable package - make sure its installed if not

Overview <- events %>% ## take the events dataframe
  group_by(player.name) %>% ## group by player.name
  summarise(xG = round(sum(shot.statsbomb_xg, na.rm=TRUE),2)) %>% ## create a xG total by summing shot.statsbomb_xg 
  arrange(desc(xG)) %>% ## sort the results by xG in desending order
  top_n(10) ## select the top 10 
```

    ## Selecting by xG

``` r
colnames(Overview) <- c("Player", "xG Total")

formattable(Overview) ## display using formattable package
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
Player
</th>
<th style="text-align:right;">
xG Total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
Jessica McDonald
</td>
<td style="text-align:right;">
3.10
</td>
</tr>
<tr>
<td style="text-align:right;">
Crystal Alyssia Dunn
</td>
<td style="text-align:right;">
1.83
</td>
</tr>
<tr>
<td style="text-align:right;">
Jodie Taylor
</td>
<td style="text-align:right;">
1.46
</td>
</tr>
<tr>
<td style="text-align:right;">
Lynn Williams
</td>
<td style="text-align:right;">
1.31
</td>
</tr>
<tr>
<td style="text-align:right;">
Ana-Maria Crnogorcevic
</td>
<td style="text-align:right;">
1.13
</td>
</tr>
<tr>
<td style="text-align:right;">
Samantha Mewis
</td>
<td style="text-align:right;">
0.91
</td>
</tr>
<tr>
<td style="text-align:right;">
Débora Cristiane de Oliveira
</td>
<td style="text-align:right;">
0.89
</td>
</tr>
<tr>
<td style="text-align:right;">
Ashley Hatch
</td>
<td style="text-align:right;">
0.88
</td>
</tr>
<tr>
<td style="text-align:right;">
Francesca Kirby
</td>
<td style="text-align:right;">
0.79
</td>
</tr>
<tr>
<td style="text-align:right;">
Alanna Kennedy
</td>
<td style="text-align:right;">
0.77
</td>
</tr>
</tbody>
</table>
xG Tables - Team Level Comparisons
----------------------------------

We can use dplyr to easily create summaries of team level xG totals by adjusting which

``` r
TeamOverview <- events %>% 
  group_by(team.name) %>% 
  summarise(xG = round(sum(shot.statsbomb_xg, na.rm=TRUE),2)) %>%
  arrange(desc(xG)) 
  
colnames(TeamOverview) <- c("Team", "xG Total")

formattable(TeamOverview)
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
Team
</th>
<th style="text-align:right;">
xG Total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
North Carolina Courage
</td>
<td style="text-align:right;">
9.92
</td>
</tr>
<tr>
<td style="text-align:right;">
Portland Thorns FC
</td>
<td style="text-align:right;">
4.34
</td>
</tr>
<tr>
<td style="text-align:right;">
Orlando Pride SC
</td>
<td style="text-align:right;">
3.32
</td>
</tr>
<tr>
<td style="text-align:right;">
Seattle Reign FC
</td>
<td style="text-align:right;">
3.08
</td>
</tr>
<tr>
<td style="text-align:right;">
Chelsea LFC
</td>
<td style="text-align:right;">
2.58
</td>
</tr>
<tr>
<td style="text-align:right;">
Washington Spirit
</td>
<td style="text-align:right;">
1.72
</td>
</tr>
<tr>
<td style="text-align:right;">
Utah Royals FC
</td>
<td style="text-align:right;">
1.60
</td>
</tr>
<tr>
<td style="text-align:right;">
Chicago Red Stars
</td>
<td style="text-align:right;">
1.16
</td>
</tr>
<tr>
<td style="text-align:right;">
Houston Dash
</td>
<td style="text-align:right;">
0.90
</td>
</tr>
<tr>
<td style="text-align:right;">
Manchester City WFC
</td>
<td style="text-align:right;">
0.90
</td>
</tr>
<tr>
<td style="text-align:right;">
Sky Blue FC
</td>
<td style="text-align:right;">
0.68
</td>
</tr>
</tbody>
</table>
