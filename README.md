# st558-project-4

This shiny app takes data from the 2022 NCAA D-1A (FBS) College Football Season and plots the data as well as models a team's bowl eligibility using the data.

The following R packages are required:

* `shiny`
* `tidyverse`
* `httr`
* `jsonlite`
* `randomForest`
* `caret`
* `ggplot2`
* `mathjaxr`

The following code can be run to install the packages:

```{r, eval = FALSE}
install.packages("shiny")
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("mathjaxr")
```

The following code can be executed to run this package:

```{r, eval = FALSE}
shiny::runGitHub("st558-project-4", "maxwellsu")
```

Because the data may be unintuitive to generate for testing, I have included some to aid in testing purposes, as well as a general range for each value.
Stats up to date on 12/5/2023 (Prior to bowl weeks)

### 2023 NC State (9-3)

Conference: Atlantic Coast

Avg. Net Passing Yards: 196.25

Avg. Rushing Yards: 150.083

Avg. First Downs: 18.667

Third Down Rate: 0.393

Fourth Down Rate: 0.615

Turnover Differential: 10

Ave. Time of Possession: 1963.917

Avg. Sacks: 2.75

Avg. Int. Yards: 14.833

Avg. Punt Ret. Yards: 14.25

### 2023 Vanderbilt (2-10)

Conference: Southeastern

Avg. Net Passing Yards: 223.333

Avg. Rushing Yards: 95.25

Avg. First Downs: 17

Third Down Rate: 0.325

Fourth Down Rate: 0.296

Turnover Differential: -4

Avg. Time of Possession: 1726.917

Avg. Sacks: 1.75

Avg. Int. Yards: 11 

Avg. Punt Ret. Yards: 13.583

### 2023 Washington (13-0)

Conference: Pac-12

Avg. Net Passing Yards: 343.846 

Avg. Rushing Yards: 125.231

Avg. First Downs: 23.308

Third Down Rate: 0.477

Fourth Down Rate: 0.733

Turnover Differential: 1

Avg. Time of Possession: 1778.846 

Avg. Sacks: 1.462

Avg. Int. Yards: 24.692

Avg. Punt Ret. Yards: 14.154

### 2023 Minnesota (5-7)

Conference: Big Ten

Avg. Net Passing Yards: 153.167

Avg. Rushing Yards: 149.417

Avg. First Downs: 18.167

Third Down Rate: 0.348

Fourth Down Rate: 0.8125

Turnover Differential: 3

Avg. Time of Possession: 1905.083 

Avg. Sacks: 1.75

Avg. Int. Yards: 9.5 

Avg. Punt Ret. Yards: 6.333 

### 2023 UCF (6-6)

Conference: Big 12

Avg. Net Passing Yards: 259.083

Avg. Rushing Yards: 233.167

Avg. First Downs: 23.417

Third Down Rate: 0.490

Fourth Down Rate: 0.6

Turnover Differential: -1 

Avg. Time of Possession: 1744.417

Avg. Sacks: 1.833

Avg. Int. Yards: 11.25

Avg. Punt Ret. Yards: 12.833

### General Ranges for values (Based on 2022 data):

Avg. Net Passing Yards: 100-400 (Possibly below range for triple option teams)

Avg. Rushing Yards: 50-300 (Possibly above range for triple option teams)

Avg. First Downs: 10-30

Third Down Rate: 0-1 (Usually between 0.2 and 0.6)

Fourth Down Rate: 0-1 (Usually between 0.2 and 0.9)

Turnover Differential: -25-25

Avg. Time of Possession: 1400-2100

Avg. Sacks: 0.5-4

Avg. Int. Yards: 0-30

Avg. Punt Ret. Yards: 0-40