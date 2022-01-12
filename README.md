# WEPPeval

Shiny application as an R package to evaluate single WEPPcloud run.         

## Getting Started

These instructions will get you a copy of the "WEPPeval" up and running on your local machine.

## How to Install

Currently, the package is available only through GitHub. 

Install devtools by running following command in your R console: 

```{r}
install.packages("devtools")
```

Now you can install the WEPPeval package from Github using devtools as:

```{r}
devtools::install_github("devalc/weppeval")
```
# Usage

Once installed the package can be loaded to the system using:
```{r}
library(weppeval)
```

Or the Shiny application can be launched directly by calling:


```{r}
weppeval::run_WEPPeval_application()
```
