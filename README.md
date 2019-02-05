# 628-module1-group4
A Github repository that contains all of the data analysis in module1 of stat628

# Project Title

stat628 module 1: A Simple Model of Estimating Bodyfat Percentage

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

What things you need to install the software and how to install them

```
install.packages("car")
library(car)
install.packages("repr")
library(repr)
```

## Containings

This repositary containing 4 folders:
1. Data: containing the raw and cleaned data.
2. Code: containing all the R code for this analysis, including the code for data cleaning, data analysis, shiny app.
3. Image: containing all the images using in this analysis, including the images used in analysis and shiny app.
4. Excutive summary: a jupyter notebook that provide a concise, replicable, and clear description of this analysis and findings.


## Deployment

Add additional notes about how to deploy this on a live system

### Running Rshiny

You have to install and library shiny packages first

```
install.packages(shiny)
library(shiny)
```

Then you have to set the working directory to the current folder

```
setwd("~/628-module1-group4")
```

## Built With

* [R](https://www.r-project.org/) - Free software environment for statistical computing and graphics
* [Shiny](https://shiny.rstudio.com/) - Interactive web apps
* [Jupyter](https://jupyter.org/) - Open-source web application

## Authors

* **Shuo Qiang** - (sqiang@wisc.edu)
* **Jiatong Li** - (jli872@wisc.edu)
* **Yixin Chen** - (chen777@wisc.edu)


## Acknowledgments

* Thanks for the instruction from Professor Hyunseung Kang
* Thanks for the help and instruction from Peigen Zhou
