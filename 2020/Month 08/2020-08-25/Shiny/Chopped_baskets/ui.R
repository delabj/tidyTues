#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)


library(tidyverse)
library(ggtext)
library(patchwork)
library(janitor)
library(delabj)
library(wesanderson)
library(forcats)


#text libraries
library(tidytext)
library(textclean)
library(tokenizers)
library(markovchain)



source("markov_functions.R")

# Define UI for application that draws a histogram
shinyUI(
    bootstrapPage(
        
    )
)
