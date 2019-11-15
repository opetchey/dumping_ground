# Introduction

This app is a work in progress (initially developed by Owen Petchey in early 2018). You can help with its development by [getting the code from github](https://github.com/opetchey/ttl-resources/tree/master/food_web/ADBM_shiny), submitting issues there, and working on the code. This website is not automatically updated from the github code -- Owen has to do it manually at present. You can contact Owen via github, or easily find his work email address.

The app runs the allometric diet breadth model of food web structure. The model is described in the paper, [Size, foraging, and food web structure (2008) by Petchey, Beckerman, Riede, and Warren](http://www.pnas.org/cgi/doi/10.1073/pnas.0710672105).

A new development as of 15.11.2019 is that effects on the mean of encounter rate (and therefore food web connectance) of changes in the allometric exponents relating attack rate and body mass (i.e. *Prey mass - attack rate scaling exponent* and *Predator mass attack rate scaling exponent*) are minimised by adjusting the attack rate-mass scaling constant. This likely makes a lot sense, due to the change in dimensions caused by changes in the exponents.
