# Data-Cleaning-Google-Apps

Original data obtained form 
https://www.kaggle.com/lava18/google-play-store-apps
While many public datasets (on Kaggle and the like) provide Apple App Store data, there are not many counterpart datasets available for Google Play Store apps anywhere on the web. On digging deeper, I found out that iTunes App Store page deploys a nicely indexed appendix-like structure to allow for simple and easy web scraping. On the other hand, Google Play Store uses sophisticated modern-day techniques (like dynamic page load) using JQuery making scraping more challenging.

The aim of this project is to pre-process user review data of Google Play applications to prepare it for analysis on the
application’s effectiveness e.g. Correlations between Price, reviews and sentiments and to rank the apps in different
categories.

The 2 data sets used, googleplaystore.csv and googleplaystore_user_reviews.csv, were imported and merged by a
common variable (Apps). For a better understanding of the data set, an analysis of the structure and variable class type
was conducted. Irrelevant variables were dropped to simplify the process. Then, data type conversions were carried out on
some variables, some variables were factored while some ordered.

