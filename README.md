# README

##  Overview

This project processes the UCI Human Activity Recognition dataset to create a tidy dataset containing the average of selected variables for each activity and each subject.

## Purpose

To demonstrate data cleaning and transformation in R by:

* Merging datasets
* Extracting relevant features
* Labeling data with descriptive names
* Creating a tidy dataset


## Processing Steps

1. Merged training and test datasets
2. Assigned descriptive variable names using `features.txt`
3. Extracted only mean and standard deviation measurements
4. Replaced activity IDs with descriptive names using `activity_labels.txt`
5. Cleaned column names
6. Grouped data by Activity and Subject
7. Calculated the average of each variable

## Output

**tidy_data.csv**

* Contains the average of each variable
* Grouped by Activity and Subject
* Includes only mean and standard deviation features

## How to Run

1. Set working directory
2. Run the R script
3. Output file will be generated as `tidy_data.csv`

## Notes

* Only mean and standard deviation measurements are included
* Activity names are descriptive
* Data follows tidy data principles
