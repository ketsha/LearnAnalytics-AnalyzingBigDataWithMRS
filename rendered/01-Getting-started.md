Getting started
================
Seth Mottaghinejad
2017-01-27

Getting started
===============

**R** is a very popular programming language whose rich set of features and packages make it ideally suited for data analysis and modeling. Traditionally, R works by loading (copying) every object (including data sets) as a memory object. This means that large data sets can quickly surpass the amount of available space in the memory. This is especially a problem when multiple users are working on the same R server, where free memory can quickly turn into a scarce resource. Over time, many R packages have been introduced that attempt to overcome this limitation. Some propose a way to more efficiently load and process the data, which would in turn allow us to work with larger data sizes. This approach however can only take us so far, since efficiency eventually hits a wall, and it also sometimes assumes a more sophisticated knowledge about programming that many R users lack.

Microsoft R Server (MSR) on the other hand takes a different approach. MRS's `RevoScaleR` package stores the dataset on the disk (hard drive) and loads it only a **chunks** at a time (where each chunk is a certain number of rows) for processing. Once the chunk is processed, we then moves to the next chunk of the data. By default, the **chunk size** is set to 500K rows, but we can change it to a lower number when dealing with *wider* datasets (lots of columns), and a larger number when dealing with *longer* data sets (few columns).

Data in `RevoScaleR` is *external* (because it's stored on disk) and *distributed* (because we process it chunk-wise). This means we are no longer bound by memory when dealing with data: Our data can be as large as we have space on the hard-disk to store it. Since at every point in time we only load one chunk of the data as a memory object (an R **list** object to be specific), we never overexert the system's memory. All this chunking is of course happening behind the scene with minimal input form the user.

But there is no such thing as a free lunch, so there is also a cost to pay when working with distributed data: Since most open-source R algorithms for data processing and analysis (including most third-party packages) rely on the whole dataset to be loaded into the R session as a `data.frame` object, they no longer work *directly* with distributed data. But as we will see, - Most data-processing steps (cleaning data, creating new columns or modifying existing ones) can still *indirectly* (and relatively easily) be used by `RevoScaleR` to process the distributed data, so that we can still leverage any R code we developed. What we mean by *indirectly* will become clear as we cover a wide range of examples. - On the other hand, some data processing steps (such as merging data or sorting data) and most analytics algorithms (such as the `lm` function used to build linear models) have their `RevoScaleR` counterparts which mirror the way they work, but work on a distributed data set in addition to a `data.frame`. For example, `RevoScaleR` has an `rxLinMod` function which replicates what `lm` does, but because `rxLinMod` is a distributed algorithm it runs both on a `data.frame` (where it far outperforms `lm` if the `data.frame` in question is large), and on a distributed dataset.

Using `RevoScaleR` we can both leverage existing R functionality (such as what's offered by R's rich set of third-party packages) and use what `RevoScaleR` offers through its own set of distributed functions. One last advantage that `RevoScaleR`'s distributed functions offer is code portability: Because open-source R's analytics functions are generally not parallel, using these algorithms in an inherently distributed environment can be a challenge. For example, deploying our code to Hadoop means having to rewrite our R code as mappers and reducers that Hadoop understands, which can be a daunting task. The inherently parallel data processing and analysis functions in `RevoScaleR` on the other hand make them ideal for porting our code from MRS running on a single machine to MRS on a Hadoop cluster or other inherently distributed environments.

Summary
-------

Let's review what the `RevoScaleR` package offers: 1. When our data is large, but still small enough to fit in the memory as a `data.frame`, we can still use `RevoScaleR`'s parallel algorithms to run models on the data much faster than their open-source counterparts (such as using `rxLinMod` instead of `lm`). 2. When the data is too large to fit in available memory, we can convert the data to an external and distributed format, i.e. data that is saved on disk and processed chunk-wise. `RevoScaleR`'s data-processing and analytics functions work directly with such data in addition to a `data.frame`. 3. When the data is saved in a distributed environment such as HDFS or SQL Server, which is often the case in production, with some minor adjustments we can deploy our code in such environments, reducing the hurdle of going from development to production.

Loading Packages
----------------

At various points throughout the analysis, we will be using to a set a third-party packages. So let's begin by loading those packages. The `RevoScaleR` package is pre-installed with Microsoft R Server (MRS), since it cannot be downloaded from CRAN, but all the other packages shown below are third-party packages that can be downloaded and installed from CRAN using the `install.packages` command.

Additionally, we override some default options to make it easier to display data or results.

``` r
options(max.print = 1000, scipen = 999, width = 90)
library(RevoScaleR)
rxOptions(reportProgress = 1) # reduces the amount of output RevoScaleR produces
library(tidyverse)
library(lubridate)
library(stringr)
options(dplyr.print_max = 2000)
options(dplyr.width = Inf) # shows all columns of a tbl_df object
library(rgeos) # spatial package
library(maptools) # spatial package
library(ggmap)
library(gridExtra) # for putting plots side by side
library(ggrepel) # avoid text overlap in plots
library(seriation) # package for reordering a distance matrix
```
