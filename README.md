# `iSEEu`

<!-- badges: start -->

[![Travis build status](https://travis-ci.com/iSEE/iSEEu.svg?branch=master)](https://travis-ci.com/iSEE/iSEEu)
<!-- badges: end -->

The `iSEEu` package contains material and code that extends the `iSEE` package (https://github.com/iSEE/iSEE).

We welcome contributions from the community, see below for more instructions. 
For example, during the Developer Day at the European Bioconductor 2019 conference (`#EuroBioc2019`, at the UCLouvain, in Brussels, Belgium), we proposed a hackathon-like session, and we focused on the design of "modes", i.e. preconfigured sets of panels and linked content to be used as starting setup when launching `iSEE`.

## Installation

`iSEEu` can be easily installed from Bioconductor using `BiocManager::install()`:

```
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("iSEEu")
```

Optionally, if you want to install the development version from GitHub, you can use:

```
BiocManager::install("iSEE/iSEEu", dependencies = TRUE)
# or alternatively...
remotes::install_github("iSEE/iSEEu", dependencies = TRUE)
```

Setting `dependencies = TRUE` should ensure that all packages, including the ones in the `Suggests:` field of the `DESCRIPTION`, are installed - this can be essential if you want to reproduce the code in the vignette, for example.


## Expanding the `iSEE` universe with `iSEEu`

- install `iSEE` first - the development version is recommended.

```{r eval=FALSE}
BiocManager::install("iSEE", version = "devel")
# or
remotes::install_github("iSEE/iSEE")
```

- fork the `iSEEu` repo (https://github.com/iSEE/iSEEu) and clone it locally.

```
git clone https://github.com/[your_github_username]/iSEEu.git
```

- make the desired changes in the files - start from the `R` folder, then document via `roxygen2` - and push to your fork. 

- once your contribution (function, panel, mode) is done, consider adding some information in the package.
  Some examples might be a screenshot of the mode in action (to be placed in the folder `inst/modes_img`), or a well-documented example use case (maybe an entry in the `vignettes` folder). Also add yourself as a contributor (`ctb`) to the DESCRIPTION file.

- make a pull request to the original repo - the GitHub site offers a practical framework to do so, enabling comments, code reviews, and other goodies.

- more on documenting and code guidelines:
  - if possible, please consider adding an example in the dedicated Roxygen preamble to show how to run each function
  - if possible, consider adding one or more unit tests - we use the `testthat` framework


## Some more info

#### Where do I look for constants within `iSEE`? {-}

Many of the "global" variables that are used in several places in `iSEE` are defined in the [constants.R](https://github.com/iSEE/iSEE/blob/master/R/constants.R) script in `iSEE`. 
We suggest to use these constants rather than hardcoding (e.g.) column names in the panel specification data frames, to protect against potential future changes of the precise column names. 
To access a constant, use `iSEE:::.constantName`. 

#### Is there any example I can check out to understand how things are supposed to work? {-}

There are several modes already defined in the `R/` directory. 

#### Are there any style guides I am supposed to follow? {-}

Yes. 
Mainly guided by common sense of "never changing a working system", please stick to the conventions we have been adopting for developing the existing codebase.
A few simple style options:

- keep the indentation as it is in the initial functions already available.
- if writing text (e.g. vignette), please use one sentence per line - this makes `git diff` operations easier to check.
- in code, use a degree of balance.
- for names, try to keep some consistency with what already is existing.
  We use camelCase for modes and some other functions, and prepend most unexported elements with a dot.

#### What if I need a custom panel type? {-}

In addition to the eight standard panel types, custom panels are easily accommodated within `iSEE` applications. 
For a guide, see the corresponding [vignette](https://bioconductor.org/packages/release/bioc/vignettes/iSEE/inst/doc/custom.html). 
For examples, see [this repo](https://github.com/iSEE/iSEE_custom).

#### Are there other examples on how to use `iSEE` for exploring other datasets/data types? {-}

Yes, you can have a look at the examples in https://github.com/iSEE/iSEE_instances, where we tried to put together fully worked vignettes to re-analyze publicly available datasets, e.g. also trying to replicate some key visualizations of the original publications.

#### Where can I find a comprehensive introduction to `iSEE`? {-}

The `iSEE` package contains several vignettes detailing the main functionality. 
You can also take a look at this [workshop](https://isee.github.io/iSEEWorkshop2019/index.html). 
A compiled version from the Bioc2019 conference (based on Bioconductor release 3.10) is available [here](http://biocworkshops2019.bioconductor.org.s3-website-us-east-1.amazonaws.com/page/iSEEWorkshop2019__iSEE-lab/).

## Code of Conduct

Please note that the `iSEEu` project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
