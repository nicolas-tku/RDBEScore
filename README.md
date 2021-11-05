<!--
[![Build Status](https://travis-ci.org/ices-tools-prod/icesRDBES.svg?branch=master)](https://travis-ci.org/ices-tools-prod/icesRDBES)
[![codecov](https://codecov.io/gh/ices-tools-prod/icesRDBES/branch/master/graph/badge.svg)](https://codecov.io/gh/ices-tools-prod/icesRDBES)
[![GitHub release](https://img.shields.io/github/release/ices-tools-prod/icesRDBES.svg?maxAge=2592001)]()
[![CRAN Status](http://www.r-pkg.org/badges/version/icesRDBES)](https://cran.r-project.org/package=icesRDBES)
[![CRAN Monthly](http://cranlogs.r-pkg.org/badges/icesRDBES)](https://cran.r-project.org/package=icesRDBES)
[![CRAN Total](http://cranlogs.r-pkg.org/badges/grand-total/icesRDBES)](https://cran.r-project.org/package=icesRDBES)
-->

[![License](https://img.shields.io/badge/license-GPL%20(%3E%3D%202)-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

[<img align="right" alt="ICES Logo" width="17%" height="17%" src="http://ices.dk/_layouts/15/1033/images/icesimg/iceslogo.png">](http://ices.dk)

icesRDBES
=========

icesRDBES Provides functions to work with the Regional DataBase and
Estimation System (RDBES)

icesRDBES is implemented as an [R](https://www.r-project.org) package and
available on <!-- [CRAN](https://cran.r-project.org/package=icesRDBES) --> 
[GitHub](https://github.com/ices-tools-dev/icesRDBES)

Installation
------------

<!--
icesRDBES can be installed from CRAN using the `install.packages` command:

```R
install.packages("icesRDBES")
```
-->

icesRDBES can be installed from GitHub using the `install_github`
command from the [remotes](https://remotes.r-lib.org/) package:

```R
library(remotes)
install_github("ices-tools-dev/icesRDBES")
```


Usage
-----

For a summary of the package:

```R
library(icesRDBES)
?icesRDBES
```

References
----------

Regional Database & Estimation System:
https://sboxrdbes.ices.dk/



Development
-----------

icesRDBES is developed openly on
[GitHub](https://github.com/ices-tools-dev/icesRDBES).

Feel free to open an
[issue](https://github.com/ices-tools-dev/icesRDBES/issues) there if you
encounter problems or have suggestions for future versions.

The current development version can be installed using:

```R
library(remotes)
install_github("ices-tools-prod/icesRDBES@development")
```
## Precommit-hook framework

For adhering to package styling guides it is advisable to use precommit checks while developing.
In order to run lintr and styler (etc) before committing code, follow the instructions at the following address: https://github.com/lorenzwalthert/precommit#installation

1. Check that python3 is installed and install if necessary
2. Run pip install
   ```bash
   pip3 install pre-commit --user
   ```
3. Install R precommit package
   ```r
   install.packages("precommit")
   ```
4. Run at the root of the git repository.
   ```r
   library(precommit)
   precommit::use_precommit()
   ```
5. Running git commit should run the various checks automatically. See [the config file](.pre-commit-config.yaml) for all the checks.


