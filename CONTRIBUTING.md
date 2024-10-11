# Contributing to dsTidyverse
We encourage everyone to contribute to dsTidyverse. You can do so by opening your own pull request (PR) on the repositories and we will review them. 

Below we have a couple pointers for developers new to DataSHIELD packages in general and dsTidyverse specifically.

## Client and serverside package
DsTidyverse consists of two parts: [the server side](https://github.com/molgenis/ds-tidyverse) and the [the clientside](https://github.com/molgenis/ds-tidyverse-client). Code that has to be run on the server where the data is stored goes in the server side package, and the functions called by the user belong in the client side package. 

## Unit tests
We have 100% test coverage and we aim to keep it that way. Therefore, if you are adding any functionality please write unit tests for them. Codecov will automatically check the coverage whenever you open your PR. It will also run all tests to ensure everything is still working the way it's supposed to. 

## Testing in Armadillo
The tests run using DSLite. To ensure functionality also works with armadillo, use the information in the [PR template](pull_request_template). Assign all data required for the test and then run the tests on the armadillo server. 

## Branch name
Please be aware that when adding server/clientside functionality, that the branchnames should match. Otherwise the automatic tests will fail because the serverside package won't get installed properly. 
