# Practicioner's Guide to DiD with Variation in Treatment Timing

## Replication Code
This repository hosts the replication code for paper "Practicioner's Guide to DiD with Variation in Treatment Timing".

The code simulates the data, analyses it, and then visualizes it.

## Dependencies
- AWS CLI
- R 
- GNU Make

### FOLDER STRUCTURE
```
├── README.md (this documentation)
├── install_packages.R
├── code
│   ├── simulation        
│   ├── analysis          
│   └── visualization     
```
\* Currently the data is only available in a private AWS S3 bucket (s3://uvt-streaming-phd/).

## Replication Instructions

__Each module contains a makefile in a subdirectory `\code`__, which can be run by navigating to the directory, and typing `make`.

1. Install software
- Install and configure AWS CLI, make sure you have access to s3://uvt-streaming-phd/ bucket to be able to download the data (contact authors for access)
- install Git, available at https://git-scm.com/download/win
- install GNU Make, available at http://gnuwin32.sourceforge.net/packages/make.htm
- install R, and make R available via the path settings
    
2. Checkout repository

Use obtained source code or check out from Git(Hub).

4. Install required R packages

Run `install_packages.R` in the main project directory.
Ps. 'stringdist' package is not installed through this script on Blade, so install it separately.

5. Run `make` in the main project directory to build each of the projects' main modules.


## About the Data
### Raw Data 
- There is only one external raw data
    - WoS litarature data
 
The files are in s3://uvt-streaming-phd/XXX/XXX/, in a date-stamped folder.
  
Let me know if you have any questions!