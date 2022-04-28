# Fortran Refactoring Activity

## Author
Remus Calugarescu

## Last Major Modification
April 28, 2022

## Purpose
An exercise in refactoring legacy Fortran code (F77), and deriving a new program in a more modern version of Fortran (F95). For more information, refer to programReport.pdf in the main directory

## Installing dependencies
First you must have gfortran installed, you can do so by running
~~~~
sudo apt install gfortran -y
~~~~

## Input file
You may then either choose to use the sample file sampleInput.txt as input, or generate your own input file by following the schema below
~~~~
Lines 1-12 (line 1 is Jan, line 2 is Feb, etc) will be formatted such that
Column 1 will be the number of days for the corresponding month
Column 2 will be the DMC daylength factors
Column 3 will be the DC daylength factors

Line 13 will be formatted such that
Column 1 is the starting FFMC
Column 2 is the starting DMC
Column 3 is the starting DC
Column 4 is the starting month
Column 5 is the number of days in the starting month

Lines 14 and on will be formatted such that
Column 1 is the day's temperature in degrees celcius
Column 2 is the day's relative humidity as a percentage
Column 3 is the day's wind speed in kilometers per hour
Column 4 is the day's rainfall in millimetre
~~~~

## Output file
Once an output file is generated, it can be read by refering to the table below
~~~~
DATE:   The date to which the data corresponds to (month, then day)
TEMP:   The temperature in celcius that was read from the input file
RH:     The relative humidity as a percentage that was read from the input file
WIND:   The wind speed in kilometres per hour that was read from the input file
RAIN:   The rainfall in millimetres that was read from the input file
FFMC:   The Fine Fuel Moisture Code that was calculated
DMC:    The Duff Moisture Code that was calculated
DC:     The Drought Code that was calculated
ISI:    The Initial Spread Index that was calculated
BUI:    The Buildup Index that was calculated
FWI:    The Fire Weather Index that was calculated
~~~~

### Running the old program
After running the following commands, you may view the output in outputOld.txt
~~~~
1. cd programOld
2. gfortran ffwi.for
3. ./a.out < sampleInput.txt > outputOld.txt
~~~~
After that, you may view the output in outputOld.txt

### Running the refactored program
While running these commands, you will be prompted for various inputs by the program, such as an input file name
~~~~
1. cd programRefactored
2. gfortran -Wall FFWIndecies.f95 ffwi.f95
3. ./a.out
~~~~
After that, you may view the output in your specified output file name