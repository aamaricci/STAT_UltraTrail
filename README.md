# Stat UltraTrail	
*beta version*  

This is a simple software to perform statistical analysis of UltraTrail events. Data are retrieved from 
[statistik.d-u-v.org](https://statistik.d-u-v.org). 
 

### Dependencies
The code is based on:  

- [X] Python3, pandas, lxml
- [X] [SciFortran](https://github.com/aamaricci/SciFortran)  


  
### Installation
Clone the repo  
Open and edit the Python script in `data`.  
Open and setup the Makefile with your favorite editor Compile  

```
git clone https://github.com/aamaricci/Stat_UltraTrail
```
```
cd Stat_UltraTrail
emacs data/get_data.py
emacs src/Makefile
```
```
cd src
make
```


### Info
For any information or suggestion contact the authors:   
adriano DOT amaricci AT gmail DOT com  

create an issue in this repo.


### DEVELOPMENT

#### Milestone 1
- [X] Retrieve data from [statistik.d-u-v.org](https://statistik.d-u-v.org)
- [X] Code a data structure to collect all the available data sets
- [X] Test working data analysis code. 
- [ ] Start performing preliminary analysis: statistics of the time-of-arrivals, speed and Probability Density Function.

***LICENSE***  
Copyright 2023- (C) Adriano Amaricci

The software is provided with no license, as such it is protected by copyright.
The software is provided as it is and can be read and copied, in agreement with 
the Terms of Service of GITHUB. Use of the code is constrained to author agreement.   

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


