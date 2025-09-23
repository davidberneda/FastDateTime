# FastDateTime
Experimental optimized Delphi TDateTime functions.

These functions are used in [TeeBI ultrafast queries](https://github.com/Steema/TeeBI) when filtering or grouping-by datetime fields of millions of records.

Aproximate speed improvements, release-mode optimized compilation:

## Windows 11:

32-bit x86 CPU (13th gen Intel):

| Function | RAD 13.0 Florence | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |41% faster|35% faster|
|MonthOf|28% faster|0% *|
|DayOf  |28% faster|0% *|
|DayOfTheYear|52% faster|46% faster|

* Lazarus RTL DateUtils functions are very fast, different algorithm

#  

64-bit CPU:

| Function | RAD 13.0 Florence | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |82% faster|(slower, FPC 62% faster)|
|MonthOf|69% faster|(slower, FPC 200% faster)|
|DayOf  |69% faster|(slower, FPC 200% faster)|
|DayOfTheYear  |80% faster|(similar speed)|

* Lazarus 64bit compiler gives faster execution 


#

## Linux Ubuntu 24.04.3 LTS

64-bit CPU:

| Function | RAD 13.0 Florence |
|----------|-----------------|
|YearOf |55% faster|
|MonthOf|49% faster|
|DayOf  |50% faster|
|DayOfTheYear|74% faster|


  
