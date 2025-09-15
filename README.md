# FastDateTime
Experimental optimized Delphi TDateTime functions

Aproximate speed improvements, release-mode optimized compilation:

32-bit x86 CPU (13th gen Intel), Windows 11:

| Function | RAD 13.0 Florence | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |41% faster|35% faster|
|MonthOf|28% faster|0% *|
|DayOf  |28% faster|0% *|
|DayOfTheYear|52% faster|46% faster|

* Lazarus RTL DateUtils functions are very fast, different algorithm

#  

64-bit CPU, Windows:

| Function | RAD 13.0 Florence | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |82% faster|(slower, FPC 62% faster)|
|MonthOf|69% faster|(slower, FPC 200% faster)|
|DayOf  |69% faster|(slower, FPC 200% faster)|
|DayOfTheYear  |80% faster|(similar speed)|

* Lazarus 64bit compiler gives faster execution 





  
