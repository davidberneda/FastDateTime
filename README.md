# FastDateTime
Experimental optimized Delphi TDateTime functions

Aproximate speed improvements, release-mode optimized compilation:

32-bit x86 CPU, Windows:

| Function | RAD 10.1 Berlin | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |45% faster|35% faster|
|MonthOf|31% faster|0% *|
|DayOf  |30% faster|0% *|
|DayOfTheYear|55% faster|46% faster|

* Lazarus RTL DateUtils functions are very fast, different algorithm

64-bit CPU, Windows:

| Function | RAD 10.1 Berlin | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |69% faster|(slower, FPC 62% faster)|
|MonthOf|45% faster|(slower, FPC 200% faster)|
|DayOf  |46% faster|(slower, FPC 200% faster)|
|DayOfTheYear  |68% faster|(similar speed)|

* Lazarus 64bit compiler gives faster execution 





  
