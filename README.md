# FastDateTime
Experimental optimized Delphi TDateTime functions

Aproximate speed improvements, release-mode optimized compilation:

32-bit x86 CPU, Windows:

| Function | RAD 10.1 Berlin | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |45% faster|35% faster|
|MonthOf|31% faster|0% *|
|DayOf  |30% faster|0% *|

* Lazarus RTL DateUtils functions are very fast, different algorithm

64-bit CPU, Windows:

| Function | RAD 10.1 Berlin | Lazarus / FreePascal |
|----------|-----------------|----------------------|
|YearOf |69% faster|(not tested yet)|
|MonthOf|45% faster|(not tested yet)|
|DayOf  |46% faster|(not tested yet)|






  
