megazeux-lang
=============

A high level language to make dynamic code easier to write

* Local variables (block scoped)
* Recursive calling stack
* Expressions are no longer a nightmare

Primary changes from standard robotic
-------------------------------------
* Math functions such as `SET` and `DEC` are replaced with assignment operations such as `=` and `-=`
* If statements are block scoped, not labled jumps
* `local30` to `local32` are reserved (global robot id, integer stack cursor, and string stack cursor respectively)
* `END` has been renamed to `STOP`
