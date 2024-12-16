# Evolution

Files, Modules and Dependencies 
-------------------------------

| name     | purpose |
| --------- | ------------------------------------ |
| xstream   | a STDIN/OUT echo program             |
| xtest     | a test harness that feeds tests from some directory |

Testing
-------

> raco test xstream 

tests the echo streaming for a range of streams, including the empty one;
if (write-out-tests #t) is on, it also creates tests files 

Running the X Programs
---------------------

at a shell prompt 
> ./xstream 
 
> ... JSON ...
 
> ^ D

run test harness as usual
> ./xtest . xstream 

Reading the Code 
----------------

xstream consists of four segments: 

1. an interface, which specifies the exported services 
2. require dependencies for the code (1)
3. an implementation (7)
4. a test suite (17)

