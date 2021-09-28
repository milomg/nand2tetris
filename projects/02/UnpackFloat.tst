load UnpackFloat.hdl,
output-file UnpackFloat.out,
compare-to UnpackFloat.cmp,
output-list in%B1.16.1 sign%B3.1.3 exp%B1.5.1 fraction%B1.11.1;

set in 1024,
eval,
output;

set in %B1111101111111111,
eval,
output;

set in 0,
eval,
output;
