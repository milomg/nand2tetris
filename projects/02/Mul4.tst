load Mul4.hdl,
output-file Mul4.out,
compare-to Mul4.cmp,
output-list a%B1.4.1 b%B1.4.1 out%B1.8.1;

set a %B0000,
set b %B0000,
eval,
output;

set a %B0001,
set b %B1111,
eval,
output;

set a %B0010,
set b %B1110,
eval,
output;

set a %B1001,
set b %B1011,
eval,
output;

set a %B1111,
set b %B1010,
eval,
output;

set a %B1011,
set b %B1010,
eval,
output;
