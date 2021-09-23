load Mul2.hdl,
output-file Mul2.out,
compare-to Mul2.cmp,
output-list a%B1.2.1 b%B1.2.1 out%B1.4.1;

set a %B01,
set b %B01,
eval,
output;

set a %B10,
set b %B01,
eval,
output;

set a %B10,
set b %B10,
eval,
output;

set a %B11,
set b %B10,
eval,
output;
