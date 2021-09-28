load Mul8.hdl,
output-file Mul8.out,
compare-to Mul8.cmp,
output-list a%B1.8.1 b%B1.8.1 out%B1.16.1;

set a %B10111010,
set b %B10010010,
eval,
output;
