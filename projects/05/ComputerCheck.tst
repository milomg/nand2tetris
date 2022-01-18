load Computer.hdl,
output-file ComputerCheck.out,
compare-to ComputerCheck.cmp,
output-list time%S1.4.1 reset%B2.1.2 ARegister[]%D1.7.1 DRegister[]%D1.7.1 PC[]%D0.4.0 RAM16K[0]%D1.7.1 RAM16K[1]%D1.7.1 RAM16K[2]%D1.7.1;

ROM32K load Check.hack,

// first run: compute max(3,5)

echo "Click the Keyboard icon and hold down the 'K' key (uppercase) until you see the next message (it should appear shortly after that) ...",
// It's important to keep holding the key down since if the system is busy,
// the memory will zero itself before being outputted.

repeat 100000000 {
    tick, tock,     // tick, tock prevents hang if sync. parts used in KB path.
}


