# Compiler Part 1

1. install zig: `brew install zig --HEAD`
2. Check that `zig version` prints 0.10.0-dev.1645+25d4c5df7
3. `zig build run -- ../Square/Main.jack`


## Running the whole folder

zig build run -- ../Average/
zig build run -- ../ComplexArrays/
zig build run -- ../ConvertToBin/
zig build run -- ../Pong/
zig build run -- ../Seven/
zig build run -- ../Square/

sdiff -s -b ../ArrayTest/Main.xml ../ArrayTest/MainMine.xml
sdiff -s -b ../ExpressionLessSquare/Main.xml ../ExpressionLessSquare/MainMine.xml
sdiff -s -b ../ExpressionLessSquare/Square.xml ../ExpressionLessSquare/SquareMine.xml
sdiff -s -b ../ExpressionLessSquare/SquareGame.xml ../ExpressionLessSquare/SquareGameMine.xml
sdiff -s -b ../Square/Main.xml ../Square/MainMine.xml
sdiff -s -b ../Square/Square.xml ../Square/SquareMine.xml
sdiff -s -b ../Square/SquareGame.xml ../Square/SquareGameMine.xml
