# Compiler Part 1

1. install zig: `brew install zig --HEAD`
2. Check that `zig version` prints 0.11.0-dev.1593+d24ebf1d1
3. `zig build run -- ../Square/Main.jack`


## Running the whole folder

zig build run -- ../ArrayTest/
zig build run -- ../ExpressionLessSquare/
zig build run -- ../Square/

sdiff -s -b ../ArrayTest/Main.xml ../ArrayTest/MainMine.xml
sdiff -s -b ../ExpressionLessSquare/Main.xml ../ExpressionLessSquare/MainMine.xml
sdiff -s -b ../ExpressionLessSquare/Square.xml ../ExpressionLessSquare/SquareMine.xml
sdiff -s -b ../ExpressionLessSquare/SquareGame.xml ../ExpressionLessSquare/SquareGameMine.xml
sdiff -s -b ../Square/Main.xml ../Square/MainMine.xml
sdiff -s -b ../Square/Square.xml ../Square/SquareMine.xml
sdiff -s -b ../Square/SquareGame.xml ../Square/SquareGameMine.xml
