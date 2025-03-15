# Compiler Part 1

1. install zig: `brew install zig`
2. Check that `zig version` prints 0.14.0
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
