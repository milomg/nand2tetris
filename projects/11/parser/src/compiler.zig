const std = @import("std");
const tokenizer = @import("./tokenizer.zig");
const scope = @import("./scope.zig");
const Tokenizer = tokenizer.Tokenizer;
const TokenType = tokenizer.TokenType;
const Token = tokenizer.Token;
const Scope = scope.Scope;

pub const Compiler = struct {
    class: []const u8,
    tokens: Tokenizer,
    currentToken: Token,
    indentation: usize,
    writer: std.fs.File.Writer,
    scope: Scope,
    allocator: std.mem.Allocator,
    label_counter: u32,

    pub fn init(tokens: *Tokenizer, writer: std.fs.File.Writer, allocator: std.mem.Allocator) !Compiler {
        var firstToken = tokens.next().?;
        return Compiler{
            .class = "",
            .currentToken = firstToken,
            .tokens = tokens.*,
            .indentation = 0,
            .writer = writer,
            .scope = Scope.init(allocator),
            .allocator = allocator,
            .label_counter = 0,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.scope.deinit();
    }
    // 'class' className '{' classVarDec* subroutineDec* '}'
    pub fn compileClass(self: *Compiler) void {
        self.eat(.keyword, "class");
        self.class = self.eatType(.identifier); // className
        self.eat(.symbol, "{");
        while (self.peekTokenList(&.{ "static", "field" })) {
            self.compileClassVarDec();
        }
        while (self.peekTokenList(&.{ "constructor", "function", "method" })) {
            self.compileSubroutineDec();
        }
        if (!self.peekToken(.symbol, "}")) {
            std.debug.panic("Expected token `}}` but got {s} with value {s}\n", .{ self.currentToken.type, self.currentToken.value });
        }
    }

    // ('static' | 'field' ) type varName (',' varName)* ';'
    fn compileClassVarDec(self: *Compiler) void {
        var x: scope.Kinds = if (self.peekToken(.keyword, "static")) .static else .field;
        self.eatList(&.{ "static", "field" });
        var symbolType = self.currentToken.value;
        self.nextToken(); // type
        var symbolName = self.eatType(.identifier); // varName
        self.scope.add(x, symbolType, symbolName);
        while (self.tryEatToken(.symbol, ",")) {
            symbolName = self.eatType(.identifier); // varName
            self.scope.add(x, symbolType, symbolName);
        }
        self.eat(.symbol, ";");
    }

    // ('constructor' | 'function' | 'method') ('void' | type) subroutineName '('parameterList ')' subroutineBody
    fn compileSubroutineDec(self: *Compiler) void {
        defer self.scope.clear();

        var isMethod = self.peekToken(.keyword, "method");
        var isConstructor = self.peekToken(.keyword, "constructor");
        self.eatList(&.{ "constructor", "function", "method" });
        self.nextToken(); // type
        var fname = self.eatType(.identifier); // subroutineName
        self.eat(.symbol, "(");
        if (isMethod) {
            self.scope.add(.argument, self.class, "this");
        }
        self.compileParameterList();
        self.eat(.symbol, ")");

        self.eat(.symbol, "{");
        while (self.peekToken(.keyword, "var")) {
            self.compileVarDec();
        }

        self.writer.print("function {s}.{s} {}\n", .{ self.class, fname, self.scope.varCount() }) catch return;

        if (isMethod) {
            // At the start of a method, we set *pointer to the first parameter from the arguments (sets the variable "this" from the value passed in)
            self.writer.print("push argument 0\n", .{}) catch return;
            self.writer.print("pop pointer 0\n", .{}) catch return;
        } else if (isConstructor) {
            // At the start of a constructor, we allocate space for the fields and sets this to the address of the allocated space
            self.writer.print("push constant {}\n", .{self.scope.classVarCount()}) catch return;
            self.writer.print("call Memory.alloc 1\n", .{}) catch return;
            self.writer.print("pop pointer 0\n", .{}) catch return;
        }

        self.compileSubroutineBody();
    }

    // ( (type varName) (',' type varName)*)?
    fn compileParameterList(self: *Compiler) void {
        if (self.peekToken(.symbol, ")")) {
            return;
        }
        var symbolType = self.currentToken.value;
        self.nextToken(); // type
        var symbolName = self.eatType(.identifier); // varName
        self.scope.add(.argument, symbolType, symbolName);
        while (self.tryEatToken(.symbol, ",")) {
            symbolType = self.currentToken.value;
            self.nextToken(); // type
            symbolName = self.eatType(.identifier); // varName
            self.scope.add(.argument, symbolType, symbolName);
        }
    }

    // '{' varDec* statements '}'
    fn compileSubroutineBody(self: *Compiler) void {
        self.compileStatements();
        self.eat(.symbol, "}");
    }

    // 'var' type varName (',' varName)* ';
    fn compileVarDec(self: *Compiler) void {
        self.eat(.keyword, "var");
        var symbolType = self.currentToken.value;
        self.nextToken(); // type
        var symbolName = self.eatType(.identifier); // varName
        self.scope.add(.local, symbolType, symbolName);
        while (self.tryEatToken(.symbol, ",")) {
            symbolName = self.eatType(.identifier); // varName
            self.scope.add(.local, symbolType, symbolName);
        }
        self.eat(.symbol, ";");
    }

    // letStatement | ifStatement | whileStatement | doStatement | returnStatement
    fn compileStatements(self: *Compiler) void {
        while (self.peekTokenList(&.{ "let", "if", "while", "do", "return" })) {
            if (self.peekToken(.keyword, "let")) {
                self.compileLet();
            } else if (self.peekToken(.keyword, "if")) {
                self.compileIf();
            } else if (self.peekToken(.keyword, "while")) {
                self.compileWhile();
            } else if (self.peekToken(.keyword, "do")) {
                self.compileDo();
            } else if (self.peekToken(.keyword, "return")) {
                self.compileReturn();
            }
        }
    }

    // 'let' varName ('[' expression ']')? '=' expression ';'
    fn compileLet(self: *Compiler) void {
        self.eat(.keyword, "let");
        var symbolName = self.eatType(.identifier); // varName
        if (self.tryEatToken(.symbol, "[")) {
            var symbol = self.scope.lookup(symbolName) orelse std.debug.panic("Symbol `{s}` not found", .{symbolName});
            self.compileExpression();
            self.writer.print("push {s} {}\n", .{ symbol.kind.toString(), symbol.index }) catch return;
            self.writer.print("add\n", .{}) catch return;
            self.eat(.symbol, "]");
            self.eat(.symbol, "=");
            self.compileExpression();
            // We store the value to assign in tmp
            self.writer.print("pop temp 0\n", .{}) catch return;
            // We get the pointer to where we should assign things
            self.writer.print("pop pointer 1\n", .{}) catch return;
            // Put the return value of the expression (currently stored in tmp) back on the stack
            self.writer.print("push temp 0\n", .{}) catch return;
            // Pop the top value of the stack into the pointer
            self.writer.print("pop that 0\n", .{}) catch return;
        } else {
            self.eat(.symbol, "=");
            self.compileExpression();
            var symbol = self.scope.lookup(symbolName) orelse return;
            self.writer.print("pop {s} {}\n", .{ symbol.kind.toString(), symbol.index }) catch return;
        }
        self.eat(.symbol, ";");
    }

    // 'if' '(' expression ')' '{' statements '}' ( 'else' '{' statements '}' )?
    fn compileIf(self: *Compiler) void {
        self.eat(.keyword, "if");
        self.eat(.symbol, "(");
        self.compileExpression();
        self.eat(.symbol, ")");
        var labelIfTrue = self.getLabel();
        var labelIfFalse = self.getLabel();

        self.writer.print("if-goto L{}\n", .{labelIfTrue}) catch return;
        self.writer.print("goto L{}\n", .{labelIfFalse}) catch return;
        self.writer.print("label L{}\n", .{labelIfTrue}) catch return;
        self.eat(.symbol, "{");
        self.compileStatements();
        self.eat(.symbol, "}");

        if (self.tryEatToken(.keyword, "else")) {
            var labelIfEnd = self.getLabel();
            self.writer.print("goto L{}\n", .{labelIfEnd}) catch return;
            self.writer.print("label L{}\n", .{labelIfFalse}) catch return;
            self.eat(.symbol, "{");
            self.compileStatements();
            self.eat(.symbol, "}");
            self.writer.print("label L{}\n", .{labelIfEnd}) catch return;
        } else {
            self.writer.print("label L{}\n", .{labelIfFalse}) catch return;
        }
    }

    // 'while' '(' expression ')' '{' statements '}'
    fn compileWhile(self: *Compiler) void {
        var labelIndex = self.getLabel();
        self.writer.print("label L{}\n", .{labelIndex}) catch return;
        self.eat(.keyword, "while");
        self.eat(.symbol, "(");
        self.compileExpression();
        self.writer.print("not\n", .{}) catch return;
        var labelIndex2 = self.getLabel();
        self.writer.print("if-goto L{}\n", .{labelIndex2}) catch return;
        self.eat(.symbol, ")");
        self.eat(.symbol, "{");
        self.compileStatements();
        self.eat(.symbol, "}");
        self.writer.print("goto L{}\n", .{labelIndex}) catch return;
        self.writer.print("label L{}\n", .{labelIndex2}) catch return;
    }

    // 'do' subroutineCall ';'
    fn compileDo(self: *Compiler) void {
        self.eat(.keyword, "do");
        self.compileSubroutineCall();
        self.writer.print("pop temp 0\n", .{}) catch return;
        self.eat(.symbol, ";");
    }

    // subroutineName '(' expressionList ')' | ( className | varName) '.' subroutineName '(' expressionList ')'
    fn compileSubroutineCall(self: *Compiler) void {
        var name = self.eatType(.identifier); // subroutineName
        if (self.tryEatToken(.symbol, "(")) {
            self.writer.print("push pointer 0\n", .{}) catch return;
            var numberOfArguments = self.compileExpressionList();
            self.eat(.symbol, ")");
            self.writer.print("call {s}.{s} {}", .{ self.class, name, numberOfArguments + 1 }) catch return;
        } else if (self.tryEatToken(.symbol, ".")) {
            var subroutineName = self.eatType(.identifier); // subroutineName
            self.eat(.symbol, "(");
            var numberOfArguments: u32 = 0;
            if (self.scope.lookup(name)) |symbol| {
                self.writer.print("push {s} {}\n", .{ symbol.kind.toString(), symbol.index }) catch return;
                numberOfArguments += 1;
                name = symbol.symbol.type;
            }
            numberOfArguments += self.compileExpressionList();
            self.eat(.symbol, ")");

            self.writer.print("call {s}.{s} {}", .{ name, subroutineName, numberOfArguments }) catch return;
        }
        self.writer.print("\n", .{}) catch return;
    }

    // 'return' expression? ';'
    fn compileReturn(self: *Compiler) void {
        self.eat(.keyword, "return");
        if (!self.peekToken(.symbol, ";")) {
            self.compileExpression();
        } else {
            self.writer.print("push constant 0\n", .{}) catch return;
        }
        self.eat(.symbol, ";");
        self.writer.print("return\n", .{}) catch return;
    }

    // term (op term)*
    fn compileExpression(self: *Compiler) void {
        self.compileTerm();
        while (self.tryEatOp()) |op| {
            self.compileTerm();
            switch (op) {
                '+' => self.writer.print("add\n", .{}) catch return,
                '-' => self.writer.print("sub\n", .{}) catch return,
                '*' => self.writer.print("call Math.multiply 2\n", .{}) catch return,
                '/' => self.writer.print("call Math.divide 2\n", .{}) catch return,
                '&' => self.writer.print("and\n", .{}) catch return,
                '|' => self.writer.print("or\n", .{}) catch return,
                '<' => self.writer.print("lt\n", .{}) catch return,
                '>' => self.writer.print("gt\n", .{}) catch return,
                '=' => self.writer.print("eq\n", .{}) catch return,
                else => unreachable,
            }
        }
    }

    // integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
    fn compileTerm(self: *Compiler) void {
        if (self.tryEatToken(.symbol, "(")) {
            self.compileExpression();
            self.eat(.symbol, ")");
        } else if (self.tryEatToken(.symbol, "-")) { // unaryOp
            self.compileTerm();
            self.writer.print("neg\n", .{}) catch return;
        } else if (self.tryEatToken(.symbol, "~")) {
            self.compileTerm();
            self.writer.print("not\n", .{}) catch return;
        } else if (self.peekTokenType(.integerConstant)) {
            self.writer.print("push constant {s}\n", .{self.currentToken.value}) catch return;
            self.nextToken();
        } else if (self.peekTokenType(.stringConstant)) {
            self.writer.print("push constant {}\n", .{self.currentToken.value.len}) catch return;
            self.writer.print("call String.new 1\n", .{}) catch return;
            for (self.currentToken.value) |char| {
                self.writer.print("push constant {}\n", .{char}) catch return;
                self.writer.print("call String.appendChar 2\n", .{}) catch return;
            }
            self.nextToken();
        } else if (self.tryEatToken(.keyword, "true")) {
            self.writer.print("push constant 0\n", .{}) catch return;
            self.writer.print("not\n", .{}) catch return;
        } else if (self.tryEatToken(.keyword, "false")) {
            self.writer.print("push constant 0\n", .{}) catch return;
        } else if (self.tryEatToken(.keyword, "null")) {
            self.writer.print("push constant 0\n", .{}) catch return;
        } else if (self.tryEatToken(.keyword, "this")) {
            self.writer.print("push pointer 0\n", .{}) catch return;
        } else if (self.peekTokenType(.identifier)) {
            var name = self.eatType(.identifier);
            if (self.tryEatToken(.symbol, "[")) {
                var x = self.scope.lookup(name) orelse return;
                self.writer.print("push {s} {}\n", .{ x.kind.toString(), x.index }) catch return;
                self.compileExpression();
                self.eat(.symbol, "]");
                self.writer.print("add\n", .{}) catch return;
                self.writer.print("pop pointer 1\n", .{}) catch return;
                self.writer.print("push that 0\n", .{}) catch return;
            } else if (self.tryEatToken(.symbol, ".")) {
                var subName = self.eatType(.identifier);
                self.eat(.symbol, "(");
                var numberOfArguments = self.compileExpressionList();
                self.eat(.symbol, ")");
                // If we are calling a method on an object, we need to push the object as the first argument (the "this" argument)
                // if we push a this argument, we have on extra argument
                if (self.scope.lookup(name)) |symbol| {
                    self.writer.print("push {s} {}\n", .{ symbol.kind.toString(), symbol.index }) catch return;
                    numberOfArguments += 1;
                    name = symbol.symbol.type;
                }
                self.writer.print("call {s}.{s} {}\n", .{ name, subName, numberOfArguments }) catch return;
            } else if (self.tryEatToken(.symbol, "(")) {
                var numberOfArguments = self.compileExpressionList();
                self.eat(.symbol, ")");
                self.writer.print("push pointer 0\n", .{}) catch return;
                self.writer.print("call {s}.{s} {}\n", .{ self.class, name, numberOfArguments + 1 }) catch return;
            } else {
                var x = self.scope.lookup(name) orelse std.debug.panic("Symbol `{s}` not found", .{name});
                self.writer.print("push {s} {}\n", .{ x.kind.toString(), x.index }) catch return;
            }
        } else {
            std.debug.panic("Inavlid expression: unexpected token {}", .{self.currentToken});
        }
    }

    // (expression (',' expression)* )?
    fn compileExpressionList(self: *Compiler) u32 {
        if (self.peekToken(.symbol, ")")) {
            return 0;
        }
        self.compileExpression();
        var i: u32 = 1;
        while (self.tryEatToken(.symbol, ",")) {
            self.compileExpression();
            i += 1;
        }
        return i;
    }

    // '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
    fn tryEatOp(self: *Compiler) ?u8 {
        if (self.peekTokenType(.symbol) and self.peekTokenList(&.{ "+", "-", "*", "/", "&", "|", "<", ">", "=" })) {
            var currentValue = self.currentToken.value;
            self.nextToken();
            return currentValue[0];
        } else {
            return null;
        }
    }

    // If the token exists, return true and eat it, else return false
    fn tryEatToken(self: *Compiler, tokenType: TokenType, value: []const u8) bool {
        if (self.peekToken(tokenType, value)) {
            self.nextToken();
            return true;
        }
        return false;
    }

    // Check information about the current token
    fn peekToken(self: *Compiler, tokenType: TokenType, value: []const u8) bool {
        return self.peekTokenType(tokenType) and self.peekTokenValue(value);
    }
    fn peekTokenType(self: *Compiler, tokenType: TokenType) bool {
        return self.currentToken.type == tokenType;
    }
    fn peekTokenValue(self: *Compiler, value: []const u8) bool {
        return std.mem.eql(u8, self.currentToken.value, value);
    }

    // Check if the current token has a value that is part of the tokenList
    fn peekTokenList(self: *Compiler, tokenList: []const []const u8) bool {
        for (tokenList) |token| {
            if (self.peekTokenValue(token)) {
                return true;
            }
        }
        return false;
    }

    // These eat functions are just peek and then nextToken or error
    fn eatList(self: *Compiler, tokenList: []const []const u8) void {
        if (!self.peekTokenList(tokenList)) {
            std.debug.panic("Expected token of value {s} but got token {s}\n", .{ tokenList, self.currentToken.value });
        }
        self.nextToken();
    }
    fn eat(self: *Compiler, tokenType: TokenType, value: []const u8) void {
        if (!self.peekToken(tokenType, value)) {
            std.debug.panic("Expected token of type {s} with value {s} but got {s} with value {s}\n", .{ tokenType, value, self.currentToken.type, self.currentToken.value });
        }
        self.nextToken();
    }
    fn eatType(self: *Compiler, tokenType: TokenType) []const u8 {
        if (!self.peekTokenType(tokenType)) {
            std.debug.panic("Expected token of type {s} but got {s}\n", .{ tokenType, self.currentToken.type });
        }
        var out = self.currentToken.value;
        self.nextToken();
        return out;
    }

    fn nextToken(self: *Compiler) void {
        self.currentToken = self.tokens.next() orelse std.debug.panic("Unexpected end of file", .{});
    }

    // We have a label counter to return the next available label index
    fn getLabel(self: *Compiler) u32 {
        self.label_counter += 1;
        return self.label_counter - 1;
    }
};
