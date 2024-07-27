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
        const firstToken = tokens.next().?;
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
            self.compileSubroutine();
        }
        if (!self.peekToken(.symbol, "}")) {
            std.debug.panic("Expected token `}}` but got {s} with value {s}\n", .{ self.currentToken.type.toString(), self.currentToken.value });
        }
    }

    // ('static' | 'field' ) type varName (',' varName)* ';'
    fn compileClassVarDec(self: *Compiler) void {
        const x: scope.Kinds = if (self.peekToken(.keyword, "static")) .static else .field;
        self.eatList(&.{ "static", "field" });
        const symbolType = self.currentToken.value;
        self.nextToken(); // type
        var symbolName = self.eatType(.identifier); // varName
        self.scope.add(x, symbolType, symbolName);
        while (self.tryEatToken(.symbol, ",")) {
            symbolName = self.eatType(.identifier); // varName
            self.scope.add(x, symbolType, symbolName);
        }
        self.eat(.symbol, ";");
    }

    // ('constructor' | 'function' | 'method') ('void' | type) subroutineName '('parameterList ')' '{' varDec* statements '}'
    fn compileSubroutine(self: *Compiler) void {
        defer self.scope.clear();

        const isMethod = self.peekToken(.keyword, "method");
        const isConstructor = self.peekToken(.keyword, "constructor");
        self.eatList(&.{ "constructor", "function", "method" });
        self.nextToken(); // type
        const fname = self.eatType(.identifier); // subroutineName
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

        self.print("function {s}.{s} {}", .{ self.class, fname, self.scope.varCount() });

        if (isMethod) {
            // At the start of a method, we set *pointer to the first parameter from the arguments (sets the variable "this" from the value passed in)
            self.print("push argument 0", .{});
            self.print("pop pointer 0", .{});
        } else if (isConstructor) {
            // At the start of a constructor, we allocate space for the fields and sets this to the address of the allocated space
            self.print("push constant {}", .{self.scope.classVarCount()});
            self.print("call Memory.alloc 1", .{});
            self.print("pop pointer 0", .{});
        }

        self.compileStatements();
        self.eat(.symbol, "}");
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

    // 'var' type varName (',' varName)* ';
    fn compileVarDec(self: *Compiler) void {
        self.eat(.keyword, "var");
        const symbolType = self.currentToken.value;
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
        const symbolName = self.eatType(.identifier); // varName
        if (self.tryEatToken(.symbol, "[")) {
            var symbol = self.scope.lookup(symbolName) orelse std.debug.panic("Symbol `{s}` not found", .{symbolName});
            self.compileExpression();
            self.print("push {s} {}", .{ symbol.kind.toString(), symbol.index });
            self.print("add", .{});
            self.eat(.symbol, "]");
            self.eat(.symbol, "=");
            self.compileExpression();
            // We store the value to assign in tmp
            self.print("pop temp 0", .{});
            // We get the pointer to where we should assign things
            self.print("pop pointer 1", .{});
            // Put the return value of the expression (currently stored in tmp) back on the stack
            self.print("push temp 0", .{});
            // Pop the top value of the stack into the pointer
            self.print("pop that 0", .{});
        } else {
            self.eat(.symbol, "=");
            self.compileExpression();
            var symbol = self.scope.lookup(symbolName) orelse return;
            self.print("pop {s} {}", .{ symbol.kind.toString(), symbol.index });
        }
        self.eat(.symbol, ";");
    }

    // 'if' '(' expression ')' '{' statements '}' ( 'else' '{' statements '}' )?
    fn compileIf(self: *Compiler) void {
        self.eat(.keyword, "if");
        self.eat(.symbol, "(");
        self.compileExpression();
        self.eat(.symbol, ")");
        const labelIfTrue = self.getLabel();
        const labelIfFalse = self.getLabel();

        self.print("if-goto L{}", .{labelIfTrue});
        self.print("goto L{}", .{labelIfFalse});
        self.print("label L{}", .{labelIfTrue});
        self.eat(.symbol, "{");
        self.compileStatements();
        self.eat(.symbol, "}");

        if (self.tryEatToken(.keyword, "else")) {
            const labelIfEnd = self.getLabel();
            self.print("goto L{}", .{labelIfEnd});
            self.print("label L{}", .{labelIfFalse});
            self.eat(.symbol, "{");
            self.compileStatements();
            self.eat(.symbol, "}");
            self.print("label L{}", .{labelIfEnd});
        } else {
            self.print("label L{}", .{labelIfFalse});
        }
    }

    // 'while' '(' expression ')' '{' statements '}'
    fn compileWhile(self: *Compiler) void {
        const labelIndex = self.getLabel();
        self.print("label L{}", .{labelIndex});
        self.eat(.keyword, "while");
        self.eat(.symbol, "(");
        self.compileExpression();
        self.print("not", .{});
        const labelIndex2 = self.getLabel();
        self.print("if-goto L{}", .{labelIndex2});
        self.eat(.symbol, ")");
        self.eat(.symbol, "{");
        self.compileStatements();
        self.eat(.symbol, "}");
        self.print("goto L{}", .{labelIndex});
        self.print("label L{}", .{labelIndex2});
    }

    // 'do' subroutineCall ';'
    fn compileDo(self: *Compiler) void {
        self.eat(.keyword, "do");
        const name = self.eatType(.identifier); // subroutineName
        self.compileSubroutineCall(name);
        self.print("pop temp 0", .{});
        self.eat(.symbol, ";");
    }

    // subroutineName '(' expressionList ')' | ( className | varName) '.' subroutineName '(' expressionList ')'
    fn compileSubroutineCall(self: *Compiler, name: []const u8) void {
        if (self.tryEatToken(.symbol, "(")) {
            self.print("push pointer 0", .{});
            const numberOfArguments = self.compileExpressionList();
            self.eat(.symbol, ")");
            self.print("call {s}.{s} {}", .{ self.class, name, numberOfArguments + 1 });
        } else if (self.tryEatToken(.symbol, ".")) {
            // If we are calling a method on an object, we need to push the object as the first argument (the "this" argument)
            // if we push a this argument, we have on extra argument
            const subroutineName = self.eatType(.identifier); // subroutineName
            self.eat(.symbol, "(");
            var className = name;
            var numberOfArguments: u32 = 0;
            if (self.scope.lookup(name)) |symbol| {
                self.print("push {s} {}", .{ symbol.kind.toString(), symbol.index });
                numberOfArguments += 1;
                className = symbol.symbol.type;
            }
            numberOfArguments += self.compileExpressionList();
            self.eat(.symbol, ")");

            self.print("call {s}.{s} {}", .{ className, subroutineName, numberOfArguments });
        }
    }

    // 'return' expression? ';'
    fn compileReturn(self: *Compiler) void {
        self.eat(.keyword, "return");
        if (!self.peekToken(.symbol, ";")) {
            self.compileExpression();
        } else {
            self.print("push constant 0", .{});
        }
        self.eat(.symbol, ";");
        self.print("return", .{});
    }

    // term (op term)*
    fn compileExpression(self: *Compiler) void {
        self.compileTerm();
        while (self.tryEatOp()) |op| {
            self.compileTerm();
            switch (op) {
                '+' => self.print("add", .{}),
                '-' => self.print("sub", .{}),
                '*' => self.print("call Math.multiply 2", .{}),
                '/' => self.print("call Math.divide 2", .{}),
                '&' => self.print("and", .{}),
                '|' => self.print("or", .{}),
                '<' => self.print("lt", .{}),
                '>' => self.print("gt", .{}),
                '=' => self.print("eq", .{}),
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
            self.print("neg", .{});
        } else if (self.tryEatToken(.symbol, "~")) {
            self.compileTerm();
            self.print("not", .{});
        } else if (self.peekTokenType(.integerConstant)) {
            self.print("push constant {s}", .{self.currentToken.value});
            self.nextToken();
        } else if (self.peekTokenType(.stringConstant)) {
            self.print("push constant {}", .{self.currentToken.value.len});
            self.print("call String.new 1", .{});
            for (self.currentToken.value) |char| {
                self.print("push constant {}", .{char});
                self.print("call String.appendChar 2", .{});
            }
            self.nextToken();
        } else if (self.tryEatToken(.keyword, "true")) {
            self.print("push constant 0", .{});
            self.print("not", .{});
        } else if (self.tryEatToken(.keyword, "false")) {
            self.print("push constant 0", .{});
        } else if (self.tryEatToken(.keyword, "null")) {
            self.print("push constant 0", .{});
        } else if (self.tryEatToken(.keyword, "this")) {
            self.print("push pointer 0", .{});
        } else if (self.peekTokenType(.identifier)) {
            const name = self.eatType(.identifier);
            if (self.tryEatToken(.symbol, "[")) {
                var x = self.scope.lookup(name) orelse return;
                self.print("push {s} {}", .{ x.kind.toString(), x.index });
                self.compileExpression();
                self.eat(.symbol, "]");
                self.print("add", .{});
                self.print("pop pointer 1", .{});
                self.print("push that 0", .{});
            } else if (self.peekToken(.symbol, ".") or self.peekToken(.symbol, "(")) {
                self.compileSubroutineCall(name);
            } else {
                var x = self.scope.lookup(name) orelse std.debug.panic("Symbol `{s}` not found", .{name});
                self.print("push {s} {}", .{ x.kind.toString(), x.index });
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
            const currentValue = self.currentToken.value;
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
            std.debug.panic("Expected token of type {s} with value {s} but got {s} with value {s}\n", .{ tokenType.toString(), value, self.currentToken.type.toString(), self.currentToken.value });
        }
        self.nextToken();
    }
    fn eatType(self: *Compiler, tokenType: TokenType) []const u8 {
        if (!self.peekTokenType(tokenType)) {
            std.debug.panic("Expected token of type {s} but got {s}\n", .{ tokenType.toString(), self.currentToken.type.toString() });
        }
        const out = self.currentToken.value;
        self.nextToken();
        return out;
    }
    // Try to move to the next token (or error if we are at the end)
    fn nextToken(self: *Compiler) void {
        self.currentToken = self.tokens.next() orelse std.debug.panic("Unexpected end of file", .{});
    }
    // Print a line to the output file (and don't return an error so we don't have to do catch return everywehere)
    fn print(self: *Compiler, comptime format: []const u8, args: anytype) void {
        self.writer.print(format ++ "\n", args) catch std.debug.panic("Failed to write to file", .{});
    }
    // We have a label counter to return the next available label index
    fn getLabel(self: *Compiler) u32 {
        self.label_counter += 1;
        return self.label_counter - 1;
    }
};
