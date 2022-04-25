const std = @import("std");
const tokenizer = @import("./tokenizer.zig");
const symbols = @import("./symbol.zig");
const Tokenizer = tokenizer.Tokenizer;
const TokenType = tokenizer.TokenType;
const Token = tokenizer.Token;
const Scope = symbols.Scope;

pub const Parser = struct {
    tokens: Tokenizer,
    currentToken: Token,
    indentation: usize,
    writer: std.fs.File.Writer,
    scope: Scope,
    allocator: std.mem.Allocator,

    pub fn init(tokens: *Tokenizer, writer: std.fs.File.Writer, allocator: std.mem.Allocator) !Parser {
        var firstToken = tokens.next().?;
        return Parser {
            .currentToken = firstToken,
            .tokens = tokens.*,
            .indentation = 0,
            .writer = writer,
            .scope = Scope.init(allocator),
            .allocator = allocator,
        };
    }

    // 'class' className '{' classVarDec* subroutineDec* '}' 
    pub fn parseClass(self: *Parser) void {
        self.writeTag("class");
        defer self.writeTagEnd("class");
        self.eat(.keyword, "class");
        self.eatType(.identifier); // className
        self.eat(.symbol, "{");
        while (self.peekTokenList(&.{ "static", "field" })) {
            self.parseClassVarDec();
        }
        while (self.peekTokenList(&.{ "constructor", "function", "method" })) {
            self.parseSubroutineDec();
        }
        self.eat(.symbol, "}");
    }

    // ('static' | 'field' ) type varName (',' varName)* ';'
    fn parseClassVarDec(self: *Parser) void {
        self.writeTag("classVarDec");
        defer self.writeTagEnd("classVarDec");
        var x: symbols.Kinds = if (self.peekTokenValue("static")) .static else .field;
        self.eatList(&.{"static", "field"});
        var symbol = symbols.Symbol {
            .type = self.currentToken.value,
            .name = "",
        };
        self.writeToken(); // type
        symbol.name = self.currentToken.value;
        self.scope.symbol_list[@enumToInt(x)].append(symbol) catch return;
        self.eatType(.identifier); // varName
        while (self.tryEatToken(.symbol, ",")) {
            self.eatType(.identifier); // varName
        }
        self.eat(.symbol, ";");
    }
    
    // ('constructor' | 'function' | 'method') ('void' | type) subroutineName '('parameterList ')' subroutineBody
    fn parseSubroutineDec(self: *Parser) void {
        self.writeTag("subroutineDec");
        defer self.writeTagEnd("subroutineDec");
        self.eatList(&.{"constructor", "function", "method"});
        self.writeToken(); // type
        self.eatType(.identifier); // subroutineName
        self.eat(.symbol, "(");
        self.parseParameterList();
        self.eat(.symbol, ")");
        self.parseSubroutineBody();
    }

    // ( (type varName) (',' type varName)*)? 
    fn parseParameterList(self: *Parser) void {
        self.writeTag("parameterList");
        defer self.writeTagEnd("parameterList");
        if (self.peekToken(.symbol, ")")) {
            return;
        }
        var symbol = symbols.Symbol {
            .type = self.currentToken.value,
            .name = "",
        };
        self.writeToken(); // type
        symbol.name = self.currentToken.value;
        self.scope.symbol_list[@enumToInt(symbols.Kinds.argument)].append(symbol) catch return;
        self.eatType(.identifier); // varName
        while (self.tryEatToken(.symbol, ",")) {
            symbol = symbols.Symbol {
                .type = self.currentToken.value,
                .name = "",
            };
            self.writeToken(); // type
            symbol.name = self.currentToken.value;
            self.scope.symbol_list[@enumToInt(symbols.Kinds.argument)].append(symbol) catch return;
            self.eatType(.identifier); // varName
        }
    }

    // '{' varDec* statements '}' 
    fn parseSubroutineBody(self: *Parser) void {
        self.writeTag("subroutineBody");
        self.pushScope();
        defer self.writeTagEnd("subroutineBody");
        defer self.popScope();
        self.eat(.symbol, "{");
        while (self.peekToken(.keyword, "var")) {
            self.parseVarDec();
        }
        self.parseStatements();
        self.eat(.symbol, "}");
    }

    // 'var' type varName (',' varName)* ';
    fn parseVarDec(self: *Parser) void {
        self.writeTag("varDec");
        defer self.writeTagEnd("varDec");
        self.eat(.keyword, "var");
        var symbol = symbols.Symbol {
            .type = self.currentToken.value,
            .name = "",
        };
        self.writeToken(); // type
        symbol.name = self.currentToken.value;
        self.scope.symbol_list[@enumToInt(symbols.Kinds.local)].append(symbol) catch return;
        self.eatType(.identifier); // varName
        while (self.tryEatToken(.symbol, ",")) {
            self.eatType(.identifier); // varName
        }
        self.eat(.symbol, ";");
    }

    // letStatement | ifStatement | whileStatement | doStatement | returnStatement
    fn parseStatements(self: *Parser) void {
        self.writeTag("statements");
        defer self.writeTagEnd("statements");
        while (self.peekTokenList(&.{ "let", "if", "while", "do", "return" })) {
            if (self.peekToken(.keyword, "let")) {
                self.parseLet();
            } else if (self.peekToken(.keyword, "if")) {
                self.parseIf();
            } else if (self.peekToken(.keyword, "while")) {
                self.parseWhile();
            } else if (self.peekToken(.keyword, "do")) {
                self.parseDo();
            } else if (self.peekToken(.keyword, "return")) {
                self.parseReturn();
            }
        }
    }

    // 'let' varName ('[' expression ']')? '=' expression ';' 
    fn parseLet(self: *Parser) void {
        self.writeTag("letStatement");
        defer self.writeTagEnd("letStatement");
        self.eat(.keyword, "let");
        self.eatType(.identifier); // varName
        if (self.tryEatToken(.symbol, "[")) {
            self.parseExpression();
            self.eat(.symbol, "]");
        }
        self.eat(.symbol, "=");
        self.parseExpression();
        self.eat(.symbol, ";");
    }

    // 'if' '(' expression ')' '{' statements '}' ( 'else' '{' statements '}' )?
    fn parseIf(self: *Parser) void {
        self.writeTag("ifStatement");
        defer self.writeTagEnd("ifStatement");
        self.eat(.keyword, "if");
        self.eat(.symbol, "(");
        self.parseExpression();
        self.eat(.symbol, ")");
        self.eat(.symbol, "{");
        self.parseStatements();
        self.eat(.symbol, "}");
        if (self.tryEatToken(.keyword, "else")) {
            self.eat(.symbol, "{");
            self.parseStatements();
            self.eat(.symbol, "}");
        }
    }

    // 'while' '(' expression ')' '{' statements '}' 
    fn parseWhile(self: *Parser) void {
        self.writeTag("whileStatement");
        defer self.writeTagEnd("whileStatement");
        self.eat(.keyword, "while");
        self.eat(.symbol, "(");
        self.parseExpression();
        self.eat(.symbol, ")");
        self.eat(.symbol, "{");
        self.parseStatements();
        self.eat(.symbol, "}");
    }

    // 'do' subroutineCall ';'
    fn parseDo(self: *Parser) void {
        self.writeTag("doStatement");
        defer self.writeTagEnd("doStatement");
        self.eat(.keyword, "do");
        self.parseSubroutineCall();
        self.eat(.symbol, ";");
    }

    // subroutineName '(' expressionList ')' | ( className | varName) '.' subroutineName '(' expressionList ')'
    fn parseSubroutineCall(self: *Parser) void {
        self.eatType(.identifier); // subroutineName
        if (self.tryEatToken(.symbol, "(")) {
            self.parseExpressionList();
            self.eat(.symbol, ")");
        } else if (self.tryEatToken(.symbol, ".")) {
            self.eatType(.identifier); // subroutineName
            self.eat(.symbol, "(");
            self.parseExpressionList();
            self.eat(.symbol, ")");
        }
    }

    // 'return' expression? ';'
    fn parseReturn(self: *Parser) void {
        self.writeTag("returnStatement");
        defer self.writeTagEnd("returnStatement");
        self.eat(.keyword, "return");
        if (!self.peekToken(.symbol, ";")) {
            self.parseExpression();
        }
        self.eat(.symbol, ";");
    }

    // term (op term)*
    fn parseExpression(self: *Parser) void {
        self.writeTag("expression");
        defer self.writeTagEnd("expression");
        self.parseTerm();
        while (self.tryEatOp()) {
            self.parseTerm();
        }
    }
    
    // integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
    fn parseTerm(self: *Parser) void {
        self.writeTag("term");
        defer self.writeTagEnd("term");
        if (self.tryEatToken(.symbol, "(")) {
            self.parseExpression();
            self.eat(.symbol, ")");
        } else if (self.tryEatToken(.symbol, "-") or self.tryEatToken(.symbol, "~")) { // unaryOp
            self.parseTerm();
        } else if (self.peekKeywordConstant() or self.peekTokenType(.stringConstant) or self.peekTokenType(.integerConstant)) {
            self.writeToken(); // keywordConstant | stringConstant | integerConstant
        } else if (self.peekTokenType(.identifier)) {
            self.eatType(.identifier); // identifier
            if (self.tryEatToken(.symbol, "[")) {
                self.parseExpression();
                self.eat(.symbol, "]");
            } else if (self.tryEatToken(.symbol, ".")) {
                self.eatType(.identifier); // identifier
                if (self.tryEatToken(.symbol, "(")) {
                    self.parseExpressionList();
                    self.eat(.symbol, ")");
                }
            } else if (self.tryEatToken(.symbol, "(")) {
                self.parseExpressionList();
                self.eat(.symbol, ")");
            }
        } else {
            std.debug.panic("Inavlid expression: unexpected token {}", .{self.currentToken});
        }
    }

    // (expression (',' expression)* )?
    fn parseExpressionList(self: *Parser) void {
        self.writeTag("expressionList");
        defer self.writeTagEnd("expressionList");
        if (self.peekToken(.symbol, ")")) {
            return;
        }
        self.parseExpression();
        while (self.tryEatToken(.symbol, ",")) {
            self.parseExpression();
        }
    }

    // 'true' | 'false' | 'null' | 'this'
    fn peekKeywordConstant(self: *Parser) bool {
        return self.peekTokenType(.keyword) and self.peekTokenList(&.{"true", "false", "null", "this"});
    }

    // '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '=' 
    fn tryEatOp(self: *Parser) bool {
        var isOp = self.peekTokenType(.symbol) and (self.peekTokenList(&.{ "+", "-", "*", "/", "&", "|", "<", ">", "=" }));
        if (isOp) {
            self.writeToken();
        }
        return isOp;
    }

    // If the token exists, return true and eat it, else return false
    fn tryEatToken(self: *Parser, tokenType: TokenType, value: []const u8) bool {
        var isValid = self.peekToken(tokenType, value);
        if (isValid) {
            self.writeToken();
        }
        return isValid;
    }

    // Check information about the current token
    fn peekToken(self: *Parser, tokenType: TokenType, value: []const u8) bool {
        return self.peekTokenType(tokenType) and self.peekTokenValue(value);
    }
    fn peekTokenType(self: *Parser, tokenType: TokenType) bool {
        return self.currentToken.type == tokenType;
    }
    fn peekTokenValue(self: *Parser, value: []const u8) bool {
        return std.mem.eql(u8, self.currentToken.value, value);
    }

    // Check if the current token has a value that is part of the tokenList
    fn peekTokenList(self: *Parser, tokenList: []const []const u8) bool {
        for (tokenList) |token| {
            if (self.peekTokenValue(token)) {
                return true;
            }
        }
        return false;
    }

    // These eat functions are just peek and then writeToken or error
    fn eatList(self: *Parser, tokenList: []const []const u8) void {
        if (!self.peekTokenList(tokenList)) {
            std.debug.panic("Expected token of value {s} but got token {s}\n", .{ tokenList, self.currentToken.value });
        }
        self.writeToken();
    }
    fn eat(self: *Parser, tokenType: TokenType, value: []const u8) void {
        if (!self.peekToken(tokenType, value)) {
            std.debug.panic("Expected token of type {s} with value {s} but got {s} with value {s}\n", .{ tokenType, value, self.currentToken.type, self.currentToken.value });
        }
        self.writeToken();
    }
    fn eatType(self: *Parser, tokenType: TokenType) void {
        if (!self.peekTokenType(tokenType)) {
            std.debug.panic("Expected token of type {s} but got {s}\n", .{ tokenType, self.currentToken.type });
        }
        self.writeToken();
    }

    // Zig has this "awesome" feature that allocations are explicit, which makes all string handling painful
    // However, writing to a file in chunks is still pretty easy, so I implement a mini tokenizer that splits on 
    // things to escape in XML. Internally, the self.writer.print will compile into this
    fn write_xml(self: *Parser) !void {
        var token = self.currentToken;
        var tokenType = token.type.toString();
        try self.writer.writeAll("<");
        try self.writer.writeAll(tokenType);
        try self.writer.writeAll("> ");

        // Tiny tokenizer to do string replacement without any allocation (by writing directly to an output file)
        var trailing: usize = 0;
        var i: usize = 0;
        while (i < token.value.len) : (i += 1) {
            switch (token.value[i]) {
                '&' => {
                    try self.writer.writeAll(token.value[trailing..i]);
                    try self.writer.writeAll("&amp;");
                    trailing = i + 1;
                },
                '<' => {
                    try self.writer.writeAll(token.value[trailing..i]);
                    try self.writer.writeAll("&lt;");
                    trailing = i + 1;
                },
                '>' => {
                    try self.writer.writeAll(token.value[trailing..i]);
                    try self.writer.writeAll("&gt;");
                    trailing = i + 1;
                },
                '"' => {
                    try self.writer.writeAll(token.value[trailing..i]);
                    try self.writer.writeAll("&quot;");
                    trailing = i + 1;
                },
                else => {}
            }
        }
        try self.writer.writeAll(token.value[trailing..]);

        try self.writer.writeAll(" </");
        try self.writer.writeAll(tokenType);
        try self.writer.writeAll(">\n");
    }
    fn writeToken(self: *Parser) void {
        self.writer.writeByteNTimes(' ', self.indentation * 2) catch return;
        self.write_xml() catch return;
        self.currentToken = self.tokens.next() orelse return;
    }
    fn writeTag(self: *Parser, tag: []const u8) void {
        self.writer.writeByteNTimes(' ', self.indentation * 2) catch return;
        self.writer.print("<{s}>\n", .{tag}) catch return;
        self.indentation += 1;
    }
    fn writeTagEnd(self: *Parser, tag: []const u8) void {
        self.indentation -= 1;
        self.writer.writeByteNTimes(' ', self.indentation * 2) catch return;
        self.writer.print("</{s}>\n", .{tag}) catch return;
    }

    // Used to test the tokenizer
    fn printTokens(self: *Parser) void {
        self.writeTag("tokens");
        defer self.writeTagEnd("tokens");
        while (!self.tokens.is_eof()) {
            self.writeToken();
        }
    }

    fn pushScope(self: *Parser) void {
        var new_scope = Scope.init(self.allocator);
        new_scope.parent = &self.scope;
        self.scope = new_scope;
    }
    fn popScope(self: *Parser) void {
        var pop = self.scope;
        self.scope = self.scope.parent.?.*;
        pop.deinit();
    }
};

// zig build test
test "Parser.tryEatOp" {
    var tokens = Tokenizer{ .contents = "", .index = 0 };
    var token = Token{ .type = .symbol, .value = "+" };
    // Neat fact, you can pass in a stdout writer instead of a file writer (also there is something called an arraylist writer)
    var x = Parser{ .tokens = tokens, .currentToken = token, .indentation = 0, .writer = std.io.getStdErr().writer() };
    try std.testing.expect(x.tryEatOp());
}
