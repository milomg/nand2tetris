const std = @import("std");

const TokenType = enum {
    keyword,
    symbol,
    integerConstant,
    stringConstant,
    identifier,
    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            TokenType.keyword => "keyword",
            TokenType.symbol => "symbol",
            TokenType.integerConstant => "integerConstant",
            TokenType.stringConstant => "stringConstant",
            TokenType.identifier => "identifier",
        };
    }
};

const Token = struct {
    type: TokenType,
    value: []const u8,
};

const keywords = [_][]const u8{ "class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return" };

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const TokenArray = std.ArrayList(Token);

const State = enum {
    start,
    stringConstant,
    integerConstant,
    line_comment,
    block_comment,
    identifier,
    slash,
    star_in_comment,
};

fn analyzeFile(file: std.fs.File) !TokenArray {
    var contents = try file.readToEndAlloc(gpa.allocator(), std.math.maxInt(usize));
    var tokens = TokenArray.init(gpa.allocator());

    var i: usize = 0;
    var tokenStart: usize = 0;
    var state: State = .start;

    while (i < contents.len) : (i += 1) {
        var c = contents[i];

        switch (state) {
            .start => switch (c) {
                '/' => state = .slash,
                '0'...'9' => {
                    tokenStart = i;
                    state = .integerConstant;
                },
                '"' => {
                    tokenStart = i + 1;
                    state = .stringConstant;
                },
                '\n', ' ', '\t' => {},
                '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '&', '|', '<', '>', '=', '~' => {
                    try tokens.append(Token{ .type = TokenType.symbol, .value = contents[i..(i + 1)] });
                },
                'a'...'z', 'A'...'Z', '_' => {
                    tokenStart = i;
                    state = .identifier;
                },
                else => {},
            },
            .slash => switch (c) {
                '/' => state = .line_comment,
                '*' => state = .block_comment,
                else => {
                    try tokens.append(Token{ .type = TokenType.symbol, .value = "/" });
                    i -= 1;
                    state = .start;
                },
            },
            .line_comment => switch (c) {
                '\n' => state = .start,
                else => {},
            },
            .block_comment => switch (c) {
                '*' => state = .star_in_comment,
                else => {},
            },
            .star_in_comment => switch (c) {
                '/' => state = .start,
                else => state = .block_comment,
            },
            .integerConstant => switch (c) {
                '0'...'9' => {},
                else => {
                    var myToken = contents[tokenStart..i];
                    try tokens.append(Token{ .type = TokenType.integerConstant, .value = myToken });
                    i -= 1;
                    state = .start;
                },
            },
            .stringConstant => switch (c) {
                '"' => {
                    var myToken = contents[tokenStart..i];
                    try tokens.append(Token{ .type = TokenType.stringConstant, .value = myToken });
                    state = .start;
                },
                else => {},
            },
            .identifier => switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                else => {
                    var myToken = contents[tokenStart..i];
                    for (keywords) |keyword| {
                        if (std.mem.eql(u8, keyword, myToken)) {
                            try tokens.append(Token{ .type = TokenType.keyword, .value = myToken });
                            break;
                        }
                    } else {
                        try tokens.append(Token{ .type = TokenType.identifier, .value = myToken });
                    }
                    i -= 1;
                    state = .start;
                },
            },
        }
    }

    return tokens;
}

const Parser = struct {
    tokens: TokenArray,
    index: usize,
    indentation: usize,
    pub fn parseClass(self: *Parser) void {
        self.writeTag("class");
        defer self.writeTagEnd("class");
        self.eat(TokenType.keyword, "class");
        self.eatType(TokenType.identifier); // className
        self.eat(TokenType.symbol, "{");
        while (self.peekToken("static") or self.peekToken("field")) {
            self.parseClassVarDec();
        }
        while (self.peekToken("constructor") or self.peekToken("function") or self.peekToken("method")) {
            self.parseSubroutineDec();
        }
        self.eat(TokenType.symbol, "}");
    }

    fn parseClassVarDec(self: *Parser) void {
        self.writeTag("classVarDec");
        defer self.writeTagEnd("classVarDec");
        self.writeToken(); // ('static' | 'field')
        self.writeToken(); // type
        self.eatType(TokenType.identifier); // varName
        while (self.peekToken(",")) {
            self.eat(TokenType.symbol, ",");
            self.eatType(TokenType.identifier); // varName
        }
        self.eat(TokenType.symbol, ";");
    }
    fn parseSubroutineDec(self: *Parser) void {
        self.writeTag("subroutineDec");
        defer self.writeTagEnd("subroutineDec");
        self.eatType(TokenType.keyword); // ('constructor' | 'function' | 'method')
        self.writeToken(); // type
        self.eatType(TokenType.identifier); // subroutineName
        self.eat(TokenType.symbol, "(");
        self.parseParameterList();
        self.eat(TokenType.symbol, ")");
        self.parseSubroutineBody();
    }
    fn parseParameterList(self: *Parser) void {
        self.writeTag("parameterList");
        defer self.writeTagEnd("parameterList");
        if (self.peekToken(")")) {
            return;
        }
        self.writeToken(); // type
        self.eatType(TokenType.identifier); // varName
        while (self.peekToken(",")) {
            self.eat(TokenType.symbol, ",");
            self.writeToken(); // type
            self.eatType(TokenType.identifier); // varName
        }
    }
    fn parseSubroutineBody(self: *Parser) void {
        self.writeTag("subroutineBody");
        defer self.writeTagEnd("subroutineBody");
        self.eat(TokenType.symbol, "{");
        while (self.peekToken("var")) {
            self.parseVarDec();
        }
        self.parseStatements();
        self.eat(TokenType.symbol, "}");
    }
    fn parseVarDec(self: *Parser) void {
        self.writeTag("varDec");
        defer self.writeTagEnd("varDec");
        self.eat(TokenType.keyword, "var");
        self.writeToken(); // type
        self.eatType(TokenType.identifier); // varName
        while (self.peekToken(",")) {
            self.eat(TokenType.symbol, ",");
            self.eatType(TokenType.identifier); // varName
        }
        self.eat(TokenType.symbol, ";");
    }
    fn parseStatements(self: *Parser) void {
        self.writeTag("statements");
        defer self.writeTagEnd("statements");
        while (self.peekToken("let") or self.peekToken("if") or self.peekToken("while") or self.peekToken("do") or self.peekToken("return")) {
            if (self.peekToken("let")) {
                self.parseLet();
            } else if (self.peekToken("if")) {
                self.parseIf();
            } else if (self.peekToken("while")) {
                self.parseWhile();
            } else if (self.peekToken("do")) {
                self.parseDo();
            } else if (self.peekToken("return")) {
                self.parseReturn();
            }
        }
    }
    fn parseLet(self: *Parser) void {
        self.writeTag("letStatement");
        defer self.writeTagEnd("letStatement");
        self.eat(TokenType.keyword, "let");
        self.eatType(TokenType.identifier); // varName
        if (self.peekToken("[")) {
            self.eat(TokenType.symbol, "[");
            self.parseExpression();
            self.eat(TokenType.symbol, "]");
        }
        self.eat(TokenType.symbol, "=");
        self.parseExpression();
        self.eat(TokenType.symbol, ";");
    }
    fn parseIf(self: *Parser) void {
        self.writeTag("ifStatement");
        defer self.writeTagEnd("ifStatement");
        self.eat(TokenType.keyword, "if");
        self.eat(TokenType.symbol, "(");
        self.parseExpression();
        self.eat(TokenType.symbol, ")");
        self.eat(TokenType.symbol, "{");
        self.parseStatements();
        self.eat(TokenType.symbol, "}");
        if (self.peekToken("else")) {
            self.eat(TokenType.keyword, "else");
            self.eat(TokenType.symbol, "{");
            self.parseStatements();
            self.eat(TokenType.symbol, "}");
        }
    }
    fn parseWhile(self: *Parser) void {
        self.writeTag("whileStatement");
        defer self.writeTagEnd("whileStatement");
        self.eat(TokenType.keyword, "while");
        self.eat(TokenType.symbol, "(");
        self.parseExpression();
        self.eat(TokenType.symbol, ")");
        self.eat(TokenType.symbol, "{");
        self.parseStatements();
        self.eat(TokenType.symbol, "}");
    }
    fn parseDo(self: *Parser) void {
        self.writeTag("doStatement");
        defer self.writeTagEnd("doStatement");
        self.eat(TokenType.keyword, "do");
        self.parseSubroutineCall();
        self.eat(TokenType.symbol, ";");
    }
    fn parseSubroutineCall(self: *Parser) void {
        self.eatType(TokenType.identifier); // subroutineName
        if (self.peekToken("(")) {
            self.eat(TokenType.symbol, "(");
            self.parseExpressionList();
            self.eat(TokenType.symbol, ")");
        } else if (self.peekToken(".")) {
            self.eat(TokenType.symbol, ".");
            self.eatType(TokenType.identifier); // subroutineName
            self.eat(TokenType.symbol, "(");
            self.parseExpressionList();
            self.eat(TokenType.symbol, ")");
        }
    }
    fn parseReturn(self: *Parser) void {
        self.writeTag("returnStatement");
        defer self.writeTagEnd("returnStatement");
        self.eat(TokenType.keyword, "return");
        if (!self.peekToken(";")) {
            self.parseExpression();
        }
        self.eat(TokenType.symbol, ";");
    }
    fn parseExpression(self: *Parser) void {
        self.writeTag("expression");
        defer self.writeTagEnd("expression");
        self.parseTerm();
        while (self.peekOp()) {
            self.eatType(TokenType.symbol); // op
            self.parseTerm();
        }
    }
    fn parseTerm(self: *Parser) void {
        self.writeTag("term");
        defer self.writeTagEnd("term");
        if (self.peekToken("(")) {
            self.eat(TokenType.symbol, "(");
            self.parseExpression();
            self.eat(TokenType.symbol, ")");
        } else if (self.peekToken("-") or self.peekToken("~")) {
            self.eatType(TokenType.symbol); // unaryOp
            self.parseTerm();
        } else if (self.peekTokenType(TokenType.keyword) or self.peekTokenType(TokenType.stringConstant) or self.peekTokenType(TokenType.integerConstant)) {
            self.writeToken(); // keywordConstant | stringConstant | integerConstant
        } else if (self.peekToken("-") or self.peekToken("~")) {
            self.eatType(TokenType.symbol); // op
            self.parseTerm();
        } else if (self.peekTokenType(TokenType.identifier)) {
            self.eatType(TokenType.identifier); // identifier
            if (self.peekToken("[")) {
                self.eat(TokenType.symbol, "[");
                self.parseExpression();
                self.eat(TokenType.symbol, "]");
            } else if (self.peekToken(".")) {
                self.eat(TokenType.symbol, ".");
                self.eatType(TokenType.identifier); // identifier
                if (self.peekToken("(")) {
                    self.eat(TokenType.symbol, "(");
                    self.parseExpressionList();
                    self.eat(TokenType.symbol, ")");
                }
            } else if (self.peekToken("(")) {
                self.eat(TokenType.symbol, "(");
                self.parseExpressionList();
                self.eat(TokenType.symbol, ")");
            }
        }
    }

    fn parseExpressionList(self: *Parser) void {
        self.writeTag("expressionList");
        defer self.writeTagEnd("expressionList");
        if (self.peekToken(")")) {
            return;
        }
        self.parseExpression();
        while (self.peekToken(",")) {
            self.eat(TokenType.symbol, ",");
            self.parseExpression();
        }
    }

    fn peekOp(self: *Parser) bool {
        return self.peekTokenType(TokenType.symbol) and (self.peekToken("+") or
            self.peekToken("-") or
            self.peekToken("*") or
            self.peekToken("/") or
            self.peekToken("&") or
            self.peekToken("|") or
            self.peekToken("<") or
            self.peekToken(">") or
            self.peekToken("="));
    }

    fn peekToken(self: *Parser, value: []const u8) bool {
        return std.mem.eql(u8, self.tokens.items[self.index].value, value);
    }
    fn peekTokenType(self: *Parser, tokenType: TokenType) bool {
        return self.tokens.items[self.index].type == tokenType;
    }
    fn eat(self: *Parser, tokenType: TokenType, value: []const u8) void {
        var token = self.tokens.items[self.index];
        if (token.type != tokenType or !std.mem.eql(u8, token.value, value)) {
            std.log.err("Expected token of type {s} with value {s} but got {s} with value {s}\n", .{ tokenType, value, token.type, token.value });
            std.process.exit(1);
        }
        self.writeToken();
    }

    fn eatType(self: *Parser, tokenType: TokenType) void {
        var token = self.tokens.items[self.index];
        if (token.type != tokenType) {
            std.log.err("Expected token of type {s} but got {s}\n", .{ tokenType, token.type });
            std.process.exit(1);
        }
        self.writeToken();
    }
    fn writeToken(self: *Parser) void {
        const token = self.tokens.items[self.index];
        var i: usize = 0;
        while (i < self.indentation) : (i += 1) {
            std.debug.print("  ", .{});
        }
        std.debug.print("<{0s}> {1s} </{0s}>\n", .{ token.type.toString(), token.value });
        self.index += 1;
    }
    fn writeTag(self: *Parser, tag: []const u8) void {
        var i: usize = 0;
        while (i < self.indentation) : (i += 1) {
            std.debug.print("  ", .{});
        }
        std.debug.print("<{s}>\n", .{tag});
        self.indentation += 1;
    }
    fn writeTagEnd(self: *Parser, tag: []const u8) void {
        self.indentation -= 1;
        var i: usize = 0;
        while (i < self.indentation) : (i += 1) {
            std.debug.print("  ", .{});
        }
        std.debug.print("</{s}>\n", .{tag});
    }

    fn printTokens(self: *Parser) void {
        self.writeTag("tokens");
        defer self.writeTagEnd("tokens");
        while (self.index < self.tokens.items.len) {
            self.writeToken();
        }
    }
};

pub fn main() anyerror!void {
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const fileName = args[1];
    var file = try std.fs.cwd().openFile(fileName, .{});
    defer file.close();

    var tokens = try analyzeFile(file);

    var parser = Parser{ .tokens = tokens, .index = 0, .indentation = 0 };
    parser.parseClass();
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
