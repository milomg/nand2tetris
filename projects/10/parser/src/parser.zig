const std = @import("std");
const tokenizer = @import("./tokenizer.zig");
const Tokenizer = tokenizer.Tokenizer;
const TokenType = tokenizer.TokenType;
const Token = tokenizer.Token;

pub const Parser = struct {
    tokens: Tokenizer,
    currentToken: Token,
    indentation: usize,
    writer: std.fs.File.Writer,
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

    fn parseClassVarDec(self: *Parser) void {
        self.writeTag("classVarDec");
        defer self.writeTagEnd("classVarDec");
        self.eatType(.keyword); // ('static' | 'field')
        self.writeToken(); // type
        self.eatType(.identifier); // varName
        while (self.tryEatToken(.symbol, ",")) {
            self.eatType(.identifier); // varName
        }
        self.eat(.symbol, ";");
    }
    fn parseSubroutineDec(self: *Parser) void {
        self.writeTag("subroutineDec");
        defer self.writeTagEnd("subroutineDec");
        self.eatType(.keyword); // ('constructor' | 'function' | 'method')
        self.writeToken(); // type
        self.eatType(.identifier); // subroutineName
        self.eat(.symbol, "(");
        self.parseParameterList();
        self.eat(.symbol, ")");
        self.parseSubroutineBody();
    }
    fn parseParameterList(self: *Parser) void {
        self.writeTag("parameterList");
        defer self.writeTagEnd("parameterList");
        if (self.peekToken(.symbol, ")")) {
            return;
        }
        self.writeToken(); // type
        self.eatType(.identifier); // varName
        while (self.tryEatToken(.symbol, ",")) {
            self.writeToken(); // type
            self.eatType(.identifier); // varName
        }
    }
    fn parseSubroutineBody(self: *Parser) void {
        self.writeTag("subroutineBody");
        defer self.writeTagEnd("subroutineBody");
        self.eat(.symbol, "{");
        while (self.peekToken(.keyword, "var")) {
            self.parseVarDec();
        }
        self.parseStatements();
        self.eat(.symbol, "}");
    }
    fn parseVarDec(self: *Parser) void {
        self.writeTag("varDec");
        defer self.writeTagEnd("varDec");
        self.eat(.keyword, "var");
        self.writeToken(); // type
        self.eatType(.identifier); // varName
        while (self.tryEatToken(.symbol, ",")) {
            self.eatType(.identifier); // varName
        }
        self.eat(.symbol, ";");
    }
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
    fn parseDo(self: *Parser) void {
        self.writeTag("doStatement");
        defer self.writeTagEnd("doStatement");
        self.eat(.keyword, "do");
        self.parseSubroutineCall();
        self.eat(.symbol, ";");
    }
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
    fn parseReturn(self: *Parser) void {
        self.writeTag("returnStatement");
        defer self.writeTagEnd("returnStatement");
        self.eat(.keyword, "return");
        if (!self.peekToken(.symbol, ";")) {
            self.parseExpression();
        }
        self.eat(.symbol, ";");
    }
    fn parseExpression(self: *Parser) void {
        self.writeTag("expression");
        defer self.writeTagEnd("expression");
        self.parseTerm();
        while (self.tryEatOp()) {
            self.parseTerm();
        }
    }
    fn parseTerm(self: *Parser) void {
        self.writeTag("term");
        defer self.writeTagEnd("term");
        if (self.tryEatToken(.symbol, "(")) {
            self.parseExpression();
            self.eat(.symbol, ")");
        } else if (self.tryEatToken(.symbol, "-") or self.tryEatToken(.symbol, "~")) {
            self.parseTerm();
        } else if (self.peekTokenType(.keyword) or self.peekTokenType(.stringConstant) or self.peekTokenType(.integerConstant)) {
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

    fn tryEatOp(self: *Parser) bool {
        var isOp = self.peekTokenType(.symbol) and (self.peekTokenList(&.{ "+", "-", "*", "/", "&", "|", "<", ">", "=" }));
        if (isOp) {
            self.writeToken();
        }
        return isOp;
    }

    fn tryEatToken(self: *Parser, tokenType: TokenType, value: []const u8) bool {
        var isValid = self.peekToken(tokenType, value);
        if (isValid) {
            self.writeToken();
        }
        return isValid;
    }
    fn peekToken(self: *Parser, tokenType: TokenType, value: []const u8) bool {
        return self.peekTokenType(tokenType) and self.peekTokenValue(value);
    }
    fn peekTokenType(self: *Parser, tokenType: TokenType) bool {
        return self.currentToken.type == tokenType;
    }
    fn peekTokenValue(self: *Parser, value: []const u8) bool {
        return std.mem.eql(u8, self.currentToken.value, value);
    }
    fn peekTokenList(self: *Parser, tokenList: []const []const u8) bool {
        for (tokenList) |token| {
            if (self.peekTokenValue(token)) {
                return true;
            }
        }
        return false;
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
    fn writeToken(self: *Parser) void {
        const token = self.currentToken;
        var i: usize = 0;
        while (i < self.indentation) : (i += 1) {
            self.writer.print("  ", .{}) catch return;
        }
        self.writer.print("<{0s}> {1s} </{0s}>\n", .{ token.type.toString(), token.value }) catch return;
        self.currentToken = self.tokens.next() orelse return;
    }
    fn writeTag(self: *Parser, tag: []const u8) void {
        var i: usize = 0;
        while (i < self.indentation) : (i += 1) {
            self.writer.print("  ", .{}) catch return;
        }
        self.writer.print("<{s}>\n", .{tag}) catch return;
        self.indentation += 1;
    }
    fn writeTagEnd(self: *Parser, tag: []const u8) void {
        self.indentation -= 1;
        var i: usize = 0;
        while (i < self.indentation) : (i += 1) {
            self.writer.print("  ", .{}) catch return;
        }
        self.writer.print("</{s}>\n", .{tag}) catch return;
    }
    fn printTokens(self: *Parser) void {
        self.writeTag("tokens");
        defer self.writeTagEnd("tokens");
        while (self.tokens.index < self.tokens.contents.len) {
            self.writeToken();
        }
    }
};

test "Parser.tryEatOp" {
    var tokens = Tokenizer{ .contents = "", .index = 0 };
    var token = Token{ .type = .symbol, .value = "+" };
    var x = Parser{ .tokens = tokens, .currentToken = token, .indentation = 0, .writer = std.io.getStdErr().writer() };
    try std.testing.expect(x.tryEatOp());
}
