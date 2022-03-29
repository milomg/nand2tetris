const std = @import("std");

const TokenType = enum {
    Keyword,
    Symbol,
    IntConstant,
    StringConstant,
    Identifier,
    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            TokenType.Keyword => "keyword",
            TokenType.Symbol => "symbol",
            TokenType.IntConstant => "integerConstant",
            TokenType.StringConstant => "stringConstant",
            TokenType.Identifier => "identifier",
        };
    }
};

const Token = struct {
    type: TokenType,
    value: []const u8,
};

const keywords = [_][]const u8{ "class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return" };

const symbols = [_]u8{ '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~' };

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

fn isInt(str: []u8) bool {
    for (str) |c| {
        if (c < '0' or c > '9') {
            return false;
        }
    }
    return true;
}

fn isIdentifier(str: []u8) bool {
    for (str) |c, i| {
        if (i == 0 and c >= '0' and c <= '9') {
            return false;
        }
        if (!nonFirstCharOfIdentifier(c)) {
            return false;
        }
    }
    return true;
}

fn nonFirstCharOfIdentifier(r: u8) bool {
    if ((r < 'a' or r > 'z') and
        (r < 'A' or r > 'Z') and
        (r < '0' or r > '9') and
        r != '_')
    {
        return false;
    }
    return true;
}

fn analyzeFile(file: std.fs.File) !void {
    var contents = try file.readToEndAlloc(gpa.allocator(), std.math.maxInt(usize));

    var tokens = std.ArrayList(Token).init(gpa.allocator());

    var i: usize = 0;
    var tokenStart: usize = 0;
    outer: while (i < contents.len) : (i += 1) {
        var c = contents[i];
        var nextChar = if (i + 1 < contents.len) contents[i + 1] else 0;
        var myToken = contents[tokenStart..(i + 1)];

        // Fast loop to consume block comments
        if (i + 1 < contents.len and c == '/' and contents[i + 1] == '*') {
            i += 2;
            while (i + 1 < contents.len and !(contents[i] == '*' and contents[i + 1] == '/')) {
                i += 1;
            }
            i += 1;
            tokenStart = i + 1;
            continue :outer;
        }

        // strip comments beginning with "//" until end of line
        if (i + 1 < contents.len and c == '/' and contents[i + 1] == '/') {
            i += 2;
            while (i < contents.len and contents[i] != '\n') {
                i += 1;
            }
            tokenStart = i + 1;
            continue :outer;
        }

        // Fast loop to find StringConstant tokens (and avoid the whitespace tokenizer below)
        if (c == '"') {
            i += 1;
            tokenStart = i;
            while (i < contents.len and contents[i] != '"') {
                i += 1;
            }

            try tokens.append(Token{ .type = TokenType.StringConstant, .value = contents[tokenStart..i] });
            tokenStart = i + 1;

            continue :outer;
        }

        // Ignore whitespace when matching tokens
        if ((c == ' ') or (c == '\t') or (c == '\n')) {
            tokenStart = i + 1;
            continue :outer;
        }

        // Zig will actually unroll the loop because keywords is constant so this just becomes if checks
        for (keywords) |keyword| {
            if (std.mem.eql(u8, keyword, myToken) and !nonFirstCharOfIdentifier(nextChar)) {
                try tokens.append(Token{ .type = TokenType.Keyword, .value = myToken });
                tokenStart = i + 1;
                continue :outer;
            }
        }
        for (symbols) |symbol| {
            if (c == symbol) { // we can do direct equality because we are only comparing u8s
                try tokens.append(Token{ .type = TokenType.Symbol, .value = myToken });
                tokenStart = i + 1;
                continue :outer;
            }
        }
        if (isIdentifier(myToken) and !nonFirstCharOfIdentifier(nextChar)) {
            try tokens.append(Token{ .type = TokenType.Identifier, .value = myToken });
            tokenStart = i + 1;
            continue :outer;
        }
        if (isInt(myToken) and (nextChar < '0' or nextChar > '9')) {
            try tokens.append(Token{ .type = TokenType.IntConstant, .value = myToken });
            tokenStart = i + 1;
            continue :outer;
        }
    }

    std.debug.print("<tokens>\n", .{});
    for (tokens.items) |token| {
        std.debug.print("<{0s}> {1s} </{0s}>\n", .{ token.type.toString(), token.value });
    }
    std.debug.print("</tokens>\n", .{});
}

pub fn main() anyerror!void {
    var file = try std.fs.cwd().openFile("../Square/Main.jack", .{});
    defer file.close();

    try analyzeFile(file);
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
