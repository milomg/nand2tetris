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

fn analyzeFile(file: std.fs.File) !void {
    var contents = try file.readToEndAlloc(gpa.allocator(), std.math.maxInt(usize));

    var tokens = std.ArrayList(Token).init(gpa.allocator());

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
                    state = .stringConstant;
                    tokenStart = i + 1;
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
