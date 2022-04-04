const std = @import("std");

const tokenizer = @import("./tokenizer.zig");
const parser = @import("./parser.zig");
const Tokenizer = tokenizer.Tokenizer;
const Token = tokenizer.Token;
const Parser = parser.Parser;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = gpa.allocator();

fn run_file(folder: std.fs.Dir, fileName: []const u8) !void {
    const file = try folder.openFile(fileName, .{});
    defer file.close();
    var contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(contents);

    // Create a file with "MM.xml" appended to the name (and .jack stripped).
    var outFile = try allocator.alloc(u8, fileName.len + 1);
    defer allocator.free(outFile);
    std.mem.copy(u8, outFile, fileName);
    std.mem.copy(u8, outFile[(fileName.len - 5) .. fileName.len + 1], "MM.xml");
    var output = try folder.createFile(outFile, .{});

    var tokens = Tokenizer{ .contents = contents, .index = 0 };
    var firstToken = tokens.next() orelse return;
    var myParser = Parser{ .tokens = tokens, .currentToken = firstToken, .indentation = 0, .writer = output.writer() };
    myParser.parseClass();
}

pub fn main() anyerror!void {
    // Check that we've deallocated evertyhing.
    defer std.debug.assert(!gpa.deinit());

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const fileOrFolder = args[1];
    const isFile = std.mem.endsWith(u8, fileOrFolder, ".jack");

    if (isFile) {
        try run_file(std.fs.cwd(), fileOrFolder);
    } else {
        var folder = try std.fs.cwd().openDir(fileOrFolder, .{});
        defer folder.close();
        var iterator = folder.iterate();
        while (try iterator.next()) |entry| {
            if (std.mem.endsWith(u8, entry.name, ".jack")) {
                try run_file(folder, entry.name);
            }
        }
    }
}
