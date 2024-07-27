const std = @import("std");

const tokenizer = @import("./tokenizer.zig");
const compiler = @import("./compiler.zig");
const scope = @import("./scope.zig");
const Tokenizer = tokenizer.Tokenizer;
const Token = tokenizer.Token;
const Parser = compiler.Compiler;
const Scope = scope.Scope;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = gpa.allocator();

fn run_file(folder: std.fs.Dir, fileName: []const u8) !void {
    const file = try folder.openFile(fileName, .{});
    defer file.close();
    const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(contents);

    // Create a file with "Mine.xml" appended to the name (and .jack stripped).
    var outFile = try allocator.alloc(u8, fileName.len - 2);
    defer allocator.free(outFile);
    std.mem.copyForwards(u8, outFile, fileName[0 .. fileName.len - 2]);
    std.mem.copyForwards(u8, outFile[(fileName.len - 5) .. fileName.len - 2], ".vm");
    var output = try folder.createFile(outFile, .{});

    // Build the tokenizer, and pass that structure to the parser
    var tokens = Tokenizer{ .contents = contents, .index = 0 };
    var myParser = try Parser.init(&tokens, output.writer(), allocator);
    defer myParser.deinit();

    myParser.compileClass();
}

// The ! means that this function is allowed to error.
pub fn main() !void {
    // Check that we've deallocated evertyhing.
    defer std.debug.assert(gpa.deinit() == .ok);

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
