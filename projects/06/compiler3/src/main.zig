const std = @import("std");
const io = std.io;

pub fn main() anyerror!void {
    var file = try std.fs.cwd().openFile("../add/Add.asm", .{});
    defer file.close();
    var buf_reader = io.bufferedReader(file.reader()).reader();

    var buf: [1024]u8 = undefined;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    var symbols = std.StringHashMap([16]u8).init(allocator);
    _ = symbols;
    
    while (try buf_reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var l: []u8 = line;
        std.log.info("line {c}.", .{l});
        // do something with line...
    }

    std.log.info("All your codebase are belong to us.", .{});
}
