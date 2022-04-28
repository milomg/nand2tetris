const std = @import("std");

pub const Kinds = enum {
    static,
    field,
    argument,
    local,
    pub fn toString(self: Kinds) []const u8 {
        return switch (self) {
            .static => "static",
            .field => "this",
            .argument => "argument",
            .local => "local",
        };
    }
};

pub const Symbol = struct {
    type: []const u8,
    name: []const u8,
};

const SymbolList = std.ArrayList(Symbol);

const kindsLen = @typeInfo(Kinds).Enum.fields.len; // otherwise known as 4

const SymbolReturn = struct {
    symbol: Symbol,
    index: u32,
    kind: Kinds,
};

// I decided to do an ArrayList because that is much faster for small numbers of variables.
pub const Scope = struct {
    // We don't even need to store a parent
    symbol_list: [kindsLen]SymbolList,
    pub fn init(allocator: std.mem.Allocator) Scope {
        var list: [kindsLen]SymbolList = undefined;
        comptime var i: usize = 0;
        inline while (i < kindsLen) : (i += 1) {
            list[i] = SymbolList.init(allocator);
        }
        return Scope{
            .symbol_list = list,
        };
    }
    pub fn add(self: *Scope, kind: Kinds, stype: []const u8, name: []const u8) void {
        var symbol = Symbol{
            .type = stype,
            .name = name,
        };
        self.symbol_list[@enumToInt(kind)].append(symbol) catch return;
    }
    pub fn clear(self: *Scope) void {
        self.symbol_list[@enumToInt(Kinds.local)].clearRetainingCapacity();
        self.symbol_list[@enumToInt(Kinds.argument)].clearRetainingCapacity();
    }
    pub fn deinit(self: *Scope) void {
        for (self.symbol_list) |list| {
            list.deinit();
        }
    }
    pub fn lookup(self: *Scope, name: []const u8) ?SymbolReturn {
        for (self.symbol_list) |list, i| {
            for (list.items) |symbol, j| {
                if (std.mem.eql(u8, symbol.name, name)) {
                    return SymbolReturn{ .index = @truncate(u32, j), .kind = @intToEnum(Kinds, i), .symbol = symbol };
                }
            }
        }
        return null;
    }
    pub fn classVarCount(self: Scope) u32 {
        return @truncate(u32, self.symbol_list[@enumToInt(Kinds.field)].items.len);
    }
};
