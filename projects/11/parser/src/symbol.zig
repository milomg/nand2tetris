const std = @import("std");

pub const Kinds = enum {
    static,
    field,
    argument,
    local,
};

pub const Symbol = struct {
    type: []const u8,
    name: []const u8,
};

const SymbolList = std.ArrayList(Symbol);

const kindsLen = @typeInfo(Kinds).Enum.fields.len;
// I decided to do an ArrayList because that is much faster for small numbers of variables.
pub const Scope = struct {
    parent: ?*Scope,
    symbol_list: [kindsLen]SymbolList,
    pub fn init(allocator: std.mem.Allocator) Scope {
        var list: [kindsLen]SymbolList = undefined;
        comptime var i: usize = 0;
        inline while (i < kindsLen) : (i += 1) {
            list[i] = SymbolList.init(allocator);
        }
        return Scope {
            .parent = null,
            .symbol_list = list,
        };
    }
    pub fn deinit(self: *Scope) void {
        comptime var i: usize = 0;
        inline while (i < kindsLen) : (i += 1) {
            self.symbol_list[i].deinit();
        }
    }
};

