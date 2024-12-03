const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Expr = Parser.Expr;
const Stmt = Parser.Stmt;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const RuntimeError = error{
    TypeError,
    DivisionByZero,
    UndefinedVariable,
    InvalidOperand,
    InvalidAssignment,
    ReturnValue,
    Break,
    Continue,
    Custom,
};

pub const Value = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    array: std.ArrayList(Value),
    null: void,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .number => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{}", .{b}),
            .array => |arr| {
                try writer.writeAll("[");
                for (arr.items, 0..) |item, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try item.format("", .{}, writer);
                }
                try writer.writeAll("]");
            },
            .null => try writer.writeAll("null"),
        }
    }
};
