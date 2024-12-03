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

pub const Environment = struct {
    values: std.StringHashMap(Value),
    enclosing: ?*Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return .{
            .values = std.StringHashMap(Value).init(allocator),
            .enclosing = enclosing,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Environment, name: Token) !Value {
        if (self.values.get(name.lexeme)) |value| {
            return value;
        }

        if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        }

        return RuntimeError.UndefinedVariable;
    }

    pub fn assign(self: *Environment, name: Token, value: Value) !void {
        if (self.values.contains(name.lexeme)) {
            try self.values.put(name.lexeme, value);
            return;
        }

        if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value);
            return;
        }

        return RuntimeError.UndefinedVariable;
    }
};

pub const Interpreter = struct {
    environment: *Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        var global_env = try allocator.create(Environment);
        global_env.* = Environment.init(allocator, null);

        return Interpreter{
            .environment = global_env,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.allocator.destroy(self.environment);
    }

    pub fn interpret(self: *Interpreter, statements: []const Stmt) !void {
        for (statements) |stmt| {
            try self.executeStatement(&stmt);
        }
    }

    fn executeStatement(self: *Interpreter, stmt: *const Stmt) !void {
        switch (stmt.*) {
            .Expression => |expr_stmt| {
                _ = try self.evaluateExpression(expr_stmt.expr);
            },
            .Declaration => |decl| {
                var value: Value = .{ .null = {} };
                if (decl.initializer) |initializer| {
                    value = try self.evaluateExpression(initializer);
                }
                try self.environment.define(decl.name.lexeme, value);
            },
            .Block => |block| {
                var new_env = Environment.init(self.allocator, self.environment);
                defer new_env.deinit();

                const previous = self.environment;
                self.environment = &new_env;
                defer self.environment = previous;

                for (block.statements.items) |block_stmt| {
                    try self.executeStatement(&block_stmt);
                }
            },
            // ... more statement types to be implemented
            else => @panic("Unimplemented statement type"),
        }
    }

    fn evaluateExpression(self: *Interpreter, expr: *const Expr) !Value {
        switch (expr.*) {
            .Literal => |lit| {
                return switch (lit.value.type) {
                    .Number => Value{ .number = try std.fmt.parseFloat(f64, lit.value.lexeme) },
                    .String => Value{ .string = lit.value.lexeme },
                    else => Value{ .null = {} },
                };
            },
            .Binary => |bin| {
                const left = try self.evaluateExpression(bin.left);
                const right = try self.evaluateExpression(bin.right);

                return try self.evaluateBinaryOp(left, bin.operator, right);
            },
            .Unary => |un| {
                const right = try self.evaluateExpression(un.right);
                return try self.evaluateUnaryOp(un.operator, right);
            },
            .Variable => |var_expr| {
                return try self.environment.get(var_expr.name);
            },
            .Assignment => |assign| {
                const value = try self.evaluateExpression(assign.value);
                try self.environment.assign(assign.name, value);
                return value;
            },
            // ... more expression types to be implemented
            else => @panic("Unimplemented expression type"),
        }
    }

    fn evaluateBinaryOp(self: *Interpreter, left: Value, operator: Token, right: Value) !Value {
        _ = self;
        switch (operator.type) {
            .Plus => {
                if (left == .number and right == .number) {
                    return Value{ .number = left.number + right.number };
                }
                if (left == .string and right == .string) {
                    // String concatenation would go here
                    @panic("String concatenation not implemented");
                }
                return RuntimeError.TypeError;
            },
            .Minus => {
                if (left == .number and right == .number) {
                    return Value{ .number = left.number - right.number };
                }
                return RuntimeError.TypeError;
            },
            .Star => {
                if (left == .number and right == .number) {
                    return Value{ .number = left.number * right.number };
                }
                return RuntimeError.TypeError;
            },
            .Slash => {
                if (left == .number and right == .number) {
                    if (right.number == 0) return RuntimeError.DivisionByZero;
                    return Value{ .number = left.number / right.number };
                }
                return RuntimeError.TypeError;
            },
            // ... implement other operators
            else => return RuntimeError.InvalidOperand,
        }
    }

    fn evaluateUnaryOp(self: *Interpreter, operator: Token, right: Value) !Value {
        _ = self;
        switch (operator.type) {
            .Minus => {
                if (right == .number) {
                    return Value{ .number = -right.number };
                }
                return RuntimeError.TypeError;
            },
            // ... implement other unary operators
            else => return RuntimeError.InvalidOperand,
        }
    }
};
