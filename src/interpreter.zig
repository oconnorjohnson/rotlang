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
    function: Function,

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

    pub fn deinit(self: *Value) void {
        switch (self.*) {
            .array => |*arr| arr.deinit(),
            else => {},
        }
    }

    pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
        return switch (self) {
            .array => |arr| Value{
                .array = try cloneArrayList(arr, allocator),
            },
            else => self,
        };
    }
};

fn cloneArrayList(original: std.ArrayList(Value), allocator: std.mem.Allocator) !std.ArrayList(Value) {
    var new_list = std.ArrayList(Value).init(allocator);
    try new_list.ensureTotalCapacity(original.items.len);

    for (original.items) |item| {
        try new_list.append(try item.clone(allocator));
    }

    return new_list;
}

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
        var iter = self.values.iterator();
        while (iter.next()) |entry| {
            var value = entry.value_ptr;
            value.deinit();
        }
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        // If we're redefining a variable, clean up the old value first
        if (self.values.getPtr(name)) |old_value| {
            old_value.deinit();
        }
        const cloned_value = try value.clone(self.allocator);
        try self.values.put(name, cloned_value);
    }

    pub fn get(self: *Environment, name: Token) !Value {
        if (self.values.get(name.lexeme)) |value| {
            return value.clone(self.allocator);
        }

        if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        }

        return RuntimeError.UndefinedVariable;
    }

    pub fn assign(self: *Environment, name: Token, value: Value) !void {
        if (self.values.getPtr(name.lexeme)) |old_value| {
            old_value.deinit();
            try self.values.put(name.lexeme, try value.clone(self.allocator));
            return;
        }

        if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value);
            return;
        }

        return RuntimeError.UndefinedVariable;
    }

    pub fn getAt(self: *Environment, distance: usize, name: []const u8) !Value {
        const environment = try self.ancestor(distance);
        if (environment.values.get(name)) |value| {
            return value.clone(self.allocator);
        }
        return RuntimeError.UndefinedVariable;
    }

    pub fn assignAt(self: *Environment, distance: usize, name: Token, value: Value) !void {
        const environment = try self.ancestor(distance);
        if (environment.values.getPtr(name.lexeme)) |old_value| {
            old_value.deinit();
            try environment.values.put(name.lexeme, try value.clone(self.allocator));
        } else {
            return RuntimeError.UndefinedVariable;
        }
    }

    fn ancestor(self: *Environment, distance: usize) !*Environment {
        var environment: *Environment = self;
        var i: usize = 0;
        while (i < distance) : (i += 1) {
            if (environment.enclosing) |enclosing| {
                environment = enclosing;
            } else {
                return RuntimeError.UndefinedVariable;
            }
        }
        return environment;
    }
};

pub const Interpreter = struct {
    environment: *Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const global_env = try allocator.create(Environment);
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
            .If => |if_stmt| {
                const condition = try self.evaluateExpression(if_stmt.condition);
                const is_truthy = try self.isTruthy(condition);

                if (is_truthy) {
                    try self.executeStatement(if_stmt.then_branch);
                } else if (if_stmt.else_branch) |else_branch| {
                    try self.executeStatement(else_branch);
                }
            },

            .While => |while_stmt| {
                while (true) {
                    const condition = try self.evaluateExpression(while_stmt.condition);
                    const is_truthy = try self.isTruthy(condition);
                    if (!is_truthy) break;

                    self.executeStatement(while_stmt.body) catch |err| {
                        switch (err) {
                            RuntimeError.Break => break,
                            RuntimeError.Continue => continue,
                            else => return err,
                        }
                    };
                }
            },

            .Function => |func| {
                // Create function value and store in environment
                const function = Value{ .function = .{
                    .type = func.type,
                    .name = func.name,
                    .params = func.params,
                    .body = func.body,
                    .closure = self.environment,
                } };
                try self.environment.define(func.name.lexeme, function);
            },

            .Return => |ret| {
                var value = Value{ .null = {} };
                if (ret.value) |expr| {
                    value = try self.evaluateExpression(expr);
                }
                return RuntimeError.ReturnValue;
            },

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
            .Array => |array| {
                var elements = std.ArrayList(Value).init(self.allocator);
                for (array.elements.items) |element| {
                    const value = try self.evaluateExpression(element);
                    try elements.append(try value.clone(self.allocator));
                }
                return Value{ .array = elements };
            },

            .Index => |index| {
                const array = try self.evaluateExpression(index.array);
                const idx = try self.evaluateExpression(index.index);

                if (array != .array) return RuntimeError.TypeError;
                if (idx != .number) return RuntimeError.TypeError;

                const i = @as(usize, @intFromFloat(idx.number));
                if (i >= array.array.items.len) return RuntimeError.IndexOutOfBounds;

                return try array.array.items[i].clone(self.allocator);
            },

            .FanumSlice => |slice| {
                const array = try self.evaluateExpression(slice.array);
                const start = try self.evaluateExpression(slice.start);
                const end = try self.evaluateExpression(slice.end);

                if (array != .array) return RuntimeError.TypeError;
                if (start != .number or end != .number) return RuntimeError.TypeError;

                const start_idx = @as(usize, @intFromFloat(start.number));
                const end_idx = @as(usize, @intFromFloat(end.number));

                if (start_idx > end_idx or end_idx > array.array.items.len) {
                    return RuntimeError.IndexOutOfBounds;
                }

                var new_array = std.ArrayList(Value).init(self.allocator);
                var i = start_idx;
                while (i < end_idx) : (i += 1) {
                    try new_array.append(try array.array.items[i].clone(self.allocator));
                }

                return Value{ .array = new_array };
            },

            .Call => |call| {
                const callee = try self.evaluateExpression(call.callee);
                if (callee != .function) return RuntimeError.TypeError;

                var args = std.ArrayList(Value).init(self.allocator);
                defer args.deinit();

                for (call.arguments.items) |arg| {
                    const value = try self.evaluateExpression(arg);
                    try args.append(try value.clone(self.allocator));
                }

                return try self.executeFunction(callee.function, args);
            },

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

    fn isTruthy(self: *Interpreter, value: Value) !bool {
        _ = self;
        return switch (value) {
            .boolean => |b| b,
            .null => false,
            .number => |n| n != 0,
            .string => |s| s.len > 0,
            .array => |arr| arr.items.len > 0,
            else => true,
        };
    }

    fn executeFunction(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        var environment = Environment.init(self.allocator, func.closure);
        defer environment.deinit();

        // Bind parameters to arguments
        for (func.params.items, 0..) |param, i| {
            if (i < args.items.len) {
                try environment.define(param.lexeme, args.items[i]);
            } else {
                try environment.define(param.lexeme, Value{ .null = {} });
            }
        }

        // Execute function body in new environment
        const previous = self.environment;
        self.environment = &environment;
        defer self.environment = previous;

        self.executeStatement(func.body) catch |err| {
            if (err == RuntimeError.ReturnValue) {
                // Handle return value
                return Value{ .null = {} }; // TODO: Implement proper return value handling
            }
            return err;
        };

        return Value{ .null = {} };
    }
};

pub const Function = struct {
    type: Parser.FunctionType,
    name: Token,
    params: std.ArrayList(Token),
    body: *Stmt,
    closure: *Environment,
};
