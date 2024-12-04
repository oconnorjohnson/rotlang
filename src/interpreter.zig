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
    IndexOutOfBounds,
    InvalidErrorValue,
    ErrorPropagation,
    AccessViolation,
    InvalidVisibility,
};

pub const ErrorValue = struct {
    message: []const u8,
    line: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, message: []const u8, line: usize) !ErrorValue {
        const msg_copy = try allocator.dupe(u8, message);
        return ErrorValue{
            .message = msg_copy,
            .line = line,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ErrorValue) void {
        self.allocator.free(self.message);
    }
};

pub const Qualifier = enum {
    Clean,
    Sus,
    Peak,
    None, // Default qualifier
};

pub const Visibility = enum {
    Lowkey, // Private to current scope
    Highkey, // Public/visible to outer scopes
    Default, // Default visibility (Highkey)
};

pub const ScopeType = enum {
    Global,
    Local,
    Function,
};

pub const QualifiedValue = struct {
    value: Value,
    qualifier: Qualifier,
    visibility: Visibility,

    pub fn init(value: Value, qualifier: Qualifier) QualifiedValue {
        return .{
            .value = value,
            .qualifier = qualifier,
            .visibility = .Default,
        };
    }

    pub fn initWithVisibility(value: Value, qualifier: Qualifier, visibility: Visibility) QualifiedValue {
        return .{
            .value = value,
            .qualifier = qualifier,
            .visibility = visibility,
        };
    }

    pub fn deinit(self: *QualifiedValue) void {
        self.value.deinit();
    }

    pub fn clone(self: QualifiedValue, allocator: std.mem.Allocator) !QualifiedValue {
        return QualifiedValue{
            .value = try self.value.clone(allocator),
            .qualifier = self.qualifier,
            .visibility = self.visibility,
        };
    }
};

pub const Value = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    array: std.ArrayList(Value),
    iterator: Iterator,
    null: void,
    function: Function,
    error_value: ErrorValue,

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
            .null => {
                try writer.writeAll("null");
            },
            .error_value => |err| try writer.print("Error: {s} at line {d}", .{ err.message, err.line }),
            .function => |func| try writer.print("Function {s}", .{func.name.lexeme}),
        }
    }

    pub fn deinit(self: *Value) void {
        switch (self.*) {
            .array => |*arr| arr.deinit(),
            .error_value => |*err| err.deinit(),
            else => {},
        }
    }

    pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
        return switch (self) {
            .array => |arr| Value{ .array = try cloneArrayList(arr, allocator) },
            .error_value => |err| Value{ .error_value = try ErrorValue.init(allocator, err.message, err.line) },
            else => self,
        };
    }

    pub fn validateQualifier(self: Value, qualifier: Qualifier) !void {
        switch (self) {
            .number => {
                if (qualifier == .Peak) return RuntimeError.InvalidQualifier;
            },
            .string => {
                if (qualifier == .Sus) return RuntimeError.InvalidQualifier;
            },
            .array => {
                // Arrays can have any qualifier
            },
            else => {
                if (qualifier != .None) return RuntimeError.InvalidQualifier;
            },
        }
    }

    pub fn getDefaultQualifier(self: Value) Qualifier {
        return switch (self) {
            .number => .Clean,
            .string => .Clean,
            .array => .None,
            else => .None,
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
    values: std.StringHashMap(QualifiedValue),
    enclosing: ?*Environment,
    allocator: std.mem.Allocator,
    scope_type: ScopeType,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return .{
            .values = std.StringHashMap(QualifiedValue).init(allocator),
            .enclosing = enclosing,
            .allocator = allocator,
            .scope_type = if (enclosing == null) .Global else .Local,
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

    pub fn define(self: *Environment, name: []const u8, value: QualifiedValue) !void {
        // Validate qualifier
        try value.value.validateQualifier(value.qualifier);

        // Check for visibility conflicts
        if (self.scope_type == .Global and value.visibility == .Lowkey) {
            return RuntimeError.InvalidVisibility;
        }

        // If we're redefining a variable, clean up the old value first
        if (self.values.getPtr(name)) |old_value| {
            // Check if we can redefine based on visibility
            if (old_value.visibility == .Lowkey and self.scope_type != .Function) {
                return RuntimeError.AccessViolation;
            }
            old_value.deinit();
        }

        const cloned_value = try value.clone(self.allocator);
        try self.values.put(name, cloned_value);
    }

    pub fn get(self: *Environment, name: Token) !QualifiedValue {
        if (self.values.get(name.lexeme)) |value| {
            return value.clone(self.allocator);
        }

        if (self.enclosing) |enclosing| {
            // Check if we can access the variable from the enclosing scope
            if (enclosing.values.get(name.lexeme)) |value| {
                if (value.visibility == .Lowkey and self.scope_type != .Function) {
                    return RuntimeError.AccessViolation;
                }
                return value.clone(self.allocator);
            }
            return enclosing.get(name);
        }

        return RuntimeError.UndefinedVariable;
    }

    pub fn assign(self: *Environment, name: Token, value: QualifiedValue) !void {
        if (self.values.getPtr(name.lexeme)) |old_value| {
            // Check visibility before assignment
            if (old_value.visibility == .Lowkey and self.scope_type != .Function) {
                return RuntimeError.AccessViolation;
            }
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
                var value = Value{ .null = {} };
                if (decl.initializer) |initializer| {
                    const evaluated = try self.evaluateExpression(initializer);
                    value = evaluated.value;
                }

                const qualified_value = QualifiedValue.initWithVisibility(value, value.getDefaultQualifier(), decl.visibility);

                try self.environment.define(decl.name.lexeme, qualified_value);
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

            .Try => |try_stmt| {
                // Create new environment for try block
                var try_env = Environment.init(self.allocator, self.environment);
                defer try_env.deinit();

                const previous = self.environment;
                self.environment = &try_env;

                // Execute try block and catch any errors
                self.executeStatement(try_stmt.body) catch |err| {
                    // Restore environment
                    self.environment = previous;

                    // Handle the error in catch block
                    if (try_stmt.catch_clause) |catch_clause| {
                        var catch_env = Environment.init(self.allocator, self.environment);
                        defer catch_env.deinit();

                        self.environment = &catch_env;
                        defer self.environment = previous;

                        // Create error value and bind it to error variable
                        const error_value = try ErrorValue.init(self.allocator, @errorName(err), catch_clause.error_var.line);
                        try self.environment.define(catch_clause.error_var.lexeme, Value{ .error_value = error_value });

                        // Execute catch block
                        try self.executeStatement(catch_clause.body);
                        return;
                    }

                    // If no catch clause, propagate the error
                    return err;
                };

                // Restore environment after successful try block
                self.environment = previous;
            },

            .Catch => |_| {
                // Catch statements should only be executed through Try statements
                return RuntimeError.InvalidErrorValue;
            },

            .Griddy => |griddy| {
                // Create a new scope for the loop
                var loop_env = Environment.init(self.allocator, self.environment);
                defer loop_env.deinit();

                const previous = self.environment;
                self.environment = &loop_env;
                defer self.environment = previous;

                // Evaluate the iterator expression
                const iterator_value = try self.evaluateExpression(griddy.iterator);
                var iterator = try Iterator.init(self.allocator, iterator_value.value);
                defer iterator.deinit();

                // Execute the loop
                while (true) {
                    const next_value = try iterator.next() orelse break;

                    // Define the loop variable in the current scope
                    const loop_var = QualifiedValue.init(next_value, .Clean);
                    try self.environment.define(griddy.variable.lexeme, loop_var);

                    // Execute the loop body
                    self.executeStatement(griddy.body) catch |err| {
                        switch (err) {
                            RuntimeError.Break => break,
                            RuntimeError.Continue => continue,
                            else => return err,
                        }
                    };
                }
            },

            else => @panic("Unimplemented statement type"),
        }
    }

    fn evaluateExpression(self: *Interpreter, expr: *const Expr) !QualifiedValue {
        switch (expr.*) {
            .Literal => |lit| {
                const value = switch (lit.value.type) {
                    .Number => Value{ .number = try std.fmt.parseFloat(f64, lit.value.lexeme) },
                    .String => Value{ .string = lit.value.lexeme },
                    else => Value{ .null = {} },
                };
                return QualifiedValue.init(value, value.getDefaultQualifier());
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

                // Check if we're calling an error value (error propagation)
                if (callee == .error_value) {
                    return RuntimeError.ErrorPropagation;
                }

                if (callee != .function) return RuntimeError.TypeError;

                var args = std.ArrayList(Value).init(self.allocator);
                defer args.deinit();

                // Check for error values in arguments
                for (call.arguments.items) |arg| {
                    const value = try self.evaluateExpression(arg);
                    if (value == .error_value) {
                        return RuntimeError.ErrorPropagation;
                    }
                    try args.append(try value.clone(self.allocator));
                }

                return try self.executeFunction(callee.function, args);
            },

            else => @panic("Unimplemented expression type"),
        }
    }

    fn evaluateBinaryOp(self: *Interpreter, left: QualifiedValue, operator: Token, right: QualifiedValue) !QualifiedValue {
        const result_qualifier = combineQualifiers(left.qualifier, right.qualifier);
        const result_value = try self.evaluateBinaryOpValues(left.value, operator, right.value);
        return QualifiedValue.init(result_value, result_qualifier);
    }

    fn combineQualifiers(left: Qualifier, right: Qualifier) Qualifier {
        if (left == .Sus or right == .Sus) return .Sus;
        if (left == .Peak or right == .Peak) return .Peak;
        return .Clean;
    }

    fn evaluateBinaryOpValues(self: *Interpreter, left: Value, operator: Token, right: Value) !Value {
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

    fn createErrorValue(self: *Interpreter, message: []const u8, line: usize) !Value {
        const error_value = try ErrorValue.init(self.allocator, message, line);
        return Value{ .error_value = error_value };
    }

    // Helper function to check if a value is an error
    fn isError(value: Value) bool {
        return value == .error_value;
    }

    // Helper function to propagate errors
    fn propagateError(self: *Interpreter, value: Value) !Value {
        if (self.isError(value)) {
            return RuntimeError.ErrorPropagation;
        }
        return value;
    }

    // Add helper method for creating iterators
    fn createIterator(self: *Interpreter, value: Value) !Value {
        const iterator = try Iterator.init(self.allocator, value);
        return Value{ .iterator = iterator };
    }
};

pub const IteratorType = enum {
    Array,
    Range,
    String,
};

pub const Iterator = struct {
    type: IteratorType,
    current: usize,
    // Using a union to store different iterator states
    data: union(IteratorType) {
        Array: struct {
            items: *std.ArrayList(Value),
        },
        Range: struct {
            start: f64,
            end: f64,
            step: f64,
        },
        String: struct {
            text: []const u8,
        },
    },
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, value: Value) !Iterator {
        return switch (value) {
            .array => |arr| Iterator{
                .type = .Array,
                .current = 0,
                .data = .{ .Array = .{ .items = &arr } },
                .allocator = allocator,
            },
            .number => |n| Iterator{
                .type = .Range,
                .current = 0,
                .data = .{ .Range = .{
                    .start = 0,
                    .end = n,
                    .step = 1,
                } },
                .allocator = allocator,
            },
            .string => |s| Iterator{
                .type = .String,
                .current = 0,
                .data = .{ .String = .{ .text = s } },
                .allocator = allocator,
            },
            else => RuntimeError.TypeError,
        };
    }

    pub fn next(self: *Iterator) !?Value {
        switch (self.type) {
            .Array => {
                if (self.current >= self.data.Array.items.items.len) return null;
                const value = try self.data.Array.items.items[self.current].clone(self.allocator);
                self.current += 1;
                return value;
            },
            .Range => {
                const current_value = self.data.Range.start + @as(f64, @floatFromInt(self.current)) * self.data.Range.step;
                if (current_value >= self.data.Range.end) return null;
                self.current += 1;
                return Value{ .number = current_value };
            },
            .String => {
                if (self.current >= self.data.String.text.len) return null;
                const char = self.data.String.text[self.current];
                self.current += 1;
                // Create a single-character string
                const char_str = try self.allocator.dupe(u8, &[_]u8{char});
                return Value{ .string = char_str };
            },
        }
    }

    pub fn deinit(self: *Iterator) void {
        // Clean up any allocated resources
        switch (self.type) {
            .String => {
                // No cleanup needed for string iterators
            },
            .Array => {
                // Array itself is cleaned up by the Value deinit
            },
            .Range => {
                // No cleanup needed for range iterators
            },
        }
    }
};

pub const FunctionBehavior = enum {
    Sigma, // reg function
    Bussin, // entry point function
    Hitting, // method function
    Tweaking, // modifier function
    Vibing, // async function
    Mewing, // generator function
};

pub const Function = struct {
    type: Parser.FunctionType,
    name: Token,
    params: std.ArrayList(Token),
    body: *Stmt,
    closure: *Environment,
    behavior: FunctionBehavior,

    pub fn init(
        allocator: std.mem.Allocator,
        name: Token,
        params: std.ArrayList(Token),
        body: *Stmt,
        closure: *Environment,
        behavior: FunctionBehavior,
    ) Function {
        return .{
            .type = .Function,
            .allocator = allocator,
            .name = name,
            .params = params,
            .body = body,
            .closure = closure,
            .behavior = behavior,
        };
    }
};

pub const StandardLib = struct {
    pub fn initializeStdLib(env: *Environment) !void {
        //initialize basic i/o functions
        try env.define("yeet", createNativeFunction("yeet", yeetPrint));
        try env.define("sheesh", createNativeFunction("sheesh", sheeshPrint));
        try env.define("clutch", createNativeFunction("clutch", clutchInput));

        // initialize array utils
        try env.define("bussin", createNativeFunction("bussin", bussinPush));
        try env.define("bruh", createNativeFunction("bruh", bruhPop));
        try env.define("rizzler", createNativeFunction("rizzler", rizzlerLength));
        try env.define("gyat", createNativeFunction("gyat", gyatSort));

        // type converesion
        try env.define("skibidi", createNativeFunction("skibidi", skibidiStr));
        try env.define("sigma", createNativeFunction("sigma", sigmaNum));
        try env.define("based", createNativeFunction("based", basedBool));

        // math utils
        try env.define("peak", createNativeFunction("peak", peakMax));
        try env.define("mid", createNativeFunction("mid", midMin));
        try env.define("clean", createNativeFunction("clean", cleanAbs));
    }

    fn createNativeFunction(name: []const u8, func: fn ([]Value) Value) QualifiedValue {
        return QualifiedValue.init(
            Value{ .function = Function.init(
                allocator,
                Token{ .type = .Identifier, .lexeme = name, .line = 0 },
                std.ArrayList(Token).init(allocator),
                undefine,
                null,
                .Sigma,
            ) },
            .Clean,
        );
    }

    fn yeetPrint(args: []Value) Value {
        for (args) |arg| {
            std.debug.print("{}", .{arg});
        }
        std.debug.print("\n", .{});
        return Value{ .null = {} };
    }

    fn sheeshPrint(args: []Value) Value {
        for (args) |arg| {
            std.debug.print("{}", .{arg});
        }
        return Value{ .null = {} };
    }

    fn bussinPush(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .array) return RuntimeError.TypeError;

        var array = args[0].array;
        try array.append(try args[1].clone(array.allocator));
        return Value{ .array = array };
    }

    fn bruhPop(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .array) return RuntimeError.TypeError;

        var array = args[0].array;
        if (array.items.len == 0) return RuntimeError.IndexOutOfBounds;

        const popped = try array.pop().clone(array.allocator);
        return popped;
    }

    fn rizzlerLength(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;

        return switch (args[0]) {
            .array => |arr| Value{ .number = @floatFromInt(arr.items.len) },
            .string => |str| Value{ .number = @floatFromInt(str.len) },
            else => RuntimeError.TypeError,
        };
    }

    fn gyatSort(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .array) return RuntimeError.TypeError;

        var array = args[0].array;
        try sortArray(&array);
        return Value{ .array = array };
    }

    // type conversion functions
    fn skibidiStr(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;

        const allocator = args[0].getAllocator();
        const str = switch (args[0]) {
            .number => |n| try std.fmt.allocPrint(allocator, "{d}", .{n}),
            .boolean => |b| try std.fmt.allocPrint(allocator, "{}", .{b}),
            .string => |s| try allocator.dupe(u8, s),
            .null => try allocator.dupe(u8, "null"),
            else => return RuntimeError.TypeError,
        };

        return Value{ .string = str };
    }
};

pub fn init(allocator: std.mem.Allocator) !Interpreter {
    const global_env = try allocator.create(Environment);
    global_env.* = Environment.init(allocator, null);

    // init std lib
    try StandardLib.initializeStdLib(global_env);

    return Interpreter{
        .environment = global_env,
        .allocator = allocator,
    };
}
