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
    AsyncNotAwaited,
    GeneratorExhausted,
    InvalidMethodCall,
    InvalidModifier,
    NotAnEntryPoint,
    StackOverflow,
    MemoryLeak,
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
            .error_value => |err| Value{ .error_value = ErrorValue.init(allocator, err.message, err.line) },
            .iterator => |iter| Value{ .iterator = try iter.clone(allocator) },
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
        } else if (self.values.count() >= 1000) { // Add reasonable limit
            return RuntimeError.StackOverflow;
        }

        const cloned_value = try value.clone(self.allocator) catch {
            return RuntimeError.MemoryLeak;
        };
        try self.values.put(name, cloned_value);
    }

    pub fn get(self: *Environment, name: Token) !QualifiedValue {
        if (self.values.get(name.lexeme)) |value| {
            return value.clone(self.allocator) catch {
                return RuntimeError.MemoryLeak;
            };
        }

        if (self.enclosing) |enclosing| {
            // Check if we can access the variable from the enclosing scope
            if (enclosing.values.get(name.lexeme)) |value| {
                if (value.visibility == .Lowkey and self.scope_type != .Function) {
                    return RuntimeError.AccessViolation;
                }
                return value.clone(self.allocator) catch {
                    return RuntimeError.MemoryLeak;
                };
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
    debug_info: *DebugInfo,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const global_env = try allocator.create(Environment);
        global_env.* = Environment.init(allocator, null);

        // Initialize debug info
        const debug_info = try DebugInfo.init(allocator, "main.rot");

        // Initialize standard library
        try StandardLib.initializeStdLib(global_env);

        return Interpreter{
            .environment = global_env,
            .allocator = allocator,
            .debug_info = debug_info,
        };
    }

    pub fn debugPrint(self: *Interpreter, value: Value) !void {
        const debug_str = try self.debug_info.inspectValue(value);
        defer self.allocator.free(debug_str);
        std.debug.print("{s}\n", .{debug_str});
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
        self.debug_info.updateLocation(stmt.line, stmt.column);

        try self.debug_info.pushScope(try std.fmt.allocPrint(
            self.allocator,
            "statement at line {d}",
            .{stmt.line},
        ));
        defer self.debug_info.popScope();

        switch (stmt.*) {
            .Declaration => |decl| {
                var value = Value{ .null = {} };
                if (decl.initializer) |initializer| {
                    const evaluated = (try self.evaluateExpression(initializer)) catch {
                        return self.handleRuntimeError(RuntimeError.InvalidAssignment, "Error in declaration initialization");
                    };
                    value = evaluated.value;
                }
                try self.environment.define(decl.name.lexeme, QualifiedValue.init(value, value.getDefaultQualifier()));
            },
            .While => |while_stmt| {
                while (true) {
                    const condition = (try self.evaluateExpression(while_stmt.condition)) catch {
                        return self.handleRuntimeError(RuntimeError.TypeError, "Invalid while condition");
                    };
                    if (!try self.isTruthy(condition.value)) break;

                    self.executeStatement(while_stmt.body) catch |err| {
                        switch (err) {
                            RuntimeError.Break => break,
                            RuntimeError.Continue => continue,
                            else => return err,
                        }
                    };
                }
            },
            // Add similar error handling for other statement types
        }
    }

    fn handleRuntimeError(self: *Interpreter, err: RuntimeError, msg: []const u8) !void {
        const error_msg = try self.debug_info.formatError(msg);
        defer self.allocator.free(error_msg);

        // Log the error
        std.debug.print("{s}\n", .{error_msg});

        // Determine if error is fatal
        switch (err) {
            RuntimeError.StackOverflow, RuntimeError.MemoryLeak, RuntimeError.FatalError => return err,
            else => {
                // For non-fatal errors, log and continue
                if (self.recovery_enabled) {
                    // Reset to safe state
                    try self.resetToSafeState();
                    return;
                }
                return err;
            },
        }
    }

    fn evaluateExpression(self: *Interpreter, expr: *const Expr) !QualifiedValue {
        self.debug_info.updateLocation(expr.line, expr.column);

        try self.debug_info.pushScope(try std.fmt.allocPrint(
            self.allocator,
            "expression at line {d}",
            .{expr.line},
        ));
        defer self.debug_info.popScope();
        switch (expr.*) {
            .Literal => |lit| {
                const value = switch (lit.value.type) {
                    .Number => Value{ .number = try std.fmt.parseFloat(f64, lit.value.lexeme) },
                    .String => Value{ .string = lit.value.lexeme },
                    else => Value{ .null = {} },
                };
                return QualifiedValue.init(value, value.getDefaultQualifier()) catch |err| {
                    return self.handleRuntimeError(err, "Error in literal evaluation");
                };
            },
            .Binary => |bin| {
                const left = try self.evaluateExpression(bin.left);
                const right = try self.evaluateExpression(bin.right);

                return self.evaluateBinaryOp(left, bin.operator, right) catch {
                    return self.handleRuntimeError(RuntimeError.TypeError, "Invalid binary operation");
                };
            },
            .Unary => |un| {
                const right = try self.evaluateExpression(un.right);
                return self.evaluateUnaryOp(un.operator, right) catch |err| {
                    return self.handleRuntimeError(err, "Error in unary operation");
                };
            },
            .Variable => |var_expr| {
                return try self.environment.get(var_expr.name) catch |err| {
                    return self.handleRuntimeError(err, "Error in variable evaluation");
                };
            },
            .Assignment => |assign| {
                const value = try self.evaluateExpression(assign.value);
                self.environment.assign(assign.name, value) catch |err| {
                    return self.handleRuntimeError(err, "Error in assignment");
                };
                return value;
            },
            .Array => |array| {
                var elements = std.ArrayList(Value).init(self.allocator);
                for (array.elements.items) |element| {
                    const value = try self.evaluateExpression(element);
                    try elements.append(try value.clone(self.allocator));
                }
                return Value{ .array = elements } catch |err| {
                    return self.handleRuntimeError(err, "Error in array evaluation");
                };
            },

            .Index => |index| {
                const array = try self.evaluateExpression(index.array);
                const idx = try self.evaluateExpression(index.index);

                if (array != .array) {
                    return self.handleRuntimeError(RuntimeError.TypeError, "Can only index into arrays");
                }
                if (idx != .number) {
                    return self.handleRuntimeError(RuntimeError.TypeError, "Array index must be a number");
                }

                const i = @as(usize, @intFromFloat(idx.number));
                if (i >= array.array.items.len) {
                    return self.handleRuntimeError(RuntimeError.IndexOutOfBounds, "Array index out of bounds");
                }

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

                if (callee == .error_value) {
                    return self.handleRuntimeError(RuntimeError.ErrorPropagation, "Error propagated through function call");
                }
                if (callee != .function) {
                    return self.handleRuntimeError(RuntimeError.TypeError, "Can only call functions");
                }

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
        const result_value = self.evaluateBinaryOpValues(left.value, operator, right.value) catch |err| {
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "Error in binary operation '{s}'",
                .{operator.lexeme},
            );
            defer self.allocator.free(msg);
            return self.handleRuntimeError(err, msg);
        };
        return QualifiedValue.init(result_value, result_qualifier);
    }

    fn combineQualifiers(left: Qualifier, right: Qualifier) Qualifier {
        if (left == .Sus or right == .Sus) return .Sus;
        if (left == .Peak or right == .Peak) return .Peak;
        return .Clean;
    }

    fn evaluateBinaryOpValues(self: *Interpreter, left: Value, operator: Token, right: Value) !Value {
        switch (operator.type) {
            .Plus => {
                if (left == .number and right == .number) {
                    return Value{ .number = left.number + right.number };
                }
                if (left == .string and right == .string) {
                    const result = try StringOps.concatenate(self.allocator, left.string, right.string);
                    return Value{ .string = result };
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
        // Handle native functions first
        if (func.native_fn) |native_fn| {
            return native_fn(args.items);
        }

        // Check for valid entry point
        if (func.isEntryPoint() and !self.is_program_start) {
            return RuntimeError.NotAnEntryPoint;
        }

        // Handle different function behaviors
        switch (func.behavior) {
            .Sigma => return self.executeRegularFunction(func, args) catch |err| {
                return self.handleRuntimeError(err, "Error in regular function execution");
            },
            .Bussin => return self.executeEntryPoint(func, args) catch |err| {
                return self.handleRuntimeError(err, "Error in entry point function execution");
            },
            .Hitting => {
                if (func.instance_context == null) {
                    return self.handleRuntimeError(RuntimeError.InvalidMethodCall, "Method called without instance context");
                }
                return self.executeMethodFunction(func, args) catch |err| {
                    return self.handleRuntimeError(err, "Error in method execution");
                };
            },
            .Tweaking => {
                if (func.modifier_target == null) {
                    return self.handleRuntimeError(RuntimeError.InvalidModifier, "Modifier function called without target");
                }
                return self.executeModifierFunction(func, args) catch |err| {
                    return self.handleRuntimeError(err, "Error in modifier function execution");
                };
            },
            .Vibing => {
                if (!self.is_awaiting) {
                    return self.handleRuntimeError(RuntimeError.AsyncNotAwaited, "Async function must be awaited");
                }
                return self.executeAsyncFunction(func, args) catch |err| {
                    return self.handleRuntimeError(err, "Error in async function execution");
                };
            },
            .Mewing => {
                if (func.generator_state) |state| {
                    if (state.is_done) {
                        return self.handleRuntimeError(RuntimeError.GeneratorExhausted, "Generator has been exhausted");
                    }
                }
                return self.executeGeneratorFunction(func, args) catch |err| {
                    return self.handleRuntimeError(err, "Error in generator function execution");
                };
            },
        }
    }

    fn executeRegularFunction(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        var environment = Environment.init(self.allocator, func.closure);
        defer environment.deinit();

        try self.bindParameters(&environment, func.params, args);

        const previous = self.environment;
        self.environment = &environment;
        defer self.environment = previous;

        if (func.body) |body| {
            try self.executeStatement(body);
        }

        return Value{ .null = {} };
    }

    fn executeEntryPoint(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        self.is_program_start = true;
        defer self.is_program_start = false;
        return self.executeRegularFunction(func, args);
    }

    fn executeMethodFunction(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        if (func.instance_context == null) {
            return RuntimeError.InvalidMethodCall;
        }

        var environment = Environment.innit(self.allocator, func.closure);
        defer environment.deinit();

        try environment.define("this", func.instance_context.?.*);

        return self.executeRegularFunction(func, args);
    }

    fn executeModifierFunction(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        if (func.modifier_target == null) {
            return RuntimeError.InvalidModifier;
        }

        const result = try self.executeRegularFunction(func, args);

        func.modifier_target.?.* = result;

        return result;
    }

    fn executeAsyncFunction(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        if (!self.is_awaiting) {
            return self.handleRuntimeError(RuntimeError.AsyncNotAwaited, "Async function must be awaited");
        }
        return self.executeRegularFunction(func, args) catch |err| {
            return self.handleRuntimeError(err, "Error in async function execution");
        };
    }

    fn executeGeneratorFunction(self: *Interpreter, func: Function, args: std.ArrayList(Value)) !Value {
        if (func.generator_state == null) {
            const state = try self.allocator.create(GeneratorState);
            state.* = GeneratorState.init(self.environment);
            func.generator_state = state;
        }

        const state = func.generator_state.?;
        if (state.is_done) {
            return self.handleRuntimeError(RuntimeError.GeneratorExhausted, "Generator has been exhausted");
        }

        const result = try self.executeNextGenerator(func, state, args) catch |err| {
            return self.handleRuntimeError(err, "Error in generator function execution");
        };
        return result;
    }

    fn executeNextGenerator(self: *Interpreter, func: Function, state: *GeneratorState, args: std.ArrayList(Value)) !Value {
        var environment = Environment.init(self.allocator, func.closure);
        defer environment.deinit();

        // Restore generator context
        try self.bindParameters(&environment, func.params, args);
        const previous = self.environment;
        self.environment = &environment;
        defer self.environment = previous;

        // Execute until next yield or end
        if (func.body) |body| {
            // Implementation would continue here with yield handling
            _ = body;
        }

        // For now, just return null and mark as done
        state.is_done = true;
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

    pub const ErrorLevel = enum {
        Warning,
        Error,
        Fatal,
    };

    fn reportError(self: *Interpreter, level: ErrorLevel, err: RuntimeError, msg: []const u8) !void {
        // Create error context
        const error_context = try self.debug_info.formatError(msg);
        defer self.allocator.free(error_context);

        // Format the error level prefix
        const level_prefix = switch (level) {
            .Warning => "\x1b[33mWarning\x1b[0m", // Yellow
            .Error => "\x1b[31mError\x1b[0m", // Red
            .Fatal => "\x1b[1;31mFatal Error\x1b[0m", // Bold Red
        };

        // Log the error with appropriate formatting
        std.debug.print(
            \\{s}: {s}
            \\Error Type: {s}
            \\{s}
            \\
        , .{
            level_prefix,
            msg,
            @tagName(err),
            error_context,
        });

        // Handle error based on severity
        switch (level) {
            .Warning => {
                if (self.debug_info.warnings_as_errors) {
                    return err;
                }
            },
            .Error => {
                try self.resetToSafeState();
                return err;
            },
            .Fatal => {
                try self.cleanup();
                return err;
            },
        }
    }

    pub fn resetToSafeState(self: *Interpreter) !void {
        // Clear temporary debug data while keeping essential information
        while (self.debug_info.scope_stack.items.len > 1) {
            const scope = self.debug_info.scope_stack.pop();
            self.allocator.free(scope);
        }

        // Reset location to global scope
        self.debug_info.line_number = 0;
        self.debug_info.column = 0;

        // Add recovery marker to stack
        try self.debug_info.pushScope("error recovery");
    }

    pub fn cleanup(self: *Interpreter) !void {
        // Log cleanup operation
        try self.debug_info.pushScope("fatal error cleanup");
        defer self.debug_info.popScope();

        // Clear all scopes
        while (self.debug_info.scope_stack.items.len > 0) {
            const scope = self.debug_info.scope_stack.pop();
            self.allocator.free(scope);
        }

        // Reset debug info to initial state
        self.debug_info.line_number = 0;
        self.debug_info.column = 0;
    }

    pub fn clearTemporaryData(self: *Interpreter) void {
        // Clear any temporary debug data while keeping essential information
        while (self.debug_info.scope_stack.items.len > 1) {
            const scope = self.debug_info.scope_stack.pop();
            self.allocator.free(scope);
        }
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
                const value = try self.data.Array.items.items[self.current].clone(self.allocator) catch {
                    return RuntimeError.MemoryLeak;
                };
                self.current += 1;
                return value;
            },
            .Range => {
                const current_value = self.data.Range.start + @as(f64, @floatFromInt(self.current)) * self.data.Range.step;
                if (current_value >= self.data.Range.end) return null;
                if (self.current >= 1000000) { // Add reasonable limit
                    return RuntimeError.StackOverflow;
                }
                self.current += 1;
                return Value{ .number = current_value };
            },
            .String => {
                if (self.current >= self.data.String.text.len) return null;
                const char = self.data.String.text[self.current];
                self.current += 1;
                const char_str = try self.allocator.dupe(u8, &[_]u8{char}) catch {
                    return RuntimeError.MemoryLeak;
                };
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

pub const StringOps = struct {
    pub fn concatenate(allocator: std.mem.Allocator, a: []const u8, b: []const u8) ![]const u8 {
        const result = try allocator.alloc(u8, a.len + b.len);
        @memcpy(result[0..a.len], a);
        @memcpy(result[a.len..], b);
        return result;
    }

    pub fn substring(allocator: std.mem.Allocator, str: []const u8, start: usize, end: usize) ![]const u8 {
        if (start > end or end > str.len) {
            return RuntimeError.IndexOutOfBounds;
        }
        const result = try allocator.alloc(u8, end - start);
        @memcpy(result[0..], str[start..end]);
        return result;
    }

    pub fn trim(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
        var start: usize = 0;
        var end: usize = str.len;

        // Trim start
        while (start < str.len and std.ascii.isWhitespace(str[start])) {
            start += 1;
        }

        // Trim end
        while (end > start and std.ascii.isWhitespace(str[end - 1])) {
            end -= 1;
        }

        const result = try allocator.alloc(u8, end - start);
        @memcpy(result[0..], str[start..end]);
        return result;
    }

    pub fn toLowerCase(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
        const result = try allocator.alloc(u8, str.len);
        for (str, 0..) |c, i| {
            result[i] = std.ascii.toLower(c);
        }
        return result;
    }

    pub fn toUpperCase(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
        const result = try allocator.alloc(u8, str.len);
        for (str, 0..) |c, i| {
            result[i] = std.ascii.toUpper(c);
        }
        return result;
    }

    pub fn split(allocator: std.mem.Allocator, str: []const u8, delimiter: u8) !std.ArrayList([]const u8) {
        var result = std.ArrayList([]const u8).init(allocator);
        var start: usize = 0;

        for (str, 0..) |c, i| {
            if (c == delimiter) {
                if (i > start) {
                    const part = try allocator.dupe(u8, str[start..i]);
                    try result.append(part);
                }
                start = i + 1;
            }
        }

        if (start < str.len) {
            const part = try allocator.dupe(u8, str[start..]);
            try result.append(part);
        }

        return result;
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

pub const GeneratorState = struct {
    current_value: Value,
    is_done: bool,
    context: *Environment,
    next_stmt_index: usize,

    pub fn init(context: *Environment) GeneratorState {
        return .{
            .current_value = Value{ .null = {} },
            .is_done = false,
            .context = context,
            .next_stmt_index = 0,
        };
    }
};

pub const Function = struct {
    type: Parser.FunctionType,
    name: Token,
    params: std.ArrayList(Token),
    body: ?*Stmt,
    closure: *Environment,
    behavior: FunctionBehavior,
    native_fn: ?fn ([]Value) Value,
    is_async: bool,
    generator_state: ?*GeneratorState,
    instance_context: ?*Value,
    modifier_target: ?*Value,

    pub fn init(
        allocator: std.mem.Allocator,
        name: Token,
        params: std.ArrayList(Token),
        body: *Stmt,
        closure: *Environment,
        behavior: FunctionBehavior,
        native_fn: ?fn ([]Value) Value,
    ) Function {
        return .{
            .type = .Function,
            .allocator = allocator,
            .name = name,
            .params = params,
            .body = body,
            .closure = closure,
            .behavior = behavior,
            .native_fn = native_fn,
            .is_async = behavior == .Vibing,
            .generator_state = null,
            .instance_context = null,
            .modifier_target = null,
        };
    }

    pub fn isEntryPoint(self: Function) bool {
        return self.behavior == .Bussin;
    }
};

pub const StandardLib = struct {
    pub fn initializeStdLib(env: *Environment) !void {
        const allocator = env.allocator;

        // Basic I/O functions
        try env.define("yeet", try createNativeFunction(allocator, "yeet", yeetPrint));
        try env.define("sheesh", try createNativeFunction(allocator, "sheesh", sheeshPrint));
        try env.define("clutch", try createNativeFunction(allocator, "clutch", clutchInput));

        // Array utilities
        try env.define("bussin", try createNativeFunction(allocator, "bussin", bussinPush));
        try env.define("bruh", try createNativeFunction(allocator, "bruh", bruhPop));
        try env.define("rizzler", try createNativeFunction(allocator, "rizzler", rizzlerLength));
        try env.define("gyat", try createNativeFunction(allocator, "gyat", gyatSort));

        // Type conversion
        try env.define("skibidi", try createNativeFunction(allocator, "skibidi", skibidiStr));
        try env.define("sigma", try createNativeFunction(allocator, "sigma", sigmaNum));
        try env.define("based", try createNativeFunction(allocator, "based", basedBool));

        // Math utilities
        try env.define("peak", try createNativeFunction(allocator, "peak", peakMax));
        try env.define("mid", try createNativeFunction(allocator, "mid", midMin));
        try env.define("clean", try createNativeFunction(allocator, "clean", cleanAbs));

        // string manipulation functions
        try env.define("cap", try createNativeFunction(allocator, "cap", capUpperCase));
        try env.define("nocap", try createNativeFunction(allocator, "nocap", noCapLowerCase));
        try env.define("slay", try createNativeFunction(allocator, "slay", slayTrim));
        try env.define("periodt", try createNativeFunction(allocator, "periodt", periodtSplit));
        try env.define("snatch", try createNativeFunction(allocator, "snatch", snatchSubstring));
    }

    // array operations
    fn bussinSort(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .array) return RuntimeError.TypeError;

        var arra = args[0].array;
        const comparator = if (args.len > 1 and args[1] == .function) args[1].function else null;

        try sortArray(&array, comparator);
        return Value{ .array = array };
    }

    fn gyatFilter(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .array or args[1] != .function) return RuntimeError.TypeError;

        const array = args[0].array;
        const predicate = args[1].function;
        var result = std.ArrayList(Value).init(array.allocator);

        for (array.items) |item| {
            var pred_args = std.ArrayList(Value).init(array.allocator);
            try pred_args.append(try item.clone(array.allocator));
            const keep = try callFunction(predicate, pred_args);
            if (try isTruthy(keep)) {
                try result.append(try item.clone(array.allocator));
            }

            return Value{ .array = result };
        }
    }

    fn rizzMap(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .array or args[1] != .function) return RuntimeError.TypeError;

        const array = args[0].array;
        const mapper = args[1].function;
        var result = std.ArrayList(Value).init(array.allocator);

        for (array.items) |item| {
            var map_args = std.ArrayList(Value).init(array.allocator);
            try map_args.append(try item.clone(array.allocator));
            const mapped = try callFunction(mapper, map_args);
            try result.append(mapped);
        }

        return Value{ .array = result };
    }

    fn skibidiReduce(args: []Value) !Value {
        if (args.len < 3) return RuntimeError.InvalidOperand;
        if (args[0] != .array or args[1] != .function) return RuntimeError.TypeError;

        const array = args[0].array;
        const reducer = args[1].function;
        var accumulator = try args[2].clone(array.allocator);

        for (array.items) |item| {
            var reduce_args = std.ArrayList(Value).init(array.allocator);
            try reduce_args.append(accumulator);
            try reduce_args.append(try item.clone(array.allocator));
            accumulator = try callFunction(reducer, reduce_args);
        }

        return accumulator;
    }

    fn fanumSlice(args: []Value) !Value {
        if (args.len < 3) return RuntimeError.InvalidOperand;
        if (args[0] != .array or args[1] != .number or args[2] != .number) {
            return RuntimeError.TypeError;
        }

        const array = args[0].array;
        const start = @as(usize, @intFromFloat(args[1].number));
        const end = @as(usize, @intFromFloat(args[2].number));

        if (start > end or end > array.items.len) {
            return RuntimeError.IndexOutOfBounds;
        }

        var result = std.ArrayList(Value).init(array.allocator);
        var i: usize = start;
        while (i < end) : (i += 1) {
            try result.append(try array.items[i].clone(array.allocator));
        }

        return Value{ .array = result };
    }

    fn createNativeFunction(
        allocator: std.mem.Allocator,
        name: []const u8,
        func: fn ([]Value) Value,
    ) !QualifiedValue {
        return QualifiedValue.init(
            Value{
                .function = Function.init(
                    allocator,
                    Token{ .type = .Identifier, .lexeme = name, .line = 0 },
                    std.ArrayList(Token).init(allocator),
                    null, // No body for native functions
                    null,
                    .Sigma,
                    func, // Store the function pointer
                ),
            },
            .Clean,
        );
    }

    fn yeetPrint(args: []Value) !Value {
        for (args) |arg| {
            std.debug.print("{}", .{arg}) catch {
                return RuntimeError.Custom;
            };
        }
        return Value{ .null = {} };
    }

    fn sheeshPrint(args: []Value) !Value {
        for (args) |arg| {
            std.debug.print("{}", .{arg}) catch {
                return RuntimeError.Custom;
            };
        }
        return Value{ .null = {} };
    }

    fn bussinPush(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .array) return RuntimeError.TypeError;

        var array = args[0].array;
        if (array.items.len >= 10000) {
            return RuntimeError.StackOverflow;
        }

        const cloned = args[1].clone(array.allocator) catch {
            return RuntimeError.MemoryLeak;
        };
        array.append(cloned) catch {
            return RuntimeError.MemoryLeak;
        };
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
        try gyatHelper(&array);
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

    fn sigmaNum(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;

        return switch (args[0]) {
            .number => args[0],
            .string => |s| Value{ .number = try std.fmt.parseFloat(f64, s) },
            .boolean => |b| Value{ .number = if (b) 1 else 0 },
            else => RuntimeError.TypeError,
        };
    }

    fn basedBool(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;

        return Value{ .boolean = switch (args[0]) {
            .boolean => |b| b,
            .number => |n| n != 0,
            .string => |s| s.len > 0,
            .null => false,
            .array => |arr| arr.items.len > 0,
            else => true,
        } };
    }

    // math utils
    fn peakMax(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;

        return Value{ .number = @max(args[0].number, args[1].number) };
    }

    fn midMin(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;

        return Value{ .number = @min(args[0].number, args[1].number) };
    }

    fn cleanAbs(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .number) return RuntimeError.TypeError;

        return Value{ .number = std.math.fabs(args[0].number) };
    }

    // helper function for array sorting
    fn gyatHelper(array: *std.ArrayList(Value)) !void {
        const Context = struct {
            pub fn lessThan(_: @This(), a: Value, b: Value) bool {
                return switch (a) {
                    .number => |n1| switch (b) {
                        .number => |n2| n1 < n2,
                        else => false,
                    },
                    .string => |s1| switch (b) {
                        .string => |s2| std.mem.lessThan(u8, s1, s2),
                        else => ?false,
                    },
                    else => false,
                };
            }
        };

        std.sort.sort(Value, array.items, Context{}, Context.lessThan);
    }

    // input function implementation
    fn clutchInput(comptime _: []Value) !Value {
        const allocator = std.heap.page_allocator;
        var buffer: [1024]u8 = undefined;

        if (try std.io.getStdIn().reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            const input = try allocator.dupe(u8, line) catch {
                return RuntimeError.MemoryLeak;
            };
            return Value{ .string = input };
        }

        return RuntimeError.InvalidOperand;
    }

    // String Manipulation
    fn capSplit(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .string or args[1] != .string) return RuntimeError.TypeError;

        const str = args[0].string;
        const delimiter = args[1].string;
        const allocator = std.heap.page_allocator;

        var result = std.ArrayList(Value).init(allocator);
        var iter = std.mem.split(u8, str, delimiter);

        while (iter.next()) |part| {
            const part_copy = try allocator.dupe(u8, part);
            try result.append(Value{ .string = part_copy });
        }

        return Value{ .array = result };
    }
    fn capUpperCase(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .string) return RuntimeError.TypeError;

        const result = try StringOps.toUpperCase(std.heap.page_allocator, args[0].string);
        return Value{ .string = result };
    }

    fn noCapLowerCase(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .string) return RuntimeError.TypeError;

        const result = try StringOps.toLowerCase(std.heap.page_allocator, args[0].string);
        return Value{ .string = result };
    }

    fn slayTrim(args: []Value) !Value {
        if (args.len < 1) return RuntimeError.InvalidOperand;
        if (args[0] != .string) return RuntimeError.TypeError;

        const result = try StringOps.trim(std.heap.page_allocator, args[0].string);
        return Value{ .string = result };
    }

    fn periodtSplit(args: []Value) !Value {
        if (args.len < 2) return RuntimeError.InvalidOperand;
        if (args[0] != .string or args[1] != .string) return RuntimeError.TypeError;
        if (args[1].string.len != 1) return RuntimeError.InvalidOperand;

        const parts = try StringOps.split(std.heap.page_allocator, args[0].string, args[1].string[0]);
        var result = std.ArrayList(Value).init(std.heap.page_allocator);

        for (parts.items) |part| {
            try result.append(Value{ .string = part });
        }

        return Value{ .array = result };
    }

    fn snatchSubstring(args: []Value) !Value {
        if (args.len < 3) return RuntimeError.InvalidOperand;
        if (args[0] != .string or args[1] != .number or args[2] != .number) {
            return RuntimeError.TypeError;
        }

        const start = @as(usize, @intFromFloat(args[1].number));
        const end = @as(usize, @intFromFloat(args[2].number));
        const result = try StringOps.substring(std.heap.page_allocator, args[0].string, start, end);
        return Value{ .string = result };
    }
};
pub const DebugInfo = struct {
    line_number: usize,
    column: usize,
    file_name: []const u8,
    scope_stack: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,
    warnings_as_errors: bool = false,

    pub const ErrorLevel = enum {
        Warning,
        Error,
        Fatal,
    };

    pub fn init(allocator: std.mem.Allocator, file_name: []const u8) !*DebugInfo {
        const debug_info = try allocator.create(DebugInfo);
        debug_info.* = .{
            .line_number = 0,
            .column = 0,
            .file_name = file_name,
            .scope_stack = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
        return debug_info;
    }

    pub fn deinit(self: *DebugInfo) void {
        self.scope_stack.deinit();
    }

    pub fn pushScope(self: *DebugInfo, scope_name: []const u8) !void {
        try self.scope_stack.append(try self.allocator.dupe(u8, scope_name));
    }

    pub fn popScope(self: *DebugInfo) void {
        if (self.scope_stack.items.len > 0) {
            const last = self.scope_stack.pop();
            self.allocator.free(last);
        }
    }

    pub fn updateLocation(self: *DebugInfo, line: usize, column: usize) void {
        self.line_number = line;
        self.column = column;
    }

    pub fn formatError(self: *DebugInfo, error_msg: []const u8) ![]const u8 {
        const stack_trace = try self.generateStackTrace();
        defer self.allocator.free(stack_trace);

        return try std.fmt.allocPrint(
            self.allocator,
            \\Error: {s}
            \\  at {s}:{d}:{d}
            \\  in {s}
            \\
            \\Stack trace:
            \\{s}
        ,
            .{
                error_msg,
                self.file_name,
                self.line_number,
                self.column,
                if (self.scope_stack.items.len > 0)
                    self.scope_stack.items[self.scope_stack.items.len - 1]
                else
                    "global scope",
                stack_trace,
            },
        );
    }

    pub fn generateStackTrace(self: *DebugInfo) ![]const u8 {
        var trace = std.ArrayList(u8).init(self.allocator);
        defer trace.deinit();

        var writer = trace.writer();

        var i: usize = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            try writer.print("  at {s}\n", .{self.scope_stack.items[i]});
        }

        return try trace.toOwnedSlice();
    }

    pub fn inspectValue(self: *DebugInfo, value: Value) ![]const u8 {
        return switch (value) {
            .number => |n| try std.fmt.allocPrint(self.allocator, "Number({d})", .{n}),
            .string => |s| try std.fmt.allocPrint(self.allocator, "String(\"{s}\")", .{s}),
            .boolean => |b| try std.fmt.allocPrint(self.allocator, "Boolean({s})", .{if (b) "true" else "false"}),
            .null => try self.allocator.dupe(u8, "Null"),
            .array => |arr| try self.inspectArray(arr),
            .function => |func| try self.inspectFunction(func),
            .error_value => |err| try std.fmt.allocPrint(self.allocator, "Error({s})", .{err.message}),
            else => try self.allocator.dupe(u8, "Unknown"),
        };
    }

    fn inspectArray(self: *DebugInfo, array: std.ArrayList(Value)) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        try result.appendSlice("Array([\n");
        for (array.items) |item| {
            const item_str = try self.inspectValue(item);
            defer self.allocator.free(item_str);
            try result.writer().print("  {s},\n", .{item_str});
        }
        try result.appendSlice("])");

        return try result.toOwnedSlice();
    }

    fn inspectFunction(self: *DebugInfo, func: Function) ![]const u8 {
        return try std.fmt.allocPrint(
            self.allocator,
            "Function({s}, {s}, params: {d})",
            .{
                func.name.lexeme,
                @tagName(func.behavior),
                func.params.items.len,
            },
        );
    }

    pub fn reportError(self: *DebugInfo, level: ErrorLevel, err: RuntimeError, msg: []const u8) !void {
        // Create error context
        const error_context = try self.formatError(msg);
        defer self.allocator.free(error_context);

        // Format the error level prefix
        const level_prefix = switch (level) {
            .Warning => "\x1b[33mWarning\x1b[0m", // Yellow
            .Error => "\x1b[31mError\x1b[0m", // Red
            .Fatal => "\x1b[1;31mFatal Error\x1b[0m", // Bold Red
        };

        // Log the error with appropriate formatting
        std.debug.print(
            \\{s}: {s}
            \\Error Type: {s}
            \\{s}
            \\
        , .{
            level_prefix,
            msg,
            @tagName(err),
            error_context,
        });

        // Handle error based on severity
        switch (level) {
            .Warning => {
                if (self.warnings_as_errors) {
                    return err;
                }
            },
            .Error => {
                try self.resetToSafeState();
                return err;
            },
            .Fatal => {
                try self.cleanup();
                return err;
            },
        }
    }

    pub fn resetToSafeState(self: *DebugInfo) !void {
        // Clear temporary debug data while keeping essential information
        while (self.scope_stack.items.len > 1) {
            const scope = self.scope_stack.pop();
            self.allocator.free(scope);
        }

        // Reset location to global scope
        self.line_number = 0;
        self.column = 0;

        // Add recovery marker to stack
        try self.pushScope("error recovery");
    }

    pub fn cleanup(self: *DebugInfo) !void {
        // Log cleanup operation
        try self.pushScope("fatal error cleanup");
        defer self.popScope();

        // Clear all scopes
        while (self.scope_stack.items.len > 0) {
            const scope = self.scope_stack.pop();
            self.allocator.free(scope);
        }

        // Reset debug info to initial state
        self.line_number = 0;
        self.column = 0;
    }

    pub fn clearTemporaryData(self: *DebugInfo) void {
        // Clear any temporary debug data while keeping essential information
        while (self.scope_stack.items.len > 1) {
            const scope = self.scope_stack.pop();
            self.allocator.free(scope);
        }
    }
};

pub const ErrorContext = struct {
    message: []const u8,
    line: usize,
    column: usize,
    scope: []const u8,
    value_context: ?Value,
    allocator: std.mem.Allocator,

    pub fn format(self: ErrorContext) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        // Write the main error message
        try result.writer().print("Error: {s}\n", .{self.message});

        // Write location information
        try result.writer().print("  at line {d}, column {d}\n", .{ self.line, self.column });

        // Write scope information
        try result.writer().print("  in scope: {s}\n", .{self.scope});

        // Add value context if available
        if (self.value_context) |value| {
            try result.writer().print("\nValue context:\n", .{});
            switch (value) {
                .number => |n| try result.writer().print("  Number: {d}\n", .{n}),
                .string => |s| try result.writer().print("  String: \"{s}\"\n", .{s}),
                .boolean => |b| try result.writer().print("  Boolean: {}\n", .{b}),
                .array => |arr| {
                    try result.writer().print("  Array with {d} elements\n", .{arr.items.len});
                    if (arr.items.len > 0) {
                        try result.writer().print("  First element: ", .{});
                        try arr.items[0].format("", .{}, result.writer());
                        try result.writer().print("\n", .{});
                    }
                },
                .function => |func| try result.writer().print("  Function: {s}\n", .{func.name.lexeme}),
                .error_value => |err| try result.writer().print("  Error: {s}\n", .{err.message}),
                .iterator => try result.writer().print("  Iterator\n", .{}),
                .null => try result.writer().print("  null\n", .{}),
            }
        }

        // Add separator line
        try result.writer().print("\n-------------------\n", .{});

        return result.toOwnedSlice();
    }

    pub fn init(
        allocator: std.mem.Allocator,
        message: []const u8,
        line: usize,
        column: usize,
        scope: []const u8,
        value_context: ?Value,
    ) ErrorContext {
        return .{
            .allocator = allocator,
            .message = message,
            .line = line,
            .column = column,
            .scope = scope,
            .value_context = value_context,
        };
    }

    pub fn deinit(self: *ErrorContext) void {
        if (self.value_context) |*value| {
            value.deinit();
        }
    }
};

pub fn init(allocator: std.mem.Allocator) !Interpreter {
    const global_env = try allocator.create(Environment);
    global_env.* = Environment.init(std.heap.page_allocator, null);

    // init std lib
    try StandardLib.initializeStdLib(global_env);

    return Interpreter{
        .environment = global_env,
        .allocator = allocator,
    };
}
