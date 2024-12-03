const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    allocator: std.mem.Allocator,
    had_error: bool,

    // Move enums inside the Parser struct
    pub const FunctionType = enum {
        Sigma, // Regular function
        Bussin, // Main/entry point function
        Hitting, // Methods/class functions
        Tweaking, // Modifier functions
        Vibing, // Async functions
        Mewing, // Generator functions
    };

    pub const ScopeType = enum {
        Lowkey, // private scope
        Highkey, // public scope
    };

    pub const ValueQualifier = enum {
        Sus, // nullable
        Clean, // validated
        Peak, // optimized
        Mid, // default
        Based, // constant
        Devious, // unsafe
    };

    // ast node types
    pub const ExprType = enum {
        Binary,
        Grouping,
        Literal,
        Unary,
        Variable,
        Assignment,
        Call,
        Array,
        Index,
        FanumSlice,
        Griddy,
    };

    pub const StmtType = enum {
        Expression,
        Declaration,
        Block,
        If,
        While,
        Return,
        Function,
        Try,
        Catch,
        Scope,
    };

    pub const Expr = union(ExprType) {
        Binary: struct {
            left: *Expr,
            operator: Token,
            right: *Expr,
        },
        Grouping: struct {
            expression: *Expr,
        },
        Literal: struct {
            value: Token,
        },
        Unary: struct {
            operator: Token,
            right: *Expr,
        },
        Variable: struct {
            name: Token,
        },
        Assignment: struct {
            name: Token,
            value: *Expr,
        },
        Call: struct {
            callee: *Expr,
            arguments: std.ArrayList(*Expr),
        },
        Array: struct {
            elements: std.ArrayList(*Expr),
        },
        Index: struct {
            array: *Expr,
            index: *Expr,
        },
        FanumSlice: struct {
            array: *Expr,
            start: *Expr,
            end: *Expr,
        },
        Griddy: struct {
            iterator: Token,
            iterable: *Expr,
            body: *Stmt,
        },
    };

    pub const Stmt = union(StmtType) {
        Expression: struct {
            expr: *Expr,
        },
        Declaration: struct {
            name: Token,
            qualifier: ValueQualifier,
            initializer: ?*Expr,
        },
        Block: struct {
            statements: std.ArrayList(Stmt),
        },
        If: struct {
            condition: *Expr,
            then_branch: *Stmt,
            else_branch: ?*Stmt,
        },
        While: struct {
            condition: *Expr,
            body: *Stmt,
        },
        Return: struct {
            keyword: Token,
            value: ?*Expr,
        },
        Function: struct {
            type: FunctionType,
            name: Token,
            params: std.ArrayList(Token),
            body: *Stmt,
        },
        Try: struct {
            body: *Stmt,
            catch_clause: *Stmt,
        },
        Catch: struct {
            error_var: Token,
            body: *Stmt,
        },
        Scope: struct {
            type: ScopeType,
            declarations: std.ArrayList(Stmt),
        },
    };

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
            .had_error = false,
        };
    }

    pub fn parse(self: *Parser) !std.ArrayList(Stmt) {
        var statements = std.ArrayList(Stmt).init(self.allocator);

        while (!self.isAtEnd()) {
            if (try self.declaration()) |stmt| {
                try statements.append(stmt);
            }
        }

        return statements;
    }

    fn declaration(self: *Parser) !?Stmt {
        if (self.match(.Sigma) or self.match(.Bussin) or
            self.match(.Hitting) or self.match(.Tweaking) or
            self.match(.Vibing) or self.match(.Mewing))
        {
            return try self.functionDeclaration();
        }
        if (self.match(.Lowkey) or self.match(.Highkey)) {
            return try self.scopeDeclaration();
        }
        if (self.match(.Sus) or self.match(.Clean) or
            self.match(.Peak) or self.match(.Based) or
            self.match(.Devious))
        {
            return try self.varDeclaration();
        }

        return try self.statement();
    }

    fn varDeclaration(self: *Parser) !Stmt {
        var qualifier = ValueQualifier.Mid;

        switch (self.previous().type) {
            .Sus => qualifier = .Sus,
            .Clean => qualifier = .Clean,
            .Peak => qualifier = .Peak,
            .Based => qualifier = .Based,
            .Devious => qualifier = .Devious,
            else => {},
        }

        const name = try self.consume(.Identifier, "Expected variable name");

        var initializer: ?*Expr = null;
        if (self.match(.Equal)) {
            initializer = try self.expression();
        }

        _ = try self.consume(.Semicolon, "Expected ';' after variable declaration");

        return Stmt{ .Declaration = .{
            .name = name,
            .qualifier = qualifier,
            .initializer = initializer,
        } };
    }

    fn statement(self: *Parser) !Stmt {
        if (self.match(.RealTalk)) return try self.blockStatement();
        if (self.match(.NoShot)) return try self.ifStatement();
        if (self.match(.Deadass)) return try self.whileStatement();
        if (self.match(.Yeet)) return try self.returnStatement();
        if (self.match(.Crashout)) return try self.tryStatement();
        if (self.match(.Griddy)) return try self.griddyLoop();

        return try self.expressionStatement();
    }

    fn blockStatement(self: *Parser) !Stmt {
        var statements = std.ArrayList(Stmt).init(self.allocator);

        while (!self.check(.Respectfully) and !self.isAtEnd()) {
            if (try self.declaration()) |stmt| {
                try statements.append(stmt);
            }
        }

        _ = try self.consume(.Respectfully, "Expected 'respectfully' after block");

        return Stmt{ .Block = .{ .statements = statements } };
    }

    fn ifStatement(self: *Parser) !Stmt {
        _ = try self.consume(.LeftParen, "Expected '(' after 'no shot'");
        const condition = try self.expression();
        _ = try self.consume(.RightParen, "Expected ')' after condition");

        const then_branch = try self.allocator.create(Stmt);
        then_branch.* = try self.statement();

        var else_branch: ?*Stmt = null;
        if (self.match(.AintNoWay)) {
            const else_stmt = try self.allocator.create(Stmt);
            else_stmt.* = try self.statement();
            else_branch = else_stmt;
        }

        return Stmt{
            .If = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        };
    }

    // helper methods
    fn match(self: *Parser, types: TokenType) bool {
        if (self.check(types)) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == .Eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) return self.advance();
        try self.reportError(self.peek(), message);
        return error.ParseError;
    }

    fn reportError(self: *Parser, token: Token, message: []const u8) !void {
        self.had_error = true;
        std.debug.print("Error at {s}: {s}\n", .{ token.lexeme, message });
    }

    fn synchronize(self: *Parser) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type == .Semicolon) return;

            switch (self.peek().type) {
                .Sigma, .Bussin, .Hitting, .Tweaking, .Vibing, .Mewing, .Lowkey, .Highkey, .Griddy, .FanumTax, .Sus, .Clean, .Peak, .Based, .Devious, .RealTalk, .NoShot, .Deadass, .Yeet, .Crashout => return,
                else => _ = self.advance(),
            }
        }
    }

    // expression parsing methods
    fn expression(self: *Parser) !*Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) !*Expr {
        const expr = try self.equality();

        if (self.match(.Equal)) {
            const equals = self.previous();
            const value = try self.assignment();

            if (expr.* == .Variable) {
                const name = expr.Variable.name;
                const node = try self.allocator.create(Expr);
                node.* = .{ .Assignment = .{ .name = name, .value = value } };
                return node;
            }

            try self.reportError(equals, "Invalid assignment target");
        }

        return expr;
    }

    fn equality(self: *Parser) !*Expr {
        var expr = try self.comparison();

        while (self.match(.EqualEqual) or self.match(.NotEqual)) {
            const operator = self.previous();
            const right = try self.comparison();
            const node = try self.allocator.create(Expr);
            node.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = node;
        }

        return expr;
    }

    fn comparison(self: *Parser) !*Expr {
        var expr = try self.term();

        while (self.match(.LessThan) or
            self.match(.LessEqual) or
            self.match(.GreaterThan) or
            self.match(.GreaterEqual))
        {
            const operator = self.previous();
            const right = try self.term();
            const node = try self.allocator.create(Expr);
            node.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = node;
        }

        return expr;
    }

    fn term(self: *Parser) !*Expr {
        var expr = try self.factor();

        while (self.match(.Plus) or self.match(.Minus)) {
            const operator = self.previous();
            const right = try self.factor();
            const node = try self.allocator.create(Expr);
            node.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = node;
        }

        return expr;
    }

    fn factor(self: *Parser) !*Expr {
        var expr = try self.unary();

        while (self.match(.Star) or self.match(.Slash)) {
            const operator = self.previous();
            const right = try self.unary();
            const node = try self.allocator.create(Expr);
            node.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = node;
        }

        return expr;
    }

    fn unary(self: *Parser) !*Expr {
        if (self.match(.Minus)) {
            const operator = self.previous();
            const right = try self.unary();
            const node = try self.allocator.create(Expr);
            node.* = .{ .Unary = .{ .operator = operator, .right = right } };
            return node;
        }

        return try self.primary();
    }

    fn primary(self: *Parser) !*Expr {
        const node = try self.allocator.create(Expr);

        if (self.match(.Number) or self.match(.String)) {
            node.* = .{ .Literal = .{ .value = self.previous() } };
            return node;
        }

        if (self.match(.Identifier)) {
            node.* = .{ .Variable = .{ .name = self.previous() } };

            // Check for array operations
            if (self.match(.Dot) and self.match(.FanumTax)) {
                return try self.fanumSlice(node);
            }
            return node;
        }

        if (self.match(.LeftBracket)) {
            var elements = std.ArrayList(*Expr).init(self.allocator);
            if (!self.check(.RightBracket)) {
                while (true) {
                    try elements.append(try self.expression());
                    if (!self.match(.Comma)) break;
                }
            }
            _ = try self.consume(.RightBracket, "Expected ']' after array elements");
            node.* = .{ .Array = .{ .elements = elements } };
            return node;
        }

        if (self.match(.LeftParen)) {
            const expr = try self.expression();
            _ = try self.consume(.RightParen, "Expected ')' after expression");
            node.* = .{ .Grouping = .{ .expression = expr } };
            return node;
        }

        try self.reportError(self.peek(), "Expected expression");
        return error.ParseError;
    }

    fn fanumSlice(self: *Parser, array: *Expr) !*Expr {
        _ = try self.consume(.LeftBracket, "Expected '[' after fanumtax");

        const start = try self.expression();
        _ = try self.consume(.Colon, "Expected ':' in slice");
        const end = try self.expression();

        _ = try self.consume(.RightBracket, "Expected ']' after slice");

        const node = try self.allocator.create(Expr);
        node.* = .{ .FanumSlice = .{
            .array = array,
            .start = start,
            .end = end,
        } };
        return node;
    }

    fn griddyLoop(self: *Parser) !Stmt {
        _ = try self.consume(.LeftParen, "Expected '(' after griddy");

        // Handle optional qualifier for iterator variable
        var qualifier = ValueQualifier.Mid;
        if (self.match(.Sus)) qualifier = .Sus else if (self.match(.Clean)) qualifier = .Clean else if (self.match(.Peak)) qualifier = .Peak else if (self.match(.Based)) qualifier = .Based else if (self.match(.Devious)) qualifier = .Devious;

        const iterator = try self.consume(.Identifier, "Expected iterator name");
        _ = try self.consume(.In, "Expected 'in' after iterator");
        const iterable = try self.expression();
        _ = try self.consume(.RightParen, "Expected ')' after griddy condition");

        const body = try self.statement();

        const node = try self.allocator.create(Stmt);
        node.* = body;

        return Stmt{ .Griddy = .{
            .iterator = iterator,
            .iterable = iterable,
            .body = node,
        } };
    }

    fn functionDeclaration(self: *Parser) !Stmt {
        var fn_type: FunctionType = .Sigma;
        switch (self.previous().type) {
            .Bussin => fn_type = .Bussin,
            .Hitting => fn_type = .Hitting,
            .Tweaking => fn_type = .Tweaking,
            .Vibing => fn_type = .Vibing,
            .Mewing => fn_type = .Mewing,
            else => {},
        }

        const name = try self.consume(.Identifier, "Expected function name");
        _ = try self.consume(.LeftParen, "Expected '(' after function name");

        var params = std.ArrayList(Token).init(self.allocator);
        if (!self.check(.RightParen)) {
            while (true) {
                if (params.items.len >= 255) {
                    try self.reportError(self.peek(), "Cannot have more than 255 parameters");
                }

                try params.append(try self.consume(.Identifier, "Expected parameter name"));

                if (!self.match(.Comma)) break;
            }
        }
        _ = try self.consume(.RightParen, "Expected ')' after parameters");

        _ = try self.consume(.RealTalk, "Expected 'real_talk' before function body");
        const body = try self.blockStatement();

        const body_node = try self.allocator.create(Stmt);
        body_node.* = body;

        return Stmt{ .Function = .{
            .type = fn_type,
            .name = name,
            .params = params,
            .body = body_node,
        } };
    }

    fn tryStatement(self: *Parser) !Stmt {
        _ = try self.consume(.RealTalk, "Expected 'real_talk' after 'crashout'");
        const try_body = try self.blockStatement();

        _ = try self.consume(.AintNoWay, "Expected 'ain't no way' after try block");
        const error_var = try self.consume(.Identifier, "Expected error variable name");
        _ = try self.consume(.RealTalk, "Expected 'real_talk' after error variable");
        const catch_body = try self.blockStatement();

        const catch_stmt = try self.allocator.create(Stmt);
        catch_stmt.* = Stmt{ .Catch = .{
            .error_var = error_var,
            .body = try self.allocator.create(Stmt),
        } };
        catch_stmt.*.Catch.body.* = catch_body;

        const try_stmt = try self.allocator.create(Stmt);
        try_stmt.* = try_body;

        return Stmt{ .Try = .{
            .body = try_stmt,
            .catch_clause = catch_stmt,
        } };
    }

    fn scopeDeclaration(self: *Parser) !Stmt {
        const scope_type: ScopeType = if (self.previous().type == .Lowkey)
            .Lowkey
        else
            .Highkey;

        _ = try self.consume(.RealTalk, "Expected 'real talk' after scope declaration");

        var declarations = std.ArrayList(Stmt).init(self.allocator);
        while (!self.check(.Respectfully) and !self.isAtEnd()) {
            if (try self.declaration()) |stmt| {
                try declarations.append(stmt);
            }
        }

        _ = try self.consume(.Respectfully, "Expected 'respectfully' after scope block");

        return Stmt{ .Scope = .{
            .type = scope_type,
            .declarations = declarations,
        } };
    }
};
