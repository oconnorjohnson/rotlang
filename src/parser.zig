const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    allocator: std.mem.Allocator,
    had_error: bool,

    // ast node types
    pub const ExprType = enum {
        Binary,
        Grouping,
        Literal,
        Unary,
        Variable,
        Assignment,
    };

    pub const StmtType = enum {
        Expression,
        Declaration,
        Block,
        If,
        While,
        Return,
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
    };

    pub const Stmt = union(StmtType) {
        Expression: struct {
            expr: *Expr,
        },
        Declaration: struct {
            name: Token,
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
        if (self.match(.Sus) or self.match(.Clean) or self.match(.Peak)) {
            return try self.varDeclaration();
        }

        return try self.statement();
    }

    fn varDeclaration(self: *Parser) !Stmt {
        const name = self.previous();

        var initializer: ?*Expr = null;
        if (self.match(.Equal)) {
            initializer = try self.expression();
        }

        _ = try self.consume(.Semicolon, "Expected ';' after variable declaration");

        return Stmt{
            .DEeclaration = .{
                .name = name,
                .initializer = initializer,
            },
        };
    }

    fn statement(self: *Parser) !Stmt {
        if (self.match(.RealTalk)) return try self.blockStatement();
        if (self.match(.NoShot)) return try self.ifStatement();
        if (self.match(.Deadass)) return try self.whileStatement();
        if (self.match(.Yeet)) return try self.returnStatement();

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
                .RealTalk, .NoShot, .Deadass, .Yeet, .Sus, .Clean, .Peak => return,
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
};
