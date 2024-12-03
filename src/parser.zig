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
};
