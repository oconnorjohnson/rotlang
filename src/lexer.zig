const std = @import("std");

pub const TokenType: type = enum {
    // populatr terms
    Skibidi,
    Rizzler,
    Gyat,
    Bussin,
    Bruh,
    Npc,
    Griddy,
    Clutch,
    FanumTax,
    Backrooms,
    Sigma,
    Beta,
    Alpha,
    NoCap,
    Based,
    FrFr,

    // actions
    Crashout, // errors
    Yeet, // return
    Slay, // success
    Mewing,
    Cranking,
    Vibing,
    Tweaking,
    Hitting,
    Sheesh,
    Ong,

    // state
    Sus,
    Mid,
    Clean,
    Peak,
    Devious,
    Cursed,
    Unhinged,
    Valid,
    Ratiod,

    // control flow
    NoShot,
    Deadass,
    AintNoWay,
    RealTalk,
    Respectfully,
    Lowkey,
    Highkey,

    // basic syntax
    Number,
    String,
    Identifier,
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,

    Eof, // end of file
    Error, // errors
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,

    pub fn init(token_type: TokenType, lexeme: []const u8, line: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .line = line,
        };
    }
};

pub const Lexer = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit();
    }
};
