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

    pub fn scanTokens(self: *Lexer) !void {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        try self.tokens.append(Token.init(.Eof, "", self.line));
    }

    fn scanToken(self: *Lexer) !void {
        const c = self.advance();
        switch (c) {
            // single char tokens
            '(' => try self.addToken(.LeftParen),
            ')' => try self.addToken(.RightParen),
            '{' => try self.addToken(.LeftBrace),
            '}' => try self.addToken(.RightBrace),
            ',' => try self.addToken(.Comma),
            '.' => try self.addToken(.Dot),
            '-' => try self.addToken(.Minus),
            '+' => try self.addToken(.Plus),
            ';' => try self.addToken(.SemiColon),
            '*' => try self.addToken(.Star),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) : (self.advance()) {}
                } else {
                    try self.addToken(.Slash);
                }
            },
            '=' => try self.addToken(.Equal),
            '"' => try self.string(),

            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,

            else => {
                if (self.isDigit(c)) {
                    try self.number();
                } else if (self.isAlpha(c)) {
                    try self.identifier();
                } else {
                    std.debug.print("Error at line {d}: Unexpected character '{c}'\n", .{ self.line, c });
                    try self.addToken(.Error);
                }
            },
        }
    }

    fn identifier(self: *Lexer) !void {
        while (self.isAlphaNumeric(self.peek())) : (self.advance()) {}

        const text = self.source[self.start..self.current];
        const token_type = self.getKeywordType(text);
        try self.addToken(token_type);
    }

    fn getKeywordType(text: []const u8) TokenType {
        const keywords = std.ComptimeStringMap(TokenType, .{
            // Popular terms
            .{ "skibidi", .Skibidi },
            .{ "rizzler", .Rizzler },
            .{ "gyat", .Gyat },
            .{ "bussin", .Bussin },
            .{ "bruh", .Bruh },
            .{ "npc", .Npc },
            .{ "griddy", .Griddy },
            .{ "clutch", .Clutch },
            .{ "fanumtax", .FanumTax },
            .{ "backrooms", .Backrooms },
            .{ "sigma", .Sigma },
            .{ "beta", .Beta },
            .{ "alpha", .Alpha },
            .{ "based", .Based },
            .{ "nocap", .NoCap },
            .{ "frfr", .FrFr },

            // Actions/Functions
            .{ "crashout", .Crashout },
            .{ "yeet", .Yeet },
            .{ "slay", .Slay },
            .{ "mewing", .Mewing },
            .{ "cranking", .Cranking },
            .{ "vibing", .Vibing },
            .{ "tweaking", .Tweaking },
            .{ "hitting", .Hitting },
            .{ "sheesh", .Sheesh },
            .{ "ong", .Ong },

            // State/Variable Related
            .{ "sus", .Sus },
            .{ "mid", .Mid },
            .{ "clean", .Clean },
            .{ "peak", .Peak },
            .{ "devious", .Devious },
            .{ "cursed", .Cursed },
            .{ "unhinged", .Unhinged },
            .{ "valid", .Valid },
            .{ "ratiod", .Ratiod },

            // Control Flow
            .{ "noshot", .NoShot },
            .{ "deadass", .Deadass },
            .{ "aintnoway", .AintNoWay },
            .{ "realtalk", .RealTalk },
            .{ "respectfully", .Respectfully },
            .{ "lowkey", .Lowkey },
            .{ "highkey", .Highkey },
        });

        return keywords.get(text) orelse .Identifier;
    }

    fn number(self: *Lexer) !void {
        while (self.isDigit(self.peek())) : (self.advance()) {}

        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            self.advance();

            while (self.isDigit(self.peek())) : (self.advance()) {}

            if (self.peek() == 'e' or self.peek() == 'E') {
                const next = self.peekNext();
                if (self.isDigit(next) or next == '+' or next == '-') {
                    self.advance(); // consume 'e'
                    if (next == '+' or next == '-') self.advance();
                    while (self.isDigit(self.peek())) : (self.advance()) {}
                }
            }
        }

        try self.addToken(.Number);
    }

    fn string(self: *Lexer) !void {
        var had_escape = false;
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                '"' => {
                    if (!had_escape) {
                        self.advance();
                        try self.addToken(.String);
                        return;
                    }
                    had_escape = false;
                },
                '\\' => {
                    had_escape = !had_escape;
                    self.advance();
                },
                '\n' => {
                    self.line += 1;
                    had_escape = false;
                    self.advance();
                },
                else => {
                    had_escape = false;
                    self.advance();
                },
            }
        }

        std.debug.print("Error at line {d}: Unterminated string\n", .{self.line});
        try self.addToken(.Error);
    }

    fn addToken(self: *Lexer, token_type: TokenType) !void {
        const lexeme = self.source[self.start..self.current];
        try self.tokens.append(Token.init(token_type, lexeme, self.line));
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }
};
