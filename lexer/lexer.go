package lexer

import (
	"bufio"
	"bytes"
	"fmt"
	"unicode"
	"unicode/utf8"
)

type escapedCharCtx int

const BOM = 0xFEFF

const (
	StringCtx escapedCharCtx = iota
	RuneCtx
)

type (
	LexError struct {
		Msg string
		Pos Position
	}

	Lexer struct {
		reader        *bufio.Reader
		data          []byte
		lexErrors     []LexError
		pos           int
		startTokenPos int
		lastToken     TokenKind
	}
)

func NewLexer(data []byte) Lexer {
	return Lexer{
		reader:    bufio.NewReader(bytes.NewReader(data)),
		data:      data,
		lastToken: -1,
	}
}

func (l LexError) String() string {
	return fmt.Sprintf("ERROR: %s %s", l.Msg, l.Pos.String())
}

func (l *Lexer) Errors() []LexError {
	return l.lexErrors
}

func (l *Lexer) isEOF() bool {
	return l.pos >= len(l.data)
}

// Consumes the current character.
// Returns `unicode.ReplacementChar (U+FFFD)` if the UTF-8 encoding is invalid.
// TODO: handle unexpected BOMs 0xFEFF
func (l *Lexer) bump() rune {
	var size int
	var char rune

	// ASCII fast-path
	if l.data[l.pos] < utf8.RuneSelf {
		size = 1
		char = rune(l.data[l.pos])
		l.reader.Discard(size)
	} else {
		r, s, err := l.reader.ReadRune()
		if err != nil {
			l.addLexError("invalid UTF-8 encoding", Position{Start: l.pos, End: l.pos + size})
		}
		char = r
		size = s
	}

	if !l.isEOF() {
		l.pos += size
	}

	return char
}

// Returns the current character of the stream without consuming it.
// If the lexer reached EOF, returns -1.
func (l *Lexer) first() rune {
	if l.isEOF() {
		return -1
	}

	// ASCII fast-path
	if l.data[l.pos] < utf8.RuneSelf {
		return rune(l.data[l.pos])
	}

	var char rune
	// Here we handle peeking multi-bytes UTF-8 characters.
	// We skip reading one byte because the ASCII case is handled above.
	// based on: https://gist.github.com/clipperhouse/add34daff16ac22449d21d4c0afc3fce#file-runereader-go-L21
	for peekBytes := 2; peekBytes <= 4; peekBytes++ { // unicode rune can be up to 4 bytes
		b, err := l.reader.Peek(peekBytes)
		if err == nil {
			r, _ := utf8.DecodeRune(b)
			char = r
		}
		// Otherwise, we ignore Peek errors and try the next smallest number of bytes
	}

	return char
}

// Returns the next character of the stream without consuming it.
// If the lexer reached EOF, returns -1.
func (l *Lexer) second() rune {
	if l.pos+1 >= len(l.data) {
		return -1
	}
	return rune(l.data[l.pos+1])
}

func (l *Lexer) eatChar(char rune) bool {
	current := l.first()
	if current == char {
		l.bump()
		return true
	}
	return false
}

// Consume characters while `predicate` is true. Stop consuming when EOF is reached.
// Return whether any character was consumed or not.
func (l *Lexer) eatWhile(predicate func(rune) bool) bool {
	start := l.pos
	for !l.isEOF() && predicate(l.first()) {
		l.bump()
	}
	return start != l.pos
}

func (l *Lexer) eatIf(predicate func(rune) bool) bool {
	if !l.isEOF() && predicate(l.first()) {
		l.bump()
		return true
	}
	return false
}

func (l *Lexer) lexKeywordOrIdentifier() Token {
	l.tokenStart()
	l.eatWhile(func(char rune) bool {
		switch {
		case unicode.IsLetter(char) || char == '_' || unicode.IsDigit(char): // ok
		case char >= utf8.RuneSelf:
			l.addLexError(fmt.Sprintf("invalid character %#U in identifier", char), l.tokenPos())
		default:
			return false
		}
		return true
	})

	// TODO: Use a perfect hash table
	var kind TokenKind
	switch l.tokenText() {
	case "break":
		kind = KwBreak
	case "case":
		kind = KwCase
	case "chan":
		kind = KwChan
	case "const":
		kind = KwConst
	case "continue":
		kind = KwContinue
	case "default":
		kind = KwDefault
	case "defer":
		kind = KwDefer
	case "else":
		kind = KwElse
	case "fallthrough":
		kind = KwFallthrough
	case "for":
		kind = KwFor
	case "func":
		kind = KwFunc
	case "go":
		kind = KwGo
	case "goto":
		kind = KwGoto
	case "if":
		kind = KwIf
	case "import":
		kind = KwImport
	case "interface":
		kind = KwInterface
	case "map":
		kind = KwMap
	case "package":
		kind = KwPackage
	case "range":
		kind = KwRange
	case "return":
		kind = KwReturn
	case "select":
		kind = KwSelect
	case "struct":
		kind = KwStruct
	case "switch":
		kind = KwSwitch
	case "type":
		kind = KwType
	case "var":
		kind = KwVar
	default:
		kind = Identifier
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) tokenText() string {
	pos := l.tokenPos()
	return string(l.data[pos.Start:pos.End])
}

func (l *Lexer) tokenStart() {
	l.startTokenPos = l.pos
}

func (l *Lexer) tokenPos() Position {
	return Position{Start: l.startTokenPos, End: l.pos}
}

func (l *Lexer) skipWhiteSpace() {
	l.eatWhile(func(char rune) bool { return char == ' ' || char == '\t' || char == '\n' || char == '\r' })
}

func (l *Lexer) createSingleCharToken(kind TokenKind) Token {
	l.tokenStart()
	l.bump()
	return Token{kind, l.tokenPos()}
}

func (l *Lexer) createSingleOrDoubleCharToken(singleCharKind TokenKind,
	secondChar rune,
	doubleCharKind TokenKind) Token {
	l.tokenStart()
	l.bump()
	if l.eatChar(secondChar) {
		return Token{doubleCharKind, l.tokenPos()}
	} else {
		return Token{singleCharKind, l.tokenPos()}
	}
}

func (l *Lexer) addIllegalTokenError(kind *TokenKind, msg string) {
	*kind = Illegal
	l.addLexError(msg, l.tokenPos())
}

func (l *Lexer) addLexError(msg string, pos Position) {
	l.lexErrors = append(l.lexErrors, LexError{msg, pos})
}

func (l *Lexer) lexLineComment() Token {
	l.tokenStart()
	l.eatWhile(func(char rune) bool { return char != '\n' && char != '\r' })

	return Token{Comment, l.tokenPos()}
}

func (l *Lexer) lexGeneralComment() Token {
	kind := Comment

	l.tokenStart()
	l.eatChar('/')
	l.eatChar('*')

	for !l.isEOF() && l.first() != '*' && l.second() != '/' {
		l.bump()
	}

	if !l.eatChar('*') || !l.eatChar('/') {
		l.addIllegalTokenError(&kind, "Comment not terminated")
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) lexString() Token {
	kind := String

	l.tokenStart()
	l.eatChar('"')

	for !l.isEOF() && l.first() != '"' && l.first() != '\n' && l.first() != '\r' {
		if l.first() == '\\' {
			l.handleEscapedChar(StringCtx)
		} else {
			l.bump()
		}
	}

	if !l.eatChar('"') {
		l.addIllegalTokenError(&kind, "String literal not terminated")
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) lexRawString() Token {
	kind := RawString

	l.tokenStart()
	l.eatChar('`')
	for !l.isEOF() && l.first() != '`' {
		l.bump()
	}

	if !l.eatChar('`') {
		l.addIllegalTokenError(&kind, "Raw string literal not terminated")
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) lexRune() Token {
	kind := Rune

	l.tokenStart()
	l.eatChar('\'')

	rune_count := 0
	for !l.isEOF() && l.first() != '\'' && l.first() != '\n' && l.first() != '\r' {
		if l.first() == '\\' {
			l.handleEscapedChar(RuneCtx)
		} else {
			l.bump()
		}
		rune_count += 1
	}

	if !l.eatChar('\'') {
		l.addIllegalTokenError(&kind, "Rune literal not terminated")
	}
	// handles the case where we have an empty rune or more than one character in the rune
	if rune_count == 0 || rune_count > 1 {
		l.addIllegalTokenError(&kind, "Illegal rune literal")
	}

	return Token{kind, l.tokenPos()}
}

// TODO: should we make the Rune or String an Illegal token if the escape is incorrect?
func (l *Lexer) handleEscapedChar(ctx escapedCharCtx) {
	escape_pos := l.pos
	l.eatChar('\\')
	char := l.first()

	var n int
	var base, max uint32
	switch char {
	case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\':
		l.bump()
		return
	case '0', '1', '2', '3', '4', '5', '6', '7':
		n, base, max = 3, 8, 255
	case 'x':
		l.bump()
		n, base, max = 2, 16, 255
	case 'u':
		l.bump()
		n, base, max = 4, 16, unicode.MaxRune
	case 'U':
		l.bump()
		n, base, max = 8, 16, unicode.MaxRune
	case '\'', '"':
		l.bump()
		if ctx == RuneCtx && char == '"' || ctx == StringCtx && char == '\'' {
			l.addLexError("Unknow escape sequence", Position{escape_pos, l.pos})
		}
	default:
		l.bump()
		l.addLexError("Unknow escape sequence", Position{escape_pos, l.pos})
	}

	var x uint32
	for n > 0 {
		d := uint32(digitVal(l.first()))
		if d >= base {
			l.addLexError(fmt.Sprintf("Illegal character %#U in escape sequence", l.first()),
				Position{escape_pos, l.pos})
		}
		x = x*base + d
		l.bump()
		n--
	}

	if x > max || 0xD800 <= x && x < 0xE000 {
		l.addLexError("Escape sequence is invalid Unicode code point", Position{escape_pos, l.pos})
	}
}

// Lex the fractional part of the float
func (l *Lexer) lexFloatNumber() Token {
	kind := Float

	if l.eatChar('_') {
		l.addIllegalTokenError(&kind, "`_` must separate successive digits")
	}

	l.consumeDecimalNumber()

	if l.eatChar('e') || l.eatChar('E') {
		switch l.first() {
		case '+', '-':
			l.bump()
		}
		if !l.consumeDecimalNumber() {
			l.addLexError("exponent has no digits", l.tokenPos())
		}
	}

	if l.eatChar('_') {
		l.addIllegalTokenError(&kind, "`_` must separate successive digits")
	}

	if l.eatChar('i') {
		kind = ImaginaryLit
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) consumeDecimalNumber() bool {
	return l.eatWhile(func(char rune) bool {
		return isDecimalDigit(char) || (char == '_' && isDecimalDigit(l.second()))
	})
}

func (l *Lexer) lexNumber() Token {
	kind := DecimalLit

	l.tokenStart()
	l.bump() // consume the first number digit

	switch {
	case l.eatChar('x') || l.eatChar('X'):
		kind = HexLit
		consumed := l.eatWhile(func(char rune) bool {
			return isHexDigit(char) || (char == '_' && isHexDigit(l.second()))
		})
		switch l.first() {
		case '.':
			l.bump()
			return l.lexHexFloat()
		case 'p', 'P':
			return l.lexHexFloat()
		}
		if !consumed {
			l.addLexError("hexadecimal literal has no digits", l.tokenPos())
		}
	case l.eatChar('o') || l.eatChar('O'):
		kind = OctalLit
		consumed := l.eatWhile(func(char rune) bool {
			return isOctalDigit(char) || (char == '_' && isDecimalDigit(l.second()))
		})
		if !consumed {
			l.addLexError("octal literal has no digits", l.tokenPos())
		}
	case l.eatChar('b') || l.eatChar('B'):
		kind = BinaryLit
		consumed := l.eatWhile(func(char rune) bool {
			return char == '0' || char == '1' || (char == '_' && isDecimalDigit(l.second()))
		})
		if !consumed {
			l.addLexError("binary literal has no digits", l.tokenPos())
		}
	default: // lex a decimal number or float number
		l.consumeDecimalNumber()

		// lex the fractional part of the float
		switch l.first() {
		case '.':
			l.bump()
			return l.lexFloatNumber()
		case 'e', 'E':
			return l.lexFloatNumber()
		}
	}

	if l.eatChar('_') {
		l.addIllegalTokenError(&kind, "`_` must separate successive digits")
	}

	if l.eatChar('i') {
		kind = ImaginaryLit
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) lexHexFloat() Token {
	kind := HexFloat
	consumed := l.eatWhile(func(char rune) bool {
		return isHexDigit(char) || (char == '_' && isHexDigit(l.second()))
	})

	if l.eatChar('p') || l.eatChar('P') {
		consumed = true
		switch l.first() {
		case '+', '-':
			l.bump()
		}
		if !l.consumeDecimalNumber() {
			l.addLexError("exponent has no digits", l.tokenPos())
		}
	} else {
		l.addLexError("hexadecimal mantissa requires a 'p' exponent", l.tokenPos())
	}

	if !consumed {
		l.addLexError("hexadecimal literal has no digits", l.tokenPos())
	}

	if l.eatChar('i') {
		kind = ImaginaryLit
	}

	return Token{kind, l.tokenPos()}
}

func (l *Lexer) insertSemicolon() bool {
	switch l.lastToken {
	case Identifier, KwBreak, KwContinue, KwFallthrough, KwReturn, DecimalLit, BinaryLit, OctalLit,
		HexLit, ImaginaryLit, HexFloat, Float, Rune, String, PlusPlus, Minus, Rparen, Rbrace, Rsqb:
		return true
	default:
		return false
	}
}

func (l *Lexer) Lex() ([]Token, []LexError) {
	var tokens []Token

	for !l.isEOF() {
		char := l.first()
		switch char {
		case ' ', '\t':
			l.skipWhiteSpace()
		case '\n', '\r':
			if l.insertSemicolon() {
				// TODO: check if the range is correct
				tokens = append(tokens, Token{SemiColon, Position{l.pos, l.pos + 1}})
			}
			if l.bump() == '\r' {
				l.eatChar('\n')
			}
		case '"':
			tokens = append(tokens, l.lexString())
		case '`':
			tokens = append(tokens, l.lexRawString())
		case '\'':
			tokens = append(tokens, l.lexRune())
		case '{':
			tokens = append(tokens, l.createSingleCharToken(Lbrace))
		case '}':
			tokens = append(tokens, l.createSingleCharToken(Rbrace))
		case '[':
			tokens = append(tokens, l.createSingleCharToken(Lsqb))
		case ']':
			tokens = append(tokens, l.createSingleCharToken(Rsqb))
		case '(':
			tokens = append(tokens, l.createSingleCharToken(Lparen))
		case ')':
			tokens = append(tokens, l.createSingleCharToken(Rparen))
		case ';':
			tokens = append(tokens, l.createSingleCharToken(SemiColon))
		case ',':
			tokens = append(tokens, l.createSingleCharToken(Comma))
		case '~':
			tokens = append(tokens, l.createSingleCharToken(Tilde))
		case '=':
			tokens = append(tokens, l.createSingleOrDoubleCharToken(Equal, '=', EqEqual))
		case ':':
			tokens = append(tokens, l.createSingleOrDoubleCharToken(Colon, '=', ColonEq))
		case '*':
			tokens = append(tokens, l.createSingleOrDoubleCharToken(Star, '=', StarEq))
		case '%':
			tokens = append(tokens, l.createSingleOrDoubleCharToken(Percent, '=', PercentEq))
		case '^':
			tokens = append(tokens, l.createSingleOrDoubleCharToken(Circumflex, '=', CircumflexEq))
		case '!':
			tokens = append(tokens, l.createSingleOrDoubleCharToken(Excla, '=', NotEq))
		case '+':
			l.tokenStart()
			l.bump()
			kind := Plus
			switch l.first() {
			case '+':
				l.bump()
				kind = PlusPlus
			case '=':
				l.bump()
				kind = PlusEq
			}
			tokens = append(tokens, Token{kind, l.tokenPos()})
		case '-':
			l.tokenStart()
			l.bump()
			kind := Minus
			switch l.first() {
			case '-':
				l.bump()
				kind = MinusMinus
			case '=':
				l.bump()
				kind = MinusEq
			}
			tokens = append(tokens, Token{kind, l.tokenPos()})
		case '/':
			switch l.second() {
			case '/':
				tokens = append(tokens, l.lexLineComment())
			case '*':
				tokens = append(tokens, l.lexGeneralComment())
			default:
				tokens = append(tokens, l.createSingleOrDoubleCharToken(Slash, '=', SlashEq))
			}
		case '.':
			l.tokenStart()
			l.bump()

			if l.first() == '.' && !l.isEOF() && l.second() == '.' {
				l.bump()
				l.bump()
				tokens = append(tokens, Token{Ellipsis, l.tokenPos()})
			} else if isDecimalDigit(l.first()) { // lex float number of the form '.05'
				tokens = append(tokens, l.lexFloatNumber())
			} else {
				tokens = append(tokens, Token{Dot, l.tokenPos()})
			}
		case '<':
			l.tokenStart()
			l.bump()
			var kind TokenKind
			switch l.first() {
			case '=':
				l.bump()
				kind = LessThanEq
			case '<':
				l.bump()
				if l.eatChar('=') {
					kind = LshiftEq
				} else {
					kind = Lshift
				}
			case '-':
				l.bump()
				kind = Larrow
			default:
				kind = LessThan
			}
			tokens = append(tokens, Token{kind, l.tokenPos()})
		case '>':
			l.tokenStart()
			l.bump()
			var kind TokenKind
			switch l.first() {
			case '=':
				l.bump()
				kind = GreaterThanEq
			case '>':
				l.bump()
				if l.eatChar('=') {
					kind = RshiftEq
				} else {
					kind = Rshift
				}
			default:
				kind = GreaterThan
			}
			tokens = append(tokens, Token{kind, l.tokenPos()})
		case '|':
			l.tokenStart()
			l.bump()
			var kind TokenKind
			switch l.first() {
			case '=':
				l.bump()
				kind = VbarEq
			case '|':
				l.bump()
				kind = Or
			default:
				kind = Vbar
			}
			tokens = append(tokens, Token{kind, l.tokenPos()})
		case '&':
			l.tokenStart()
			l.bump()
			var kind TokenKind
			switch l.first() {
			case '=':
				l.bump()
				kind = AmperEq
			case '^':
				l.bump()
				if l.eatChar('=') {
					kind = BitclearEq
				} else {
					kind = Bitclear
				}
			case '&':
				l.bump()
				kind = And
			default:
				kind = Amper
			}
			tokens = append(tokens, Token{kind, l.tokenPos()})
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			tokens = append(tokens, l.lexNumber())
		default:
			if isAsciiAlphabetic(char) || unicode.IsLetter(char) || char == '_' || char >= utf8.RuneSelf {
				tokens = append(tokens, l.lexKeywordOrIdentifier())
			} else {
				l.bump()
			}
		}

		if len(tokens) > 0 {
			l.lastToken = tokens[len(tokens)-1].Kind
		}
	}
	// Insert a SemiColon token before Eof
	if l.insertSemicolon() {
		// TODO: check if the range is correct
		tokens = append(tokens, Token{SemiColon, Position{l.pos, l.pos}})
	}

	return append(tokens, Token{Eof, Position{l.pos, l.pos}}), l.lexErrors
}

func isAsciiAlphabetic(char rune) bool {
	return char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z'
}

func isDecimalDigit(char rune) bool { return char >= '0' && char <= '9' }

func isHexDigit(char rune) bool {
	return isDecimalDigit(char) || char >= 'a' && char <= 'f' || char >= 'A' && char <= 'F'
}

func isOctalDigit(char rune) bool {
	return char >= '0' && char <= '7'
}

func digitVal(ch rune) int {
	switch {
	case '0' <= ch && ch <= '9':
		return int(ch - '0')
	case 'a' <= lower(ch) && lower(ch) <= 'f':
		return int(lower(ch) - 'a' + 10)
	}
	return 16 // larger than any legal digit val
}

func lower(ch rune) rune { return ('a' - 'A') | ch } // returns lower-case ch iff ch is ASCII letter
