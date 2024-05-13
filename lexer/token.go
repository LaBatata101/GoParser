package lexer

import "fmt"

type TokenKind int32

const (
	Identifier TokenKind = iota

	String
	RawString
	Rune

	Comment

	// Punctuation

	// Token value for `(`
	Lparen
	// Token value for `)`
	Rparen
	// Token value for `[`
	Lsqb
	// Token value for `]`
	Rsqb
	// Token value for `{`
	Lbrace
	// Token value for `}`
	Rbrace
	// Token value for `;`
	SemiColon
	// Token value for `:`
	Colon
	// Token value for `,`
	Comma
	// Token value for `.`
	Dot

	// Operators

	// Token value for `+`
	Plus
	// Token value for `-`
	Minus
	// Token value for `*`
	Star
	// Token value for `/`
	Slash
	// Token value for `%`
	Percent
	// Token value for `&`
	Amper
	// Token value for `|`
	Vbar
	// Token value for `^`
	Circumflex
	// Token value for `<<`
	Lshift
	// Token value for `>>`
	Rshift
	// Token value for `&^`
	Bitclear
	// Token value for `+=`
	PlusEq
	// Token value for `-=`
	MinusEq
	// Token value for `*=`
	StarEq
	// Token value for `/=`
	SlashEq
	// Token value for `%=`
	PercentEq
	// Token value for `&=`
	AmperEq
	// Token value for `|=`
	VbarEq
	// Token value for `^=`
	CircumflexEq
	// Token value for `<<=`
	LshiftEq
	// Token value for `>>=`
	RshiftEq
	// Token value for `&^=`
	BitclearEq
	// Token value for `&&`
	And
	// Token value for `||`
	Or
	// Token value for `<-`
	Larrow
	// Token value for `++`
	PlusPlus
	// Token value for `--`
	MinusMinus
	// Token value for `==`
	EqEqual
	// Token value for `<`
	LessThan
	// Token value for `>`
	GreaterThan
	// Token value for `=`
	Equal
	// Token value for `!`
	Excla
	// Token value for `~`
	Tilde
	// Token value for `!=`
	NotEq
	// Token value for `<=`
	LessThanEq
	// Token value for `>=`
	GreaterThanEq
	// Token value for `:=`
	ColonEq
	// Token value for `...`
	Ellipsis

	// Numbers

	// 1, 123, 01, 1_000_000
	DecimalLit
	// 0xff, 0xFFFF, 0x_67_7a, 0X20, 0X2Ff
	HexLit
	// 0b01, 0B101, 0b1_1_1
	BinaryLit
	// 0o12347, 0O1_1_5
	OctalLit

	Float
	HexFloat

	ImaginaryLit

	// Keywords

	KwBreak
	KwCase
	KwChan
	KwConst
	KwContinue
	KwDefault
	KwDefer
	KwElse
	KwFallthrough
	KwFor
	KwFunc
	KwGo
	KwGoto
	KwIf
	KwImport
	KwInterface
	KwMap
	KwPackage
	KwRange
	KwReturn
	KwSelect
	KwStruct
	KwSwitch
	KwType
	KwVar

	Illegal
	Newline
	Eof
)

var tokenKindStrTable = [...]string{
	"Identifier",
	"String",
	"RawString",
	"Rune",
	"Comment",
	"Lparen",
	"Rparen",
	"Lsqb",
	"Rsqb",
	"Lbrace",
	"Rbrace",
	"SemiColon",
	"Colon",
	"Comma",
	"Dot",
	"Plus",
	"Minus",
	"Star",
	"Slash",
	"Percent",
	"Amper",
	"Vbar",
	"Circumflex",
	"Lshift",
	"Rshift",
	"Bitclear",
	"PlusEq",
	"MinusEq",
	"StarEq",
	"SlashEq",
	"PercentEq",
	"AmperEq",
	"VbarEq",
	"CircumflexEq",
	"LshiftEq",
	"RshiftEq",
	"BitclearEq",
	"And",
	"Or",
	"Larrow",
	"PlusPlus",
	"MinusMinus",
	"EqEqual",
	"LessThan",
	"GreaterThan",
	"Equal",
	"Excla",
	"Tilde",
	"NotEq",
	"LessThanEq",
	"GreaterThanEq",
	"ColonEq",
	"Ellipsis",
	"Decimal",
	"HexDecimal",
	"Binary",
	"Octal",
	"Float",
	"HexFloat",
	"Imaginary",
	"KwBreak",
	"KwCase",
	"KwChan",
	"KwConst",
	"KwContinue",
	"KwDefault",
	"KwDefer",
	"KwElse",
	"KwFallthrough",
	"KwFor",
	"KwFunc",
	"KwGo",
	"KwGoto",
	"KwIf",
	"KwImport",
	"KwInterface",
	"KwMap",
	"KwPackage",
	"KwRange",
	"KwReturn",
	"KwSelect",
	"KwStruct",
	"KwSwitch",
	"KwType",
	"KwVar",
	"Illegal",
	"Newline",
	"EOF",
}

func (t TokenKind) String() string {
	return tokenKindStrTable[t]
}

type Position struct {
	Start int
	End   int
}

type Token struct {
	Kind TokenKind
	Pos  Position
}

func (t Token) String() string {
	return fmt.Sprintf("%s@%s", t.Kind, t.Pos.String())
}

func (p Position) String() string {
	return fmt.Sprintf("%d..%d", p.Start, p.End)
}
