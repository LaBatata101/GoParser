package lexer_test

import (
	"parser/lexer"
	"testing"

	"github.com/LaBatata101/goinsta/assert"
)

func prettyPrint(tokens []lexer.Token, lexErrors []lexer.LexError, source []byte) []string {
	var strs []string
	for _, token := range tokens {
		strs = append(strs, token.String()+" "+string(source[token.Pos.Start:token.Pos.End]))
	}

	if len(lexErrors) > 0 {
		strs = append(strs, "--- ERRORS ---")
	}
	for _, err := range lexErrors {
		strs = append(strs, err.String())
	}
	return strs
}

func TestLexComments(t *testing.T) {
	src := []byte(`// This is a one line comment!
/*
    This
    is
    Multiline
    Comment
    !
*/
`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexIdentifiers(t *testing.T) {
	src := []byte(`a
_x9
ThisVariableIsExported
αβ
`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexKeywords(t *testing.T) {
	src := []byte(`break
default
func
interface
select
case
defer
go
map
struct
chan
else
goto
package
switch
const
fallthrough
if
range
type
continue
for
import
return
var`)
	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexOperators(t *testing.T) {
	src := []byte(`+    &     +=    &=     &&    ==    !=
-    |     -=    |=     ||    <     <=     ~
*    ^     *=    ^=     <-    >     >=     &^=
/    <<    /=    <<=    ++    =     :=     &^
%    >>    %=    >>=    --    !`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexPunctuation(t *testing.T) {
	src := []byte(`() [] {} , ; ... . :`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexIntegerLit(t *testing.T) {
	src := []byte(`42
4_2
0600
0_600
0o600
0O600
0xBadFace
0xBad_Face
0x_67_7a_2f_cc_40_c6
170141183460469231731687303715884105727
170_141183_460469_231731_687303_715884_105727`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexFloatLit(t *testing.T) {
	src := []byte(`0.
72.40
072.40
2.71828
1.e+0
6.67428e-11
1E6
.25
.12345E+5
1_5.
0.15e+0_2
0x1p-2
0x2.p10
0x1.Fp+0
0X.8p-0
0X_1FFFP-16`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexImaginaryLit(t *testing.T) {
	src := []byte(`0i
0123i
0o123i
0xabci
0.i
2.71828i
1.e+0i
6.67428e-11i
1E6i
.25i
.12345E+5i
0x1p-2i`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexRuneLit(t *testing.T) {
	src := []byte(`'a'
'ä'
'本'
'\t'
'\000'
'\007'
'\377'
'\x07'
'\xff'
'\u12e4'
'\U00101234'`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}

func TestLexStringLit(t *testing.T) {
	src := []byte(`
"\n"
"\""
"Hello, world!\n"
"日本語"
"\u65e5本\U00008a9e"
"\xff\u00FF"
`)

	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	assert.Snapshot(t, prettyPrint(tokens, err, src))
}
