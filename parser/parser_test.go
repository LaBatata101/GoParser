package parser_test

import (
	"go/ast"

	"parser/lexer"
	"parser/parser"
	"testing"

	"github.com/LaBatata101/goinsta/assert"
)

func parse(src []byte) ast.File {
	lexer := lexer.NewLexer(src)
	tokens, err := lexer.Lex()
	parser := parser.New(src, tokens, err)
	return parser.Parse()
}

func TestParsePackage(t *testing.T) {
	src := []byte(`package foo`)
	assert.Snapshot(t, parse(src))
}

func TestParseImportSpec(t *testing.T) {
	src := []byte(`package foo
import "fmt"`)
	assert.Snapshot(t, parse(src))
}
