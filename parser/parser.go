package parser

import (
	"fmt"
	"go/ast"
	"go/token"
	"parser/lexer"
)

type (
	ParserError struct {
		Msg string
		Pos lexer.Position
	}

	Parser struct {
		src    []byte
		pos    int
		fuel   uint8
		tokens []lexer.Token
		errors []ParserError

		decls   []ast.Decl
		imports []*ast.ImportSpec
	}

	PackageClause struct {
		ident ast.Ident
		pos   token.Pos
	}
)

func New(src []byte, tokens []lexer.Token, lex_errors []lexer.LexError) Parser {
	errors := []ParserError{}
	for _, lex_error := range lex_errors {
		errors = append(errors, ParserError{Msg: lex_error.Msg, Pos: lex_error.Pos})
	}
	return Parser{src: src, tokens: tokens, errors: errors}
}

func (p *Parser) src_str(pos lexer.Position) string {
	return string(p.src[pos.Start:pos.End])
}

func (p *Parser) addParseError(msg string, pos lexer.Position) {
	p.errors = append(p.errors, ParserError{Msg: msg, Pos: pos})
}

func (p *Parser) isEOF() bool {
	return p.currToken().Kind == lexer.Eof
}

func (p *Parser) currToken() lexer.Token {
	return p.tokens[p.pos]
}

func (p *Parser) currPos() lexer.Position {
	return p.tokens[p.pos].Pos
}

func (p *Parser) peekNextToken() lexer.Token {
	// Return the EOF token
	if p.pos+1 >= len(p.tokens) {
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.pos+1]
}

func (p *Parser) nextToken() lexer.Token {
	pos := p.pos
	if !p.isEOF() {
		p.pos += 1
	}
	return p.tokens[pos]
}

func (p *Parser) at(token lexer.TokenKind) bool {
	return p.currToken().Kind == token
}

func (p *Parser) eat(token lexer.TokenKind) bool {
	if p.at(token) {
		p.nextToken()
		return true
	}
	return false
}

func (p *Parser) expect(token lexer.TokenKind) bool {
	if p.eat(token) {
		return true
	}
	currToken := p.currToken()
	p.addParseError(fmt.Sprintf("expecting %s found %s", token, currToken.Kind), currToken.Pos)
	return false
}

func (p *Parser) Parse() ast.File {
	start := p.currPos().Start
	pkg := p.parsePackageClause()
	p.eat(lexer.Newline) // TODO: find final solution for consuming newlines

	for !p.isEOF() {
		p.decls = append(p.decls, p.parseDecl())
		p.eat(lexer.Newline) // TODO: find final solution for consuming newlines
	}
	end := p.currPos().End
	p.eat(lexer.Eof)

	// TODO: create teh correct position using `base`
	return ast.File{Package: pkg.pos, Name: &pkg.ident, FileStart: token.Pos(start),
		FileEnd: token.Pos(end), Decls: p.decls, Imports: p.imports}
}

func (p *Parser) parseIdent() ast.Ident {
	pos := p.currPos()
	p.expect(lexer.Identifier)

	// TODO: create teh correct position using `base`
	return ast.Ident{NamePos: token.Pos(pos.Start), Name: p.src_str(pos)}
}

func (p *Parser) parsePackageClause() PackageClause {
	pos := p.currPos().Start
	p.expect(lexer.KwPackage)
	ident := p.parseIdent()

	return PackageClause{ident, token.Pos(pos)}
}

func (p *Parser) parseDecl() ast.Decl {
	token := p.currToken()
	switch token.Kind {
	case lexer.KwImport, lexer.KwConst, lexer.KwType, lexer.KwVar:
		return p.parseGenDecl(&token)
	default:
		panic(fmt.Sprintf("Not implemented yet %s\n", token))
	}
}

func (p *Parser) parseGenDecl(genTok *lexer.Token) *ast.GenDecl {
	pos := genTok.Pos.Start

	var tok token.Token
	switch genTok.Kind {
	case lexer.KwImport:
		tok = token.IMPORT
		p.eat(lexer.KwImport)
	case lexer.KwType:
		tok = token.TYPE
		p.eat(lexer.KwType)
	case lexer.KwConst:
		tok = token.CONST
		p.eat(lexer.KwConst)
	case lexer.KwVar:
		tok = token.VAR
		p.eat(lexer.KwVar)
	default:
		panic(fmt.Sprintf("Unknow generic declaration `%s`\n", genTok.Kind))
	}

	lParen := token.NoPos
	rParen := token.NoPos

	var specs []ast.Spec
	if p.at(lexer.Lparen) {
		lParen = token.Pos(p.currPos().Start)
		p.eat(lexer.Lparen)
		// TODO: make sure to not go into infinite loop, handle missing Rparen
		for !p.at(lexer.Rparen) {
			specs = append(specs, p.parseSpec(tok))
		}
		rParen = token.Pos(p.currPos().Start)
		p.expect(lexer.Rparen)
	} else {
		specs = append(specs, p.parseSpec(tok))
	}

	return &ast.GenDecl{Specs: specs, Tok: tok, TokPos: token.Pos(pos), Lparen: lParen, Rparen: rParen}
}

func (p *Parser) parseSpec(kind token.Token) ast.Spec {
	switch kind {
	case token.IMPORT:
		return p.parseImportSpec()
	case token.TYPE:
	case token.CONST:
	case token.VAR:
		panic(fmt.Sprintf("Not implemented yet %s\n", kind))
	}
	return &ast.ImportSpec{}
}

func (p *Parser) parseImportSpec() *ast.ImportSpec {
	currToken := p.currToken()
	var (
		name   *ast.Ident    = nil
		path   *ast.BasicLit = nil
		endPos token.Pos     = token.NoPos
	)
	switch currToken.Kind {
	case lexer.Dot, lexer.Identifier:
		tok := p.nextToken()
		name = &ast.Ident{Name: p.src_str(tok.Pos), NamePos: token.Pos(tok.Pos.Start)}
		fallthrough
	case lexer.String:
		if p.at(lexer.String) {
			endPos = token.Pos(p.currPos().End)
			path = p.parseBasicLit()
		} else {
			p.addParseError("missing import path", currToken.Pos)
		}
	default:
		p.addParseError("missing import path", currToken.Pos)
	}

	spec := &ast.ImportSpec{Name: name, Path: path, EndPos: endPos}
	p.imports = append(p.imports, spec)
	return spec
}

func (p *Parser) parseBasicLit() *ast.BasicLit {
	tok := p.currToken()
	value := p.src_str(tok.Pos)
	var kind token.Token

	switch tok.Kind {
	case lexer.String:
		p.eat(lexer.String)
		kind = token.STRING
	case lexer.Rune:
		p.eat(lexer.Rune)
		kind = token.CHAR
	case lexer.DecimalLit, lexer.HexLit, lexer.BinaryLit, lexer.OctalLit:
		p.eat(tok.Kind)
		kind = token.INT
	case lexer.Float, lexer.HexFloat:
		p.eat(tok.Kind)
		kind = token.FLOAT
	case lexer.ImaginaryLit:
		p.eat(lexer.ImaginaryLit)
		kind = token.IMAG
	default:
		panic(fmt.Sprintf("%s is not a basic literal", tok))

	}
	return &ast.BasicLit{ValuePos: token.Pos(tok.Pos.Start), Kind: kind, Value: value}
}
