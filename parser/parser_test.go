package parser

import (
	"monkey/ast"
	"monkey/lexer"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

var _ = Describe("Parser", func() {
	Context("Parsing a valid program", Ordered, func() {
		var program *ast.Program

		BeforeAll(func() {
			input := `
let x = 5;
let y = 10;
let foobar = 838383;
`

			l := lexer.New(input)
			p := New(l)

			program = p.ParseProgram()
			Expect(p.Errors()).To(BeEmpty())

			Expect(program).ToNot(BeNil())
			Expect(program.Statements).To(HaveLen(3))
		})

		DescribeTable("Parses Let statements",
			func(index int, name string) {
				s := program.Statements[index]

				Expect(s.TokenLiteral()).To(Equal("let"))
				letStmt, ok := s.(*ast.LetStatement)
				Expect(ok).To(BeTrue())
				Expect(letStmt.Name.Value).To(Equal(name))
			},
			Entry("first statement", 0, "x"),
			Entry("second statement", 1, "y"),
			Entry("third statement", 2, "foobar"),
		)
	})

	Context("Parsing an invalid program", Ordered, func() {
		var p *Parser

		BeforeAll(func() {
			input := `
let x 5;
let = 10;
let 838383;
`

			l := lexer.New(input)
			p = New(l)

			p.ParseProgram()
			Expect(p.Errors()).To(HaveLen(3))
		})

		DescribeTable("Returns errors",
			func(index int, msg string) {
				e := p.Errors()[index]
				Expect(e).To(Equal(msg))
			},
			Entry("first error", 0, "expected next token to be =, got INT instead"),
			Entry("second error", 1, "expected next token to be IDENT, got = instead"),
			Entry("third error", 2, "expected next token to be IDENT, got INT instead"),
		)
	})
})
