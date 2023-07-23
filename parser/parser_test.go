package parser

import (
	"fmt"
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
			Expect(p.Errors()).To(HaveLen(4))
		})

		DescribeTable("Returns errors",
			func(index int, msg string) {
				e := p.Errors()[index]
				Expect(e).To(Equal(msg))
			},
			Entry("first error", 0, "expected next token to be =, got INT instead"),
			Entry("second error", 1, "expected next token to be IDENT, got = instead"),
			Entry("second error", 2, "no prefix parse function for = found"),
			Entry("third error", 3, "expected next token to be IDENT, got INT instead"),
		)
	})

	Context("Parsing Return Statements", Ordered, func() {
		var program *ast.Program

		BeforeAll(func() {
			input := `
return 5;
return 10;
return 993322;
`

			l := lexer.New(input)
			p := New(l)

			program = p.ParseProgram()
			Expect(p.Errors()).To(BeEmpty())

			Expect(program.Statements).To(HaveLen(3), "not enough statements")
		})

		DescribeTable("Parses Return statement",
			func(index int) {
				s := program.Statements[index]

				Expect(s.TokenLiteral()).To(Equal("return"))
				_, ok := s.(*ast.ReturnStatement)
				Expect(ok).To(BeTrue())
			},
			Entry("first statement", 0),
			Entry("second statement", 1),
			Entry("third statement", 2),
		)
	})

	It("Parses Identifier Expressions", func() {
		input := "foobar;"

		l := lexer.New(input)
		p := New(l)

		program := p.ParseProgram()
		Expect(p.Errors()).To(BeEmpty())
		Expect(program.Statements).To(HaveLen(1), "not enough statements")

		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		Expect(ok).To(BeTrue())

		ident, ok := stmt.Expression.(*ast.Identifier)
		Expect(ok).To(BeTrue())

		Expect(ident.Value).To(Equal("foobar"))
		Expect(ident.TokenLiteral()).To(Equal("foobar"))
	})

	It("Parses Integer Literals", func() {
		input := "5;"

		l := lexer.New(input)
		p := New(l)

		program := p.ParseProgram()
		Expect(p.Errors()).To(BeEmpty())
		Expect(program.Statements).To(HaveLen(1), "not enough statements")

		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		Expect(ok).To(BeTrue())

		literal, ok := stmt.Expression.(*ast.IntegerLiteral)
		Expect(ok).To(BeTrue(), "exp not *ast.IntegerLiteral")

		Expect(literal.Value).To(Equal(int64(5)))
		Expect(literal.TokenLiteral()).To(Equal("5"))
	})

	Context("Parsing Prefix Expressions", Ordered, func() {
		DescribeTable("Parses Prefix statement",
			func(input string, operator string, integerValue int64) {
				l := lexer.New(input)
				p := New(l)

				program := p.ParseProgram()
				Expect(p.Errors()).To(BeEmpty())
				Expect(program.Statements).To(HaveLen(1))

				stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
				Expect(ok).To(BeTrue())

				exp, ok := stmt.Expression.(*ast.PrefixExpression)
				Expect(ok).To(BeTrue())
				Expect(exp.Operator).To(Equal(operator))

				integ, ok := exp.Right.(*ast.IntegerLiteral)
				Expect(ok).To(BeTrue())
				Expect(integ.Value).To(Equal(integerValue))
				Expect(integ.TokenLiteral()).To(Equal(fmt.Sprintf("%d", integerValue)))

			},
			Entry("infix not", "!5", "!", int64(5)),
			Entry("infix minus", "-15", "-", int64(15)),
		)
	})

})
