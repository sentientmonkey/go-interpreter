package parser

import (
	"fmt"
	"monkey/ast"
	"monkey/lexer"
	"reflect"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/onsi/gomega/gcustom"
	"github.com/onsi/gomega/types"
)

func BeIntegerLiteral(expected int64) types.GomegaMatcher {
	return gcustom.MakeMatcher(func(exp ast.Expression) (bool, error) {
		integ, ok := exp.(*ast.IntegerLiteral)
		if !ok {
			return false, fmt.Errorf("Expected %v to be *ast.IntegerLiteral", reflect.TypeOf(exp))
		}

		if integ.Value != expected {
			return false, fmt.Errorf("Expected .Value %d to be %d", integ.Value, expected)
		}

		actualToken := integ.TokenLiteral()
		expectedToken := fmt.Sprintf("%d", expected)
		if actualToken != expectedToken {
			return false, fmt.Errorf("Expected .TokenLiteral() %q to be %q", actualToken, expectedToken)
		}

		return true, nil
	})
}

func BeIdentifier(expected string) types.GomegaMatcher {
	return gcustom.MakeMatcher(func(exp ast.Expression) (bool, error) {
		ident, ok := exp.(*ast.Identifier)
		if !ok {
			return false, fmt.Errorf("Expected %v to be *ast.Identifier", reflect.TypeOf(exp))
		}

		if ident.Value != expected {
			return false, fmt.Errorf("Expected .Value %q to be %q", ident.Value, expected)
		}

		actualToken := ident.TokenLiteral()
		if actualToken != expected {
			return false, fmt.Errorf("Expected .TokenLiteral() %q to be %q", actualToken, expected)
		}

		return true, nil
	})
}

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

		Expect(stmt.Expression).To(BeIdentifier("foobar"))
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

		Expect(stmt.Expression).To(BeIntegerLiteral(5))
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

				Expect(exp.Right).To(BeIntegerLiteral(integerValue))
			},
			Entry("infix not", "!5", "!", int64(5)),
			Entry("infix minus", "-15", "-", int64(15)),
		)
	})

})
