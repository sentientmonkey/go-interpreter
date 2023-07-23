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
			return false, fmt.Errorf("Expected %T to be *ast.Identifier", exp)
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

func BeBooleanLiteral(expected bool) types.GomegaMatcher {
	return gcustom.MakeMatcher(func(exp ast.Expression) (bool, error) {
		bl, ok := exp.(*ast.Boolean)
		if !ok {
			return false, fmt.Errorf("Expected %T to be *ast.Boolean", exp)
		}

		if bl.Value != expected {
			return false, fmt.Errorf("Expected .Value %v to be %v", bl.Value, expected)
		}

		actualToken := bl.TokenLiteral()

		if expected && actualToken != "true" {
			return false, fmt.Errorf("Expected .TokenLiteral() %q to be TRUE", actualToken)
		}
		if !expected && actualToken != "false" {
			return false, fmt.Errorf("Expected .TokenLiteral() %q to be FALSE", actualToken)
		}

		return true, nil
	})
}

func BeLiteralExpression(expected interface{}) types.GomegaMatcher {
	switch v := expected.(type) {
	case int:
		return BeIntegerLiteral(int64(v))
	case int64:
		return BeIntegerLiteral(v)
	case string:
		return BeIdentifier(v)
	case bool:
		return BeBooleanLiteral(v)
	default:
		Fail(fmt.Sprintf("Unknown type %T in BeLiteralExpression() \n", v))
	}

	return nil
}

func BeInfixExpression(left interface{}, operator string, right interface{}) types.GomegaMatcher {
	return gcustom.MakeMatcher(func(exp ast.Expression) (bool, error) {
		opExp, ok := exp.(*ast.InfixExpression)

		if !ok {
			return false, fmt.Errorf("Expected %T to be *ast.InfixExpression", opExp)
		}

		Expect(opExp.Left).To(BeLiteralExpression(left))
		if opExp.Operator != operator {
			return false, fmt.Errorf("Expected operator %q to be %q", opExp.Operator, operator)
		}

		Expect(opExp.Right).To(BeLiteralExpression(right))

		return true, nil
	})
}

func BePrefixExpression(operator string, right interface{}) types.GomegaMatcher {
	return gcustom.MakeMatcher(func(exp ast.Expression) (bool, error) {
		opExp, ok := exp.(*ast.PrefixExpression)

		if !ok {
			return false, fmt.Errorf("Expected %T to be *ast.PrefixExpression", opExp)
		}

		if opExp.Operator != operator {
			return false, fmt.Errorf("Expected operator %q to be %q", opExp.Operator, operator)
		}

		Expect(opExp.Right).To(BeLiteralExpression(right))

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

		Expect(stmt.Expression).To(BeLiteralExpression("foobar"))
	})

	It("Parses Integer Literals", func() {
		input := "5;"

		l := lexer.New(input)
		p := New(l)

		program := p.ParseProgram()
		Expect(p.Errors()).To(BeEmpty())

		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		Expect(ok).To(BeTrue())

		Expect(stmt.Expression).To(BeLiteralExpression(5))
	})

	Context("Parsing Prefix Expressions", func() {
		DescribeTable("Parses Prefix statement",
			func(input string, operator string, value interface{}) {
				l := lexer.New(input)
				p := New(l)

				program := p.ParseProgram()
				Expect(p.Errors()).To(BeEmpty())
				Expect(program.Statements).To(HaveLen(1))

				stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
				Expect(ok).To(BeTrue())

				Expect(stmt.Expression).To(BePrefixExpression(operator, value))
			},
			Entry(nil, "!5", "!", 5),
			Entry(nil, "-15", "-", 15),
			Entry(nil, "!true", "!", true),
			Entry(nil, "!false", "!", false),
		)
	})

	Context("Parsing Infix Expressions", func() {
		DescribeTable("Parses Infix statement",
			func(input string, leftValue interface{}, operator string, rightValue interface{}) {
				l := lexer.New(input)
				p := New(l)

				program := p.ParseProgram()
				Expect(p.Errors()).To(BeEmpty())

				stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
				Expect(ok).To(BeTrue())

				Expect(stmt.Expression).To(BeInfixExpression(leftValue, operator, rightValue))
			},
			Entry(nil, "5 + 5", 5, "+", 5),
			Entry(nil, "5 - 5", 5, "-", 5),
			Entry(nil, "5 * 5", 5, "*", 5),
			Entry(nil, "5 / 5", 5, "/", 5),
			Entry(nil, "5 > 5", 5, ">", 5),
			Entry(nil, "5 < 5", 5, "<", 5),
			Entry(nil, "5 == 5", 5, "==", 5),
			Entry(nil, "5 != 5", 5, "!=", 5),
			Entry(nil, "true == true", true, "==", true),
			Entry(nil, "true != false", true, "!=", false),
		)
	})

	Context("Parsing Operator Precedence Parsing", func() {
		DescribeTable("Parses",
			func(input, expected string) {
				l := lexer.New(input)
				p := New(l)

				program := p.ParseProgram()
				Expect(p.Errors()).To(BeEmpty())

				Expect(program.String()).To(Equal(expected))
			},
			Entry(nil, "-a * b", "((-a) * b)"),
			Entry(nil, "!-a", "(!(-a))"),
			Entry(nil, "a + b + c", "((a + b) + c)"),
			Entry(nil, "a + b - c", "((a + b) - c)"),
			Entry(nil, "a * b * c", "((a * b) * c)"),
			Entry(nil, "a * b / c", "((a * b) / c)"),
			Entry(nil, "a + b / c", "(a + (b / c))"),
			Entry(nil, "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
			Entry(nil, "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
			Entry(nil, "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
			Entry(nil, "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
			Entry(nil, "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
			Entry(nil, "true", "true"),
			Entry(nil, "false", "false"),
			Entry(nil, "3 > 5 == false", "((3 > 5) == false)"),
			Entry(nil, "3 < 5 == true", "((3 < 5) == true)"),
		)
	})

	Context("Parses Boolean Literals", func() {
		DescribeTable("Parses",
			func(input string, expected bool) {
				l := lexer.New(input)
				p := New(l)

				program := p.ParseProgram()
				Expect(p.Errors()).To(BeEmpty())

				stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
				Expect(ok).To(BeTrue())

				Expect(stmt.Expression).To(BeLiteralExpression(expected))
			},
			Entry(nil, "true", true),
			Entry(nil, "false", false),
		)
	})
})
