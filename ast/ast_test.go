package ast

import (
	"monkey/token"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

var _ = Describe("Ast", func() {
	It("Can build String", func() {
		program := &Program{
			Statements: []Statement{
				&LetStatement{
					Token: token.Token{Type: token.LET, Literal: "let"},
					Name: &Identifier{
						Token: token.Token{Type: token.IDENT, Literal: "myVar"},
						Value: "myVar",
					},
					Value: &Identifier{
						Token: token.Token{Type: token.IDENT, Literal: "anotherVar"},
						Value: "anotherVar",
					},
				},
			},
		}

		Expect(program.String()).To(Equal("let myVar = anotherVar;"))
	})
})
