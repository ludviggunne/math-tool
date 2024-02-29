#! /usr/bin/env python3

from enum import Enum, auto, unique
from copy import copy
import math, sys

class IllegalCharacter(Exception):
    def __init__(self, char, pos):
        self.char = char
        self.pos = pos

    def __str__(self):
        return f"Illegal character in expression: '{self.char}'"

class UnexpectedEndOfInput(Exception):
    def __init__(self, pos):
        self.pos = pos

    def __str__(self):
        return "Unexpected end of input"

class UnexpectedToken(Exception):
    def __init__(self, found, msg):
        self.found = found
        self.msg = msg
        self.pos = found.pos

    def __str__(self):
        return f"Unexpected token {TokenTag.string(self.found.tag)}: {self.msg}"

class SubstitutionError(Exception):
    def __init__(self, token):
        self.pos = token.pos

    def __str__(self):
        return "Evaluation of undefined variable"

class Table:
    def __init__(self):
        self.data = {}
        self.counter = 0

    def resolve(self, name):
        if name in self.data:
            return self.data[name]
        else:
            id = self.counter
            self.data[name] = self.counter
            self.counter += 1
            return id

@unique
class FuncTag(Enum):
    EXP = auto()
    SIN = auto()
    COS = auto()
    TAN = auto()
    LOG = auto()

function_map = { FuncTag.EXP: math.exp,
                FuncTag.SIN: math.sin,
                FuncTag.COS: math.cos,
                FuncTag.TAN: math.tan,
                FuncTag.LOG: math.log }

function_names = { FuncTag.EXP: "exp",
                  FuncTag.SIN: "sin",
                  FuncTag.COS: "cos",
                  FuncTag.TAN: "tan",
                  FuncTag.LOG: "log" }

@unique
class TokenTag(Enum):
    PLUS = auto()
    MINUS = auto()
    ASTERISK = auto()
    SLASH = auto()
    CARET = auto()
    LPAREN = auto()
    RPAREN = auto()
    VAR = auto()
    FUNCTION = auto()
    NUMBER = auto()

    @staticmethod
    def string(tag):
        return token_tag_strings[tag]

token_tag_strings = { TokenTag.PLUS: '+',
                     TokenTag.MINUS: '-',
                     TokenTag.ASTERISK: '*',
                     TokenTag.SLASH: '/',
                     TokenTag.CARET: '^',
                     TokenTag.LPAREN: '(',
                     TokenTag.RPAREN: ')',
                     TokenTag.VAR: 'variable',
                     TokenTag.FUNCTION: 'function',
                     TokenTag.NUMBER: 'number' }

class Token:
    def __init__(self, tag, pos):
        self.tag = tag
        self.pos = pos

    def __str__(self):
        string = str(self.tag)
        if self.tag == TokenTag.VAR:
            string += ": " + str(self.id)
        elif self.tag == TokenTag.FUNCTION:
            string += ": " + str(self.func)
        elif self.tag == TokenTag.NUMBER:
            string += ": " + str(self.value)
        return string

class Lexer:
    funcs = { "exp": FuncTag.EXP,
              "sin": FuncTag.SIN,
              "cos": FuncTag.COS,
              "tan": FuncTag.TAN,
              "log": FuncTag.LOG,
              "ln":  FuncTag.LOG }

    ops = { "+": TokenTag.PLUS,
            "-": TokenTag.MINUS,
            "*": TokenTag.ASTERISK,
            "/": TokenTag.SLASH,
            "^": TokenTag.CARET,
            "(": TokenTag.LPAREN,
            ")": TokenTag.RPAREN }

    consts = { "e": math.e,
               "pi": math.pi }

    def __init__(self, source, table):
        self.source = source
        self.cursor = 0
        self.peeked = None
        self.length = len(source)
        self.table = table

    @staticmethod
    def is_whitespace(c):
        return c == ' '

    @staticmethod
    def is_digit(c):
        return '0' <= c and c <= '9'

    @staticmethod
    def is_alphabetic(c):
        return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z')

    def advance(self):
        self.cursor += 1

    def operator(self, tag):
        self.advance()
        return Token(tag, self.cursor - 1)

    def current(self):
        return self.source[self.cursor]

    def at_end(self):
        return self.cursor == self.length

    def number(self):
        begin = self.cursor
        x = 0
        while Lexer.is_digit(self.current()):
            x *= 10
            x += ord(self.current()) - ord('0')
            self.advance()
            if self.at_end():
                break
        token = Token(TokenTag.NUMBER, begin)
        token.value = x
        return token

    def identifier(self):
        begin = self.cursor
        while Lexer.is_alphabetic(self.current()):
            self.advance()
            if self.at_end():
                break
        name = self.source[begin:self.cursor]
        if name in Lexer.funcs:
            token = Token(TokenTag.FUNCTION, begin)
            token.func = Lexer.funcs[name]
            return token
        if name in Lexer.consts:
            token = Token(TokenTag.NUMBER, begin)
            token.value = Lexer.consts[name]
            return token
        token = Token(TokenTag.VAR, begin)
        token.name = self.source[begin:self.cursor]
        token.id = self.table.resolve(name)
        return token

    def take(self):
        if self.peeked != None:
            tmp = self.peeked
            self.peeked = None
            return tmp

        if self.at_end():
            return None

        while Lexer.is_whitespace(self.current()):
            self.advance()
            if self.at_end():
                return None

        c = self.current()

        if c in Lexer.ops: return self.operator(Lexer.ops[c])
        if Lexer.is_digit(c): return self.number()
        if Lexer.is_alphabetic(c): return self.identifier()

        raise IllegalCharacter(c, self.cursor)

    def peek(self):
        if self.peeked == None:
            self.peeked = self.take()
        return self.peeked

    def cursor(self):
        return _cursor

@unique
class NodeTag(Enum):
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    NEG = auto()
    EXP = auto()
    FUNCTION = auto()
    VAR = auto()
    NUMBER = auto()

precedence = { NodeTag.ADD: 0,
              NodeTag.SUB: 0,
              NodeTag.MUL: 1,
              NodeTag.DIV: 1,
              NodeTag.EXP: 2,
              NodeTag.FUNCTION: 3,
              NodeTag.VAR: 3,
              NodeTag.NUMBER: 3 }

class Node:
    def __init__(self):
        pass

    # Init addition
    @staticmethod
    def add(token, left, right):
        self = Node()
        self.tag = NodeTag.ADD
        self.token = token
        self.left = left
        self.right = right
        return self

    # Init subtraction
    @staticmethod
    def sub(token, left, right):
        self = Node()
        self.tag = NodeTag.SUB
        self.token = token
        self.left = left
        self.right = right
        return self

    # Init multiplication
    @staticmethod
    def mul(token, left, right):
        self = Node()
        self.tag = NodeTag.MUL
        self.token = token
        self.left = left
        self.right = right
        return self

    # Init division
    @staticmethod
    def div(token, left, right):
        self = Node()
        self.tag = NodeTag.DIV
        self.token = token
        self.left = left
        self.right = right
        return self

    # Init function evaluation
    @staticmethod
    def function(token, func, arg):
        self = Node()
        self.tag = NodeTag.FUNCTION
        self.token = token
        self.func = func
        self.arg = arg
        return self

    # Init variable
    @staticmethod
    def var(token, id, name):
        self = Node()
        self.tag = NodeTag.VAR
        self.token = token
        self.id = id
        self.name = name
        return self

    # Init number
    @staticmethod
    def number(token, value):
        self = Node()
        self.tag = NodeTag.NUMBER
        self.token = token
        self.value = value
        return self

    # Init negation
    @staticmethod
    def neg(token, operand):
        self = Node()
        self.tag = NodeTag.NEG
        self.token = token
        self.operand = operand
        return self

    # Init exponentation
    @staticmethod
    def exp(token, base, exponent):
        self = Node()
        self.tag = NodeTag.EXP
        self.token = token
        self.base = base
        self.exponent = exponent
        return self

    # AST printing
    def print(self, indent, extra):
        print("  " * indent, end="")
        if extra != None:
            print(extra + ": ", end="")
        if self.tag == NodeTag.ADD:
            print("add")
            self.left.print(indent + 1, "left")
            self.right.print(indent + 1, "right")
            return
        if self.tag == NodeTag.SUB:
            print("subtract")
            self.left.print(indent + 1, "left")
            self.right.print(indent + 1, "right")
            return
        if self.tag == NodeTag.MUL:
            print("multiply")
            self.left.print(indent + 1, "left")
            self.right.print(indent + 1, "right")
            return
        if self.tag == NodeTag.DIV:
            print("divide")
            self.left.print(indent + 1, "left")
            self.right.print(indent + 1, "right")
            return
        if self.tag == NodeTag.NEG:
            print("negate")
            self.operand.print(indent + 1, "operand")
            return
        if self.tag == NodeTag.EXP:
            print("exponentation")
            self.base.print(indent + 1, "base")
            self.exponent.print(indent + 1, "exponent")
            return
        if self.tag == NodeTag.FUNCTION:
            print("function ", end="")
            print(self.func)
            self.arg.print(indent + 1, "argument")
            return
        if self.tag == NodeTag.VAR:
            print(f"variable id={self.id}")
            return
        if self.tag == NodeTag.NUMBER:
            print(f"number {self.value}")
            return

    # Expression evaluation
    def eval(self, subst):
        if self.tag == NodeTag.ADD:
            return self.left.eval(subst) + self.right.eval(subst)
        if self.tag == NodeTag.SUB:
            return self.left.eval(subst) - self.right.eval(subst)
        if self.tag == NodeTag.MUL:
            return self.left.eval(subst) * self.right.eval(subst)
        if self.tag == NodeTag.DIV:
            return self.left.eval(subst) / self.right.eval(subst)
        if self.tag == NodeTag.NEG:
            return -self.operand.eval(subst)
        if self.tag == NodeTag.EXP:
            return self.base.eval(subst) ** self.exponent.eval(subst)
        if self.tag == NodeTag.VAR:
            if self.id in subst:
                return subst[self.id]
            else:
                raise SubstitutionError(self.token)
        if self.tag == NodeTag.NUMBER:
            return self.value
        if self.tag == NodeTag.FUNCTION:
            return function_map[self.func](self.arg.eval(subst))
        return 0

    def __str__(self):
        if self.tag == NodeTag.ADD:
            return f"{self.left} + {self.right}"
        if self.tag == NodeTag.SUB:
            if precedence[self.right.tag] <= precedence[self.tag]:
                return f"{self.left} - ({self.right})"
            return f"{self.left} - {self.right}"
        if self.tag == NodeTag.MUL:
            left, right = None, None
            if precedence[self.left.tag] < precedence[self.tag]:
                left = f"({self.left})"
            else:
                left = f"{self.left}"
            if precedence[self.right.tag] < precedence[self.tag]:
                right = f"({self.right})"
            else:
                right = f"{self.right}"
            return f"{left} * {right}"
        if self.tag == NodeTag.DIV:
            left, right = None, None
            if precedence[self.left.tag] < precedence[self.tag]:
                left = f"({self.left})"
            else:
                left = f"{self.left}"
            if precedence[self.right.tag] <= precedence[self.tag]:
                right = f"({self.right})"
            else:
                right = f"{self.right}"
            return f"{left} / {right}"
        if self.tag == NodeTag.EXP:
            if precedence[self.exponent.tag] < precedence[self.tag]:
                return f"{self.base} ^ ({self.exponent})"
            return f"{self.base} ^ {self.exponent}"
        if self.tag == NodeTag.FUNCTION:
            return f"{function_names[self.func]}({self.arg})"
        if self.tag == NodeTag.VAR:
            return self.name
        if self.tag == NodeTag.NUMBER:
            return str(self.value)

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer

    def parse(self):
        root = self.sum()
        trailing = self.lexer.take()
        if trailing != None:
            raise UnexpectedToken(trailing, "trailing token(s)")
        return root

    def sum(self):
        left = self.product()
        while True:
            token = self.lexer.peek()
            # At end
            if token == None:
                return left
            # Addition
            if token.tag == TokenTag.PLUS:
                self.lexer.take()
                right = self.product()
                left = Node.add(token, left, right)
                continue
            # Subtraction
            if token.tag == TokenTag.MINUS:
                self.lexer.take()
                right = self.product()
                left = Node.sub(token, left, right)
                continue
            return left

    def product(self):
        left = self.factor()
        while True:
            token = self.lexer.peek()
            # At end
            if token == None:
                return left
            # Multiplication
            if token.tag == TokenTag.ASTERISK:
                self.lexer.take()
                right = self.factor()
                left = Node.mul(token, left, right)
                continue
            # Division
            if token.tag == TokenTag.SLASH:
                self.lexer.take()
                right = self.factor()
                left = Node.div(token, left, right)
                continue
            return left

    def factor(self):
        token = self.lexer.peek()
        if token == None:
            raise UnexpectedEndOfInput(self.lexer.cursor)
        node = None
        # Parenthesized expression
        if token.tag == TokenTag.LPAREN:
            self.lexer.take()
            node = self.sum()
            self.expect(TokenTag.RPAREN, "missing closing parenthesis")
        # Number literal
        elif token.tag == TokenTag.NUMBER:
            self.lexer.take()
            node = Node.number(token, token.value)
        # Function evaluation
        elif token.tag == TokenTag.FUNCTION:
            self.lexer.take()
            self.expect(TokenTag.LPAREN, "expected '(' after function name")
            arg = self.sum()
            self.expect(TokenTag.RPAREN, "missing closing parenthesis")
            node = Node.function(token, token.func, arg)
        # Negating
        elif token.tag == TokenTag.MINUS:
            self.lexer.take()
            operand = self.factor()
            node = Node.neg(token, operator)
        # Variable
        elif token.tag == TokenTag.VAR:
            self.lexer.take()
            node = Node.var(token, token.id, token.name)
        # Undefined
        else:
            raise UnexpectedToken(token, "unexpected token in expression")
        # Trailing exponents
        while self.lexer.peek() != None and self.lexer.peek().tag == TokenTag.CARET:
            token = self.lexer.take()
            exp = self.factor()
            node = Node.exp(token, node, exp)
        return node

    def expect(self, tag, msg):
        token = self.lexer.take()
        # At end
        if token == None:
            raise UnexpectedEndOfInput(self.lexer.cursor)
        # Mismatch
        if token.tag != tag:
            raise UnexpectedToken(token, msg)
        return token

class ParsedExpression:
    def __init__(self, expr_string):
        self.expr_string = expr_string
        self.table = Table()
        self.root = None
        lexer = Lexer(self.expr_string, self.table)
        parser = Parser(lexer)
        self.root = parser.parse()

# Base class for defining passes over the AST
class Pass:
    def __init__(self):
        pass
    def exec(self, root):
        if root == None:
            return None
        if root.tag == NodeTag.ADD:
            return self.add(root)
        if root.tag == NodeTag.SUB:
            return self.sub(root)
        if root.tag == NodeTag.MUL:
            return self.mul(root)
        if root.tag == NodeTag.DIV:
            return self.div(root)
        if root.tag == NodeTag.EXP:
            return self.exp(root)
        if root.tag == NodeTag.FUNCTION:
            return self.function(root)
        if root.tag == NodeTag.NUMBER:
            return self.number(root)
        if root.tag == NodeTag.VAR:
            return self.var(root)
    def add(self, root): return root
    def sub(self, root): return root
    def mul(self, root): return root
    def div(self, root): return root
    def exp(self, root): return root
    def function(self, root): return root
    def number(self, root): return root
    def var(self, root): return root

class Simplify(Pass):
    def add(self, root):
        root.left = self.exec(root.left)
        root.right = self.exec(root.right)
        if root.left.tag == NodeTag.NUMBER and root.right.tag == NodeTag.NUMBER:
            root.tag = NodeTag.NUMBER
            root.value = root.left.value + root.right.value
            del root.left
            del root.right
        return root

    def sub(self, root):
        root.left = self.exec(root.left)
        root.right = self.exec(root.right)
        if root.left.tag == NodeTag.NUMBER and root.right.tag == NodeTag.NUMBER:
            root.tag = NodeTag.NUMBER
            root.value = root.left.value - root.right.value
            del root.left
            del root.right
        return root

    def mul(self, root):
        root.left = self.exec(root.left)
        root.right = self.exec(root.right)
        if root.left.tag == NodeTag.NUMBER and root.right.tag == NodeTag.NUMBER:
            root.tag = NodeTag.NUMBER
            root.value = root.left.value * root.right.value
            del root.left
            del root.right
        return root

    def div(self, root):
        root.left = self.exec(root.left)
        root.right = self.exec(root.right)
        if root.left.tag == NodeTag.NUMBER and root.right.tag == NodeTag.NUMBER:
            root.tag = NodeTag.NUMBER
            root.value = root.left.value / root.right.value
            del root.left
            del root.right
        return root

    def exp(self, root):
        root.base = self.exec(root.base)
        root.exponent = self.exec(root.exponent)
        if root.base.tag == NodeTag.NUMBER and root.exponent.tag == NodeTag.NUMBER:
            root.tag = NodeTag.NUMBER
            root.value = root.base.value ** root.exponent.value
            del root.base
            del root.exponent
        return root

    def function(self, root):
        root.arg = self.exec(root.arg)
        if root.arg.tag == NodeTag.NUMBER:
            root.tag = NodeTag.NUMBER
            root.value = function_map[root.func](root.arg.value)
            del root.func
            del root.arg
        return root

def _main():
    if len(sys.argv) == 1:
        sys.exit("Error: missing expression string")
    string = sys.argv[1]
    expr = None
    try:
        expr = ParsedExpression(string)
    except Exception as e:
        print(e)
        print(string)
        print(' ' * e.pos + '^')
        sys.exit(-1)
    simplify = Simplify()
    expr.root = simplify.exec(expr.root)
    print(expr.root)
    pass

if __name__ == "__main__": _main()
