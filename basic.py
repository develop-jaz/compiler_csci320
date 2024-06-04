#####
##CONSTANTS#######
###########

DIGITS = '0123456789'

#####
##Accepted characters
##########
ACCEPTED_CHARS = '<>=!&|()+-/*1234567890'

########
#Errors
#############

class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name} : {self.details} \n'
        result += f'Line: {self.pos_start.ln + 1}'
        return result

class IllegalCharError(Error):
    def __init__(self,  pos_start, pos_end, details):
        super().__init__( pos_start, pos_end,'Illegal Character', details)

class IllegalSyntaxError(Error):
    def __init__(self,  pos_start, pos_end, details):
        super().__init__( pos_start, pos_end,'Invalid Syntax', details)

class RTError(Error):
    def __init__(self,  pos_start, pos_end, details):
        super().__init__( pos_start, pos_end,'Runtime Error', details)



class Position:
    def __init__(self, idx, ln, col):
        self.idx = idx
        self.ln = ln
        self.col = col

    def advance(self, current_char = None):
        self.idx +=1
        self.col += 1

        if current_char == ' \n':
            self.ln += 1
            self.col = 0
        return self
    
    def copy(self):
        return Position(self.idx, self.ln, self.col)
        

##################
####Tokens#####
###################

TT_INT   = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MULTIPLICATION'
TT_DIV = 'DIVISION'
TT_LPAREN = 'LEFT-PARENTHESIS'
TT_RPAREN = 'RIGHT-PARENTHESIS'
TT_GREATER = 'GREATER THAN'
TT_LESS = 'LESS THAN'
TT_EGREATER = 'GREATER THAN/EQUAL TO'
TT_ELESS = 'LESS THAN/EQUAL TO'
TT_EQUAL = 'EQUAL'
TT_NEQUAL = 'NOT EQUAL'
TT_NOT = 'NOT'
TT_AND = 'AND'
TT_OR = 'OR'
TT_EOF = 'EOF'


class Token:
    def __init__(self, type_, value=None, pos_start = None, pos_end = None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = Position(-1,0,-1)
        self.nextpos = Position(0,0,0) 
        self.current_char = None
        self.next_char = None
        self.advance()
    
    def advance(self):
        self.pos.advance(self.current_char)
        self.nextpos.advance(self.next_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
        if self.pos.idx < len(self.text) - 1:
            self.next_char = self.text[self.nextpos.idx]
        else:
            self.next_char = None

    
    def make_tokens(self):
        tokens =[]

        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()            
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif (self.current_char == '>' and self.next_char == '='):
                tokens.append(Token(TT_EGREATER, pos_start=self.pos))
                self.advance()
                self.advance()
            elif (self.current_char == '<' and self.next_char == '='):
                tokens.append(Token(TT_ELESS, pos_start=self.pos))
                self.advance()
                self.advance()
            elif self.current_char == '>' and (self.next_char in ' \t' or self.next_char in ACCEPTED_CHARS):
                tokens.append(Token(TT_GREATER, pos_start=self.pos))
                self.advance()
            elif self.current_char == '<' and (self.next_char in ' \t' or self.next_char in ACCEPTED_CHARS):
                tokens.append(Token(TT_LESS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '=' and self.next_char == '=':
                tokens.append(Token(TT_EQUAL, pos_start=self.pos))
                self.advance()
                self.advance()
            elif self.current_char == '!' and self.next_char == '=':
                tokens.append(Token(TT_NEQUAL, pos_start=self.pos))
                self.advance()
                self.advance()
            elif self.current_char == '!' and (self.next_char in ' \t' or self.next_char in ACCEPTED_CHARS):
                tokens.append(Token(TT_NOT, pos_start=self.pos))
                self.advance()
            elif self.current_char == '&' and self.next_char == '&':
                tokens.append(Token(TT_AND, pos_start=self.pos))
                self.advance()
                self.advance()
            elif self.current_char == '|' and self.next_char == '|':
                tokens.append(Token(TT_OR, pos_start=self.pos))
                self.advance()
                self.advance()        
            elif self.current_char not in ACCEPTED_CHARS:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return[], IllegalCharError(pos_start, self.pos,"'" + char + "'")
            elif self.next_char not in ACCEPTED_CHARS and self.next_char not in ' \t':
                pos_start = self.pos.copy()
                char = self.next_char
                self.advance()
                return[], IllegalCharError(pos_start, self.pos,"'" + char + "'")
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return[], IllegalCharError(pos_start, self.pos, "'" + char + "'")
        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens , None
       
    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start=self.pos.copy()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count +=1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()
        
        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

##############
        ###NODES
################

class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'
    
class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'
    
class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end
    
    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error: self.error = res.error
            return res.node
        return res

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self

###################
#######PARSER######
    #################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok
    
########################
    
    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(IllegalSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected an accepted operation"))
        return res
    
    def expr(self):
        return self.bin_op(self.andOp, (TT_OR))
    
    def andOp(self):
        return self.bin_op(self.compare, (TT_AND))
    
    def compare(self):
        return self.bin_op(self.arithmetic, (TT_NEQUAL, TT_EQUAL, TT_LESS, TT_ELESS, TT_EGREATER, TT_GREATER))
   
    def arithmetic(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))
    
    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_NOT))  
    
    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_NOT):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (TT_PLUS, TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (TT_INT, TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))
        
        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(IllegalSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                    )
                )


        return res.failure(IllegalSyntaxError(tok.pos_start, tok.pos_end, "Expected int or float"))
          
    #####################################

    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.current_tok.type in ops:
            op_tok = self.current_tok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)


###################
#######RUNTIME RESULT######
    #################

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error: self.error = res.error
        return res.value
    
    def success(self, value):
        self.value = value
        return self
    
    def failure(self, error):
        self.error = error
        return self


###################
#######VALUES######
    #################

class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self
    
    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value), None
        
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value), None
        
    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value), None
        
    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Division by 0'
                )
            return Number(self.value / other.value), None
        
    def less_than(self, other):
        if isinstance(other, Number):
            return Number(self.value < other.value), None
        
    def greater_than(self, other):
        if isinstance(other, Number):
            return Number(self.value > other.value), None
        
    def less_than_eq(self, other):
        if isinstance(other, Number):
            return Number(self.value <= other.value), None
        
    def greater_than_eq(self, other):
        if isinstance(other, Number):
            return Number(self.value >= other.value), None
        
    def not_eq(self, other):
        if isinstance(other, Number):
            return Number(self.value != other.value), None
        
    def eq(self, other):
        if isinstance(other, Number):
            return Number(self.value == other.value), None
        
    def and_op(self, other):
        if isinstance(other, Number):
            return Number(self.value and other.value), None
        
    def or_op(self, other):
        if isinstance(other, Number):
            return Number(self.value or other.value), None
        
    def not_op(self, other):
        if isinstance(other, Number):
            return Number(not self.value)
        
    def neg_op(self, other):
        if isinstance(other, Number):
            return Number(-1 * self.value)
        
    def __repr__(self):
        return str(self.value)

###################
#######INTERPRETER######
    #################

class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)

    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    
    def visit_NumberNode(self, node):
        return RTResult().success(
            Number(node.tok.value).set_pos(node.pos_start, node.pos_end)
        )
    
    def visit_BinOpNode(self, node):
        res = RTResult()
        #print("Found bin op node!")
        left = res.register(self.visit(node.left_node))
        right = res.register(self.visit(node.right_node))
        if res.error: return res

        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.dived_by(right)
        elif node.op_tok.type == TT_LESS:
            result, error = left.less_than(right)
        elif node.op_tok.type == TT_GREATER:
            result, error = left.greater_than(right)
        elif node.op_tok.type == TT_ELESS:
            result, error = left.less_than_eq(right)
        elif node.op_tok.type == TT_EGREATER:
            result, error = left.greater_than_eq(right)
        elif node.op_tok.type == TT_NEQUAL:
            result, error = left.not_eq(right)
        elif node.op_tok.type == TT_EQUAL:
            result, error = left.eq(right)
        elif node.op_tok.type == TT_AND:
            result, error = left.and_op(right)
        elif node.op_tok.type == TT_OR:
            result, error = left.or_op(right)
            
        
        
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node):
        res = RTResult()
        node1 = res.register(self.visit(node.node))
        if res.error: return res

        error = None
        

        if node.op_tok.type == TT_MINUS:
            node1 = node1.neg_op(node1)
            if error:
                return res.failure(error)
        
            else:
                return res.success(node1)
        
        if node.op_tok.type == TT_NOT:
            node1 = node1.not_op(node1)
            if error:
                return res.failure(error)
        
            else:
                return res.success(node1)
            





#######
##RUN###
###############
        
def run(text):
    lexer = Lexer(text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    ##Generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    #Run program
    interpreter = Interpreter()
    result = interpreter.visit(ast.node)

    #return ast.node, ast.error
    return result.value, result.error
