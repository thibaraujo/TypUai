# ------------------------------------------------------------
# calclex.py
#
# tokenizer for a simple expression evaluator for
# numbers and +,-,*,/
# ------------------------------------------------------------
import ply.lex as lex
from ply.lex import TOKEN
import ply.yacc as yacc

# List of token names.   This is always required

reserved = {
   'caso' : 'SE',
   'then' : 'THEN',
   'naodeu' : 'SENAO',
   'uai' : 'ENQUANTO',
   'ese' : 'SENAOSE',
   'floop' : 'PARA',
   'no' : 'NO',
   'retorna' : 'RETORNA',
   'intero' : 'INT',
   'real' : 'FLOAT',
   'caractere' : 'CHAR',
   'bool' : 'BOOLEANO',
   'e' : 'E',
   'ou' : 'OU',
   'vaza' : 'BREAK',
   'oito' : 'FALSE',
   'oitenta' : 'TRUE',

}

tokens = [
   'INTEIRO',
   'STRING',
   'ID',
   'SOMA',
   'SUBTRACAO',
   'MULTIPLICACAO',
   'DIVISAO',
   'ABREPARENTESES',
   'FECHAPARENTESES',
   'NAO', 
   'IGUAL', 
   'ATRIBUICAO',
   'COMECO',
   'FIM',
   'MAIORIGUAL',
   'MAIOR',
   'MENORIGUAL',
   'MENOR',
   'DIFERENTE',
   'INVERTE',
   'RESTO',
   'PONTO',
   'ASPADUPLA',
   'FIMCOMANDO',
   'VIRGULA',
   'DECIMAL',
   'ASPASSIMPLES',
   
   
] + list(reserved.values())



# Regular expression rules for simple tokens
t_SOMA    = r'\+'
t_SUBTRACAO   = r'-'
t_MULTIPLICACAO   = r'\*'
t_DIVISAO  = r'/'
t_ABREPARENTESES  = r'\('
t_FECHAPARENTESES  = r'\)'
t_NAO  = r'\!'
t_IGUAL  = r'\=='
t_ATRIBUICAO  = r'\='
t_COMECO = r'\:'
t_FIM = r';'
t_MAIORIGUAL = r'\>='
t_MAIOR = r'\>'
t_MENORIGUAL = r'\<='
t_MENOR = r'\<'
t_DIFERENTE = r'\!!'
t_INVERTE = r'\~'
t_RESTO = r'\%'
t_PONTO = r'\.'
t_ASPADUPLA = r'\"'
t_FIMCOMANDO = r'\^'
t_VIRGULA = r'\,'
t_ASPASSIMPLES = r'\''

digit = r'([0-9])'
nondigitMin = r'([_a-z])'
nondigitMax = r'([_A-Z])'
#identifier = r'(' + nondigitMin + r'(' + digit + r'|' + nondigitMin + r' | ' + nondigitMax + r')*)'


# Define the regular expression for STRING
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

def t_STRING(t):
    r'\".*?\"'
    t.type = (t.type)
    return t

# alterar para receber antes identificador de tipo
#@TOKEN(identifier)
def t_NAME_VAR(t):
    r'intero'
    t.type = reserved.get(t.value,'INT')
    r'[a-z][a-zA-Z0-9]*'
    t.type = 'NAME_VAR'
    return t

def t_DECIMAL(t):
    r'[0-9]+.[0-9]+'
    t.value = float(t.value)
    t.type = "DECIMAL"
    return t

# A regular expression rule with some action code
def t_INTEIRO(t):
    r'\d+'
    t.value = int(t.value)
    return t


# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# Regra de manipulação de EOF
# def t_eof(t):
#     # Obtenha mais entrada (Exemplo)
#     mais_input = input('Digite mais entrada: ')
#     if mais_input:
#         lexer.input(mais_input)
#         return lexer.token()
#     return None


# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

t_ignore_COMMENT = r'\@.*'

# Error handling rule
def t_error(t):
    
    #if t.type == "ID":
    #    print("Illegal sequence '%s'" % t.value)
        
    #else:
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    


# Build the lexer
lexer = lex.lex()

#ANALISE SINTATICAAAAAAAAA

# Adicione a produção para várias declarações
def p_code(p):
    '''
    code : code declaration
         | declaration
    '''
    pass

declared_variables = {}
def p_code(p):
    '''
    code : code declaration
         | declaration
    '''
    pass

def p_declaration(p):
    '''
    declaration : tipoVar ID ATRIBUICAO expression
    '''
    variable_type = p[1]
    variable_name = p[2]
    variable_value = p[4]

    # Verificar se a variável já foi declarada
    if variable_name in declared_variables:
        print(f"Error: Variable '{variable_name}' already declared.")
    else:
        # Verificar a compatibilidade do tipo
        if (variable_type == 'intero' and isinstance(variable_value, int)) or \
           (variable_type == 'real' and (isinstance(variable_value, int) or isinstance(variable_value, float))) or \
           (variable_type == 'caractere' and isinstance(variable_value, str)):
            declared_variables[variable_name] = {'type': variable_type, 'value': variable_value}
            print("Declaration:", variable_type, variable_name, "=", variable_value)
        else:
            print(f"Error: Incompatible type for variable '{variable_name}'. Expected {variable_type}, got {type(variable_value).__name__}.")

def p_expression(p):
    '''
    expression : expression SOMA term
               | expression SUBTRACAO term
               | term
    '''
    p[0] = p[1] if len(p) == 2 else p[1] + p[3]

def p_term(p):
    '''
    term : term MULTIPLICACAO factor
         | term DIVISAO factor
         | term RESTO factor
         | factor
    '''
    p[0] = p[1] if len(p) == 2 else p[1] * p[3]

def p_factor(p):
    '''
    factor : ID
           | INTEIRO
           | DECIMAL
           | STRING
    '''
    if p[1] in declared_variables:
        p[0] = declared_variables[p[1]]['value']
    else:
        p[0] = p[1]

def p_tipoVar(p):
    '''
    tipoVar : INT
           | FLOAT
           | CHAR
    '''
    p[0] = p[1]



parser = yacc.yacc()

data = '''
  real numeroReal = 1 * 9
  intero numeroInteiro = 42
  intero numeroInteiro2 = 23
  intero num = 1
  real somaVar = 3 + 2 * 2
  intero somaInteroVar = num + numeroInteiro / 1
  real realzin = 4.554344
  intero ddd = 2.5 / 2
  caractere ddddd = 3
  caractere charnovinho = "f"

'''


parser_result = parser.parse(data)