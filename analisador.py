import ply.lex as lex
import ply.yacc as yacc
import pickle

# Usada para o controle de variáveis já declaradas
symbol_table = {}

# Palavras reservadas
reserved = {
   'caso' : 'SE',
   'intao' : 'THEN',
   'naodeu' : 'SENAO',
   'e' : 'E',
   'ou': 'OU',
   'print': 'PRINT',
   'intero': 'INT',
   'real': 'FLOAT',
   'bool': 'BOOLEANO',
   'uai' : 'WHILE',
   'floop': 'FOR',
   'ate':'ATE',
}

# Lista de tokens -----------------------------------------------------
tokens = (
    'SE', 'THEN', 'SENAO', 'FIM',
    'NUMBER', 'DECIMAL',
    'IGUAL', 'MAIORIGUAL', 'MAIOR','MENORIGUAL', 'MENOR','DIFERENTE',
    'PRINT',
    'SOMA', 'SUBTRACAO', 'MULTIPLICACAO', 'DIVISAO', 'RESTO',
    'ABREPARENTESES', 'FECHAPARENTESES',
    'E', 'OU', 'NEGACAO',
    'ID', 'ATRIBUICAO', 'INT', 'FLOAT', 'BOOLEANO'
)

tokens += tuple(reserved.values())


# Regras de expressão regular para tokens simples -----------------------
t_IGUAL = r'=='
t_MAIORIGUAL = r'>='
t_MAIOR = r'>'
t_MENORIGUAL = r'<='
t_MENOR = r'<'
t_DIFERENTE = r'!!'
t_SOMA    = r'\+'
t_SUBTRACAO   = r'-'
t_MULTIPLICACAO   = r'\*'
t_DIVISAO  = r'/'
t_RESTO = r'\%'
t_ABREPARENTESES  = r'\('
t_FECHAPARENTESES  = r'\)'
t_NEGACAO = r'!'
t_FIM = r';'
t_ATRIBUICAO = r'='


# Ignorar espaços e tabs --------------------------------------------------
t_ignore = ' \t'
t_ignore_COMMENT = r'\@.*'


# Definição de mais regras -----------------------------------------------
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_DECIMAL(t):
    r'[0-9]+.[0-9]+'
    t.value = float(t.value)
    t.type = "DECIMAL"
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID') 
    return t


# Mensagem de erro para caracteres inválidos ---------------------------------
def t_error(t):
    print(f"Caractere inválido: '{t.value[0]}'")
    t.lexer.skip(1)


# Construção da árvore sintática abstrata ------------------------------------
precedence = (
    ('left', 'SOMA', 'SUBTRACAO'),
    ('left', 'MULTIPLICACAO', 'DIVISAO'),
)

def p_statement_declaration(p):
    '''
    statement : INT ID ATRIBUICAO condition
              | FLOAT ID ATRIBUICAO condition
              | BOOLEANO ID ATRIBUICAO condition
              | ID ATRIBUICAO condition
    '''

    if p.slice[1].type == "ID":
        variable_name = p[1]
        if variable_name in symbol_table:
            if p[2] == '=':
                new_value = p[3]
                current_type = symbol_table[variable_name]['type']
                current_value = symbol_table[variable_name]['value']

                # Verifica se o novo valor é idêntico ao valor atual e do mesmo tipo
                if isinstance(new_value, type(symbol_table[variable_name]['value'])):
                    symbol_table[variable_name]['value'] = new_value
                    print("Updated var:", variable_name, "=", new_value)       
                else:
                    print(f"Error: Incompatible type for variable '{variable_name}'. Expected {current_type}, got {type(new_value).__name__}.")
                    symbol_table[variable_name]['value'] = current_value
        else:
            print(f"Error: Variable {variable_name} not declared.") 
    else:
        variable_type = p[1]
        variable_name = p[2]
        variable_value = p[4]

        if variable_name in symbol_table:
            print(f"Error: Variable {variable_name} already declared.")
        else:
            if variable_name in symbol_table and variable_type:
                print(f"Error: Variable '{variable_name}' already declared.")
            else:
                if (variable_type == 'intero' and isinstance(variable_value, int)) or \
                (variable_type == 'real' and (isinstance(variable_value, int) or isinstance(variable_value, float))) or \
                (variable_type == 'bool' and (isinstance(variable_value, bool))) or \
                (variable_type == 'caractere' and isinstance(variable_value, str)):
                    symbol_table[variable_name] = {'type': variable_type, 'value': variable_value}
                    print("Declaration:", variable_type, variable_name, "=", variable_value)
                    p[0] = p[4]
                else:
                    print(f"Error: Incompatible type for variable '{variable_name}'. Expected {variable_type}, got {type(variable_value).__name__}.")
                    p[0] = "error"

def p_statement_if_else(p):
    '''
    statement : SE condition THEN block SENAO block FIM
              | SE condition THEN block FIM
    '''
    if len(p) == 8:
        # If-else statement
        if p[2]:
            p[0] = p[4]  # Execute the 'if' block
        else:
            p[0] = p[6]  # Execute the 'else' block
    else:
        # If statement without 'else'
        if p[2]:
            p[0] = p[4]  # Execute the 'if' block

# ...


def p_condition(p):
    '''
    condition : expression IGUAL expression
              | expression MAIOR expression
              | expression MAIORIGUAL expression
              | expression MENOR expression
              | expression MENORIGUAL expression
              | expression DIFERENTE expression
              | ABREPARENTESES condition FECHAPARENTESES
              | NEGACAO condition
              | condition E condition
              | condition OU condition
              | expression
              
    '''
    if len(p) == 2: p[0] = p[1]
    elif len(p) == 2:
        if isinstance(p[1], bool):  # Se for um valor booleano, não avalie como uma variável
            p[0] = p[1]
        elif p[1] in symbol_table:
            p[0] = symbol_table[p[1]]['value']
        else:
            p[0] = p[1]
    elif p[2] in ['>', '>=', '==', '<', '<=', '!!']:
        operator = p[2] # sem ser comentário
        if operator == '!!': p[0] = p[1] != p[3]
        else: p[0] = eval(f"{p[1]} {operator} {p[3]}")
    elif p[2] in ['e', 'ou']: 
        operator = p[2]
        if operator == 'e': p[0] =  p[1] and p[3]
        if operator == 'ou': p[0] = p[1] or p[3]
    elif len(p) == 3 and p[1] == '!':
        p[0] = not p[2]
    elif p[1] == '(' and p[3] == ')':
            p[0] = p[2]
    else:
        print("Erro: Parênteses desequilibrados.")



def p_statement_print(p):
    '''
    statement : PRINT expression
    ''' 
    p[0] = f'-> {p[2]}'


def p_block(p):
    '''
    block : statement 
          | block statement
    '''

    if len(p) == 3:
        if isinstance(p[1], list):
            p[0] = [elem for elem in p[1] + [p[2]] if elem != '^']
        else:
            p[0] = [elem for elem in [p[1], p[2]] if elem != '^']
    elif len(p) == 2:
        p[0] = [p[1]] if p[1] != '^' else []
    else:
        p[0] = [elem for elem in p[1] + [p[2]] if elem != '^']


def p_expression(p):
    '''
    expression : NUMBER
               | DECIMAL
               | ID
               | expression SOMA expression
               | expression SUBTRACAO expression
               | expression MULTIPLICACAO expression
               | expression DIVISAO expression
               | expression RESTO expression
               | ABREPARENTESES expression FECHAPARENTESES
    '''

    if len(p) == 2:
        if isinstance(p[1], str):
            p[0] = symbol_table[p[1]]['value'] if p[1] in symbol_table else p[1]
        else:
            p[0] = p[1]
    elif len(p) == 4:
        if p[1] == '(' and p[3] == ')':
            p[0] = p[2]
        else:
            operator = p[2]
            left = symbol_table[p[1]]['value'] if isinstance(p[1], str) else p[1]
            right = symbol_table[p[3]]['value'] if isinstance(p[3], str) else p[3]
            p[0] = eval(f"{left} {operator} {right}")


def p_statement_while(p):
    '''
    statement : WHILE condition THEN block FIM
    '''
    condition = p[2]
    while condition:
         for statement in p[4]:
            eval(statement)

        
def p_statement_for(p):
    '''
    statement : FOR ID expression ATE expression THEN block FIM
    '''

    start_value = p[3]
    end_value = p[5]
    step_value = p[7] if len(p) == 12 else 1
    
    symbol_table[p[2]] = {'type': 'int', 'value': start_value}

    while (step_value > 0 and symbol_table[p[2]]['value'] <= end_value) or (step_value < 0 and symbol_table[p[2]]['value'] >= end_value):
        # Execute the block inside the loop
        for sub_statement in p[7]:
            if isinstance(sub_statement, str):
                print(sub_statement)
            elif isinstance(sub_statement, list):
                for sub_sub_statement in sub_statement:
                    if isinstance(sub_sub_statement, str):
                        print(sub_sub_statement)

        # Move the variable update outside the loop
        symbol_table[p[2]]['value'] += step_value


# Tratamento de erros sintáticos
def p_error(p):
    if p:
        print(f"Erro de sintaxe em '{p.value}'")
    else:
        print("Erro de sintaxe no final da entrada")


# Inicialização do lexer e parser
lexer = lex.lex()
parser = yacc.yacc()

# Importando código
with open('code_example.uai', 'r') as file:
    data = file.read()

# Gerando Token
lexer.input(data)
with open('token.txt', 'w') as output_file:
    for token in lexer:
        print(token, file=output_file)



result = parser.parse(data)
print(result)