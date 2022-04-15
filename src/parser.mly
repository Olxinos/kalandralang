%{
  open AST

  let node node =
    {
      loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ();
      node;
    }
%}

%token COLON AND OR NOT PLUS DOT_DOT TRUE FALSE EOF COMMA
%token EQUAL DOUBLEEQUAL GREATER GREATEREQUAL LESS LESSEQUAL
%token ASTERISK MINUS SLASH
%token GETMIN GETMAX BASE RETURN FUNCTION
%token BUY ILVL WITH FRACTURED FOR CRAFT ECHO SHOW SHOW_MOD_POOL
%token SHAPER ELDER CRUSADER HUNTER REDEEMER WARLORD EXARCH EATER SYNTHESIZED
%token IF THEN ELSE UNTIL REPEAT WHILE DO GOTO STOP SET_ASIDE SWAP USE_IMPRINT GAIN HAS
%token PREFIX_COUNT NO_PREFIX OPEN_PREFIX FULL_PREFIXES
%token SUFFIX_COUNT NO_SUFFIX OPEN_SUFFIX FULL_SUFFIXES
%token LPAR RPAR LBRACE RBRACE
%token <AST.currency> CURRENCY
%token <Fossil.t> FOSSIL
%token <string> STRING LABEL NAME
%token <int> INT

%left OR
%left AND
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc NOT

%type <AST.t> program
%start program
%%

amount:
| INT CURRENCY amount
  { ($1, $2) :: $3 }
| INT CURRENCY
  { [ $1, $2 ] }

influence:
| SHAPER { Influence.SEC Shaper }
| ELDER { Influence.SEC Elder }
| CRUSADER { Influence.SEC Crusader }
| HUNTER { Influence.SEC Hunter }
| REDEEMER { Influence.SEC Redeemer }
| WARLORD { Influence.SEC Warlord }
| EXARCH { Influence.Exarch }
| EATER { Influence.Eater }
| SYNTHESIZED { Influence.Synthesized }

buy_arguments:
| influence buy_arguments
  { BA_influence $1 :: $2 }
| STRING buy_arguments
  { BA_base (Id.make $1) :: $2 }
| ILVL INT buy_arguments
  { BA_ilvl $2 :: $3 }
| WITH STRING buy_arguments
  { BA_with { modifier = Id.make $2; fractured = false } :: $3 }
| WITH FRACTURED STRING buy_arguments
  { BA_with { modifier = Id.make $3; fractured = true } :: $4 }
| FOR amount buy_arguments
  { BA_for $2 :: $3 }
|
  { [] }

non_empty_expression_list:
| arithmetic_expression COMMA non_empty_expression_list
  { $1::$3 }
| arithmetic_expression
  { [$1] }

expression_list:
| non_empty_expression_list
  { $1 }
|
  { [] }

arithmetic_expression:
| INT
  { Constant $1 }
| NAME LPAR expression_list RPAR
  { Function_call ($1, $3) }
| NAME
  { Variable $1 }
| GETMIN STRING
  { Get_min (Id.make $2) }
| GETMAX STRING
  { Get_max (Id.make $2) }
| BASE STRING
  { Base (Id.make $2) }
| arithmetic_expression PLUS arithmetic_expression
  { Sum ($1, $3) }
| arithmetic_expression ASTERISK arithmetic_expression
  { Product ($1, $3) }
| arithmetic_expression MINUS arithmetic_expression
  { Difference ($1, $3) }
| arithmetic_expression SLASH arithmetic_expression
  { Quotient ($1, $3) }
| LPAR arithmetic_expression RPAR
  { $2 }

condition:
| TRUE
  { True }
| FALSE
  { False }
| NOT condition
  { Not $2 }
| condition AND condition
  { And ($1, $3) }
| condition OR condition
  { Or ($1, $3) }
| HAS STRING
  { Has (Id.make $2) }
| PREFIX_COUNT INT
  { Prefix_count ($2, $2) }
| PREFIX_COUNT INT DOT_DOT INT
  { Prefix_count ($2, $4) }
| NO_PREFIX
  { Prefix_count (0, 0) }
| OPEN_PREFIX
  { Open_prefix }
| FULL_PREFIXES
  { Full_prefixes }
| SUFFIX_COUNT INT
  { Suffix_count ($2, $2) }
| SUFFIX_COUNT INT DOT_DOT INT
  { Suffix_count ($2, $4) }
| NO_SUFFIX
  { Suffix_count (0, 0) }
| OPEN_SUFFIX
  { Open_suffix }
| FULL_SUFFIXES
  { Full_suffixes }
| LPAR condition RPAR
  { $2 }
| arithmetic_expression DOUBLEEQUAL arithmetic_expression
  { Is_equal ($1, $3) }
| arithmetic_expression GREATER arithmetic_expression
  { Greater_than ($1, $3) }
| arithmetic_expression GREATEREQUAL arithmetic_expression
  { Greater_equal ($1, $3) }
| arithmetic_expression LESS arithmetic_expression
  { Less_than ($1, $3) }
| arithmetic_expression LESSEQUAL arithmetic_expression
  { Less_equal ($1, $3) }

plus_fossils:
| PLUS FOSSIL plus_fossils
  { $2 :: $3 }
|
  { [] }

simple_instruction:
| LABEL COLON
  { node @@ Label (AST.Label.make $1) }
| GOTO LABEL
  { node @@ Simple (Goto (AST.Label.make $2)) }
| STOP
  { node @@ Simple Stop }
| BUY buy_arguments
  { node @@ Simple (Buy (AST.make_buy $2)) }
| CURRENCY
  { node @@ Simple (Apply $1) }
| FOSSIL plus_fossils
  { node @@ Simple (Apply (Fossils ($1 :: $2))) }
| CRAFT STRING
  { node @@ Simple (Apply (Craft (Id.make $2))) }
| SET_ASIDE
  { node @@ Simple Set_aside }
| SWAP
  { node @@ Simple Swap }
| USE_IMPRINT
  { node @@ Simple Use_imprint }
| GAIN amount
  { node @@ Simple (Gain $2) }
| ECHO STRING
  { node @@ Simple (Echo $2) }
| ECHO arithmetic_expression
  { node @@ Simple (Echo_int $2) }
| SHOW
  { node @@ Simple Show }
| SHOW_MOD_POOL
  { node @@ Simple Show_mod_pool }
| LBRACE instructions RBRACE
  { $2 }
| RETURN arithmetic_expression
  { node @@ Simple (Return $2) }
| NAME EQUAL arithmetic_expression
  { node @@ Simple (Assignment ($1, $3)) }

name_list:
| NAME name_list
  { $1::$2 }
|
  { [] }

function_declaration:
| FUNCTION NAME name_list EQUAL simple_instruction
  { ($2, $3, $5) }

preamble:
| function_declaration preamble
  { $1::$2 }
|
  { [] }

instruction:
| simple_instruction
  { $1 }
| IF condition THEN simple_instruction
  { node @@ If ($2, $4, None) }
| IF condition THEN simple_instruction ELSE simple_instruction
  { node @@ If ($2, $4, Some $6) }
| UNTIL condition DO simple_instruction
  { node @@ Until ($2, $4) }
| WHILE condition DO simple_instruction
  { node @@ While ($2, $4) }
| REPEAT simple_instruction UNTIL condition
  { node @@ Repeat ($2, $4) }

instructions:
| instruction instructions
  { node @@ Seq ($1, $2) }
|
  { node @@ Noop }

program:
| preamble instructions EOF
  { ($1, $2) }
