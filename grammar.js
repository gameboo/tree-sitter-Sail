// general parsing helpers
////////////////////////////////////////////////////////////////////////////////
sepList1 = (sep, item) => seq(item, repeat(seq(sep, item)))
commaSepList1 = item => sepList1(',', item)
sepList = (sep, item) => optional(sepList1(sep, item))
commaSepList = item => sepList(',', item)

// Sail grammar
////////////////////////////////////////////////////////////////////////////////
module.exports = grammar({
  name: 'Sail'
, rules: {
  // literal
    lit: $ => choice( 'true', 'false'
                    , '()'
                    , $._number
                    , 'undefined'
                    , 'bitzero'
                    , $._binary_literal
                    , $._hexadecimal_literal
                    , $._string_literal )
  // kind
  , kind: $ => choice('Int', 'Type', 'Order', 'Bool')
  , kopt: $ =>
      choice( seq('(', optional('constant'), $.typ_var, ':', $.kind, ')')
            , $.typ_var )
  , quantifier: $ => seq($.kopt, optional($.typ))
  // type schem
  , typ_var: $ => $._type_variable
  , typ_list: $ => commaSepList1($.typ)
  , tyarg: $ => seq('(', $.typ_list, ')')
  , typschm: $ => choice(
      seq($.typ, '->', $.typ)
    , seq('forall', $.quantifier, '.', $.typ, '->', $.typ)
    , seq($.typ, '<->', $.typ)
    , seq('forall', $.quantifier, '.', $.typ, '<->', $.typ)
    )
  , prefix_typ_op: $ => choice('2^', '-', '*')
  , typ: $ => seq( optional($.prefix_typ_op)
                 , $.atomic_typ
                 , optional(seq( '('
                               , $._op_no_caret
                               , optional($.prefix_typ_op)
                               , $.atomic_typ
                               , ')'))
                 )
  , atomic_typ: $ => choice( $._id
                           , '_'
                           , $.typ_var
                           , $.lit
                           , 'dec'
                           , 'inc'
                           , seq($._id, $.tyarg)
                           , seq('register', '(', $.typ, ')')
                           , seq('(', commaSepList1($.typ), ')')
                           , seq('{', commaSepList1($._number), '}')
                           , seq('{', $.kopt, '.', $.typ, '}')
                           , seq('{', $.kopt, ',', $.typ, '.', $.typ, '}')
                           )
  // val def
  , val_spec_def: $ =>
      seq( 'val'
         , choice(
             seq(field('name', $._id), optional(seq('=', $.externs)))
           , $._string_literal )
         , ':', $.typschm )
  , externs: $ => choice(
      $._string_literal
    , seq('{', commaSepList1($.extern_binding), '}')
    , seq(choice('monadic', 'pure'), $._string_literal)
    , seq(choice('monadic', 'pure'), '{', commaSepList1($.extern_binding), '}')
    )
  , extern_binding: $ => seq(choice($._id, '_'), ':', $._string_literal)
  // literals
  , _string_literal: $ => /\".*\"/
  , _number: $ => /[0-9]+/
  , _binary_literal: $ => /0b[0-1_]+/
  , _hexadecimal_literal: $ => /0x[A-Fa-f0-9_]+/
  // operators
  , _operator: $ => choice( '^' // 8
                          , '*', '/', '%' // 7
                          , '+', '-' // 6
                          , '<', '<=', '>', '>=', '!=', '=', '=='// 4
                          , '&' // 3
                          , '|' // 2
                          )
  , _op_no_caret: $ => choice( $._operator
                             , '-'
                             , '|'
                             , '*'
                             , 'in' )
  , _op: $ => choice( $._operator
                    , '-'
                    , '|'
                    , '^'
                    , '*'
                    , 'in' )
  // identifiers
  , _identifier: $ => /[a-z][a-zA-Z0-9$_]*/
  , _id: $ => choice( $._identifier
                    , seq('operator', $._operator)
                    , seq('operator', '-')
                    , seq('operator', '|')
                    , seq('operator', '^')
                    , seq('operator', '*')
                    )
  // type variable
  , _type_variable: $ => seq("'", $._identifier)
  // comments
  , _line_comment: $ => seq('//', /.*\n/)
  , _block_comment: $ => seq('/*', repeat(/./), '*/')
  }
, extras: $ => [/\s/, $._line_comment, $._block_comment]
});



/*
 *
 *<id> ::= ID
       | operator OPERATOR
       | operator -
       | operator |
       | operator ^
       | operator *

<op_no_caret> ::= OPERATOR
                | -
                | |
                | *
                | in

<op> ::= OPERATOR
       | -
       | |
       | ^
       | *
       | in

<exp_op> ::= OPERATOR
           | -
           | |
           | @
           | ::
           | ^
           | *

<pat_op> ::= @
           | ::
           | ^

<typ_var> ::= TYPE_VARIABLE

<tyarg> ::= ( <typ_list> )


<prefix_typ_op> ::= epsilon
                  | 2^
                  | -
                  | *

<postfix_typ> ::= <atomic_typ>

<typ_no_caret> ::= <prefix_typ_op> <postfix_typ> (<op_no_caret> <prefix_typ_op> <postfix_typ>)*

<typ> ::= <prefix_typ_op> <postfix_typ> (<op> <prefix_typ_op> <postfix_typ>)*

<atomic_typ> ::= <id>
               | _
               | <typ_var>
               | <lit>
               | dec
               | inc
               | <id> <tyarg>
               | register ( <typ> )
               | ( <typ> )
               | ( <typ> , <typ_list> )
               | { NUMBER (, NUMBER)* }
               | { <kopt> . <typ> }
               | { <kopt> , <typ> . <typ> }

<typ_list> ::= <typ> [,]
             | <typ> , <typ_list>

<kind> ::= Int
         | Type
         | Order
         | Bool

<kopt> ::= ( constant <typ_var> : <kind> )
         | ( <typ_var> : <kind> )
         | <typ_var>

<quantifier> ::= <kopt> , <typ>
             | <kopt>


<effect> ::= <id>

<effect_set> ::= { <effect> (, <effect>)* }
               | pure

<typschm> ::= <typ> -> <typ>
            | forall <quantifier> . <typ> -> <typ>
            | <typ> <-> <typ>
            | forall <quantifier> . <typ> <-> <typ>


<pat1> ::= <atomic_pat> (<pat_op> <atomic_pat>)*

<pat> ::= <pat1>
        | $[ATTRIBUTE] <pat>
        | <pat1> as <typ>

<pat_list> ::= <pat> [,]
             | <pat> , <pat_list>

<atomic_pat> ::= _
               | <lit>
               | <id>
               | <typ_var>
               | <id> ()
               | <id> [ NUMBER ]
               | <id> [ NUMBER .. NUMBER ]
               | <id> ( <pat_list> )
               | <atomic_pat> : <typ_no_caret>
               | ( <pat> )
               | ( <pat> , <pat_list> )
               | [ <pat_list> ]
               | [| |]
               | [| <pat_list> |]
               | struct { <fpat> (, <fpat>)* }

<fpat> ::= <id> = <pat>
         | <id>
         | _

<lit> ::= true
        | false
        | ()
        | NUMBER
        | undefined
        | bitzero
        | bitone
        | BINARY_LITERAL
        | HEXADECIMAL_LITERAL
        | STRING_LITERAL


<exp> ::= <exp0>
        | $[ATTRIBUTE] <exp>
        | <exp0> = <exp>
        | let <letbind> in <exp>
        | var <atomic_exp> = <exp> in <exp>
        | { <block> }
        | return <exp>
        | throw <exp>
        | if <exp> then <exp> else <exp>
        | if <exp> then <exp>
        | match <exp> { <case_list> }
        | try <exp> catch { <case_list> }
        | foreach ( <id> ID <atomic_exp> ID <atomic_exp> by <atomic_exp> in <typ> ) <exp>
        | foreach ( <id> ID <atomic_exp> ID <atomic_exp> by <atomic_exp> ) <exp>
        | foreach ( <id> ID <atomic_exp> ID <atomic_exp> ) <exp>
        | repeat [termination_measure { <exp> }] <exp> until <exp>
        | while [termination_measure { <exp> }] <exp> do <exp>

<prefix_op> ::= epsilon
              | 2^
              | -
              | *

<exp0> ::= <prefix_op> <atomic_exp> (<exp_op> <prefix_op> <atomic_exp>)*

<case> ::= <pat> => <exp>
         | <pat> if <exp> => <exp>

<case_list> ::= <case>
              | <case> ,
              | <case> , <case_list>

<block> ::= <exp> [;]
          | let <letbind> [;]
          | let <letbind> ; <block>
          | var <atomic_exp> = <exp> [;]
          | var <atomic_exp> = <exp> ; <block>
          | <exp> ; <block>

<letbind> ::= <pat> = <exp>

<atomic_exp> ::= <atomic_exp> : <atomic_typ>
               | <lit>
               | <id> -> <id> ()
               | <id> -> <id> ( <exp_list> )
               | <atomic_exp> . <id> ()
               | <atomic_exp> . <id> ( <exp_list> )
               | <atomic_exp> . <id>
               | <id>
               | <typ_var>
               | ref <id>
               | <id> ()
               | <id> ( <exp_list> )
               | sizeof ( <typ> )
               | constraint ( <typ> )
               | <atomic_exp> [ <exp> ]
               | <atomic_exp> [ <exp> .. <exp> ]
               | <atomic_exp> [ <exp> , <exp> ]
               | struct { <fexp_exp_list> }
               | { <exp> with <fexp_exp_list> }
               | [ ]
               | [ <exp_list> ]
               | [ <exp> with <vector_update> (, <vector_update>)* ]
               | [| |]
               | [| <exp_list> |]
               | ( <exp> )
               | ( <exp> , <exp_list> )

<fexp_exp> ::= <atomic_exp> = <exp>
             | <id>

<fexp_exp_list> ::= <fexp_exp>
                  | <fexp_exp> ,
                  | <fexp_exp> , <fexp_exp_list>

<exp_list> ::= <exp> [,]
             | <exp> , <exp_list>

<vector_update> ::= <atomic_exp> = <exp>
                  | <atomic_exp> .. <atomic_exp> = <exp>
                  | <id>

<funcl_annotation> ::= Private
                     | $[ATTRIBUTE]

<funcl_patexp> ::= <pat> = <exp>
                 | ( <pat> if <exp> ) = <exp>

<funcl_patexp_typ> ::= <pat> = <exp>
                     | <pat> -> <typ> = <exp>
                     | forall <quantifier> . <pat> -> <typ> = <exp>
                     | ( <pat> if <exp> ) = <exp>
                     | ( <pat> if <exp> ) -> <typ> = <exp>
                     | forall <quantifier> . ( <pat> if <exp> ) -> <typ> = <exp>

<funcl> ::= <funcl_annotation> <id> <funcl_patexp>
          | <id> <funcl_patexp>

<funcls> ::= <funcl_annotation> <id> <funcl_patexp_typ>
           | <id> <funcl_patexp_typ>
           | <funcl_annotation> <id> <funcl_patexp> and <funcl> (and <funcl>)*
           | <id> <funcl_patexp> and <funcl> (and <funcl>)*

<funcl_typ> ::= forall <quantifier> . <typ>
              | <typ>

<paren_index_range> ::= ( <paren_index_range> @ <paren_index_range> (@ <paren_index_range>)* )
                      | <atomic_index_range>

<atomic_index_range> ::= <typ>
                       | <typ> .. <typ>
                       | ( <typ> .. <typ> )

<r_id_def> ::= <id> : <paren_index_range> (@ <paren_index_range>)*

<r_def_body> ::= <r_id_def>
               | <r_id_def> ,
               | <r_id_def> , <r_def_body>

<param_kopt> ::= <typ_var> : <kind>
               | <typ_var>

<typaram> ::= ( <param_kopt> (, <param_kopt>)* ) , <typ>
            | ( <param_kopt> (, <param_kopt>)* )

<type_def> ::= type <id> <typaram> = <typ>
             | type <id> = <typ>
             | type <id> <typaram> -> <kind> = <typ>
             | type <id> : <kind> = <typ>
             | struct <id> = { <struct_fields> }
             | struct <id> <typaram> = { <struct_fields> }
             | enum <id> = <id> (| <id>)*
             | enum <id> = { <enum_comma> }
             | enum <id> with <enum_functions> = { <enum_comma> }
             | union <id> = { <type_unions> }
             | union <id> <typaram> = { <type_unions> }
             | bitfield <id> : <typ> = { <r_def_body> }

<enum_functions> ::= <id> -> <typ> , <enum_functions>
                   | <id> -> <typ> ,
                   | <id> -> <typ>

<enum_comma> ::= <id> [,]
         | <id> => <exp> [,]
         | <id> , <enum_comma>
         | <id> => <exp> , <enum_comma>

<struct_field> ::= <id> : <typ>

<struct_fields> ::= <struct_field>
                  | <struct_field> ,
                  | <struct_field> , <struct_fields>

<type_union> ::= Private <type_union>
               | $[ATTRIBUTE] <type_union>
               | <id> : <typ>
               | <id> : { <struct_fields> }

<type_unions> ::= <type_union>
                | <type_union> ,
                | <type_union> , <type_unions>

<rec_measure> ::= { <pat> => <exp> }

<fun_def> ::= function [<rec_measure>] <funcls>

<mpat> ::= <atomic_mpat> (<pat_op> <atomic_mpat>)*
         | <atomic_mpat> as <id>

<atomic_mpat> ::= <lit>
                | <id>
                | <id> [ NUMBER ]
                | <id> [ NUMBER .. NUMBER ]
                | <id> ()
                | <id> ( <mpat> (, <mpat>)* )
                | ( <mpat> )
                | ( <mpat> , <mpat> (, <mpat>)* )
                | [ <mpat> (, <mpat>)* ]
                | [| |]
                | [| <mpat> (, <mpat>)* |]
                | <atomic_mpat> : <typ_no_caret>
                | struct { <fmpat> (, <fmpat>)* }

<fmpat> ::= <id> = <mpat>
          | <id>

<mpexp> ::= <mpat>
          | <mpat> if <exp>

<mapcl> ::= $[ATTRIBUTE] <mapcl>
          | <mpexp> <-> <mpexp>
          | <mpexp> => <exp>
          | forwards <mpexp> => <exp>
          | backwards <mpexp> => <exp>

<mapcl_list> ::= <mapcl> [,]
               | <mapcl> , <mapcl_list>

<map_def> ::= mapping <id> = { <mapcl_list> }
            | mapping <id> : <typschm> = { <mapcl_list> }

<let_def> ::= let <letbind>


<pure_opt> ::= monadic
             | pure

<extern_binding> ::= <id> : STRING_LITERAL
                   | _ : STRING_LITERAL

<externs> ::= epsilon
            | = STRING_LITERAL
            | = { <extern_binding> (, <extern_binding>)* }
            | = <pure_opt> STRING_LITERAL
            | = <pure_opt> { <extern_binding> (, <extern_binding>)* }

<val_spec_def> ::= val STRING_LITERAL : <typschm>
                 | val <id> <externs> : <typschm>

<register_def> ::= register <id> : <typ>
                 | register <id> : <typ> = <exp>

<default_def> ::= default <kind> inc
                | default <kind> dec

<scattered_def> ::= scattered enum <id>
                  | scattered union <id> <typaram>
                  | scattered union <id>
                  | scattered function <id>
                  | scattered mapping <id>
                  | scattered mapping <id> : <funcl_typ>
                  | enum clause <id> = <id>
                  | function clause <funcl>
                  | union clause <id> = <type_union>
                  | mapping clause <id> = <mapcl>
                  | end <id>

<loop_measure> ::= until <exp>
                 | repeat <exp>
                 | while <exp>

<subst> ::= <typ_var> = <typ>
          | <id> = <id>

<instantiation_def> ::= instantiation <id>
                      | instantiation <id> with <subst> (, <subst>)*

<overload_def> ::= overload <id> = { <id> (, <id>)* }
                 | overload <id> = <id> (| <id>)*

<def_aux> ::= <fun_def>
            | <map_def>
            | FIXITY_DEF
            | <val_spec_def>
            | <instantiation_def>
            | <type_def>
            | <let_def>
            | <register_def>
            | <overload_def>
            | <scattered_def>
            | <default_def>
            | $LINE_DIRECTIVE
            | termination_measure <id> <pat> = <exp>
            | termination_measure <id> <loop_measure> (, <loop_measure>)*

<def> ::= Private <def>
        | $[ATTRIBUTE] <def>
        | <def_aux>

 *
 * */
