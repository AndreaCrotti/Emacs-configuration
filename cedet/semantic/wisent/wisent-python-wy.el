;;; wisent-python-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003, 2004, 2007 Richard Kim

;; Author: andrea <andrea@ip1-201.halifax.rwth-aachen.de>
;; Created: 2011-02-26 20:43:28+0100
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file wisent-python.wy.

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-python-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("and" . AND)
     ("as" . AS)
     ("assert" . ASSERT)
     ("break" . BREAK)
     ("class" . CLASS)
     ("continue" . CONTINUE)
     ("def" . DEF)
     ("del" . DEL)
     ("elif" . ELIF)
     ("else" . ELSE)
     ("except" . EXCEPT)
     ("exec" . EXEC)
     ("finally" . FINALLY)
     ("for" . FOR)
     ("from" . FROM)
     ("global" . GLOBAL)
     ("if" . IF)
     ("import" . IMPORT)
     ("in" . IN)
     ("is" . IS)
     ("lambda" . LAMBDA)
     ("not" . NOT)
     ("or" . OR)
     ("pass" . PASS)
     ("print" . PRINT)
     ("raise" . RAISE)
     ("return" . RETURN)
     ("try" . TRY)
     ("while" . WHILE)
     ("with" . WITH)
     ("yield" . YIELD))
   '(("yield" summary "Create a generator function")
     ("with" summary "Start statement with an associated context object")
     ("while" summary "Start a 'while' loop")
     ("try" summary "Start of statements protected by exception handlers")
     ("return" summary "Return from a function")
     ("raise" summary "Raise an exception")
     ("print" summary "Print each argument to standard output")
     ("pass" summary "Statement that does nothing")
     ("or" summary "Binary logical 'or' operator")
     ("not" summary "Unary boolean negation operator")
     ("lambda" summary "Create anonymous function")
     ("is" summary "Binary operator that tests for object equality")
     ("in" summary "Part of 'for' statement ")
     ("import" summary "Load specified modules")
     ("if" summary "Start 'if' conditional statement")
     ("global" summary "Declare one or more symbols as global symbols")
     ("from" summary "Modify behavior of 'import' statement")
     ("for" summary "Start a 'for' loop")
     ("finally" summary "Specify code to be executed after 'try' statements whether or not an exception occurred")
     ("exec" summary "Dynamically execute Python code")
     ("except" summary "Specify exception handlers along with 'try' keyword")
     ("else" summary "Start the 'else' clause following an 'if' statement")
     ("elif" summary "Shorthand for 'else if' following an 'if' statement")
     ("del" summary "Delete specified objects, i.e., undo what assignment did")
     ("def" summary "Define a new function")
     ("continue" summary "Skip to the next iteration of enclosing 'for' or 'while' loop")
     ("class" summary "Define a new class")
     ("break" summary "Terminate 'for' or 'while' loop")
     ("assert" summary "Raise AssertionError exception if <expr> is false")
     ("as" summary "EXPR as NAME makes value of EXPR available as variable NAME")
     ("and" summary "Logical AND binary operator ... ")))
  "Table of language keywords.")

(defconst wisent-python-wy--token-table
  (semantic-lex-make-type-table
   '(("symbol"
      (NAME))
     ("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("punctuation"
      (AT . "@")
      (BACKQUOTE . "`")
      (ASSIGN . "=")
      (COMMA . ",")
      (SEMICOLON . ";")
      (COLON . ":")
      (BAR . "|")
      (TILDE . "~")
      (PERIOD . ".")
      (MINUS . "-")
      (PLUS . "+")
      (MOD . "%")
      (DIV . "/")
      (MULT . "*")
      (AMP . "&")
      (GT . ">")
      (LT . "<")
      (HAT . "^")
      (NE . "!=")
      (LTGT . "<>")
      (HATEQ . "^=")
      (OREQ . "|=")
      (AMPEQ . "&=")
      (MODEQ . "%=")
      (DIVEQ . "/=")
      (MULTEQ . "*=")
      (MINUSEQ . "-=")
      (PLUSEQ . "+=")
      (LE . "<=")
      (GE . ">=")
      (EQ . "==")
      (EXPONENT . "**")
      (GTGT . ">>")
      (LTLT . "<<")
      (DIVDIV . "//")
      (DIVDIVEQ . "//=")
      (EXPEQ . "**=")
      (GTGTEQ . ">>=")
      (LTLTEQ . "<<="))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)"))
     ("indentation"
      (INDENT_BLOCK . "(INDENT DEDENT)")
      (DEDENT . "[^:INDENT:]")
      (INDENT . "^\\s-+"))
     ("newline"
      (NEWLINE . "\n"))
     ("charquote"
      (BACKSLASH . "\\")))
   '(("keyword" :declared t)
     ("symbol" :declared t)
     ("number" :declared t)
     ("punctuation" :declared t)
     ("block" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-python-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((BACKSLASH NEWLINE INDENT DEDENT INDENT_BLOCK PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LTLTEQ GTGTEQ EXPEQ DIVDIVEQ DIVDIV LTLT GTGT EXPONENT EQ GE LE PLUSEQ MINUSEQ MULTEQ DIVEQ MODEQ AMPEQ OREQ HATEQ LTGT NE HAT LT GT AMP MULT DIV MOD PLUS MINUS PERIOD TILDE BAR COLON SEMICOLON COMMA ASSIGN BACKQUOTE AT STRING_LITERAL NUMBER_LITERAL NAME AND AS ASSERT BREAK CLASS CONTINUE DEF DEL ELIF ELSE EXCEPT EXEC FINALLY FOR FROM GLOBAL IF IMPORT IN IS LAMBDA NOT OR PASS PRINT RAISE RETURN TRY WHILE WITH YIELD)
       nil
       (goal
	((NEWLINE))
	((simple_stmt))
	((compound_stmt)))
       (simple_stmt
	((small_stmt_list semicolon_opt NEWLINE)))
       (small_stmt_list
	((small_stmt))
	((small_stmt_list SEMICOLON small_stmt)))
       (small_stmt
	((expr_stmt))
	((print_stmt))
	((del_stmt))
	((pass_stmt))
	((flow_stmt))
	((import_stmt))
	((global_stmt))
	((exec_stmt))
	((assert_stmt)))
       (print_stmt
	((PRINT print_stmt_trailer)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (print_stmt_trailer
	((test_list_opt)
	 nil)
	((GTGT test trailing_test_list_with_opt_comma_opt)
	 nil))
       (trailing_test_list_with_opt_comma_opt
	(nil)
	((trailing_test_list comma_opt)
	 nil))
       (trailing_test_list
	((COMMA test)
	 nil)
	((trailing_test_list COMMA test)
	 nil))
       (expr_stmt
	((testlist expr_stmt_trailer)
	 (if
	     (and $2
		  (stringp $1)
		  (string-match "^\\(\\sw\\|\\s_\\)+$" $1))
	     (wisent-raw-tag
	      (semantic-tag-new-variable $1 nil nil))
	   (wisent-raw-tag
	    (semantic-tag-new-code $1 nil)))))
       (expr_stmt_trailer
	((augassign testlist))
	((eq_testlist_zom)))
       (eq_testlist_zom
	(nil)
	((eq_testlist_zom ASSIGN testlist)
	 (identity $3)))
       (augassign
	((PLUSEQ))
	((MINUSEQ))
	((MULTEQ))
	((DIVEQ))
	((MODEQ))
	((AMPEQ))
	((OREQ))
	((HATEQ))
	((LTLTEQ))
	((GTGTEQ))
	((EXPEQ))
	((DIVDIVEQ)))
       (del_stmt
	((DEL exprlist)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (exprlist
	((expr_list comma_opt)
	 nil))
       (expr_list
	((expr)
	 nil)
	((expr_list COMMA expr)
	 nil))
       (pass_stmt
	((PASS)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (flow_stmt
	((break_stmt))
	((continue_stmt))
	((return_stmt))
	((raise_stmt))
	((yield_stmt)))
       (break_stmt
	((BREAK)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (continue_stmt
	((CONTINUE)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (return_stmt
	((RETURN testlist_opt)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (testlist_opt
	(nil)
	((testlist)
	 nil))
       (yield_stmt
	((YIELD)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil)))
	((YIELD testlist)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (raise_stmt
	((RAISE zero_one_two_or_three_tests)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (zero_one_two_or_three_tests
	(nil)
	((test zero_one_or_two_tests)
	 nil))
       (zero_one_or_two_tests
	(nil)
	((COMMA test zero_or_one_comma_test)
	 nil))
       (zero_or_one_comma_test
	(nil)
	((COMMA test)
	 nil))
       (import_stmt
	((IMPORT dotted_as_name_list)
	 (wisent-raw-tag
	  (semantic-tag-new-include $2 nil)))
	((FROM dotted_name IMPORT star_or_import_as_name_list)
	 (wisent-raw-tag
	  (semantic-tag-new-include $2 nil))))
       (dotted_as_name_list
	((dotted_as_name))
	((dotted_as_name_list COMMA dotted_as_name)))
       (star_or_import_as_name_list
	((MULT)
	 nil)
	((import_as_name_list)
	 nil))
       (import_as_name_list
	((import_as_name)
	 nil)
	((import_as_name_list COMMA import_as_name)
	 nil))
       (import_as_name
	((NAME as_name_opt)
	 nil))
       (dotted_as_name
	((dotted_name as_name_opt)))
       (as_name_opt
	(nil)
	((AS NAME)
	 (identity $2)))
       (dotted_name
	((NAME))
	((dotted_name PERIOD NAME)
	 (format "%s.%s" $1 $3)))
       (global_stmt
	((GLOBAL comma_sep_name_list)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (comma_sep_name_list
	((NAME))
	((comma_sep_name_list COMMA NAME)))
       (exec_stmt
	((EXEC expr exec_trailer)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (exec_trailer
	(nil)
	((IN test comma_test_opt)
	 nil))
       (comma_test_opt
	(nil)
	((COMMA test)
	 nil))
       (assert_stmt
	((ASSERT test comma_test_opt)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (compound_stmt
	((if_stmt))
	((while_stmt))
	((for_stmt))
	((try_stmt))
	((with_stmt))
	((funcdef))
	((class_declaration)))
       (if_stmt
	((IF test COLON suite elif_suite_pair_list else_suite_pair_opt)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (elif_suite_pair_list
	(nil)
	((elif_suite_pair_list ELIF test COLON suite)
	 nil))
       (else_suite_pair_opt
	(nil)
	((ELSE COLON suite)
	 nil))
       (suite
	((simple_stmt)
	 (list $1))
	((NEWLINE indented_block)
	 (progn $2)))
       (indented_block
	((INDENT_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'indented_block_body 1)))
       (indented_block_body
	((INDENT)
	 nil)
	((DEDENT)
	 nil)
	((simple_stmt))
	((compound_stmt)))
       (while_stmt
	((WHILE test COLON suite else_suite_pair_opt)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (for_stmt
	((FOR exprlist IN testlist COLON suite else_suite_pair_opt)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (try_stmt
	((TRY COLON suite except_clause_suite_pair_list else_suite_pair_opt)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil)))
	((TRY COLON suite FINALLY COLON suite)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (except_clause_suite_pair_list
	((except_clause COLON suite)
	 nil)
	((except_clause_suite_pair_list except_clause COLON suite)
	 nil))
       (except_clause
	((EXCEPT zero_one_or_two_test)
	 nil))
       (zero_one_or_two_test
	(nil)
	((test zero_or_one_comma_test)
	 nil))
       (with_stmt
	((WITH test COLON suite)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil)))
	((WITH test with_var COLON suite)
	 (wisent-raw-tag
	  (semantic-tag-new-code $1 nil))))
       (with_var
	((AS expr)
	 nil))
       (decorator
	((AT dotted_name varargslist_opt NEWLINE)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 "decorator" $3))))
       (decorators
	((decorator)
	 (list $1))
	((decorator decorators)
	 (cons $1 $2)))
       (funcdef
	((DEF NAME function_parameter_list COLON suite)
	 (wisent-python-reconstitute-function-tag
	  (wisent-raw-tag
	   (semantic-tag-new-function $2 nil $3))
	  $5))
	((decorators DEF NAME function_parameter_list COLON suite)
	 (wisent-python-reconstitute-function-tag
	  (wisent-raw-tag
	   (semantic-tag-new-function $3 nil $4 :decorators $1))
	  $6)))
       (function_parameter_list
	((PAREN_BLOCK)
	 (let
	     ((wisent-python-EXPANDING-block t))
	   (semantic-parse-region
	    (car $region1)
	    (cdr $region1)
	    'function_parameters 1))))
       (function_parameters
	((LPAREN)
	 nil)
	((RPAREN)
	 nil)
	((function_parameter COMMA))
	((function_parameter RPAREN)))
       (function_parameter
	((fpdef_opt_test))
	((MULT NAME)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $2 nil nil)))
	((EXPONENT NAME)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $2 nil nil))))
       (class_declaration
	((CLASS NAME paren_class_list_opt COLON suite)
	 (wisent-python-reconstitute-class-tag
	  (wisent-raw-tag
	   (semantic-tag-new-type $2 $1 $5
				  (cons $3 nil))))))
       (paren_class_list_opt
	(nil)
	((paren_class_list)))
       (paren_class_list
	((PAREN_BLOCK)
	 (let
	     ((wisent-python-EXPANDING-block t))
	   (mapcar 'semantic-tag-name
		   (semantic-parse-region
		    (car $region1)
		    (cdr $region1)
		    'paren_classes 1)))))
       (paren_classes
	((LPAREN)
	 nil)
	((RPAREN)
	 nil)
	((paren_class COMMA)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $1 nil nil)))
	((paren_class RPAREN)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $1 nil nil))))
       (paren_class
	((dotted_name)))
       (test
	((test_test))
	((lambdef)))
       (test_test
	((and_test))
	((test_test OR and_test)
	 nil))
       (and_test
	((not_test))
	((and_test AND not_test)
	 nil))
       (not_test
	((NOT not_test)
	 nil)
	((comparison)))
       (comparison
	((expr))
	((comparison comp_op expr)
	 nil))
       (comp_op
	((LT))
	((GT))
	((EQ))
	((GE))
	((LE))
	((LTGT))
	((NE))
	((IN))
	((NOT IN))
	((IS))
	((IS NOT)))
       (expr
	((xor_expr))
	((expr BAR xor_expr)
	 nil))
       (xor_expr
	((and_expr))
	((xor_expr HAT and_expr)
	 nil))
       (and_expr
	((shift_expr))
	((and_expr AMP shift_expr)
	 nil))
       (shift_expr
	((arith_expr))
	((shift_expr shift_expr_operators arith_expr)
	 nil))
       (shift_expr_operators
	((LTLT))
	((GTGT)))
       (arith_expr
	((term))
	((arith_expr plus_or_minus term)
	 nil))
       (plus_or_minus
	((PLUS))
	((MINUS)))
       (term
	((factor))
	((term term_operator factor)
	 nil))
       (term_operator
	((MULT))
	((DIV))
	((MOD))
	((DIVDIV)))
       (factor
	((prefix_operators factor)
	 nil)
	((power)))
       (prefix_operators
	((PLUS))
	((MINUS))
	((TILDE)))
       (power
	((atom trailer_zom exponent_zom)
	 (concat $1
		 (if $2
		     (concat " " $2 " ")
		   "")
		 (if $3
		     (concat " " $3)
		   ""))))
       (trailer_zom
	(nil)
	((trailer_zom trailer)
	 nil))
       (exponent_zom
	(nil)
	((exponent_zom EXPONENT factor)
	 nil))
       (trailer
	((PAREN_BLOCK)
	 nil)
	((BRACK_BLOCK)
	 nil)
	((PERIOD NAME)
	 nil))
       (atom
	((PAREN_BLOCK)
	 nil)
	((BRACK_BLOCK)
	 nil)
	((BRACE_BLOCK)
	 nil)
	((BACKQUOTE testlist BACKQUOTE)
	 nil)
	((NAME))
	((NUMBER_LITERAL))
	((one_or_more_string)))
       (test_list_opt
	(nil)
	((testlist)
	 nil))
       (testlist
	((comma_sep_test_list comma_opt)))
       (comma_sep_test_list
	((test))
	((comma_sep_test_list COMMA test)
	 (format "%s, %s" $1 $3)))
       (one_or_more_string
	((STRING_LITERAL))
	((one_or_more_string STRING_LITERAL)
	 (concat $1 $2)))
       (lambdef
	((LAMBDA varargslist_opt COLON test)
	 (format "%s %s" $1
		 (or $2 ""))))
       (varargslist_opt
	(nil)
	((varargslist)))
       (varargslist
	((fpdef_opt_test_list_comma_zom rest_args)
	 (nconc $2 $1))
	((fpdef_opt_test_list comma_opt)))
       (rest_args
	((MULT NAME multmult_name_opt)
	 nil)
	((EXPONENT NAME)
	 nil))
       (multmult_name_opt
	(nil)
	((COMMA EXPONENT NAME)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $3 nil nil))))
       (fpdef_opt_test_list_comma_zom
	(nil)
	((fpdef_opt_test_list_comma_zom fpdef_opt_test COMMA)
	 (nconc $2 $1)))
       (fpdef_opt_test_list
	((fpdef_opt_test))
	((fpdef_opt_test_list COMMA fpdef_opt_test)
	 (nconc $3 $1)))
       (fpdef_opt_test
	((fpdef eq_test_opt)))
       (fpdef
	((NAME)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $1 nil nil))))
       (fplist
	((fpdef_list comma_opt)))
       (fpdef_list
	((fpdef))
	((fpdef_list COMMA fpdef)))
       (eq_test_opt
	(nil)
	((ASSIGN test)
	 nil))
       (comma_opt
	(nil)
	((COMMA)))
       (semicolon_opt
	(nil)
	((SEMICOLON))))
     '(goal function_parameter paren_class indented_block function_parameters paren_classes indented_block_body)))
  "Parser table.")

(defun wisent-python-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-python-wy--parse-table
	semantic-debug-parser-source "wisent-python.wy"
	semantic-flex-keywords-obarray wisent-python-wy--keyword-table
	semantic-lex-types-obarray wisent-python-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)

(define-lex-keyword-type-analyzer wisent-python-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")

(define-lex-block-type-analyzer wisent-python-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACK))
  )

(define-lex-regex-type-analyzer wisent-python-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  nil
  'NAME)

(define-lex-regex-type-analyzer wisent-python-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-string-type-analyzer wisent-python-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((AT . "@")
    (BACKQUOTE . "`")
    (ASSIGN . "=")
    (COMMA . ",")
    (SEMICOLON . ";")
    (COLON . ":")
    (BAR . "|")
    (TILDE . "~")
    (PERIOD . ".")
    (MINUS . "-")
    (PLUS . "+")
    (MOD . "%")
    (DIV . "/")
    (MULT . "*")
    (AMP . "&")
    (GT . ">")
    (LT . "<")
    (HAT . "^")
    (NE . "!=")
    (LTGT . "<>")
    (HATEQ . "^=")
    (OREQ . "|=")
    (AMPEQ . "&=")
    (MODEQ . "%=")
    (DIVEQ . "/=")
    (MULTEQ . "*=")
    (MINUSEQ . "-=")
    (PLUSEQ . "+=")
    (LE . "<=")
    (GE . ">=")
    (EQ . "==")
    (EXPONENT . "**")
    (GTGT . ">>")
    (LTLT . "<<")
    (DIVDIV . "//")
    (DIVDIVEQ . "//=")
    (EXPEQ . "**=")
    (GTGTEQ . ">>=")
    (LTLTEQ . "<<="))
  'punctuation)


;;; Epilogue
;;

(provide 'wisent-python-wy)

;;; wisent-python-wy.el ends here
