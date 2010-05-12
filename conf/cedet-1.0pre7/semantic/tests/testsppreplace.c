/* Test CPP/SPP Replacement
 */

/* TEST: The EMU keyword doesn't screw up the function defn. */
#define EMU
#define EMU2 /*comment*/
char EMU parse_around_emu EMU2 (EMU)
{
}

/* TEST: A simple word can be replaced in a definition. */
#define SUBFLOAT /* Some Float */ float
SUBFLOAT returnanfloat()
{
}

/* TEST: Punctuation an be replaced in a definition. */
#define COLON :
int foo COLON COLON bar ()
{
}

/* TEST: Multiple lexical characters in a definition */
#define SUPER mysuper::
int SUPER baz ()
{
}

/* TEST: Macro replacement. */
#define INT_FCN(name) int name (int in)

INT_FCN(increment) {
  return in+1;
}

/* TEST: Macro replacement with complex args */
#define P_(proto) ()

int myFcn1 P_((a,b));

#define P__(proto) proto

int myFcn2 P__((int a, int b));
int myFcn3 (int a, int b);

/* TEST: Macro replacement for very long argument lists. (See xdisp.c in Emacs) */
int myFcn4 P__((int a,
		int b,
		int c,
		int d,
		int e,
		int f));

/* TEST: Multiple args to a macro. */
#define MULTI_ARGS(name, field1, field2, field3) struct name { int field1; int field2; int field3; }

MULTI_ARGS(ma_struct, moose, penguin, emu);

/* TEST: Macro w/ args, but no body. */
#define NO_BODY(name)

NO_BODY(Moose);

/* TEST: Not a macro with args, but close. */
#define NOT_WITH_ARGS     (moose)

int not_with_args_fcn NOT_WITH_ARGS
{
}

/* TEST: macro w/ continuation. */
#define WITH_CONT \
  continuation_symbol

int WITH_CONT () { };

/* TEST: macros in a macro - tail processing */
#define tail_with_args_and_long_name(a) (int a)
#define int_arg tail_with_args_and_long_name

int tail int_arg(q) {}

/* TEST: macros used improperly. */
#define tail_fail tail_with_args_and_long_name(q)

int tail_fcn tail_fail(q);

/* TEST: feature of CPP from LSD <lsdsgster@...> */
#define __gthrw_(name) __gthrw_ ## name

int __gthrw_(foo) (int arg1) { }

/* TEST: macros using macros */
#define macro_foo foo
#define mf_declare int macro_foo

mf_declare;

/* TEST: macros with args using macros */
#define Amacro(A) (int A)
#define mf_Amacro(B) int B Amacro(B)

mf_Amacro(noodle);

/* TEST: Double macro using the argument stack. */
#define MACRO0(name) int that_ ## name(int i);
#define MACRO1(name) int this_ ## name(int i);
#define MACRO2(name) MACRO0(name) MACRO1(name)

MACRO2(foo)

/* TEST: The G++ namespace macro hack.  Not really part of SPP. */
_GLIBCXX_BEGIN_NAMESPACE(baz)

  int bazfnc(int b) { }

_GLIBCXX_END_NAMESPACE;

_GLIBCXX_BEGIN_NESTED_NAMESPACE(foo,bar)

  int foo_bar_func(int a) { }

_GLIBCXX_END_NESTED_NAMESPACE;


/* TEST: The VC++ macro hack. */
_STD_BEGIN

  int inside_std_namespace(int a) { }

_STD_END

/* TEST: Recursion prevention.  CPP doesn't allow even 1 level of recursion. */
#define STARTMACRO MACROA
#define MACROA MACROB
#define MACROB MACROA

int STARTMACRO () {

}

/* TEST: Fancy concat/recursive macros */
#define CONCAT(x,y)     x##y
#define PASTE(x,y)      CONCAT(x,y)
#define OBJ(fn)         PASTE(PREFIX, fn)
#define PREFIX          PASTE(A,B)

int 
OBJ(test)     /* expands to ABtest */
  ;

/* TEST: Macro Recursion limits in arguments to a macro. 
 * This code is from ALSA (with names changed to moose), noticed by Yupeng. */
#define mr_moose(n) list_entry(n, struct mr_moose, list)

struct mr_moose_ops {
  int (*mr_moose_disconnect)(struct mr_moose *dev);
};


/* END */
