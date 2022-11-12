/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         lattepp_parse
#define yylex           lattepp_lex
#define yyerror         lattepp_error
#define yydebug         lattepp_debug
#define yynerrs         lattepp_nerrs

/* First part of user prologue.  */
#line 20 "Lattepp.y"

/* Begin C preamble code */

#include <algorithm> /* for std::reverse */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Absyn.H"

#define YYMAXDEPTH 10000000

/* The type yyscan_t is defined by flex, but we need it in the parser already. */
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE lattepp__scan_string(const char *str, yyscan_t scanner);
extern void lattepp__delete_buffer(YY_BUFFER_STATE buf, yyscan_t scanner);

extern void lattepp_lex_destroy(yyscan_t scanner);
extern char* lattepp_get_text(yyscan_t scanner);

extern yyscan_t lattepp__initialize_lexer(FILE * inp);

/* End C preamble code */

#line 105 "Parser.C"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "Bison.H"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL__ERROR_ = 3,                    /* _ERROR_  */
  YYSYMBOL__BANG = 4,                      /* _BANG  */
  YYSYMBOL__BANGEQ = 5,                    /* _BANGEQ  */
  YYSYMBOL__PERCENT = 6,                   /* _PERCENT  */
  YYSYMBOL__DAMP = 7,                      /* _DAMP  */
  YYSYMBOL__LPAREN = 8,                    /* _LPAREN  */
  YYSYMBOL__RPAREN = 9,                    /* _RPAREN  */
  YYSYMBOL__STAR = 10,                     /* _STAR  */
  YYSYMBOL__PLUS = 11,                     /* _PLUS  */
  YYSYMBOL__DPLUS = 12,                    /* _DPLUS  */
  YYSYMBOL__COMMA = 13,                    /* _COMMA  */
  YYSYMBOL__MINUS = 14,                    /* _MINUS  */
  YYSYMBOL__DMINUS = 15,                   /* _DMINUS  */
  YYSYMBOL__DOT = 16,                      /* _DOT  */
  YYSYMBOL__SLASH = 17,                    /* _SLASH  */
  YYSYMBOL__COLON = 18,                    /* _COLON  */
  YYSYMBOL__SEMI = 19,                     /* _SEMI  */
  YYSYMBOL__LT = 20,                       /* _LT  */
  YYSYMBOL__LDARROW = 21,                  /* _LDARROW  */
  YYSYMBOL__EQ = 22,                       /* _EQ  */
  YYSYMBOL__DEQ = 23,                      /* _DEQ  */
  YYSYMBOL__GT = 24,                       /* _GT  */
  YYSYMBOL__GTEQ = 25,                     /* _GTEQ  */
  YYSYMBOL__LBRACK = 26,                   /* _LBRACK  */
  YYSYMBOL__EMPTYBRACK = 27,               /* _EMPTYBRACK  */
  YYSYMBOL__RBRACK = 28,                   /* _RBRACK  */
  YYSYMBOL__KW_boolean = 29,               /* _KW_boolean  */
  YYSYMBOL__KW_class = 30,                 /* _KW_class  */
  YYSYMBOL__KW_else = 31,                  /* _KW_else  */
  YYSYMBOL__KW_extends = 32,               /* _KW_extends  */
  YYSYMBOL__KW_false = 33,                 /* _KW_false  */
  YYSYMBOL__KW_for = 34,                   /* _KW_for  */
  YYSYMBOL__KW_if = 35,                    /* _KW_if  */
  YYSYMBOL__KW_int = 36,                   /* _KW_int  */
  YYSYMBOL__KW_new = 37,                   /* _KW_new  */
  YYSYMBOL__KW_null = 38,                  /* _KW_null  */
  YYSYMBOL__KW_return = 39,                /* _KW_return  */
  YYSYMBOL__KW_string = 40,                /* _KW_string  */
  YYSYMBOL__KW_true = 41,                  /* _KW_true  */
  YYSYMBOL__KW_void = 42,                  /* _KW_void  */
  YYSYMBOL__KW_while = 43,                 /* _KW_while  */
  YYSYMBOL__LBRACE = 44,                   /* _LBRACE  */
  YYSYMBOL__DBAR = 45,                     /* _DBAR  */
  YYSYMBOL__RBRACE = 46,                   /* _RBRACE  */
  YYSYMBOL__STRING_ = 47,                  /* _STRING_  */
  YYSYMBOL__INTEGER_ = 48,                 /* _INTEGER_  */
  YYSYMBOL__IDENT_ = 49,                   /* _IDENT_  */
  YYSYMBOL_YYACCEPT = 50,                  /* $accept  */
  YYSYMBOL_Program = 51,                   /* Program  */
  YYSYMBOL_TopDef = 52,                    /* TopDef  */
  YYSYMBOL_ListTopDef = 53,                /* ListTopDef  */
  YYSYMBOL_Arg = 54,                       /* Arg  */
  YYSYMBOL_ListArg = 55,                   /* ListArg  */
  YYSYMBOL_Block = 56,                     /* Block  */
  YYSYMBOL_ClassBlock = 57,                /* ClassBlock  */
  YYSYMBOL_ListClassStmt = 58,             /* ListClassStmt  */
  YYSYMBOL_ListStmt = 59,                  /* ListStmt  */
  YYSYMBOL_ClassStmt = 60,                 /* ClassStmt  */
  YYSYMBOL_Stmt = 61,                      /* Stmt  */
  YYSYMBOL_Item = 62,                      /* Item  */
  YYSYMBOL_ListItem = 63,                  /* ListItem  */
  YYSYMBOL_ExtIdent = 64,                  /* ExtIdent  */
  YYSYMBOL_PrimType = 65,                  /* PrimType  */
  YYSYMBOL_Type = 66,                      /* Type  */
  YYSYMBOL_Expr7 = 67,                     /* Expr7  */
  YYSYMBOL_Expr6 = 68,                     /* Expr6  */
  YYSYMBOL_Expr5 = 69,                     /* Expr5  */
  YYSYMBOL_Expr4 = 70,                     /* Expr4  */
  YYSYMBOL_Expr3 = 71,                     /* Expr3  */
  YYSYMBOL_Expr2 = 72,                     /* Expr2  */
  YYSYMBOL_Expr1 = 73,                     /* Expr1  */
  YYSYMBOL_Expr = 74,                      /* Expr  */
  YYSYMBOL_ListExpr = 75,                  /* ListExpr  */
  YYSYMBOL_AddOp = 76,                     /* AddOp  */
  YYSYMBOL_MulOp = 77,                     /* MulOp  */
  YYSYMBOL_RelOp = 78                      /* RelOp  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;


/* Second part of user prologue.  */
#line 79 "Lattepp.y"

void yyerror(YYLTYPE *loc, yyscan_t scanner, YYSTYPE *result, const char *msg)
{
  fprintf(stderr, "error: %d,%d: %s at %s\n",
    loc->first_line, loc->first_column, msg, lattepp_get_text(scanner));
}

int yyparse(yyscan_t scanner, YYSTYPE *result);

extern int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, yyscan_t scanner);

#line 229 "Parser.C"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  13
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   267

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  50
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  29
/* YYNRULES -- Number of rules.  */
#define YYNRULES  89
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  165

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   304


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   173,   173,   175,   176,   177,   179,   180,   182,   184,
     185,   186,   188,   190,   192,   193,   195,   196,   198,   199,
     200,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   213,   214,   216,   217,   219,   220,   222,   223,
     224,   226,   227,   228,   229,   231,   232,   233,   239,   240,
     241,   243,   244,   245,   246,   247,   248,   249,   250,   251,
     252,   253,   254,   256,   257,   258,   260,   261,   263,   264,
     266,   267,   269,   270,   272,   273,   275,   276,   277,   279,
     280,   282,   283,   284,   286,   287,   288,   289,   290,   291
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "_ERROR_", "_BANG",
  "_BANGEQ", "_PERCENT", "_DAMP", "_LPAREN", "_RPAREN", "_STAR", "_PLUS",
  "_DPLUS", "_COMMA", "_MINUS", "_DMINUS", "_DOT", "_SLASH", "_COLON",
  "_SEMI", "_LT", "_LDARROW", "_EQ", "_DEQ", "_GT", "_GTEQ", "_LBRACK",
  "_EMPTYBRACK", "_RBRACK", "_KW_boolean", "_KW_class", "_KW_else",
  "_KW_extends", "_KW_false", "_KW_for", "_KW_if", "_KW_int", "_KW_new",
  "_KW_null", "_KW_return", "_KW_string", "_KW_true", "_KW_void",
  "_KW_while", "_LBRACE", "_DBAR", "_RBRACE", "_STRING_", "_INTEGER_",
  "_IDENT_", "$accept", "Program", "TopDef", "ListTopDef", "Arg",
  "ListArg", "Block", "ClassBlock", "ListClassStmt", "ListStmt",
  "ClassStmt", "Stmt", "Item", "ListItem", "ExtIdent", "PrimType", "Type",
  "Expr7", "Expr6", "Expr5", "Expr4", "Expr3", "Expr2", "Expr1", "Expr",
  "ListExpr", "AddOp", "MulOp", "RelOp", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-45)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-57)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      72,   -45,   -41,   -45,   -45,   -45,   -45,    12,    72,   -45,
     -45,   -22,    15,   -45,   -45,   -45,    35,    17,   -45,   -45,
     167,    49,    41,    83,    64,   -14,   -45,   -45,   -45,   -45,
     -13,   167,    55,   -45,    11,    87,    84,   -45,   -45,   -45,
     167,   184,    61,   -45,    90,   104,   197,   153,   197,   -45,
     200,   -45,   -45,   -45,   -45,    23,   -45,    99,   -45,    78,
      60,   203,    71,   -45,   100,   -45,   -45,   127,   133,    20,
     134,   -45,     3,   -45,   -45,    -5,    -7,   129,   124,    55,
      99,    29,   138,   139,    99,    52,    65,   184,   184,   197,
     -45,   -45,   -45,   184,   -45,   -45,   184,   -45,   184,   -45,
     -45,   -45,   -45,   -45,   184,   184,   167,   184,   -45,   132,
     184,   184,   135,   141,   184,   143,   197,   -45,   -45,   197,
     197,   -45,   184,   145,   147,   140,    99,   -45,    78,   -45,
      60,   -45,    -1,   154,   -45,   155,   159,   -45,   -45,   162,
     -45,   137,    99,    99,   169,   184,   -45,   -45,   148,   136,
     136,   241,   -45,   -45,   -45,   210,   168,   -45,    38,   195,
     129,   136,   136,   -45,   -45
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    43,     0,    41,    42,    44,    46,     0,     6,     2,
      45,     0,     0,     1,     7,    47,     0,     0,    14,     4,
       9,     0,     0,    10,     0,     0,     5,    18,    13,    15,
       0,     9,     0,     8,    34,    36,     0,    11,    16,     3,
       9,     0,     0,    19,     0,     0,     0,     0,     0,    59,
       0,    53,    58,    61,    57,    56,    62,    65,    67,    69,
      71,    73,    75,    35,    34,    37,    21,     0,     0,     0,
       0,    12,    56,    22,    17,     0,     0,    65,     0,     0,
      64,    56,     0,     0,    63,    51,     0,    76,     0,     0,
      83,    81,    82,     0,    79,    80,     0,    89,     0,    84,
      85,    88,    86,    87,     0,     0,     0,     0,    28,     0,
       0,     0,     0,     0,     0,     0,     0,    33,    20,     0,
       0,    50,     0,    77,     0,     0,    54,    66,    68,    72,
      70,    74,     0,     0,    27,     0,     0,    25,    26,     0,
      23,    54,    48,    49,     0,    76,    60,    55,     0,     0,
       0,    55,    24,    52,    78,     0,    29,    31,    38,     0,
       0,     0,     0,    30,    32
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -45,   -45,   -45,   198,   -45,    25,   -29,   190,   -45,   -45,
     -45,   105,   -45,   -36,    57,   166,     1,   -45,   -44,   121,
     119,   115,   -45,   122,   -25,    92,   -45,   -45,   -45
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     7,     8,     9,    23,    24,    73,    19,    22,    44,
      29,    74,    35,    36,    75,    10,    76,    56,    57,    58,
      59,    60,    61,    62,    78,   124,    96,    93,   104
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      77,    11,    80,    39,    84,    15,    65,   112,    12,    11,
     113,    87,    13,    15,    15,   -38,    63,   114,   -38,    40,
      15,    25,    83,    30,    46,   -38,    15,    16,    47,   111,
     -46,    87,    25,    41,    48,    33,    34,    87,   119,   108,
     115,    25,    64,    20,   109,   126,    87,    17,   148,    88,
     118,    86,   -46,    49,   -56,    88,    37,    50,    51,    18,
      27,    52,   123,   125,   111,    45,    21,    53,    54,    55,
       1,    94,   141,    32,    95,   142,   143,     3,   -46,   -46,
     131,     4,   133,     5,    90,   135,   136,    28,    91,   139,
       6,   122,    15,    18,    46,    92,    31,   144,    47,    38,
      42,     1,     2,    43,    48,    77,    77,   132,     3,    66,
      64,   160,     4,    79,     5,    89,   105,    77,    77,     1,
     123,     6,    41,    49,    67,    68,     3,    50,    51,    69,
       4,    52,     5,    70,    38,   106,    71,    53,    54,    72,
      46,   107,   110,   117,    47,   116,   -40,   120,   121,   -40,
      48,   134,   -40,    89,   137,    66,   146,    46,   145,   -40,
     138,    47,   140,   149,   150,     1,   155,    48,   147,    49,
      67,    68,     3,    50,    51,    69,     4,    52,     5,    70,
      38,   152,     1,    53,    54,    72,    49,   151,    46,     3,
      50,    51,    47,     4,    52,     5,     1,   153,    48,   161,
      53,    54,    81,     3,   162,    47,    14,     4,    97,     5,
      98,    26,   159,    82,   127,   128,     6,    49,    47,   130,
     129,    50,    51,    99,   100,    52,   101,   102,   103,     1,
      49,    53,    54,    55,    50,    51,     3,   154,    52,     0,
       4,     0,     5,    49,    53,    54,    55,    50,    51,    85,
     -39,    52,     0,   -39,   156,   157,   -39,    53,    54,   158,
       0,     0,     0,   -39,     0,     0,   163,   164
};

static const yytype_int16 yycheck[] =
{
      44,     0,    46,    32,    48,    27,    42,    12,    49,     8,
      15,     8,     0,    27,    27,    12,    41,    22,    15,     8,
      27,    20,    47,    22,     4,    22,    27,    49,     8,    26,
      27,     8,    31,    22,    14,    49,    49,     8,     9,    19,
      76,    40,    49,     8,    69,    89,     8,    32,    49,    26,
      79,    50,    49,    33,    16,    26,    31,    37,    38,    44,
      19,    41,    87,    88,    26,    40,    49,    47,    48,    49,
      29,    11,   116,     9,    14,   119,   120,    36,    26,    27,
     105,    40,   107,    42,     6,   110,   111,    46,    10,   114,
      49,    26,    27,    44,     4,    17,    13,   122,     8,    44,
      13,    29,    30,    19,    14,   149,   150,   106,    36,    19,
      49,   155,    40,     9,    42,    16,    45,   161,   162,    29,
     145,    49,    22,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     8,    46,    47,    48,    49,
       4,     8,     8,    19,     8,    16,     9,     9,     9,    12,
      14,    19,    15,    16,    19,    19,     9,     4,    13,    22,
      19,     8,    19,     9,     9,    29,    18,    14,    28,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    19,    29,    47,    48,    49,    33,    28,     4,    36,
      37,    38,     8,    40,    41,    42,    29,    28,    14,    31,
      47,    48,    49,    36,     9,     8,     8,    40,     5,    42,
       7,    21,   155,    47,    93,    96,    49,    33,     8,   104,
      98,    37,    38,    20,    21,    41,    23,    24,    25,    29,
      33,    47,    48,    49,    37,    38,    36,   145,    41,    -1,
      40,    -1,    42,    33,    47,    48,    49,    37,    38,    49,
       9,    41,    -1,    12,   149,   150,    15,    47,    48,    49,
      -1,    -1,    -1,    22,    -1,    -1,   161,   162
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    29,    30,    36,    40,    42,    49,    51,    52,    53,
      65,    66,    49,     0,    53,    27,    49,    32,    44,    57,
       8,    49,    58,    54,    55,    66,    57,    19,    46,    60,
      66,    13,     9,    49,    49,    62,    63,    55,    44,    56,
       8,    22,    13,    19,    59,    55,     4,     8,    14,    33,
      37,    38,    41,    47,    48,    49,    67,    68,    69,    70,
      71,    72,    73,    74,    49,    63,    19,    34,    35,    39,
      43,    46,    49,    56,    61,    64,    66,    68,    74,     9,
      68,    49,    65,    74,    68,    49,    66,     8,    26,    16,
       6,    10,    17,    77,    11,    14,    76,     5,     7,    20,
      21,    23,    24,    25,    78,    45,     8,     8,    19,    74,
       8,    26,    12,    15,    22,    63,    16,    19,    56,     9,
       9,     9,    26,    74,    75,    74,    68,    69,    70,    73,
      71,    74,    66,    74,    19,    74,    74,    19,    19,    74,
      19,    68,    68,    68,    74,    13,     9,    28,    49,     9,
       9,    28,    19,    28,    75,    18,    61,    61,    49,    64,
      68,    31,     9,    61,    61
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    50,    51,    52,    52,    52,    53,    53,    54,    55,
      55,    55,    56,    57,    58,    58,    59,    59,    60,    60,
      60,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    62,    62,    63,    63,    64,    64,
      64,    65,    65,    65,    65,    66,    66,    66,    67,    67,
      67,    68,    68,    68,    68,    68,    68,    68,    68,    68,
      68,    68,    68,    69,    69,    69,    70,    70,    71,    71,
      72,    72,    73,    73,    74,    74,    75,    75,    75,    76,
      76,    77,    77,    77,    78,    78,    78,    78,    78,    78
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     6,     3,     5,     1,     2,     2,     0,
       1,     3,     3,     3,     0,     2,     0,     2,     1,     3,
       6,     1,     1,     3,     4,     3,     3,     3,     2,     5,
       7,     5,     8,     2,     1,     3,     1,     3,     1,     4,
       3,     1,     1,     1,     1,     1,     1,     2,     4,     4,
       3,     2,     5,     1,     3,     4,     1,     1,     1,     1,
       4,     1,     1,     2,     2,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     0,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, scanner, result, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, scanner, result); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (result);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, scanner, result);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, yyscan_t scanner, YYSTYPE *result)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), scanner, result);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, scanner, result); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (result);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (yyscan_t scanner, YYSTYPE *result)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, scanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Program: ListTopDef  */
#line 173 "Lattepp.y"
                     { std::reverse((yyvsp[0].listtopdef_)->begin(),(yyvsp[0].listtopdef_)->end()) ;(yyval.program_) = new Prog((yyvsp[0].listtopdef_)); (yyval.program_)->line_number = (yyloc).first_line; (yyval.program_)->char_number = (yyloc).first_column; result->program_ = (yyval.program_); }
#line 1442 "Parser.C"
    break;

  case 3: /* TopDef: Type _IDENT_ _LPAREN ListArg _RPAREN Block  */
#line 175 "Lattepp.y"
                                                    { std::reverse((yyvsp[-2].listarg_)->begin(),(yyvsp[-2].listarg_)->end()) ;(yyval.topdef_) = new FnDef((yyvsp[-5].type_), (yyvsp[-4]._string), (yyvsp[-2].listarg_), (yyvsp[0].block_)); (yyval.topdef_)->line_number = (yyloc).first_line; (yyval.topdef_)->char_number = (yyloc).first_column; }
#line 1448 "Parser.C"
    break;

  case 4: /* TopDef: _KW_class _IDENT_ ClassBlock  */
#line 176 "Lattepp.y"
                                 { (yyval.topdef_) = new ClassDef((yyvsp[-1]._string), (yyvsp[0].classblock_)); (yyval.topdef_)->line_number = (yyloc).first_line; (yyval.topdef_)->char_number = (yyloc).first_column; }
#line 1454 "Parser.C"
    break;

  case 5: /* TopDef: _KW_class _IDENT_ _KW_extends _IDENT_ ClassBlock  */
#line 177 "Lattepp.y"
                                                     { (yyval.topdef_) = new ExtClassDef((yyvsp[-3]._string), (yyvsp[-1]._string), (yyvsp[0].classblock_)); (yyval.topdef_)->line_number = (yyloc).first_line; (yyval.topdef_)->char_number = (yyloc).first_column; }
#line 1460 "Parser.C"
    break;

  case 6: /* ListTopDef: TopDef  */
#line 179 "Lattepp.y"
                    { (yyval.listtopdef_) = new ListTopDef(); (yyval.listtopdef_)->push_back((yyvsp[0].topdef_)); }
#line 1466 "Parser.C"
    break;

  case 7: /* ListTopDef: TopDef ListTopDef  */
#line 180 "Lattepp.y"
                      { (yyvsp[0].listtopdef_)->push_back((yyvsp[-1].topdef_)); (yyval.listtopdef_) = (yyvsp[0].listtopdef_); }
#line 1472 "Parser.C"
    break;

  case 8: /* Arg: Type _IDENT_  */
#line 182 "Lattepp.y"
                   { (yyval.arg_) = new Ar((yyvsp[-1].type_), (yyvsp[0]._string)); (yyval.arg_)->line_number = (yyloc).first_line; (yyval.arg_)->char_number = (yyloc).first_column; }
#line 1478 "Parser.C"
    break;

  case 9: /* ListArg: %empty  */
#line 184 "Lattepp.y"
                      { (yyval.listarg_) = new ListArg(); }
#line 1484 "Parser.C"
    break;

  case 10: /* ListArg: Arg  */
#line 185 "Lattepp.y"
        { (yyval.listarg_) = new ListArg(); (yyval.listarg_)->push_back((yyvsp[0].arg_)); }
#line 1490 "Parser.C"
    break;

  case 11: /* ListArg: Arg _COMMA ListArg  */
#line 186 "Lattepp.y"
                       { (yyvsp[0].listarg_)->push_back((yyvsp[-2].arg_)); (yyval.listarg_) = (yyvsp[0].listarg_); }
#line 1496 "Parser.C"
    break;

  case 12: /* Block: _LBRACE ListStmt _RBRACE  */
#line 188 "Lattepp.y"
                                 { (yyval.block_) = new Bloc((yyvsp[-1].liststmt_)); (yyval.block_)->line_number = (yyloc).first_line; (yyval.block_)->char_number = (yyloc).first_column; }
#line 1502 "Parser.C"
    break;

  case 13: /* ClassBlock: _LBRACE ListClassStmt _RBRACE  */
#line 190 "Lattepp.y"
                                           { (yyval.classblock_) = new ClassBloc((yyvsp[-1].listclassstmt_)); (yyval.classblock_)->line_number = (yyloc).first_line; (yyval.classblock_)->char_number = (yyloc).first_column; }
#line 1508 "Parser.C"
    break;

  case 14: /* ListClassStmt: %empty  */
#line 192 "Lattepp.y"
                            { (yyval.listclassstmt_) = new ListClassStmt(); }
#line 1514 "Parser.C"
    break;

  case 15: /* ListClassStmt: ListClassStmt ClassStmt  */
#line 193 "Lattepp.y"
                            { (yyvsp[-1].listclassstmt_)->push_back((yyvsp[0].classstmt_)); (yyval.listclassstmt_) = (yyvsp[-1].listclassstmt_); }
#line 1520 "Parser.C"
    break;

  case 16: /* ListStmt: %empty  */
#line 195 "Lattepp.y"
                       { (yyval.liststmt_) = new ListStmt(); }
#line 1526 "Parser.C"
    break;

  case 17: /* ListStmt: ListStmt Stmt  */
#line 196 "Lattepp.y"
                  { (yyvsp[-1].liststmt_)->push_back((yyvsp[0].stmt_)); (yyval.liststmt_) = (yyvsp[-1].liststmt_); }
#line 1532 "Parser.C"
    break;

  case 18: /* ClassStmt: _SEMI  */
#line 198 "Lattepp.y"
                  { (yyval.classstmt_) = new ClassEmpty(); (yyval.classstmt_)->line_number = (yyloc).first_line; (yyval.classstmt_)->char_number = (yyloc).first_column; }
#line 1538 "Parser.C"
    break;

  case 19: /* ClassStmt: Type ListItem _SEMI  */
#line 199 "Lattepp.y"
                        { std::reverse((yyvsp[-1].listitem_)->begin(),(yyvsp[-1].listitem_)->end()) ;(yyval.classstmt_) = new ClassDecl((yyvsp[-2].type_), (yyvsp[-1].listitem_)); (yyval.classstmt_)->line_number = (yyloc).first_line; (yyval.classstmt_)->char_number = (yyloc).first_column; }
#line 1544 "Parser.C"
    break;

  case 20: /* ClassStmt: Type _IDENT_ _LPAREN ListArg _RPAREN Block  */
#line 200 "Lattepp.y"
                                               { std::reverse((yyvsp[-2].listarg_)->begin(),(yyvsp[-2].listarg_)->end()) ;(yyval.classstmt_) = new ClassMethod((yyvsp[-5].type_), (yyvsp[-4]._string), (yyvsp[-2].listarg_), (yyvsp[0].block_)); (yyval.classstmt_)->line_number = (yyloc).first_line; (yyval.classstmt_)->char_number = (yyloc).first_column; }
#line 1550 "Parser.C"
    break;

  case 21: /* Stmt: _SEMI  */
#line 202 "Lattepp.y"
             { (yyval.stmt_) = new Empty(); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1556 "Parser.C"
    break;

  case 22: /* Stmt: Block  */
#line 203 "Lattepp.y"
          { (yyval.stmt_) = new BStmt((yyvsp[0].block_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1562 "Parser.C"
    break;

  case 23: /* Stmt: Type ListItem _SEMI  */
#line 204 "Lattepp.y"
                        { std::reverse((yyvsp[-1].listitem_)->begin(),(yyvsp[-1].listitem_)->end()) ;(yyval.stmt_) = new Decl((yyvsp[-2].type_), (yyvsp[-1].listitem_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1568 "Parser.C"
    break;

  case 24: /* Stmt: ExtIdent _EQ Expr _SEMI  */
#line 205 "Lattepp.y"
                            { (yyval.stmt_) = new Ass((yyvsp[-3].extident_), (yyvsp[-1].expr_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1574 "Parser.C"
    break;

  case 25: /* Stmt: ExtIdent _DPLUS _SEMI  */
#line 206 "Lattepp.y"
                          { (yyval.stmt_) = new Incr((yyvsp[-2].extident_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1580 "Parser.C"
    break;

  case 26: /* Stmt: ExtIdent _DMINUS _SEMI  */
#line 207 "Lattepp.y"
                           { (yyval.stmt_) = new Decr((yyvsp[-2].extident_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1586 "Parser.C"
    break;

  case 27: /* Stmt: _KW_return Expr _SEMI  */
#line 208 "Lattepp.y"
                          { (yyval.stmt_) = new Ret((yyvsp[-1].expr_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1592 "Parser.C"
    break;

  case 28: /* Stmt: _KW_return _SEMI  */
#line 209 "Lattepp.y"
                     { (yyval.stmt_) = new VRet(); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1598 "Parser.C"
    break;

  case 29: /* Stmt: _KW_if _LPAREN Expr _RPAREN Stmt  */
#line 210 "Lattepp.y"
                                     { (yyval.stmt_) = new Cond((yyvsp[-2].expr_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1604 "Parser.C"
    break;

  case 30: /* Stmt: _KW_if _LPAREN Expr _RPAREN Stmt _KW_else Stmt  */
#line 211 "Lattepp.y"
                                                   { (yyval.stmt_) = new CondElse((yyvsp[-4].expr_), (yyvsp[-2].stmt_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1610 "Parser.C"
    break;

  case 31: /* Stmt: _KW_while _LPAREN Expr _RPAREN Stmt  */
#line 212 "Lattepp.y"
                                        { (yyval.stmt_) = new While((yyvsp[-2].expr_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1616 "Parser.C"
    break;

  case 32: /* Stmt: _KW_for _LPAREN Type _IDENT_ _COLON ExtIdent _RPAREN Stmt  */
#line 213 "Lattepp.y"
                                                              { (yyval.stmt_) = new For((yyvsp[-5].type_), (yyvsp[-4]._string), (yyvsp[-2].extident_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1622 "Parser.C"
    break;

  case 33: /* Stmt: Expr _SEMI  */
#line 214 "Lattepp.y"
               { (yyval.stmt_) = new SExp((yyvsp[-1].expr_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1628 "Parser.C"
    break;

  case 34: /* Item: _IDENT_  */
#line 216 "Lattepp.y"
               { (yyval.item_) = new NoInit((yyvsp[0]._string)); (yyval.item_)->line_number = (yyloc).first_line; (yyval.item_)->char_number = (yyloc).first_column; }
#line 1634 "Parser.C"
    break;

  case 35: /* Item: _IDENT_ _EQ Expr  */
#line 217 "Lattepp.y"
                     { (yyval.item_) = new Init((yyvsp[-2]._string), (yyvsp[0].expr_)); (yyval.item_)->line_number = (yyloc).first_line; (yyval.item_)->char_number = (yyloc).first_column; }
#line 1640 "Parser.C"
    break;

  case 36: /* ListItem: Item  */
#line 219 "Lattepp.y"
                { (yyval.listitem_) = new ListItem(); (yyval.listitem_)->push_back((yyvsp[0].item_)); }
#line 1646 "Parser.C"
    break;

  case 37: /* ListItem: Item _COMMA ListItem  */
#line 220 "Lattepp.y"
                         { (yyvsp[0].listitem_)->push_back((yyvsp[-2].item_)); (yyval.listitem_) = (yyvsp[0].listitem_); }
#line 1652 "Parser.C"
    break;

  case 38: /* ExtIdent: _IDENT_  */
#line 222 "Lattepp.y"
                   { (yyval.extident_) = new Id((yyvsp[0]._string)); (yyval.extident_)->line_number = (yyloc).first_line; (yyval.extident_)->char_number = (yyloc).first_column; }
#line 1658 "Parser.C"
    break;

  case 39: /* ExtIdent: _IDENT_ _LBRACK Expr _RBRACK  */
#line 223 "Lattepp.y"
                                 { (yyval.extident_) = new ArrId((yyvsp[-3]._string), (yyvsp[-1].expr_)); (yyval.extident_)->line_number = (yyloc).first_line; (yyval.extident_)->char_number = (yyloc).first_column; }
#line 1664 "Parser.C"
    break;

  case 40: /* ExtIdent: Expr6 _DOT Expr6  */
#line 224 "Lattepp.y"
                     { (yyval.extident_) = new AttrId((yyvsp[-2].expr_), (yyvsp[0].expr_)); (yyval.extident_)->line_number = (yyloc).first_line; (yyval.extident_)->char_number = (yyloc).first_column; }
#line 1670 "Parser.C"
    break;

  case 41: /* PrimType: _KW_int  */
#line 226 "Lattepp.y"
                   { (yyval.primtype_) = new Int(); (yyval.primtype_)->line_number = (yyloc).first_line; (yyval.primtype_)->char_number = (yyloc).first_column; }
#line 1676 "Parser.C"
    break;

  case 42: /* PrimType: _KW_string  */
#line 227 "Lattepp.y"
               { (yyval.primtype_) = new Str(); (yyval.primtype_)->line_number = (yyloc).first_line; (yyval.primtype_)->char_number = (yyloc).first_column; }
#line 1682 "Parser.C"
    break;

  case 43: /* PrimType: _KW_boolean  */
#line 228 "Lattepp.y"
                { (yyval.primtype_) = new Bool(); (yyval.primtype_)->line_number = (yyloc).first_line; (yyval.primtype_)->char_number = (yyloc).first_column; }
#line 1688 "Parser.C"
    break;

  case 44: /* PrimType: _KW_void  */
#line 229 "Lattepp.y"
             { (yyval.primtype_) = new Void(); (yyval.primtype_)->line_number = (yyloc).first_line; (yyval.primtype_)->char_number = (yyloc).first_column; }
#line 1694 "Parser.C"
    break;

  case 45: /* Type: PrimType  */
#line 231 "Lattepp.y"
                { (yyval.type_) = new Primitive((yyvsp[0].primtype_)); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1700 "Parser.C"
    break;

  case 46: /* Type: _IDENT_  */
#line 232 "Lattepp.y"
            { (yyval.type_) = new ObjectType((yyvsp[0]._string)); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1706 "Parser.C"
    break;

  case 47: /* Type: Type _EMPTYBRACK  */
#line 233 "Lattepp.y"
                     { (yyval.type_) = new Array((yyvsp[-1].type_)); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1712 "Parser.C"
    break;

  case 48: /* Expr7: _LPAREN _IDENT_ _RPAREN Expr6  */
#line 239 "Lattepp.y"
                                      { (yyval.expr_) = new ECast((yyvsp[-2]._string), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1718 "Parser.C"
    break;

  case 49: /* Expr7: _LPAREN PrimType _RPAREN Expr6  */
#line 240 "Lattepp.y"
                                   { (yyval.expr_) = new ECastPrim((yyvsp[-2].primtype_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1724 "Parser.C"
    break;

  case 50: /* Expr7: _LPAREN Expr _RPAREN  */
#line 241 "Lattepp.y"
                         { (yyval.expr_) = (yyvsp[-1].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1730 "Parser.C"
    break;

  case 51: /* Expr6: _KW_new _IDENT_  */
#line 243 "Lattepp.y"
                        { (yyval.expr_) = new ENewObject((yyvsp[0]._string)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1736 "Parser.C"
    break;

  case 52: /* Expr6: _KW_new Type _LBRACK Expr _RBRACK  */
#line 244 "Lattepp.y"
                                      { (yyval.expr_) = new ENewArr((yyvsp[-3].type_), (yyvsp[-1].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1742 "Parser.C"
    break;

  case 53: /* Expr6: _KW_null  */
#line 245 "Lattepp.y"
             { (yyval.expr_) = new ENull(); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1748 "Parser.C"
    break;

  case 54: /* Expr6: Expr6 _DOT Expr6  */
#line 246 "Lattepp.y"
                     { (yyval.expr_) = new EObject((yyvsp[-2].expr_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1754 "Parser.C"
    break;

  case 55: /* Expr6: _IDENT_ _LBRACK Expr _RBRACK  */
#line 247 "Lattepp.y"
                                 { (yyval.expr_) = new EArr((yyvsp[-3]._string), (yyvsp[-1].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1760 "Parser.C"
    break;

  case 56: /* Expr6: _IDENT_  */
#line 248 "Lattepp.y"
            { (yyval.expr_) = new EVar((yyvsp[0]._string)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1766 "Parser.C"
    break;

  case 57: /* Expr6: _INTEGER_  */
#line 249 "Lattepp.y"
              { (yyval.expr_) = new ELitInt((yyvsp[0]._int)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1772 "Parser.C"
    break;

  case 58: /* Expr6: _KW_true  */
#line 250 "Lattepp.y"
             { (yyval.expr_) = new ELitTrue(); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1778 "Parser.C"
    break;

  case 59: /* Expr6: _KW_false  */
#line 251 "Lattepp.y"
              { (yyval.expr_) = new ELitFalse(); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1784 "Parser.C"
    break;

  case 60: /* Expr6: _IDENT_ _LPAREN ListExpr _RPAREN  */
#line 252 "Lattepp.y"
                                     { std::reverse((yyvsp[-1].listexpr_)->begin(),(yyvsp[-1].listexpr_)->end()) ;(yyval.expr_) = new EApp((yyvsp[-3]._string), (yyvsp[-1].listexpr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1790 "Parser.C"
    break;

  case 61: /* Expr6: _STRING_  */
#line 253 "Lattepp.y"
             { (yyval.expr_) = new EString((yyvsp[0]._string)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1796 "Parser.C"
    break;

  case 62: /* Expr6: Expr7  */
#line 254 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1802 "Parser.C"
    break;

  case 63: /* Expr5: _MINUS Expr6  */
#line 256 "Lattepp.y"
                     { (yyval.expr_) = new Neg((yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1808 "Parser.C"
    break;

  case 64: /* Expr5: _BANG Expr6  */
#line 257 "Lattepp.y"
                { (yyval.expr_) = new Not((yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1814 "Parser.C"
    break;

  case 65: /* Expr5: Expr6  */
#line 258 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1820 "Parser.C"
    break;

  case 66: /* Expr4: Expr4 MulOp Expr5  */
#line 260 "Lattepp.y"
                          { (yyval.expr_) = new EMul((yyvsp[-2].expr_), (yyvsp[-1].mulop_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1826 "Parser.C"
    break;

  case 67: /* Expr4: Expr5  */
#line 261 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1832 "Parser.C"
    break;

  case 68: /* Expr3: Expr3 AddOp Expr4  */
#line 263 "Lattepp.y"
                          { (yyval.expr_) = new EAdd((yyvsp[-2].expr_), (yyvsp[-1].addop_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1838 "Parser.C"
    break;

  case 69: /* Expr3: Expr4  */
#line 264 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1844 "Parser.C"
    break;

  case 70: /* Expr2: Expr2 RelOp Expr3  */
#line 266 "Lattepp.y"
                          { (yyval.expr_) = new ERel((yyvsp[-2].expr_), (yyvsp[-1].relop_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1850 "Parser.C"
    break;

  case 71: /* Expr2: Expr3  */
#line 267 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1856 "Parser.C"
    break;

  case 72: /* Expr1: Expr2 _DAMP Expr1  */
#line 269 "Lattepp.y"
                          { (yyval.expr_) = new EAnd((yyvsp[-2].expr_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1862 "Parser.C"
    break;

  case 73: /* Expr1: Expr2  */
#line 270 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1868 "Parser.C"
    break;

  case 74: /* Expr: Expr1 _DBAR Expr  */
#line 272 "Lattepp.y"
                        { (yyval.expr_) = new EOr((yyvsp[-2].expr_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1874 "Parser.C"
    break;

  case 75: /* Expr: Expr1  */
#line 273 "Lattepp.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1880 "Parser.C"
    break;

  case 76: /* ListExpr: %empty  */
#line 275 "Lattepp.y"
                       { (yyval.listexpr_) = new ListExpr(); }
#line 1886 "Parser.C"
    break;

  case 77: /* ListExpr: Expr  */
#line 276 "Lattepp.y"
         { (yyval.listexpr_) = new ListExpr(); (yyval.listexpr_)->push_back((yyvsp[0].expr_)); }
#line 1892 "Parser.C"
    break;

  case 78: /* ListExpr: Expr _COMMA ListExpr  */
#line 277 "Lattepp.y"
                         { (yyvsp[0].listexpr_)->push_back((yyvsp[-2].expr_)); (yyval.listexpr_) = (yyvsp[0].listexpr_); }
#line 1898 "Parser.C"
    break;

  case 79: /* AddOp: _PLUS  */
#line 279 "Lattepp.y"
              { (yyval.addop_) = new Plus(); (yyval.addop_)->line_number = (yyloc).first_line; (yyval.addop_)->char_number = (yyloc).first_column; }
#line 1904 "Parser.C"
    break;

  case 80: /* AddOp: _MINUS  */
#line 280 "Lattepp.y"
           { (yyval.addop_) = new Minus(); (yyval.addop_)->line_number = (yyloc).first_line; (yyval.addop_)->char_number = (yyloc).first_column; }
#line 1910 "Parser.C"
    break;

  case 81: /* MulOp: _STAR  */
#line 282 "Lattepp.y"
              { (yyval.mulop_) = new Times(); (yyval.mulop_)->line_number = (yyloc).first_line; (yyval.mulop_)->char_number = (yyloc).first_column; }
#line 1916 "Parser.C"
    break;

  case 82: /* MulOp: _SLASH  */
#line 283 "Lattepp.y"
           { (yyval.mulop_) = new Div(); (yyval.mulop_)->line_number = (yyloc).first_line; (yyval.mulop_)->char_number = (yyloc).first_column; }
#line 1922 "Parser.C"
    break;

  case 83: /* MulOp: _PERCENT  */
#line 284 "Lattepp.y"
             { (yyval.mulop_) = new Mod(); (yyval.mulop_)->line_number = (yyloc).first_line; (yyval.mulop_)->char_number = (yyloc).first_column; }
#line 1928 "Parser.C"
    break;

  case 84: /* RelOp: _LT  */
#line 286 "Lattepp.y"
            { (yyval.relop_) = new LTH(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1934 "Parser.C"
    break;

  case 85: /* RelOp: _LDARROW  */
#line 287 "Lattepp.y"
             { (yyval.relop_) = new LE(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1940 "Parser.C"
    break;

  case 86: /* RelOp: _GT  */
#line 288 "Lattepp.y"
        { (yyval.relop_) = new GTH(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1946 "Parser.C"
    break;

  case 87: /* RelOp: _GTEQ  */
#line 289 "Lattepp.y"
          { (yyval.relop_) = new GE(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1952 "Parser.C"
    break;

  case 88: /* RelOp: _DEQ  */
#line 290 "Lattepp.y"
         { (yyval.relop_) = new EQU(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1958 "Parser.C"
    break;

  case 89: /* RelOp: _BANGEQ  */
#line 291 "Lattepp.y"
            { (yyval.relop_) = new NE(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1964 "Parser.C"
    break;


#line 1968 "Parser.C"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (&yylloc, scanner, result, YY_("syntax error"));
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, scanner, result);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, scanner, result);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, scanner, result, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, scanner, result);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, scanner, result);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 294 "Lattepp.y"



/* Entrypoint: parse Program* from file. */
Program* pProgram(FILE *inp)
{
  YYSTYPE result;
  yyscan_t scanner = lattepp__initialize_lexer(inp);
  if (!scanner) {
    fprintf(stderr, "Failed to initialize lexer.\n");
    return 0;
  }
  int error = yyparse(scanner, &result);
  lattepp_lex_destroy(scanner);
  if (error)
  { /* Failure */
    return 0;
  }
  else
  { /* Success */
    return result.program_;
  }
}

/* Entrypoint: parse Program* from string. */
Program* psProgram(const char *str)
{
  YYSTYPE result;
  yyscan_t scanner = lattepp__initialize_lexer(0);
  if (!scanner) {
    fprintf(stderr, "Failed to initialize lexer.\n");
    return 0;
  }
  YY_BUFFER_STATE buf = lattepp__scan_string(str, scanner);
  int error = yyparse(scanner, &result);
  lattepp__delete_buffer(buf, scanner);
  lattepp_lex_destroy(scanner);
  if (error)
  { /* Failure */
    return 0;
  }
  else
  { /* Success */
    return result.program_;
  }
}



