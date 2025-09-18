#ifndef ECZEMA_HH
#define ECZEMA_HH

// #include "llvm/ADT/APFloat.h"
// #include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <strings.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <assert.h>
#include <time.h>

/* Forward declarations */
typedef struct Token Token;
typedef struct File File;
typedef struct JSValue JSValue;
typedef enum JSValueTag JSValueTag;
typedef struct JSObject JSObject;
typedef struct Property Property;
typedef struct PropertyMap PropertyMap;
typedef struct ASTNode ASTNode;
typedef enum ASTKind ASTKind;
typedef struct Lexer Lexer;
typedef struct Parser Parser;
typedef struct VM VM;
typedef struct Env Env;
typedef struct JSFunction JSFunction;
typedef struct JSError JSError;
//typedef struct Type Type;

/* 
    File / Token 
*/

// for file metadata
struct File {
  char *name;
  int file_no;
  char *contents;     // full file contents ('\0' terminated)

  // For #line directive
  char *display_name;
  int line_delta;
};

// TokenKind definitions (extended for JS)
typedef enum {
    tok_identifier,
    tok_punct,
    tok_keyword,
    tok_str,
    tok_num,
    tok_regex,          // not sure what I can do with this thought ngl
    tok_template,
    tok_eof,
} TokenKind;

// Token interaction info
struct Token {
    TokenKind kind;     // Token kind
    Token *next;        // next Token in stream
    int64_t ival;       // if tok_num && integer 
    long double fval;   // If tok_num && float
    char *loc;          // ptr into File->contents where token starts
    int len;            // tok len
    //Type *ty;           // optional typed info
    char *str;          // String literal (including '\0')

    File *file;         // src file
    int line_no;        // line no.
    bool at_bol;        // token at beginning of line
    bool has_space;     // true if token follows a space character
};

/* 
    Runtime value / objects 
*/

// JSValueTag: low-level tag set returned by typeof() and used in the runtime
typedef enum JSValueTag {
    JSV_TAG_OBJECT,
    JSV_TAG_UNDEFINED,
    JSV_TAG_NULL,
    JSV_TAG_BOOLEAN,
    JSV_TAG_NUMBER,
    JSV_TAG_STRING,
    JSV_TAG_SYMBOL,
    JSV_TAG_BIGINT,
    JSV_TAG_FUNCTION,
    JSV_TAG_REGEXP,
} JSValueTag;

// JSValue: a simple tagged-union representing a JavaScript value
struct JSValue {
    JSValueTag tag;
    union {
        double as_number;
        bool   as_bool;
        char  *as_string;
        int64_t as_int64;
        JSObject *as_obj;   // obj/func/regex
        void *as_ptr;       // generic ptr slot
    } u;
};

/* Property / Property map - minimal implementation */
struct Property {
    char *key;           // property name (NUL terminated)
    JSValue value;       // property value
    uint32_t attributes; // writable/enumerable/configurable flags
    Property *next;      // collision chain
};

/*
    Each property key is hashed, the idx is computed as hash(key) & bucket_count
    buckets[i] holds a pointer to the head of a linked list of property entries whose 
    hash mapped to that bucket. Collisions can be resolved be chaining property->next

    Avg chain len = prop_ct / bucket_ct (load factor). Lookup cost is O(1 + load factor)
    when prop_ct grows past a certain threshold the map will need to be resized and everything rehashed.
    say bc = 4, pc = 0 -> insert, insert, insert -> bc = 4, pc = 3. If threshold is .75, then at this point in time the table will resize itself.
*/
struct PropertyMap {
    size_t bucket_count;    // size of hash-table array (only changes when table is reallocated/resized)
    Property **buckets;     // array of bucket_count pointers (properties)
    size_t property_count;  // total # props in map (all buckets combined)
};  

// JSObject: minimal object shape with prototype and property map
struct JSObject {
    PropertyMap *props;
    JSObject *prototype;
    JSValueTag class_tag; // (Object/Array/Function/etc.)
    uint32_t flags;
    void *internal;       // pointer for things like array storage and other object specific data
};

/* 
    AST 
*/
typedef enum ASTKind {
    AST_Program,
    AST_FunctionDecl,
    AST_FunctionExpr,
    AST_VarDecl,
    AST_BlockStmt,
    AST_ExpressionStmt,
    AST_IfStmt,
    AST_WhileStmt,
    AST_ForStmt,
    AST_ReturnStmt,
    AST_BinaryExpr,
    AST_UnaryExpr,
    AST_AssignmentExpr,
    AST_CallExpr,
    AST_MemberExpr,
    AST_ArrayExpr,
    AST_ObjectExpr,
    AST_Identifier,
    AST_Literal,
    AST_ThisExpr,
    AST_NewExpr,
    AST_TernaryExpr,
    AST_LogicalExpr,
    AST_TemplateLiteral,
    AST_RegexLiteral,       // absolute woke nonsense right here
    // ... add as needed
} ASTKind;

// Source range for nodes
typedef struct SourceRange {
    File *file;
    int start_offset; // byte offset into file->contents
    int end_offset;
} SourceRange;

struct ASTNode {
    ASTKind kind;
    SourceRange range;

    union {
        // Identifier
        struct { char *name; } id;

        // Literal
        struct { JSValue value; } lit;

        // Binary / logical / assignment
        struct { ASTNode *left; ASTNode *right; char *op; } binary;

        // Unary
        struct { ASTNode *operand; char *op; bool prefix; } unary;         // Mainly ++/-- since they can be pre and post

        // Call
        struct { ASTNode *callee; ASTNode **args; int argc; } call;        // I don't like that it's called callee instead of caller

        // Member
        struct { ASTNode *object; ASTNode *property; bool computed; } member;

        // Function
        struct { char *name; ASTNode **params; int param_count; ASTNode *body;} function;

        struct { ASTNode **items; int count; } list; 
        /* 
            say kind is AST_blockstmt, which is a ordered list of statements. 
            that would fall under the list in this category. 
            Most similar to a *Basic Block*
        */
    } as;
};

/* 
    Lexer / Parser prototypes 
*/
struct Lexer {
    File *file;
    char *cur;        // current pointer into file->contents
    Token *head;      // linked token list - shouldn't change throughout execution i think
    Token *curtok;    // last produced token
    int line_no;
};

struct Parser {
    Lexer *lex;
    Token *tok;       // Semi-redundant with the lexer's tok*, used to peek consume and backtrack w/o affecting lexer
    Env *env;         // parser state: current scope, flags
};

// Lexer API 
Lexer *lexer_new(File *file);
void lexer_free(Lexer *lex);
Token *lexer_next(Lexer *lex); // get next tok

// Parser API
Parser *parser_new(Lexer *lex);
void parser_free(Parser *p);
ASTNode *parse_program(Parser *p);

/*
    Environment / Function / VM 
*/
struct Env {
    PropertyMap *bindings; // local bindings
    Env *parent;           // outer scope
    bool is_function_scope;
    // add flags as needed
};

struct JSFunction {
    JSObject obj;      // functions are objects
    ASTNode *ast;      // AST node for the function body
    Env *env;          // closure environment
                       // if native C function, handle that make ast null
};

struct VM {
    JSObject *global;
    Env *global_env;
    // simple stack/frames for interpreter
    JSValue *stack;
    size_t stack_capacity;
    size_t sp;
    //void *gc_state;
};

// VM API 
VM *vm_new(void);
void vm_free(VM *vm);
JSValue vm_eval(VM *vm, ASTNode *program);

/* 
    GC / allocator / builtins / error 
*/

// GC + allocator API
// make a garbage collectr here

struct JSError {
    char *message;
    File *file;
    int line;
    int col;
};

// Builtins
void init_builtins(VM *vm)

#endif // ECZEMA_HH