%{
#include <stdio.h>
#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"
#include <string.h>

#include "uthash.h"

#include <errno.h>
  //#include <search.h>

extern FILE *yyin;
int yylex(void);
int yyerror(const char *);

extern char *fileNameOut;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;

LLVMValueRef Function;
LLVMBasicBlockRef BasicBlock;
LLVMBuilderRef Builder;

//struct hsearch_data params;
//struct hsearch_data tmps;

int params_cnt=0; // Number of Variable Parameters used in program

//STRUCTURE TO HOLD TEMPORARY VARIABLES AND THEIR HASHED EXPRESSIONS

struct TmpMap{
  char *key;                  /* key */
  LLVMValueRef val;                /* data */
  UT_hash_handle hh;         /* makes this structure hashable */
};
 

struct TmpMap *map = NULL;    /* important! initialize to NULL */

void add_tmp(char *tmp, LLVMValueRef val) { 
  struct TmpMap *s; 
  s = malloc(sizeof(struct TmpMap)); 
  s->key = strdup(tmp); 
  s->val = val; 
  HASH_ADD_KEYPTR( hh, map, s->key, strlen(s->key), s ); 
}

LLVMValueRef get_val(char *tmp) {
  struct TmpMap *s;
  HASH_FIND_STR( map, tmp, s );  /* s: output pointer */
  if (s) 
    return s->val;
  else 
    return NULL; // returns NULL if not found
}

// STRUCTURE TO HOLD IDENT VARIABLES AND THEIR POSITIONS 

struct IdMap{
  char *key;                  /* key */
  int val;                /* data */
  UT_hash_handle hh;         /* makes this structure hashable */
};
 

struct IdMap *map_2 = NULL;    /* important! initialize to NULL */

void add_id(char *tmp, int val) { 
  struct IdMap *s; 
  s = malloc(sizeof(struct IdMap)); 
  s->key = strdup(tmp); 
  s->val = val; 
  HASH_ADD_KEYPTR( hh, map_2, s->key, strlen(s->key), s ); 
}

int get_val_2(char *tmp) {
  struct IdMap *s;
  HASH_FIND_STR( map_2, tmp, s );  /* s: output pointer */
  if (s) 
    return s->val;
  else 
    return -1;
}

%}



%token VARS TMP ID NUM ASSIGN SEMI COMMA MINUS PLUS MULTIPLY DIVIDE RAISE LESSTHAN //DECLARATION OF TOKENS FOUND OUT BY THE SCANNER

%union {
  char *tmp;
  int num;
  char *id;
  LLVMValueRef val;
}

%type <tmp> TMP 
%type <num> NUM 
%type <id> ID
%type <val> expr stmt stmtlist;


//PRECEDENCE LIST

%left LESSTHAN
%left PLUS MINUS 
%left MULTIPLY DIVIDE RAISE 

%start program

%%

program: decl stmtlist 
{ 
  /* 
    IMPLEMENT: FOUND LAST HASHED KEY IN TMP STRUCTURE AND RETURNED THE SAME FUNCTIONS USED HASH_COUNT() AND ITERATOR OF HASH TABLE
  */
  unsigned int num_users;
  num_users = HASH_COUNT(map);
  unsigned int count = 1;
  LLVMValueRef dummy;
  struct TmpMap *s;
  for(s = map;s!=NULL;s=s->hh.next){
	if(count == num_users){
		dummy = s->val;
		break;	
	}
	count++;
  }
	
  LLVMBuildRet(Builder,dummy);
//LLVMConstInt(LLVMInt64TypeInContext(Context),dummy,(LLVMBool)1)
}
  ;

decl: VARS varlist SEMI 
{  
  /* Now we know how many parameters we need.  Create a function type
     and add it to the Module */

  LLVMTypeRef Integer = LLVMInt64TypeInContext(Context);

  LLVMTypeRef *IntRefArray = malloc(sizeof(LLVMTypeRef)*params_cnt);
  int i;
  
  /* Build type for function */
  for(i=0; i<params_cnt; i++)
    IntRefArray[i] = Integer;

  LLVMBool var_arg = 0; /* false */
  LLVMTypeRef FunType = LLVMFunctionType(Integer,IntRefArray,params_cnt,var_arg);

  /* Found in LLVM-C -> Core -> Modules */
  char *tmp, *out = fileNameOut;

  if ((tmp=strchr(out,'.'))!='\0')
    {
      *tmp = 0;
    }

  /* Found in LLVM-C -> Core -> Modules */
  Function = LLVMAddFunction(Module,out,FunType);

  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);
}
;

varlist:   varlist COMMA ID 
{
  /* IMPLEMENT: remember ID and its position so that you can
     reference the parameter later
   */
    if(get_val_2($3) == -1){
    	add_id($3,params_cnt);
	params_cnt++;
    }
    else{
	printf("ERROR EXITING REDECLARATION OF VARIABLE %s\n",$3);   
	exit(0);
     }
  //  get_val_2($3);
  params_cnt++;
}
| ID
{
     if(get_val_2($1) == -1){
    	add_id($1,params_cnt);
	params_cnt++;
    }
    else{
	printf("ERROR EXITING REDECLARATION OF VARIABLE %s\n",$1);
	exit(0);
     }
    //get_val_2($1); 	
  /* IMPLEMENT: remember ID and its position for later reference*/
  
}
;

stmtlist:  stmtlist stmt { $$ = $2; }
| stmt                   { $$ = $1; }
;         

stmt: TMP ASSIGN expr SEMI
{
  /* IMPLEMENT: remember temporary and associated expression $3 */
  
  add_tmp($1,$3);
  $$ = $3;
}
;

expr:   expr MINUS expr
{
  /* IMPLEMENT: subtraction */
  	$$ = LLVMBuildSub(Builder,$1,$3,"");
} 
     | expr PLUS expr
{
  /* IMPLEMENT: addition */
  $$ = LLVMBuildAdd(Builder,$1,$3,"");
}
      | MINUS expr 
{
  /* IMPLEMENT: negation */
    $$ = LLVMBuildNeg(Builder,$2,"");
}
      | expr MULTIPLY expr
{
  /* IMPLEMENT: multiply */ 
   $$ = LLVMBuildMul(Builder,$1,$3,"");
  
}
      | expr DIVIDE expr
{
  /* IMPLEMENT: divide */
    $$ = LLVMBuildSDiv(Builder,$1,$3,"");
}
      | expr RAISE expr
{
	if(LLVMIsAConstantInt($3)){    //CHECKING WHETHER EXPONENT IS A CONSTANT IF IT IS GOING INSIDE ......
	  if(LLVMConstIntGetSExtValue($3) >= 0){  // CHECKING WHETHER EXPONENT IS NON-NEGATIVE IF IS GOING INSIDE .....
		int i;
		LLVMValueRef prod = LLVMConstInt(LLVMInt64Type(),1,1);   //PREPARING "prod" variable for Product ACCUMULATION		
		for(i=0;i<LLVMConstIntGetSExtValue ($3);i++){     // USING INTEGER VALUE OF EXPONENT TO ITERATE 
			prod = LLVMBuildMul(Builder,$1,prod,"");  // BUILDING ONE MULTIPLIES FOR EXPONENTIATION
		}
		$$ = prod;  //FINAL RESULT STORED
	 }
	 else{
		yyerror("ERROR GENERATED NEGATIVE EXPONENT NOT ALLOWED !!\n");
		exit(0);
	}
       }
	else{
		yyerror("ERROR GENERATED EXPONENT NOT A CONSTANT !!\n");
		exit(0);
	}
}
      | expr LESSTHAN expr
{
	LLVMValueRef a;
        $$ =  LLVMBuildZExt(Builder, LLVMBuildICmp(Builder,LLVMIntSLT,$1,$3,""),LLVMInt64Type(),""); // COMPARING TWO ARGUMENTS AND THEN USING BUILDER FUNCTION TO RETURN INTEGER 64 VALUE WITH ZERO EXTENSION RETURNS 0 in FALSE AND 1 ON TRUE
}
      | NUM
{ 
  /* IMPLEMENT: constant */
    $$ = LLVMConstInt(LLVMInt64Type(),$1,0);
}
      | ID
{
  /* IMPLEMENT: get reference to function parameter
     Hint: LLVMGetParam(...)
   */   if(get_val_2($1) == -1){         // UNDECLARED VARIABLE USED ERROR CHECKING 
		printf("VARIABLE %s NOT DECLARED !!\n",$1);
		exit(0);
	}
	else
		$$ = LLVMGetParam(Function,get_val_2($1));
}
      | TMP
{
  /* IMPLEMENT: get expression associated with TMP */
	if(get_val($1) == NULL){
		printf("TEMPORARY VARIABLE %s NOT DECLARED !!\n",$1);
		exit(0);
	}
	else{
  		$$ = get_val($1);
	}
}
;

%%


void initialize()
{
  /* IMPLEMENT: add something here if needed */
}

int yyerror(const char *msg)
{
  printf("%s",msg);
  return 0;
}
