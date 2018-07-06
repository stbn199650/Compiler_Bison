%{
/*	Definition section */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define ANSI_COLOR_RED     	 		"\x1b[31m"
#define ANSI_COLOR_PURPLE			"\x1b[1;35m"
#define ANSI_COLOR_RESET   	 		"\x1b[0m"

/*Extern variables that communicate with lex*/

extern int yylineno;
extern int yylex();
extern FILE *yyin;
void yyerror(char *);
void print_error(char *s,char *variable);


/*	Symbol table function */

void create_symbol();										/*Create symbol table*/
void insert_symbol(char* id, char* type, int type_mode);	/*Insert an undeclared ID*/
void symbol_assign(int id, double data);					/*Assign value to a declared ID*/
void symbol_arith(int id, int arith, double data);
int lookup_symbol(char* id);								/*Check the ID exists in the symbol table*/
void dump_symbol();											/*List the ids and values of all data*/
void print_value(double value,int type);
void print_error(char *s,char *variable);
void compare_value(int comp_flag,double value1,double value2);

int var_num;		/*The number of the symbol*/
int assign_type;	//1:int 2:float
int error_flag;
int arith_flag;
int comp_flag;
int scope_num;
int row;
int col;
int if_tag;	//if exist or not
int else_tag;	//else exist or not
int if_condition;	//if condition is true oor not

/* Note that you should define the data structure of the symbol table yourself by any form */
struct data_block{
	char name[30];
	
	int type_num;	//1:int, 2:float
	char type_name[10];
	
	int int_value;
	double double_value;

	int assign_bit;
}table[100];

/* To ecord scope */
struct scope_blocl{
	int scope_value;
}scope[40][40];

%}

/* Token definition */
%token LB RB LCB RCB
%token PRINT PRINTLN IF ELSE FOR 
%token ADD SUB MUL DIV MOD INCREMENT DECREMENT
%token LARGETHAN LESSTHAN LARGE_EQ LESS_EQ EQ NOT_EQ
%token AND OR NOT
%token VAR INT DOUBLE FLOAT VOID
%token STRING NEWLINE 
%token ASSIGNMENT NUMBER FLOATNUM ID 
%token ASSIGN_ADD ASSIGN_SUB ASSIGN_MUL ASSIGN_DIV ASSIGN_MOD
%token PRINT PRINLN 


/* Type definition : Define the type by %union{} to specify the type of token */
%union {
	int i_val;
	double f_val;
	char string[150];
}

/* Type declaration : Use %type to specify the type of token within < > if the token or name of grammar rule will return value($$) */
%type <f_val> expression term print_func initializer higher_expr parenthesis 
%type <f_val> compound_stmt else_if_stmts else_if_stmt relation compare
%type <i_val> dcl stmt ASSIGNMENT type assign_arith LB RB
%type <i_val> ASSIGN_ADD ASSIGN_SUB ASSIGN_MUL ASSIGN_DIV ASSIGN_MOD
%type <i_val> INCREMENT DECREMENT

%type <i_val> NUMBER
%type <f_val> FLOATNUM
%type <i_val> VAR INT DOUBLE FLOAT
%type <string> ID 
%type <string> STRING

%%

/* Define your parser grammar rule and the rule action */
prog
	: 
	| prog stmt { error_flag=0; comp_flag=0;}
	;

stmt
	: dcl  
	| expression 
	| compound_stmt	
	| print_func 
	| parenthesis
	;

parenthesis	
	: LB	
	| RB	
	| LCB	{	
			//	printf("LCB\n");
				scope_num++;
				scope[row][col].scope_value = scope_num;
				printf("Block %d\n",scope_num);
				//printf("col.value: %d\n",scope[row][col].scope_value);
				//printf("(col-1).value: %d\n",scope[row][col-1].scope_value);
				
				if(col>0 &&(scope[row][col].scope_value - scope[row][col-1].scope_value) == 1){
					printf(ANSI_COLOR_PURPLE "Block %d is in Block %d\n" ANSI_COLOR_RESET, scope_num,scope_num-1);
				}
				else if(col>0 && (scope[row][col].scope_value - scope[row][col-1].scope_value) > 1){
					printf(ANSI_COLOR_PURPLE "Block %d is not accessible in Block %d\n" ANSI_COLOR_RESET, scope_num,scope_num-1);
				}else if(col==0 && row > 0){
					printf(ANSI_COLOR_PURPLE "Block %d is not accessible in Block %d\n"ANSI_COLOR_RESET,scope[row][0].scope_value,scope[row-1][0].scope_value);
				}
				
				col++;
			}
	| RCB
			{
			//	printf("RCB\n");
				col--;
				if(col==0)
					row++;
				//printf("RCB scope_col %d\n",scope_col);
			}
	;

compound_stmt
	: IF  	{ if_tag=1; printf("IF\n");}
	| else_if_stmts
	| relation
	;

relation
	: initializer compare initializer
				{ 
					//printf("$1 %lf $3 %lf\n",$1,$3);
					compare_value(comp_flag,$1,$3);
				}
	;

compare
	: LARGETHAN	{ comp_flag = 1; }
	| LESSTHAN	{ comp_flag = 2; }
	| LARGE_EQ	{ comp_flag = 3; }
	| LESS_EQ	{ comp_flag = 4; }
	| EQ		{ comp_flag = 5; }
	| NOT_EQ	{ comp_flag = 6; }
	;

else_if_stmts
	: 	 
	| else_if_stmts else_if_stmt 
	;

else_if_stmt
	: 
	| ELSE IF stmt	{ printf("ELSE IF\n");}
	| ELSE stmt		{ 
						if(if_condition == 0)
							else_tag = 1;
						printf("ELSE\n");
					}
	;

dcl
	: ID ASSIGNMENT higher_expr
						{
							
							if(lookup_symbol($1) == -1){
								error_flag = 1;
								print_error("Undeclare the variable",$1);
							}
							else{
								if(error_flag == 0){
									if(if_tag==1 && if_condition==1){	//if condition is true
										symbol_assign(lookup_symbol($1),$3);
										printf("ASSIGN\n");
										if_tag=2;
									}else if(if_tag==1 && else_tag == 1){
										//no if condition is true,execute else condition
										symbol_assign(lookup_symbol($1),$3);
										printf("ASSIGN\n");
										if_tag = 0;
										else_tag = 0;
									}else if(if_tag==0 && else_tag==0 && if_condition==0){
										symbol_assign(lookup_symbol($1),$3);
										printf("ASSIGN\n");
									}
									//else 
									//	printf("ASSIGN\n");
								}
							}
						}

	| ID assign_arith initializer
						{
							//printf("arith_flag %d\n",arith_flag);
							int x = lookup_symbol($1);
							if(x !=- 1){
								if(error_flag == 0){
									if(if_tag==1 && if_condition==1){	//if condition is true
										symbol_arith(x, arith_flag,$3);
										if_tag=2;
									}else if(if_tag==1 && else_tag == 1){
										//no if condition is true,execute else condition
										symbol_arith(x, arith_flag,$3);
										if_tag = 0;
										else_tag = 0;
									}else if(if_tag==0 && else_tag==0 && if_condition==0)
										symbol_arith(x, arith_flag,$3);
									}else{
										if(arith_flag == 1){
											printf("ASSIGN_ADD\n");
										}else if(arith_flag == 2){
											printf("ASSIGN_SUB\n");
										}else if(arith_flag == 3){
											printf("ASSIGN_MUL\n");
										}else if(arith_flag == 4){
											printf("ASSIGN_DIV\n");
										}else if(arith_flag == 5){
											printf("ASSIGN_MOD\n");
										}
									}
							}else
								print_error("Undeclare the variable",$1);
						}
	| VAR ID type ASSIGNMENT higher_expr 	
						{		
							//printf("yylineno in VAR %d\n",yylineno);
							if(lookup_symbol($2) == -1 && error_flag == 0){
									if(if_tag==1 && if_condition==1){	//if condition is true
										if(assign_type == 1){
											insert_symbol($2,"int",1);
											symbol_assign(lookup_symbol($2),$5);
											printf("ASSIGN\n");
										}
										else if(assign_type == 2){
											insert_symbol($2,"double",2);
											symbol_assign(lookup_symbol($2),$5);
											printf("ASSIGN\n");
										}
										if_tag=2;
									}else if(if_tag==1 && else_tag == 1){
										//no if condition is true,execute else condition
										if(assign_type == 1){
											insert_symbol($2,"int",1);
											symbol_assign(lookup_symbol($2),$5);
											printf("ASSIGN\n");
										}
										else if(assign_type == 2){
											insert_symbol($2,"double",2);
											symbol_assign(lookup_symbol($2),$5);
											printf("ASSIGN\n");
										}
										if_tag = 0;
										else_tag = 0;
									}else if(if_tag==0 && else_tag==0 && if_condition==0){
										if(assign_type == 1){
											insert_symbol($2,"int",1);
											symbol_assign(lookup_symbol($2),$5);
											printf("ASSIGN\n");
										}
										else if(assign_type == 2){
											insert_symbol($2,"double",2);
											symbol_assign(lookup_symbol($2),$5);
											printf("ASSIGN\n");
										}
									}else{
										printf("Insert a symbol: %s\nASSIGN\n",$2);
									}
						  	}else{
								print_error("Redeclare the variable",$2);
							}
						}
	| VAR ID type  		
						{ 	
							if(lookup_symbol($2) == -1 && error_flag == 0){
								if(if_tag==1 && if_condition==1){	//if condition is true
									if(assign_type == 1){
										insert_symbol($2,"int",1);
									}
									else if(assign_type == 2){
										insert_symbol($2,"double",2);
									}
									if_tag=2;
								}else if(if_tag==1 && else_tag == 1){
									//no if condition is true,execute else condition
									if(assign_type == 1){
										insert_symbol($2,"int",1);
									}
									else if(assign_type == 2){
										insert_symbol($2,"double",2);
									}
									if_tag = 0;
									else_tag = 0;
								}else if(if_tag==0 && else_tag==0 && if_condition==0){
									if(assign_type == 1){
										insert_symbol($2,"int",1);
									}
									else if(assign_type == 2){
										insert_symbol($2,"double",2);
									}
								}else{
									printf("Insert a symbol: %s\n",$2);
								}
							}else{
								error_flag = 1;
						  		print_error("Redeclare the variable",$2);
							}
						}	
	| ID INCREMENT	
					{
						int x = lookup_symbol($1);
						if(x != -1){
							printf("INCREMENT\n");

							if(if_tag==1 && if_condition==1){	//if condition is true
								if(table[x].type_num == 1) 		//int
									table[x].int_value += 1;
								else							//double
									table[x].double_value += 1;
								if_tag=2;
							}else if(if_tag==1 && else_tag == 1){
								if(table[x].type_num == 1) 		//int
									table[x].int_value += 1;
								else							//double
								table[x].double_value += 1;
								//no if condition is true,execute else condition
								if_tag = 0;
								else_tag = 0;
							}else if(if_tag==0 && else_tag==0 && if_condition==0){
								if(table[x].type_num == 1) 		//int
									table[x].int_value += 1;
								else							//double
									table[x].double_value += 1;
							}
						}
						else{
							print_error("Undeclare the variable",$1);
						}
					}
	| INCREMENT	ID
					{
						int x = lookup_symbol($2);
						if(x != -1){
							printf("INCREMENT\n");

							if(if_tag==1 && if_condition==1){	//if condition is true
								if(table[x].type_num == 1) 		//int
									table[x].int_value += 1;
								else							//double
									table[x].double_value += 1;
								if_tag=2;
							}else if(if_tag==1 && else_tag == 1){
								if(table[x].type_num == 1) 		//int
									table[x].int_value += 1;
								else							//double
								table[x].double_value += 1;
								//no if condition is true,execute else condition
								if_tag = 0;
								else_tag = 0;
							}else if(if_tag==0 && else_tag==0 && if_condition==0){
								if(table[x].type_num == 1) 		//int
									table[x].int_value += 1;
								else							//double
									table[x].double_value += 1;
							}
						}
						else{
							print_error("Undeclare the variable",$2);
						}
					}
	| ID DECREMENT	
					{
						int x = lookup_symbol($1);
						if(x != -1){
							printf("DECREMENT\n");
							
							if(if_tag==1 && if_condition==1){	//if condition is true
								if(table[x].type_num == 1) 		//int
									table[x].int_value -= 1;
								else							//double
									table[x].double_value -= 1;
								if_tag=2;
							}else if(if_tag==1 && else_tag == 1){
								if(table[x].type_num == 1) 		//int
									table[x].int_value -= 1;
								else							//double
								table[x].double_value -= 1;
								//no if condition is true,execute else condition
								if_tag = 0;
								else_tag = 0;
							}else if(if_tag==0 && else_tag==0 && if_condition==0){
								if(table[x].type_num == 1) 		//int
									table[x].int_value -= 1;
								else							//double
									table[x].double_value -= 1;
							}
						}
						else{
							print_error("Undeclare the variable",$1);
						}
					}
	| DECREMENT	ID
					{
						int x = lookup_symbol($2);
						if(x != -1){
							printf("DECREMENT\n");

							if(if_tag==1 && if_condition==1){	//if condition is true
								if(table[x].type_num == 1) 		//int
									table[x].int_value -= 1;
								else							//double
									table[x].double_value -= 1;
								if_tag=2;
							}else if(if_tag==1 && else_tag == 1){
								if(table[x].type_num == 1) 		//int
									table[x].int_value -= 1;
								else							//double
								table[x].double_value -= 1;
								//no if condition is true,execute else condition
								if_tag = 0;
								else_tag = 0;
							}else if(if_tag==0 && else_tag==0 && if_condition==0){
								if(table[x].type_num == 1) 		//int
									table[x].int_value -= 1;
								else							//double
									table[x].double_value -= 1;
							}
						}
						else{
							print_error("Undeclare the variable",$2);
						}
					}
	;

assign_arith
	: ASSIGN_ADD	{ arith_flag = 1; $$=$1;}
	| ASSIGN_SUB	{ arith_flag = 2; $$=$1;}
	| ASSIGN_MUL	{ arith_flag = 3; $$=$1;}
	| ASSIGN_DIV	{ arith_flag = 4; $$=$1;}
	| ASSIGN_MOD	{ arith_flag = 5; $$=$1;}
	;

type
	: INT 		{ assign_type = 1;}
	| FLOAT		{ assign_type = 2;}
	;

initializer
	: NUMBER	{ 	assign_type = 1; $$=$1;}
	| FLOATNUM	{	assign_type = 2; $$=$1;}
	| ID	{
				int x = lookup_symbol($1);
				if(x == -1){
					error_flag = 1;
					print_error("Undeclare the variable",$1);
				}
				else if(error_flag == 0){
					if(table[x].type_num == 1){
						assign_type = 1;
						$$ = table[x].int_value;	//return integer
					}else{
						assign_type = 2;
						$$ = table[x].double_value;	//return double
					}
				}
			}
	;

higher_expr
	: expression			{ $$ = $1;}
	| LB higher_expr RB		{ $$ = $2;}
	| LB higher_expr RB ADD higher_expr	{ printf("ADD\n");$$ = $2 + $5;}
	| LB higher_expr RB SUB higher_expr  { printf("SUB\n");$$ = $2 - $5;}
	| LB higher_expr RB MUL higher_expr  { printf("MUL\n");$$ = $2 * $5;}
	| LB higher_expr RB DIV higher_expr  
						{ 
							if($5 != 0){
								printf("DIV\n");
								$$ = $2 / $5;
							}else{
								error_flag = 1;
								print_error("DIV by 0",NULL);
							}
						}
	| LB higher_expr RB MOD higher_expr	
						{ 
							if($2 == 0){
								error_flag = 1;
								print_error("Mod by 0",NULL);
								$$ = $2;
							}
							else if(($2>(int)$2) || ($2<(int)$2) || ($5>(int)$5) || ($5<(int)$5)){
								error_flag = 1;
								print_error("Mod by Float",NULL);
							}
							else {
								printf("MOD\n");				
								$$ = (int)$2 % (int)$5;
							}
						}
	;

expression
	: term	{ $$=$1;}	
	| expression ADD /*term */ higher_expr
			{ 	
				printf("ADD\n");
				$$ = $1 + $3;
			}
	| expression SUB /*term*/ higher_expr
			{ 	
				printf("SUB\n");
				$$ = $1 - $3;
			}
	;

term
	: initializer	{ $$ = $1;}
	| term MUL /*initializer*/ higher_expr
				{ 	
					printf("MUL\n");
				  	$$ = $1 * $3;
				}
	| term DIV /*initializer*/ higher_expr
				{	
					if($3 == 0){
						error_flag = 1;
						print_error("Div by 0",NULL);
						$$ = $1;
					}
					else if(error_flag == 0){
						printf("DIV\n");
						$$ = $1 / $3;
					}
				}
	| term MOD /*initializer*/ higher_expr
				{	
					if($3 == 0){
						error_flag = 1;
						print_error("Mod by 0",NULL);
						$$ = $1;
					}
					else if(($1>(int)$1) || ($1<(int)$1) || ($3>(int)$3) || ($3<(int)$3)){
						error_flag = 1;
						print_error("Mod by Float",NULL);
					}
					else {
						printf("MOD\n");				
						$$ = (int)$1 % (int)$3;
					}
				}
	;


print_func
	: PRINT LB STRING RB 			
			{ 	printf("Print:\t \" %s \"\n",$3);	}

	| PRINTLN LB STRING RB 			
			{	printf("Println: \" %s \"\n",$3);	}

	| PRINT LB higher_expr RB
				{	
					if(error_flag == 0){
						if(assign_type == 1){
							printf("print:    %d\n",(int)$3);
						}
						else if(assign_type == 2){
							printf("print:    %.4lf\n",$3);
						}
					}
				}		
	| PRINTLN LB higher_expr RB
				{	
					if(error_flag == 0){
						if(assign_type == 1){
							printf("println:  %d\n",(int)$3);
						}
						else if(assign_type == 2){
							printf("println:  %.4lf\n",$3);
						}
					}
				}		
	;


%%

// define statement type Declaration, Assign, Print, Arithmetic and Branch

int main(int argc, char** argv)
{
    yylineno = 1;
    var_num = 0;
	scope_num = 0;
	row = 0; col = 0;
	if_tag = 0;
	else_tag = 0;
	if_condition = 0;

	for(int i=0;i<40;i++)
		for(int j=0;j<40;j++)
			scope[i][j].scope_value = 0;			

	yyparse();

	printf("\nTotal lines: %d \n\n",yylineno-1);
	
	dump_symbol();
    return 0;
}

void yyerror(char *s) {
    printf(ANSI_COLOR_RED "<Error> %s" ANSI_COLOR_RESET,s);
}

void compare_value(int comp_flag,double value1,double value2){
	
	if(comp_flag == 1){			// >
		if(value1 > value2){
			printf("true\n");
			if_condition = 1;
		}else{
			print_error("false",NULL);
		}
	}else if(comp_flag == 2){	// <
		if(value1 < value2){
			printf("true\n");
			if_condition = 1;
		}else{
			print_error("false",NULL);
		}
	}else if(comp_flag == 3){	// >=
		if(value2 >= value2){
			printf("true\n");
			if_condition = 1;
		}else{
			print_error("false",NULL);
		}
	}else if(comp_flag == 4){	// <=
		if(value1 <= value2){
			printf("true\n");
			if_condition = 1;
		}else{
			print_error("false",NULL);
		}
	}else if(comp_flag == 5){	// ==
		if(value1 == value2){
			printf("true\n");
			if_condition = 1;
		}else{
			print_error("false",NULL);
		}
	}else if(comp_flag == 6){	// !=
		if(value1 != value2){
			printf("true\n");
			if_condition = 1;
		}else{
			print_error("false",NULL);
		}
	}
	comp_flag = 0;
	
}

void print_error(char *s,char *variable){
	yyerror(s);
	if(variable!=NULL)
    	printf(ANSI_COLOR_RED " \"%s\"" ANSI_COLOR_RESET ,variable);
    printf(ANSI_COLOR_RED" on line %d\n" ANSI_COLOR_RESET,yylineno);
}

void print_value(double value,int type){
	if(type == 1){
		printf("print:  %d\n",(int)value);
	}
	else if(type == 2){
		printf("print:  %.2lf\n",value);
	}
}


void create_symbol(){
	printf("Create a symbol table\n");
}

void insert_symbol(char* s,char* type,int type_mode){
		if(var_num==0)
			create_symbol();

		printf("Insert a symbol: %s\n",s);
		strcpy(table[var_num].name,s);

		strcpy(table[var_num].type_name,type);
		table[var_num].type_num = type_mode;

		table[var_num].assign_bit = 0;

		var_num++;	
}

void symbol_assign(int id, double data){
	int type = table[id].type_num;
	
//	printf("ASSIGN\n");
	if(type==1){
		table[id].assign_bit = 1;
		table[id].int_value = (int)data;
	}
	else if(type==2){
		table[id].assign_bit = 1;
		table[id].double_value = data;
	}
	//printf("data: %lf\n",data);
}

void symbol_arith(int id, int arith, double data){
	int type = table[id].type_num;
	if(type==1){
		if(arith == 1){
			table[id].assign_bit = 1;
			table[id].int_value += (int)data;
			printf("ASSIGN_ADD\n");
		}else if(arith == 2){
			table[id].assign_bit = 1;
			table[id].int_value -= (int)data;
			printf("ASSIGN_SUB\n");
		}else if(arith == 3){
			table[id].assign_bit = 1;
			table[id].int_value *= (int)data;
			printf("ASSIGN_MUL\n");
		}else if(arith == 4){
			table[id].assign_bit = 1;
			table[id].int_value /= (int)data;
			printf("ASSIGN_DIV\n");
		}else if(arith == 5){
			table[id].assign_bit = 1;
			table[id].int_value %= (int)data;
			printf("ASSIGN_MOD\n");
		}
		//printf("value: %d\n",table[id].int_value);
	}
	else if(type==2){
		if(arith == 1){
			table[id].assign_bit = 1;
			table[id].double_value += data;
			printf("ASSIGN_ADD\n");
		}else if(arith == 2){
			table[id].assign_bit = 1;
			table[id].double_value -= data;
			printf("ASSIGN_SUB\n");
		}else if(arith == 3){
			table[id].assign_bit = 1;
			table[id].double_value *= data;
			printf("ASSIGN_MUL\n");
		}else if(arith == 4){
			table[id].assign_bit = 1;
			table[id].double_value /= data;
			printf("ASSIGN_DIV\n");
		}else if(arith == 5){
			print_error("MOD by float number",NULL);
		}
	}
}

int lookup_symbol(char* sym){
	for(int i=0;i<var_num;i++){
		if(strcmp(table[i].name,sym)==0)
			return i;
	}
	return -1;
}

void dump_symbol(){
	printf("---  Dump the symbol table  ---\n\n");
	printf("NO.   Type     ID    Data\n");
	for(int i=0;i<var_num;i++){
		if(table[i].assign_bit==0){
			if(table[i].type_num==1)
				printf("%2d    int      %s    undefined\n",i+1,table[i].name);
			else
				printf("%2d    float    %s    undefined\n",i+1,table[i].name);
		}
		else{
			if(table[i].type_num==1)
				printf("%2d    int      %s    %3d\n",i+1,table[i].name,table[i].int_value);
			else 
				printf("%2d    float    %s     %.4lf\n",i+1,table[i].name,table[i].double_value);
		}
	}
}
