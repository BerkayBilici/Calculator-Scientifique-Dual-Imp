%{
#ifdef YYDEBUG
	yydebug = 1;
#endif
extern int yylineno;
extern char expression_buffer[1024]; 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int yylex();

void yyerror (const char *msg) 
{
	return;
}

//structs

typedef struct Term{
	int coefficient;
	int coefficient_denominator;
	int powers[26];
	struct Term * next;
} Term;

typedef struct Polynomial{
	Term* head;
} Polynomial;

typedef struct Function {
	char fname;
	int parameter_count;
	char parameters[26];
	Polynomial* body;
} Function;

typedef struct ErrorNode {
	char errorMessage[150];
	struct ErrorNode* next;
} ErrorNode;

typedef struct OutputNode {
    char line[20000]; // Buffer size increased
    struct OutputNode* next;
} OutputNode;

// Linked List for Arguments (Fixes Shift/Reduce issues)
typedef struct ArgNode {
    Polynomial* poly;
    struct ArgNode* next;
} ArgNode;

//global var

ErrorNode* error_head = NULL;
OutputNode* output_head = NULL;
OutputNode* output_tail = NULL;

int arity_error_reported[10000][26] = {{0}};

Function FunctionList[26]; // a-z
char current_params[26];
int current_param_count = 0;

int current_calc_line = 1;

int definition_mode = 0; 
int has_semantic_errors = 0;
int undefined_function_reported[26] = {0};


//prototypes

int greatest_common_divisor(int a, int b);
void add_error(const char* error);
void add_output(char* expr, Polynomial* p);
void report_arity_error(int line, char f);
void report_undefined_function(int line, char f);
void print_results();

int integer_exponent(int a, int b);
Term* create_term(int coeff, int denom);
Term* deep_copy_term(Term* t);
void delete_term(Term* t);
int compare_terms(Term* t1, Term* t2);
Term* multiply_terms(Term* t1, Term* t2);

Polynomial* create_polynomial();
Polynomial* create_polynomial_with_term(Term * t);
void delete_polynomial(Polynomial* p);
void add_term_to_polynomial(Polynomial* p, Term* t);
Polynomial* add_polynomial_to_polynomial(Polynomial* p1, Polynomial* p2);
Polynomial* make_polynomial_negative(Polynomial* p);
Polynomial* multiply_polynomials(Polynomial* p1, Polynomial* p2);

void create_function(char f, Polynomial* p);
Function* find_function(char f);
int check_parameter_defined(char x);
int check_parameter(Function* f, char x);

Polynomial* integral(Function* f, char x, int count);
Polynomial* derive(Function* f, char x, int count);
Polynomial* evaluate_function(Function* f, Polynomial** args, int count);
void print_polynomial_string(char* buffer, Polynomial* p);

ArgNode* create_arg_node(Polynomial* p);
ArgNode* append_arg_node(ArgNode* list, Polynomial* p);

%}

%union {
	int value;
	char str;
	struct Polynomial *polyPtr;
    struct ArgNode *argListPtr;
};
%locations

%token <str> tIDENTIFIER 
%token <value> tINTEGER
%token tPLUS tMINUS tMUL tEXP tLPR tRPR tASSIGN tCOMMA tSEMICOLON tDERIVATION tINTEGRATION tCALCULATE 
%type <polyPtr> oe oe1 oe2 ee ee1 ee2 integration derivation
%type <argListPtr> ee3

%expect 1
%start program

%%

program:	  statement_list
			| empty
;

statement_list:	  statement
			| statement statement_list
;

statement:	  fn_def
			| calc
			| tSEMICOLON 
;


fn_def: 	  tIDENTIFIER tLPR var_parameters tRPR tASSIGN { definition_mode = 1; } oe tSEMICOLON	{ 
                                                                    definition_mode = 0;
                                                                    create_function($1, $7); 
                                                                }
			| tIDENTIFIER tLPR tRPR tASSIGN { definition_mode = 1; } oe tSEMICOLON					{ 
                                                                    definition_mode = 0;
                                                                    create_function($1, $6); 
                                                                }
;

var_parameters:   tIDENTIFIER						{	current_params[current_param_count] = $1;
														current_param_count++;
													}
				| var_parameters tCOMMA tIDENTIFIER	{	current_params[current_param_count] = $3;
														current_param_count++;
													}
;

oe:				  oe tPLUS oe1	{	$$ = add_polynomial_to_polynomial($1,$3);
									delete_polynomial($3);
								}
				| oe tMINUS oe1	{	$$ = add_polynomial_to_polynomial($1, make_polynomial_negative($3));
									delete_polynomial($3);
								}	
				| oe1			{	$$ = $1; }
;

oe1:			  oe1 tMUL oe2	{	$$ = multiply_polynomials($1,$3);
									delete_polynomial($1);
									delete_polynomial($3);
								}
				| oe1 oe2		{	$$ = multiply_polynomials($1,$2);
									delete_polynomial($1);
									delete_polynomial($2);
								}
				| oe2			{	$$ = $1; }
;

oe2:			  tINTEGER					{	Term* temp = create_term($1,1);
												$$ = create_polynomial_with_term( temp ); 
												delete_term(temp);
											}
				| tIDENTIFIER				{	
                                                if (definition_mode && !check_parameter_defined($1)) {
                                                    char tempMsg[150];
                                                    sprintf(tempMsg, "%d_UNDEFINED_FUNCTION_PARAMETER_(%c)", yylineno, $1);
                                                    add_error(tempMsg);
                                                }
                                                Term* temp = create_term(1,1);
												temp->powers[$1 - 'a'] += 1;
												$$ = create_polynomial_with_term(temp);
												delete_term(temp);
											}
				| tLPR oe tRPR				{	$$ = $2; }
				| tIDENTIFIER tEXP tINTEGER	{	
                                                if (definition_mode && !check_parameter_defined($1)) {
                                                    char tempMsg[150];
                                                    sprintf(tempMsg, "%d_UNDEFINED_FUNCTION_PARAMETER_(%c)", yylineno, $1);
                                                    add_error(tempMsg);
                                                }
                                                Term* temp = create_term(1,1);
												temp->powers[$1 - 'a'] += $3;
												$$ = create_polynomial_with_term(temp);
												delete_term(temp);
											}
				| tINTEGER tEXP tINTEGER	{	Term* temp = create_term(integer_exponent($1,$3),1);
												$$ = create_polynomial_with_term(temp);
												delete_term(temp);
											}
;

calc:      tCALCULATE {
                current_calc_line = @1.first_line;
                for (int i = 0; i < 26; i++) {
                    undefined_function_reported[i] = 0;
                }
          }
          ee tSEMICOLON {
                if (!has_semantic_errors) {
                    add_output(expression_buffer, $3);
                }
                delete_polynomial($3);
          }
;


ee:			  ee tPLUS ee1 	{	$$ = add_polynomial_to_polynomial($1,$3);
								delete_polynomial($3);
							}
			| ee tMINUS ee1	{	$$ = add_polynomial_to_polynomial($1, make_polynomial_negative($3));
								delete_polynomial($3);
							}
			| ee1			{	$$ = $1; }
;

ee1:		  ee1 tMUL ee2	{	$$ = multiply_polynomials($1,$3);
								delete_polynomial($1);
								delete_polynomial($3);
							}
			| ee1 ee2		{	$$ = multiply_polynomials($1,$2);
								delete_polynomial($1);
								delete_polynomial($2);
							}
			| ee2			{	$$ = $1; }
;

ee2:		  tINTEGER					{	Term* temp = create_term($1,1);
											$$ = create_polynomial_with_term( temp ); 
											delete_term(temp);
										}
			| tIDENTIFIER				{	Term* temp = create_term(1,1);
											temp->powers[$1 - 'a'] += 1;
											$$ = create_polynomial_with_term(temp);
											delete_term(temp);
										}
			| tLPR tRPR					{	$$ = create_polynomial(); }
            
			| tIDENTIFIER tLPR ee3 tRPR {
                Polynomial* args[50];
                int arg_count = 0;
                ArgNode* temp = $3;
                while(temp) {
                    args[arg_count++] = temp->poly;
                    ArgNode* toFree = temp;
                    temp = temp->next;
                    free(toFree);
                }

                Function* f = find_function($1);
				if (f != NULL) {
					if (f->parameter_count != arg_count) {
						report_arity_error(@1.first_line, $1);
						$$ = create_polynomial(); 
					} else {
						$$ = evaluate_function(f, args, arg_count);
					}
				} 
    			else {
        			if (arg_count == 1 && args[0]->head != NULL) {
             			// f( expr )  ->  f * expr
						Term* tIdentifier = create_term(1,1);
						tIdentifier->powers[$1 - 'a'] += 1;
						Polynomial* pIdentifier = create_polynomial_with_term(tIdentifier);
						delete_term(tIdentifier);
             
             			$$ = multiply_polynomials(pIdentifier, args[0]);
             			delete_polynomial(pIdentifier);
        			}
					else {
             			report_undefined_function(@1.first_line, $1);
             			$$ = create_polynomial();
        			}
    			}
						
                for(int i=0; i<arg_count; i++) delete_polynomial(args[i]);
            }

            | tLPR ee3 tRPR {
                Polynomial* args[50];
                int arg_count = 0;
                ArgNode* temp = $2;
                while(temp) {
                    args[arg_count++] = temp->poly;
                    ArgNode* toFree = temp;
                    temp = temp->next;
                    free(toFree);
                }

                if (arg_count > 1) {
                    char tempMsg[150];
                    sprintf(tempMsg, "%d_MISSING_FUNCTION_NAME", current_calc_line);
                    add_error(tempMsg);
                    $$ = create_polynomial();
                    // Delete all
                    for(int i=0; i<arg_count; i++) delete_polynomial(args[i]);
                } else {
                    /* (x+y) case */
                    if (arg_count == 1) {
                        $$ = args[0]; // Pass ownership up
                    } else {
                        $$ = create_polynomial();
                    }
                }
            }
			| tIDENTIFIER tLPR tRPR {

										Function* f = find_function($1);
										if (f != NULL) {
											if (f->parameter_count != 0) {
												report_arity_error(@1.first_line, $1);
												$$ = create_polynomial();
											} else {
												$$ = evaluate_function(f, NULL, 0);
											}
										} else {
											report_undefined_function(@1.first_line, $1);
											$$ = create_polynomial();
										}
										}


			| tIDENTIFIER tEXP tINTEGER	{	Term* temp = create_term(1,1);
											temp->powers[$1 - 'a'] += $3;
											$$ = create_polynomial_with_term(temp);
											delete_term(temp);
										}
			| tINTEGER tEXP tINTEGER	{	Term* temp = create_term(integer_exponent($1,$3),1);
											$$ = create_polynomial_with_term(temp);
											delete_term(temp);
										}
			| integration				{	$$ = $1;}
			| derivation				{	$$ = $1;}
;

ee3:		  ee {
                $$ = create_arg_node($1);
            }
			| ee3 tCOMMA ee {
                $$ = append_arg_node($1, $3);
            }
;

integration:  tINTEGRATION tLPR tIDENTIFIER tCOMMA tIDENTIFIER tCOMMA tINTEGER tRPR	{		
                                                                                            Function* func = find_function($3);
																							if(func != NULL){
																								if(check_parameter(func,$5) == 1){
																									$$ = integral(func,$5,$7);
																								}
																								else{
																									char tempMsg[150];
																									sprintf(tempMsg,"%d_UNDEFINED_VARIABLE_FOR_INTEGRATION_(%c)",@5.first_line,$5);
																									add_error(tempMsg);
																									$$ = create_polynomial();
																								}
																							}
																							else{
																								char tempMsg[150];
																								sprintf(tempMsg,"%d_UNDEFINED_FUNCTION_FOR_INTEGRATION_(%c)",@3.first_line,$3);
																								add_error(tempMsg);
																								$$ = create_polynomial();
																							}
																					}
;

derivation:   tDERIVATION tLPR tIDENTIFIER tCOMMA tIDENTIFIER tCOMMA tINTEGER tRPR	{		
                                                                                            Function* func = find_function($3);
																							if(func != NULL){
																								if(check_parameter(func,$5) == 1){
																									$$ = derive(func,$5,$7);
																								}
																								else{
																									char tempMsg[150];
																									sprintf(tempMsg,"%d_UNDEFINED_VARIABLE_FOR_DERIVATION_(%c)",@5.first_line,$5);
																									add_error(tempMsg);
																									$$ = create_polynomial();
																								}
																							}
																							else{
																								char tempMsg[150];
																								sprintf(tempMsg,"%d_UNDEFINED_FUNCTION_FOR_DERIVATION_(%c)",@3.first_line,$3);
																								add_error(tempMsg);
																								$$ = create_polynomial();
																							}
																					}
;

empty: 
;
			
%%
//functions
int greatest_common_divisor(int a, int b){
	if(b == 0){
		return a;
	} 
	return greatest_common_divisor(abs(b), abs(a % b));
	}

int get_error_line(const char* msg) {
    int line = 0;
    int i = 0;
    while (msg[i] >= '0' && msg[i] <= '9') {
        line = line * 10 + (msg[i] - '0');
        i++;
    }
    return line;
}

void add_error(const char* error){
    has_semantic_errors = 1;

    ErrorNode* newError = (ErrorNode*)malloc(sizeof(ErrorNode));
    strcpy(newError->errorMessage, error);
    newError->next = NULL;

    if (error_head == NULL){
        error_head = newError;
        return;
    }

    int new_line = get_error_line(newError->errorMessage);

    ErrorNode* curr = error_head;
    ErrorNode* prev = NULL;

    while (curr) {
        int curr_line = get_error_line(curr->errorMessage);

        if (new_line < curr_line ||
            (new_line == curr_line &&
             strcmp(newError->errorMessage, curr->errorMessage) < 0)) {

            if (curr == error_head) {
                newError->next = curr;
                error_head = newError;
            } else {
                newError->next = curr;
                prev->next = newError;
            }
            return;
        }

        prev = curr;
        curr = curr->next;
    }

    prev->next = newError;
}

void add_output(char* expr, Polynomial* p) {
    if (has_semantic_errors) return;
    OutputNode* node = (OutputNode*)malloc(sizeof(OutputNode));
    node->next = NULL;
    
    char resultStr[1024];
    print_polynomial_string(resultStr, p);
    
    sprintf(node->line, "%s=%s", expr, resultStr);
    
    if (output_head == NULL) {
        output_head = node;
        output_tail = node;
    } else {
        output_tail->next = node;
        output_tail = node;
    }
}


void report_arity_error(int line, char f) {
    if (line < 0) line = 0;
    if (line >= 10000) line = 9999;

    int idx = f - 'a';
    if (idx < 0 || idx >= 26) return;

    if (arity_error_reported[line][idx]) return;      
    arity_error_reported[line][idx] = 1;

    char tempMsg[150];
    sprintf(tempMsg, "%d_ARITY_CONTRADICTION_(%c)", line, f);
    add_error(tempMsg);
}



void report_undefined_function(int line, char f) {
    int hasDefined = 0;
    for (int i = 0; i < 26; i++) {
        if (FunctionList[i].fname != 0) {
            hasDefined = 1;
            break;
        }
    }
    if (!hasDefined) return;

    int idx = f - 'a';
    if (idx < 0 || idx >= 26) return;
    if (undefined_function_reported[idx]) return;
    undefined_function_reported[idx] = 1;

    char tempMsg[150];
    sprintf(tempMsg, "%d_UNDEFINED_FUNCTION_(%c)", line, f);
    add_error(tempMsg);
}



void print_results() {
    if (has_semantic_errors) {
        ErrorNode* temp = error_head;
        while(temp){
            printf("%s\n", temp->errorMessage);
            ErrorNode* toDel = temp;
            temp = temp->next;
            free(toDel);
        }
    } else {
        OutputNode* temp = output_head;
        while(temp) {
            printf("%s\n", temp->line);
            OutputNode* toDel = temp;
            temp = temp->next;
            free(toDel);
        }
    }
}


int integer_exponent(int a, int b){
	int result = 1;
	for(int i = 0; i < b; i++){
		result = result * a;
	}
	return result;
}

Term* create_term(int coeff, int denom) {
	Term* t = (Term*)malloc(sizeof(Term));
	t->coefficient = coeff;
	t->coefficient_denominator= denom;
	for(int i = 0; i < 26; i++) t->powers[i] = 0;
	t->next = NULL;
	return t;
}

Term* deep_copy_term(Term* t){
    if(!t) return NULL;
	Term* copy = create_term(t->coefficient,t->coefficient_denominator);
	for(int i = 0; i < 26; i++){
		copy->powers[i] = t->powers[i];
	}
	return copy;
}

void delete_term(Term* t){
	if(t != NULL) free(t);
}

int compare_terms(Term* t1, Term* t2){
    for(int i = 0; i < 26; i++){
        if(t1->powers[i] > t2->powers[i]){
            return 1; // t1 > t2
        }
        else if(t1->powers[i] < t2->powers[i]){
            return 2; // t2 > t1
        }
    }
    return 0; // Equal
}


Term* multiply_terms(Term* t1, Term* t2){
	Term* result = deep_copy_term(t1);
	result->coefficient = result->coefficient * t2->coefficient;
	result->coefficient_denominator = result->coefficient_denominator * t2->coefficient_denominator;
	int divisor = greatest_common_divisor(abs(result->coefficient), result->coefficient_denominator);

	result->coefficient = result->coefficient / divisor;
	result->coefficient_denominator = result->coefficient_denominator / divisor;
	for(int i = 0; i < 26; i++){
		result->powers[i] += t2->powers[i];
	}
	return result;
}

Polynomial* create_polynomial(){
	Polynomial* p = (Polynomial*)malloc(sizeof(Polynomial));
	p->head = NULL;
	return p;
}

Polynomial* create_polynomial_with_term(Term * t){
	Polynomial* p = (Polynomial*)malloc(sizeof(Polynomial));
	p->head = deep_copy_term(t);
	return p;
}

void delete_polynomial(Polynomial* p){
	if (p == NULL) return;
	Term* curr = NULL;
	Term* temp = p->head;
	while(temp){
		curr = temp;
		temp = temp->next;
		delete_term(curr);
	}
	free(p);
}

void add_term_to_polynomial(Polynomial* p, Term* t){
    if (t->coefficient == 0) return;

	if(p->head == NULL){
		p->head = deep_copy_term(t);
		return;
	}
	Term* temp = p->head;
	Term* prev = NULL;

	while(temp){
		int priority = compare_terms(temp, t);
		if(priority == 0){ // Equal
			int a = temp->coefficient;
			int b = temp->coefficient_denominator;
			int c = t->coefficient;
			int d = t->coefficient_denominator;

			int new_coeff = (a*d) + (c*b); 
			int new_denom = b * d;
            
            if (new_coeff == 0) {
                if (prev == NULL) p->head = temp->next;
                else prev->next = temp->next;
                delete_term(temp);
            } else {
                int divisor = greatest_common_divisor(abs(new_coeff), new_denom);
                temp->coefficient = new_coeff / divisor;
                temp->coefficient_denominator = new_denom / divisor;
            }
			return;
		}
		else if(priority == 2){ // t2 > temp
            Term* newT = deep_copy_term(t);
            newT->next = temp;
            if (prev == NULL) p->head = newT;
            else prev->next = newT;
			return;
		}
        prev = temp;
		temp = temp->next;
	}
	prev->next = deep_copy_term(t);
}

Polynomial* add_polynomial_to_polynomial(Polynomial* p1, Polynomial* p2){
	if (p2 == NULL) return p1;
	Term* temp = p2->head;
	while(temp){
		add_term_to_polynomial(p1,temp);
		temp = temp->next;
	}
	return p1;
}

Polynomial* make_polynomial_negative(Polynomial* p){
	if(p == NULL || p->head == NULL){
		return p;
	}
	Term* temp = p->head;
	while(temp){
		temp->coefficient = temp->coefficient * (-1);
		temp= temp->next;
	}
	return p;
}

Polynomial* multiply_polynomials(Polynomial* p1, Polynomial* p2){
	Polynomial* result = create_polynomial();
    if(p1 == NULL || p2 == NULL || p1->head == NULL || p2->head == NULL){
		return result;
	}
	Term* temp_p1 = p1->head;
	while(temp_p1){
		Term* temp_p2 = p2->head;
		while(temp_p2){
			Term* multiplication = multiply_terms(temp_p1,temp_p2);
			add_term_to_polynomial(result, multiplication);
			delete_term(multiplication);
			temp_p2 = temp_p2->next;
		}
		temp_p1 = temp_p1->next;
	}
	return result;
}

void create_function(char f, Polynomial* p){
	Function func;
	func.fname = f;
	func.body = p;
	int count = current_param_count;
	current_param_count = 0;
	for(int i = 0; i < 26; i++){
		func.parameters[i] = current_params[i];
		current_params[i] = 0;
	}
	func.parameter_count = count;
	if(FunctionList[f-'a'].fname != 0){
		char tempMsg[150];
		sprintf(tempMsg, "%d_REDEFINED_FUNCTION_(%c)", yylineno, f);
		add_error(tempMsg);
		delete_polynomial(p);
		return;
	}
	FunctionList[f - 'a'] = func;
	return;
}

Function* find_function(char f){
	int index = f-'a';
	if(index >= 0 && index<26 && FunctionList[index].fname == f){
		return &FunctionList[index];	
	}
	return NULL;
}

int check_parameter_defined(char x) {
    for(int i = 0; i < current_param_count; i++) {
        if(current_params[i] == x) return 1;
    }
    return 0;
}

int check_parameter(Function* f, char x){
	for(int i = 0; i < f->parameter_count; i++){
		if(f->parameters[i] == x){
			return 1;
		}
	}
	return 0;
}

Polynomial* integral(Function* f, char x, int count){
	int index = x - 'a';
	Polynomial* result = create_polynomial();
	add_polynomial_to_polynomial(result, f->body);

	for(int i = 0; i < count; i++){
		Term* temp = result->head;
		while(temp){
			int old_value = temp->powers[index];
			temp->powers[index] += 1;
			temp->coefficient_denominator *= (old_value+1);
			int divisor = greatest_common_divisor(abs(temp->coefficient), temp->coefficient_denominator);
			temp->coefficient = temp->coefficient / divisor;
			temp->coefficient_denominator = temp->coefficient_denominator / divisor;
			temp = temp->next;
		}
	}
	return result;
}

Polynomial* derive(Function* f, char x, int count){
	int index = x - 'a';
	Polynomial* result = create_polynomial();
	add_polynomial_to_polynomial(result, f->body);

	for(int i = 0; i < count; i++){
		Term* temp = result->head;
		while(temp){
			int old_value = temp->powers[index];
			if(old_value == 0){
				temp->coefficient = 0;				
			}
			else{
				temp->powers[index] -= 1;
				temp->coefficient *= (old_value);
				int divisor = greatest_common_divisor(abs(temp->coefficient), temp->coefficient_denominator);
				temp->coefficient = temp->coefficient / divisor;
				temp->coefficient_denominator = temp->coefficient_denominator / divisor;
			}
			temp = temp->next;
		}
        Term* cleaner = result->head;
        result->head = NULL;
        while(cleaner) {
             Term* next = cleaner->next;
             if(cleaner->coefficient != 0) {
                 cleaner->next = NULL;
                 add_term_to_polynomial(result, cleaner);
                 free(cleaner); 
             } else {
                 free(cleaner);
             }
             cleaner = next;
        }
	}
	return result;
}

Polynomial* evaluate_function(Function* f, Polynomial** args, int count) {
    Polynomial* final_result = create_polynomial();
    Term* t = f->body->head;
    while(t) {
        Polynomial* term_val = create_polynomial();
        Term* base_coeff = create_term(t->coefficient, t->coefficient_denominator);
        term_val->head = base_coeff;
        
        for(int i=0; i < f->parameter_count; i++) {
            char param_char = f->parameters[i];
            int power = t->powers[param_char - 'a'];
            
            if (power > 0) {
                Polynomial* p_arg = args[i]; 
                Polynomial* p_pow = create_polynomial();
                
                Term* one = create_term(1, 1);
                p_pow->head = one;
                
                for(int k=0; k<power; k++) {
                     Polynomial* next = multiply_polynomials(p_pow, p_arg);
                     delete_polynomial(p_pow);
                     p_pow = next;
                }
                
                Polynomial* next_term_val = multiply_polynomials(term_val, p_pow);
                delete_polynomial(term_val);
                delete_polynomial(p_pow);
                term_val = next_term_val;
            }
        }
        add_polynomial_to_polynomial(final_result, term_val);
        delete_polynomial(term_val);
        t = t->next;
    }
    return final_result;
}

void print_polynomial_string(char* buffer, Polynomial* p) {
    if (p == NULL || p->head == NULL) {
        sprintf(buffer, "0");
        return;
    }
    buffer[0] = '\0';
    Term* t = p->head;
    int first = 1;
    
    while(t) {
        if (t->coefficient == 0) { t = t->next; continue; }
        
        if (t->coefficient > 0) {
            if (!first) strcat(buffer, "+");
        } else {
            strcat(buffer, "-");
        }
        
        int abs_coeff = abs(t->coefficient);
        int denom = t->coefficient_denominator;
        
        int is_const = 1;
        for(int i=0; i<26; i++) if(t->powers[i] != 0) is_const = 0;
        
        if (abs_coeff != 1 || denom != 1 || is_const) {
             char num[50];
             if (denom == 1) sprintf(num, "%d", abs_coeff);
             else sprintf(num, "%d/%d", abs_coeff, denom);
             strcat(buffer, num);
        }
        
        for(int i=0; i<26; i++) {
            if (t->powers[i] > 0) {
                char var[20];
                if (t->powers[i] == 1) sprintf(var, "%c", i+'a');
                else sprintf(var, "%c^%d", i+'a', t->powers[i]);
                strcat(buffer, var);
            }
        }
        
        t = t->next;
        first = 0;
    }
    if (first) sprintf(buffer, "0");
}


ArgNode* create_arg_node(Polynomial* p) {
    ArgNode* node = (ArgNode*)malloc(sizeof(ArgNode));
    node->poly = p;
    node->next = NULL;
    return node;
}

ArgNode* append_arg_node(ArgNode* list, Polynomial* p) {
    ArgNode* temp = list;
    while(temp->next != NULL) temp = temp->next;
    temp->next = create_arg_node(p);
    return list;
}

int main()
{
	if (yyparse()) {
		printf("ERROR\n");
		return 1;
	}
	else {
        print_results();
		return 0;
	}
}