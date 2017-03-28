/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "symtable.h"

IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
}
void IntConstant::PrintChildren(int indentLevel) { 
    printf("%d", value);
}


llvm::Value* IntConstant::Emit() {
    llvm::Value* val = llvm::ConstantInt::get(irgen->GetIntType(),value);

    return val;
}





FloatConstant::FloatConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
}
void FloatConstant::PrintChildren(int indentLevel) { 
    printf("%g", value);
}

llvm::Value* FloatConstant::Emit()  {
    llvm::Value* val = llvm::ConstantFP::get(irgen->GetFloatType(),value);
    return val;
}





BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
}
void BoolConstant::PrintChildren(int indentLevel) { 
    printf("%s", value ? "true" : "false");
}

llvm::Value* BoolConstant::Emit()  {
    llvm::Value* val = llvm::ConstantInt::get(irgen->GetBoolType(),value);
    return val;
}





VarExpr::VarExpr(yyltype loc, Identifier *ident) : Expr(loc) {
    Assert(ident != NULL);
    this->id = ident;
}

void VarExpr::PrintChildren(int indentLevel) {
    id->Print(indentLevel+1);
}



llvm::Value* VarExpr::Emit() {
    Symbol *sym;
    llvm::Value* val;
    llvm::BasicBlock* blk = irgen -> GetBasicBlock();

    sym = symtab -> findall(id->GetName());
    if(sym == NULL) 
        return NULL;
     

    val = new llvm::LoadInst(sym->value,id->GetName(),blk);
    return val;
    
}





Operator::Operator(yyltype loc, const char *tok) : Node(loc) {
    Assert(tok != NULL);
    strncpy(tokenString, tok, sizeof(tokenString));
}

void Operator::PrintChildren(int indentLevel) {
    printf("%s",tokenString);
}

bool Operator::IsOp(const char *op) const {
    return strcmp(tokenString, op) == 0;
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r) 
  : Expr(Join(l->GetLocation(), r->GetLocation())) {
    Assert(l != NULL && o != NULL && r != NULL);
    (op=o)->SetParent(this);
    (left=l)->SetParent(this); 
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Operator *o, Expr *r) 
  : Expr(Join(o->GetLocation(), r->GetLocation())) {
    Assert(o != NULL && r != NULL);
    left = NULL; 
    (op=o)->SetParent(this);
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o) 
  : Expr(Join(l->GetLocation(), o->GetLocation())) {
    Assert(l != NULL && o != NULL);
    (left=l)->SetParent(this);
    (op=o)->SetParent(this);
}

void CompoundExpr::PrintChildren(int indentLevel) {
   if (left) left->Print(indentLevel+1);
   op->Print(indentLevel+1);
   if (right) right->Print(indentLevel+1);
}
   


llvm::Value* ArithmeticExpr::Emit() {
    llvm::Value* rhs = right -> Emit();
    llvm::LoadInst* rhsLoc = llvm::cast<llvm::LoadInst>(rhs);

    llvm::Value* lhs = NULL;

    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    FieldAccess* faL = dynamic_cast<FieldAccess*>(left);
    FieldAccess* faR = dynamic_cast<FieldAccess*>(right);

    //Unary Operations (++ , --)
    if(left == NULL && right != NULL)  {
        if(rhs->getType() == irgen->GetIntType()) {
            llvm::Value *inc = llvm::ConstantInt::get(irgen->GetIntType(),1);


            if(op -> IsOp("++")) {                
                llvm::Value* sum = llvm::BinaryOperator::CreateAdd(rhs,inc,"",currBlk);

                new llvm::StoreInst(sum,rhsLoc->getPointerOperand(),currBlk);
                return sum;
            }
            else if(op->IsOp("--")){
                llvm::Value* dif = llvm::BinaryOperator::CreateSub(rhs,inc,"",currBlk);

                new llvm::StoreInst(dif,rhsLoc->getPointerOperand(),currBlk);
                return dif;
            }
            else if(op->IsOp("+"))  {
                llvm::Value* pos = llvm::BinaryOperator::CreateMul(rhs,inc,"",currBlk);

                new llvm::StoreInst(pos,rhsLoc->getPointerOperand(),currBlk);
                return pos;
            }
            else if(op->IsOp("-"))  {
                llvm::Value* zero = llvm::ConstantInt::get(irgen->GetIntType(),0);
                llvm::Value* neg = llvm::BinaryOperator::CreateSub(zero,rhs,"",currBlk);


                new llvm::StoreInst(neg,rhsLoc->getPointerOperand(),currBlk);
                return neg;
            }
        }
        else if (faR == NULL) { 
            llvm::Value* fInc = llvm::ConstantFP::get(irgen->GetFloatType(),1.0);
            

            if(op->IsOp("++"))  {
                 llvm::Value* fSum = llvm::BinaryOperator::CreateFAdd(rhs,fInc,"",currBlk);
        
                new llvm::StoreInst(fSum,rhsLoc->getPointerOperand(),currBlk);
                return  fSum;
            }
            else if(op->IsOp("--"))  {
                llvm::Value* fDiff = llvm::BinaryOperator::CreateFSub(rhs,fInc,"",currBlk);

                new llvm::StoreInst(fDiff,rhsLoc->getPointerOperand(),currBlk);
                return fDiff;
            }
            else if(op->IsOp("+"))  {
                llvm::Value* Fpos = llvm::BinaryOperator::CreateFMul(rhs,fInc,"",currBlk);

                new llvm::StoreInst(Fpos,rhsLoc->getPointerOperand(),currBlk);
                return Fpos;
            }
            else if(op->IsOp("-"))  {
                llvm::Value* zero = llvm::ConstantFP::get(irgen->GetFloatType(),0.0);
                llvm::Value* Fneg = llvm::BinaryOperator::CreateFSub(zero,rhs,"",currBlk);

                new llvm::StoreInst(Fneg,rhsLoc->getPointerOperand(),currBlk);
                return Fneg;
            }

        }
        else  {
            return NULL;  
        }

    }


    //Binary Operations
    if(left != NULL && right != NULL)  {
        lhs = left -> Emit();
        llvm::LoadInst* lhsLoc = llvm::cast<llvm::LoadInst>(lhs);



        // INT BINARY OPERATIONS
        if(lhs->getType() == irgen->GetIntType() && rhs->getType() == irgen->GetIntType()) {
            if(op->IsOp("+")) {
                llvm::Value* sum  = llvm::BinaryOperator::CreateAdd(lhs,rhs,"",currBlk);

                return sum;
            }
            else if(op->IsOp("-"))  {
                llvm::Value* dif = llvm::BinaryOperator::CreateSub(lhs,rhs,"",currBlk);

                return dif;
            }
            else if(op->IsOp("*")) {
                llvm::Value* prod = llvm::BinaryOperator::CreateMul(lhs,rhs,"",currBlk);

                return prod;
            }
            else if(op->IsOp("/"))  {
                llvm::Value* quot = llvm::BinaryOperator::CreateSDiv(lhs,rhs,"",currBlk);

                return quot;
            }
            else // Error(which shouldn't happen)
                return NULL;

        } 
        // FLOAT FLOAT BINARY OPERATIONS
        else if(lhs->getType() == rhs->getType() && 
                (lhs->getType() == irgen->GetFloatType() ||
                 lhs->getType() == irgen->GetVec2Type()  ||
                 lhs->getType() == irgen->GetVec3Type()  || 
                 lhs->getType() == irgen->GetVec4Type()  
                )
               )  {
            if(op->IsOp("+"))  {
                llvm::Value* Fsum = llvm::BinaryOperator::CreateFAdd(lhs,rhs,"",currBlk);

                return Fsum;
            }
            else if(op->IsOp("-"))  {
                llvm::Value* Fdif = llvm::BinaryOperator::CreateFSub(lhs,rhs,"",currBlk);

                return Fdif;
            }
            else if(op->IsOp("*"))  {
                llvm::Value* Fmul = llvm::BinaryOperator::CreateFMul(lhs,rhs,"",currBlk);

                return Fmul;
            }
            else if(op->IsOp("/"))  {
                llvm::Value* Fdiv = llvm::BinaryOperator::CreateFDiv(lhs,rhs,"",currBlk);

                return Fdiv;
            }
            else
                return NULL;
        }
        //FLOAT VEC / VEC FLOAT BINARY OPERATIONS
        else  {
            llvm::Value* vec;
            llvm::Value* idx;
            int vecType;

             // CREATE VECTOR FILLED WITH FLOATS
            if(lhs->getType() == irgen->GetFloatType())  {
                vec = llvm::UndefValue::get(rhs->getType());

                if(rhs->getType() == irgen->GetVec2Type())
                    vecType = 2;
                else if(rhs->getType() == irgen->GetVec3Type())
                    vecType = 3;
                else
                    vecType = 4;
            
                for(int i = 0; i < vecType; i++) {
                    idx = llvm::ConstantInt::get(irgen->GetIntType(),i);

                    vec = llvm::InsertElementInst::Create(vec,lhs,idx,"",currBlk);
                }
                lhs = vec;       
            }
            else  {
                vec = llvm::UndefValue::get(lhs->getType());

                if(lhs->getType() == irgen->GetVec2Type())
                    vecType = 2;
                else if(lhs->getType() == irgen->GetVec3Type())
                    vecType = 3;
                else
                    vecType = 4;

                for(int i = 0; i < vecType; i++)  {
                    idx = llvm::ConstantInt::get(irgen->GetIntType(),i);
   
                    vec = llvm::InsertElementInst::Create(vec,rhs,idx,"",currBlk);
                }
                rhs = vec;
            }


            if(op->IsOp("+"))  {
                llvm::Value* Fsum = llvm::BinaryOperator::CreateFAdd(lhs,rhs,"",currBlk);

                return Fsum;
            }
            else if(op->IsOp("-"))  {
                llvm::Value* Fdif = llvm::BinaryOperator::CreateFSub(lhs,rhs,"",currBlk);

                return Fdif;
            }
            else if(op->IsOp("*"))  {
                llvm::Value* Fmul = llvm::BinaryOperator::CreateFMul(lhs,rhs,"",currBlk);

                return Fmul;
            }
            else if(op->IsOp("/"))  {
                llvm::Value* Fdiv = llvm::BinaryOperator::CreateFDiv(lhs,rhs,"",currBlk);

                return Fdiv;
            }
            else
                return NULL;
            
        }
    }

    return NULL;
}








llvm::Value* RelationalExpr::Emit() {
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::Value* lhs = left -> Emit();
    llvm::Value* rhs = right -> Emit();
    llvm::Value* res = NULL;
    llvm::CmpInst::Predicate pred = llvm::CmpInst::FCMP_FALSE;

    // INT INT Comparisons
    if(lhs->getType() == irgen->GetIntType() && rhs->getType() == irgen->GetIntType()) {
        if(op->IsOp(">")) 
            pred = llvm::CmpInst::ICMP_SGT;
        else if(op->IsOp(">="))
            pred = llvm::CmpInst::ICMP_SGE;
        else if(op->IsOp("<"))
            pred = llvm::CmpInst::ICMP_SLT;
        else if(op->IsOp("<="))
            pred = llvm::CmpInst::ICMP_SLE;
        else //Should never reach here
            return NULL;
        
        res = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred,lhs,rhs,"",currBlk);
    }
    //FLOAT FLOAT COMPARISONs
    else  { //ASSUMING THAT BOTH LHS AND RHS WILL BE FLOAT
        if(op->IsOp(">"))
            pred = llvm::CmpInst::FCMP_OGT;
        else if(op->IsOp(">="))
            pred = llvm::CmpInst::FCMP_OGE;
        else if(op->IsOp("<"))
            pred = llvm::CmpInst::FCMP_OLT;
        else if(op->IsOp("<="))
            pred = llvm::CmpInst::FCMP_OLE;
        else NULL;

        res = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred,lhs,rhs,"",currBlk);

    }

    return res;
}


                                   



llvm::Value* EqualityExpr::Emit()  {
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::Value* lhs = left->Emit();
    llvm::Value* rhs = right->Emit();
    llvm::Value* res = NULL;
                                     
    llvm::CmpInst::Predicate pred = llvm::CmpInst::FCMP_FALSE;

    //INT INT EQUALITY COMPARISONS
    if(lhs->getType() == irgen->GetIntType() && rhs->getType() == irgen->GetIntType())  {
        if(op->IsOp("=="))
            pred = llvm::CmpInst::ICMP_EQ;
        else if(op->IsOp("!="))
            pred = llvm::CmpInst::ICMP_NE;
        else
            return NULL;

        res = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred,lhs,rhs,"",currBlk);
    }
    //FLOAT FLOAT EQUALITY COMPARISON
    else if(lhs->getType() == irgen->GetFloatType() && rhs->getType() == irgen->GetFloatType())  {
        if(op->IsOp("=="))
            pred = llvm::CmpInst::FCMP_OEQ;
        else if(op->IsOp("!="))
            pred = llvm::CmpInst::FCMP_ONE;
        else
            return NULL;

        res = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred,lhs,rhs,"",currBlk);
    }


    return res;
}





llvm::Value* LogicalExpr::Emit() {
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::Value* lhs = left -> Emit();
    llvm::Value* rhs = right -> Emit();
    llvm::Value* res = NULL;

    if(op->IsOp("&&"))  
        res = llvm::BinaryOperator::CreateAnd(lhs,rhs,"",currBlk);
    else  // Assuming that it will be ||
        res = llvm::BinaryOperator::CreateOr(lhs,rhs,"",currBlk);
    
    return res;
}




llvm::Value* AssignExpr::Emit()  {
    Symbol* Rsym = NULL;

    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::Value* rhs = right -> Emit();


    FieldAccess* faR = dynamic_cast<FieldAccess*>(right);
    FieldAccess* faL = dynamic_cast<FieldAccess*>(left);
    char* rSwizz = NULL;
    int swizLen = 0; 
    
    if(faR){
        rSwizz = faR -> GetField() -> GetName();
        int swizLen = strlen(rSwizz);
    }



    //Left Side is a regular variable
    //Assuming that type is the same.
    if(faL == NULL)  {
        llvm::Value* lhs = left -> Emit();

        llvm::LoadInst* lhsInst = llvm::cast<llvm::LoadInst>(lhs);
        llvm::Value* lhsLoc = lhsInst -> getPointerOperand();


        // Right side is a regular variable
        if(op->IsOp("="))  {
           new llvm::StoreInst(rhs,lhsLoc,currBlk);
        }
        else if(op->IsOp("+="))  {
            llvm::Value* sum;

            if(lhs->getType() == irgen->GetIntType())
                sum = llvm::BinaryOperator::CreateAdd(lhs,rhs,"",currBlk);
            else
                sum = llvm::BinaryOperator::CreateFAdd(lhs,rhs,"",currBlk);

            new llvm::StoreInst(sum,lhsLoc,currBlk);
        }
        else if(op->IsOp("-="))  {
            llvm::Value* dif;
                
            if(lhs->getType() == irgen->GetIntType())
                dif = llvm::BinaryOperator::CreateSub(lhs,rhs,"",currBlk);
            else
                dif = llvm::BinaryOperator::CreateFSub(lhs,rhs,"",currBlk);

            new llvm::StoreInst(dif,lhsLoc,currBlk);
        }
        else if(op->IsOp("*="))  {
            llvm::Value* prod;
        
            if(lhs->getType() == irgen->GetIntType())
                prod = llvm::BinaryOperator::CreateFMul(lhs,rhs,"",currBlk);
            else
                prod = llvm::BinaryOperator::CreateFMul(lhs,rhs,"",currBlk);

            new llvm::StoreInst(prod,lhsLoc,currBlk);
        }
        else if(op->IsOp("/="))  {
            llvm::Value* quot;
 
            if(lhs->getType() == irgen->GetIntType())
                quot = llvm::BinaryOperator::CreateSDiv(lhs,rhs,"",currBlk);
            else
                quot = llvm::BinaryOperator::CreateFDiv(lhs,rhs,"",currBlk);

            new llvm::StoreInst(quot,lhsLoc,currBlk);
        }
    }

    // LEFT IS A Field Access
    else {
        Symbol* sym;
        llvm::Value* lhsLoc;
        llvm::Value* value;
        llvm::Constant* idx;

        char* lSwizz = faL -> GetField() -> GetName();
        int swLength = strlen(lSwizz);

        VarExpr* baseAddr = dynamic_cast<VarExpr*>(faL->GetBase());
        sym = symtab->findall(baseAddr->GetIdentifier()->GetName());

        //Right side is a variable
        if(faR == NULL) {
                        
            if(op->IsOp("="))  {
                
                if(rhs->getType() == irgen->GetFloatType())  {                
                    if(lSwizz[0] == 'x')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(lSwizz[0] == 'y')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(lSwizz[0] == 'z')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                    else
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                    lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                    value = llvm::InsertElementInst::Create(lhsLoc,rhs,idx,"",currBlk);
                    new llvm::StoreInst(value,sym->value,"",currBlk);
                }
                else {
                    llvm::Value* rhsVal;
                    llvm::Constant* rhsIdx;
                    llvm::Value* leftAddr = sym->value;

                    for(int i = 0; i < swLength; i++)  {
 
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),i);
                        rhsVal = llvm::ExtractElementInst::Create(rhs,rhsIdx,"",currBlk);

                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                        lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,rhsVal,idx,"",currBlk);
                        new llvm::StoreInst(value,leftAddr,"",currBlk);
                    }
                }
            } // "="
            else if(op->IsOp("+=")) {

                llvm::Value* sum;
                llvm::Value* leftVal;
                
                if (rhs->getType() == irgen->GetFloatType())  {
       
                    for(int i = 0; i < swLength; i++) {
                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);
 
                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);

                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);                    
                        sum = llvm::BinaryOperator::CreateFAdd(leftVal,rhs,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,sum,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } // FA += float
                else {
                    llvm::Value* rhsVal;
                    llvm::Value* rhsIdx;
              
                    for(int i = 0; i < swLength; i++)  {
 
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),i);
                        rhsVal = llvm::ExtractElementInst::Create(rhs,rhsIdx,"",currBlk);

                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        
                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                        sum = llvm::BinaryOperator::CreateFAdd(leftVal,rhsVal,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,sum,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } // FA += Vec
            }//else if "+="

            else if(op->IsOp("-="))  {

                llvm::Value* diff;
                llvm::Value* leftVal;
                if(rhs->getType() == irgen -> GetFloatType()) {

                    for(int i = 0; i < swLength; i++) {
                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);
 
                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);

                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);                    
                        diff = llvm::BinaryOperator::CreateFSub(leftVal,rhs,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,diff,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } // FA -= float
                else {
                    llvm::Value* rhsVal;
                    llvm::Value* rhsIdx;
              
                    for(int i = 0; i < swLength; i++)  {
 
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),i);
                        rhsVal = llvm::ExtractElementInst::Create(rhs,rhsIdx,"",currBlk);

                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        
                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                        diff = llvm::BinaryOperator::CreateFSub(leftVal,rhsVal,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,diff,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } //FA -= Vec
            } // else if "-="
            else if(op->IsOp("*="))  {

                llvm::Value* prod;
                llvm::Value* leftVal;

                if(rhs->getType() == irgen -> GetFloatType()) {

                    for(int i = 0; i < swLength; i++) {
                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);
 
                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);

                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);                    
                        prod = llvm::BinaryOperator::CreateFMul(leftVal,rhs,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,prod,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } // FA *= float
                else {

                    llvm::Value* rhsVal;
                    llvm::Value* rhsIdx;
              
                    for(int i = 0; i < swLength; i++)  {
 
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),i);
                        rhsVal = llvm::ExtractElementInst::Create(rhs,rhsIdx,"",currBlk);

                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        
                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                        prod = llvm::BinaryOperator::CreateFMul(leftVal,rhsVal,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,prod,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } //FA *= Vec
            } // else if "*="
             else if(op->IsOp("/="))  {

                llvm::Value* quot;
                llvm::Value* leftVal;

                if(rhs->getType() == irgen -> GetFloatType()) {

                    for(int i = 0; i < swLength; i++) {
                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);
 
                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);

                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);                    
                        quot = llvm::BinaryOperator::CreateFDiv(leftVal,rhs,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,quot,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } // FA /= float
                else {

                    llvm::Value* rhsVal;
                    llvm::Value* rhsIdx;
              
                    for(int i = 0; i < swLength; i++)  {
 
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),i);
                        rhsVal = llvm::ExtractElementInst::Create(rhs,rhsIdx,"",currBlk);

                        if(lSwizz[i] == 'x')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                        else if(lSwizz[i] == 'y')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                        else if(lSwizz[i] == 'z')
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                        else
                            idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        
                        leftVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                        quot = llvm::BinaryOperator::CreateFDiv(leftVal,rhsVal,"",currBlk);

                        lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                        value = llvm::InsertElementInst::Create(lhsLoc,quot,idx,"",currBlk);
                        new llvm::StoreInst(value,sym->value,"",currBlk);
                    }
                } //FA /= Vec
            } // else if "/="


        


                
        } // FaR == null

        // Right Side is a Field Access
        else  if (faR != NULL){
            
            VarExpr* rAddr = dynamic_cast<VarExpr*>(faR->GetBase());
            Rsym = symtab->findall(rAddr->GetIdentifier()->GetName());


            if(op->IsOp("=")) {
                                
                llvm::Value* rhsVal;
                llvm::Constant *rhsIdx;
                //llvm::Value* leftAddr = sym -> value;
                llvm::Value* rhsLoc = new llvm::LoadInst(Rsym->value,"",currBlk);

                

                for (int i = 0; i < swLength; i++)  {
                    if(rSwizz[i] == 'x')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(rSwizz[i] == 'y')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(rSwizz[i] == 'z')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),2);  
                    else
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),3);
 
                    rhsVal = llvm::ExtractElementInst::Create(rhsLoc,rhsIdx,"",currBlk);
                    
                    if(lSwizz[i] == 'x')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(lSwizz[i] == 'y')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(lSwizz[i] == 'z')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                    else
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                    lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                    value = llvm::InsertElementInst::Create(lhsLoc,rhsVal,idx,"",currBlk);
                    new llvm::StoreInst(value,sym->value,"",currBlk);
                }
              
            } // FA "=" FA

            else if (op->IsOp("+="))  {
                llvm::Value* rhsVal;
                llvm::Value* lhsVal;
                llvm::Constant *rhsIdx;
                llvm::Value* sum;
                llvm::Value* leftAddr = sym -> value;
                llvm::Value* rhsLoc = new llvm::LoadInst(Rsym->value,"",currBlk);

                bool swEqual = false;
                int j;
             
                if(swLength == swizLen)
                    swEqual = true;

                for (int i = 0; i < swLength; i++)  {
 
                    if(swEqual == true)
                        j = i;
                    else
                        j = 0;

                    if(rSwizz[j] == 'x')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(rSwizz[j] == 'y')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(rSwizz[j] == 'z')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),2);  
                    else
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                     
                    rhsVal = llvm::ExtractElementInst::Create(rhsLoc,rhsIdx,"",currBlk);

                    if(lSwizz[i] == 'x')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(lSwizz[i] == 'y')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(lSwizz[i] == 'z')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                    else
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    lhsVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                    sum = llvm::BinaryOperator::CreateFAdd(lhsVal,rhsVal,"",currBlk);
                    

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    value = llvm::InsertElementInst::Create(lhsLoc,sum,idx,"",currBlk);
                    new llvm::StoreInst(value,leftAddr,"",currBlk);
                }
            } // FA += FA

            else if (op->IsOp("-="))  {
                llvm::Value* rhsVal;
                llvm::Value* lhsVal;
                llvm::Constant *rhsIdx;
                llvm::Value* diff;
                llvm::Value* leftAddr = sym -> value;
                llvm::Value* rhsLoc = new llvm::LoadInst(Rsym->value,"",currBlk);

                bool swEqual = false;
                int j;
             
                if(swLength == swizLen)
                    swEqual = true;

                for (int i = 0; i < swLength; i++)  {
 
                    if(swEqual == true)
                        j = i;
                    else
                        j = 0;

                    if(rSwizz[j] == 'x')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(rSwizz[j] == 'y')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(rSwizz[j] == 'z')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),2);  
                    else
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                     
                    rhsVal = llvm::ExtractElementInst::Create(rhsLoc,rhsIdx,"",currBlk);

                    if(lSwizz[i] == 'x')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(lSwizz[i] == 'y')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(lSwizz[i] == 'z')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                    else
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    lhsVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                    diff = llvm::BinaryOperator::CreateFSub(lhsVal,rhsVal,"",currBlk);
                    

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    value = llvm::InsertElementInst::Create(lhsLoc,diff,idx,"",currBlk);
                    new llvm::StoreInst(value,leftAddr,"",currBlk);
                }
            } // FA -= FA

            else if (op->IsOp("*="))  {
                llvm::Value* rhsVal;
                llvm::Value* lhsVal;
                llvm::Constant *rhsIdx;
                llvm::Value* prod;
                llvm::Value* leftAddr = sym -> value;
                llvm::Value* rhsLoc = new llvm::LoadInst(Rsym->value,"",currBlk);

                bool swEqual = false;
                int j;
             
                if(swLength == swizLen)
                    swEqual = true;

                for (int i = 0; i < swLength; i++)  {
 
                    if(swEqual == true)
                        j = i;
                    else
                        j = 0;

                    if(rSwizz[j] == 'x')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(rSwizz[j] == 'y')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(rSwizz[j] == 'z')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),2);  
                    else
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                     
                    rhsVal = llvm::ExtractElementInst::Create(rhsLoc,rhsIdx,"",currBlk);

                    if(lSwizz[i] == 'x')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(lSwizz[i] == 'y')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(lSwizz[i] == 'z')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                    else
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    lhsVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                    prod = llvm::BinaryOperator::CreateFMul(lhsVal,rhsVal,"",currBlk);
                    

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    value = llvm::InsertElementInst::Create(lhsLoc,prod,idx,"",currBlk);
                    new llvm::StoreInst(value,leftAddr,"",currBlk);
                }
            } // FA *= FA

            else if (op->IsOp("/="))  {
                llvm::Value* rhsVal;
                llvm::Value* lhsVal;
                llvm::Constant *rhsIdx;
                llvm::Value* quot;
                llvm::Value* leftAddr = sym -> value;
                llvm::Value* rhsLoc = new llvm::LoadInst(Rsym->value,"",currBlk);

                bool swEqual = false;
                int j;
             
                if(swLength == swizLen)
                    swEqual = true;

                for (int i = 0; i < swLength; i++)  {
 
                    if(swEqual == true)
                        j = i;
                    else
                        j = 0;

                    if(rSwizz[j] == 'x')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(rSwizz[j] == 'y')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(rSwizz[j] == 'z')
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),2);  
                    else
                        rhsIdx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                     
                    rhsVal = llvm::ExtractElementInst::Create(rhsLoc,rhsIdx,"",currBlk);

                    if(lSwizz[i] == 'x')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                    else if(lSwizz[i] == 'y')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                    else if(lSwizz[i] == 'z')
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                    else
                        idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    lhsVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);
                    quot = llvm::BinaryOperator::CreateFDiv(lhsVal,rhsVal,"",currBlk);
                    

                    lhsLoc = new llvm::LoadInst(leftAddr,"",currBlk);
                    value = llvm::InsertElementInst::Create(lhsLoc,quot,idx,"",currBlk);
                    new llvm::StoreInst(value,leftAddr,"",currBlk);
                }
            } // FA /= FA
        } // else 
    } // else FAL != null

    return rhs;
}




llvm::Value* PostfixExpr::Emit()  {
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::Value* lhs = left -> Emit();
    llvm::LoadInst *lhsInst = llvm::cast<llvm::LoadInst>(lhs);
    llvm::Value* lhsLoc = lhsInst->getPointerOperand();

    FieldAccess* faL = dynamic_cast<FieldAccess*>(left);

    llvm::Value* inc = llvm::ConstantInt::get(irgen->GetIntType(),1);
    llvm::Value* fInc = llvm::ConstantFP::get(irgen->GetFloatType(),1.0);

    //If Left is NOT A Field Access
    if(faL == NULL)  {

        if(op->IsOp("++"))  {

            if(lhs->getType() == irgen->GetIntType()) {
                llvm::Value* sum = llvm::BinaryOperator::CreateAdd(lhs,inc,"",currBlk);
 
                new llvm::StoreInst(sum,lhsLoc,"",currBlk);
               
            }
            else if(lhs->getType() == irgen->GetFloatType()) {
                llvm::Value* Fsum = llvm::BinaryOperator::CreateFAdd(lhs,fInc,"",currBlk);

                new llvm::StoreInst(Fsum,lhsLoc,"",currBlk);
            }
            else {
                int vecType = 0;

                llvm::Value* vec = llvm::UndefValue::get(lhs->getType());

                if(lhs->getType() == irgen->GetVec2Type())
                    vecType = 2;
                else if(lhs->getType() == irgen->GetVec3Type())
                    vecType = 3;
                else
                    vecType = 4;
            
                for(int i = 0; i < vecType; i++) {
                    llvm::Value* idx = llvm::ConstantInt::get(irgen->GetIntType(),i);

                    vec = llvm::InsertElementInst::Create(vec,fInc,idx,"",currBlk);
                    llvm::Value* Vsum = llvm::BinaryOperator::CreateFAdd(lhs,vec,"",currBlk);

                    new llvm::StoreInst(Vsum,lhsLoc,"",currBlk);
                }
            }
        }
        else if (op->IsOp("--"))  {
            
            if(lhs->getType() == irgen->GetIntType()) {
                llvm::Value* IDif = llvm::BinaryOperator::CreateSub(lhs,inc,"",currBlk);

                new llvm::StoreInst(IDif,lhsLoc,currBlk);
            }
            else if(lhs->getType() == irgen->GetFloatType())  { 
                llvm::Value* FDif = llvm::BinaryOperator::CreateFSub(lhs,fInc,"",currBlk);

                new llvm::StoreInst(FDif,lhsLoc,"",currBlk);
            }
            else {
                int vecType = 0;

                llvm::Value* vec = llvm::UndefValue::get(lhs->getType());

                if(lhs->getType() == irgen->GetVec2Type())
                    vecType = 2;
                else if(lhs->getType() == irgen->GetVec3Type())
                    vecType = 3;
                else
                    vecType = 4;
            
                for(int i = 0; i < vecType; i++) {
                    llvm::Value* idx = llvm::ConstantInt::get(irgen->GetIntType(),i);

                    vec = llvm::InsertElementInst::Create(vec,fInc,idx,"",currBlk);
                    llvm::Value* Vsum = llvm::BinaryOperator::CreateSub(lhs,vec,"",currBlk);

                    new llvm::StoreInst(Vsum,lhsLoc,"",currBlk);
                }
            }

        }
    }
    // Left is a Field Access
    else  {
        Symbol* sym;
        llvm::Value* val;
        llvm::Value* idx;

        char* swizzle = faL -> GetField() -> GetName();
        VarExpr* baseAddr = dynamic_cast<VarExpr*>(faL->GetBase());
        sym = symtab->findall(baseAddr->GetIdentifier()->GetName());

        for(int i = 0; i < strlen(swizzle); i++)  {
            if(swizzle[i] == 'x')
                    idx = llvm::ConstantInt::get(irgen->GetIntType(),0);
                else if(swizzle[i] == 'y')
                    idx = llvm::ConstantInt::get(irgen->GetIntType(),1);
                else if(swizzle[i] == 'z')
                    idx = llvm::ConstantInt::get(irgen->GetIntType(),2);
                else
                    idx = llvm::ConstantInt::get(irgen->GetIntType(),3);

                lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                llvm::Value* lhsVal = llvm::ExtractElementInst::Create(lhsLoc,idx,"",currBlk);

                if(op->IsOp("++"))
                    val = llvm::BinaryOperator::CreateFAdd(lhsVal,fInc,"",currBlk);
                else
                    val = llvm::BinaryOperator::CreateFSub(lhsVal,fInc,"",currBlk);

               
                lhsLoc = new llvm::LoadInst(sym->value,"",currBlk);
                llvm::Value* finVal = llvm::InsertElementInst::Create(lhsLoc,val,idx,"",currBlk);
                new llvm::StoreInst(finVal,sym->value,"",currBlk);
        }


    }

    return lhs;
}








ConditionalExpr::ConditionalExpr(Expr *c, Expr *t, Expr *f)
  : Expr(Join(c->GetLocation(), f->GetLocation())) {
    Assert(c != NULL && t != NULL && f != NULL);
    (cond=c)->SetParent(this);
    (trueExpr=t)->SetParent(this);
    (falseExpr=f)->SetParent(this);
}

void ConditionalExpr::PrintChildren(int indentLevel) {
    cond->Print(indentLevel+1, "(cond) ");
    trueExpr->Print(indentLevel+1, "(true) ");
    falseExpr->Print(indentLevel+1, "(false) ");
}
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}

void ArrayAccess::PrintChildren(int indentLevel) {
    base->Print(indentLevel+1);
    subscript->Print(indentLevel+1, "(subscript) ");
}
     





llvm::Value* ArrayAccess::Emit() {
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
   
    llvm::Value* idx = subscript -> Emit();
    llvm::Value* baseAddr = base -> Emit();
    
    VarExpr* baseVar = dynamic_cast<VarExpr*> (base);
    Symbol* sym = symtab->findall(baseVar -> GetIdentifier() -> GetName());


    vector<llvm::Value*> val;
    val.push_back(llvm::ConstantInt::get(irgen->GetIntType(),0));
    val.push_back(idx);
    
//    llvm::Value* baseLoc = new llvm::LoadInst(sym->value,"",currBlk);


    llvm::Value* retVal = llvm::GetElementPtrInst::Create(sym->value,val,"",currBlk);
    llvm::Value* ret =  new llvm::LoadInst(retVal,"",currBlk);
    return ret;
} 









FieldAccess::FieldAccess(Expr *b, Identifier *f) 
  : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b; 
    if (base) base->SetParent(this); 
    (field=f)->SetParent(this);
}


void FieldAccess::PrintChildren(int indentLevel) {
    if (base) base->Print(indentLevel+1);
    field->Print(indentLevel+1);
}






llvm::Value* FieldAccess::Emit() {
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::Value* baseAddr = base -> Emit();
    llvm::Value* returnVal = NULL;

    int swizzleLen = strlen(field->GetName());
    string swizzle = field -> GetName();

    vector<llvm::Constant*> idxMask;

    //Get the Vector of indicies
    for(int i = 0; i < swizzleLen; i++) {
        if(swizzle[i] == 'x')
            idxMask.push_back(llvm::ConstantInt::get(irgen->GetIntType(),0));
        else if(swizzle[i] == 'y')
            idxMask.push_back(llvm::ConstantInt::get(irgen->GetIntType(),1));
        else if(swizzle[i] == 'z')
            idxMask.push_back(llvm::ConstantInt::get(irgen->GetIntType(),2));
        else if(swizzle[i] == 'w')
            idxMask.push_back(llvm::ConstantInt::get(irgen->GetIntType(),3));
    }
    
    //Handle if field length is 1
    if(swizzleLen == 1) {
        returnVal = llvm::ExtractElementInst::Create(baseAddr,idxMask[0],"",currBlk);
    }
    else { // Handle when field length is greater than 1.
        
        llvm::Value* mask = llvm::ConstantVector::get(idxMask);
        returnVal = new llvm::ShuffleVectorInst(baseAddr,baseAddr,mask,"",currBlk);

    }



    return returnVal;
}













Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}

void Call::PrintChildren(int indentLevel) {
   if (base) base->Print(indentLevel+1);
   if (field) field->Print(indentLevel+1);
   if (actuals) actuals->PrintAll(indentLevel+1, "(actuals) ");
}


llvm::Value* Call::Emit()  {
    llvm::BasicBlock* curBlk = irgen -> GetBasicBlock();
    vector<llvm::Value*> param;
    llvm::Value* retVal;
    Symbol* sym = symtab->findall(field -> GetName());

    llvm::Function* func = llvm::cast<llvm::Function>(sym->value);

    for(int i = 0; i < actuals->NumElements(); i++)  {
        llvm::Value* var = actuals->Nth(i)->Emit();
        param.push_back(var);

    }

    retVal = llvm::CallInst::Create(func,param,"",curBlk);
    
    return retVal;
}
