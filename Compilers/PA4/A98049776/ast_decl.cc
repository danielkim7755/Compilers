/* File: ast_decl.cc
 * -----------------
 * Implementation of Decl node classes.
 */
#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "symtable.h"        
         
Decl::Decl(Identifier *n) : Node(*n->GetLocation()) {
    Assert(n != NULL);
    (id=n)->SetParent(this); 
}

VarDecl::VarDecl(Identifier *n, Type *t, Expr *e) : Decl(n) {
    Assert(n != NULL && t != NULL);
    (type=t)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
    typeq = NULL;
}

VarDecl::VarDecl(Identifier *n, TypeQualifier *tq, Expr *e) : Decl(n) {
    Assert(n != NULL && tq != NULL);
    (typeq=tq)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
    type = NULL;
}

VarDecl::VarDecl(Identifier *n, Type *t, TypeQualifier *tq, Expr *e) : Decl(n) {
    Assert(n != NULL && t != NULL && tq != NULL);
    (type=t)->SetParent(this);
    (typeq=tq)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
}
  
void VarDecl::PrintChildren(int indentLevel) { 
   if (typeq) typeq->Print(indentLevel+1);
   if (type) type->Print(indentLevel+1);
   if (id) id->Print(indentLevel+1);
   if (assignTo) assignTo->Print(indentLevel+1, "(initializer) ");
}

llvm::Value* VarDecl::Emit() {

    Symbol* sym;
    llvm::Value* val = NULL;
    llvm::Twine* twine= new llvm::Twine(this->GetIdentifier()->GetName());
    llvm::Module* MOD = irgen -> GetOrCreateModule("");
 

    //Check if it was declared in the global variable.
    if (symtab->isGlobalScope())  {
        val = new llvm::GlobalVariable( *MOD,
                                        type -> GetllvmType(),
                                        false,
                                        llvm::GlobalValue::ExternalLinkage,
                                        llvm::Constant::getNullValue(type -> GetllvmType()),
                                        *twine
                                      );   
    }
    else   {
        //Insert the variable at the end of the current block.
        llvm::BasicBlock* blk = irgen->GetBasicBlock();
        val = new llvm::AllocaInst(type -> GetllvmType(),*twine, blk);
    }

    sym = new Symbol(GetIdentifier() -> GetName(),this, E_VarDecl, val);
    symtab -> insert(*sym);
    

    return val;

}







FnDecl::FnDecl(Identifier *n, Type *r, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r!= NULL && d != NULL);
    (returnType=r)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
    returnTypeq = NULL;
}

FnDecl::FnDecl(Identifier *n, Type *r, TypeQualifier *rq, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r != NULL && rq != NULL&& d != NULL);
    (returnType=r)->SetParent(this);
    (returnTypeq=rq)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
}

void FnDecl::SetFunctionBody(Stmt *b) { 
    (body=b)->SetParent(this);
}

void FnDecl::PrintChildren(int indentLevel) {
    if (returnType) returnType->Print(indentLevel+1, "(return type) ");
    if (id) id->Print(indentLevel+1);
    if (formals) formals->PrintAll(indentLevel+1, "(formals) ");
    if (body) body->Print(indentLevel+1, "(body) ");
}


llvm::Value* FnDecl::Emit()  {


    Symbol *sym;
    llvm::Module* mod = irgen -> GetOrCreateModule("test.bc");

    // Get llvmType of return type
    llvm::Type* retType = returnType -> GetllvmType();


    //Set up parameter list
    vector<llvm::Type*> param;
    
    for(int i = 0; i < formals -> NumElements(); i++) {
        
        llvm::Type* ty = formals->Nth(i)->GetType()->GetllvmType();
        
        param.push_back(ty);
    }
    llvm::ArrayRef<llvm::Type*>Args(param);

   
    //Get Function Type
    llvm::FunctionType *funcType = llvm::FunctionType::get(retType,Args,false);

    //Set and get Function from Module
    llvm::Function* func = llvm::cast<llvm::Function>(mod -> getOrInsertFunction(id->GetName(),funcType));
    irgen -> SetFunction(func);
   


    //Change Argument Names
    llvm::Function::arg_iterator argIt = func -> arg_begin();
    int i = 0;

    for(argIt; argIt != func->arg_end(); argIt++) {
        VarDecl *vdecl = formals -> Nth(i);

        argIt->setName(vdecl->GetIdentifier()->GetName());

        i++;
    }


    //Create Entry Block
    llvm::LLVMContext *con = irgen -> GetContext();
    llvm::BasicBlock *blk = llvm::BasicBlock::Create(*con,"entry",func);
    irgen -> SetBasicBlock(blk);

    
    symtab -> push();


    //Emit Formals onto the entry block
    argIt = func -> arg_begin();
    i = 0;
    Symbol* tempSym = NULL;


    for(argIt; argIt != func->arg_end(); argIt++) {
        llvm::Value* vVal = formals->Nth(i)->Emit();

        new llvm::StoreInst(argIt, vVal, irgen->GetBasicBlock());

        i++;
    }


/*
    llvm::BasicBlock* nextBlk = llvm::BasicBlock::Create(*con,"next",func);

    if(blk->getTerminator() == NULL)
        llvm::BranchInst::Create(nextBlk,irgen->GetBasicBlock());

    irgen->SetBasicBlock(nextBlk);
*/

    // Emit Body
    body -> Emit();

    symtab -> pop();
    sym = new Symbol(GetIdentifier()->GetName(),this,E_FunctionDecl,func);
    symtab -> insert(*sym);

    return func;
}
