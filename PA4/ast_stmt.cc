/* File: ast_stmt.cc
 * -----------------
 * Implementation of statement node classes.
 */
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "symtable.h"

#include "irgen.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/raw_ostream.h"                                                   


Program::Program(List<Decl*> *d) {
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}

void Program::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    printf("\n");
}

llvm::Value* Program::Emit() {
    // TODO:
    // This is just a reference for you to get started
    //
    // You can use this as a template and create Emit() function
    // for individual node to fill in the module structure and instructions.
    
    
    llvm::Module *mod = irgen->GetOrCreateModule("test.bc");


    for(int i = 0; i < decls->NumElements(); i++)  {
        Decl* decl = decls->Nth(i);
        decl -> Emit();
    }
    symtab -> pop();
    
    // write the BC into standard output 
    llvm::WriteBitcodeToFile(mod, llvm::outs());

    //uncomment the next line to generate the human readable/assembly file
    //mod->dump();
    //

    return NULL;
}

StmtBlock::StmtBlock(List<VarDecl*> *d, List<Stmt*> *s) {
    Assert(d != NULL && s != NULL);
    (decls=d)->SetParentAll(this);
    (stmts=s)->SetParentAll(this);
}

void StmtBlock::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    stmts->PrintAll(indentLevel+1);
}


llvm::Value* StmtBlock::Emit() {
    symtab -> push();
    Expr* expr = NULL;

    //Emit Each VarDecl
    for(int i = 0; i < decls->NumElements(); i++)  {
        VarDecl* vdecl = decls->Nth(i);
        vdecl -> Emit();
    }

    //Emit Each Stmt
    for(int i = 0; i < stmts->NumElements(); i++)  {

        Stmt* stmt = stmts->Nth(i);

        expr = dynamic_cast<Expr*>(stmt);
        if(expr == NULL)        
            stmt -> Emit();
        else
            expr -> Emit();

    }


    symtab -> pop();

    return NULL;
}





DeclStmt::DeclStmt(Decl *d) {
    Assert(d != NULL);
    (decl=d)->SetParent(this);
}

void DeclStmt::PrintChildren(int indentLevel) {
 
   decl->Print(indentLevel+1);
}


llvm::Value* DeclStmt::Emit()  {
    llvm::Value* val = decl -> Emit();
    return val;
}






ConditionalStmt::ConditionalStmt(Expr *t, Stmt *b) { 
    Assert(t != NULL && b != NULL);
    (test=t)->SetParent(this); 
    (body=b)->SetParent(this);
}

ForStmt::ForStmt(Expr *i, Expr *t, Expr *s, Stmt *b): LoopStmt(t, b) { 
    Assert(i != NULL && t != NULL && b != NULL);
    (init=i)->SetParent(this);
    step = s;
    if ( s )
      (step=s)->SetParent(this);
}

void ForStmt::PrintChildren(int indentLevel) {
    init->Print(indentLevel+1, "(init) ");
    test->Print(indentLevel+1, "(test) ");
    if ( step )
      step->Print(indentLevel+1, "(step) ");
    body->Print(indentLevel+1, "(body) ");
}


llvm::Value* ForStmt::Emit() {
    llvm::Function* func = irgen -> GetFunction();
    llvm::LLVMContext* con = irgen -> GetContext();
    llvm::BasicBlock *stepBlk = NULL;
    llvm::BasicBlock *bodyBlk = NULL;

    // Create Blocks
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::BasicBlock* footBlk = llvm::BasicBlock::Create(*con,"FORfooter",func);
    irgen -> footStack -> push(footBlk);
    
    if(step != NULL) {
        stepBlk = llvm::BasicBlock::Create(*con,"step",func);
      //  irgen -> footStack -> push(stepBlk);
    }

    if(body != NULL) {
        bodyBlk = llvm::BasicBlock::Create(*con,"FORbody",func);
     //   irgen -> footStack -> push(bodyBlk);
    }

    llvm::BasicBlock* headBlk = llvm::BasicBlock::Create(*con,"FORhead",func);
    // irgen -> footStack -> push(headBlk);


    //Emit Init
    if(init != NULL)
        init -> Emit();


    //Create Branch to Terminate Current Block
    llvm::BranchInst::Create(headBlk,currBlk);


    //IrGen for Head Block + Emit for Test
    irgen -> SetBasicBlock(headBlk);
    llvm::Value* val = test -> Emit();

    
    //Jump to Footer
    llvm::BranchInst::Create(bodyBlk,footBlk,val,headBlk);


    symtab->push();
    //Emit For Body
    irgen -> SetBasicBlock(bodyBlk);
    irgen -> brkStack -> push(footBlk);
    irgen -> contStack -> push(stepBlk);
    body -> Emit();


 
    // Check for Terminator Inst
    llvm::BasicBlock* checkBlk = irgen -> GetBasicBlock();
    if(bodyBlk -> getTerminator() == NULL)
        llvm::BranchInst::Create(stepBlk,bodyBlk);
    symtab -> pop();

   
    //Emit for Step
    irgen -> SetBasicBlock(stepBlk);
    step -> Emit();

    // Create Terminator for Step
    llvm::BranchInst::Create(headBlk,stepBlk);



   // Check for nested
    llvm::BasicBlock* stkBlk = irgen -> footStack -> top();
    if(footBlk != stkBlk)  {
        if(checkBlk->getTerminator() == NULL)
            llvm::BranchInst::Create(stepBlk,stkBlk);
    }



    //irgen -> footStack -> pop();
    irgen -> brkStack -> pop();
    irgen -> contStack -> pop();
    irgen -> SetBasicBlock(footBlk);

    return NULL;
}





void WhileStmt::PrintChildren(int indentLevel) {
    test->Print(indentLevel+1, "(test) ");
    body->Print(indentLevel+1, "(body) ");
}


llvm::Value* WhileStmt::Emit()  {
    llvm::LLVMContext* con = irgen->GetContext();
    llvm::Function* func = irgen->GetFunction();

    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::BasicBlock* headBlk = llvm::BasicBlock::Create(*con,"Whhead",func);
    llvm::BasicBlock* bodyBlk = llvm::BasicBlock::Create(*con,"Whbody",func);
    llvm::BasicBlock* footBlk = llvm::BasicBlock::Create(*con,"WHfooter",func);

    irgen -> footStack -> push(footBlk);
    irgen -> brkStack -> push(footBlk);
    irgen -> contStack -> push(headBlk);

   
    //Create Branch Instruction to header
    llvm::BranchInst::Create(headBlk,currBlk);

    //Emit Value into Header
    irgen -> SetBasicBlock(headBlk);
    llvm::Value* val = test -> Emit();
    

    //Create Branch Instruction to decide between body or footer
    llvm::BranchInst::Create(bodyBlk,footBlk,val,headBlk);

    
    // Emit Body Code in Body Block
    symtab -> push();

    irgen -> SetBasicBlock(bodyBlk);
    body -> Emit();

    if(bodyBlk -> getTerminator() == NULL)
        llvm::BranchInst::Create(headBlk,bodyBlk);
    

    // Check for nested
    llvm::BasicBlock* checkBlk = irgen -> footStack -> top();
    if(footBlk != checkBlk)  {
        if(checkBlk->getTerminator() == NULL)
            llvm::BranchInst::Create(headBlk,checkBlk);
        
    }

    symtab -> pop();
    
 //   irgen -> footStack -> pop();
    irgen -> brkStack -> pop();
    irgen -> contStack -> pop();
    irgen -> SetBasicBlock(footBlk);


    return NULL;
}






IfStmt::IfStmt(Expr *t, Stmt *tb, Stmt *eb): ConditionalStmt(t, tb) { 
    Assert(t != NULL && tb != NULL); // else can be NULL
    elseBody = eb;
    if (elseBody) elseBody->SetParent(this);
}

void IfStmt::PrintChildren(int indentLevel) {
    if (test) test->Print(indentLevel+1, "(test) ");
    if (body) body->Print(indentLevel+1, "(then) ");
    if (elseBody) elseBody->Print(indentLevel+1, "(else) ");
}




llvm::Value* IfStmt::Emit()  {    
    llvm::LLVMContext* con = irgen -> GetContext();
    llvm::Function* func = irgen -> GetFunction();


    // Emit Test
    llvm::Value* Val = test -> Emit();

   
    // Create Blocks
    llvm::BasicBlock* elseBlk = NULL;
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::BasicBlock* thenBlk = llvm::BasicBlock::Create(*con,"then",func);
    if(elseBody != NULL)
        elseBlk = llvm::BasicBlock::Create(*con,"else",func);
    llvm::BasicBlock* footBlk = llvm::BasicBlock::Create(*con,"IFfooter",func);
    
    // Push footer? 
    irgen -> footStack -> push(footBlk);
    // if(elseBlk)
    //     irgen -> footStack -> push(elseBlk);
    

    // Create Branch Inst
    llvm::BranchInst::Create(thenBlk,elseBody ? elseBlk:footBlk,Val,currBlk);

    symtab -> push();

    irgen -> SetBasicBlock(thenBlk);
    body -> Emit();
    llvm::BasicBlock* checkBlk = irgen -> GetBasicBlock();
    if(checkBlk->getTerminator() == NULL)
        llvm::BranchInst::Create(footBlk,checkBlk);

    symtab -> pop();

    // Emit ElseBody if needed
    if(elseBody != NULL)  {
        symtab -> push();

        irgen -> SetBasicBlock(elseBlk);
        elseBody -> Emit();
        checkBlk = irgen -> GetBasicBlock();
        if(checkBlk->getTerminator() == NULL)
            llvm::BranchInst::Create(footBlk,checkBlk);

        symtab -> pop();
    }

    llvm::BasicBlock* stkBlk = irgen->footStack->top();
    if(footBlk != stkBlk) {
        irgen->footStack->pop();
        if (stkBlk->getTerminator() == NULL)
            llvm::BranchInst::Create(footBlk,stkBlk);
    }


    irgen -> SetBasicBlock(footBlk);


    return NULL;
}












llvm::Value* BreakStmt::Emit()  {
    hasReturned = true;

    llvm::BasicBlock *blk = irgen -> GetBasicBlock();
    llvm::BasicBlock *target = irgen -> brkStack -> top();
    llvm::BranchInst::Create(target,blk);

    return NULL;
}






llvm::Value* ContinueStmt::Emit()  {
    llvm::BasicBlock *blk = irgen -> GetBasicBlock();
    llvm::BasicBlock *target = irgen -> contStack -> top();
    llvm::BranchInst::Create(target,blk);


    return NULL;
}







ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) { 
    expr = e;
    if (e != NULL) expr->SetParent(this);
}

void ReturnStmt::PrintChildren(int indentLevel) {
    if ( expr ) 
      expr->Print(indentLevel+1);
}


llvm::Value* ReturnStmt::Emit() {

    llvm::Value* val = NULL;
    if(expr != NULL){ 
        val = expr -> Emit();
     }
    
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::LLVMContext* con = irgen -> GetContext();


    if(val == NULL)
        llvm::ReturnInst::Create(*con,currBlk);
    else
        llvm::ReturnInst::Create(*con,val,currBlk);
        
    
    return NULL;
}





SwitchLabel::SwitchLabel(Expr *l, Stmt *s) {
    Assert(l != NULL && s != NULL);
    (label=l)->SetParent(this);
    (stmt=s)->SetParent(this);
}

SwitchLabel::SwitchLabel(Stmt *s) {
    Assert(s != NULL);
    label = NULL;
    (stmt=s)->SetParent(this);
}

void SwitchLabel::PrintChildren(int indentLevel) {
    if (label) label->Print(indentLevel+1);
    if (stmt)  stmt->Print(indentLevel+1);
}


llvm::Value* Case::Emit() {
    stmt -> Emit();

    return NULL;
}


llvm::Value* Default::Emit() {
    stmt -> Emit();

    return NULL;
}

SwitchStmt::SwitchStmt(Expr *e, List<Stmt *> *c, Default *d) {
    Assert(e != NULL && c != NULL && c->NumElements() != 0 );
    (expr=e)->SetParent(this);
    (cases=c)->SetParentAll(this);
    def = d;
    if (def) def->SetParent(this);
}

void SwitchStmt::PrintChildren(int indentLevel) {
    if (expr) expr->Print(indentLevel+1);
    if (cases) cases->PrintAll(indentLevel+1);
    if (def) def->Print(indentLevel+1);
}

llvm::Value* SwitchStmt::Emit() {
    llvm::Function* func = irgen -> GetFunction();
    llvm::LLVMContext *con = irgen -> GetContext();

    vector<llvm::BasicBlock*> caseBlockList;
    llvm::BasicBlock* currBlk = irgen -> GetBasicBlock();
    llvm::BasicBlock* defBlk = llvm::BasicBlock::Create(*con,"default",func);
    llvm::BasicBlock* footBlk = llvm::BasicBlock::Create(*con,"SWTfooter",func);

    irgen -> brkStack -> push(footBlk);

    for(int i = 0 ; i < cases->NumElements(); i++)  {
        if(dynamic_cast<Case*>(cases->Nth(i)))  {
            llvm::BasicBlock* caseBlk = llvm::BasicBlock::Create(*con,"case",func);
            caseBlockList.push_back(caseBlk);
        }
        else if (dynamic_cast<Default*>(cases->Nth(i))) {
            caseBlockList.push_back(defBlk);
        }
    }

    //push scope?
    symtab -> push();

    // Emit Expression
    llvm::Value* val = expr -> Emit();

  
    //Create Switch Instruction
    llvm::SwitchInst* swInst = llvm::SwitchInst::Create(val,footBlk,cases->NumElements(),currBlk);

    currBlk = irgen -> GetBasicBlock();    


    for (int i = 0; i < cases->NumElements(); i++)  {
        hasReturned = false;

        llvm::BasicBlock* blk = caseBlockList[i];

        symtab -> push();
          
        if(dynamic_cast<Case*>(cases->Nth(i))) {

            Case* ca = dynamic_cast<Case*>(cases->Nth(i));
            llvm::Value* label = ca->GetLabel()->Emit();

            swInst -> addCase(llvm::cast<llvm::ConstantInt>(label),blk);
            
            if(currBlk -> getTerminator() == NULL)
                llvm::BranchInst::Create(blk,currBlk);

            irgen->SetBasicBlock(blk);
            ca -> Emit();

            if(hasReturned == false) {
                if( i+1 < cases->NumElements()) {
                    llvm::BranchInst::Create(caseBlockList[i+1],blk);
                }
                else
                    llvm::BranchInst::Create(footBlk,blk);
            }

        }
        else if (dynamic_cast<Default*>(cases->Nth(i)))  {
            
            if(currBlk -> getTerminator() == NULL)
                llvm::BranchInst::Create(defBlk,currBlk);

            swInst -> setDefaultDest(defBlk);

            Default* defStmt = dynamic_cast<Default*>(cases->Nth(i));

            irgen->SetBasicBlock(blk);
            defStmt -> Emit();

        }

        symtab->pop();
    }


    if(defBlk -> getTerminator() == NULL)
        llvm::BranchInst::Create(footBlk,defBlk);

    irgen -> brkStack -> pop();
    symtab -> pop();

    irgen->SetBasicBlock(footBlk);

    return NULL;
}

