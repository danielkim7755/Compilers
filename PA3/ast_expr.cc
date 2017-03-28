/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "symtable.h"

void Expr::Check()  {}


/**** INT CONSTANT ***/
IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
}
void IntConstant::PrintChildren(int indentLevel) { 
    printf("%d", value);
}


/**** FLOAT CONSTANT ****/
FloatConstant::FloatConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
}
void FloatConstant::PrintChildren(int indentLevel) { 
    printf("%g", value);
}


/**** BOOL CONSTANT ****/
BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
}
void BoolConstant::PrintChildren(int indentLevel) { 
    printf("%s", value ? "true" : "false");
}



/**** VAR EXPR ****/
VarExpr::VarExpr(yyltype loc, Identifier *ident) : Expr(loc) {
    Assert(ident != NULL);
    this->id = ident;
}

void VarExpr::PrintChildren(int indentLevel) {
    id->Print(indentLevel+1);
}

Type* VarExpr::GetType() {
    Symbol* sym = symtab -> findall(id->GetName());
    
    if(sym == NULL)
        return Type::errorType;

    VarDecl* vDecl = dynamic_cast<VarDecl*>(sym->decl);

    if(vDecl == NULL)
        return Type::errorType;

    return vDecl->GetType();
}


void VarExpr::Check()  {
    Symbol* sym;
    sym = symtab -> findall(id->GetName());
    if(sym == NULL)
        ReportError::IdentifierNotDeclared(id,LookingForVariable);
}



/**** OPERATOR ****/
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



/**** COMPOUND EXPR ****/
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
   


/**** ARITHMETIC EXPR ****/
void ArithmeticExpr::Check()  {
    bool num, vec, mat, err;

    if(left != NULL) 
        left -> Check();

    if(right != NULL)
        right -> Check();

    if(left != NULL && right != NULL) {
        if(!(left -> GetType() -> IsConvertibleTo(right -> GetType())))
            ReportError::IncompatibleOperands(op,left->GetType(),right->GetType());
    } else if(left == NULL) {
        num = right->GetType()->IsNumeric();
        vec = right->GetType()->IsVector();
        mat = right->GetType()->IsMatrix();
        err = right->GetType()->IsError();

        if( !(num || vec || mat || err) )
             ReportError::IncompatibleOperand(op,right->GetType());
    }

}

Type* ArithmeticExpr::GetType()  {
    Type* lhs = NULL;
    Type* rhs = NULL;

    if(left != NULL)
        lhs = left -> GetType();

    if(right != NULL)
        rhs = right -> GetType();

    if(left != NULL && right != NULL)  {
        if(rhs -> IsEquivalentTo(lhs))
            return rhs;
        else
            return Type::errorType;
    }
    return rhs;

  
}



/**** RELATIONAL EXPR ****/
void RelationalExpr::Check()  {
    left -> Check();
    right -> Check();


    if(!(left -> GetType() -> IsConvertibleTo (right -> GetType())))
        ReportError::IncompatibleOperands(op,left->GetType(),right->GetType());

}

Type* RelationalExpr::GetType() {
    if(left->GetType()->IsEquivalentTo(right->GetType()))
        return Type::boolType;
    else
        return Type::errorType;
}

/**** Equality Expr ****/
void EqualityExpr::Check() {
    Type* lhs = left -> GetType();
    Type* rhs = right -> GetType();
    left -> Check();
    right -> Check();

    if(!(lhs->IsConvertibleTo(rhs)))
        ReportError::IncompatibleOperands(op,lhs,rhs);
}

Type* EqualityExpr::GetType() {
    if(left->GetType()->IsEquivalentTo(right->GetType()))
        return Type::boolType;
    else
        return Type::errorType;
}


/**** LOGICAL TYPE ****/
void LogicalExpr::Check()  {
    bool num, vec, mat, err;

    if(left != NULL) 
        left -> Check();

    if(right != NULL)
        right -> Check();

    if(left != NULL && right != NULL) {
        if(!(left -> GetType() -> IsConvertibleTo(right -> GetType())))
            ReportError::IncompatibleOperands(op,left->GetType(),right->GetType());
    } else if(left == NULL) {
        num = right->GetType()->IsNumeric();
        vec = right->GetType()->IsVector();
        mat = right->GetType()->IsMatrix();
        err = right->GetType()->IsError();

        if( !(num || vec || mat || err) )
             ReportError::IncompatibleOperand(op,right->GetType());
    }
}


Type* LogicalExpr::GetType()  {
    if(left == NULL)
        return right->GetType();

    if(left->GetType()->IsEquivalentTo(right->GetType()))
        return Type::boolType;

    return Type::errorType;
}



/**** ASSIGN EXPR ****/
void AssignExpr::Check()  {
    Symbol* syml = NULL;
    Symbol* symr = NULL; 
    bool lvar = false;
    bool rvar = false;


    if(left != NULL) {
        if(VarExpr* vExpr = dynamic_cast<VarExpr*>(left))  { 
            syml = symtab->findall(vExpr->GetIdentifier()->GetName());
            vExpr -> Check();
            lvar = true;
        }
        else
            left -> Check();
    }
    
    if(right != NULL)  {
        if(VarExpr* vExpr = dynamic_cast<VarExpr*>(right)) {
            symr = symtab->findall(vExpr->GetIdentifier()->GetName());
            vExpr -> Check();
            rvar = true;
        }
        else
            right -> Check();
    }

    if( (left && right) ) {
        if( (lvar && syml) || (!lvar) ) {
                if( (rvar && symr) || (!rvar) ) {
                    if( !(right -> GetType() -> IsConvertibleTo(left -> GetType())) ){
                        ReportError::IncompatibleOperands(op,left->GetType(),right->GetType());
                    }
                }
        }
    }

}

Type* AssignExpr::GetType()  {
    Symbol* syml = NULL;
    Symbol* symr = NULL;
    bool lvar = false;
    bool rvar = false;
    
    if(VarExpr* vexp = dynamic_cast<VarExpr*>(left)) {
        syml = symtab -> findall(vexp ->GetIdentifier()->GetName());
        lvar = true;
    }

    if(VarExpr* vexp = dynamic_cast<VarExpr*>(right)) {
        symr = symtab -> findall (vexp->GetIdentifier()->GetName());
        rvar = true;
    }

    if( (left && right) ) {
        if( (lvar && syml) || (!lvar) ) {
            if( (rvar && symr) || (!rvar) ) {
                if( (right -> GetType() -> IsEquivalentTo(left -> GetType())) ){
                    return left->GetType();
                }
            }
        }
    }
    
    return Type::errorType;
}


/**** POSTFIX EXPR ****/
void PostfixExpr::Check()  {
    Expr* curr;
    bool num, vec, mat, err;

    if(left != NULL)
        curr = left;
    else if(right != NULL)
        curr = right;
   
    curr -> Check();

    num = curr->GetType()->IsNumeric();
    vec = curr->GetType()->IsVector();
    mat = curr->GetType()->IsMatrix();
    err = curr->GetType()->IsError();

    if( ! (num || vec || mat || err) )
        ReportError::IncompatibleOperand(op,curr->GetType());
    
}

Type* PostfixExpr::GetType()  {
    Expr* curr;

    if (right != NULL)
        curr = right;

    return curr->GetType();
}



/**** CONDITIONAL EXPR ****/
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




/**** ARRAY ACCESS ****/
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}

void ArrayAccess::PrintChildren(int indentLevel) {
    base->Print(indentLevel+1);
    subscript->Print(indentLevel+1, "(subscript) ");
}

void ArrayAccess::Check() {
    if (base != NULL)
        base -> Check();

    if(subscript != NULL)
        subscript -> Check();

    // Check.
    VarExpr* vexpr = dynamic_cast<VarExpr*>(base);
    vexpr -> Check();

    ArrayType* arr = dynamic_cast<ArrayType*>(vexpr->GetType());
    if( !(arr) )
        ReportError::NotAnArray(vexpr->GetIdentifier());



}

Type* ArrayAccess::GetType() {
    VarExpr* vexpr = dynamic_cast<VarExpr*>(base);
    ArrayType* arr = dynamic_cast<ArrayType*>(vexpr->GetType());
    if(arr)
        return arr->GetElemType();

    cout << "Array is errorType " << endl;
    return Type::errorType;

}


/**** FIELD ACCESS ****/
     
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

void FieldAccess::Check()  {
    // Check Base
    if(base != NULL) 
        base -> Check();

    // Check if valid type.
    if( !(base->GetType()->IsVector()) ) 
        ReportError::InaccessibleSwizzle(field,base);
    
    // Check if right swizzle.
    string name = field->GetName();
    for (int i = 0; i < name.size(); i++) {
        if( name[i] != 'w' && name[i] != 'x' && name[i] != 'y' && name[i] != 'z')  {
            ReportError::InvalidSwizzle(field, base);
            break;
        }
    }

    // Check if Swizzle is in bounds.
    if(base->GetType()->IsEquivalentTo(Type::vec2Type))  {
        for(int i = 0; i < name.size(); i++)  {
            if( (name[i] == 'z') || (name[i] == 'w'))  {
                ReportError::SwizzleOutOfBound(field,base);
                break;
            }
        }
    }

    if(base->GetType()->IsEquivalentTo(Type::vec3Type))  {
        for(int i = 0; i < name.size(); i++)  {
            if( name[i] == 'w' )  {
                ReportError::SwizzleOutOfBound(field,base);
                break;
            }
        }
    }

    if(name.size() > 4)
        ReportError::OversizedVector(field,base);

}


Type* FieldAccess::GetType()  {
    if(base == NULL)
        return Type::errorType;
    
    if( !(base->GetType()->IsVector()) )
        return Type::errorType;
    
    string name = field->GetName();
    for (int i = 0; i < name.size(); i++) {
        if( name[i] != 'w' && name[i] != 'x' && name[i] != 'y' && name[i] != 'z')  
            return Type::errorType;      
    }

    if(base->GetType()->IsEquivalentTo(Type::vec2Type))  {
        for(int i = 0; i < name.size(); i++)  {
            if( (name[i] == 'z') || (name[i] == 'w'))  
                return Type::errorType;
        }
    }

     if(base->GetType()->IsEquivalentTo(Type::vec3Type))  {
        for(int i = 0; i < name.size(); i++)  {
            if( name[i] == 'w' )  
                return Type::errorType;
        }
    }

    if(name.size() > 4)
        return Type::errorType;
    else if(name.size() == 1)
        return Type::floatType;
    else if(name.size() == 2)
        return Type::vec2Type;
    else if(name.size() == 3)
        return Type::vec3Type;
    else
        return Type::vec4Type;

}




/**** CALL EXPR ****/
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

void Call::Check()  {
    if(base != NULL)
        base -> Check();

    Symbol* sym = symtab -> findall(field -> GetName());
    List<VarDecl*>* formals;

    if(sym == NULL)
        ReportError::IdentifierNotDeclared(field,LookingForFunction);
    else  {
        FnDecl* fdecl = dynamic_cast<FnDecl*>(sym->decl);
        if(fdecl == NULL)
            ReportError::NotAFunction(field);
        else {
            formals = fdecl->GetFormals();

       	    if(formals -> NumElements() > actuals -> NumElements())
                ReportError::LessFormals(field,formals->NumElements(),actuals->NumElements());
            else if(formals -> NumElements() < actuals -> NumElements())
                ReportError::ExtraFormals(field,formals->NumElements(),actuals->NumElements());
            else  {
            	Type* fType;
            	Type* aType;
            	for(int i = 0; i < formals -> NumElements(); i++) {
                    fType = formals->Nth(i)->GetType();
                    aType = actuals->Nth(i)->GetType();
                    if( !(aType->IsConvertibleTo(fType)) ) {
                        ReportError::FormalsTypeMismatch(formals->Nth(i)->GetIdentifier(),i,fType,aType);
                    }
            	}
            }
    	}
    }    
}

Type* Call::GetType()  {
    Symbol* sym = symtab -> findall(field->GetName());
    if (sym == NULL)
        return Type::errorType;

    FnDecl* fndecl = dynamic_cast<FnDecl*>(sym->decl);
    if(fndecl == NULL)
        return Type::errorType;

    if(fndecl->GetFormals()->NumElements() != actuals->NumElements())
        return Type::errorType;

    for(int i = 0; i < actuals->NumElements(); i++)  {
        if(!actuals->Nth(i)->GetType()->IsEquivalentTo(fndecl->GetFormals()->Nth(i)->GetType()))
            return Type::errorType;
    }
 
    return fndecl -> GetType();
}
