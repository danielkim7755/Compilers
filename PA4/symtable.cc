/*
 * Symbol table implementation
 *
 */

#include "symtable.h"

// ScopedTable Class Implementation
ScopedTable::ScopedTable() {
    SymbolIterator it = symbols.begin();
}
ScopedTable::~ScopedTable() { }

void ScopedTable::insert(Symbol &sym) {
    it = symbols.find(sym.name);
    if(it != symbols.end()) { 
        ReportError::DeclConflict(sym.decl,it->second.decl);
        remove(it->second);
    }
 
    it = symbols.end();
    symbols.insert(it,std::pair<const char*,Symbol> (sym.name,sym));
}

void ScopedTable::remove(Symbol &sym) {
    it = symbols.find(sym.name);
    symbols.erase(it);
}


Symbol* ScopedTable::find(const char *name) {
    it = symbols.find(name);
    
    if (it == symbols.end() || symbols.empty())  {
        return NULL;
    }
    
    Symbol* s = &(it->second);
    return s;
}


// SymbolTable Class Implementation
SymbolTable::SymbolTable() {
    currScope = 0;
    tables.push_back(new ScopedTable());
}

SymbolTable::~SymbolTable() { }

void SymbolTable::push() {
    tables.push_back(new ScopedTable());
    currScope++;
}

void SymbolTable::pop() {
    tables.pop_back();
    currScope--;
}

void SymbolTable::insert(Symbol &sym) {
    tables[currScope]->insert(sym);

}

void SymbolTable::remove(Symbol &sym) {
    tables[currScope]->remove(sym);
}

Symbol* SymbolTable::find(const char *name) {
    return tables[currScope]->find(name);
}

Symbol* SymbolTable::findall(const char *name)  {
    Symbol* sym;

    for (int i = currScope; i >= 0; i--) {
        sym = tables[i] -> find(name);
        if(sym != NULL)
            return sym;
    }

    return NULL;
}

