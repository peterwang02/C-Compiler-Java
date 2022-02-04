package ast;

import compiler.Scope;

import ast.visitor.ASTVisitor;
import compiler.Compiler;

public class TypeCastNode extends ExpressionNode {
    ExpressionNode arg;
    String funcName;
    Scope.FunctionSymbolTableEntry ste;

    public TypeCastNode(Scope.Type t, ExpressionNode arg) {
		this.ste = (Scope.FunctionSymbolTableEntry) Compiler.symbolTable.getFunctionSymbol(funcName);
		this.setType(t);
		this.arg = arg;
	}

    @Override
    public <R> R accept(ASTVisitor<R> visitor) {
        return visitor.visit(this);
    }

    public ExpressionNode getArg() {
        return arg;
    }

    public String getFuncName() {
        return funcName;
    }
}