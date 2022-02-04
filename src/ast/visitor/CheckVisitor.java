package ast.visitor;

import java.util.Iterator;
import java.util.List;

import ast.ASTNode;
import ast.AddrOfNode;
import ast.AssignNode;
import ast.BinaryOpNode;
import ast.CallNode;
import ast.WhileNode;
import ast.IfStatementNode;
import ast.IntLitNode;
import ast.MallocNode;
import ast.PtrDerefNode;
import ast.ReadNode;
import ast.StatementListNode;
import ast.VarNode;
import ast.WriteNode;
import compiler.Scope.Type;
import ast.ReturnNode;
import ast.CondNode;
import ast.FunctionNode;
import ast.FloatLitNode;
import ast.FreeNode;
import ast.ExpressionNode;
import ast.TypeCastNode;

import compiler.Compiler;
import compiler.Scope.FunctionSymbolTableEntry;
import compiler.Scope.Type;
import compiler.Scope;

public class CheckVisitor extends AbstractASTVisitor<Void> {
	int depth;
	
	public CheckVisitor() {depth = 0;}
	
	@Override
	public Void run(ASTNode node) {
		depth = 0;
		return node.accept(this);
	}
	
	@Override
	protected void preprocess(VarNode node) {
	}

	@Override
	protected void preprocess(IntLitNode node) {
	}

	@Override
	protected void preprocess(FloatLitNode node) {
	}

	@Override
	protected void preprocess(BinaryOpNode node) {
		ExpressionNode left = (ExpressionNode) node.getLeft();
		ExpressionNode right = (ExpressionNode) node.getRight();
		if (left.getType().getWrappedType() != right.getType().getWrappedType()) {
			System.err.println("binary op");
			System.err.println("TYPE ERROR");
			System.exit(7);
		}
		
		depth++;
	}
	
	@Override
	protected Void postprocess(BinaryOpNode node, Void left, Void right) {
		--depth;
		return null;
	}
	
	@Override
	protected void preprocess(AssignNode node) {
		ExpressionNode left = (ExpressionNode) node.getLeft();
		ExpressionNode right = (ExpressionNode) node.getRight();
		Type leftType = left.getType();
		Type rightType = right.getType();
		if (leftType == Type.PTR) {
			leftType = leftType.getWrappedType();
		}
		if (rightType == Type.PTR) {
			rightType = rightType.getWrappedType();
		}
		if (leftType != rightType) {
			System.err.println("assign " + leftType + rightType);
			System.err.println("TYPE ERROR");
			System.exit(7);			
		}
		depth++;
	}
	
	@Override
	protected Void postprocess(AssignNode node, Void left, Void right) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(StatementListNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(StatementListNode node, List<Void> statements) {
		--depth;
		return null;
	}
	
	@Override
	protected void preprocess(ReadNode node) {
		depth++;
	}

	@Override
	protected void preprocess(WriteNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(WriteNode node, Void writeExpr) {
		--depth;
		return null;
	}

	@Override
	protected Void postprocess(ReadNode node, Void var) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(ReturnNode node) {
		if (node.getRetExpr().getType() != node.getFuncSymbol().getReturnType()) {
			//System.err.println("return");
			System.err.println("TYPE ERROR");
			System.exit(7);
		}
		depth++;
	}

	@Override
	protected Void postprocess(ReturnNode node, Void retExpr) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(CondNode node) {
		if (node.getLeft().getType() != node.getRight().getType()) {
			//System.err.println("cond");
			System.err.println("TYPE ERROR");
			System.exit(7);
		}
		depth++;
	}
	
	@Override
	protected Void postprocess(CondNode node, Void left, Void right) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(IfStatementNode node) {
		depth++;
	}
	
	@Override
	protected Void postprocess(IfStatementNode node, Void cond, Void slist, Void epart) {
		--depth;
		return null;
	}
	
	@Override
	protected void preprocess(WhileNode node) {
		depth++;
	}
	
	@Override
	protected Void postprocess(WhileNode node, Void cond, Void slist) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(FunctionNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(FunctionNode node, Void body) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(CallNode node) {
		FunctionSymbolTableEntry ste = Compiler.symbolTable.getFunctionSymbol(node.getFuncName());
		if (node.getArgs().size() == ste.getArgTypes().size()) {
			Iterator<ExpressionNode> i1 = node.getArgs().iterator();
			Iterator<Type> i2 = ste.getArgTypes().iterator();
			while (i1.hasNext() && i2.hasNext()) {
				if (i1.next().getType() != i2.next()) {
					//System.err.println("call type");
					System.err.println("TYPE ERROR");
					System.exit(7);					
				}
			}
		} else {
			//System.err.println("call size");
			System.err.println("TYPE ERROR");
			System.exit(7);
		}
		depth++;
	}

	@Override
	protected Void postprocess(CallNode node, List<Void> args) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(PtrDerefNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(PtrDerefNode node, Void expr) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(AddrOfNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(AddrOfNode node, Void expr) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(MallocNode node) {
		if (node.getType() != Type.INFER) {
			System.err.println("TYPE ERROR");
			System.exit(7);		
		}
		depth++;
	}

	@Override
	protected Void postprocess(MallocNode node, Void expr) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(FreeNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(FreeNode node, Void expr) {
		--depth;
		return null;
	}

	@Override
	protected void preprocess(TypeCastNode node) {
		depth++;
	}

	@Override
	protected Void postprocess(TypeCastNode node, Void expr) {
		--depth;
		return null;
	}

}
