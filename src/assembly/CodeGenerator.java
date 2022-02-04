package assembly;

import java.util.List;

import compiler.Scope.SymbolTableEntry;
import compiler.Scope.Type;
import ast.visitor.AbstractASTVisitor;

import ast.*;
import assembly.instructions.*;
import ast.UnaryOpNode.OpType;
import compiler.Scope;

public class CodeGenerator extends AbstractASTVisitor<CodeObject> {

	int intRegCount;
	int floatRegCount;
	static final public char intTempPrefix = 't';
	static final public char floatTempPrefix = 'f';
	
	int loopLabel;
	int elseLabel;
	int outLabel;

	String currFunc;
	
	public CodeGenerator() {
		loopLabel = 0;
		elseLabel = 0;
		outLabel = 0;
		intRegCount = 0;		
		floatRegCount = 0;
	}

	public int getIntRegCount() {
		return intRegCount;
	}

	public int getFloatRegCount() {
		return floatRegCount;
	}
	
	/**
	 * Generate code for Variables
	 * 
	 * Create a code object that just holds a variable
	 * 
	 * Important: add a pointer from the code object to the symbol table entry
	 *            so we know how to generate code for it later (we'll need to find
	 *            the address)
	 * 
	 * Mark the code object as holding a variable, and also as an lval
	 */
	@Override
	protected CodeObject postprocess(VarNode node) {
		
		Scope.SymbolTableEntry sym = node.getSymbol();
		
		CodeObject co = new CodeObject(sym);
		co.lval = true;
		co.type = node.getType();

		return co;
	}

	/** Generate code for IntLiterals
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(IntLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a register
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new Li(generateTemp(Scope.Type.INT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/** Generate code for FloatLiteras
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(FloatLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a regisster
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new FImm(generateTemp(Scope.Type.FLOAT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/**
	 * Generate code for binary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from left child
	 * Step 1a: if left child is an lval, add a load to get the data
	 * Step 2: add code from right child
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate binary operation using temps from left and right
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(BinaryOpNode node, CodeObject left, CodeObject right) {

		CodeObject co = new CodeObject();
		
		/* FILL IN FROM STEP 2 */
		if (left.lval) {
			left = rvalify(left);
		}
		co.code.addAll(left.code);
		if (right.lval) {
			right = rvalify(right);
		}
		co.code.addAll(right.code);
		if (left.type == Type.FLOAT && right.type == Type.INT) {
			co.code.add(cast(right));
		} else if (left.type == Type.INT && right.type == Type.FLOAT) {
			co.code.add(cast(left));
		}
		switch(left.getType()) {
			case INT:
				switch(node.getOp()) {
					case ADD:
						Instruction i = new Add(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(i);
						co.lval = false;
						co.temp = i.getDest();
						break;
					case MUL:
						Instruction j = new Mul(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(j);
						co.lval = false;
						co.temp = j.getDest();
						break;
					case SUB:
						Instruction k = new Sub(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(k);
						co.lval = false;
						co.temp = k.getDest();
						break;
					case DIV:
						Instruction l = new Div(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(l);
						co.lval = false;
						co.temp = l.getDest();
						break;
					default:
						throw new Error("Not binary operator");
				}
				break;
			case FLOAT:
				switch(node.getOp()) {
					case ADD:
						Instruction i = new FAdd(left.temp, right.temp, generateTemp(Scope.Type.FLOAT));
						co.code.add(i);
						co.lval = false;
						co.temp = i.getDest();
						break;
					case MUL:
						Instruction j = new FMul(left.temp, right.temp, generateTemp(Scope.Type.FLOAT));
						co.code.add(j);
						co.lval = false;
						co.temp = j.getDest();
						break;
					case SUB:
						Instruction k = new FSub(left.temp, right.temp, generateTemp(Scope.Type.FLOAT));
						co.code.add(k);
						co.lval = false;
						co.temp = k.getDest();
						break;
					case DIV:
						Instruction l = new FDiv(left.temp, right.temp, generateTemp(Scope.Type.FLOAT));
						co.code.add(l);
						co.lval = false;
						co.temp = l.getDest();
						break;
					default:
						throw new Error("Not binary operator");
				}
				break;
			case PTR :
				switch(node.getOp()) {
					case ADD:
						Instruction i = new Add(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(i);
						co.lval = false;
						co.temp = i.getDest();
						break;
					case MUL:
						Instruction j = new Mul(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(j);
						co.lval = false;
						co.temp = j.getDest();
						break;
					case SUB:
						Instruction k = new Sub(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(k);
						co.lval = false;
						co.temp = k.getDest();
						break;
					case DIV:
						Instruction l = new Div(left.temp, right.temp, generateTemp(Scope.Type.INT));
						co.code.add(l);
						co.lval = false;
						co.temp = l.getDest();
						break;
					default:
						throw new Error("Not binary operator");
				}
				break;
			default:
				throw new Error("Not INT or FLOAT or PTR");
			}
		co.type = left.getType();
		return co;
	}

	/**
	 * Generate code for unary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from child expression
	 * Step 1a: if child is an lval, add a load to get the data
	 * Step 2: generate instruction to perform unary operation
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(UnaryOpNode node, CodeObject expr) {
		
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 2 */
		if (expr.lval) {
			expr = rvalify(expr);
		}
		co.code.addAll(expr.code);
		switch(expr.getType()) {
			case INT:
				if (node.getOp() == OpType.NEG) {
					Instruction i = new Neg(expr.temp, generateTemp(Scope.Type.INT));
					co.lval = false;
					co.temp = i.getDest();
					co.code.add(i);
				} else {
					throw new Error ("Not Unary Operator");
				}
				break;
			case FLOAT:
				if (node.getOp() == OpType.NEG) {
					Instruction i = new Neg(expr.temp, generateTemp(Scope.Type.FLOAT));
					co.lval = false;
					co.temp = i.getDest();
					co.code.add(i);
				} else {
					throw new Error ("Not Unary Operator");
				}
				break;
			default:
				throw new Error ("Not INT or FLOAT");
		}
		co.type = expr.getType();
		return co;
	}

	/**
	 * Generate code for assignment statements
	 * 
	 * Step 0: create new code object
	 * Step 1: if LHS is a variable, generate a load instruction to get the address into a register
	 * Step 1a: add code from LHS of assignment (make sure it results in an lval!)
	 * Step 2: add code from RHS of assignment
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate store
	 * 
	 * Hint: it is going to be easiest to just generate a store with a 0 immediate
	 * offset, and the complete store address in a register:
	 * 
	 * sw rhs 0(lhs)
	 */
	@Override
	protected CodeObject postprocess(AssignNode node, CodeObject left,
			CodeObject right) {
		
		CodeObject co = new CodeObject();
		assert(left.lval == true); //left hand side had better hold an address

		//Step 1a

		/* FILL IN FOR STEP 2 */
		
		if (right.lval) {
			right = rvalify(right);
		}
		co.code.addAll(left.code);
		if (left.isVar() && !left.getSTE().isLocal()) {
			co.code.addAll(generateAddrFromVariable(left));
		}
		co.code.addAll(right.code);
		Instruction i = null;
		if(node.getType() == Scope.Type.INT || node.getType() == Scope.Type.PTR) {
			if (node.getType() == Scope.Type.INT && right.type == Scope.Type.FLOAT) {
				co.code.add(cast(right));
			}
			if (left.isVar() && left.getSTE().isLocal()) {
				i = new Sw(right.temp, "fp", left.getSTE().addressToString());
			} else {
				i = new Sw(right.temp, left.temp, "0");		
			} 
		} else if (node.getType() == Scope.Type.FLOAT) {
			if (right.type == Scope.Type.INT) {
				co.code.add(cast(right));
			}
			if (left.isVar() && left.getSTE().isLocal()) {
				i = new Fsw(right.temp, "fp", left.getSTE().addressToString());
			} else {
				i = new Fsw(right.temp, left.temp, "0");		
			} 
		}
		co.type = node.getType(); 		
		co.code.add(i);
		co.temp = i.getDest();
		co.lval = true;
		return co;
	}

	/**
	 * Add together all the lists of instructions generated by the children
	 */
	@Override
	protected CodeObject postprocess(StatementListNode node,
			List<CodeObject> statements) {
		CodeObject co = new CodeObject();
		//add the code from each individual statement
		for (CodeObject subcode : statements) {
			co.code.addAll(subcode.code);
		}
		co.type = null; //set to null to trigger errors
		return co;
	}
	
	/**
	 * Generate code for read
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from VarNode (make sure it's an lval)
	 * Step 2: generate GetI instruction, storing into temp
	 * Step 3: generate store, to store temp in variable
	 */
	@Override
	protected CodeObject postprocess(ReadNode node, CodeObject var) {
		
		//Step 0
		CodeObject co = new CodeObject();

		//Generating code for read(id)
		assert(var.getSTE() != null); //var had better be a variable

		InstructionList il = new InstructionList();
		switch(node.getType()) {
			case INT: 
				//Code to generate if INT:
				//geti tmp
				//if var is global: la tmp', <var>; sw tmp 0(tmp')
				//if var is local: sw tmp offset(fp)
				Instruction geti = new GetI(generateTemp(Scope.Type.INT));
				il.add(geti);
				InstructionList store = new InstructionList();
				if (var.getSTE().isLocal()) {
					store.add(new Sw(geti.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					store.addAll(generateAddrFromVariable(var));
					store.add(new Sw(geti.getDest(), store.getLast().getDest(), "0"));
				}
				il.addAll(store);
				break;
			case FLOAT:
				//Code to generate if FLOAT:
				//getf tmp
				//if var is global: la tmp', <var>; fsw tmp 0(tmp')
				//if var is local: fsw tmp offset(fp)
				Instruction getf = new GetF(generateTemp(Scope.Type.FLOAT));
				il.add(getf);
				InstructionList fstore = new InstructionList();
				if (var.getSTE().isLocal()) {
					fstore.add(new Fsw(getf.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					fstore.addAll(generateAddrFromVariable(var));
					fstore.add(new Fsw(getf.getDest(), fstore.getLast().getDest(), "0"));
				}
				il.addAll(fstore);
				break;
			default:
				throw new Error("Shouldn't read into other variable");
		}
		
		co.code.addAll(il);

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * Generate code for print
	 * 
	 * Step 0: create new code object
	 * 
	 * If printing a string:
	 * Step 1: add code from expression to be printed (make sure it's an lval)
	 * Step 2: generate a PutS instruction printing the result of the expression
	 * 
	 * If printing an integer:
	 * Step 1: add code from the expression to be printed
	 * Step 1a: if it's an lval, generate a load to get the data
	 * Step 2: Generate PutI that prints the temporary holding the expression
	 */
	@Override
	protected CodeObject postprocess(WriteNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		//generating code for write(expr)

		//for strings, we expect a variable
		if (expr.getType() == Scope.Type.STRING) {
			//Step 1:
			assert(expr.getSTE() != null);
			
			System.out.println("; generating code to print " + expr.getSTE());

			//Get the address of the variable
			InstructionList addrCo = generateAddrFromVariable(expr);
			co.code.addAll(addrCo);

			//Step 2:
			Instruction write = new PutS(addrCo.getLast().getDest());
			co.code.add(write);
		} else {
			//Step 1a:
			//if expr is an lval, load from it
			if (expr.lval == true) {
				expr = rvalify(expr);
			}
			
			//Step 1:
			co.code.addAll(expr.code);

			//Step 2:
			//if type of writenode is int, use puti, if float, use putf
			Instruction write = null;
			switch(expr.getType()) {
			case STRING: throw new Error("Shouldn't have a STRING here");
			case INT: 
			case PTR: //should work the same way for pointers
				write = new PutI(expr.temp); break;
			case FLOAT: write = new PutF(expr.temp); break;
			default: throw new Error("WriteNode has a weird type");
			}

			co.code.add(write);
		}

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Generating an instruction sequence for a conditional expression
	 * 
	 * Implement this however you like. One suggestion:
	 *
	 * Create the code for the left and right side of the conditional, but defer
	 * generating the branch until you process IfStatementNode or WhileNode (since you
	 * do not know the labels yet). Modify CodeObject so you can save the necessary
	 * information to generate the branch instruction in IfStatementNode or WhileNode
	 * 
	 * Alternate idea 1:
	 * 
	 * Don't do anything as part of CodeGenerator. Create a new visitor class
	 * that you invoke *within* your processing of IfStatementNode or WhileNode
	 * 
	 * Alternate idea 2:
	 * 
	 * Create the branch instruction in this function, then tweak it as necessary in
	 * IfStatementNode or WhileNode
	 * 
	 * Hint: you may need to preserve extra information in the returned CodeObject to
	 * make sure you know the type of branch code to generate (int vs float)
	 */
	@Override
	protected CodeObject postprocess(CondNode node, CodeObject left, CodeObject right) {
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		if (left.lval) {
			left = rvalify(left);
		}
		if (right.lval) {
			right = rvalify(right);
		}
		co.ltemp = left.temp;
		co.rtemp = right.temp;
		co.code.addAll(left.code);
		co.code.addAll(right.code);
		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		<cond code>
	 *		<flipped branch> elseLabel
	 *		<then code>
	 *		j outLabel
	 *		elseLabel:
	 *		<else code>
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(IfStatementNode node, CodeObject cond, CodeObject tlist, CodeObject elist) {
		//Step 0:
		CodeObject co = new CodeObject();

		String outlabel = generateOutLabel();
		Instruction olabel = new Label(outlabel);
		
		String elselabel = generateElseLabel();
		Instruction elabel = new Label(elselabel);
		
		if (elist == null) {
			outLabel--;
			elselabel = generateOutLabel();
			olabel = new Label(elselabel);	
		}


		co.code.addAll(cond.code);
		switch(node.getCondExpr().getLeft().getType()) {
			case INT:
			switch(node.getCondExpr().getReversedOp()) {
				case EQ:
					Instruction b1 = new Beq(cond.ltemp, cond.rtemp, elselabel);
					co.code.add(b1);
					break;
				case GE:
					Instruction b2 = new Bge(cond.ltemp, cond.rtemp, elselabel);
					co.code.add(b2);
					break;
				case GT:
					Instruction b3 = new Bgt(cond.ltemp, cond.rtemp, elselabel);
					co.code.add(b3);
					break;
				case LE:
					Instruction b4 = new Ble(cond.ltemp, cond.rtemp, elselabel);
					co.code.add(b4);
					break;
				case LT:
					Instruction b5 = new Blt(cond.ltemp, cond.rtemp, elselabel);
					co.code.add(b5);
					break;
				case NE:
					Instruction b6 = new Bne(cond.ltemp, cond.rtemp, elselabel);
					co.code.add(b6);
					break;
				default:
					throw new Error("Branch condition not recognized");
			}
			break;
			case FLOAT:
			switch(node.getCondExpr().getReversedOp()) {
				case EQ:
					Instruction f1 = new Feq(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f1);
					Instruction b1 = new Bne(f1.getDest(), "x0", elselabel);
					co.code.add(b1);
					break;
				case LE:
					Instruction f2 = new Fle(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f2);
					Instruction b2 = new Bne(f2.getDest(), "x0", elselabel);
					co.code.add(b2);
					break;
				case LT:
					Instruction f3 = new Flt(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f3);
					Instruction b3 = new Bne(f3.getDest(), "x0", elselabel);
					co.code.add(b3);
					break;
				case NE:
					Instruction f4 = new Feq(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f4);
					Instruction b4 = new Beq(f4.getDest(), "x0", elselabel);
					co.code.add(b4);
					break;
				case GT:
					Instruction f5 = new Fle(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f5);
					Instruction b5 = new Beq(f5.getDest(), "x0", elselabel);
					co.code.add(b5);
					break;
				case GE:
					Instruction f6 = new Flt(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f6);
					Instruction b6 = new Beq(f6.getDest(), "x0", elselabel);
					co.code.add(b6);
					break;
				default:
					throw new Error("Branch condition not recognized");
			}
			break;
			default:
			throw new Error("Not INT or FLOAT");
		}	

		co.code.addAll(tlist.code);
		
		if (elist != null) {
			Instruction i = new J(outlabel);
			co.code.add(i);
			co.code.add(elabel);
			co.code.addAll(elist.code);
		}	
		co.code.add(olabel);

		return co;
	}

		/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		loopLabel:
	 *		<cond code>
	 *		<flipped branch> outLabel
	 *		<body code>
	 *		j loopLabel
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(WhileNode node, CodeObject cond, CodeObject slist) {
		//Step 0:
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		String looplabel = generateLoopLabel();
		String outlabel = generateOutLabel();
		Instruction llabel = new Label(looplabel);
		Instruction olabel = new Label(outlabel);

		co.code.add(llabel);
		co.code.addAll(cond.code);

		switch(node.getCond().getLeft().getType()) {
			case INT:
			switch(node.getCond().getReversedOp()) {
				case EQ:
					Instruction b1 = new Beq(cond.ltemp, cond.rtemp, outlabel);
					co.code.add(b1);
					break;
				case GE:
					Instruction b2 = new Bge(cond.ltemp, cond.rtemp, outlabel);
					co.code.add(b2);
					break;
				case GT:
					Instruction b3 = new Bgt(cond.ltemp, cond.rtemp, outlabel);
					co.code.add(b3);
					break;
				case LE:
					Instruction b4 = new Ble(cond.ltemp, cond.rtemp, outlabel);
					co.code.add(b4);
					break;
				case LT:
					Instruction b5 = new Blt(cond.ltemp, cond.rtemp, outlabel);
					co.code.add(b5);
					break;
				case NE:
					Instruction b6 = new Bne(cond.ltemp, cond.rtemp, outlabel);
					co.code.add(b6);
					break;
				default:
					throw new Error("Branch condition not recognized");
			}
			break;
			case FLOAT:
			switch(node.getCond().getReversedOp()) {
				case EQ:
					Instruction f1 = new Feq(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f1);
					Instruction b1 = new Bne(f1.getDest(), "x0", outlabel);
					co.code.add(b1);
					break;
				case LE:
					Instruction f2 = new Fle(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f2);
					Instruction b2 = new Bne(f2.getDest(), "x0", outlabel);
					co.code.add(b2);
					break;
				case LT:
					Instruction f3 = new Flt(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f3);
					Instruction b3 = new Bne(f3.getDest(), "x0", outlabel);
					co.code.add(b3);
					break;
				case NE:
					Instruction f4 = new Feq(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f4);
					Instruction b4 = new Beq(f4.getDest(), "x0", outlabel);
					co.code.add(b4);
					break;
				case GT:
					Instruction f5 = new Fle(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f5);
					Instruction b5 = new Beq(f5.getDest(), "x0", outlabel);
					co.code.add(b5);
					break;
				case GE:
					Instruction f6 = new Flt(cond.ltemp, cond.rtemp, generateTemp(Scope.Type.INT));
					co.code.add(f6);
					Instruction b6 = new Beq(f6.getDest(), "x0", outlabel);
					co.code.add(b6);
					break;
				default:
					throw new Error("Branch condition not recognized");
			}
			break;
			default:
			throw new Error("Not INT or FLOAT");
		}
		
		co.code.addAll(slist.code);
		Instruction i = new J(looplabel);
		co.code.add(i);
		co.code.add(olabel);
		return co;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generating code for returns
	 * 
	 * Step 0: Generate new code object
	 * 
	 * Step 1: Add retExpr code to code object (rvalify if necessary)
	 * 
	 * Step 2: Store result of retExpr in appropriate place on stack (fp + 8)
	 * 
	 * Step 3: Jump to out label (use @link{generateFunctionOutLabel()})
	 */
	@Override
	protected CodeObject postprocess(ReturnNode node, CodeObject retExpr) {
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 4 */
		if (retExpr != null) {
			if (retExpr.lval) {
				retExpr = rvalify(retExpr);
			}
			co.code.addAll(retExpr.getCode());
			Instruction i = null;
			if (retExpr.getType() == Scope.Type.INT || retExpr.getType() == Scope.Type.PTR) {
				i = new Sw(retExpr.temp, "fp", "8");
			} else {
				i = new Fsw(retExpr.temp, "fp", "8");
			}	
			co.code.add(i);		
			co.temp = i.getDest();	
			Instruction j = new J(generateFunctionOutLabel());
			co.code.add(j);
			co.lval = true;
		}
		return co;
	}

	@Override
	protected void preprocess(FunctionNode node) {
		// Generate function label information, used for other labels inside function
		currFunc = node.getFuncName();

		//reset register counts; each function uses new registers!
		intRegCount = 0;
		floatRegCount = 0;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generate code for functions
	 * 
	 * Step 1: add the label for the beginning of the function
	 * 
	 * Step 2: manage frame  pointer
	 * 			a. Save old frame pointer
	 * 			b. Move frame pointer to point to base of activation record (current sp)
	 * 			c. Update stack pointer
	 * 
	 * Step 3: allocate new stack frame (use scope infromation from FunctionNode)
	 * 
	 * Step 4: save registers on stack (Can inspect intRegCount and floatRegCount to know what to save)
	 * 
	 * Step 5: add the code from the function body
	 * 
	 * Step 6: add post-processing code:
	 * 			a. Label for `return` statements inside function body to jump to
	 * 			b. Restore registers
	 * 			c. Deallocate stack frame (set stack pointer to frame pointer)
	 * 			d. Reset fp to old location
	 * 			e. Return from function
	 */
	@Override
	protected CodeObject postprocess(FunctionNode node, CodeObject body) {
		CodeObject co = new CodeObject();

		/* FILL IN */
		Instruction funclabel = new Label(generateFunctionLabel());
		co.code.add(funclabel);

		Instruction savefp = new Sw("fp", "sp", "0");
		Instruction movefp = new Mv("sp", "fp");
		Instruction updatesp = new Addi("sp", "-4", "sp");

		co.code.add(savefp);
		co.code.add(movefp);
		co.code.add(updatesp);

		String offset = Integer.toString(node.getScope().getNumLocals() * 4);
		Instruction allocatestack = new Addi("sp", "-"+offset, "sp");
		co.code.add(allocatestack);
		for (int i = 1; i <= intRegCount; i++) {
			Instruction intreg = new Sw("t" + Integer.toString(i), "sp", "0");
			co.code.add(intreg);
			co.code.add(updatesp);
		}
		for (int i = 1; i <= floatRegCount; i++) {
			Instruction floatreg = new Fsw("f" + Integer.toString(i), "sp", "0");
			co.code.add(floatreg);
			co.code.add(updatesp);
		}

		co.code.addAll(body.getCode());
		Instruction retlabel = new Label(generateFunctionOutLabel());
		co.code.add(retlabel);
		Instruction restoresp = new Addi("sp", "4", "sp");

		for (int i = floatRegCount; i != 0; i--) {
			Instruction floatreg = new Flw("f" + Integer.toString(i), "sp", "0");
			co.code.add(restoresp);
			co.code.add(floatreg);
		}
		for (int i = intRegCount; i != 0; i--) {
			Instruction intreg = new Lw("t" + Integer.toString(i), "sp", "0");
			co.code.add(restoresp);
			co.code.add(intreg);
		}		

		Instruction movesp = new Mv("fp", "sp");
		Instruction restorefp = new Lw("fp", "fp", "0");
		Instruction ret = new Ret();
		co.code.add(movesp);
		co.code.add(restorefp);
		co.code.add(ret);
		return co;
	}

	/**
	 * Generate code for the list of functions. This is the "top level" code generation function
	 * 
	 * Step 1: Set fp to point to sp
	 * 
	 * Step 2: Insert a JR to main
	 * 
	 * Step 3: Insert a HALT
	 * 
	 * Step 4: Include all the code of the functions
	 */
	@Override
	protected CodeObject postprocess(FunctionListNode node, List<CodeObject> funcs) {
		CodeObject co = new CodeObject();

		co.code.add(new Mv("sp", "fp"));
		co.code.add(new Jr(generateFunctionLabel("main")));
		co.code.add(new Halt());
		co.code.add(new Blank());

		//add code for each of the functions
		for (CodeObject c : funcs) {
			co.code.addAll(c.code);
			co.code.add(new Blank());
		}

		return co;
	}

	/**
	* 
	* FILL IN FOR STEP 4
	* 
	* Generate code for a call expression
	 * 
	 * Step 1: For each argument:
	 * 
	 * 	Step 1a: insert code of argument (don't forget to rvalify!)
	 * 
	 * 	Step 1b: push result of argument onto stack 
	 * 
	 * Step 2: alloate space for return value
	 * 
	 * Step 3: push current return address onto stack
	 * 
	 * Step 4: jump to function
	 * 
	 * Step 5: pop return address back from stack
	 * 
	 * Step 6: pop return value into fresh temporary (destination of call expression)
	 * 
	 * Step 7: remove arguments from stack (move sp)
	 * 
	 * Add special handling for malloc and free
	 */

	 /**
	  * FOR STEP 6: Make sure to handle VOID functions properly
	  */
	@Override
	protected CodeObject postprocess(CallNode node, List<CodeObject> args) {
		
		//STEP 0
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 4 */
		Instruction decrSp = new Addi("sp", "-4", "sp");
		for (CodeObject arg: args) {
			if (arg.lval) {
				arg = rvalify(arg);
			}
			co.code.addAll(arg.getCode());
			if (arg.type == Scope.Type.INT || arg.type == Scope.Type.PTR) {
				Instruction storeresult = new Sw(arg.temp, "sp", "0");
				co.code.add(storeresult);
			} else if (arg.type == Scope.Type.FLOAT) {
				Instruction storeresult = new Fsw(arg.temp, "sp", "0");
				co.code.add(storeresult);
			}
			co.code.add(decrSp);
		}
		co.code.add(decrSp);

		Instruction storeRetAddr = new Sw("ra", "sp", "0");
		co.code.add(storeRetAddr);
		co.code.add(decrSp);
		Instruction jmpFunc = new Jr(generateFunctionLabel(node.getFuncName()));
		co.code.add(jmpFunc);
		
		Instruction restoreSp = new Addi("sp", "4", "sp");
		Instruction popRa = new Lw("ra", "sp", "0");
		co.code.add(restoreSp);
		co.code.add(popRa);

		co.code.add(restoreSp);
		Instruction popRetval = null;
		if (node.getType() == Scope.Type.INT || node.getType() == Scope.Type.PTR) {
			popRetval = new Lw(generateTemp(Scope.Type.INT), "sp", "0");
		} else if (node.getType() == Scope.Type.FLOAT) {
			popRetval = new Flw(generateTemp(Scope.Type.FLOAT), "sp", "0");
		}
		if (popRetval != null) {
			co.code.add(popRetval);
		}
		String arglength = Integer.toString(args.size() * 4);
		Instruction popArg = new Addi("sp", arglength, "sp");
		co.code.add(popArg);

		if (popRetval != null) {
			co.temp = popRetval.getDest();
		}
		co.lval = false;
		co.type = node.getType();

		return co;
	}	
	
	/**
	 * Generate code for * (expr)
	 * 
	 * Goal: convert the r-val coming from expr (a computed address) into an l-val (an address that can be loaded/stored)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Rvalify expr if needed
	 * 
	 * Step 2: Copy code from expr (including any rvalification) into new code object
	 * 
	 * Step 3: New code object has same temporary as old code, but now is marked as an l-val
	 * 
	 * Step 4: New code object has an "unwrapped" type: if type of expr is * T, type of temporary is T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(PtrDerefNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if (expr.lval) {
			expr = rvalify(expr);
		}
		co.code.addAll(expr.getCode());
		co.temp = expr.temp;
		co.lval = true;
		co.type = node.getType();

		return co;
	}

	/**
	 * Generate code for a & (expr)
	 * 
	 * Goal: convert the lval coming from expr (an address) to an r-val (a piece of data that can be used)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: If lval is a variable, generate code to put address into a register (e.g., generateAddressFromVar)
	 *			Otherwise just copy code from other code object
	 * 
	 * Step 2: New code object has same temporary as existing code, but is an r-val
	 * 
	 * Step 3: New code object has a "wrapped" type. If type of expr is T, type of temporary is *T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(AddrOfNode node, CodeObject expr) {
		CodeObject co = new CodeObject();
		co.code.addAll(expr.getCode());
		if (expr.isVar()) {
			co.code.addAll(generateAddrFromVariable(expr));
		}
		co.temp = expr.temp;
		co.lval = false;
		co.type = node.getType();

		return co;
	}

	/**
	 * Generate code for malloc
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new MALLOC instruction
	 * 
	 * Step 3: Set code object type to INFER
	 */
	@Override
	protected CodeObject postprocess(MallocNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if (expr.lval) {
			expr = rvalify(expr);
		}
		co.code.addAll(expr.getCode());
		Instruction i = new Malloc(expr.temp, generateTemp(Type.PTR));
		co.code.add(i);
		co.type = Type.INFER;
		co.temp = i.getDest();
		co.lval = false;

		return co;
	}
	
	/**
	 * Generate code for free
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new FREE instruction
	 */
	@Override
	protected CodeObject postprocess(FreeNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if (expr.lval) {
			expr = rvalify(expr);
		}
		co.code.addAll(expr.getCode());
		Instruction i = new Free(expr.temp);
		co.code.add(i);
		co.type = Type.VOID;
		co.temp = i.getDest();
		co.lval = false;

		return co;
	}

	@Override
	protected CodeObject postprocess(TypeCastNode node, CodeObject expr) {
		CodeObject co = new CodeObject();
		if (expr.lval) {
			expr = rvalify(expr);
		}
		co.code.addAll(expr.code);
		Instruction i = null;
		if (node.getType() == Type.INT) {
			i = new FMovi(expr.temp, generateTemp(Type.INT));
		} else {
			i = new IMovf(expr.temp, generateTemp(Type.FLOAT));
		}
		co.code.add(i);
		co.type = node.getType();
		co.temp = i.getDest();
		co.lval = false;
		return co;
	}

	public Instruction cast(CodeObject lco) {
		Instruction i = null;
		if (lco.type == Type.INT) {
			i = new IMovf(lco.temp, generateTemp(Type.FLOAT));
			lco.type = Type.FLOAT;
		} else if (lco.type == Type.FLOAT) {
			i = new FMovi(lco.temp, generateTemp(Type.INT));
			lco.type = Type.INT;
		} else {
			throw new Error("NOT INT OR FLOAT");
		}
		lco.temp = i.getDest();
		return i;
	}

	/**
	 * Generate a fresh temporary
	 * 
	 * @return new temporary register name
	 */
	protected String generateTemp(Scope.Type t) {
		switch(t) {
			case INT: 
			case PTR: //works the same for pointers
				return intTempPrefix + String.valueOf(++intRegCount);
			case FLOAT: return floatTempPrefix + String.valueOf(++floatRegCount);
			default: throw new Error("Generating temp for bad type");
		}
	}

	protected String generateLoopLabel() {
		return "loop_" + String.valueOf(++loopLabel);
	}

	protected String generateElseLabel() {
		return  "else_" + String.valueOf(++elseLabel);
	}

	protected String generateOutLabel() {
		return "out_" +  String.valueOf(++outLabel);
	}

	protected String generateFunctionLabel() {
		return "func_" + currFunc;
	}

	protected String generateFunctionLabel(String func) {
		return "func_" + func;
	}

	protected String generateFunctionOutLabel() {
		return "func_ret_" + currFunc;
	}
	
	/**
	 * Take a code object that results in an lval, and create a new code
	 * object that adds a load to generate the rval.
	 * 
	 * @param lco The code object resulting in an address
	 * @return A code object with all the code of <code>lco</code> followed by a load
	 *         to generate an rval
	 */
	protected CodeObject rvalify(CodeObject lco) {
		
		assert (lco.lval == true);
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 2 */
		co.code.addAll(lco.code);
		Instruction i = null;
		if (lco.getType() == Scope.Type.INT || lco.getType() == Scope.Type.PTR) {
			if (lco.getSTE() != null) {
				String address = lco.getSTE().addressToString();
				if (lco.getSTE().isLocal()) {
					i = new Lw(generateTemp(Scope.Type.INT), "fp", address);
				} else {
					Instruction compAddr = new La(generateTemp(Scope.Type.INT), address);
					co.code.add(compAddr);
					i = new Lw(generateTemp(Scope.Type.INT), compAddr.getDest(), "0");
				}
			} else {
				i = new Lw(generateTemp(Scope.Type.INT), lco.temp, "0");
			}
		} else if (lco.getType() == Scope.Type.FLOAT) {
			if (lco.getSTE() != null) {
				String address = lco.getSTE().addressToString();
				if (lco.getSTE().isLocal()) {
					i = new Flw(generateTemp(Scope.Type.FLOAT), "fp", address);
				} else {
					Instruction compAddr = new La(generateTemp(Scope.Type.INT), address);
					co.code.add(compAddr);
					i = new Flw(generateTemp(Scope.Type.FLOAT), compAddr.getDest(), "0");
				}
			} else {
				i = new Flw(generateTemp(Type.FLOAT), lco.temp, "0");
			}
		} 
		else {
			throw new Error ("Not INT or FLOAT");
		}
		co.temp = i.getDest();
		co.lval = false;
		co.type = lco.getType();
		co.code.add(i);
		return co;
	}

	/**
	 * Generate an instruction sequence that holds the address of the variable in a code object
	 * 
	 * If it's a global variable, just get the address from the symbol table
	 * 
	 * If it's a local variable, compute the address relative to the frame pointer (fp)
	 * 
	 * @param lco The code object holding a variable
	 * @return a list of instructions that puts the address of the variable in a register
	 */
	private InstructionList generateAddrFromVariable(CodeObject lco) {

		InstructionList il = new InstructionList();

		//Step 1:
		SymbolTableEntry symbol = lco.getSTE();
		String address = symbol.addressToString();

		//Step 2:
		Instruction compAddr = null;
		if (symbol.isLocal()) {
			//If local, address is offset
			//need to load fp + offset
			//addi tmp' fp offset
			compAddr = new Addi("fp", address, generateTemp(Scope.Type.INT));
			lco.temp = compAddr.getDest();
		} else {
			//If global, address in symbol table is the right location
			//la tmp' addr //Register type needs to be an int
			compAddr = new La(generateTemp(Scope.Type.INT), address);
			lco.temp = compAddr.getDest();
		}
		il.add(compAddr); //add instruction to code object
		lco.lval = true; //co holds an lval, because it's an address
		lco.ste = null; //not a variable
		lco.type = symbol.getType(); //even though register type is an int, address points to Type

		return il;
	}

}
