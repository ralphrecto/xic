package edu.cornell.cs.cs4120.xic.ir.interpret;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Stack;

import edu.cornell.cs.cs4120.util.InternalCompilerError;
import edu.cornell.cs.cs4120.xic.ir.IRBinOp;
import edu.cornell.cs.cs4120.xic.ir.IRCJump;
import edu.cornell.cs.cs4120.xic.ir.IRCall;
import edu.cornell.cs.cs4120.xic.ir.IRCompUnit;
import edu.cornell.cs.cs4120.xic.ir.IRConst;
import edu.cornell.cs.cs4120.xic.ir.IRExp;
import edu.cornell.cs.cs4120.xic.ir.IRFuncDecl;
import edu.cornell.cs.cs4120.xic.ir.IRJump;
import edu.cornell.cs.cs4120.xic.ir.IRMem;
import edu.cornell.cs.cs4120.xic.ir.IRMove;
import edu.cornell.cs.cs4120.xic.ir.IRName;
import edu.cornell.cs.cs4120.xic.ir.IRNode;
import edu.cornell.cs.cs4120.xic.ir.IRReturn;
import edu.cornell.cs.cs4120.xic.ir.IRTemp;
import edu.cornell.cs.cs4120.xic.ir.visit.InsnMapsBuilder;

/**
 * A simple IR interpreter
 *
 * The interpreter makes the following assumption about registers.
 * Registers whose names begin with {@link Configuration#ABSTRACT_ARG_PREFIX}
 * and {@link Configuration#ABSTRACT_RET_PREFIX} are shared across function
 * calls.  IR code are responsible for saving the value of these registers
 * before CALL is executed.
 */
public class IRSimulator {
    /** compilation unit to be interpreted */
    private IRCompUnit compUnit;

    /** map from address to instruction */
    protected Map<Long, IRNode> indexToInsn;
    protected Map<IRNode, Long> insnToIndex;

    /** map from labeled name to address */
    private Map<String, Long> nameToIndex;

    /** a random number generator for initializing garbage */
    protected Random r;

    /** global registers (register name -> value) */
    private Map<String, Long> regs;

    /** heap */
    private long mem[];

    /** heap pointer to lowest unallocated region */
    private long heapPtr;

    private ExprStack exprStack;
    private BufferedReader inReader;

    private Set<String> libraryFunctions;
    private List<String> ctors;

    protected static int debugLevel = 0;

    public static final int DEFAULT_HEAP_SIZE = 20480;

    /**
     * Construct an IR interpreter with a default heap size
     * @param compUnit the compilation unit to be interpreted
     */
    public IRSimulator(IRCompUnit compUnit) {
        this(compUnit, DEFAULT_HEAP_SIZE);
    }

    /**
     * Construct an IR interpreter
     * @param compUnit the compilation unit to be interpreted
     * @param heapSize the heap size
     */
    public IRSimulator(IRCompUnit compUnit, int heapSize) {
        this.compUnit = compUnit;

        r = new Random();

        regs = new HashMap<>();

        mem = new long[heapSize];
        // initialize heap to garbage
        for (int i = 0; i < heapSize; i++)
            mem[i] = r.nextLong();
        // initialize heap pointer
        heapPtr = 0;

        exprStack = new ExprStack();
        inReader = new BufferedReader(new InputStreamReader(System.in));

        libraryFunctions = new LinkedHashSet<>();
        // io declarations
        libraryFunctions.add("_Iprint_pai");
        libraryFunctions.add("_Iprintln_pai");
        libraryFunctions.add("_Ireadln_ai");
        libraryFunctions.add("_Igetchar_i");
        libraryFunctions.add("_Ieof_b");
        // conv declarations
        libraryFunctions.add("_IparseInt_t2ibai");
        libraryFunctions.add("_IunparseInt_aii");
        // special declarations
        libraryFunctions.add("_I_alloc_i");
        libraryFunctions.add("_I_outOfBounds_p");
        // other declarations
        libraryFunctions.add("_Iassert_pb");

        InsnMapsBuilder imb = new InsnMapsBuilder();
        compUnit = (IRCompUnit) imb.visit(compUnit);
        indexToInsn = imb.indexToInsn();
        insnToIndex = imb.insnToIndex();
        nameToIndex = imb.nameToIndex();
        ctors = imb.ctors();

        for (int i = 0; i < ctors.size(); ++i)
            call(ctors.get(i), new long[] {});
    }

    /**
     * Fetch the value at the given register
     * @param frame the current execution frame
     * @param tempName name of the register
     * @return the value at the given register
     */
    public long get(ExecutionFrame frame, String tempName) {
        if (isGlobalRegister(tempName)) {
            if (!regs.containsKey(tempName)) {
                /* Referencing a temp before having written to it - initialize
                   with garbage */
                regs.put(tempName, r.nextLong());
            }
            return regs.get(tempName);
        }
        else return frame.get(tempName);
    }

    /**
     * Store a value into the given register
     * @param frame the current execution frame
     * @param tempName name of the register
     * @param value value to be stored
     */
    public void put(ExecutionFrame frame, String tempName, long value) {
        if (isGlobalRegister(tempName))
            regs.put(tempName, value);
        else frame.put(tempName, value);
    }

    protected boolean isGlobalRegister(String name) {
        return name.startsWith(Configuration.ABSTRACT_ARG_PREFIX)
                || name.startsWith(Configuration.ABSTRACT_RET_PREFIX);
    }

    /**
     * Allocate a specified amount of bytes on the heap
     * @param size the number of bytes to be allocated
     * @return the starting address of the allocated region on the heap
     */
    public long malloc(long size) {
        if (size < 0) throw new Trap("Invalid size");
        if (size % Configuration.WORD_SIZE != 0)
            throw new Trap("Can only allocate in chunks of "
                    + Configuration.WORD_SIZE + " bytes!");

        long retval = heapPtr;
        heapPtr += size;
        return retval;
    }

    /**
     * Read a value at the specified location on the heap
     * @param addr the address to be read
     * @return the value at {@code addr}
     */
    public long read(long addr) {
        if (addr % Configuration.WORD_SIZE != 0)
            throw new Trap("Unaligned memory access!");
        return mem[(int) (addr / Configuration.WORD_SIZE)];
    }

    /**
     * Write a value at the specified location on the heap
     * @param addr the address to be written
     * @param value the value to be written
     */
    public void store(long addr, long value) {
        if (addr % Configuration.WORD_SIZE != 0)
            throw new Trap("Unaligned memory access!");
        mem[(int) (addr / Configuration.WORD_SIZE)] = value;
    }

    /**
     * Simulate a function call.
     * All arguments to the function call are passed via registers with prefix
     * {@link Configuration#ABSTRACT_ARG_PREFIX} and indices starting from 0.
     * The function call should return the results via registers with prefix
     * {@link Configuration#ABSTRACT_RET_PREFIX} and indices starting from 0.
     * @param name name of the function call
     * @param args arguments to the function call
     * @return the value of register
     *          {@link Configuration#ABSTRACT_RET_PREFIX} index 0
     */
    public long call(String name, long... args) {
        // Catch standard library calls.
        if (libraryFunctions.contains(name)) return libraryCall(name, args);

        IRFuncDecl fDecl = compUnit.getFunction(name);
        if (fDecl == null)
            throw new InternalCompilerError("Tried to call an unknown function: '"
                    + name + "'");

        // Create a new stack frame.
        ExecutionFrame frame = new ExecutionFrame(fDecl);

        // Pass the remaining arguments into registers.
        for (int i = 0; i < args.length; ++i)
            put(frame, Configuration.ABSTRACT_ARG_PREFIX + i, args[i]);

        // Simulate!
        while (frame.advance());

        return get(frame, Configuration.ABSTRACT_RET_PREFIX + 0);
    }

    /**
     * Simulate a library function call
     * @param name name of the function call
     * @param args arguments to the function call, which may include
     *          the pointer to the location of multiple results
     * @return the address of the result
     */
    protected long libraryCall(String name, long[] args) {
        final int ws = Configuration.WORD_SIZE;
        try {
            switch (name) {
            // io declarations
            case "_Iprint_pai": {
                long ptr = args[0], size = read(ptr - ws);
                for (long i = 0; i < size; ++i)
                    System.out.print((char) read(ptr + i * ws));
                return 0;
            }
            case "_Iprintln_pai": {
                long ptr = args[0], size = read(ptr - ws);
                for (long i = 0; i < size; ++i)
                    System.out.print((char) read(ptr + i * ws));
                System.out.println();
                return 0;
            }
            case "_Ireadln_ai": {
                String line = inReader.readLine();
                int len = line.length();
                long ptr = malloc((len + 1) * ws);
                store(ptr, len);
                for (int i = 0; i < len; ++i)
                    store(ptr + (i + 1) * ws, line.charAt(i));
                return ptr + ws;
            }
            case "_Igetchar_i": {
                return inReader.read();
            }
            case "_Ieof_b": {
                return inReader.ready() ? 0 : 1;
            }
            // conv declarations
            case "_IunparseInt_aii": {
                String line = String.valueOf(args[0]);
                int len = line.length();
                long ptr = malloc((len + 1) * ws);
                store(ptr, len);
                for (int i = 0; i < len; ++i)
                    store(ptr + (i + 1) * ws, line.charAt(i));
                return ptr + ws;
            }
            case "_IparseInt_t2ibai": {
                StringBuffer buf = new StringBuffer();
                long ptr = args[0], size = read(ptr - ws);
                for (int i = 0; i < size; ++i)
                    buf.append((char) read(ptr + i * ws));
                int result = 0, success = 1;
                try {
                    result = Integer.parseInt(buf.toString());
                }
                catch (NumberFormatException e) {
                    success = 0;
                }
                put(null, Configuration.ABSTRACT_RET_PREFIX + 0, result);
                put(null, Configuration.ABSTRACT_RET_PREFIX + 1, success);
                return result;
            }
            // special declarations
            case "_I_alloc_i": {
                return malloc(args[0]);
            }
            case "_I_outOfBounds_p": {
                throw new Trap("Out of bounds!");
            }
            // other declarations
            case "_Iassert_pb": {
                if (args[0] != 1) throw new Trap("Assertion error!");
                return 0;
            }
            default:
                throw new InternalCompilerError("Unsupported library function: "
                        + name);
            }
        }
        catch (IOException e) {
            throw new InternalCompilerError("I/O Exception in simulator");
        }
    }

    protected void leave(ExecutionFrame frame) {
        if (frame.ip instanceof IRConst)
            exprStack.pushValue(((IRConst) frame.ip).value());
        else if (frame.ip instanceof IRTemp) {
            String tempName = ((IRTemp) frame.ip).name();
            exprStack.pushTemp(get(frame, tempName), tempName);
        }
        else if (frame.ip instanceof IRBinOp) {
            long r = exprStack.popValue();
            long l = exprStack.popValue();
            long result;
            switch (((IRBinOp) frame.ip).opType()) {
            case ADD:
                result = l + r;
                break;
            case SUB:
                result = l - r;
                break;
            case MUL:
                result = l * r;
                break;
            case HMUL:
                result = BigInteger.valueOf(l)
                                   .multiply(BigInteger.valueOf(r))
                                   .shiftRight(64)
                                   .longValue();
                break;
            case DIV:
                if (r == 0) throw new Trap("Division by zero!");
                result = l / r;
                break;
            case MOD:
                if (r == 0) throw new Trap("Division by zero!");
                result = l % r;
                break;
            case AND:
                result = l & r;
                break;
            case OR:
                result = l | r;
                break;
            case XOR:
                result = l ^ r;
                break;
            case LSHIFT:
                result = l << r;
                break;
            case RSHIFT:
                result = l >>> r;
                break;
            case ARSHIFT:
                result = l >> r;
                break;
            case EQ:
                result = l == r ? 1 : 0;
                break;
            case NEQ:
                result = l != r ? 1 : 0;
                break;
            case LT:
                result = l < r ? 1 : 0;
                break;
            case GT:
                result = l > r ? 1 : 0;
                break;
            case LEQ:
                result = l <= r ? 1 : 0;
                break;
            case GEQ:
                result = l >= r ? 1 : 0;
                break;
            default:
                throw new InternalCompilerError("Invalid binary operation");
            }
            exprStack.pushValue(result);
        }
        else if (frame.ip instanceof IRMem) {
            long addr = exprStack.popValue();
            if (addr % Configuration.WORD_SIZE != 0)
                throw new Trap("Unaligned memory access: " + addr
                        + " (word size=" + Configuration.WORD_SIZE + ")");
            addr /= Configuration.WORD_SIZE;
            exprStack.pushAddr(mem[(int) addr], addr);
        }
        else if (frame.ip instanceof IRCall) {
            int argsCount = ((IRCall) frame.ip).args().size();
            long args[] = new long[argsCount];
            for (int i = argsCount - 1; i >= 0; --i)
                args[i] = exprStack.popValue();
            StackItem target = exprStack.pop();
            String targetName = target.name;
            if (target.type != StackItem.Kind.NAME) {
                if (indexToInsn.containsKey(target.value)) {
                    IRNode node = indexToInsn.get(target.value);
                    if (node instanceof IRFuncDecl)
                        targetName = ((IRFuncDecl) node).name();
                    else throw new InternalCompilerError("Call to a non-function instruction!");
                }
                else throw new InternalCompilerError("Invalid function call '"
                        + frame.ip + "' (target '" + target.value
                        + "' is unknown)!");
            }

            long retVal = call(targetName, args);
            exprStack.pushValue(retVal);
        }
        else if (frame.ip instanceof IRName) {
            String name = ((IRName) frame.ip).name();
            if (libraryFunctions.contains(name))
                exprStack.pushName(-1, name);
            else if (nameToIndex.containsKey(name))
                exprStack.pushName(nameToIndex.get(name), name);
            else throw new InternalCompilerError("Invalid destination in NAME: '"
                    + name + "'");
        }
        else if (frame.ip instanceof IRMove) {
            long r = exprStack.popValue();
            StackItem stackItem = exprStack.pop();
            if (stackItem.type == StackItem.Kind.MEM) {
                if (debugLevel > 0) System.out.println("mem["
                        + stackItem.addr * Configuration.WORD_SIZE + "]=" + r);
                mem[(int) stackItem.addr] = r;
            }
            else if (stackItem.type == StackItem.Kind.TEMP) {
                if (debugLevel > 0)
                    System.out.println("temp[" + stackItem.temp + "]=" + r);
                put(frame, stackItem.temp, r);
            }
            else throw new InternalCompilerError("Invalid MOVE!");
        }
        else if (frame.ip instanceof IRExp)
            // Discard result.
            exprStack.pop();
        else if (frame.ip instanceof IRJump)
            frame.setIP(indexToInsn.get(exprStack.popValue()));
        else if (frame.ip instanceof IRCJump) {
            IRCJump irCJump = (IRCJump) frame.ip;
            long top = exprStack.popValue();
            String label;
            if (top == 0)
                label = irCJump.falseLabel();
            else if (top == 1)
                label = irCJump.trueLabel();
            else throw new InternalCompilerError("Invalid value in CJUMP - expected 0/1, got "
                    + top);
            if (label != null) frame.setIP(findLabel(label));
        }
        else if (frame.ip instanceof IRReturn) frame.setIP(null);
    }

    /**
     *
     * @param name name of the label
     * @return the IR node at the named label
     */
    private IRNode findLabel(String name) {
        if (!nameToIndex.containsKey(name))
            throw new Trap("Could not find label '" + name + "'!");
        return indexToInsn.get(nameToIndex.get(name));
    }

    /**
     * Holds the instruction pointer and temporary registers
     * within an execution frame.
     */
    private class ExecutionFrame {
        /** instruction pointer */
        public IRNode ip;

        /** local registers (register name -> value) */
        private Map<String, Long> regs;

        public ExecutionFrame(IRNode ip) {
            this.ip = ip;
            regs = new HashMap<>();
        }

        /**
         * Fetch the value at the given register
         * @param tempName name of the register
         * @return the value at the given register
         */
        public long get(String tempName) {
            if (!regs.containsKey(tempName)) {
                /* Referencing a temp before having written to it - initialize
                   with garbage */
                put(tempName, r.nextLong());
            }
            return regs.get(tempName);
        }

        /**
         * Store a value into the given register
         * @param tempName name of the register
         * @param value value to be stored
         */
        public void put(String tempName, long value) {
            regs.put(tempName, value);
        }

        /**
         * Advance the instruction pointer. Since we're dealing with a tree,
         * this is postorder traversal, one step at a time, modulo jumps.
         */
        public boolean advance() {
            long index = insnToIndex.get(ip);
            if (debugLevel > 1) System.out.println("Evaluating " + ip.label());
            IRNode backupIP = ip;
            leave(this);

            if (ip == null) return false; /* RETURN */

            if (ip != backupIP) /* A jump was performed */
                return true;

            ip = indexToInsn.get(index + 1);
            if (ip == null)
                throw new Trap("No next instruction.  Forgot RETURN?");
            return true;
        }

        public void setIP(IRNode node) {
            if (debugLevel > 1)
                System.out.println("Jumping to " + node.label());
            ip = node;
        }
    };

    /**
     * While traversing the IR tree, we require a stack in order to hold
     * a number of single-word values (e.g. to evaluate binary expressions).
     * This also keeps track of whether a value was created by a TEMP
     * or MEM, or NAME reference, which is useful when executing moves.
     */
    private static class ExprStack {

        private Stack<StackItem> stack;

        public ExprStack() {
            stack = new Stack<>();
        }

        public long popValue() {
            if (debugLevel > 1)
                System.out.println("Popping value " + stack.peek().value);
            return stack.pop().value;
        }

        public StackItem pop() {
            return stack.pop();
        }

        public void pushAddr(long value, long addr) {
            if (debugLevel > 1)
                System.out.println("Pushing MEM " + value + " (" + addr + ")");
            stack.push(new StackItem(value, addr));
        }

        public void pushTemp(long value, String temp) {
            if (debugLevel > 1)
                System.out.println("Pushing TEMP " + value + " (" + temp + ")");
            stack.push(new StackItem(StackItem.Kind.TEMP, value, temp));
        }

        public void pushName(long value, String name) {
            if (debugLevel > 1)
                System.out.println("Pushing NAME " + value + " (" + name + ")");
            stack.push(new StackItem(StackItem.Kind.NAME, value, name));
        }

        public void pushValue(long value) {
            if (debugLevel > 1) System.out.println("Pushing value " + value);
            stack.push(new StackItem(value));
        }
    }

    public static class StackItem {
        public enum Kind {
            COMPUTED, MEM, TEMP, NAME;
        }

        public Kind type;
        public long value;
        public long addr;
        public String temp;
        public String name;

        public StackItem(long value) {
            type = Kind.COMPUTED;
            this.value = value;
        }

        public StackItem(long value, long addr) {
            type = Kind.MEM;
            this.value = value;
            this.addr = addr;
        }

        public StackItem(Kind type, long value, String string) {
            this.type = type;
            this.value = value;
            if (type == Kind.TEMP)
                temp = string;
            else name = string;
        }
    };

    public static class Trap extends RuntimeException {
        private static final long serialVersionUID = 8429929900405296472L;

        public Trap(String message) {
            super(message);
        }
    };
}
