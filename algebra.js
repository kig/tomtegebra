// Algebra.js - Port of Algebra.hs
// Handles expression manipulation and rules

// Expression types
class Expr {
    constructor(type, ...args) {
        this.type = type;
        this.args = args;
    }

    equals(other) {
        if (!(other instanceof Expr)) return false;
        if (this.type !== other.type) return false;
        if (this.args.length !== other.args.length) return false;
        return this.args.every((arg, i) => {
            if (arg instanceof Expr) return arg.equals(other.args[i]);
            return arg === other.args[i];
        });
    }

    toString() {
        switch(this.type) {
            case 'Op':
                return `(${this.args[1].toString()} ${this.args[0]} ${this.args[2].toString()})`;
            case 'A': return 'a';
            case 'B': return 'b';
            case 'C': return 'c';
            case 'X': return 'x';
            case 'Y': return 'y';
            case 'Z': return 'z';
            case 'Literal': return this.args[0].toString();
            case 'Inv': return `inv(${this.args[0]},${this.args[1].toString()})`;
            case 'Neutral': return `e(${this.args[0]})`;
            default: return '';
        }
    }

    clone() {
        const clonedArgs = this.args.map(arg => 
            arg instanceof Expr ? arg.clone() : arg
        );
        return new Expr(this.type, ...clonedArgs);
    }
}

// Factory functions for expressions
const A = new Expr('A');
const B = new Expr('B');
const C = new Expr('C');
const X = new Expr('X');
const Y = new Expr('Y');
const Z = new Expr('Z');

function Op(op, left, right) {
    return new Expr('Op', op, left, right);
}

function Literal(n) {
    return new Expr('Literal', n);
}

function Inv(op, expr) {
    return new Expr('Inv', op, expr);
}

function Neutral(op) {
    return new Expr('Neutral', op);
}

// Rule class
class Rule {
    constructor(left, right) {
        this.left = left;
        this.right = right;
    }

    equals(other) {
        return other instanceof Rule && 
               this.left.equals(other.left) && 
               this.right.equals(other.right);
    }

    toString() {
        return `${this.left.toString()} = ${this.right.toString()}`;
    }

    reverse() {
        return new Rule(this.right, this.left);
    }

    toExpr() {
        return Op("=", this.left, this.right);
    }

    length() {
        return this.left.length() + 1 + this.right.length();
    }
}

// Expression manipulation
Expr.prototype.length = function() {
    if (this.type === 'Op') {
        return 1 + this.args[1].length() + this.args[2].length();
    }
    return 1;
};

// Pattern matching and binding
class Binding {
    constructor() {
        this.bindings = { A: null, B: null, C: null, X: null, Y: null, Z: null };
        this.invalid = false;
    }

    bind(varName, expr) {
        if (this.invalid) return this;
        
        if (this.bindings[varName] === null) {
            this.bindings[varName] = expr;
        } else if (!this.bindings[varName].equals(expr)) {
            this.invalid = true;
        }
        return this;
    }

    apply(expr) {
        if (this.invalid) return null;
        return this.applyToExpr(expr);
    }

    applyToExpr(expr) {
        if (expr.type in this.bindings && this.bindings[expr.type] !== null) {
            return this.bindings[expr.type].clone();
        }
        if (expr.type === 'Op') {
            return Op(expr.args[0], 
                     this.applyToExpr(expr.args[1]), 
                     this.applyToExpr(expr.args[2]));
        }
        if (expr.type === 'Inv') {
            return Inv(expr.args[0], this.applyToExpr(expr.args[1]));
        }
        return expr.clone();
    }
}

function matchPattern(pattern, expr) {
    const binding = new Binding();
    return matchPatternHelper(pattern, expr, binding) ? binding : null;
}

function matchPatternHelper(pattern, expr, binding) {
    if (binding.invalid) return false;

    if (['A', 'B', 'C', 'X', 'Y', 'Z'].includes(pattern.type)) {
        binding.bind(pattern.type, expr);
        return !binding.invalid;
    }

    if (pattern.type === 'Literal') {
        return expr.type === 'Literal' && pattern.args[0] === expr.args[0];
    }

    if (pattern.type === 'Neutral') {
        return expr.type === 'Neutral' && pattern.args[0] === expr.args[0];
    }

    if (pattern.type === 'Inv') {
        return expr.type === 'Inv' && 
               pattern.args[0] === expr.args[0] &&
               matchPatternHelper(pattern.args[1], expr.args[1], binding);
    }

    if (pattern.type === 'Op') {
        return expr.type === 'Op' && 
               pattern.args[0] === expr.args[0] &&
               matchPatternHelper(pattern.args[1], expr.args[1], binding) &&
               matchPatternHelper(pattern.args[2], expr.args[2], binding);
    }

    return false;
}

// Apply rule to expression
function applyRule(rule, expr) {
    const binding = matchPattern(rule.left, expr);
    if (binding && !binding.invalid) {
        return binding.apply(rule.right);
    }
    return expr.clone();
}

function applyEquality(rule, expr) {
    let result = applyRule(rule, expr);
    if (result.equals(expr)) {
        result = applyRule(rule.reverse(), expr);
    }
    return result;
}

// Apply rule at a specific index
function applyEqualityAt(idx, rule, expr) {
    let currentIdx = 0;
    
    function applyAtIndex(e) {
        if (e.type === 'Op') {
            const leftResult = applyAtIndex(e.args[1]);
            const leftIdx = currentIdx;
            
            if (currentIdx === idx) {
                currentIdx++;
                return applyEquality(rule, e);
            }
            currentIdx++;
            
            const rightResult = applyAtIndex(e.args[2]);
            return Op(e.args[0], leftResult, rightResult);
        } else {
            if (currentIdx === idx) {
                currentIdx++;
                return applyEquality(rule, e);
            }
            currentIdx++;
            return e.clone();
        }
    }
    
    return applyAtIndex(expr);
}

// Get subexpression at index
function subExprAt(idx, expr) {
    let currentIdx = 0;
    
    function findAtIndex(e) {
        if (e.type === 'Op') {
            const leftResult = findAtIndex(e.args[1]);
            if (leftResult) return leftResult;
            
            if (currentIdx === idx) {
                currentIdx++;
                return e;
            }
            currentIdx++;
            
            return findAtIndex(e.args[2]);
        } else {
            if (currentIdx === idx) {
                currentIdx++;
                return e;
            }
            currentIdx++;
            return null;
        }
    }
    
    return findAtIndex(expr);
}

// Check if rule is true
function isTrue(rule) {
    return rule.left.equals(rule.right);
}

function isBinding(expr, rule) {
    return isTrue(rule) || rule.left.equals(expr) || rule.right.equals(expr);
}

// Replace ABC with XYZ
function replaceABCwithXYZ(rule) {
    function replace(expr) {
        if (expr.type === 'A') return X;
        if (expr.type === 'B') return Y;
        if (expr.type === 'C') return Z;
        if (expr.type === 'Op') {
            return Op(expr.args[0], replace(expr.args[1]), replace(expr.args[2]));
        }
        if (expr.type === 'Inv') {
            return Inv(expr.args[0], replace(expr.args[1]));
        }
        return expr.clone();
    }
    
    return new Rule(replace(rule.left), replace(rule.right));
}

// Group axioms
function associativity(op) {
    const left = Op(op, A, Op(op, B, C));
    const right = Op(op, Op(op, A, B), C);
    return { predicate: 'bothEqual', rule: new Rule(left, right) };
}

function rightNeutral(op) {
    const left = Op(op, A, Neutral(op));
    const right = A;
    return { predicate: 'bindNeutral', rule: new Rule(left, right), expr: Neutral(op) };
}

function leftNeutral(op) {
    const left = Op(op, Neutral(op), A);
    const right = A;
    return { predicate: 'bindNeutral', rule: new Rule(left, right), expr: Neutral(op) };
}

function rightInverse(op) {
    const left = Op(op, A, Inv(op, A));
    const right = Neutral(op);
    return { predicate: 'bindInverse', rule: new Rule(left, right), expr: Inv(op, A) };
}

function leftInverse(op) {
    const left = Op(op, Inv(op, A), A);
    const right = Neutral(op);
    return { predicate: 'bindInverse', rule: new Rule(left, right), expr: Inv(op, A) };
}

function commutativity(op) {
    const left = Op(op, A, B);
    const right = Op(op, B, A);
    return { predicate: 'bothEqual', rule: new Rule(left, right) };
}

// Build Abelian group axioms
function abelianGroup(op) {
    return [
        associativity(op),
        rightNeutral(op),
        leftNeutral(op),
        rightInverse(op),
        leftInverse(op),
        commutativity(op)
    ];
}

// Equality transforms
function eqTrans(op, c) {
    const left = Op("=", Op(op, X, c), Op(op, Y, c));
    const right = Op("=", X, Y);
    return new Rule(left, right);
}

function getUniqueOps(expr) {
    const ops = new Set();
    
    function collect(e) {
        if (e.type === 'Op') {
            ops.add(e.args[0]);
            collect(e.args[1]);
            collect(e.args[2]);
        }
    }
    
    collect(expr);
    return Array.from(ops);
}

function getUniqueVariables(expr) {
    const vars = new Set();
    
    function collect(e) {
        if (['A', 'B', 'C', 'X', 'Y', 'Z'].includes(e.type)) {
            vars.add(e);
        } else if (e.type === 'Op') {
            collect(e.args[1]);
            collect(e.args[2]);
        } else if (e.type === 'Inv') {
            collect(e.args[1]);
        }
    }
    
    collect(expr);
    return Array.from(vars);
}

function equalityTransforms(expr) {
    if (expr.type !== 'Op' || expr.args[0] !== '=') return [];
    
    const transforms = [];
    const ops = getUniqueOps(expr).filter(op => op !== '=');
    const vars = getUniqueVariables(expr);
    
    for (const op of ops) {
        for (const v of vars) {
            transforms.push(eqTrans(op, v));
            if (v.type !== 'Inv') {
                transforms.push(eqTrans(op, Inv(op, v)));
            }
        }
    }
    
    return transforms;
}

// Find matching equalities
function findMatchingEqualities(expr, inventory) {
    return inventory.filter(rule => {
        const binding1 = matchPattern(rule.left, expr);
        const binding2 = matchPattern(rule.right, expr);
        return (binding1 && !binding1.invalid) || (binding2 && !binding2.invalid);
    });
}

function inventoryFor(idx, checkableRule, inventory) {
    const ruleExpr = checkableRule.rule.toExpr();
    const subExpr = subExprAt(idx, ruleExpr);
    
    if (!subExpr) return [];
    
    if (subExpr.type === 'Op' && subExpr.args[0] === '=') {
        return equalityTransforms(subExpr);
    }
    
    return findMatchingEqualities(subExpr, inventory);
}
