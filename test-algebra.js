/**
 * Test harness for algebra.js
 * Verifies JavaScript implementation matches Haskell Algebra.hs
 * Can run in browser or Node.js
 * No dependencies required
 */

// Import algebra.js functions if in Node.js
if (typeof module !== 'undefined' && module.exports) {
    // Running in Node.js - would need to require algebra.js
    // For now, assume it's included via script tag in browser
}

// ============================================================================
// Random Expression Generator (QuickCheck style)
// ============================================================================

class Random {
    constructor(seed = Date.now()) {
        this.seed = seed;
    }
    
    // Linear congruential generator
    next() {
        this.seed = (this.seed * 1664525 + 1013904223) % 4294967296;
        return this.seed / 4294967296;
    }
    
    nextInt(max) {
        return Math.floor(this.next() * max);
    }
    
    choice(arr) {
        return arr[this.nextInt(arr.length)];
    }
}

function generateRandomExpr(rng, depth = 0, maxDepth = 3) {
    // Haskell source: data Expr = Expr (Op, Expr, Expr) | A | B | C | X | Y | Z | Inv (Op, Expr) | Neutral Op | Literal Int
    
    if (depth >= maxDepth) {
        // Generate leaf nodes only
        const leafTypes = ['A', 'B', 'C', 'X', 'Y', 'Z', 'Literal', 'Neutral'];
        const type = rng.choice(leafTypes);
        
        if (type === 'Literal') {
            return Literal(rng.nextInt(10));
        } else if (type === 'Neutral') {
            const op = rng.choice(['+', 'o', 'f', 'x']);
            return Neutral(op);
        } else {
            return new Expr(type);
        }
    }
    
    // Can generate either leaf or branch
    const allTypes = ['A', 'B', 'C', 'X', 'Y', 'Z', 'Literal', 'Neutral', 'Op', 'Inv'];
    const type = rng.choice(allTypes);
    
    if (type === 'Op') {
        const op = rng.choice(['+', 'o', 'f', 'x', '=']);
        const left = generateRandomExpr(rng, depth + 1, maxDepth);
        const right = generateRandomExpr(rng, depth + 1, maxDepth);
        return Op(op, left, right);
    } else if (type === 'Inv') {
        const op = rng.choice(['+', 'o', 'f', 'x']);
        const expr = generateRandomExpr(rng, depth + 1, maxDepth);
        return Inv(op, expr);
    } else if (type === 'Literal') {
        return Literal(rng.nextInt(10));
    } else if (type === 'Neutral') {
        const op = rng.choice(['+', 'o', 'f', 'x']);
        return Neutral(op);
    } else {
        return new Expr(type);
    }
}

function generateRandomRule(rng, maxDepth = 2) {
    const left = generateRandomExpr(rng, 0, maxDepth);
    const right = generateRandomExpr(rng, 0, maxDepth);
    return new Rule(left, right);
}

// ============================================================================
// Test Functions with Haskell Source Comments
// ============================================================================

function testExprEquals() {
    // Haskell source:
    // instance Eq Expr where
    //     Expr a == Expr b = a == b
    //     A == A = True
    //     B == B = True
    //     ...
    //     _ == _ = False
    
    const tests = [
        { expr1: A, expr2: A, expected: true, name: "A equals A" },
        { expr1: A, expr2: B, expected: false, name: "A not equals B" },
        { expr1: Literal(5), expr2: Literal(5), expected: true, name: "Literal 5 equals Literal 5" },
        { expr1: Literal(5), expr2: Literal(3), expected: false, name: "Literal 5 not equals Literal 3" },
        { expr1: Op("+", A, B), expr2: Op("+", A, B), expected: true, name: "Op equals" },
        { expr1: Op("+", A, B), expr2: Op("+", B, A), expected: false, name: "Op not equals (different order)" },
        { expr1: Neutral("+"), expr2: Neutral("+"), expected: true, name: "Neutral equals" },
        { expr1: Neutral("+"), expr2: Neutral("o"), expected: false, name: "Neutral not equals (different op)" },
        { expr1: Inv("+", A), expr2: Inv("+", A), expected: true, name: "Inv equals" },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = test.expr1.equals(test.expr2);
        if (result === test.expected) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Expected: ${test.expected}, Got: ${result}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testMatchPattern() {
    // Haskell source:
    // matchPattern :: Pattern -> Expr -> Bool
    // matchPattern pat expr = not $ invalidBinding (bindPattern pat expr)
    
    const tests = [
        { pattern: A, expr: Literal(5), expected: true, name: "A matches Literal 5" },
        { pattern: A, expr: Op("+", B, C), expected: true, name: "A matches complex expr" },
        { pattern: Literal(5), expr: Literal(5), expected: true, name: "Literal matches same" },
        { pattern: Literal(5), expr: Literal(3), expected: false, name: "Literal doesn't match different" },
        { pattern: Op("+", A, B), expr: Op("+", Literal(1), Literal(2)), expected: true, name: "Op pattern matches" },
        { pattern: Op("+", A, B), expr: Op("o", Literal(1), Literal(2)), expected: false, name: "Op pattern doesn't match different op" },
        { pattern: Op("+", A, A), expr: Op("+", Literal(1), Literal(1)), expected: true, name: "Repeated variable matches same value" },
        { pattern: Op("+", A, A), expr: Op("+", Literal(1), Literal(2)), expected: false, name: "Repeated variable doesn't match different values" },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = matchPattern(test.pattern, test.expr);
        if (result === test.expected) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Pattern: ${test.pattern.toString()}`);
            console.error(`  Expr: ${test.expr.toString()}`);
            console.error(`  Expected: ${test.expected}, Got: ${result}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testApplyRule() {
    // Haskell source:
    // applyRule :: Rule -> Expr -> Expr
    // applyRule (Rule (pat, sub)) expression =
    //     maybe expression id (evalExpr binding sub)
    //     where binding = bindPattern pat expression
    
    const tests = [
        {
            rule: new Rule(A, Op("+", A, Literal(1))),
            expr: Literal(5),
            expected: Op("+", Literal(5), Literal(1)),
            name: "Add 1 to literal"
        },
        {
            rule: new Rule(Op("+", A, B), Op("+", B, A)),
            expr: Op("+", Literal(3), Literal(4)),
            expected: Op("+", Literal(4), Literal(3)),
            name: "Swap operands"
        },
        {
            rule: new Rule(Op("o", A, B), Op("+", Op("+", A, B), Literal(1))),
            expr: Op("o", Literal(2), Literal(3)),
            expected: Op("+", Op("+", Literal(2), Literal(3)), Literal(1)),
            name: "Cat operator expansion"
        },
        {
            rule: new Rule(A, B),
            expr: Op("+", Literal(1), Literal(2)),
            expected: Op("+", Literal(1), Literal(2)),
            name: "Non-matching pattern returns original"
        },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = applyRule(test.rule, test.expr);
        if (result.equals(test.expected)) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Rule: ${test.rule.toString()}`);
            console.error(`  Expr: ${test.expr.toString()}`);
            console.error(`  Expected: ${test.expected.toString()}`);
            console.error(`  Got: ${result.toString()}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testApplyEquality() {
    // Haskell source:
    // applyEquality :: Rule -> Expr -> Expr
    // applyEquality r@(Rule (pat, _)) expr =
    //     if matchPattern pat expr
    //         then applyRule r expr
    //         else applyRule (reverseRule r) expr
    
    const tests = [
        {
            rule: new Rule(Op("+", A, B), Op("+", B, A)),
            expr: Op("+", Literal(3), Literal(4)),
            expected: Op("+", Literal(4), Literal(3)),
            name: "Forward rule application"
        },
        {
            rule: new Rule(Op("+", A, B), Op("+", B, A)),
            expr: Op("+", Literal(4), Literal(3)),
            expected: Op("+", Literal(3), Literal(4)),
            name: "Reverse rule application"
        },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = applyEquality(test.rule, test.expr);
        if (result.equals(test.expected)) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Rule: ${test.rule.toString()}`);
            console.error(`  Expr: ${test.expr.toString()}`);
            console.error(`  Expected: ${test.expected.toString()}`);
            console.error(`  Got: ${result.toString()}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testApplyEqualityAt() {
    // Haskell source:
    // applyEqualityAt :: Int -> Rule -> Expr -> Expr
    // applyEqualityAt idx rule expr =
    //     outerMapExprWithIndex (\e i -> if i == idx then applyEquality rule e else e) expr
    
    const tests = [
        {
            idx: 0,
            rule: new Rule(A, Literal(0)),
            expr: Op("+", A, B),
            expected: Op("+", Literal(0), B),
            name: "Apply at index 0"
        },
        {
            idx: 1,
            rule: new Rule(A, Literal(1)),
            expr: Op("+", A, B),
            expected: Op("+", A, Literal(1)),
            name: "Apply at index 1 (operator)"
        },
        {
            idx: 2,
            rule: new Rule(B, Literal(2)),
            expr: Op("+", A, B),
            expected: Op("+", A, Literal(2)),
            name: "Apply at index 2"
        },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = applyEqualityAt(test.idx, test.rule, test.expr);
        if (result.equals(test.expected)) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Index: ${test.idx}`);
            console.error(`  Rule: ${test.rule.toString()}`);
            console.error(`  Expr: ${test.expr.toString()}`);
            console.error(`  Expected: ${test.expected.toString()}`);
            console.error(`  Got: ${result.toString()}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testSubExprAt() {
    // Haskell source:
    // subExprAt :: Int -> Expr -> Maybe Expr
    // subExprAt idx expr = fst $ subExprAt' expr idx 0
    
    const expr = Op("+", Op("o", A, B), C);
    // Index order: A(0), o(1), B(2), +(3), C(4)
    
    const tests = [
        { idx: 0, expected: A, name: "Get expr at index 0" },
        { idx: 1, expected: Op("o", A, B), name: "Get expr at index 1" },
        { idx: 2, expected: B, name: "Get expr at index 2" },
        { idx: 3, expected: Op("+", Op("o", A, B), C), name: "Get expr at index 3" },
        { idx: 4, expected: C, name: "Get expr at index 4" },
        { idx: 10, expected: null, name: "Out of bounds returns null" },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = subExprAt(test.idx, expr);
        const matches = (result === null && test.expected === null) || 
                       (result !== null && test.expected !== null && result.equals(test.expected));
        if (matches) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Index: ${test.idx}`);
            console.error(`  Expected: ${test.expected ? test.expected.toString() : 'null'}`);
            console.error(`  Got: ${result ? result.toString() : 'null'}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testReplaceABCwithXYZ() {
    // Haskell source:
    // replaceABCwithXYZ :: Rule -> Rule
    // replaceABCwithXYZ (Rule (l,r)) = Rule (mapExpr exprABCtoXYZ l, mapExpr exprABCtoXYZ r)
    // exprABCtoXYZ A = X
    // exprABCtoXYZ B = Y
    // exprABCtoXYZ C = Z
    
    const tests = [
        {
            rule: new Rule(A, B),
            expected: new Rule(X, Y),
            name: "Replace A with X, B with Y"
        },
        {
            rule: new Rule(Op("+", A, B), Op("+", B, C)),
            expected: new Rule(Op("+", X, Y), Op("+", Y, Z)),
            name: "Replace in complex expression"
        },
        {
            rule: new Rule(X, Y),
            expected: new Rule(X, Y),
            name: "X and Y unchanged"
        },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        const result = replaceABCwithXYZ(test.rule);
        if (result.equals(test.expected)) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Input: ${test.rule.toString()}`);
            console.error(`  Expected: ${test.expected.toString()}`);
            console.error(`  Got: ${result.toString()}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testGroupAxioms() {
    // Haskell source:
    // associativity op = (isTrueC, (A `ox` (B `ox` C)) `eq` ((A `ox` B) `ox` C))
    // rightNeutral op = (isNeutralC (Neutral op), (A `ox` Neutral op) `eq` A)
    // leftNeutral op = (isNeutralC (Neutral op), (Neutral op `ox` A) `eq` A)
    // rightInverse op = (isInverseC (Inv (op, A)), (A `ox` Inv (op, A)) `eq` Neutral op)
    // leftInverse op = (isInverseC (Inv (op, A)), (Inv (op, A) `ox` A) `eq` Neutral op)
    // commutativity op = (isTrueC, (A `ox` B) `eq` (B `ox` A))
    
    const op = "+";
    
    const assoc = associativity(op);
    const expectedAssoc = new Rule(
        Op("+", A, Op("+", B, C)),
        Op("+", Op("+", A, B), C)
    );
    
    const rNeutral = rightNeutral(op);
    const expectedRNeutral = new Rule(Op("+", A, Neutral("+")), A);
    
    const lNeutral = leftNeutral(op);
    const expectedLNeutral = new Rule(Op("+", Neutral("+"), A), A);
    
    const rInv = rightInverse(op);
    const expectedRInv = new Rule(Op("+", A, Inv("+", A)), Neutral("+"));
    
    const lInv = leftInverse(op);
    const expectedLInv = new Rule(Op("+", Inv("+", A), A), Neutral("+"));
    
    const comm = commutativity(op);
    const expectedComm = new Rule(Op("+", A, B), Op("+", B, A));
    
    const tests = [
        { result: assoc.rule, expected: expectedAssoc, name: "Associativity axiom" },
        { result: rNeutral.rule, expected: expectedRNeutral, name: "Right neutral axiom" },
        { result: lNeutral.rule, expected: expectedLNeutral, name: "Left neutral axiom" },
        { result: rInv.rule, expected: expectedRInv, name: "Right inverse axiom" },
        { result: lInv.rule, expected: expectedLInv, name: "Left inverse axiom" },
        { result: comm.rule, expected: expectedComm, name: "Commutativity axiom" },
    ];
    
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        if (test.result.equals(test.expected)) {
            passed++;
        } else {
            console.error(`FAIL: ${test.name}`);
            console.error(`  Expected: ${test.expected.toString()}`);
            console.error(`  Got: ${test.result.toString()}`);
            failed++;
        }
    }
    
    return { passed, failed, total: tests.length };
}

function testRandomTransformations(count = 1000) {
    // QuickCheck-style property tests
    // Test that transformations are consistent and deterministic
    
    const rng = new Random(42); // Fixed seed for reproducibility
    let passed = 0;
    let failed = 0;
    const errors = [];
    
    for (let i = 0; i < count; i++) {
        try {
            const expr = generateRandomExpr(rng, 0, 2);
            const rule = generateRandomRule(rng, 2);
            
            // Property: applyRule is deterministic
            const result1 = applyRule(rule, expr);
            const result2 = applyRule(rule, expr);
            
            if (!result1.equals(result2)) {
                failed++;
                if (errors.length < 10) {
                    errors.push({
                        test: i,
                        property: "applyRule determinism",
                        expr: expr.toString(),
                        rule: rule.toString()
                    });
                }
            } else {
                passed++;
            }
            
            // Property: matchPattern is consistent with applyRule
            const matches = matchPattern(rule.left, expr);
            const applied = applyRule(rule, expr);
            const changed = !applied.equals(expr);
            
            if (matches && !changed) {
                // If pattern matches, expression should change (unless substitution is identical)
                // This is actually OK - pattern can match but substitution might be same
                passed++;
            } else if (!matches && changed) {
                failed++;
                if (errors.length < 10) {
                    errors.push({
                        test: i,
                        property: "matchPattern consistency",
                        expr: expr.toString(),
                        rule: rule.toString(),
                        matches: matches,
                        changed: changed
                    });
                }
            } else {
                passed++;
            }
            
        } catch (e) {
            failed++;
            if (errors.length < 10) {
                errors.push({
                    test: i,
                    error: e.message,
                    stack: e.stack
                });
            }
        }
    }
    
    if (errors.length > 0) {
        console.error(`\nRandom test errors (showing first ${Math.min(10, errors.length)}):`);
        errors.forEach(err => console.error(JSON.stringify(err, null, 2)));
    }
    
    return { passed, failed, total: count * 2 };
}

// ============================================================================
// Main Test Runner
// ============================================================================

function runAllTests() {
    console.log("=".repeat(70));
    console.log("Tomtegebra Algebra.js Test Suite");
    console.log("Verifying JavaScript matches Haskell implementation");
    console.log("=".repeat(70));
    console.log("");
    
    const suites = [
        { name: "Expression Equality", fn: testExprEquals },
        { name: "Pattern Matching", fn: testMatchPattern },
        { name: "Apply Rule", fn: testApplyRule },
        { name: "Apply Equality", fn: testApplyEquality },
        { name: "Apply Equality At Index", fn: testApplyEqualityAt },
        { name: "Sub-expression At Index", fn: testSubExprAt },
        { name: "Replace ABC with XYZ", fn: testReplaceABCwithXYZ },
        { name: "Group Axioms", fn: testGroupAxioms },
        { name: "Random Transformations (1000 cases)", fn: () => testRandomTransformations(1000) },
    ];
    
    let totalPassed = 0;
    let totalFailed = 0;
    let totalTests = 0;
    
    for (const suite of suites) {
        console.log(`\nRunning: ${suite.name}`);
        console.log("-".repeat(70));
        
        try {
            const result = suite.fn();
            totalPassed += result.passed;
            totalFailed += result.failed;
            totalTests += result.total;
            
            if (result.failed === 0) {
                console.log(`✓ PASSED: ${result.passed}/${result.total} tests`);
            } else {
                console.log(`✗ FAILED: ${result.failed}/${result.total} tests failed`);
            }
        } catch (e) {
            console.error(`✗ ERROR: ${e.message}`);
            console.error(e.stack);
            totalFailed++;
            totalTests++;
        }
    }
    
    console.log("");
    console.log("=".repeat(70));
    console.log("SUMMARY");
    console.log("=".repeat(70));
    console.log(`Total Tests: ${totalTests}`);
    console.log(`Passed: ${totalPassed} (${((totalPassed/totalTests)*100).toFixed(1)}%)`);
    console.log(`Failed: ${totalFailed} (${((totalFailed/totalTests)*100).toFixed(1)}%)`);
    console.log("");
    
    if (totalFailed === 0) {
        console.log("✓ SUCCESS - All tests passed!");
        return true;
    } else {
        console.log("✗ FAILURE - Some tests failed");
        return false;
    }
}

// Run tests automatically when loaded in browser
if (typeof window !== 'undefined') {
    window.addEventListener('load', () => {
        // Wait a bit for algebra.js to load
        setTimeout(runAllTests, 100);
    });
}

// Export for Node.js
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { runAllTests };
}
