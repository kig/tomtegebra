// Game.js - Port of Game.hs
// Game state and logic

class Level {
    constructor(op, rules) {
        this.op = op;
        this.rules = rules;
    }
}

// Level definitions
const levelCat = new Level("o", [
    new Rule(Op("o", A, B), Op("+", Op("+", A, B), Literal(1)))
]);

const levelFrog = new Level("f", [
    new Rule(Op("f", A, B), Op("o", Op("o", A, Literal(1)), B))
]);

const levelBunny = new Level("x", [
    new Rule(Op("x", A, B), Op("f", Op("f", Literal(1), B), A))
]);

class AppState {
    constructor() {
        // Current equation being solved
        this.equation = null;
        
        // Cursor position in equation
        this.cursorLocation = 0;
        
        // Available rules/lemmas
        this.inventory = [];
        
        // Selected inventory item
        this.inventoryIndex = -1;
        
        // Is current equation completed?
        this.equationCompleted = true;
        
        // Remaining equations in current level
        this.equations = [];
        
        // Remaining levels
        this.levels = [];
        
        // Game over flag
        this.gameOver = true;
        
        // Canvas dimensions
        this.width = 900;
        this.height = 570;
    }

    initGame() {
        // Start with basic + operator axioms
        this.inventory = abelianGroup("+").map(eq => replaceABCwithXYZ(eq.rule));
        this.levels = [levelCat, levelFrog, levelBunny];
        this.equations = [];
        this.cursorLocation = 0;
        this.inventoryIndex = -1;
        this.equationCompleted = true;
        this.gameOver = true;
        return this;
    }

    resetGame() {
        this.equationCompleted = false;
        this.gameOver = false;
        return this;
    }

    resetCursor() {
        this.cursorLocation = 0;
        this.inventoryIndex = -1;
        return this;
    }

    changeLevel(level) {
        const equations = abelianGroup(level.op);
        this.equations = equations;
        this.inventory = this.inventory.concat(level.rules.map(r => replaceABCwithXYZ(r)));
        return this.firstEquation();
    }

    nextLevel() {
        if (this.levels.length === 0) {
            this.gameOver = true;
            return this;
        }
        
        const nextLevel = this.levels.shift();
        return this.changeLevel(nextLevel);
    }

    changeEquation(checkableRule) {
        this.resetCursor();
        this.equation = checkableRule;
        this.equationCompleted = false;
        return this;
    }

    firstEquation() {
        if (this.equations.length === 0) {
            return this.nextLevel();
        }
        return this.changeEquation(this.equations[0]);
    }

    nextEquation() {
        if (this.equations.length === 0) {
            return this.nextLevel();
        }
        
        const completedEq = this.equations.shift();
        this.addToInventory(completedEq);
        
        if (this.equations.length === 0) {
            return this.nextLevel();
        }
        
        return this.changeEquation(this.equations[0]);
    }

    addToInventory(checkableRule) {
        this.inventory.push(replaceABCwithXYZ(checkableRule.rule));
    }

    moveCursorLeft() {
        if (!this.equation) return this;
        const len = this.equation.rule.toExpr().length();
        this.cursorLocation = (this.cursorLocation - 1 + len) % len;
        return this;
    }

    moveCursorRight() {
        if (!this.equation) return this;
        const len = this.equation.rule.toExpr().length();
        this.cursorLocation = (this.cursorLocation + 1) % len;
        return this;
    }

    scrollInventoryUp() {
        if (this.inventoryIndex > -1) {
            this.inventoryIndex--;
        }
        return this;
    }

    scrollInventoryDown() {
        this.inventoryIndex++;
        return this;
    }

    applyCurrentRule() {
        if (!this.equation) return this;
        
        const inv = inventoryFor(this.cursorLocation, this.equation, this.inventory);
        if (inv.length === 0) return this;
        
        const invIdx = ((this.inventoryIndex % inv.length) + inv.length) % inv.length;
        const rule = inv[invIdx];
        
        const equExpr = this.equation.rule.toExpr();
        const newExpr = applyEqualityAt(this.cursorLocation, rule, equExpr);
        
        // Try to convert back to rule
        if (newExpr.type === 'Op' && newExpr.args[0] === '=') {
            const newRule = new Rule(newExpr.args[1], newExpr.args[2]);
            
            // Check if equation is completed
            let completed = false;
            if (this.equation.predicate === 'bothEqual') {
                completed = isTrue(newRule);
            } else if (this.equation.predicate === 'bindNeutral') {
                completed = isBinding(this.equation.expr, newRule);
            } else if (this.equation.predicate === 'bindInverse') {
                completed = isBinding(this.equation.expr, newRule);
            }
            
            this.equation = {
                predicate: this.equation.predicate,
                rule: newRule,
                expr: this.equation.expr
            };
            this.equationCompleted = completed;
            
            // Adjust cursor if needed
            const newLen = newRule.toExpr().length();
            if (this.cursorLocation >= newLen) {
                this.cursorLocation = newLen - 1;
            }
        }
        
        return this;
    }

    checkEquationCompleted() {
        if (!this.equation) return false;
        
        if (this.equation.predicate === 'bothEqual') {
            return isTrue(this.equation.rule);
        } else if (this.equation.predicate === 'bindNeutral') {
            return isBinding(this.equation.expr, this.equation.rule);
        } else if (this.equation.predicate === 'bindInverse') {
            return isBinding(this.equation.expr, this.equation.rule);
        }
        
        return false;
    }
}
