// Render.js - Rendering logic for the game
// Port of RenderGame.hs using Canvas 2D API

class Renderer {
    constructor(canvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        this.images = {};
        this.imageMap = {
            'cursor': 'cursor.png',
            'A': 'mushroom.png',
            'B': 'cherry.png',
            'C': 'orange.png',
            'X': 'empty_green.png',
            'Y': 'empty_red.png',
            'Z': 'empty_purple.png',
            'L': 'literal.png',
            'E': 'neutral.png',
            'I': 'neutral.png',
            '=': 'eq.png',
            '+': 'plusbun.png',
            'x': 'mulbun.png',
            'f': 'frog.png',
            'o': 'cat.png'
        };
        this.imagesLoaded = 0;
        this.imagesToLoad = Object.keys(this.imageMap).length;
    }

    loadImages(callback) {
        for (const [key, filename] of Object.entries(this.imageMap)) {
            const img = new Image();
            img.onload = () => {
                this.imagesLoaded++;
                if (this.imagesLoaded === this.imagesToLoad && callback) {
                    callback();
                }
            };
            img.src = `images/${filename}`;
            this.images[key] = img;
        }
    }

    clear() {
        this.ctx.fillStyle = 'white';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
    }

    drawTitleScreen(state) {
        this.clear();
        
        this.ctx.fillStyle = '#667eea';
        this.ctx.font = 'bold 72px Trebuchet MS, sans-serif';
        this.ctx.textAlign = 'center';
        this.ctx.fillText('Tomtegebra', this.canvas.width / 2, 200);
        
        this.ctx.fillStyle = '#333';
        this.ctx.font = '32px Trebuchet MS, sans-serif';
        this.ctx.fillText('Press space to play', this.canvas.width / 2, 350);
    }

    drawLevel(state) {
        this.clear();
        
        if (!state.equation) return;

        // Draw instruction text
        this.drawInstructionText(state);
        
        // Draw equation
        this.drawEquation(state);
        
        // Draw inventory
        this.drawInventory(state);
    }

    drawInstructionText(state) {
        this.ctx.fillStyle = '#333';
        this.ctx.font = '18px sans-serif';
        this.ctx.textAlign = 'center';
        
        let text = 'Make both sides equal';
        if (state.equation.predicate === 'bindNeutral') {
            text = 'Reduce one side to the circled animal';
        } else if (state.equation.predicate === 'bindInverse') {
            text = 'Reduce one side to the circled mushroom';
        }
        
        if (state.equationCompleted) {
            this.ctx.fillStyle = '#00aa00';
            text = 'Level complete! Press space to continue';
        }
        
        this.ctx.fillText(text, this.canvas.width / 2, 30);
    }

    drawEquation(state) {
        const expr = state.equation.rule.toExpr();
        const items = this.exprToDrawListWithDepth(expr);
        
        // Calculate scaling based on number of items
        // Baseline: 5 symbols fit comfortably, scale down if more
        const baseItemSize = 60;
        const baseSpacing = 70;
        const baseSymbolCount = 5;
        const maxWidth = this.canvas.width - 100; // Leave margin
        
        let itemSize = baseItemSize;
        let spacing = baseSpacing;
        
        if (items.length > baseSymbolCount) {
            const scaleFactor = Math.min(1, baseSymbolCount / items.length);
            itemSize = baseItemSize * scaleFactor;
            spacing = baseSpacing * scaleFactor;
        }
        
        // Find max depth for vertical positioning
        const maxDepth = Math.max(...items.map(item => item.depth));
        
        // Calculate positions
        const centerX = this.canvas.width / 2;
        const baseY = 200; // Base Y position
        const depthOffset = 40; // Vertical offset per depth level
        
        const totalWidth = items.length * spacing;
        let x = centerX - totalWidth / 2;
        
        items.forEach((item, idx) => {
            // Calculate Y position based on depth (arguments are lower)
            const y = baseY + item.depth * depthOffset;
            
            // Draw cursor
            if (idx === state.cursorLocation && this.images['cursor']) {
                this.ctx.drawImage(this.images['cursor'], 
                    x - 5, y - itemSize / 2 - 10, 
                    itemSize + 10, itemSize + 10);
            }
            
            // Draw item
            if (this.images[item.symbol]) {
                this.ctx.drawImage(this.images[item.symbol], 
                    x, y - itemSize / 2, 
                    itemSize, itemSize);
            } else {
                // Fallback: draw text
                this.ctx.fillStyle = '#333';
                this.ctx.font = `bold ${Math.floor(24 * (itemSize / baseItemSize))}px monospace`;
                this.ctx.textAlign = 'center';
                this.ctx.fillText(item.symbol, x + itemSize / 2, y + 10);
            }
            
            // Draw literal number
            if (item.number !== null) {
                this.ctx.fillStyle = '#000';
                this.ctx.font = `bold ${Math.floor(20 * (itemSize / baseItemSize))}px sans-serif`;
                this.ctx.textAlign = 'center';
                this.ctx.fillText(item.number, x + itemSize / 2, y + itemSize / 2 + 5);
            }
            
            x += spacing;
        });
    }

    exprToDrawListWithDepth(expr, depth = 0, list = []) {
        if (expr.type === 'Op') {
            // Left argument is drawn at depth + 1
            this.exprToDrawListWithDepth(expr.args[1], depth + 1, list);
            // Operator is drawn at current depth
            list.push({ symbol: expr.args[0], number: null, depth: depth });
            // Right argument is drawn at depth + 1
            this.exprToDrawListWithDepth(expr.args[2], depth + 1, list);
        } else if (expr.type === 'Literal') {
            list.push({ symbol: 'L', number: expr.args[0], depth: depth });
        } else if (expr.type === 'Neutral') {
            list.push({ symbol: 'E', number: null, depth: depth });
        } else if (expr.type === 'Inv') {
            list.push({ symbol: 'I', number: null, depth: depth });
        } else {
            list.push({ symbol: expr.type, number: null, depth: depth });
        }
        return list;
    }

    drawInventory(state) {
        const startY = 350;
        const itemHeight = 30;
        const maxVisible = 5;
        
        this.ctx.fillStyle = '#f5f5f5';
        this.ctx.fillRect(50, startY - 20, this.canvas.width - 100, 200);
        
        this.ctx.strokeStyle = '#ddd';
        this.ctx.strokeRect(50, startY - 20, this.canvas.width - 100, 200);
        
        const inv = inventoryFor(state.cursorLocation, state.equation, state.inventory);
        if (inv.length === 0) {
            this.ctx.fillStyle = '#999';
            this.ctx.font = '16px sans-serif';
            this.ctx.textAlign = 'center';
            this.ctx.fillText('No applicable rules', this.canvas.width / 2, startY + 50);
            return;
        }
        
        const invIdx = ((state.inventoryIndex % inv.length) + inv.length) % inv.length;
        const start = Math.max(0, Math.min(inv.length - maxVisible, invIdx - 2));
        
        for (let i = 0; i < Math.min(maxVisible, inv.length); i++) {
            const ruleIdx = start + i;
            const rule = inv[ruleIdx];
            const y = startY + i * itemHeight;
            
            if (ruleIdx === invIdx) {
                this.ctx.fillStyle = '#667eea';
                this.ctx.fillRect(60, y - 15, this.canvas.width - 120, itemHeight - 5);
            }
            
            this.ctx.fillStyle = ruleIdx === invIdx ? '#fff' : '#333';
            this.ctx.font = '14px monospace';
            this.ctx.textAlign = 'left';
            this.ctx.fillText(rule.toString(), 70, y);
        }
        
        // Draw scroll indicators
        if (inv.length > maxVisible) {
            this.ctx.fillStyle = '#999';
            this.ctx.font = '12px sans-serif';
            this.ctx.textAlign = 'center';
            if (start > 0) {
                this.ctx.fillText('▲', this.canvas.width / 2, startY - 10);
            }
            if (start + maxVisible < inv.length) {
                this.ctx.fillText('▼', this.canvas.width / 2, startY + maxVisible * itemHeight + 10);
            }
        }
    }

    render(state) {
        if (state.gameOver) {
            this.drawTitleScreen(state);
        } else {
            this.drawLevel(state);
        }
    }
}
