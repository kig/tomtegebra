// Main.js - Main game loop and event handling

let state;
let renderer;

function init() {
    const canvas = document.getElementById('game-canvas');
    renderer = new Renderer(canvas);
    
    // Initialize game state
    state = new AppState().initGame();
    
    // Load images and start
    renderer.loadImages(() => {
        render();
        setupEventHandlers();
    });
}

function setupEventHandlers() {
    document.addEventListener('keydown', handleKeyDown);
}

function handleKeyDown(e) {
    // Prevent default browser behavior for arrow keys and space
    if (['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', ' '].includes(e.key)) {
        e.preventDefault();
    }
    
    switch(e.key) {
        case 'ArrowLeft':
            state.moveCursorLeft();
            break;
            
        case 'ArrowRight':
            state.moveCursorRight();
            break;
            
        case 'ArrowUp':
            state.scrollInventoryUp();
            break;
            
        case 'ArrowDown':
            state.scrollInventoryDown();
            break;
            
        case ' ':
            if (state.gameOver) {
                state.nextLevel().resetGame();
            } else if (state.equationCompleted) {
                state.nextEquation();
            } else {
                state.applyCurrentRule();
            }
            break;
            
        case 'q':
        case 'Q':
            // Reset game
            state = new AppState().initGame();
            break;
            
        default:
            return; // Don't render if key wasn't handled
    }
    
    render();
}

function render() {
    renderer.render(state);
}

// Start the game when page loads
window.addEventListener('load', init);
