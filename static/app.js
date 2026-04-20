const form      = document.getElementById('form');
const prompt    = document.getElementById('prompt');
const output    = document.getElementById('output');
const statusEl  = document.getElementById('status');
const actionBtn = document.getElementById('action-btn');
const clearBtn  = document.getElementById('clear-btn');
const copyBtn   = document.getElementById('copy-btn');

// Control-code framing from the server. Keep in sync with handler.lisp.
const SOH = '\x01';  // begin generation (client clears output box)
const EOT = '\x04';  // end of generation
const NAK = '\x15';  // error: message text follows

// state: 'idle' | 'generating' | 'done'
let state = 'idle';
let ws = null;
let retries = 0;
const maxRetries = 5;

// --- Typewriter queue ------------------------------------------------------
// Tokens arrive from the server in bursts. We queue the raw characters and
// drain them at a steady cadence so the output feels like a typewriter
// regardless of how the network delivered them.
//
// Design decision: no "catch up" — we always drain at CHAR_INTERVAL_MS.
// The server caps total output at MAX_OUTPUT_CHARS (~3000 by default), and
// a paused queue will still finish in a comfortable time window.

const CHAR_INTERVAL_MS = 12;   // ~83 chars/sec — fast, still reads as typing
let   charQueue    = [];
let   drainTimerId = null;
let   generationComplete = false;  // server sent EOT; drain the rest then 'done'

function enqueueChars(s) {
    for (let i = 0; i < s.length; i++) charQueue.push(s[i]);
    startDrainIfNeeded();
}

function startDrainIfNeeded() {
    if (drainTimerId !== null) return;
    drainTimerId = setInterval(drainOne, CHAR_INTERVAL_MS);
}

function drainOne() {
    if (charQueue.length === 0) {
        if (generationComplete) {
            clearInterval(drainTimerId);
            drainTimerId = null;
            generationComplete = false;
            setState('done');
        }
        return;
    }
    output.textContent += charQueue.shift();
}

function resetTypewriter() {
    charQueue = [];
    generationComplete = false;
    if (drainTimerId !== null) {
        clearInterval(drainTimerId);
        drainTimerId = null;
    }
}

// --- State machine ---------------------------------------------------------

function setState(s) {
    state = s;
    if (s === 'idle') {
        // readOnly (not disabled): the input still accepts focus and Ctrl+A.
        prompt.readOnly = false;
        actionBtn.textContent = 'generate';
        actionBtn.disabled = false;
        clearBtn.disabled = prompt.value.length === 0;
        copyBtn.classList.remove('visible');
        output.classList.remove('visible');
        output.textContent = '';
        resetTypewriter();
        prompt.focus();
    } else if (s === 'generating') {
        prompt.readOnly = true;
        actionBtn.textContent = 'generating...';
        actionBtn.disabled = true;
        clearBtn.disabled = true;
        copyBtn.classList.remove('visible');
    } else if (s === 'done') {
        prompt.readOnly = true;
        actionBtn.textContent = 'new poem';
        actionBtn.disabled = false;
        clearBtn.disabled = true;
        copyBtn.classList.add('visible');
        statusEl.textContent = '';
    }
}

prompt.addEventListener('input', function () {
    if (state === 'idle') {
        clearBtn.disabled = prompt.value.length === 0;
    }
});

clearBtn.onclick = function () {
    prompt.value = '';
    clearBtn.disabled = true;
    prompt.focus();
};

// --- WebSocket wiring ------------------------------------------------------

function connect() {
    const scheme = location.protocol === 'https:' ? 'wss:' : 'ws:';
    ws = new WebSocket(scheme + '//' + location.host + '/ws');

    ws.onopen = function () {
        retries = 0;
        if (state === 'idle') statusEl.textContent = '';
    };

    ws.onmessage = function (e) {
        const data = e.data;

        if (data === SOH) {
            output.textContent = '';
            output.classList.add('visible');
            resetTypewriter();
            return;
        }

        if (data === EOT) {
            // Don't flip to 'done' yet — let the typewriter drain the tail
            // of the queue first. drainOne() will flip the state when empty.
            generationComplete = true;
            startDrainIfNeeded();
            return;
        }

        if (data.length > 0 && data.charAt(0) === NAK) {
            // Server-side error or validation problem. Abort generation,
            // surface the message, go back to idle. Output is cleared by
            // setState('idle') so the user isn't left staring at a partial
            // result with an error pasted at the end.
            resetTypewriter();
            setState('idle');
            statusEl.textContent = data.substring(1);
            return;
        }

        enqueueChars(data);
    };

    ws.onclose = function () {
        if (state === 'generating') {
            resetTypewriter();
            setState('idle');
        }
        if (retries < maxRetries) {
            retries++;
            const delay = Math.min(1000 * Math.pow(2, retries), 30000);
            statusEl.textContent = 'disconnected — retrying in ' + Math.round(delay / 1000) + 's';
            setTimeout(connect, delay);
        } else {
            statusEl.textContent = 'disconnected — reload to retry';
        }
    };

    ws.onerror = function () {
        // onclose will run immediately after; let it handle reconnect UX.
    };
}

// --- Form submission -------------------------------------------------------

form.onsubmit = function (e) {
    e.preventDefault();

    if (state === 'done') {
        setState('idle');
        return;
    }

    if (state === 'idle') {
        const text = prompt.value.trim();
        if (!text) return;
        if (!ws || ws.readyState !== WebSocket.OPEN) {
            statusEl.textContent = 'not connected — try again in a moment';
            return;
        }
        setState('generating');
        statusEl.textContent = 'generating...';
        ws.send(text);
    }
};

copyBtn.onclick = function () {
    navigator.clipboard.writeText(output.textContent).then(function () {
        copyBtn.textContent = 'copied!';
        setTimeout(function () { copyBtn.textContent = 'copy'; }, 1500);
    });
};

connect();
