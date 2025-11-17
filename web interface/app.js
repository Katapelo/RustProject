// app.js
const http = require('http');
const path = require('path');
const fs   = require('fs');
const WebSocket = require('ws');
const net  = require('net');

const HTTP_PORT = 3000;
const TCP_PORT  = 7000;

// Keep track of the single Pico socket (or undefined if not connected)
let picoSocket = null;

/**
 * 1) Start the TCP “echo” (really forward) server for the Pico
 */
const tcpServer = net.createServer(socket => {
    console.log('▶ [TCP] Pico connected from', socket.remoteAddress, socket.remotePort);

    picoSocket = socket;

    socket.on('data', data => {
        console.log('◀ [TCP] From Pico:', data.toString().trim());
        // (Optionally) you could broadcast this back to all WS clients here
    });

    socket.on('error', err => {
        console.warn('[TCP] socket error', err.code);
    });

    socket.on('close', hadErr => {
        console.log(`[TCP] Pico disconnected${hadErr ? ' with error' : ''}`);
        });
    });


tcpServer.listen(TCP_PORT, '0.0.0.0', () => {
    console.log(`✅ [TCP] Server listening on 0.0.0.0:${TCP_PORT}`);
});

/**
 * 2) Start the HTTP server (serves index.html) & hook up WS
 */
const server = http.createServer((req, res) => {
    // Only support GET /
    if (req.method !== 'GET' || (req.url !== '/' && req.url !== '/index.html')) {
        res.writeHead(404);
        return res.end('Not found');
    }
    // Serve public/index.html
    const p = path.join(__dirname, 'public', 'index.html');
    fs.readFile(p, (err, data) => {
        if (err) {
            res.writeHead(500);
            return res.end('Server error');
        }
        res.writeHead(200, { 'Content-Type': 'text/html' });
        res.end(data);
    });
});

server.listen(HTTP_PORT, () => {
    console.log(`✅ [HTTP] Server listening on http://localhost:${HTTP_PORT}`);
});

// 3) WebSocket upgrade on the same HTTP server
const wss = new WebSocket.Server({ server });

wss.on('connection', ws => {
    console.log('▶ [WS] Browser connected');

    ws.on('message', msg => {
        console.log('◀ [WS] Received from browser:', msg.toString());
        // forward to Pico if connected
        if (picoSocket) {
            picoSocket.write(msg + '\r\n');
            console.log('[TCP] Forwarded to Pico:', msg.toString());
        } else {
            console.warn('[WS] Cannot forward, no Pico connected');
        }
        // optionally ack back to browser
        ws.send(`→ sent to Pico: ${msg}`);
    });

    ws.on('close', () => console.log('⚪ [WS] Browser disconnected'));
    ws.on('error', e => console.error('[WS] error:', e));
});
