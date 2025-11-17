// tcp-echo.js
const net = require('net');
const PORT = 7000;

const server = net.createServer(socket => {
    console.log('▶ Client connected:', socket.remoteAddress);
    // Send initial greeting
    socket.write('HELLO FROM SERVER!\r\n');

    // Then echo *all* data back
    socket.on('data', data => {
        console.log('◀ Received:', data.toString().trim());
        socket.write(data);
    });

    socket.on('end', () => console.log('⚪ Client disconnected'));
    socket.on('error', err => console.error('Socket error:', err));
});

server.listen(PORT, '0.0.0.0', () => {
    console.log(`✅ Echo server listening on port ${PORT}`);
});
