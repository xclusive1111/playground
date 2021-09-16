// TextToBytes returns UTF-8 byte array from the given plain text
function TextToBytes(text) {
    return new TextEncoder("utf-8").encode(text);
}

// BytesToText returns UTF-8 text from the given byte array
function BytesToText(bytes) {
    return new TextDecoder().decode(bytes);
}


// Return a random number between min & max
function RandomNumber(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min
}

// Encrypt returns a base64 encrypted text from the given UTF-8 text
function Encrypt(text, keyText) {
    // Convert text key to bytes
    const keyBytes = TextToBytes(keyText);

    // Generate random IV of 16 bytes
    const iv = [...Array(16)].map(i => RandomNumber(0, 255))

    const textBytes = Padding(TextToBytes(text));

    // CBC encrypted
    const aesCbc = new aesjs.ModeOfOperation.cbc(keyBytes, iv);
    const encryptedBytes = aesCbc.encrypt(textBytes);

    // Concat random IV & encrypted data
    const encrypted = new Uint8Array([
        ...iv,
        ...encryptedBytes,
    ]);

    // Encode encrypted data as a base64 string
    return btoa(String.fromCharCode(...new Uint8Array(encrypted)));
}

// Decrypt returns a UTF-8 plain text from the given base64 encryptedText
function Decrypt(encryptedText, keyText) {
    // Convert based 64 encrypted text to bytes
    const encryptedBytes = Uint8Array.from(atob(encryptedText), c => c.charCodeAt(0));

    // First 16 bytes as IV
    const iv = encryptedBytes.slice(0, 16);

    // Remaining bytes as encrypted data
    const encrypted = encryptedBytes.slice(16);

    // Convert text key to bytes
    const keyBytes = TextToBytes(keyText);

    // CBC decrypt
    const aesCbc = new aesjs.ModeOfOperation.cbc(keyBytes, iv);
    const decryptedBytes = aesCbc.decrypt(encrypted);

    // Remove padding
    const decrypted = UnPadding(decryptedBytes);

    // Convert to UTF-8 string
    return BytesToText(decrypted);
}

// UnPadding remove padding of decrypted bytes
function UnPadding(decryptedBytes) {
    const len = decryptedBytes.length;

    // Last byte indicates the padding size
    const size = decryptedBytes[len - 1];

    return decryptedBytes.slice(0, len - size)
}

// Padding add missing bytes into the source
function Padding(bytes) {
    // Calculate missing bytes
    const size = 16 - bytes.length % 16;
    const missingBytes = new Array(size).fill(size);

    // Append missing bytes into the source
    return new Uint8Array([
        ...bytes,
        ...missingBytes,
    ]);
}