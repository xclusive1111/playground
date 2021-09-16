## Javascript AES/CBC/PKCS7PADDING example

+ This example uses a random IV to encrypt data
+ The IV then get prepended into the encrypted data before returning
+ Decryption takes the first 16 bytes as an IV, bytes from the 17th as data to be decrytped
