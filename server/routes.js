const ObjectId = require('mongodb').ObjectID
const MongoClient = require('mongodb').MongoClient
const crypto = require('crypto');
const base32 = require('hi-base32');

const url = 'mongodb://localhost:27017';
const client = new MongoClient(url, {useNewUrlParser:true, useUnifiedTopology:true})
const connection = client.connect()

//Oznaceny kod prebraty zo zdroja: https://hackernoon.com/how-to-implement-google-authenticator-two-factor-auth-in-javascript-091wy3vh3
//V kode boli vykonane minimalne upravy
//*****************************************************************************
//*****************************************************************************

function generateSecret(length){
    const randomBuffer = crypto.randomBytes(length);
    return base32.encode(randomBuffer).replace(/=/g, '');
}

function generateHOTP(secret, counter){
    let length = 6
    const decodedSecret = base32.decode.asBytes(secret);
    const buffer = Buffer.alloc(8);

    for (let i = 0; i < 8; i++) {
        buffer[7 - i] = counter & 0xff;
        counter = counter >> 8;
    }

    // Step 1: Generate an HMAC-SHA-1 value
    const hmac = crypto.createHmac('sha1', Buffer.from(decodedSecret));
    hmac.update(buffer);
    const hmacResult = hmac.digest();
 
    // Step 2: Generate a 4-byte string (Dynamic Truncation)
    const code = dynamicTruncationFn(hmacResult);

    // Step 3: Compute an HOTP value
    return code % 10 ** length;
}

function dynamicTruncationFn(hmacValue) {
    const offset = hmacValue[hmacValue.length - 1] & 0xf;
    return (
        ((hmacValue[offset] & 0x7f) << 24) |
        ((hmacValue[offset + 1] & 0xff) << 16) |
        ((hmacValue[offset + 2] & 0xff) << 8) |
        (hmacValue[offset + 3] & 0xff)
    );
}

function generateTOTP(secret, window = 0) {
    const counter = Math.floor(Date.now() / 30000);
    return generateHOTP(secret, counter + window);
}

function verifyTOTP(token, secret, window = 2) {
    if (Math.abs(+window) > 10) {
        console.error('Window size is too large');
        return false;
    }

    for (let errorWindow = -window; errorWindow <= +window; errorWindow++) {
        const totp = generateTOTP(secret, errorWindow);
        if (token === totp) {
            return true;
        }
    }
    return false;
}
//*****************************************************************************
//*****************************************************************************

async function routes(fastify) {

    fastify.post('/sign_up', async (req, res) => {
        //cesta ktora ma na starosti registraciu noveho uctu
        //registrovat sa moze pouzivatel iba s unikatnym menom, inak server vrati kod 400
        res.header("Access-Control-Allow-Origin", "*")
        res.header("Access-Control-Allow-Headers", "X-Requested-With")
        const db = client.db('PIB')
        let username = req.body.username
        let password = req.body.password
        let cursor = db.collection('accounts').findOne({ username: username }, async function(err, result){
            if(err){
                res.code(500).send(new Error("Something went wrong on the server's side"))
            }
            else if(result){
                res.code(400).send(new Error("Username is already taken!"))
            }
            else{    
                let code = generateSecret(20)
                let insert = db.collection('accounts').insertOne({username: username, password: password, secret: code})
                let response = "otpauth://totp/Example:" + username + "?secret=" + code + "&issuer=Example"
                res.send({ secret : response })
            }
        })
    })

    fastify.post('/sign_in', async (req, res) => {    //login
        //cesta ma na starosti prihlasenie prostrednictvom mena a hesla
        let username = req.body.username
        let password = req.body.password
        const db = client.db('PIB')
        var cursor = await db.collection('accounts').findOne({username: username, password: password}, function(err, result){
            if(err){
                res.code(500).send(new Error("Something went wrong on the server's side"))
            }
            else if(result){
                res.code(200).send()
            }
            else{
                res.code(400).send(new Error("Invalid creditentials"))
            }
        })
    })

    fastify.post('/validate', async (req, res) => {
        //cesta ma na starosti overenie jedorazoveho hesla generovaneho algoritmom TOTP
        //pracuje so sukromnym klucom ktory prilieha uzivatelovi s menom prijatym v tele poziadavky
        let username = req.body.username
        let code = req.body.otp
        const db = client.db('PIB')
        let user = db.collection('accounts').findOne({ username: username }, async function(err, result){
            if(err){
                res.code(500).send(new Error("Something went wrong on the server's side"))
            }
            else if(result){
                if(verifyTOTP(code, result.secret) == true){
                    res.code(200).send()
                }
                else{
                    res.code(400).send(new Error("Invalid OTP"))
                }
            }
            else{    
                res.code(400).send(new Error("No user with this username found!"))
            }
        })
    })

}

module.exports = routes