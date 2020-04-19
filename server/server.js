const fastify = require('fastify')()

fastify.register(require('./routes'), { prefix: '' })

//enables CORS
fastify.register(require('fastify-cors'), {
   origin: "*",
   allowedHeaders: ['Origin', 'X-Requested-With', 'Accept', 'Content-Type', 'Authorization', 'Content-Disposition' ],
   methods: ['GET', 'PUT', 'PATCH', 'POST', 'DELETE']
})

fastify.listen(3000, (err) => {
    if (err) {
        console.log(err)
        process.exit(1)
    } else {
        console.log('Server is up on port 3000...')
    }

})
