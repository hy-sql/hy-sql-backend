const express = require('express')
const app = express()
const cors = require('cors')
const morgan = require('morgan')
const queryRouter = require('./controllers/query')
const pingRouter = require('./controllers/ping')

morgan.token('body', function (request) {
    return JSON.stringify(request.body)
})

app.use(express.json())
app.use(cors())
app.use(
    morgan(
        ':method :url :status :res[content-length] - :response-time ms :body'
    )
)

const unknownEndpoint = (request, response) => {
    response.status(404).send({ error: 'unknown endpoint' })
}

app.get('/', (req, res) => {
    res.send('<h1>Hello World!</h1>')
})

app.use('/api/ping', pingRouter)
app.use('/api/query', queryRouter)

app.use(unknownEndpoint)

module.exports = app
