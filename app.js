const express = require('express')
const app = express()
const cors = require('cors')
const morgan = require('morgan')
const executer = require('./middleware/executer')

morgan.token('body', function (req) {
    return JSON.stringify(req.body)
})

app.use(express.json())
app.use(cors())
app.use(
    morgan(
        ':method :url :status :res[content-length] - :response-time ms :body'
    )
)
app.use('/api/query', executer)

const unknownEndpoint = (req, res) => {
    res.status(404).send({ error: 'unknown endpoint' })
}

app.get('/', (req, res) => {
    res.send('<h1>Hello World!</h1>')
})

app.get('/api/ping/', (req, res) => {
    const json = { value: 'pong' }
    res.json(json)
})

app.post('/api/query', (req, res) => {
    return res.status(200).json(req.resultArray)
})

app.use(unknownEndpoint)

module.exports = app
