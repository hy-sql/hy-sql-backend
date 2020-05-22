const express = require('express')
const app = express()
const cors = require('cors')

app.use(express.json())
app.use(cors())

const requestLogger = require('./middleware/logger')
const executer = require('./middleware/executer')

app.use(requestLogger)
app.use('/test', executer)

app.get('/', (req, res) => {
    res.send('<h1>Hello World!</h1>')
})

app.get('/api/ping/', (req, res) => {
    const json = { value: 'pong' }
    res.json(json)
})

app.post('/test', (req, res) => {
    return res.status(200).json(req.resultArray)
})

module.exports = app
