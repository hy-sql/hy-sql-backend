const express = require('express')
const app = express()
const cors = require('cors')
const morgan = require('morgan')

morgan.token('body', function (request) { return JSON.stringify(request.body) })

app.use(express.json())
app.use(cors())
app.use(morgan(':method :url :status :res[content-length] - :response-time ms :body'))

app.get('/', (req, res) => {
    res.send('<h1>Hello World!</h1>')
})

app.get('/api/ping/', (req, res) => {
    const json = { value: 'pong' }
    res.json(json)
})
module.exports = app
