const express = require('express')
const app = express()

app.get('/', (req,res) => {
    res.send('<h1>Hello World!</h1>')
})

app.get('/api/ping/', (req, res) => {
    const json = {value: 'pong'}
    res.json(json)
})

const PORT = 3001
app.listen(PORT, () => {
    console.log('hello world!')
})
