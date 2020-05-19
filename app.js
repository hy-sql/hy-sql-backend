const express = require('express')
const app = express()
const cors = require('cors')

app.use(express.json())
app.use(cors())

app.get('/', (req, res) => {
    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 1')
    const selectCommand = require('./commands/selectAllCommand')
    const query = 'SELECT * FROM Taulu;'
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand = selectCommand.isCommand(query)
    selectCommand.execute(query)
    console.log('Select *.isCommand:', isCommand)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 2')
    const query2 = 'SELEC * FROM Taulu;'
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand2 = selectCommand.isCommand(query2)
    selectCommand.execute(query2)
    console.log('Select *.isCommand:', isCommand2)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 3')
    const query3 = 'SELECT * FRM Taulu;'
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand3 = selectCommand.isCommand(query3)
    selectCommand.execute(query3)
    console.log('Select *.isCommand:', isCommand3)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 4')
    const query4 = 'SELECT * FROM Tau&lu;'
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand4 = selectCommand.isCommand(query4)
    selectCommand.execute(query4)
    console.log('Select *.isCommand:', isCommand4)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 5')
    const query5 = 'SELECT * FROM Taulu'
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand5 = selectCommand.isCommand(query5)
    selectCommand.execute(query5)
    console.log('Select *.isCommand:', isCommand5)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 6')
    const query6 = 'SELECT * FROM Taulu:'
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand6 = selectCommand.isCommand(query6)
    selectCommand.execute(query6)
    console.log('Select *.isCommand:', isCommand6)

    res.send('<h1>Hello World!</h1>')
})

app.get('/api/ping/', (req, res) => {
    const json = { value: 'pong' }
    res.json(json)
})

module.exports = app
