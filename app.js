const express = require('express')
const app = express()
const cors = require('cors')

app.use(express.json())
app.use(cors())

app.get('/', (req, res) => {
    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 1: true, valid')
    const selectCommand = require('./commands/selectAllCommand')
    const query = 'SELECT * FROM Taulu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand = selectCommand.isCommand(query)
    selectCommand.execute(query)
    console.log('Select *.isCommand:', isCommand)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 2: false, invalid')
    const query2 = 'SELEC * FROM Taulu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand2 = selectCommand.isCommand(query2)
    selectCommand.execute(query2)
    console.log('Select *.isCommand:', isCommand2)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 3: true, invalid')
    const query3 = 'SELECT * FRM Taulu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand3 = selectCommand.isCommand(query3)
    selectCommand.execute(query3)
    console.log('Select *.isCommand:', isCommand3)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 4: true, invalid')
    const query4 = 'SELECT * FROM Tau&lu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand4 = selectCommand.isCommand(query4)
    selectCommand.execute(query4)
    console.log('Select *.isCommand:', isCommand4)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 5: true, invalid')
    const query5 = 'SELECT * FROM Taulu'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand5 = selectCommand.isCommand(query5)
    selectCommand.execute(query5)
    console.log('Select *.isCommand:', isCommand5)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 6: true, invalid')
    const query6 = 'SELECT * FROM Taulu:'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand6 = selectCommand.isCommand(query6)
    selectCommand.execute(query6)
    console.log('Select *.isCommand:', isCommand6)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 7 true, invalid')
    const query7 = 'SELECT * FROM'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand7 = selectCommand.isCommand(query7)
    selectCommand.execute(query7)
    console.log('Select *.isCommand:', isCommand7)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 8: true, valid')
    const query8 = 'SELECT    * FROM        Taulu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand8 = selectCommand.isCommand(query8)
    selectCommand.execute(query8)
    console.log('Select *.isCommand:', isCommand8)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 9: true, valid')
    const query9 = 'SELECT * FrOM Taulu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand9 = selectCommand.isCommand(query9)
    selectCommand.execute(query9)
    console.log('Select *.isCommand:', isCommand9)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 10: true, valid')
    const query10 = 'SELECT * FROM Taulun_nimi;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand10 = selectCommand.isCommand(query10)
    selectCommand.execute(query10)
    console.log('Select *.isCommand:', isCommand10)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 11: true, valid')
    const query11 = 'sELeCT * FROM Taulu;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand11 = selectCommand.isCommand(query11)
    selectCommand.execute(query11)
    console.log('Select *.isCommand:', isCommand11)

    // Following few lines to be removed before merge, here for testing purposes
    console.log('------ 12: true, valid')
    const query12 = 'SELECT * FROM Taulun_nimi;'
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)
    const isCommand12 = selectCommand.isCommand(query12)
    selectCommand.execute(query12)
    console.log('Select *.isCommand:', isCommand12)

    res.send('<h1>Hello World!</h1>')
})

app.get('/api/ping/', (req, res) => {
    const json = { value: 'pong' }
    res.json(json)
})

module.exports = app
