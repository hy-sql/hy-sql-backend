const queryRouter = require('express').Router()
const State = require('../models/State')
const parseCommand = require('../commands/parseCommand')

queryRouter.post('/', (req, res) => {
    const body = req.body

    if (!body.query) {
        return res.status(400).json({
            error: 'query missing'
        })
    }

    // TODO: array validatation
    // body.query = [ 'CREATE TABLE Nimet (nimi text);', 'CREATE TABLE Luvut (numero integer);' ]
    const commandArray = body.query
    let resultArray = []
    const state = new State([])

    try {
        for (command of commandArray) {
            result = parseCommand(state, command)
            resultArray.push(result)
        }
    } catch (error) {
        // 200 OK + resultArray + "SQL ERROR"
    }

    console.log(resultArray)

    // mitä palautetaan käyttäjälle?
    commandArray.length === resultArray.length ? console.log('SQL OK') : console.log('SQL ERROR')

    const queryObj = {
        query: body.query
    }
    res.json(queryObj)
})

module.exports = queryRouter