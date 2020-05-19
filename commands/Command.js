/* eslint-disable indent */
/* eslint-disable no-unused-vars */
const CreateTableCommand = require('./createTableCommand')
const InsertIntoCommand = require('./insertIntoCommand')
const SelectAllCommand = require('./selectAllCommand')

let testInput =
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);'

const Command = (input) => {
    const commands = [CreateTableCommand, InsertIntoCommand, SelectAllCommand]

    const fullCommandAsStringList = input
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    console.log(fullCommandAsStringList)

    commands.forEach((c) => {
        if (c.isCommand(fullCommandAsStringList))
            c.execute(fullCommandAsStringList)
    })
}

Command(testInput)

module.exports = Command
