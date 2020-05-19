/* eslint-disable no-unused-vars */
const Command = require('../commands/Command')

let command =
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);'

const parseCommand = (input) => {
    const fullCommandAsStringList = input
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    const command = Command(fullCommandAsStringList)

    return command
        ? console.log(command.execute(fullCommandAsStringList))
        : console.log('error')
}

parseCommand(command)
