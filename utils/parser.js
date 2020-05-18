/* eslint-disable no-unused-vars */
const validator = require('validator')

let fullCommand =
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);'

const parseFullCommand = (fullCommand) => {
    const fullCommandAsStringList = fullCommand.trim().split(/[\s]|(?<=\()/)

    console.log(fullCommandAsStringList)

    if (commandIsCreateTable(fullCommandAsStringList)) {
        return parseCreateTableCommand(fullCommandAsStringList)
    }

    return { commandError: 'INVALID command' }
}

const parseCreateTableCommand = (fullCommandAsStringList) => {
    let error = false
    const errorMessages = {}

    const tableName = fullCommandAsStringList[2]

    if (!validateTableName(tableName)) {
        error = true
        errorMessages.tableNameError = 'Invalid table name'
    }

    const columns = fullCommandAsStringList.slice(3)

    if (!validateColumns(columns)) {
        error = true
        errorMessages.tableNameError = 'Invalid columns format'
    }

    const data = parseColumns(columns)

    return !error ? data : errorMessages
}

const validateTableName = (tableName) => {
    if (
        !validator.isAlphanumeric(tableName) ||
        !validator.isAlpha(
            tableName.charAt(0) || validator.isLength(tableName, { max: 64 })
        )
    ) {
        return false
    }

    return true
}

const commandIsCreateTable = (fullCommandAsStringList) => {
    return fullCommandAsStringList.slice(0, 2).join(' ') === 'CREATE TABLE'
}

const commandIsInsertInto = (fullCommandAsStringList) => {
    // TODO
    return true
}

const commandIsSelect = (fullCommandAsStringList) => {
    //TODO
    return true
}

const validateColumns = (columns) => {
    // TODO
    return true
}

const parseColumns = (columns) => {
    // TODO
    return columns
}

console.log(parseFullCommand(fullCommand))
