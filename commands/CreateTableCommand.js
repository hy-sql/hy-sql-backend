/* eslint-disable no-unused-vars */
const validator = require('validator')

const CreateTableCommand = (fullCommandAsStringList) => ({
    execute: () => {
        console.log(fullCommandAsStringList)

        return validateCommand(fullCommandAsStringList)
    },
})

const validateCommand = (fullCommandAsStringList) => {
    const command = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        openingBracket: fullCommandAsStringList[3],
        columns: fullCommandAsStringList.slice(
            4,
            fullCommandAsStringList.length - 2
        ),
        closingBracket: fullCommandAsStringList[fullCommandAsStringList.length - 2],
        finalSemicolon: fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    return command
}

const createTable = (tableName, columns) => {
    const newTable = {
        name: tableName,
        columns: columns,
    }

    return validateTable(newTable)
}

const validateTable = (table) => {
    // TODO
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

const validateColumns = (columns) => {
    // TODO
    return columns
}

const parseColumns = (columns) => {
    // TODO
    return columns
}

module.exports = CreateTableCommand
