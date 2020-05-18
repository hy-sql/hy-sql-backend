/* eslint-disable no-unused-vars */
const validator = require('validator')

const CreateTableCommand = (fullCommandAsStringList) => ({
    execute: () => {
        console.log(fullCommandAsStringList)
        let error = false
        const errorMessages = {}

        const tableName = fullCommandAsStringList[2]

        console.log(tableName)

        if (!validateTableName(tableName)) {
            error = true
            errorMessages.tableNameError = 'Invalid table name'
        }

        const columns = fullCommandAsStringList.slice(3)

        console.log(columns)

        if (!validateColumns(columns)) {
            error = true
            errorMessages.tableNameError = 'Invalid columns format'
        }

        const data = parseColumns(columns)

        console.log('CREATE TABLE')
    },
})

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
