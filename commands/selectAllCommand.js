/* eslint-disable no-unused-vars */
const selectAllSchema = require('../models/SelectAllSchema')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'SELECT *'

const execute = (fullCommandAsStringList) => {
    console.log('Input of SELECT * execute:', fullCommandAsStringList)

    const command = parseCommand(fullCommandAsStringList)

    const validationResult = selectAllSchema.validate(command)
    console.log('Validation result:', validationResult)

    // check correct format of returned data
    return validationResult.error
        ? validationResult.error
        : validationResult.value
}

// Any need to handle missing values for correct error recognition?
const parseCommand = (fullCommandAsStringList) => {
    const command = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon: fullCommandAsStringList[4],
    }

    console.log('Parsed command:', command)

    return command
}

module.exports = { isCommand, execute }
