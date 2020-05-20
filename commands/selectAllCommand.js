/* eslint-disable no-unused-vars */
const selectAllSchema = require('../models/SelectAllSchema')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ').toUpperCase() === 'SELECT *'

const execute = (fullCommandAsStringList) => {
    console.log('Input of SELECT * execute:', fullCommandAsStringList)

    const command = parseCommand(fullCommandAsStringList)

    const validationResult = selectAllSchema.validate(command)
    console.log('Validation result:', validationResult)
    validationResult.error
        ? console.log('Error type & message:', validationResult.error.details)
        : ''

    // check correct format of returned data
    // return validationResult is one option. validationResult is either {error: , value:} or {value:}
    //should in case of error instead of whole error only the details of error be returned? So:  validationResult.error.details
    return validationResult.error
        ? validationResult.error
        : validationResult.value
}

const parseCommand = (fullCommandAsStringList) => {
    const name = fullCommandAsStringList.slice(0, 2).join(' ')

    const command = {
        name: name ? name.toUpperCase() : name,
        from: fullCommandAsStringList[2]
            ? fullCommandAsStringList[2].toUpperCase()
            : fullCommandAsStringList,
        tableName: fullCommandAsStringList[3],
        finalSemicolon: fullCommandAsStringList[4],
    }

    console.log('Parsed command:', command)

    return command
}

module.exports = { isCommand, execute }
