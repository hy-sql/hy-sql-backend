const Joi = require('@hapi/joi')
const { InsertIntoSchema } = require('../schemas/InsertIntoSchema')
const { parseColumnNames } = require('./parserTools/parseColumnNames')
const SQLError = require('../models/SQLError')

/**
 * Parses and validates an INSERT INTO command object from the given array.
 * Returns the validated command object or throws a validation error if the
 * object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseCommand = (fullCommandAsStringArray) => {
    let anchorLocation = fullCommandAsStringArray
        .join(' ')
        .toUpperCase()
        .split(' ')
        .indexOf('VALUES')

    if (anchorLocation === -1) {
        throw new SQLError(
            'INSERT INTO needs a VALUES keyword before the actual values to be inserted'
        )
    }

    let parsedCommand = {
        name: fullCommandAsStringArray.slice(0, 2).join(' '),
        size: fullCommandAsStringArray.length,
        parserCounter: 3,
        tableName: fullCommandAsStringArray[2],
        anchorKeyword: fullCommandAsStringArray[anchorLocation],
    }

    if (fullCommandAsStringArray[parsedCommand.parserCounter] === '(') {
        parsedCommand.columnsOpeningBracket = '('
        parsedCommand.parserCounter++
    }

    const { pccolumns, columns } = parseColumnNames(
        fullCommandAsStringArray,
        parsedCommand.parserCounter
    )
    if (columns) {
        parsedCommand.parserCounter = pccolumns
        parsedCommand.columns = columns
    }

    if (fullCommandAsStringArray[parsedCommand.parserCounter] === ')') {
        parsedCommand.columnsClosingBracket = ')'
    }

    //VALUES fields
    let block = []
    loop1: for (
        let index = anchorLocation + 1;
        index < fullCommandAsStringArray.length;
        index++
    ) {
        switch (fullCommandAsStringArray[index]) {
            case ';':
                parsedCommand.finalSemicolon = ';'
                if (index < fullCommandAsStringArray.length - 1)
                    throw new SQLError('There is unparsed text after semicolon')
                break loop1
            case '(':
                if (!parsedCommand.valuesOpeningBracket) {
                    parsedCommand.valuesOpeningBracket = '('
                    continue loop1
                } else {
                    throw new SQLError('Too many opening brackets in values')
                }
            case '))':
                throw new SQLError('Too many closing brackets in values')
            case ')':
                if (!parsedCommand.valuesClosingBracket) {
                    parsedCommand.values = addAttributesToValuesArray(
                        parsedCommand.columns,
                        cleanStringArray(block)
                    )
                    parsedCommand.valuesClosingBracket = ')'
                    block = []
                    continue loop1
                } else {
                    throw new SQLError('Too many closing brackets in values')
                }
            default:
                block = block.concat(fullCommandAsStringArray[index])
        }
    }
    if (block.length !== 0) {
        parsedCommand.values = addAttributesToValuesArray(
            parsedCommand.columns,
            cleanStringArray(block)
        )
    }

    const validatedCommand = Joi.attempt(parsedCommand, InsertIntoSchema)

    return validatedCommand
}

/**
 * Creates a new array by forming a string from the contents of the given array
 * and splitting it using ',' as the separator.
 * @param {string[]} columnsAsStringList string array
 */
const cleanStringArray = (columnsAsStringList) => {
    return columnsAsStringList
        .join(' ')
        .split(', ')
        .map((col) => col.trim())
}

/**
 * Handles parsing of the column values information from the given arrays.
 * @param {string[]} columnList array containing the column information
 * @param {string[]} stringArray array containing the values information
 */
const addAttributesToValuesArray = (columnList, stringArray) => {
    const arr = stringArray.map((value, index) =>
        value.match('[0-9]')
            ? {
                  column: columnList[index] ? columnList[index].name : null,
                  value: Number(value),
                  type: 'INTEGER',
              }
            : {
                  column: columnList[index] ? columnList[index].name : null,
                  value: value.replace(/'/g, ' ').trim(),
                  type: 'TEXT',
              }
    )
    return arr
}

module.exports = { parseCommand }
