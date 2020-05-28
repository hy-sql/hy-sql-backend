const { InsertIntoSchema } = require('../models/InsertIntoSchema')
const { parseColumnNames } = require('./parserTools/parseColumnNames')

const parseCommand = (fullCommandAsStringList) => {
    let anchorLocation = fullCommandAsStringList
        .join(' ')
        .toUpperCase()
        .split(' ')
        .indexOf('VALUES')

    if (anchorLocation === -1) {
        return {
            value: { name: fullCommandAsStringList.slice(0, 2).join(' ') },
            error: {
                details: [
                    {
                        message:
                            'INSERT INTO needs a VALUES keyword before the actual values to be inserted',
                    },
                ],
            },
        }
    }

    let parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        size: fullCommandAsStringList.length,
        parserCounter: 3,
        tableName: fullCommandAsStringList[2],
        anchorKeyword: fullCommandAsStringList[anchorLocation],
    }

    if (fullCommandAsStringList[parsedCommand.parserCounter] === '(') {
        parsedCommand.columnsOpeningBracket = '('
        parsedCommand.parserCounter++
    }

    parsedCommand = parseColumnNames(fullCommandAsStringList, parsedCommand)

    if (fullCommandAsStringList[parsedCommand.parserCounter] === ')') {
        parsedCommand.columnsClosingBracket = ')'
    }

    //VALUES kentät
    const parseErrors = []
    let lohko = []
    loop1: for (
        let index = anchorLocation + 1;
        index < fullCommandAsStringList.length;
        index++
    ) {
        switch (fullCommandAsStringList[index]) {
            case ';':
                parsedCommand.finalSemicolon = ';'
                if (index < fullCommandAsStringList.length - 1)
                    parseErrors.push({
                        message: 'There is unparsed text after semicolon',
                    })
                break loop1
            case '(':
                if (!parsedCommand.valuesOpeningBracket) {
                    parsedCommand.valuesOpeningBracket = '('
                    continue loop1
                } else {
                    parseErrors.push({
                        message: 'Too many opening brackets in values',
                    })
                    continue loop1
                }
            case '))':
                if (!parsedCommand.valuesClosingBracket) {
                    parsedCommand.values = addAttributesToValuesArray(
                        parsedCommand.columns,
                        cleanStringArray(lohko)
                    )
                    parseErrors.push({
                        message: 'Too many closing brackets in values',
                    })
                    parsedCommand.valuesClosingBracket = ')'
                    lohko = []
                    continue loop1
                } else {
                    parseErrors.push({
                        message: 'Too many closing brackets in values',
                    })
                    continue loop1
                }
            case ')':
                if (!parsedCommand.valuesClosingBracket) {
                    parsedCommand.values = addAttributesToValuesArray(
                        parsedCommand.columns,
                        cleanStringArray(lohko)
                    )
                    parsedCommand.valuesClosingBracket = ')'
                    lohko = []
                    continue loop1
                } else {
                    parseErrors.push({
                        message: 'Too many closing brackets in values',
                    })
                    continue loop1
                }
            default:
                lohko.push(fullCommandAsStringList[index])
        }
    }
    if (lohko.length !== 0) {
        parsedCommand.values = addAttributesToValuesArray(
            parsedCommand.columns,
            cleanStringArray(lohko)
        )
    }

    const palautettava = InsertIntoSchema.validate(parsedCommand)
    if (!palautettava.error && parseErrors.length > 0) {
        palautettava.error = { details: [] }
        parseErrors.map((pe) => palautettava.error.details.push(pe))
    }
    return palautettava
}

const cleanStringArray = (columnsAsStringList) => {
    return columnsAsStringList
        .join(' ')
        .split(', ')
        .map((col) => col.trim())
}

const addAttributesToValuesArray = (columnList, stringArray) => {
    const taulukko = stringArray.map((value, index) =>
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
    return taulukko
}

module.exports = { parseCommand }
