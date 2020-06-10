const { InsertIntoSchema } = require('../schemas/InsertIntoSchema')
const { parseColumnNames } = require('./parserTools/parseColumnNames')

const parseCommand = (fullCommandAsStringArray) => {
    let anchorLocation = fullCommandAsStringArray
        .join(' ')
        .toUpperCase()
        .split(' ')
        .indexOf('VALUES')

    if (anchorLocation === -1) {
        return {
            value: { name: fullCommandAsStringArray.slice(0, 2).join(' ') },
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

    //VALUES kentät
    const parseErrors = []
    let lohko = []
    loop1: for (
        let index = anchorLocation + 1;
        index < fullCommandAsStringArray.length;
        index++
    ) {
        switch (fullCommandAsStringArray[index]) {
            case ';':
                parsedCommand.finalSemicolon = ';'
                if (index < fullCommandAsStringArray.length - 1)
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
                lohko.push(fullCommandAsStringArray[index])
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
